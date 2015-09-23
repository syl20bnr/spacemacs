;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq python-packages
  '(
    anaconda-mode
    company
    company-anaconda
    cython-mode
    eldoc
    evil-jumper
    evil-matchit
    flycheck
    helm-cscope
    helm-pydoc
    hy-mode
    pip-requirements
    pyenv-mode
    pytest
    python
    pyvenv
    semantic
    smartparens
    stickyfunc-enhance
    xcscope
    ))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (progn
      (setq anaconda-mode-installation-directory
            (concat spacemacs-cache-directory "anaconda-mode"))
      (add-hook 'python-mode-hook 'anaconda-mode))
    :config
    (progn
      (evil-leader/set-key-for-mode 'python-mode
        "mhh" 'anaconda-mode-show-doc
        "mgg" 'anaconda-mode-find-definitions
        "mga" 'anaconda-mode-find-assignments
        "mgu" 'anaconda-mode-find-references)
      (evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
               (kbd "q") 'quit-window)
      (spacemacs|hide-lighter anaconda-mode))))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'cython-mode
        "mhh" 'anaconda-mode-view-doc
        "mgg" 'anaconda-mode-goto
        "mgu" 'anaconda-mode-usages))))

(defun python/post-init-eldoc ()
  (add-hook 'python-mode-hook 'eldoc-mode))

(defun python/post-init-evil-jumper ()
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil-jumper--push)))

(defun python/init-pip-requirements ()
  (use-package pip-requirements
    :defer t
    :init
    (progn
      ;; company support
      (push 'company-capf company-backends-pip-requirements-mode)
      (spacemacs|add-company-hook pip-requirements-mode))))

(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :defer t
    :init (progn
            (evil-leader/set-key-for-mode 'python-mode
              "mvs" 'pyenv-mode-set
              "mvu" 'pyenv-mode-unset))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (evil-leader/set-key-for-mode 'python-mode
      "mV" 'pyvenv-workon)))

(defun python/init-pytest ()
  (use-package pytest
    :if (eq 'pytest python-test-runner)
    :defer t
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init (evil-leader/set-key-for-mode 'python-mode
            "mtA" 'pytest-pdb-all
            "mta" 'pytest-all
            "mtB" 'pytest-pdb-module
            "mtb" 'pytest-module
            "mtT" 'pytest-pdb-one
            "mtt" 'pytest-one
            "mtM" 'pytest-pdb-module
            "mtm" 'pytest-module)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (progn
      (defun python-default ()
        (setq mode-name "Python"
              tab-width 4
              fill-column 79
              ;; auto-indent on colon doesn't work well with if statement
              electric-indent-chars (delq ?: electric-indent-chars))
        (annotate-pdb)
        (spacemacs/highlight-TODO-words)
        ;; make C-j work the same way as RET
        (local-set-key (kbd "C-j") 'newline-and-indent))

      (defun python-setup-shell ()
        (if (executable-find "ipython")
            (progn
              (setq python-shell-interpreter "ipython")
              (when (version< emacs-version "24.4")
                ;; these settings are unnecessary and even counter-productive on emacs 24.4 and newer
                (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
          (setq python-shell-interpreter "python")))

      (defun inferior-python-setup-hook ()
        (setq indent-tabs-mode t))

      (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
      (spacemacs/add-all-to-hook 'python-mode-hook
                                 'python-default
                                 'python-setup-shell))
    :config
    (progn
      (add-hook 'inferior-python-mode-hook 'smartparens-mode)
      ;; add support for `ahs-range-beginning-of-defun' for python-mode
      (eval-after-load 'auto-highlight-symbol
        '(add-to-list 'ahs-plugin-bod-modes 'python-mode))

      (defun python-shell-send-buffer-switch ()
        "Send buffer content to shell and switch to it in insert mode."
        (interactive)
        (python-shell-send-buffer)
        (python-shell-switch-to-shell)
        (evil-insert-state))

      (defun python-shell-send-defun-switch ()
        "Send function content to shell and switch to it in insert mode."
        (interactive)
        (python-shell-send-defun nil)
        (python-shell-switch-to-shell)
        (evil-insert-state))

      (defun python-shell-send-region-switch (start end)
        "Send region content to shell and switch to it in insert mode."
        (interactive "r")
        (python-shell-send-region start end)
        (python-shell-switch-to-shell)
        (evil-insert-state))

      (defun python-start-or-switch-repl ()
        "Start and/or switch to the REPL."
        (interactive)
        (python-shell-switch-to-shell)
        (evil-insert-state))

      ;; reset compile-command (by default it is `make -k')
      (setq compile-command nil)
      (defun spacemacs/python-execute-file (arg)
        "Execute a python script in a shell."
        (interactive "P")
        ;; set compile command to buffer-file-name
        ;; universal argument put compile buffer in comint mode
        (setq universal-argument t)
        (if arg
            (call-interactively 'compile)

          (setq compile-command (format "python %s" (file-name-nondirectory
                                                     buffer-file-name)))
          (compile compile-command t)
          (with-current-buffer (get-buffer "*compilation*")
            (inferior-python-mode))))

      (defun spacemacs/python-execute-file-focus (arg)
        "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
        (interactive "P")
        (spacemacs/python-execute-file arg)
        (switch-to-buffer-other-window "*compilation*")
        (end-of-buffer)
        (evil-insert-state))

      (evil-leader/set-key-for-mode 'python-mode
        "mcc" 'spacemacs/python-execute-file
        "mcC" 'spacemacs/python-execute-file-focus
        "mdb" 'python-toggle-breakpoint
        "mri" 'python-remove-unused-imports
        "msB" 'python-shell-send-buffer-switch
        "msb" 'python-shell-send-buffer
        "msF" 'python-shell-send-defun-switch
        "msf" 'python-shell-send-defun
        "msi" 'python-start-or-switch-repl
        "msR" 'python-shell-send-region-switch
        "msr" 'python-shell-send-region)

      ;; Emacs users won't need these key bindings
      ;; TODO: make these key bindings dynamic given the current style
      ;; Doing it only at init time won't update it if the user switches style
      ;; Also find a way to generalize these bindings.
      (when (eq dotspacemacs-editing-style 'vim)
        ;; the default in Emacs is M-n
        (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
        ;; the default in Emacs is M-p and this key binding overrides default C-k
        ;; which prevents Emacs users to kill line
        (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
        ;; the default in Emacs is M-r; C-r to search backward old output
        ;; and should not be changed
        (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)
        ;; this key binding is for recentering buffer in Emacs
        ;; it would be troublesome if Emacs user
        ;; Vim users can use this key since they have other key
        (define-key inferior-python-mode-map (kbd "C-l") 'spacemacs/comint-clear-buffer))

      ;; add this optional key binding for Emacs user, since it is unbound
      (define-key inferior-python-mode-map (kbd "C-c M-l") 'spacemacs/comint-clear-buffer)

      ;; fix for issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569)
      ;; use `semantic-create-imenu-index' only when `semantic-mode' is enabled,
      ;; otherwise use `python-imenu-create-index'
      (defun spacemacs/python-imenu-create-index-python-or-semantic ()
        (if (bound-and-true-p semantic-mode)
            (semantic-create-imenu-index)
          (python-imenu-create-index)))

      (defadvice wisent-python-default-setup
          (after spacemacs/python-set-imenu-create-index-function activate)
        (setq imenu-create-index-function
              #'spacemacs/python-imenu-create-index-python-or-semantic)))))

(defun python/post-init-evil-matchit ()
    (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (add-hook 'python-mode-hook 'flycheck-mode))

(defun python/init-hy-mode ()
  (use-package hy-mode
    :defer t))

(defun python/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (evil-leader/set-key-for-mode 'python-mode "mhd" 'helm-pydoc)))

(defun python/post-init-smartparens ()
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun python/post-init-company ()
    (spacemacs|add-company-hook python-mode)
    (spacemacs|add-company-hook inferior-python-mode)
    (push '(company-files company-capf) company-backends-inferior-python-mode)
    (add-hook 'inferior-python-mode-hook (lambda ()
                                           (setq-local company-minimum-prefix-length 0)
                                           (setq-local company-idle-delay 0.5))))

  (defun python/init-company-anaconda ()
    (use-package company-anaconda
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push 'company-anaconda company-backends-python-mode))))

(defun python/post-init-semantic ()
  (semantic/enable-semantic-mode 'python-mode)
  (defadvice semantic-python-get-system-include-path (around semantic-python-skip-error-advice activate)
    "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
    (condition-case nil
        ad-do-it
      (error nil))))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/lazy-load-stickyfunc-enhance))

(defun python/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (evil-leader/set-key-for-mode 'python-mode "mgi" 'cscope/run-pycscope)))

(defun python/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/setup-helm-cscope 'python-mode)))
