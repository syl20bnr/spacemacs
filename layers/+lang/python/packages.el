;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    py-yapf
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
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" 'anaconda-mode-show-doc
        "gg" 'anaconda-mode-find-definitions
        "ga" 'anaconda-mode-find-assignments
        "gu" 'anaconda-mode-find-references)
      (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
        (kbd "q") 'quit-window)
      (spacemacs|hide-lighter anaconda-mode)

      (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
        (evil--jumps-push)))))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'anaconda-mode-view-doc
        "gg" 'anaconda-mode-goto
        "gu" 'anaconda-mode-usages))))

(defun python/post-init-eldoc ()
  (defun spacemacs//init-eldoc-python-mode ()
    (eldoc-mode)
    (when (configuration-layer/package-usedp 'anaconda-mode)
      (anaconda-eldoc-mode)))
  (add-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun python/init-nose ()
  (use-package nose
    :if (eq 'nose python-test-runner)
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "tA" 'nosetests-pdb-all
      "ta" 'nosetests-all
      "tB" 'nosetests-pdb-module
      "tb" 'nosetests-module
      "tT" 'nosetests-pdb-one
      "tt" 'nosetests-one
      "tM" 'nosetests-pdb-module
      "tm" 'nosetests-module
      "tS" 'nosetests-pdb-suite
      "ts" 'nosetests-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

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
    :if (executable-find "pyenv")
    :commands (pyenv-mode-versions)
    :init
    (progn
      (pcase python-auto-set-local-pyenv-version
       (`on-visit
        (add-hook 'python-mode-hook 'pyenv-mode-set-local-version))
       (`on-project-switch
        (add-hook 'projectile-after-switch-project-hook 'pyenv-mode-set-local-version)))
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "vs" 'pyenv-mode-set
        "vu" 'pyenv-mode-unset))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "V" 'pyvenv-workon)))

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
    :init (spacemacs/set-leader-keys-for-major-mode 'python-mode
            "tA" 'pytest-pdb-all
            "ta" 'pytest-all
            "tB" 'pytest-pdb-module
            "tb" 'pytest-module
            "tT" 'pytest-pdb-one
            "tt" 'pytest-one
            "tM" 'pytest-pdb-module
            "tm" 'pytest-module)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (progn
      (defun python-default ()
        (setq mode-name "Python"
              tab-width 4
              fill-column python-fill-column
              ;; auto-indent on colon doesn't work well with if statement
              electric-indent-chars (delq ?: electric-indent-chars))
        (annotate-pdb)
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
      (with-eval-after-load 'auto-highlight-symbol
        (add-to-list 'ahs-plugin-bod-modes 'python-mode))

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

      (spacemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
      (spacemacs/declare-prefix-for-mode 'python-mode "md" "debug")
      (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL")
      (spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'python-mode "mv" "venv")
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "cc" 'spacemacs/python-execute-file
        "cC" 'spacemacs/python-execute-file-focus
        "db" 'python-toggle-breakpoint
        "ri" 'python-remove-unused-imports
        "sB" 'python-shell-send-buffer-switch
        "sb" 'python-shell-send-buffer
        "sF" 'python-shell-send-defun-switch
        "sf" 'python-shell-send-defun
        "si" 'python-start-or-switch-repl
        "sR" 'python-shell-send-region-switch
        "sr" 'python-shell-send-region)

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
      (define-key inferior-python-mode-map
        (kbd "C-c M-l") 'spacemacs/comint-clear-buffer))))

(defun python/post-init-evil-matchit ()
    (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'python-mode-hook))

(defun python/init-hy-mode ()
  (use-package hy-mode
    :defer t))

(defun python/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc)))

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

(defun python/init-py-yapf ()
  (use-package py-yapf
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "=" 'py-yapf-buffer)
    :config
    (when python-enable-yapf-format-on-save
      (add-hook 'python-mode-hook 'py-yapf-enable-on-save))))

(defun python/post-init-semantic ()
  (when (configuration-layer/package-usedp 'anaconda-mode)
      (add-hook 'python-mode-hook
                'spacemacs//disable-semantic-idle-summary-mode t))
  (add-hook 'python-mode-hook 'semantic-mode)
  (add-hook 'python-mode-hook 'spacemacs//python-imenu-create-index-use-semantic)

  (defadvice semantic-python-get-system-include-path
      (around semantic-python-skip-error-advice activate)
    "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
    (condition-case-unless-debug nil
        ad-do-it
      (error nil))))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/lazy-load-stickyfunc-enhance))

(defun python/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "gi" 'cscope/run-pycscope)))

(defun python/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/setup-helm-cscope 'python-mode)))
