;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    (company-anaconda :toggle (configuration-layer/package-usedp 'company))
    cython-mode
    eldoc
    evil-matchit
    flycheck
    ggtags
    helm-cscope
    helm-gtags
    (helm-pydoc :toggle (configuration-layer/package-usedp 'helm))
    hy-mode
    live-py-mode
    (nose :location local)
    org
    pip-requirements
    py-isort
    pyenv-mode
    (pylookup :location local)
    pytest
    (python :location built-in)
    pyvenv
    semantic
    smartparens
    stickyfunc-enhance
    xcscope
    yapfify
    ))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (progn
      (setq anaconda-mode-installation-directory
            (concat spacemacs-cache-directory "anaconda-mode"))
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-to-list 'spacemacs-jump-handlers-python-mode
                '(anaconda-mode-find-definitions :async t)))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" 'anaconda-mode-show-doc
        "ga" 'anaconda-mode-find-assignments
        "gb" 'anaconda-mode-go-back
        "gu" 'anaconda-mode-find-references)
      (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
        (kbd "q") 'quit-window)
      (spacemacs|hide-lighter anaconda-mode)

      (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
        (evil--jumps-push)))))

(defun python/post-init-company ()
  (spacemacs|add-company-hook python-mode)
  (spacemacs|add-company-hook inferior-python-mode)
  (push '(company-files company-capf) company-backends-inferior-python-mode)
  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (setq-local company-minimum-prefix-length 0)
                                         (setq-local company-idle-delay 0.5))))

(defun python/init-company-anaconda ()
  (use-package company-anaconda
    :defer t
    :init
    (push 'company-anaconda company-backends-python-mode)))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'anaconda-mode-view-doc
        "gu" 'anaconda-mode-usages))))

(defun python/post-init-eldoc ()
  (defun spacemacs//init-eldoc-python-mode ()
    (eldoc-mode)
    (when (configuration-layer/package-usedp 'anaconda-mode)
      (anaconda-eldoc-mode)))
  (add-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode))

(defun python/post-init-evil-matchit ()
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'python-mode))

(defun python/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/setup-helm-cscope 'python-mode)))

(defun python/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'python-mode))

(defun python/post-init-ggtags ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun python/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc)))

(defun python/init-hy-mode ()
  (use-package hy-mode
    :defer t
    :init
    (progn
      (let ((hy-path (executable-find "hy")))
        (when hy-path
          (setq hy-mode-inferior-lisp-command (concat hy-path " --spy"))
          (spacemacs/set-leader-keys-for-major-mode 'hy-mode
            "si" 'inferior-lisp
            "sb" 'lisp-load-file
            "sB" 'switch-to-lisp
            "se" 'lisp-eval-last-sexp
            "sf" 'lisp-eval-defun
            "sF" 'lisp-eval-defun-and-go
            "sr" 'lisp-eval-region
            "sR" 'lisp-eval-region-and-go))))))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun python/init-nose ()
  (use-package nose
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :init (spacemacs//bind-python-testing-keys)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun python/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun python/init-pip-requirements ()
  (use-package pip-requirements
    :defer t
    :init
    (progn
      ;; company support
      (push 'company-capf company-backends-pip-requirements-mode)
      (spacemacs|add-company-hook pip-requirements-mode))))

(defun python/init-py-isort ()
  (use-package py-isort
    :defer t
    :init
    (progn
      (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rI" 'py-isort-buffer))))

(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :if (executable-find "pyenv")
    :commands (pyenv-mode-versions)
    :init
    (progn
      (pcase python-auto-set-local-pyenv-version
       (`on-visit
        (spacemacs/add-to-hooks 'spacemacs//pyenv-mode-set-local-version
                                '(python-mode-hook
                                  hy-mode-hook)))
       (`on-project-switch
        (add-hook 'projectile-after-switch-project-hook
                  'spacemacs//pyenv-mode-set-local-version)))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyenv-mode-set pyenv-mode-unset))
        (advice-add func :after 'spacemacs/python-setup-shell))
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "vu" 'pyenv-mode-unset
        "vs" 'pyenv-mode-set))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (progn
      (pcase python-auto-set-local-pyvenv-virtualenv
        (`on-visit
         (spacemacs/add-to-hooks 'spacemacs//pyvenv-mode-set-local-virtualenv
                                 '(python-mode-hook
                                   hy-mode-hook)))
        (`on-project-switch
         (add-hook 'projectile-after-switch-project-hook
                   'spacemacs//pyvenv-mode-set-local-virtualenv)))
      (dolist (mode '(python-mode hy-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "Va" 'pyvenv-activate
          "Vd" 'pyvenv-deactivate
          "Vw" 'pyvenv-workon))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
        (advice-add func :after 'spacemacs/python-setup-shell)))))

(defun python/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hH" 'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'python)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.py")
              pylookup-db-file (concat pylookup-dir "pylookup.db")))
        (setq pylookup-completing-read 'completing-read))))

(defun python/init-pytest ()
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init (spacemacs//bind-python-testing-keys)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'python 'python-start-or-switch-repl "python")

      (defun python-default ()
        (setq mode-name "Python"
              tab-width python-tab-width
              fill-column python-fill-column)
        (when (version< emacs-version "24.5")
          ;; auto-indent on colon doesn't work well with if statement
          ;; should be fixed in 24.5 and above
          (setq electric-indent-chars (delq ?: electric-indent-chars)))
        (setq-local comment-inline-offset 2)
        (spacemacs/python-annotate-pdb)
        ;; make C-j work the same way as RET
        (local-set-key (kbd "C-j") 'newline-and-indent))


      (defun inferior-python-setup-hook ()
        (setq indent-tabs-mode t))

      (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
      (add-hook 'python-mode-hook #'python-default)
      ;; call `spacemacs/python-setup-shell' once, don't put it in a hook (see issue #5988)
      (spacemacs/python-setup-shell))
    :config
    (progn
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
        (let ((shell-process
               (or (python-shell-get-process)
                   ;; `run-python' has different return values and different
                   ;; errors in different emacs versions. In 24.4, it throws an
                   ;; error when the process didn't start, but in 25.1 it
                   ;; doesn't throw an error, so we demote errors here and
                   ;; check the process later
                   (with-demoted-errors "Error: %S"
                     ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
                     ;; shell process
                     (call-interactively #'run-python)
                     (python-shell-get-process)))))
          (unless shell-process
            (error "Failed to start python shell properly"))
          (pop-to-buffer (process-buffer shell-process))
          (evil-insert-state)))

      (defun spacemacs/python-execute-file (arg)
        "Execute a python script in a shell."
        (interactive "P")
        ;; set compile command to buffer-file-name
        ;; universal argument put compile buffer in comint mode
        (let ((universal-argument t)
              (compile-command (format "python %s" (file-name-nondirectory
                                                    buffer-file-name))))
          (if arg
              (call-interactively 'compile)
            (compile compile-command t)
            (with-current-buffer (get-buffer "*compilation*")
              (inferior-python-mode)))))

      (defun spacemacs/python-execute-file-focus (arg)
        "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
        (interactive "P")
        (spacemacs/python-execute-file arg)
        (switch-to-buffer-other-window "*compilation*")
        (end-of-buffer)
        (evil-insert-state))

      ;; fix for issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569)
      (when (version< emacs-version "25")
        (advice-add 'wisent-python-default-setup :after
                    #'spacemacs//python-imenu-create-index-use-semantic-maybe))

      (spacemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
      (spacemacs/declare-prefix-for-mode 'python-mode "md" "debug")
      (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL")
      (spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'python-mode "mv" "pyenv")
      (spacemacs/declare-prefix-for-mode 'python-mode "mV" "pyvenv")
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "'"  'python-start-or-switch-repl
        "cc" 'spacemacs/python-execute-file
        "cC" 'spacemacs/python-execute-file-focus
        "db" 'spacemacs/python-toggle-breakpoint
        "ri" 'spacemacs/python-remove-unused-imports
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
        ;; the default in Emacs is M-p and this key binding overrides
        ;; default C-k which prevents Emacs users to kill line
        (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
        ;; the default in Emacs is M-r; C-r to search backward old output
        ;; and should not be changed
        (define-key inferior-python-mode-map
          (kbd "C-r") 'comint-history-isearch-backward)
        ;; this key binding is for recentering buffer in Emacs
        ;; it would be troublesome if Emacs user
        ;; Vim users can use this key since they have other key
        (define-key inferior-python-mode-map
          (kbd "C-l") 'spacemacs/comint-clear-buffer))

      ;; add this optional key binding for Emacs user, since it is unbound
      (define-key inferior-python-mode-map
        (kbd "C-c M-l") 'spacemacs/comint-clear-buffer))))

(defun python/post-init-semantic ()
  (when (configuration-layer/package-usedp 'anaconda-mode)
      (add-hook 'python-mode-hook
                'spacemacs//disable-semantic-idle-summary-mode t))
  (spacemacs/add-to-hook 'python-mode-hook
                         '(semantic-mode
                           spacemacs//python-imenu-create-index-use-semantic-maybe))
  (defadvice semantic-python-get-system-include-path
      (around semantic-python-skip-error-advice activate)
    "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
    (condition-case-unless-debug nil
        ad-do-it
      (error nil))))

(defun python/post-init-smartparens ()
  (spacemacs/add-to-hooks 'smartparens-mode '(inferior-python-mode-hook
                                              hy-mode-hook))
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/lazy-load-stickyfunc-enhance))

(defun python/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "gi" 'cscope/run-pycscope)))

(defun python/init-yapfify ()
  (use-package yapfify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "=" 'yapfify-buffer)
      (when python-enable-yapf-format-on-save
        (add-hook 'python-mode-hook 'yapf-mode)))))
