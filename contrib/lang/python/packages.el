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

(defvar python-packages
  '(
    anaconda-mode
    ac-anaconda
    company-anaconda
    eldoc
    evil-jumper
    flycheck
    hy-mode
    pony-mode
    pyenv-mode
    pyvenv
    python
    semantic
    smartparens
    cython-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun python/init-ac-anaconda ()
  (use-package ac-anaconda
    :if (boundp 'ac-sources)
    :defer t
    :init (add-hook 'python-mode-hook 'ac-anaconda-setup)))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init (add-hook 'python-mode-hook 'anaconda-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'python-mode
        "mhd" 'anaconda-mode-view-doc
        "mgg"  'anaconda-mode-goto)
      (spacemacs|hide-lighter anaconda-mode))))

(defun python/init-company-anaconda ()
  (use-package company-anaconda
    :if (boundp 'company-backends)
    :defer t
    :init (add-to-list 'company-backends 'company-anaconda)))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'cython-mode
        "mhd" 'anaconda-mode-view-doc
        "mgg"  'anaconda-mode-goto)
      )))

(defun python/init-eldoc ()
  (add-hook 'python-mode-hook 'eldoc-mode))

(defun python/init-evil-jumper ()
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil-jumper--push)))

(defun python/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init (progn
            (evil-leader/set-key-for-mode 'python-mode
              ; d*j*ango *r*unserver
              "mjru" 'pony-runserver
              "mjrr" 'pony-restart-server
              "mjrd" 'pony-stopserver
              "mjrt" 'pony-temp-server
              "mjro" 'pony-browser

              ; d*j*ango *i*nteractive
              "mjis" 'pony-shell
              "mjid" 'pony-db-shell

              ; d*j*ango *s*outh/*s*yncdb
              "mjss" 'pony-syncdb
              "mjsc" 'pony-south-convert
              "mjsi" 'pony-south-initial
              "mjsh" 'pony-south-schemamigration
              "mjsm" 'pony-south-migrate

              ; d*j*ango *t*est
              "mjtt" 'pony-test
              "mjto" 'pony-test-open
              "mjtu" 'pony-test-up
              "mjtd" 'pony-test-down
              "mjtg" 'pony-test-goto-err

              ; d*j*ango *m*anage
              ; not including one-off management commands like "flush" and
              ; "startapp" even though they're implemented in pony-mode,
              ; because this is much handier
              "mjm" 'pony-manage

              ; d*j*ango *f*iles
              "mjfs" 'pony-goto-settings
              "mjfc" 'pony-setting
              "mjft" 'pony-goto-template
              "mjfr" 'pony-resolve

              ; d*j*ango f*a*bric
              "mjaf" 'pony-fabric
              "mjad" 'pony-fabric-deploy))))

(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :defer t
    :init (progn
            (evil-leader/set-key-for-mode 'python-mode
              "mps" 'pyenv-mode-set
              "mpu" 'pyenv-mode-unset))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (evil-leader/set-key-for-mode 'python-mode
      "mv" 'pyvenv-workon)))

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (progn
      (defun python-default ()
        (setq mode-name "Python"
              tab-width 4
              ;; auto-indent on colon doesn't work well with if statement
              electric-indent-chars (delq ?: electric-indent-chars))
        (annotate-pdb)
        (spacemacs/highlight-TODO-words)
        ;; make C-j work the same way as RET
        (local-set-key (kbd "C-j") 'newline-and-indent))

      (defun python-setup-shell ()
        (if (executable-find "ipython")
            (setq python-shell-interpreter "ipython"
                  ;; python-shell-interpreter-args (if (system-is-mac)
                  ;;                                   "--gui=osx --matplotlib=osx --colors=Linux"
                  ;;                                 (if (system-is-linux)
                  ;;                                     "--gui=wx --matplotlib=wx --colors=Linux"))
                  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                  python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                  python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                  python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
          (setq python-shell-interpreter "python")))

      (add-all-to-hook 'python-mode-hook
                       'python-default
                       'python-setup-shell))
    :config
    (progn
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

      (defun python-shell-send-region-switch ()
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

      (evil-leader/set-key-for-mode 'python-mode
        "mB"  'python-shell-send-buffer-switch
        "mb"  'python-shell-send-buffer
        "mdb" 'python-toggle-breakpoint
        "mF"  'python-shell-send-defun-switch
        "mf"  'python-shell-send-defun
        "mi"  'python-start-or-switch-repl
        "mR"  'python-shell-send-region-switch
        "mr"  'python-shell-send-region)

      (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
      (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input))))

(defun python/init-flycheck ()
  (add-hook 'python-mode-hook 'flycheck-mode))

(defun spacemacs/init-hy-mode ()
  (use-package hy-mode
    :defer t))

(defun python/init-semantic ()
  ;; required to correctly load semantic mode
  ;; using the python-mode-hook triggers an error about a deleted buffer.
  (eval-after-load 'python '(semantic-mode 1)))

(defun python/init-smartparens ()
  (defadvice python-indent-dedent-line-backspace
      (around python/sp-backward-delete-char activate)
    (let ((pythonp (or (not smartparens-strict-mode)
                       (char-equal (char-before) ?\s))))
      (if pythonp
          ad-do-it
        (call-interactively 'sp-backward-delete-char)))))
