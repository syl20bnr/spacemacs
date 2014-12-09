(defvar python-packages
  '(
    anaconda-mode
    ac-anaconda
    company-anaconda
    eldoc
    flycheck
    pyvenv
    python
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (progn
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

      (defun python-default ()
        (setq mode-name "Python"
              tab-width 4
              ;; auto-indent on colon doesn't work well with if statement
              electric-indent-chars (delq ?: electric-indent-chars))
        (annotate-pdb)
        (anaconda-mode)
        (eldoc-mode)
        (spacemacs|diminish anaconda-mode " (Ⓐ)")
        (spacemacs|hide-lighter eldoc-mode)
        (spacemacs/highlight-TODO-words)
        (when (boundp 'ac-sources)
          (ac-anaconda-setup))
        (when (boundp 'company-backends)
          (add-to-list 'company-backends 'company-anaconda))
        ;; make C-j work the same way as RET
        (local-set-key (kbd "C-j") 'newline-and-indent))

      (add-hook 'python-mode-hook 'python-default)
      (add-hook 'python-mode-hook 'python-setup-shell))
    :config
    (progn
      ;; add support for `ahs-range-beginning-of-defun' for python-mode
      (eval-after-load 'auto-highlight-symbol
        '(add-to-list 'ahs-plugin-bod-modes 'python-mode))
      (evil-leader/set-key-for-mode 'python-mode
        "msB"  (lambda ()
                 " Send buffer content to shell and switch to it in insert mode."
                 (interactive)
                 (python-shell-send-buffer)
                 (python-shell-switch-to-shell)
                 (evil-insert-state))
        "msb"  'python-shell-send-buffer
        "md"  'anaconda-mode-view-doc
        "mD"  'pylookup-lookup
        "msF"  (lambda ()
                 " Send function content to shell and switch to it in insert mode."
                 (interactive)
                 (python-shell-send-defun nil)
                 (python-shell-switch-to-shell)
                 (evil-insert-state))
        "msf"  'python-shell-send-defun
        "mg"  (lambda ()
                (interactive)
                (when (fboundp 'evil-jumper--push)
                  (evil-jumper--push))
                (anaconda-mode-goto))
        "mi"  (lambda ()
                " Switch to shell in insert mode."
                (interactive)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "mb"  'python-toggle-breakpoint
        "msR"  (lambda (start end)
                " Send region content to shell and switch to it in insert mode."
                (interactive "r")
                (python-shell-send-region start end)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "msr"  'python-shell-send-region
        "mv"  'pyvenv-workon)

      (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
      (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input))))

(defun python/init-flycheck ()
  (add-hook 'python-mode-hook 'flycheck-mode))
