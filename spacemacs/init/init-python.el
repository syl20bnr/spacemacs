(use-package python
  :defer t
  :init
  (progn
    (add-hook 'python-mode-hook '(lambda() (setq tab-width 4)))
    ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
    (defun annotate-pdb ()
      "Highlight break point lines."
      (interactive)
      (highlight-lines-matching-regexp "import pdb")
      (highlight-lines-matching-regexp "pdb.set_trace()"))
    (add-hook 'python-mode-hook 'annotate-pdb)
        (setq
         python-shell-interpreter "ipython"
         ;; python-shell-interpreter-args (if (system-is-mac)
         ;;                                   "--gui=osx --matplotlib=osx --colors=Linux"
         ;;                                 (if (system-is-linux)
         ;;                                     "--gui=wx --matplotlib=wx --colors=Linux"))
         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
         python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
         python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
         python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (use-package jedi
      :defer t
      :init
      (progn
        (setq jedi:setup-keys t)
        (add-hook 'python-mode-hook 'jedi:setup))
      :config
      (progn
        (setq jedi:complete-on-dot t))))
  :config
  (progn
    ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
    (defun python-add-breakpoint ()
      "Add a break point, highlight it and save the buffer."
      (interactive)
      (evil-end-of-line)
      (newline-and-indent)
      (insert "import pdb; pdb.set_trace()")
      (save-buffer))
    (evil-leader/set-key-for-mode 'python-mode
      "mB"  (lambda ()
              " Send buffer content to shell and switch to it in insert mode."
              (interactive)
              (python-shell-send-buffer)
              (python-shell-switch-to-shell)
              (evil-insert-state))
      "mb"  'python-shell-send-buffer
      "md"  'pylookup-lookup
      "mF"  (lambda ()
              " Send function content to shell and switch to it in insert mode."
              (interactive)
              (python-shell-send-defun nil)
              (python-shell-switch-to-shell)
              (evil-insert-state))
      "mf"  'python-shell-send-defun
      "mg"  'jedi:goto-definition
      "mi"  (lambda ()
              " Switch to shell in insert mode."
              (interactive)
              (python-shell-switch-to-shell)
              (evil-insert-state))
      "mp"  'python-add-breakpoint
      "mR"  (lambda (start end)
              " Send region content to shell and switch to it in insert mode."
              (interactive "r")
              (python-shell-send-region start end)
              (python-shell-switch-to-shell)
              (evil-insert-state))
      "mr"  'python-shell-send-region
      "mTf" 'nosetests-pdb-one
      "mtf" 'nosetests-one
      "mTa" 'nosetests-pdb-all
      "mta" 'nosetests-all
      "mTm" 'nosetests-pdb-module
      "mtm" 'nosetests-module
      "mTs" 'nosetests-pdb-suite
      "mts" 'nosetests-suite
      "m RET" 'quickrun)
     (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
     (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)))
