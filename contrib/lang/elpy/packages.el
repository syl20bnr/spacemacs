(defvar elpy-packages
  '(
    elpy
    evil-jumper
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar elpy-excluded-packages '()
  "List of packages to exclude.")

(defun elpy/init-evil-jumper ()
  (evil-jumper-mode)
  (use-package elpy
    :defer t
    :init (add-hook 'elpy-mode-hook 'evil-jumper-mode)
    :config (progn
              (defadvice elpy-goto-definition (before elpy/elpy-goto-definition activate)
                (evil-jumper--push)))))

(defun elpy/init-elpy ()
  (use-package elpy
    :init (add-all-to-hook 'inferior-python-mode-hook 'company-mode 'smartparens-mode)
    :config (progn
              (if (executable-find "ipython")
                  (elpy-use-ipython))
              (setq elpy-rpc-backend "jedi")
              (elpy-enable)

              (defun python-shell-send-buffer-switch ()
                " Send buffer content to shell and switch to it in insert mode."
                (interactive)
                (elpy-shell-send-region-or-buffer)
                (elpy-shell-switch-to-shell)
                (evil-insert-state))

              (defun python-shell-send-defun-switch ()
                " Send function content to shell and switch to it in insert mode."
                (interactive)
                (python-shell-send-defun nil)
                (elpy-shell-switch-to-shell)
                (evil-insert-state))

              (defun python-start-or-switch-repl ()
                " Switch to shell in insert mode."
                (interactive)
                (python-shell-switch-to-shell)
                (evil-insert-state))

              (defun python-test-and-switch ()
                " Run tests and switch to test-buffer"
                (interactive)
                (elpy-test)
                (switch-to-buffer-other-window "*compilation*"))

              (defun python-shell-send-region-switch (start end)
                " Send region content to shell and switch to it in insert mode."
                (interactive "r")
                (python-shell-send-region start end)
                (python-shell-switch-to-shell)
                (evil-insert-state)))

              (evil-leader/set-key-for-mode 'python-mode
                "mB"  'python-shell-send-buffer-switch
                "mb"  'elpy-shell-send-region-or-buffer
                "md"  'elpy-doc
                "mF"  'python-shell-send-defun-switch
                "mf"  'python-shell-send-defun
                "mg"  'elpy-goto-definition
                "mi"  'python-start-or-switch-repl
                "mp"  'python-toggle-breakpoint
                "mR"  'python-shell-send-region-switch
                "mr"  'python-shell-send-region
                "mt"  'python-test-and-switch
                "mv"  'pyvenv-workon
                "mz"  'elpy-shell-switch-to-shell)

              (evil-leader/set-key-for-mode 'inferior-python-mode
                "md" 'elpy-doc
                "mg" 'elpy-goto-definition)

              (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
              (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)))
