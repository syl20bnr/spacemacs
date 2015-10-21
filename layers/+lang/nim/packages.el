(setq nim-packages
      '(company
        company-nim
        flycheck
        flycheck-nim
        nim-mode))

(defun nim/post-init-company ()
  (spacemacs|add-company-hook nim-mode))

(defun nim/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'nim-mode))

(defun nim/init-flycheck-nim ()
  (use-package flycheck-nim
    :if (configuration-layer/package-usedp 'flycheck)))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :init (when (configuration-layer/package-usedp 'company)
            (push 'company-nim company-backends-nim-mode))
    :config
    (progn
      (defun spacemacs/nim-compile-run ()
        (interactive)
        (shell-command "nim compile --run main.nim"))
      (evil-leader/set-key-for-mode 'nim-mode
        "mcr" 'spacemacs/nim-compile-run))))
