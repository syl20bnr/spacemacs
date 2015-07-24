(setq nim-packages
      '(company
        flycheck
        nim-mode))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :config
    (progn
      (defun spacemacs/nim-compile-run ()
        (interactive)
        (shell-command "nim compile --run main.nim"))

      (evil-leader/set-key-for-mode 'nim-mode
        "mcr" 'spacemacs/nim-compile-run))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun nim/post-init-company ()
    (spacemacs|add-company-hook nim-mode))

  (defun nim/init-company-nim ()
    (use-package company-nim
      :defer t
      :init
      (push 'company-nim company-backends-nim-mode))))
