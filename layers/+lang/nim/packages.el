(setq nim-packages
      '(company
        flycheck
        flycheck-nim
        nim-mode))

(defun nim/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes nim-mode nimscript-mode))

(defun nim/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nim-mode))

(defun nim/init-flycheck-nim ()
  (use-package flycheck-nim
    :if (configuration-layer/package-usedp 'flycheck)))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :init
    (progn
      (add-hook 'nim-mode-hook 'nimsuggest-mode)
      (push 'nimsuggest-find-definition spacemacs-jump-handlers-nim-mode))
    :config
    (progn
      (defun spacemacs/nim-compile-run ()
        (interactive)
        (shell-command "nim compile --run main.nim"))

      (spacemacs/set-leader-keys-for-major-mode 'nim-mode
        "cr" 'spacemacs/nim-compile-run
        "gb" 'pop-tag-mark))))
