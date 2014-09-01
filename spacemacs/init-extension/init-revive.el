(use-package revive
  :init
  (require 'revive-mode-config)
  :config
  (progn
    ;; save and restore layout
    (add-hook 'kill-emacs-hook 'emacs-save-layout)
    (add-hook 'after-init-hook 'emacs-load-layout t)))

