(require 'revive-mode-config)

;; save and restore layout
(add-hook 'kill-emacs-hook 'emacs-save-layout)
(add-hook 'after-init-hook 'emacs-load-layout t)
