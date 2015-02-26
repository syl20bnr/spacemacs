(defvar evil-snipe-packages '(evil-snipe)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun evil-snipe/init-evil-snipe ()
  (require 'evil-snipe)
  (global-evil-snipe-mode t)

  ;; (evil-snipe-enable-sS)
  (evil-snipe-replace-evil)

  ;; or 'buffer, 'whole-visible or 'whole-buffer
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-repeat-scope 'whole-buffer)

  (setq evil-snipe-enable-highlight t)
  (setq evil-snipe-enable-incremental-highlight t)
  (setq evil-snipe-enable-half-cursor nil)

  (setq evil-snipe-show-prompt nil)
  (setq evil-snipe-smart-case t)
)

(defvar evil-snipe-excluded-packages '()
  "List of packages to exclude.")
