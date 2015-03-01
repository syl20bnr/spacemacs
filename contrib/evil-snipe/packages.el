(defvar evil-snipe-packages '(evil-snipe)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar evil-snipe-excluded-packages '()
  "List of packages to exclude.")

(defun evil-snipe/init-evil-snipe ()
  (require 'evil-snipe)
  (global-evil-snipe-mode t)

  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-enable-half-cursor nil
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t
        evil-snipe-auto-scroll t
        evil-snipe-repeat-keys t)

  (when evil-snipe-enable-alternate-f-and-t-behaviors
    (evil-snipe-override-mode t)
    (setq evil-snipe-repeat-scope 'whole-buffer)))
