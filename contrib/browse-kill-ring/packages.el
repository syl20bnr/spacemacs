;;; packages.el --- browse-kill-ring Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar browse-kill-ring-packages
  '(browse-kill-ring)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar browse-kill-ring-excluded-packages '()
  "List of packages to exclude.")

(defun browse-kill-ring/init-browse-kill-ring ()
  "Initialize `browse-kill-ring'."
  (use-package browse-kill-ring
    :config
    (setq kill-ring-max 128)
    (browse-kill-ring-default-keybindings)
    (evil-leader/set-key
      "ab" 'browse-kill-ring)))
