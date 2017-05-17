;;; packages.el --- dark-window layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst cycle-decoration-packages
  '(frame-fns))

(defun cycle-decoration/init-frame-fns ()
  (use-package frame-fns
    :config
    (progn
      (defun cycle-spacemacs-decoration (&optional frame-name)
        "Change color of window decoration in GNOME Adwaita theme"
        ;; Inspired by Nicolas Petton http://nicolas.petton.fr/blog/emacs-dark-window-decoration.html
        (when (eq window-system 'x)
          (if (eq spacemacs--cur-theme 'spacemacs-light)
              (setq color "light")
            (setq color "dark"))
          (setq frame-name (get-frame-name))
          (call-process-shell-command
           (format "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT %s -name %s" color frame-name))))

      (cycle-spacemacs-decoration)
      (add-hook 'after-make-frame-functions 'cycle-spacemacs-decoration 'APPEND)
      (add-hook 'spacemacs-post-theme-change-hook 'cycle-spacemacs-decoration 'APPEND))))
