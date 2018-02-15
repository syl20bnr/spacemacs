;;; config.el --- Theming Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//in-or-all (key seq)
  (or (eq 'all seq) (memq key seq)))

(defun spacemacs//theming (theme &optional no-confirm no-enable)
  "Removes existing user theming and applies customizations for the given
theme."
  (unless no-enable

    ;; Remove existing modifications
    (dolist (face spacemacs--theming-modified-faces)
      (custom-set-faces `(,face ((t ())))))
    (setq spacemacs--theming-modified-faces nil)

    ;; Headings
    (let ((mods nil))
      (when (spacemacs//in-or-all theme theming-headings-inherit-from-default)
        (setq mods (plist-put mods :inherit 'default)))
      (when (spacemacs//in-or-all theme theming-headings-same-size)
        (setq mods (plist-put mods :height 1.0)))
      (when (spacemacs//in-or-all theme theming-headings-bold)
        (setq mods (plist-put mods :weight 'bold)))
      (when mods
        (dolist (face spacemacs--theming-header-faces)
          (custom-set-faces `(,face ((t ,mods))))
          (push face spacemacs--theming-modified-faces))))

    ;; Add new modifications
    (dolist (spec (append (cdr (assq theme theming-modifications))
                          (cdr (assq t theming-modifications))))
      (custom-set-faces `(,(car spec) ((t ,(cdr spec)))))
      (push (car spec) spacemacs--theming-modified-faces))))

(defun spacemacs/update-theme ()
  (interactive)
  (spacemacs//theming spacemacs--cur-theme))
