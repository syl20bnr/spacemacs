;;; funcs.el --- Theming Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//in-or-all (key seq)
  (or (eq 'all seq) (memq key seq)))

(defun space-macs//theming (theme &optional no-confirm no-enable)
  "Removes existing user theming and applies customizations for the given
theme."
  (unless no-enable

    ;; Remove existing modifications
    (dolist (face space-macs--theming-modified-faces)
      (custom-set-faces `(,face ((t ())))))
    (setq space-macs--theming-modified-faces nil)

    ;; Headings
    (let ((mods nil))
      (when (space-macs//in-or-all theme theming-headings-inherit-from-default)
        (setq mods (plist-put mods :inherit 'default)))
      (when (space-macs//in-or-all theme theming-headings-same-size)
        (setq mods (plist-put mods :height 1.0)))
      (when (space-macs//in-or-all theme theming-headings-bold)
        (setq mods (plist-put mods :weight 'bold)))
      (when mods
        (dolist (face space-macs--theming-header-faces)
          (custom-set-faces `(,face ((t ,mods))))
          (push face space-macs--theming-modified-faces))))

    ;; Add new modifications
    (dolist (spec (append (cdr (assq theme theming-modifications))
                          (cdr (assq t theming-modifications))))
      (custom-set-faces `(,(car spec) ((t ,(cdr spec)))))
      (push (car spec) space-macs--theming-modified-faces))))

(defun space-macs/update-theme ()
  (interactive)
  (space-macs//theming space-macs--cur-theme))


