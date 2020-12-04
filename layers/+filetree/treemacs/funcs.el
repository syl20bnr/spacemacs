;;; funcs.el --- Tree-macs Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/tree-macs-project-toggle ()
  "Toggle and add the current project to tree-macs if not already added."
  (interactive)
  (if (eq (tree-macs-current-visibility) 'visible)
      (delete-window (tree-macs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
          (name (projectile-project-name)))
      (unless (tree-macs-current-workspace)
        (tree-macs--find-workspace))
      (tree-macs-do-add-project-to-workspace path name)
      (tree-macs-select-window))))

(defun space-macs/tree-macs-setup-width-lock ()
  "Setup the width lock of tree-macs buffer based on
`tree-macs-lock-width'."
  (interactive)
  (unless (eq (not tree-macs--width-is-locked)
              (not tree-macs-lock-width))
    (tree-macs-without-messages
     (tree-macs-toggle-fixed-width))))


