;;; funcs.el --- Confluence Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defun space-macs/confluence-save-to-confluence-minor-edit ()
  "Store a minor edit to the Confluence server."
  (interactive)
  (let ((confluence-save-page-minor-edits t))
    (cfln-save-page)))

(defun space-macs/confluence-save-to-confluence-major-edit ()
  "Store a major edit to the Confluence server."
  (interactive)
  (let ((confluence-save-page-minor-edits nil))
    (cfln-save-page)))

(defun space-macs//confluence-remove-save-hook ()
  "Remove the save to confluence functions from write hook."
  (remove-hook 'write-contents-hooks 'cfln-save-page))


