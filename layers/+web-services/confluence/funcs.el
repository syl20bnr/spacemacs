;;; funcs.el --- Confluence Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun spacemacs/confluence-save-to-confluence-minor-edit ()
  "Store a minor edit to the Confluence server."
  (interactive)
  (let ((confluence-save-page-minor-edits t))
    (cfln-save-page)))

(defun spacemacs/confluence-save-to-confluence-major-edit ()
  "Store a major edit to the Confluence server."
  (interactive)
  (let ((confluence-save-page-minor-edits nil))
    (cfln-save-page)))

(defun spacemacs//confluence-remove-save-hook ()
  "Remove the save to confluence functions from write hook."
  (remove-hook 'write-contents-hooks 'cfln-save-page))
