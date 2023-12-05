;;; funcs.el --- Confluence Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



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
