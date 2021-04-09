;;; funcs.el --- Windows-scripts Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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

(defun windows-scripts/bat-outline-setup ()
  "Select position by mouse and return to `bat-mode'."
  (local-set-key [mouse-1] (lambda () (interactive) (bat-mode) (beginning-of-line)))
  (local-set-key [return] 'bat-mode))

;;;###autoload
(defun windows-scripts/bat-outline ()
  "Navigate within Batch script using outline-mode."
  (interactive)
  (setq-local outline-regexp ":[^:]")
  (outline-mode)
  (hide-body)
  (define-key evil-normal-state-local-map (kbd "SPC m z") 'bat-mode))
