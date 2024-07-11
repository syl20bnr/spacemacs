;;; funcs.el --- reasonml layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Dave Aitken <dave.aitken@gmail.com>
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


(defun reason/rtop-prompt ()
  "The rtop prompt function."
  (let ((prompt (format "rtop[%d]> " utop-command-number)))
    (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
    prompt))

(defun reason/refmt-re-to-ml ()
  (interactive)
  (if (use-region-p)
      (apply-refmt (region-beginning) (region-end) "re" "ml")
    (apply-refmt nil nil "re" "ml")))

(defun reason/refmt-ml-to-re ()
  (interactive)
  (if (use-region-p)
      (apply-refmt (region-beginning) (region-end) "ml" "re")
    (apply-refmt nil nil "ml" "re")))
