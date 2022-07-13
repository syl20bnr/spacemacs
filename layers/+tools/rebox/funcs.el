;;; funcs.el --- rebox layer function file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Christian E. Hopps <chopps@gmail.com>
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


(defun rebox-enable-hook ()
  "Until rebox is fixed for C style comments we have to disable"
  (and (not (or (eq major-mode 'c-mode)
                (eq major-mode 'c++-mode)
                (eq major-mode 'objc-mode)))
       (rebox-mode)))

(defun spacemacs/rebox-dwim-previous ()
  "Cycle to previous style in `rebox-style-loop'."
  (interactive)
  (rebox-dwim '(4)))
