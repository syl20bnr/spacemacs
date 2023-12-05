;;; config.el --- Better Emacs Defaults Layer configuration variables File
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
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


(defvar better-defaults-move-to-beginning-of-code-first t
  "when t, first stroke of C-a will move the cursor to the beginning of code.
When nil, first stroke will go to the beginning of line.
Subsequent strokes will toggle between beginning of line and beginning of code.")

(defvar better-defaults-move-to-end-of-code-first nil
  "when t, first stroke of C-e will move the cursor to the end of code (before comments).
When nil, first stroke will go to the end of line (after comments).
Subsequent strokes will toggle between end of line and end of code.")
