;;; funcs.el --- Asm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
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


(defun asm-generic-setup ()
  (setq indent-tabs-mode nil) ; use spaces to indent
  (setq tab-stop-list (number-sequence 2 60 2))) ; 2 spaces per tab

(defun asm-electric-indent-local-mode-off ()
  (electric-indent-local-mode -1))

;; when we press ':' character, it runs `asm-colon' command in asm-mode.
;; The command automatically removes the indentation ofcurrent line, since
;; every non-whitespace character before a colon is a label in asm and
;; label has to be at the beginning of a line. However, the problem is
;; that when deleting indentation, trailing spaces are left between the
;; colon and the original point the colon was inserted.
;;
;; These functions solve that problem. First, check whether we have any
;; space or tab after point. If so, don't do anything because the spaces are
;; there intentionally. If not, we delete all trailing spaces between
;; point and colon.
(defvar asm-colon-has-space nil)
(defun asm-colon-check-space ()
  (setq asm-colon-has-space nil)
  (when (and (not (null (char-after)))
             (member (string (char-after)) '(" " "\t")))
    (setq asm-colon-has-space t)))
(defun asm-colon-delete-spaces ()
  (unless asm-colon-has-space
    (call-interactively 'delete-horizontal-space)))
(advice-add 'asm-colon :before 'asm-colon-check-space)
(advice-add 'asm-colon :after 'asm-colon-delete-spaces)
