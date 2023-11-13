;; citeproc-macro.el --- functions to render CSL macros -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions to render the output of CSL macros.

;;; Code:

(require 'citeproc-lib)
(require 'citeproc-rt)
(require 'citeproc-context)

;;; For macro evaluation
(defun citeproc--macro (attrs context &rest body)
  "Render the content of a macro element with ATTRS and BODY."
  (let* ((spliced-body (citeproc-lib-splice-into body 'splice))
	 (val (citeproc-rt-typed-join attrs spliced-body context)))
    (if (eq 'empty-vars (cdr val))
	(cons nil 'text-only)
      val)))

(defun citeproc-macro-output (macro context)
  "Return the output of MACRO.
MACRO is the macro's name as a string and the returned value is a
(RICH-TEXT-CONTENT . CONTENT-TYPE) cons cell."
  (let ((macro-fun (assoc-default macro (citeproc-context-macros context))))
    (if macro-fun
	(funcall macro-fun context)
      (error "There is no macro called `%s' in style" macro))))

(defun citeproc-macro-output-as-text (macro context)
  "Return the output of MACRO as plain text.
MACRO is the macro's name as a string."
  (citeproc-rt-to-plain (citeproc-rt-render-affixes
			 (car (citeproc-macro-output macro context)))))

(provide 'citeproc-macro)

;;; citeproc-macro.el ends here
