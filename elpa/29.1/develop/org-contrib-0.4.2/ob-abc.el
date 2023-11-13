;;; ob-abc.el --- Org Babel Functions for ABC -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

;; Author: William Waites
;; Maintainer: William Waites
;; Keywords: literate programming, music
;; Homepage: https://www.tardis.ed.ac.uk/~wwaites

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file adds support to Org Babel for music in ABC notation.
;;; It requires that the abcm2ps program is installed.
;;; See http://moinejf.free.fr/

;;; Code:

(require 'ob)

;; optionally define a file extension for this language

(add-to-list 'org-babel-tangle-lang-exts '("abc" . "abc"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:abc
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating an ABC source block.")

(defun org-babel-expand-body:abc (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "\\$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body))))
     vars)
    body))

(defun org-babel-execute:abc (body params)
  "Execute a block of ABC code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Abc source code block")
  (let* ((cmdline (cdr (assq :cmdline params)))
	 (out-file (let ((file (cdr (assq :file params))))
		     (if file (replace-regexp-in-string "\\.pdf$" ".ps" file)
		       (error "abc code block requires :file header argument"))))
	 (in-file (org-babel-temp-file "abc-"))
	 (render (concat "abcm2ps" " " cmdline
		      " -O " (org-babel-process-file-name out-file)
		      " " (org-babel-process-file-name in-file))))
    (with-temp-file in-file (insert (org-babel-expand-body:abc body params)))
    (org-babel-eval render "")
    ;;; handle where abcm2ps changes the file name (to support multiple files
    (when (or (string= (file-name-extension out-file) "eps")
	      (string= (file-name-extension out-file) "svg"))
      (rename-file (concat
		    (file-name-sans-extension out-file) "001."
		    (file-name-extension out-file))
		   out-file t))
    ;;; if we were asked for a pdf...
    (when (string= (file-name-extension (cdr (assq :file params))) "pdf")
      (org-babel-eval (concat "ps2pdf" " " out-file " " (cdr (assq :file params))) ""))
    ;;; indicate that the file has been written
    nil))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:abc (_session _params)
  "Return an error because abc does not support sessions."
  (error "ABC does not support sessions"))

(provide 'ob-abc)

;;; ob-abc.el ends here
