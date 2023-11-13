;;; ob-fomus.el --- Org-babel functions for fomus evaluation

;; Copyright (C) 2011-2014, 2021 Torsten Anders

;; Author: Torsten Anders
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating Fomus source code.
;; For information on Fomus see http://fomus.sourceforge.net/
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in fomus
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:fomus
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a fomus source block.")

(defun org-babel-expand-body:fomus (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "\$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body))))
     vars)
    body))

(defun org-babel-execute:fomus (body params)
  "Execute a block of Fomus code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assq :result-params params)))
	 (out-file (cdr (assq :file params)))
	 (cmdline (cdr (assq :cmdline params)))
	 (cmd (or (cdr (assq :cmd params)) "fomus"))
	 (in-file (org-babel-temp-file "fomus-" ".fms")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:fomus body params)))
    ;; TMP: testing
    ;; (message (concat cmd
    ;; 	     " " (org-babel-process-file-name in-file)
    ;; 	     " " cmdline
    ;; 	     " -o " (org-babel-process-file-name out-file)))
    (org-babel-eval
     (concat cmd
	     " " (org-babel-process-file-name in-file)
	     " " cmdline
	     " -o " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:fomus (session params)
  "Return an error because Fomus does not support sessions."
  (error "Fomus does not support sessions"))

(provide 'ob-fomus)

;;; ob-fomus.el ends here
