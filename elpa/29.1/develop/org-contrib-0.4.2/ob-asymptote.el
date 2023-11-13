;;; ob-asymptote.el --- Babel Functions for Asymptote -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2021 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Jarmo Hurri <jarmo.hurri@iki.fi>
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib

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

;; Org-Babel support for evaluating asymptote source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in asymptote
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments, if file
;;    is omitted then the -V option is passed to the asy command for
;;    interactive viewing

;;; Requirements:

;; - The asymptote program :: http://asymptote.sourceforge.net/
;;
;; - asy-mode :: Major mode for editing asymptote files

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("asymptote" . "asy"))

(defvar org-babel-default-header-args:asymptote
  '((:results . "file") (:exports . "results"))
  "Default arguments when evaluating an Asymptote source block.")

(defun org-babel-execute:asymptote (body params)
  "Execute a block of Asymptote code.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (assq :file params)))
         (format (or (file-name-extension out-file)
                     "pdf"))
         (cmdline (cdr (assq :cmdline params)))
         (in-file (org-babel-temp-file "asymptote-"))
         (cmd
	  (concat "asy "
		  (if out-file
		      (concat
		       "-globalwrite -f " format
		       " -o " (org-babel-process-file-name out-file))
		    "-V")
		  " " cmdline
		  " " (org-babel-process-file-name in-file))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:generic
	       body params
	       (org-babel-variable-assignments:asymptote params))))
    (message cmd) (shell-command cmd)
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:asymptote (_session _params)
  "Return an error if the :session header argument is set.
Asymptote does not support sessions."
  (error "Asymptote does not support sessions"))

(defun org-babel-variable-assignments:asymptote (params)
  "Return list of asymptote statements assigning the block's variables."
  (mapcar #'org-babel-asymptote-var-to-asymptote
	  (org-babel--get-vars params)))

(defun org-babel-asymptote-var-to-asymptote (pair)
  "Convert an elisp value into an Asymptote variable.
The elisp value PAIR is converted into Asymptote code specifying
a variable of the same value."
  (let ((var (car pair))
        (val (let ((v (cdr pair)))
	       (if (symbolp v) (symbol-name v) v))))
    (cond
     ((integerp val)
      (format "int %S=%S;" var val))
     ((floatp val)
      (format "real %S=%S;" var val))
     ((stringp val)
      (format "string %S=\"%s\";" var val))
     ((and (listp val) (not (listp (car val))))
      (let* ((type (org-babel-asymptote-define-type val))
	     (fmt (if (eq 'string type) "\"%s\"" "%s"))
	     (vect (mapconcat (lambda (e) (format fmt e)) val ", ")))
	(format "%s[] %S={%s};" type var vect)))
     ((listp val)
      (let* ((type (org-babel-asymptote-define-type val))
	     (fmt (if (eq 'string type) "\"%s\"" "%s"))
             (array (mapconcat (lambda (row)
				 (concat "{"
					 (mapconcat (lambda (e) (format fmt e))
						    row ", ")
					 "}"))
			       val ",")))
        (format "%S[][] %S={%s};" type var array))))))

(defun org-babel-asymptote-define-type (data)
  "Determine type of DATA.

DATA is a list.  Return type as a symbol.

The type is `string' if any element in DATA is a string.
Otherwise, it is either `real', if some elements are floats, or
`int'."
  (letrec ((type 'int)
	   (find-type
	    (lambda (row)
	      (dolist (e row type)
		(cond ((listp e) (setq type (funcall find-type e)))
		      ((stringp e) (throw 'exit 'string))
		      ((floatp e) (setq type 'real)))))))
    (catch 'exit (funcall find-type data)) type))

(provide 'ob-asymptote)

;;; ob-asymptote.el ends here
