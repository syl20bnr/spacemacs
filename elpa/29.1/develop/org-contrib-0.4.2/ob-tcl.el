;;; ob-tcl.el --- Org-babel functions for tcl evaluation

;; Copyright (C) 2009-2021  Free Software Foundation, Inc.

;; Authors: Dan Davison
;;	 Eric Schulte
;;   Luis Anaya (tcl)
;;
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating tcl source code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("tcl" . "tcl"))

(defvar org-babel-default-header-args:tcl nil)

(defcustom org-babel-tcl-command "tclsh"
"Name of command to use for executing Tcl code."
  :group 'org-babel
  :type 'string)


(defun org-babel-execute:tcl (body params)
  "Execute a block of Tcl code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assq :session params)))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:tcl params)))
	(session (org-babel-tcl-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-tcl-evaluate session full-body result-type)
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-prep-session:tcl (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Tcl"))

(defun org-babel-variable-assignments:tcl (params)
  "Return list of tcl statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "set %s %s"
	     (car pair)
	     (org-babel-tcl-var-to-tcl (cdr pair))))
   (org-babel--get-vars params)))

;; helper functions

(defun org-babel-tcl-var-to-tcl (var)
  "Convert an elisp value to a tcl variable.
The elisp value, VAR, is converted to a string of tcl source code
specifying a var of the same value."
  (if (listp var)
      (concat "{" (mapconcat #'org-babel-tcl-var-to-tcl var "  ") "}")
    (format "%s" var)))

(defvar org-babel-tcl-buffers '(:default . nil))

(defun org-babel-tcl-initiate-session (&optional session params)
  "Return nil because sessions are not supported by tcl."
nil)

(defvar org-babel-tcl-wrapper-method
  "
proc main {} {
   %s
}

set r [eval main]
set o [open \"%s\" \"w\"];
puts $o $r
flush $o
close $o

")

(defvar org-babel-tcl-pp-wrapper-method
  nil)

(defun org-babel-tcl-evaluate (session body &optional result-type)
  "Pass BODY to the Tcl process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Tcl"))
  (case result-type
    (output (org-babel-eval org-babel-tcl-command body))
    (value (let ((tmp-file (org-babel-temp-file "tcl-")))
             (org-babel-eval
              org-babel-tcl-command
              (format org-babel-tcl-wrapper-method body
                      (org-babel-process-file-name tmp-file 'noquote)))
             (org-babel-eval-read-file tmp-file)))))

(provide 'ob-tcl)



;;; ob-tcl.el ends here
