;;; ob-mathomatic.el --- Org-babel functions for mathomatic evaluation

;; Copyright (C) 2009-2021  Free Software Foundation, Inc.

;; Author: Eric S Fraga
;;	Eric Schulte
;;  Luis Anaya (Mathomatic)

;; Keywords: literate programming, reproducible research, mathomatic
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

;; Org-Babel support for evaluating mathomatic entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in mathomatic
;;
;; 2) we are adding the "cmdline" header argument

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("mathomatic" . "math"))

(defvar org-babel-default-header-args:mathomatic '())

(defcustom org-babel-mathomatic-command
  (if (boundp 'mathomatic-command) mathomatic-command "mathomatic")
  "Command used to call mathomatic on the shell."
  :group 'org-babel)

(defun org-babel-mathomatic-expand (body params)
  "Expand a block of Mathomatic code according to its header arguments."
  (let ((vars (org-babel--get-vars params)))
    (mapconcat 'identity
	       (list
		;; graphic output
		(let ((graphic-file (org-babel-mathomatic-graphical-output-file params)))
		  (if graphic-file
		      (cond
		       ((string-match ".\.eps$" graphic-file)
			(format ;; Need to add command to send to file.
			 "set plot set terminal postscript eps\\;set output %S "
			 graphic-file))
		       ((string-match ".\.ps$" graphic-file)
			(format ;; Need to add command to send to file.
			 "set plot set terminal postscript\\;set output %S "
			 graphic-file))

		       ((string-match ".\.pic$" graphic-file)
			(format ;; Need to add command to send to file.
			 "set plot set terminal gpic\\;set output %S "
			 graphic-file))
		       (t
			(format ;; Need to add command to send to file.
			 "set plot set terminal png\\;set output %S "
			 graphic-file)))
		    ""))
		;; variables
		(mapconcat 'org-babel-mathomatic-var-to-mathomatic vars "\n")
		;; body
		body
		"")
	       "\n")))

(defun org-babel-execute:mathomatic (body params)
  "Execute a block of Mathomatic entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Mathomatic source code block")
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(result
	 (let* ((cmdline (or (cdr (assq :cmdline params)) ""))
		(in-file (org-babel-temp-file "mathomatic-" ".math"))
		(cmd (format "%s -t -c -q  %s %s"
			     org-babel-mathomatic-command in-file cmdline)))
	   (with-temp-file in-file (insert (org-babel-mathomatic-expand body params)))
	   (message cmd)
	   ((lambda (raw) ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
	      (mapconcat
	       #'identity
	       (delq nil
		     (mapcar (lambda (line)
			       (unless (or (string-match "batch" line)
					   (string-match "^rat: replaced .*$" line)
					   (= 0 (length line)))
				 line))
			     (split-string raw "[\r\n]"))) "\n"))
	    (org-babel-eval cmd "")))))
    (if (org-babel-mathomatic-graphical-output-file params)
	nil
      (if (or (member "scalar" result-params)
	      (member "verbatim" result-params)
	      (member "output" result-params))
	  result
	(let ((tmp-file (org-babel-temp-file "mathomatic-res-")))
	  (with-temp-file tmp-file (insert result))
	  (org-babel-import-elisp-from-file tmp-file))))))

(defun org-babel-prep-session:mathomatic (session params)
  (error "Mathomatic does not support sessions"))

(defun org-babel-mathomatic-var-to-mathomatic (pair)
  "Convert an elisp val into a string of mathomatic code specifying a var
of the same value."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
      (format "%s=%s" var
	      (org-babel-mathomatic-elisp-to-mathomatic val))))

(defun org-babel-mathomatic-graphical-output-file (params)
  "Name of file to which mathomatic should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defun org-babel-mathomatic-elisp-to-mathomatic (val)
  "Return a string of mathomatic code which evaluates to VAL."
  (if (listp val)
      (mapconcat #'org-babel-mathomatic-elisp-to-mathomatic val " ")
    (format "%s" val)))

(provide 'ob-mathomatic)

;;; ob-mathomatic.el ends here
