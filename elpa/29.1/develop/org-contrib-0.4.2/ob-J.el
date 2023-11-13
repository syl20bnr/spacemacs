;;; ob-J.el --- Babel Functions for J                -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; Maintainer: Joseph Novakovich <josephnovakovich@gmail.com>
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

;; Org-Babel support for evaluating J code.
;;
;; Session interaction depends on `j-console' from package `j-mode'
;; (available in MELPA).

;;; Code:

(require 'ob)
(require 'org-macs)

(declare-function j-console-ensure-session "ext:j-console" ())

(defcustom org-babel-J-command "jconsole"
  "Command to call J."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-babel-expand-body:J (body _params &optional _processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
PROCESSED-PARAMS isn't used yet."
  (org-babel-J-interleave-echos-except-functions body))

(defun org-babel-J-interleave-echos (body)
  "Interleave echo',' between each source line of BODY."
  (mapconcat #'identity (split-string body "\n") "\necho','\n"))

(defun org-babel-J-interleave-echos-except-functions (body)
  "Interleave echo',' between source lines of BODY that aren't functions."
  (if (obj-string-match-m "\\(?:^\\|\n\\)[^\n]*\\(?:0\\|1\\|2\\|3\\|4\\|dyad\\) : 0\n.*\n)\\(?:\n\\|$\\)" body)
      (let ((s1 (substring body 0 (match-beginning 0)))
	    (s2 (match-string 0 body))
	    (s3 (substring body (match-end 0))))
	(concat
	 (if (string= s1 "")
	     ""
	   (concat (org-babel-J-interleave-echos s1)
		   "\necho','\n"))
	 s2
	 "\necho','\n"
	 (org-babel-J-interleave-echos-except-functions s3)))
    (org-babel-J-interleave-echos body)))

(defalias 'org-babel-execute:j 'org-babel-execute:J)

(defun org-babel-execute:J (body params)
  "Execute a block of J code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing J source code block")
  (let* ((processed-params (org-babel-process-params params))
	 (sessionp (cdr (assq :session params)))
	 (sit-time (let ((sit (assq :sit params)))
		     (if sit (cdr sit) .1)))
         (full-body (org-babel-expand-body:J
                     body params processed-params))
	 (tmp-script-file (org-babel-temp-file "J-src")))
    (org-babel-j-initiate-session sessionp)
    (org-babel-J-strip-whitespace
     (if (string= sessionp "none")
	 (progn
	   (with-temp-file tmp-script-file
	     (insert full-body))
	   (org-babel-eval (format "%s < %s" org-babel-J-command tmp-script-file) ""))
       (org-babel-J-eval-string full-body sit-time)))))

(defun org-babel-J-eval-string (str sit-time)
  "Sends STR to the `j-console-cmd' session and execute it."
  (let ((session (j-console-ensure-session)))
    (with-current-buffer (process-buffer session)
      (goto-char (point-max))
      (insert (format "\n%s\n" str))
      (let ((beg (point)))
	(comint-send-input)
	(sit-for sit-time)
	(buffer-substring-no-properties
	 beg (point-max))))))

(defun org-babel-J-strip-whitespace (str)
  "Remove whitespace from jconsole output STR."
  (mapconcat
   #'identity
   (delete "" (mapcar
	       #'org-babel-J-print-block
	       (split-string str "^ *,\n" t)))
   "\n\n"))

(defun obj-get-string-alignment (str)
  "Return a number to describe STR alignment.
STR represents a table.
Positive/negative/zero result means right/left/undetermined.
Don't trust first line."
  (let* ((str (org-trim str))
	 (lines (split-string str "\n" t))
	 n1 n2)
    (cond ((<= (length lines) 1)
	   0)
	  ((= (length lines) 2)
	   ;; numbers are right-aligned
	   (if (and
		(numberp (read (car lines)))
		(numberp (read (cadr lines)))
		(setq n1 (obj-match-second-space-right (nth 0 lines)))
		(setq n2 (obj-match-second-space-right (nth 1 lines))))
	       n2
	     0))
	  ((not (obj-match-second-space-left (nth 0 lines)))
	   0)
	  ((and
	    (setq n1 (obj-match-second-space-left (nth 1 lines)))
	    (setq n2 (obj-match-second-space-left (nth 2 lines)))
	    (= n1 n2))
	   n1)
	  ((and
	    (setq n1 (obj-match-second-space-right (nth 1 lines)))
	    (setq n2 (obj-match-second-space-right (nth 2 lines)))
	    (= n1 n2))
	   (- n1))
	  (t 0))))

(defun org-babel-J-print-block (x)
  "Prettify jconsole output X."
  (let* ((x (org-trim x))
	 (a (obj-get-string-alignment x))
	 (lines (split-string x "\n" t))
	 b)
    (cond ((< a 0)
	   (setq b (obj-match-second-space-right (nth 0 lines)))
	   (concat (make-string (+ a b) ? ) x))
	  ((> a 0)
	   (setq b (obj-match-second-space-left (nth 0 lines)))
	   (concat (make-string (- a b) ? ) x))
	  (t x))))

(defun obj-match-second-space-left (s)
  "Return position of leftmost space in second space block of S or nil."
  (and (string-match "^ *[^ ]+\\( \\)" s)
       (match-beginning 1)))

(defun obj-match-second-space-right (s)
  "Return position of rightmost space in second space block of S or nil."
  (and (string-match "^ *[^ ]+ *\\( \\)[^ ]" s)
       (match-beginning 1)))

(defun obj-string-match-m (regexp string &optional start)
  "Call (string-match REGEXP STRING START).
REGEXP is modified so that .* matches newlines as well."
  (string-match
   (replace-regexp-in-string "\\.\\*" "[\0-\377[:nonascii:]]*" regexp)
   string
   start))

(defun org-babel-j-initiate-session (&optional session)
  "Initiate a J session.
SESSION is a parameter given by org-babel."
  (unless (string= session "none")
    (require 'j-console)
    (j-console-ensure-session)))

(provide 'ob-J)

;;; ob-J.el ends here
