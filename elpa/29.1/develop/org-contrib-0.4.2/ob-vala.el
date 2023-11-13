;;; ob-vala.el --- Babel functions for Vala -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 Free Software Foundation, Inc.

;; Author: Christian Garbs <mitch@cgarbs.de>
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

;; ob-vala.el provides Babel support for the Vala language
;; (see https://live.gnome.org/Vala for details)

;;; Requirements:

;; - Vala compiler binary (valac)
;; - Vala development environment (Vala libraries etc.)
;;
;; vala-mode.el is nice to have for code formatting, but is not needed
;; for ob-vala.el

;;; Code:

(require 'ob)
(require 'org-macs)

;; File extension.
(add-to-list 'org-babel-tangle-lang-exts '("vala" . "vala"))

;; Header arguments empty by default.
(defvar org-babel-default-header-args:vala '())

(defcustom org-babel-vala-compiler "valac"
  "Command used to compile a C source code file into an executable.
May be either a command in the path, like \"valac\"
or an absolute path name, like \"/usr/local/bin/valac\".
Parameters may be used like this: \"valac -v\""
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'string)

;; This is the main function which is called to evaluate a code
;; block.
;;
;; - run Vala compiler and create a binary in a temporary file
;;   - compiler/linker flags can be set via :flags header argument
;; - if compilation succeeded, run the binary
;;   - commandline parameters to the binary can be set via :cmdline
;;     header argument
;;   - stdout will be parsed as RESULT (control via :result-params
;;     header argument)
;;
;; There is no session support because Vala is a compiled language.
;;
;; This function is heavily based on ob-C.el
(defun org-babel-execute:vala (body params)
  "Execute a block of Vala code with Babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Vala source code block")
  (let* ((tmp-src-file (org-babel-temp-file
			"vala-src-"
			".vala"))
         (tmp-bin-file (org-babel-temp-file "vala-bin-" org-babel-exeext))
         (cmdline (cdr (assq :cmdline params)))
         (flags (cdr (assq :flags params))))
    (with-temp-file tmp-src-file (insert body))
    (org-babel-eval
     (format "%s %s -o %s %s"
	     org-babel-vala-compiler
	     (mapconcat #'identity
			(if (listp flags) flags (list flags)) " ")
	     (org-babel-process-file-name tmp-bin-file)
	     (org-babel-process-file-name tmp-src-file)) "")
    (when (file-executable-p tmp-bin-file)
	(let ((results
	       (org-trim
		(org-babel-eval
		 (concat tmp-bin-file (if cmdline (concat " " cmdline) "")) ""))))
	  (org-babel-reassemble-table
	   (org-babel-result-cond (cdr (assq :result-params params))
	     (org-babel-read results)
	     (let ((tmp-file (org-babel-temp-file "vala-")))
	       (with-temp-file tmp-file (insert results))
	       (org-babel-import-elisp-from-file tmp-file)))
	   (org-babel-pick-name
	    (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
	   (org-babel-pick-name
	    (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-prep-session:vala (_session _params)
  "Prepare a session.
This function does nothing as Vala is a compiled language with no
support for sessions."
  (error "Vala is a compiled language -- no support for sessions"))

(provide 'ob-vala)

;;; ob-vala.el ends here
