;;; ob-io.el --- Babel Functions for Io              -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.

;; Author: Andrzej Lichnerowicz
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
;; Currently only supports the external execution.  No session support yet.
;; :results output -- runs in scripting mode
;; :results output repl -- runs in repl mode

;;; Requirements:
;; - Io language :: https://iolanguage.org/
;; - Io major mode :: Can be installed from Io sources
;;  https://github.com/stevedekorte/io/blob/master/extras/SyntaxHighlighters/Emacs/io-mode.el

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts) ;; Autoloaded
(add-to-list 'org-babel-tangle-lang-exts '("io" . "io"))
(defvar org-babel-default-header-args:io '())
(defvar org-babel-io-command "io"
  "Name of the command to use for executing Io code.")

(defun org-babel-execute:io (body params)
  "Execute a block of Io code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Io source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-io-initiate-session (nth 0 processed-params)))
         (result-params (nth 2 processed-params))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params))
         (result (org-babel-io-evaluate
                  session full-body result-type result-params)))

    (org-babel-reassemble-table
     result
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defvar org-babel-io-wrapper-method
  "(
%s
) asString print
")


(defun org-babel-io-evaluate (session body &optional result-type result-params)
  "Evaluate BODY in external Io process.
If RESULT-TYPE equals `output' then return standard output as a string.
If RESULT-TYPE equals `value' then return the value of the last statement
in BODY as elisp."
  (when session (error "Sessions are not (yet) supported for Io"))
  (pcase result-type
    (`output
     (if (member "repl" result-params)
         (org-babel-eval org-babel-io-command body)
       (let ((src-file (org-babel-temp-file "io-")))
         (progn (with-temp-file src-file (insert body))
                (org-babel-eval
                 (concat org-babel-io-command " " src-file) "")))))
    (`value (let* ((src-file (org-babel-temp-file "io-"))
		   (wrapper (format org-babel-io-wrapper-method body)))
	      (with-temp-file src-file (insert wrapper))
	      (let ((raw (org-babel-eval
			  (concat org-babel-io-command " " src-file) "")))
		(org-babel-result-cond result-params
		  raw
		  (org-babel-script-escape raw)))))))

(defun org-babel-prep-session:io (_session _params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not (yet) supported for Io"))

(defun org-babel-io-initiate-session (&optional _session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session.  Sessions are not
supported in Io."
  nil)

(provide 'ob-io)

;;; ob-io.el ends here
