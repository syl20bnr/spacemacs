;;; ob-ledger.el --- Babel Functions for Ledger      -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Free Software Foundation, Inc.

;; Author: Eric S Fraga
;; Maintainer: Eric S Fraga
;; Keywords: literate programming, reproducible research, accounting
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

;; Org-Babel support for evaluating ledger entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ledger
;;
;; 2) we are generally only going to return output from the ledger program
;;
;; 3) we are adding the "cmdline" header argument
;;
;; 4) there are no variables

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:ledger
  '((:results . "output") (:cmdline . "bal"))
  "Default arguments to use when evaluating a ledger source block.")

(defun org-babel-execute:ledger (body params)
  "Execute a block of Ledger entries with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Ledger source code block")
  (let ((cmdline (cdr (assq :cmdline params)))
        (in-file (org-babel-temp-file "ledger-"))
	(out-file (org-babel-temp-file "ledger-output-")))
    (with-temp-file in-file (insert body))
    (message "%s" (concat "ledger"
			  " -f " (org-babel-process-file-name in-file)
			  " " cmdline))
    (with-output-to-string
      (shell-command (concat "ledger"
			     " -f " (org-babel-process-file-name in-file)
			     " " cmdline
			     " > " (org-babel-process-file-name out-file))))
    (with-temp-buffer (insert-file-contents out-file) (buffer-string))))

(defun org-babel-prep-session:ledger (_session _params)
  (error "Ledger does not support sessions"))

(provide 'ob-ledger)

;;; ob-ledger.el ends here
