;;; ob-hledger.el --- Babel Functions for hledger      -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Free Software Foundation, Inc.

;; Author: Simon Michael
;; Keywords: literate programming, reproducible research, plain text accounting
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

;; Babel support for evaluating hledger entries.
;;
;; Based on ob-ledger.el.
;; If the source block is empty, hledger will use a default journal file,
;; probably ~/.hledger.journal (it may not notice your $LEDGER_FILE env var).
;; So make ~/.hledger.journal a symbolic link to the real file if necessary.

;; TODO Unit tests are more than welcome, too.

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:hledger
  '((:results . "output") (:exports . "results") (:cmdline . "bal"))
  "Default arguments to use when evaluating a hledger source block.")

(defun org-babel-execute:hledger (body params)
  "Execute a block of hledger entries with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing hledger source code block")
  (letrec ( ;(result-params (split-string (or (cdr (assq :results params)) "")))
	   (cmdline (cdr (assq :cmdline params)))
	   (in-file (org-babel-temp-file "hledger-"))
	   (out-file (org-babel-temp-file "hledger-output-"))
	   (hledgercmd (concat "hledger"
			       (if (> (length body) 0)
				   (concat " -f " (org-babel-process-file-name in-file))
				 "")
			       " " cmdline)))
    (with-temp-file in-file (insert body))
;; TODO This is calling for some refactoring:
;;  (concat "hledger" (if ...) " " cmdline)
;; could be built only once and bound to a symbol.
    (message "%s" hledgercmd)
    (with-output-to-string
      (shell-command (concat hledgercmd " > " (org-babel-process-file-name out-file))))
    (with-temp-buffer (insert-file-contents out-file) (buffer-string))))

(defun org-babel-prep-session:hledger (_session _params)
  (error "hledger does not support sessions"))

(provide 'ob-hledger)

;;; ob-hledger.el ends here
