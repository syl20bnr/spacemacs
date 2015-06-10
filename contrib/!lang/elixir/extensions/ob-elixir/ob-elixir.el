;;; ob-elixir.el --- org-babel functions for elixir evaluation

;; Copyright (C) 2015 Victor Olinasc

;; URL: https://github.com/victorolinasc/ob-elixir
;; Author: Victor Olinasc
;; Keywords: literate programming, reproducible research
;; Package-Requires: ((emacs "24"))
;; Homepage: http://orgmode.org

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Org-mode language support for elixir. Currently this only supports
;; the external compilation and execution of elixir code blocks (i.e.,
;; no session support). This code is inspired by ob-java.el in org-mode
;; sources.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "exs"))

(defun org-babel-execute:elixir (body params)
  (let* ((src-file "orgmode_elixir_src.exs")
         (vars (org-babel-variable-assignments:elixir params))
	 (full-body (org-babel-expand-body:generic body params vars))
	 (results (progn (with-temp-file src-file (insert full-body))
                         (org-babel-eval
                          (concat "elixir" " " src-file) ""))))
    (org-babel-reassemble-table
     (org-babel-elixir-table-or-string results)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

;; Helpers, borrowed liberally from `ob-python'

(defun org-babel-variable-assignments:elixir (params)
  "Return a list of Elixir statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s = %s"
	     (car pair)
	     (org-babel-elixir-var-to-elixir (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-elixir-var-to-elixir (var)
  "Convert an elisp value to an Elixir variable.
Convert an elisp value, VAR, into a string of Elixir source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-elixir-var-to-elixir var ", ") "]")
    (if (equal var 'hline)
	"nil"  ; replace with variable?
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-elixir-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value."
  (org-babel-script-escape (org-babel-elixir-trim-string results)))

(defun org-babel-elixir-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(provide 'ob-elixir)
;;; ob-elixir.el ends here
