;;; load-env-vars.el --- Load environment variables from files                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jorge Dias

;; Author: Jorge Dias <jorge@mrdias.com>
;; URL: https://github.com/diasjorge/emacs-load-env-vars
;; Keywords: lisp
;; Version: 0.0.2
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you set environment variables loaded from a
;; file with bash style variable declarations.
;; Supported syntax:
;;
;; export KEY=VALUE
;; KEY=VALUE
;; KEY='VALUE'
;; KEY="VALUE"
;; # Comment lines are ignored
;; KEY=VALUE # Inline comments are ignored
;; KEY: VALUE
;;
;; Updates for Spacemacs:
;; - set `exec-path' from PATH

;;; Code:

(defvar load-env-vars-env-var-regexp
  (rx
   line-start
   (0+ space)
   (optional "export" (0+ space)) ;; optional export
   (group (1+ (in "_" alnum))) ;; key
   (or
    (and (0+ space) "=" (0+ space))
    (and ":" (1+ space))) ;; separator
   (or
    line-start
    (and "'" (group (0+ (or "\\'" (not (any "'"))))) "'") ;; single quoted value
    (and ?\" (group (0+ (or "\\\"" (not (any "\""))))) ?\") ;; double quoted value
    (group (1+ (not (in "#" "\n")))) ;; unquoted value
    )
   (0+ space)
   (optional "#" (0+ any))
   )
  "Regexp to match env vars in file."
  )

(defun load-env-vars-re-seq (regexp)
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list (match-string-no-properties 1) (or (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4))) matches))
        matches))))

(defun load-env-vars-extract-env-vars ()
  "Extract environment variable name and value from STRING."
  (load-env-vars-re-seq load-env-vars-env-var-regexp))

(defun load-env-vars-set-env (env-vars)
  "Set envariable variables from key value lists from ENV-VARS."
  (dolist (element env-vars)
    (let ((key (car element)) (value (cadr element)))
      (when (string-equal "PATH" key)
        (let ((paths (split-string value path-separator)))
          (dolist (p paths)
            (add-to-list 'exec-path p 'append))))
      (setenv key value))))

;;;###autoload
(defun load-env-vars (file-path)
  "Load environment variables found in FILE-PATH."
  (interactive "fEnvironment variables file: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((env-vars (load-env-vars-extract-env-vars)))
      (load-env-vars-set-env env-vars))))

(provide 'load-env-vars)
;;; load-env-vars.el ends here
