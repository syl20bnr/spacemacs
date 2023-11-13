;;; ob-shen.el --- Babel Functions for Shen          -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, shen
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

;; Currently this only works using session evaluation as there is no
;; defined method for executing shen code outside of a session.

;;; Requirements:

;; - shen-mode and inf-shen will soon be available through the GNU
;;   elpa, however in the interim they are available at
;;   https://github.com/eschulte/shen-mode

;;; Code:
(require 'ob)

(declare-function shen-eval-defun "ext:inf-shen" (&optional and-go))
(declare-function org-babel-ruby-var-to-ruby "ob-ruby" (var))

(defvar org-babel-default-header-args:shen '()
  "Default header arguments for shen code blocks.")

(defun org-babel-expand-body:shen (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (if (> (length vars) 0)
        (concat "(let "
                (mapconcat (lambda (var)
			     (format "%s %s" (car var)
				     (org-babel-shen-var-to-shen (cdr var))))
			   vars " ")
		body ")")
      body)))

(defun org-babel-shen-var-to-shen (var)
  "Convert VAR into a shen variable."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-ruby-var-to-ruby var " ") "]")
    (format "%S" var)))

(defun org-babel-execute:shen (body params)
  "Execute a block of Shen code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'inf-shen)
  (let* ((result-params (cdr (assq :result-params params)))
         (full-body (org-babel-expand-body:shen body params)))
    (let ((results
           (with-temp-buffer
             (insert full-body)
             (call-interactively #'shen-eval-defun))))
      (org-babel-result-cond result-params
        results
        (condition-case nil (org-babel-script-escape results)
          (error results))))))

(provide 'ob-shen)

;;; ob-shen.el ends here
