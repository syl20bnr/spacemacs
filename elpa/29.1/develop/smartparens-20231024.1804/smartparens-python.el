;;; smartparens-python.el --- Additional configuration for Python based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018, 2020, 2022-2023 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 8 February 2015
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Python based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-python)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;; If you have good ideas about what should be added please file an
;; issue on the github tracker.
;;
;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defgroup smartparens-python ()
  "smartparens settings for python-mode"
  :group 'smartparens)

(defcustom sp-python-insert-colon-in-function-definitions nil
  "If non-nil, auto-insert a colon after parens insertion in definition.

Supported definitions are:

- def
- async def
- class"
  :group 'smartparens-python
  :type  'boolean)

;; Python has no sexp suffices.  This fixes slurping
;; (|sys).path.append---the dot should not travel with the closing
;; paren
(--each '(python-mode python-ts-mode inferior-python-mode)
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(defun sp-python-maybe-add-colon-python (_id action _context)
  "Adds a colon after parentheses in a python definition.

Works for (async) def forms and class forms.

See also the option `sp-python-insert-colon-in-function-definitions'."
  ;; here, caret supposed to be in between parens, i.e. (|)
  (when (and sp-python-insert-colon-in-function-definitions
             (eq action 'insert)
             (looking-at ")\\s-*$")
             (save-excursion
               (goto-char (line-beginning-position))
               (re-search-forward
                (rx bol
                    (* (syntax whitespace))
                    (? "async")
                    (* (syntax whitespace))
                    (or "def" "class")
                    word-boundary)
                (line-end-position)
                t)))
    (save-excursion
      (forward-char) ;; skip closing paren
      (insert ":"))))

(sp-with-modes '(python-mode python-ts-mode)
  (sp-local-pair "'" "'" :unless '(sp-in-comment-p sp-in-string-quotes-p) :post-handlers '(:add sp-python-fix-tripple-quotes))
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
  (sp-local-pair "'''" "'''")
  (sp-local-pair "\\'" "\\'")
  (sp-local-pair "\"\"\"" "\"\"\"")
  (sp-local-pair "(" ")" :post-handlers '(:add sp-python-maybe-add-colon-python)))

(defun sp-python-fix-tripple-quotes (id action _context)
  "Properly rewrap tripple quote pairs.

When the user rewraps a tripple quote pair to the other pair
type (i.e. ''' to \") we check if the old pair was a
tripple-quote pair and if so add two pairs to beg/end of the
newly formed pair (which was a single-quote \"...\" pair)."
  (when (eq action 'rewrap-sexp)
    (let ((old (plist-get sp-handler-context :parent)))
      (when (or (and (equal old "'''") (equal id "\""))
                (and (equal old "\"\"\"") (equal id "'")))
        (save-excursion
          (sp-get sp-last-wrapped-region
            (goto-char :end-in)
            (insert (make-string 2 (aref id 0)))
            (goto-char :beg)
            (insert (make-string 2 (aref id 0)))))))))

(defadvice python-indent-dedent-line-backspace
    (around sp-backward-delete-char-advice activate)
  "Fix indend."
  (if smartparens-strict-mode
      (cl-letf (((symbol-function 'delete-backward-char)
                 (lambda (arg &optional killp)
                   (sp-backward-delete-char arg))))
        ad-do-it)
    ad-do-it))

(provide 'smartparens-python)
;;; smartparens-python.el ends here
