;; citeproc-choose.el --- conditionally rendered CSL elements -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CSL supports conditional rendering via the cs:choose rendering element, which
;; can contain cs:if, cs:else-if and cs:else elements as children. This file
;; contains functions corresponding to all these elements with some auxiliary
;; functions.

;; In order to support conditional rendering by cs:choose, the functions
;; corresponding to cs:if, cs:else-if and cs:else (`citeproc--if',
;; `citeproc--else-if', `citeproc--else') return the generalized boolean value
;; of their condition in addition to their rendered content in the form of a
;; (BOOLEAN . CONTENT) pair.

;;; Code:

(require 's)

(require 'citeproc-lib)
(require 'citeproc-context)
(require 'citeproc-date)

(defun citeproc-choose-eval-conditions (attrs context)
  "Eval (possibly complex) boolean conditions in ATTRS."
  (-let* ((conditions (citeproc-choose--elementary-conditions
		       (--remove (eq (car it) 'match) attrs)))
	  (match (or (alist-get 'match attrs) "all"))
	  (values (--map (citeproc-choose--eval-elementary-condition (car it)
								     (intern (cdr it))
								     context)
			 conditions)))
    (pcase match
      ("all" (--all? it values))
      ("any" (--any? it values))
      ("none" (--none? it values)))))

(defun citeproc-choose--elementary-conditions (attrs)
  "Expand complex conditions in ATTRS into elementary ones.
Return a list of elementary (CONDITION-TYPE . PARAM) pairs."
  (cl-mapcan (lambda (x)
	       (--map (cons (car x) it)
		      (s-split " " (cdr x))))
	     attrs))

(defun citeproc-choose--eval-elementary-condition (type param context)
  "Evaluate an elementary choose condition of TYPE with PARAM.
TYPE is one of the symbols `variable', `type', `locator',
`is-numeric', `is-uncertain-date', `position' and `disambiguate'.
Return the result of evaluation, which is a generalized boolean."
  (pcase type
    ('variable (citeproc-var-value param context))
    ('type (string= param (citeproc-var-value 'type context)))
    ('locator (string= param (citeproc-var-value 'label context)))
    ('is-numeric (let ((val (citeproc-var-value param context)))
		   (citeproc-lib-numeric-p val)))
    ;; We return t iff the first date is uncertain. A more complicated alternative
    ;; would be to test the second date of date ranges as well.
    ('is-uncertain-date (-when-let (dates (citeproc-var-value param context))
			  (citeproc-date-circa (car dates))))
    ('position (and (eq (citeproc-context-mode context) 'cite)
		    (or (and (eq param 'near-note) (citeproc-var-value 'near-note context))
			(let ((pos (citeproc-var-value 'position context)))
			  (or (eq param pos)
			      (and (eq param 'subsequent)
				   (or (eq pos 'ibid) (eq pos 'ibid-with-locator)))
			      (and (eq param 'ibid)
				   (eq pos 'ibid-with-locator)))))))
    ('disambiguate (citeproc-var-value 'disambiguate context))))

(defmacro citeproc--choose (_attrs _context &rest body)
  "Return the content of the first element in BODY with t boolean value.
Return the empty (nil . `text-only') content if there is no such
element."
  `(let ((first-true
	  (--first (car it) (list ,@body))))
     (if first-true
	 (cdr first-true)
       (cons nil (quote text-only)))))

(defmacro citeproc--if (attrs context &rest body)
  "If conditions in ATTRS eval to t return t with rendered BODY.
Return nil otherwise."
  `(if (citeproc-choose-eval-conditions ,attrs ,context)
       (cons t (citeproc-lib-add-splice-tag
		(citeproc-lib-splice-into (list ,@body) 'splice)
		'splice))
     nil))

(defalias 'citeproc--else-if 'citeproc--if)

(defun citeproc--else (_attrs _context &rest body)
  "Always return t boolean plus rendered BODY"
  (let ((spliced-body (citeproc-lib-splice-into body 'splice)))
    (cons t (citeproc-lib-add-splice-tag spliced-body 'splice))))

(provide 'citeproc-choose)

;;; citeproc-choose.el ends here
