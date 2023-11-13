;;; citeproc-number.el --- render CSL number elements -*- lexical-binding: t; -*-

;; Copyright "(C) 2017 András Simonyi

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

;; Functions to render CSL number elements.

;;; Code:

(require 'subr-x)
(require 'rst)
(require 's)

(require 'citeproc-context)
(require 'citeproc-lib)
(require 'citeproc-s)
(require 'citeproc-rt)

(defun citeproc-number-extract (val)
  "Return the parse of string VAL holding numeric content."
  (cdr (s-match
	"\\`\\([[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\)\\(?:\\(?: *\\([,&-–—―]\\|--\\) *\\)\\([[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\)\\)?\\'"
	val)))

(defun citeproc--number (attrs context &rest _body)
  "Render a cs:number element with the given ATTRS in CONTEXT."
  (-let* (((&alist 'variable var
		   'form form)
	   attrs)
	  (variable (intern var))
	  (value (citeproc-var-value variable context))
	  (form (citeproc-lib-intern form)))
    (if (not value)
	(cons nil 'empty-var)
      (let ((result-attrs
	     (cons `(rendered-var . ,variable) attrs))
	    (result-cont
	     (citeproc-number-var-value value variable form context)))
	(cons (citeproc-rt-format-single result-attrs result-cont context)
	      'present-var)))))

(defconst citeproc--format-numsep-alist
  '(("&" . " & ")
    ("," . ", ")
    ("-" . "-")
    ("--" . "-")
    ("—" . "-")
    ("―" . "-"))
  "Alist specifying the proper formatting of number-pair separators.")

(defun citeproc-number-var-value (value variable form context)
  "Return the numeric VALUE of VARIABLE formatted in FORM.
VARIABLE is a symbol."
  (cond ((not value) nil)
	((numberp value) (number-to-string value))
	(t (--if-let (citeproc-number-extract value)
	       (let ((formatted-first
		      (citeproc-number--format (car it) form variable context)))
		 (if (> (length it) 1)
		     (concat
		      formatted-first
		      (assoc-default (cadr it) citeproc--format-numsep-alist)
		      (citeproc-number--format (cl-caddr it) form variable context))
		   formatted-first))
	     value))))

(defun citeproc-number--format (s form term context)
  "Render the number in string S for TERM in format FORM."
  (if (s-matches-p "[[:alpha:]]" s) s
    (pcase form
      ('roman (downcase (rst-arabic-to-roman (string-to-number s))))
      ('ordinal (citeproc-number--format-as-ordinal s term context))
      ('long-ordinal (citeproc-number--format-as-long-ordinal s term context))
      (_ s))))

(defun citeproc-number--format-as-ordinal (s term context)
  "Format numeric string S as ordinal agreeing with TERM."
  (let* ((terms (citeproc-context-terms context))
	 (padded (if (= 1 (length s))
		     (concat "0" s)
		   s))
	 (gender (citeproc-term-get-gender term context))
	 (matches
	  (--filter (and (string= (s-left 8 (citeproc-term-name it)) "ordinal-")
			 (citeproc-number--ordinal-matches-p padded gender it))
		    terms))
	 (suffix
	  (citeproc-term-text
	   (if (not matches)
	       (-when-let (ordinal-matches
			   (--filter (string= (citeproc-term-name it) "ordinal")
				     terms))
		 (-if-let (match (--first (eq (citeproc-term-gender-form it) gender)
					  ordinal-matches))
		     match
		   (car ordinal-matches)))
	     (let ((first-term (car matches)))
	       (-if-let (second-term (cadr matches))
		   (if (= (elt (citeproc-term-name first-term) 8) ?0)
		       second-term
		     first-term)
		 first-term))))))
    (concat s suffix)))

(defconst citeproc-number--ordinal-match-alist
  '((last-two-digits . 2)
    (last-digit . 1)
    (whole-number . nil))
  "Alist mapping ordinal match attributes to the required match lengths.")

(defun citeproc-number--ordinal-matches-p (s gender ord-term)
  "Whether S term with GENDER matches ordinal-term ORD-TERM."
  (and (eq gender (citeproc-term-gender-form ord-term))
       (let ((match (citeproc-term-match ord-term))
	     (term-num (s-right 2 (citeproc-term-name ord-term))))
	 (unless match
	   (setq match
		 (if (= (elt (citeproc-term-name ord-term) 8) ?0)
		     'last-digit
		   'last-two-digits)))
	 (let ((l (assoc-default match citeproc-number--ordinal-match-alist)))
	   (string= (citeproc-s-tail s l)
		    (citeproc-s-tail term-num l))))))

(defun citeproc-number--format-as-long-ordinal (s term context)
  "Format numeric string S as a long ordinal agreeing with TERM."
  (let ((num-val (string-to-number s)))
    (if (> num-val 10)
	(citeproc-number--format-as-ordinal s term context)
      (when (= 1 (length s)) (setq s (concat "0" s)))
      (let* ((name (concat "long-ordinal-" s))
	     (gender (citeproc-term-get-gender term context))
	     (match (--first (and (string= (citeproc-term-name it) name)
				  (eq (citeproc-term-gender-form it) gender))
			     (citeproc-context-terms context))))
	(if match
	    (citeproc-term-text match)
	  (citeproc-term-get-text name context))))))

(provide 'citeproc-number)

;;; citeproc-number ends here
