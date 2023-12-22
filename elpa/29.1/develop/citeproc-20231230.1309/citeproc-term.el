;;; citeproc-term.el --- functions for term localization -*- lexical-binding: t; -*-

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

;; Functions for localizing the terms of a CSL style according to a locale.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'let-alist)

(require 'citeproc-lib)
(require 'citeproc-s)

(cl-defstruct (citeproc-term (:constructor citeproc-term--create))
  "A struct representing a localized term."
  (name nil) (form nil) (number nil) (gender nil) (gender-form nil)
  (match nil) (text nil))

(defun citeproc-term--compare (t1 t2)
  "Compare terms T1 and T2.
The comparison is based on the term fields except the last one,
and relies on the alphabetical ordering of fields' string
content (see the function `citeproc-lib-s-content'). Return 1, -1
or 0 iff T1 precedes, succeeds or is equal according to the
ordering."
  (cond ((not t2) 1)
	((not t1) -1)
	(t (let ((idx 1)
		 (result 0))
	     (while (and (= result 0) (< idx 7))
	       (let ((s1 (citeproc-s-content (aref t1 idx)))
		     (s2 (citeproc-s-content (aref t2 idx))))
		 (cond ((string< s1 s2) (setq result 1))
		       ((string> s1 s2) (setq result -1))))
	       (cl-incf idx)) result))))

(defun citeproc-term-list--sort (tl)
  "Sort termlist TL in place using `citeproc-term--compare'."
  (cl-sort tl (lambda (x y) (> (citeproc-term--compare x y) -1))))

(defun citeproc-term-list-update (tl1 tl2 &optional sorted-input)
  "Return a term list which is TL1 updated with term list TL2.
TL1 and TL2 are list of citeproc-term structs. The order of terms
in the returned term list is undetermined. If the optional
SORTED-INPUT is non-nil then the term lists are supposed to be
already sorted according to `citeproc-term--compare', otherwise
they are sorted in-place."
  (let (result)
    (unless sorted-input
      (setq tl1 (citeproc-term-list--sort tl1)
	    tl2 (citeproc-term-list--sort tl2)))
    (while (or tl1 tl2)
      (let* ((t1 (car tl1))
	     (t2 (car tl2))
	     (cmp (citeproc-term--compare t1 t2)))
	(cond ((= cmp 1) (push t1 result) (pop tl1))
	      ((= cmp -1) (push t2 result) (pop tl2))
	      (t (push t2 result) (pop tl1) (pop tl2)))))
    result))

(defun citeproc-term--from-xml-frag (frag)
  "Transform xml FRAG representing a term into a citeproc-term struct."
  (let-alist (car frag)
    (-let* ((.form (or .form "long"))
	    (term (citeproc-term--create
		   :name .name
		   :form (citeproc-lib-intern .form)
		   :gender (citeproc-lib-intern .gender)
		   :match (citeproc-lib-intern .match)
		   :gender-form (citeproc-lib-intern .gender-form))))
      (if (= (length frag) 2)
	  (progn
	    (setf (citeproc-term-text term) (cadr frag))
	    (list term))
	(setf (citeproc-term-text term) (cl-caddr (cadr frag)))
	(setf (citeproc-term-number term) 'single)
	(let ((multi-term (copy-citeproc-term term)))
	  (setf (citeproc-term-text multi-term) (cl-caddr (cl-caddr frag)))
	  (setf (citeproc-term-number multi-term) 'multiple)
	  (list term multi-term))))))

(defun citeproc-term-text-from-terms (term terms)
  "Return the first text associated with TERM in TERMS.
Return nil if TERM is not in TERMS."
  (-if-let (match (--first (string= term (citeproc-term-name it))
			   terms))
      (citeproc-term-text match)
    nil))

(provide 'citeproc-term)

;;; citeproc-term.el ends here
