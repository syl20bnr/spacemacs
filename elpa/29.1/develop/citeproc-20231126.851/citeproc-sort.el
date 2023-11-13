;;; citeproc-sort.el --- cite and bibliography sorting  -*- lexical-binding: t; -*-

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

;; Functions to sort cites and bibliography items.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)
(require 's)

(require 'citeproc-lib)
(require 'citeproc-s)
(require 'citeproc-rt)
(require 'citeproc-macro)
(require 'citeproc-proc)
(require 'citeproc-name)

(defun citeproc--sort (_attrs _context &rest body)
  "Placeholder function corresponding to the cs:sort element of CSL."
  body)

(defun citeproc-sort--name-var-key (var context)
  "Return the sort-key for name-var VAR using CONTEXT.
VAR is a CSL name-var name as a symbol. The returned value is a
string containing a semicolon-separated list of all full names in
sort order."
  (citeproc-rt-to-plain
   (citeproc-rt-render-affixes
    (citeproc-name--render-var var '((form . "long") (name-as-sort-order . "all")
				     (et-al-min . nil) (et-al-use-first . "0")
				     (delimiter . "; "))
			       nil nil nil nil nil context))))

(defun citeproc-sort--date-as-key (d _context)
  "Render D citeproc-date struct as a sort key."
  (if d (let ((year (citeproc-date-year d))
	      (month (or (citeproc-date-month d) 0))
	      (day (or (citeproc-date-day d) 0)))
	  ;; We add 5000 as an offset to deal with sorting BC years properly
	  (concat (number-to-string (+ 5000 year))
		  (citeproc-s-fill-left-to-len (number-to-string month) 2 ?0)
		  (citeproc-s-fill-left-to-len (number-to-string day) 2 ?0)))
    ""))

(defun citeproc-sort--date-var-key (var context)
  "Return the sort-key for name-var VAR using CONTEXT.
VAR is a symbol."
  (-let* (((d1 d2) (citeproc-var-value var context))
	  (rendered-first (citeproc-sort--date-as-key d1 context)))
    (if d2
	(concat rendered-first "–" (citeproc-sort--date-as-key d2 context))
      rendered-first)))

(defun citeproc--key (attrs context &rest _body)
  "Return a sort key corresponding to ATTRS and CONTEXT."
  (-let (((&alist 'macro macro
		  'variable var)
	  attrs)
	 (global-attrs (--filter (memq (car it)
				       '(names-min names-use-first names-use-last))
				 attrs)))
    (if var (let ((var-sym (intern var)))
	      (cond
	       ((memq var-sym citeproc--number-vars)
		;; OPTIMIZE: This is way too complicated to simply get a filled
		;; numeric value..
		(citeproc-s-fill-left-to-len
		 (citeproc-number-var-value
		  (citeproc-var-value var-sym context) var-sym 'numeric context)
		 5))
	       ((memq var-sym citeproc--date-vars)
		(citeproc-sort--date-var-key var-sym context))
	       ((memq var-sym citeproc--name-vars)
		(citeproc-sort--name-var-key var-sym context))
	       (t (citeproc-rt-to-plain (citeproc-var-value var-sym context)))))
      (let ((new-context (citeproc-context--create
			  :vars (citeproc-context-vars context)
			  :macros (citeproc-context-macros context)
			  :terms (citeproc-context-terms context)
			  :date-text (citeproc-context-date-text context)
			  :date-numeric (citeproc-context-date-numeric context)
			  :opts (nconc global-attrs (citeproc-context-opts context))
			  :mode (citeproc-context-mode context)
			  :render-mode 'sort
			  :render-year-suffix nil)))
	(citeproc-macro-output-as-text macro new-context)))))

(defun citeproc-sort--compare-keys (k1 k2 &optional desc)
  "Return 1, 0 or -1 depending on the sort-order of keys K1 and K2.
If optional DESC is non-nil then reverse the comparison for
descending sort."
  (cond ((string-collate-equalp k1 k2) 0)
	((s-blank? k1) -1)
	((s-blank? k2) 1)
	(t (* (if (string-collate-lessp k1 k2) 1 -1)
	      (if desc -1 1)))))

(defun citeproc-sort--compare-keylists (k1 k2 sort-orders)
  "Whether keylist K1 precedes keylist K2 in the sort order.
SORT-ORDERS is a list of sort orders to use (see the bib- and
cite-sort-orders slots of `citeproc-style' for details)."
  (citeproc-lib-lex-compare k1 k2 #'citeproc-sort--compare-keys sort-orders))

(defun citeproc-sort--render-keys (style var-alist mode)
  "Render the sort keys of an item with STYLE and VAR-ALIST.
MODE is either `cite' or `bib'."
  (let ((context (citeproc-context-create var-alist style mode 'sort))
	(sort (cl-ecase mode
		(cite (citeproc-style-cite-sort style))
		(bib (citeproc-style-bib-sort style)))))
    (if sort (funcall sort context) nil)))

(defun citeproc-itd-update-sortkey (itd style)
  "Update the sort key of itemdata ITD for STYLE."
  (setf (citeproc-itemdata-sort-key itd)
	(citeproc-sort--render-keys style (citeproc-itemdata-varvals itd) 'bib)))

(defun citeproc-proc-update-sortkeys (proc)
  "Update all sort keys of the itemdata in PROC."
  (let ((style (citeproc-proc-style proc))
	(itds (citeproc-proc-itemdata proc)))
    (maphash (lambda (_id itd)
	       (citeproc-itd-update-sortkey itd style))
	     itds)))

(defun citeproc-sort-itds-on-citnum (itds)
  "Sort itemdata struct list ITDS according to citation number."
  (sort itds
	(lambda (x y)
	  (< (string-to-number (citeproc-itd-getvar x 'citation-number))
	     (string-to-number (citeproc-itd-getvar y 'citation-number))))))

(defun citeproc-sort-itds-on-subbib (itd1 itd2)
  "Sort itemdata structs ITD1 ITD2 according to subbib order."
  (let ((idx1 (car (citeproc-itemdata-subbib-nos itd1)))
	(idx2 (car (citeproc-itemdata-subbib-nos itd2))))
    (and idx1
	 (or (null idx2) (< idx1 idx2)))))

(defun citeproc-sort-itds (itds sort-orders)
  "Sort the itemdata struct list ITDS according to SORT-ORDERS."
  (sort itds
	(lambda (x y)
	  (citeproc-sort--compare-keylists (citeproc-itemdata-sort-key x)
					   (citeproc-itemdata-sort-key y)
					   sort-orders))))

(defun citeproc-proc-sort-itds (proc)
  "Sort the itemdata in PROC."
  (let ((is-sorted-bib (citeproc-style-bib-sort (citeproc-proc-style proc)))
	(is-filtered (citeproc-proc-filtered-bib-p proc)))
    (when (or is-sorted-bib is-filtered)
      (let* ((itds (hash-table-values (citeproc-proc-itemdata proc)))
	     (sorted (if is-sorted-bib
			 (let ((sort-orders (citeproc-style-bib-sort-orders
					     (citeproc-proc-style proc))))
			   (citeproc-sort-itds itds sort-orders))
		       (citeproc-sort-itds-on-citnum itds))))
	;; Additionally sort according to subbibliographies if there are filters.
	(when is-filtered
	  (setq sorted (sort sorted #'citeproc-sort-itds-on-subbib)))
	;; Set the CSL citation-number field according to the sort order.
	(--each-indexed sorted
	  (citeproc-itd-setvar it 'citation-number
			       (number-to-string (1+ it-index))))))))

(provide 'citeproc-sort)

;;; citeproc-sort.el ends here
