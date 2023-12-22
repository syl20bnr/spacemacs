;; citeproc-style.el --- CSL style structure and related functions -*- lexical-binding: t; -*-

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

;; Structure type and functions for constructing and accessing CSL style
;; objects.

;;; Code:

(require 'subr-x)
(require 'let-alist)
(require 'dash)
(require 'cl-lib)
(require 's)

(require 'citeproc-lib)
(require 'citeproc-locale)

(cl-defstruct (citeproc-style (:constructor citeproc-style--create))
  "A struct representing a parsed and localized CSL style.
INFO is the style's general info (currently simply the
  corresponding fragment of the parsed xml),
OPTS, BIB-OPTS, CITE-OPTS and LOCALE-OPTS are alists of general
  and bibliography-, cite- and locale-specific CSL options,
BIB-SORT, BIB-LAYOUT, CITE-SORT and CITE-LAYOUT are anonymous
  functions for calculating sort-keys and rendering,
BIB-SORT-ORDERS and CITE-SORT-ORDERS are the lists of sort orders
  for bibliography and cite sort (the value is a list containg t
  or nil as its n-th element depending on whether the sort for on
  the n-th key should be in ascending or desending order,
CITE-LAYOUT-ATTRS contains the attributes of the citation layout
  as an alist,
CITE-NOTE is non-nil iff the style's citation-format is \"note\",
DATE-TEXT and DATE-NUMERIC are the style's date formats,
LOCALE contains the locale to be used or nil if not set,
MACROS is an alist with macro names as keys and corresponding
  anonymous rendering functions,
TERMS is the style's parsed term-list,
USES-YS-VAR is non-nil iff the style uses the YEAR-SUFFIX
  CSL-variable."
  info opts bib-opts bib-sort bib-sort-orders
  bib-layout cite-opts cite-note cite-sort cite-sort-orders
  cite-layout cite-layout-attrs locale-opts macros terms
  uses-ys-var date-text date-numeric locale)

(defun citeproc-style-parse (style)
  "Return the parsed representation of csl STYLE.
STYLE is either a path to a style file or a style as a string.
Returns a (YEAR-SUFF-P . PARSED-STYLE) cons cell. YEAR-SUFF-P is
non-nil if the style uses the `year-suffix' csl var; PARSED-STYLE
is the parsed form of the xml STYLE-FILE."
  (let ((xml-input (s-matches-p " ?<" style)))
    (with-temp-buffer
      (let ((case-fold-search t))
	(if xml-input (insert style)
	  (insert-file-contents style))
	(goto-char 1)
	(cons (re-search-forward "variable=\"year-suffix\"" nil t)
	      (citeproc-lib-remove-xml-comments
	       (libxml-parse-xml-region (point-min) (point-max))))))))

;; TODO: Parse and store info in a more structured and sensible form. Also,
;; currently the first in-style locale is loaded that is compatible with the
;; locale to be used. In theory, there may be more than one compatible in-style
;; locales that should be merged in an order reflecting their closeness to the
;; requested locale.
(defun citeproc-create-style-from-locale (parsed-style year-suffix locale)
  "Create a citation style from parsed xml style PARSED-STYLE.
YEAR-SUFFIX specifies whether the style explicitly uses the
`year-suffix' csl variable. LOCALE is the locale for which
in-style locale information will be loaded (if available)."
  (let* ((style (citeproc-style--create))
	 (style-opts (cadr parsed-style))
	 locale-loaded)
    (setf (citeproc-style-opts style) style-opts
	  (citeproc-style-uses-ys-var style) year-suffix
	  (citeproc-style-locale style)
	  (or locale (alist-get 'default-locale style-opts)))
    (--each (cddr parsed-style)
      (pcase (car it)
	('info
	 (let ((info-lst (cddr it)))
	   (setf (citeproc-style-info style) info-lst
		 (citeproc-style-cite-note style)
		 (not (not (member '(category
				     ((citation-format . "note")))
				   info-lst))))))
	('locale
	 (let ((lang (alist-get 'lang (cadr it))))
	   (when (and (citeproc-locale--compatible-p lang locale)
		      (not locale-loaded))
	     (citeproc-style--update-locale style it)
	     (setq locale-loaded t))))
	('citation
	 (citeproc-style--update-cite-info style it))
	('bibliography
	 (citeproc-style--update-bib-info style it))
	('macro
	 (citeproc-style--update-macros style it))))
    style))

(defun citeproc-style--parse-layout-and-sort-frag (frag)
  "Parse a citation or bibliography style xml FRAG.
Return an alist with keys `layout', `opts', `layout-attrs', `sort'
and `sort-orders'."
  (let* ((opts (cadr frag))
	 (sort-p (eq (cl-caaddr frag) 'sort))
	 (layout (citeproc-style--transform-xmltree
		  (elt frag (if sort-p 3 2))))
	 (layout-attrs (cl-cadadr (cl-caddr layout)))
	 sort sort-orders)
    (when sort-p
      (let* ((sort-frag (cl-caddr frag)))
	(setq sort (citeproc-style--transform-xmltree sort-frag)
	      sort-orders (--map (not (string= "descending" (alist-get 'sort (cadr it))))
				 (cddr sort-frag)))))
    `((opts . ,opts) (layout . ,layout) (layout-attrs . ,layout-attrs)
      (sort . ,sort) (sort-orders . ,sort-orders))))

(defun citeproc-style--update-cite-info (style frag)
  "Update the cite info of STYLE on the basis of its parsed FRAG."
  (let-alist (citeproc-style--parse-layout-and-sort-frag frag)
    (setf (citeproc-style-cite-opts style) .opts
	  (citeproc-style-cite-layout style) .layout
	  (citeproc-style-cite-layout-attrs style) .layout-attrs
	  (citeproc-style-cite-sort style) .sort
	  (citeproc-style-cite-sort-orders style) .sort-orders)))

(defun citeproc-style--update-bib-info (style frag)
  "Update the bib info of STYLE on the basis of its parsed FRAG."
  (let-alist (citeproc-style--parse-layout-and-sort-frag frag)
    (setf (citeproc-style-bib-opts style) .opts
	  (citeproc-style-bib-layout style) .layout
	  (citeproc-style-bib-sort style) .sort
	  (citeproc-style-bib-sort-orders style) .sort-orders)))

(defun citeproc-style--update-macros (style frag)
  "Update the macro info of STYLE on the basis of its parsed FRAG."
  (let ((name (cl-cdaadr frag)))
    (setf (car frag) 'macro)
    (setf (cadr frag) nil)
    (push (cons name (citeproc-style--transform-xmltree frag))
	  (citeproc-style-macros style))))

(defun citeproc-style--update-locale (style frag)
  "Update locale info in STYLE using xml fragment FRAG.
FRAG should be a parsed locale element from a style or a locale."
  (--each (cddr frag)
    (pcase (car it)
      ('style-options (setf (citeproc-style-locale-opts style)
			    (-concat (citeproc-style-locale-opts style)
				     (cadr it))))
      ('date
       (citeproc-style--update-locale-date style it))
      ('terms
       (let ((parsed-terms (citeproc-locale-termlist-from-xml-frag (cddr it))))
	 (setf (citeproc-style-terms style)
	       (if (citeproc-style-terms style)
		   (citeproc-term-list-update parsed-terms (citeproc-style-terms style))
		 parsed-terms)))))))

(defun citeproc-style--update-locale-date (style frag)
  "Update date info in STYLE using xml fragment FRAG.
FRAG should be a parsed locale element from a style or a locale."
  (let* ((date-attrs (cadr frag))
	 (form (alist-get 'form date-attrs))
	 (date-format (cons date-attrs
			    (citeproc-lib-named-parts-to-alist frag))))
    (if (string= form "text")
	(unless (citeproc-style-date-text style)
	  (setf (citeproc-style-date-text style) date-format))
      (unless (citeproc-style-date-numeric style)
	(setf (citeproc-style-date-numeric style) date-format)))))

(defconst citeproc-style--opt-defaults
  '((cite-opts near-note-distance "5")
    (locale-opts punctuation-in-quote "false")
    (locale-opts limit-day-ordinals-to-day-1 "false")
    (bib-opts hanging-indent "false")
    (bib-opts line-spacing "1")
    (bib-opts entry-spacing "1")
    (opts initialize-with-hyphen "true")
    (opts demote-non-dropping-particle "display-and-sort"))
  "Global style options.
Specified as a list of (STYLE-SLOT OPTION-NAME OPTION-DEFAULT)
lists.

Note: Collapse-related options are not specified here since their
default settings are interdependent.")

(defun citeproc-style--set-opt (style opt-slot opt value)
  "Set OPT in STYLE's OPT-SLOT to VALUE."
  (setf (cl-struct-slot-value 'citeproc-style opt-slot style)
	(cons (cons opt value)
	      (cl-struct-slot-value 'citeproc-style opt-slot style))))

(defun citeproc-style--set-opt-defaults (style)
  "Set missing options of STYLE to their default values."
  (cl-loop
   for (slot option value) in citeproc-style--opt-defaults do
   (let ((slot-value (cl-struct-slot-value 'citeproc-style slot style)))
     (unless (alist-get option slot-value)
       (setf (cl-struct-slot-value 'citeproc-style slot style)
	     (cons (cons option value) slot-value)))))
  (let* ((cite-opts (citeproc-style-cite-opts style))
	 (collapse (alist-get 'collapse cite-opts)))
    (when (and collapse (not (string= collapse "citation-number")))
      (let ((cite-layout-dl
	     (alist-get 'delimiter (citeproc-style-cite-layout-attrs style)))
	    (cite-group-dl
	     (alist-get 'cite-group-delimiter cite-opts)))
	(unless cite-group-dl
	  (citeproc-style--set-opt style 'cite-opts 'cite-group-delimiter ", "))
	(unless (alist-get 'after-collapse-delimiter cite-opts)
	  (citeproc-style--set-opt
	   style 'cite-opts 'after-collapse-delimiter cite-layout-dl))
	(when (and (member collapse '("year-suffix" "year-suffix-ranged"))
		   (null (alist-get 'year-suffix-delimiter cite-opts)))
	  (citeproc-style--set-opt
	   style 'cite-opts 'year-suffix-delimiter cite-layout-dl))))))

(defun citeproc-style--transform-xmltree (tree)
  "Transform parsed csl xml fragment TREE into a lambda."
  `(lambda (context) ,(citeproc-style--transform-xmltree-1 tree)))

(defun citeproc-style--transform-xmltree-1 (tree)
  "Transform parsed xml fragment TREE into an eval-able form.
Symbols in car position are prefixed with `citeproc--' and the
symbol `context' is inserted everywhere after the second (attrs)
position and before the (possibly empty) body."
  (pcase tree
    ((pred atom) tree)
    (`(names . ,_) (citeproc-style--transform-names tree))
    (_
     `(,(intern (concat "citeproc--" (symbol-name (car tree))))
       ,(list 'quote (cadr tree))
       context
       ,@(mapcar #'citeproc-style--transform-xmltree-1 (cddr tree))))))

(defun citeproc-style--transform-names (frag)
  "Transform the content of a cs:names CSL element xml FRAG."
  (let* ((names-attrs (cadr frag))
	 (body (-remove #'stringp (cddr frag)))
	 (vars (alist-get 'variable names-attrs))
	 substs name-attrs name-parts et-al-attrs
	 is-label label-attrs label-before-names)
    (--each body
      (pcase (car it)
	('name
	 (setq name-attrs (cadr it)
	       name-parts (citeproc-lib-named-parts-to-alist it)
	       label-before-names t))
	('et-al
	 (setq et-al-attrs (cadr it)))
	('label
	 (setq is-label t
	       label-attrs (cadr it)
	       label-before-names nil))
	('substitute
	 (setq substs
	       (mapcar
		(lambda (x)
		  (if (eq (car x) 'names)
		      `(citeproc-name-render-vars
			,(alist-get 'variable (cadr x))
			names-attrs name-attrs name-parts et-al-attrs
			is-label label-before-names label-attrs context)
		    (citeproc-style--transform-xmltree-1 x)))
		(cddr it))))))
    `(if (citeproc-var-value 'suppress-author context) (cons nil 'empty-vars)
       (let* ((names-attrs ',names-attrs)
	      (name-attrs ',name-attrs)
	      (count (string= (alist-get 'form name-attrs) "count"))
	      (et-al-attrs ',et-al-attrs)
	      (name-parts ',name-parts)
	      (label-attrs ',label-attrs)
	      (is-label ,is-label)
	      (label-before-names ,label-before-names)
	      (val (citeproc-name-render-vars
		    ,vars names-attrs name-attrs name-parts et-al-attrs
		    is-label label-before-names label-attrs context))
	      (result (if (car val)
			  val
			(-if-let ((cont . type) (--first (car it)
							 (list ,@substs)))
			    (cons (cons (list '(subst . t)) (list cont)) type)
			  (cons nil 'empty-vars))))
	      (final (if count
			 (let* ((number (citeproc-rt-count-names (car result)))
				(str (if (= 0 number) "" (number-to-string number))))
			   (cons str (cdr result)))
		       result)))
	 ;; Handle `author' citation mode by stopping if needed
	 (citeproc-lib-maybe-stop-rendering 'names context final)))))

(defun citeproc-style-global-opts (style layout)
  "Return the global opts in STYLE for LAYOUT.
LAYOUT is either `bib' or `cite'."
  (-concat (cl-ecase layout
	     (bib (citeproc-style-bib-opts style))
	     (cite (citeproc-style-cite-opts style)))
	   (citeproc-style-opts style)))

(defun citeproc-style-bib-opts-to-formatting-params (bib-opts)
  "Convert BIB-OPTS to a formatting parameters alist."
  (let ((result
	 (cl-loop
	  for (opt . val) in bib-opts
	  if (memq opt
		   '(hanging-indent line-spacing entry-spacing second-field-align))
	  collect (cons opt
			(pcase val
			  ("true" t)
			  ("false" nil)
			  ("flush" 'flush)
			  ("margin" 'margin)
			  (_ (string-to-number val)))))))
    (if (alist-get 'second-field-align result)
	result
      (cons (cons 'second-field-align nil)
	    result))))

(provide 'citeproc-style)

;;; citeproc-style.el ends here
