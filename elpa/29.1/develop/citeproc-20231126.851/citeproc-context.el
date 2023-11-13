;;; citeproc-context.el --- rendering context for CSL elements -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 András Simonyi

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

;; Provides the `citeproc-context' CL-structure type to represent rendering
;; contexts for CSL elements, and functions for accessing context-dependent
;; information (variable and term values etc.). Also contains some functions
;; that perform context-dependent formatting (e.g., quoting).

;;; Code:

(require 'let-alist)
(require 'subr-x)

(require 'citeproc-lib)
(require 'citeproc-rt)
(require 'citeproc-term)
(require 'citeproc-prange)
(require 'citeproc-style)

(cl-defstruct (citeproc-context (:constructor citeproc-context--create))
  "A struct representing the context for rendering CSL elements."
  vars macros terms date-text date-numeric opts locale-opts
  mode render-mode render-year-suffix no-external-links locale)

(defun citeproc-context-create (var-alist style mode render-mode
					  &optional no-external-links)
  "Create a citeproc-context struct from var-values VAR-ALIST and csl style STYLE.
MODE is either `bib' or `cite', RENDER-MODE is `display' or `sort'."
  (citeproc-context--create
   :vars var-alist
   :macros (citeproc-style-macros style)
   :terms (citeproc-style-terms style)
   :date-text (citeproc-style-date-text style)
   :date-numeric (citeproc-style-date-numeric style)
   :opts (citeproc-style-global-opts style mode)
   :locale (citeproc-style-locale style)
   :locale-opts (citeproc-style-locale-opts style)
   :mode mode
   :render-mode render-mode
   :render-year-suffix (not (citeproc-style-uses-ys-var style))
   :no-external-links no-external-links))

(defconst citeproc--short-long-var-alist
  '((title . title-short) (container-title . container-title-short))
  "Alist mapping the long form of variables names to their short form.")

(defun citeproc-var-value (var context &optional form)
  "Return the value of csl variable VAR in CONTEXT.
VAR is a symbol, CONTEXT is a `citeproc-context' struct, and the
optional FORM can be nil, `short' or `long'."
  (let ((var-vals (citeproc-context-vars context)))
    (if (or (eq form 'short)
	    ;; Also use the short form of title when the cite contains the
	    ;; (use-short-title . t) pair. This is used for title-only citations.
	    (and (eq var 'title) (alist-get 'use-short-title var-vals)))
	(-if-let* ((short-var (alist-get var citeproc--short-long-var-alist))
		   (short-var-val (alist-get short-var var-vals)))
	    short-var-val
	  (alist-get var var-vals))
      (let ((var-val (alist-get var var-vals)))
	(if (and var-val (or (and (eq var 'locator)
				  (string= (citeproc-var-value 'label context) "page"))
			     (eq var 'page)))
	    (let ((prange-format (citeproc-lib-intern (alist-get 'page-range-format
								 (citeproc-context-opts context))))
		  (sep (or (citeproc-term-text-from-terms "page-range-delimiter"
							  (citeproc-context-terms context))
			   "–")))
	      (citeproc-rt-from-str (citeproc-prange-render var-val prange-format sep)))
	  var-val)))))

(defun citeproc-locator-label (context)
  "Return the current locator label variable from CONTEXT."
  (citeproc-var-value 'label context))

(defun citeproc-rt-quote (rt context)
  "Return the quoted version of rich-text RT using CONTEXT."
  (let ((oq (citeproc-term-get-text "open-quote" context))
	(cq (citeproc-term-get-text "close-quote" context))
	(oiq (citeproc-term-get-text "open-inner-quote" context))
	(ciq (citeproc-term-get-text "close-inner-quote" context)))
    `(,oq ,@(citeproc-rt-replace-all-sim `((,oq . ,oiq) (,cq . ,ciq)
					   (,oiq . ,oq) (,ciq . ,cq))
					 (format "\\(%s\\|%s\\|%s\\|%s\\)" oq cq oiq ciq)
					 rt)
	  ,cq)))

(defun citeproc-rt-textcased (rts case context)
  "Return rich-text content RTS in text-case CASE using CONTEXT.
CASE is one of the following: `lowercase', `uppercase',
`capitalize-first', `capitalize-all', `sentence', `title'."
  (pcase case
    ('uppercase
     (citeproc-rt-map-strings #'upcase rts t))
    ('lowercase
     (citeproc-rt-map-strings #'downcase rts t))
    ('capitalize-first
     (--map (citeproc-rt-change-case it #'citeproc-s-capitalize-first) rts))
    ('capitalize-all
     (--map (citeproc-rt-change-case it #'citeproc-s-capitalize-all) rts))
    ('sentence
     (--map (citeproc-rt-change-case it #'citeproc-s-sentence-case) rts))
    ('title
     (let ((locale (citeproc-context-locale context))
	   (language (citeproc-var-value 'language context)))
       (if (or (and language (string-prefix-p "en" language))
	       (and (null language)
		    (or (and locale (string-prefix-p "en" locale))
			(null locale))))
	   (--map (citeproc-rt-change-case it #'citeproc-s-title-case) rts)
	 rts)))))

(defun citeproc-rt-join-formatted (attrs rts context)
  "Join and format according to ATTRS the rich-texts in RTS."
  (let-alist attrs
    (let ((result (delq nil rts)))
      (when .text-case
	(setq result (citeproc-rt-textcased result (intern .text-case) context)))
      (when (string= .strip-periods "true") (setq result (citeproc-rt-strip-periods result)))
      (when (string= .quotes "true") (setq result (citeproc-rt-quote result context)))
      (push (citeproc-rt-select-attrs attrs citeproc-rt-ext-format-attrs) result)
      (if (and .delimiter
	       (> (length result) 2))
	  result
	(citeproc-rt-simplify-shallow result)))))

(defun citeproc-rt-format-single (attrs rt context)
  "Format according to ATTRS rich-text RT using CONTEXT."
  (if (or (not rt) (and (char-or-string-p rt) (string= rt "")))
      nil
    (citeproc-rt-join-formatted attrs (list rt) context)))

(defun citeproc-rt-typed-join (attrs typed-rts context)
  "Join and format according to ATTRS contents in list TYPED-RTS.
TYPED RTS is a list of (RICH-TEXT . TYPE) pairs"
  (-let* ((types (--map (cdr it) typed-rts))
	  (type (cond ((--all? (eq it 'text-only) types)
		       'text-only)
		      ((--any? (eq it 'present-var) types)
		       'present-var)
		      (t 'empty-vars))))
    (cons (citeproc-rt-join-formatted attrs
				      (--map (car it) typed-rts)
				      context)
	  type)))

(defun citeproc-term-get-text (term context)
  "Return the first text associated with TERM in CONTEXT."
  (citeproc-term-text-from-terms term (citeproc-context-terms context)))

(defun citeproc-term-inflected-text (term form number context)
  "Return the text associated with TERM having FORM and NUMBER."
  (let ((matches
	 (--select (string= term (citeproc-term-name it))
		   (citeproc-context-terms context))))
    (cond ((not matches) nil)
	  ((= (length matches) 1)
	   (citeproc-term-text (car matches)))
	  (t (citeproc-term--inflected-text-1 matches form number)))))

(defconst citeproc--term-form-fallback-alist
  '((verb-short . verb)
    (symbol . short)
    (verb . long)
    (short . long))
  "Alist containing the fallback form for each term form.")

(defun citeproc-term--inflected-text-1 (matches form number)
  (let ((match (--first (and (eq form (citeproc-term-form it))
			     (or (not (citeproc-term-number it))
				 (eq number (citeproc-term-number it))))
			matches)))
    (if match
	(citeproc-term-text match)
      (citeproc-term--inflected-text-1
       matches
       (alist-get form citeproc--term-form-fallback-alist)
       number))))

(defun citeproc-term-get-gender (term context)
  "Return the gender of TERM or nil if none is given."
  (-if-let (match
	    (--first (and (string= (citeproc-term-name it) term)
			  (citeproc-term-gender it)
			  (eq (citeproc-term-form it) 'long))
		     (citeproc-context-terms context)))
      (citeproc-term-gender match)
    nil))

(defun citeproc-context-int-link-attrval (style internal-links mode cite-pos)
  "Return an appropriate attribute to represent internal linking info.
INTERNAL-LINKS is the internal linking mode, see the
documentation of `citeproc-render-varlist-in-rt', while MODE is
the rendering mode, `bib' or `cite', and CITE-POS is a cite
position. Returns an appropriate attribute to be added or nil if
no internal links should be produced."
  (let ((note-style (citeproc-style-cite-note style)))
    (unless (or (and internal-links (not (memq internal-links '(auto bib-links))))
		(and note-style (eq mode 'bib) (or (null internal-links)
						   (eq internal-links 'auto))))
      (if (and note-style (not (eq internal-links 'bib-links)))
	  ;; For note styles link subsequent cites to the first ones.
	  (if (eq cite-pos 'first) 'bib-item-no 'cited-item-no)
	;; Else link each cite to the corresponding bib item.
	(if (eq mode 'cite) 'cited-item-no 'bib-item-no)))))

(defun citeproc-render-varlist-in-rt (var-alist style mode render-mode &optional
						internal-links no-external-links)
  "Render an item described by VAR-ALIST with STYLE in rich-text.
Does NOT finalize the rich-text rendering. MODE is either `bib'
or `cite', RENDER-MODE is `display' or `sort'.
  If the optional INTERNAL-LINKS is `bib-links' then link cites
to the bibliography regardless of the style type, if `no-links'
then don't add internal links, if nil or `auto' then add internal
links based on the style type (cite-cite links for note styles
and cite-bib links else). For legacy reasons, any other value is
treated as `no-links'.
  If the optional NO-EXTERNAL-LINKS is non-nil then don't add
external links."
  (-if-let (unprocessed-id (alist-get 'unprocessed-with-id var-alist))
      ;; Itemid received no associated csl fields from the getter!
      (list nil (concat "NO_ITEM_DATA:" unprocessed-id))
    (let* ((context (citeproc-context-create var-alist style mode render-mode
					     no-external-links))
	   (layout-fun-accessor (if (eq mode 'cite) 'citeproc-style-cite-layout
				  'citeproc-style-bib-layout))
	   (layout-fun (funcall layout-fun-accessor style)))
      (if (null layout-fun) "[NO BIBLIOGRAPHY LAYOUT IN CSL STYLE]"
	(let ((rendered (catch 'stop-rendering
			  (funcall layout-fun context))))
	  ;; Finalize external linking by linking the title if needed
	  (when (and (eq mode 'bib) (not no-external-links))
	    (-when-let ((var . val) (--any (assoc it var-alist)
					   citeproc--linked-vars))
	      (unless (cl-intersection (citeproc-rt-rendered-vars rendered)
				       citeproc--linked-vars)
		(citeproc-rt-link-title rendered
					(concat (alist-get var citeproc--link-prefix-alist
							   "")
						(alist-get var var-alist))))))
	  ;; Add appropriate item-no information
	  (when-let* ((cite-no-attr
		       (citeproc-context-int-link-attrval
			style internal-links mode (alist-get 'position var-alist)))
		      (cite-no-attr-val (cons cite-no-attr
					     (alist-get 'citation-number var-alist))))
	    (cond ((consp rendered) (setf (car rendered)
					  (-snoc (car rendered) cite-no-attr-val)))
		  ((stringp rendered) (setq rendered
					    (list (list cite-no-attr-val) rendered)))))
	  ;; Add year-suffix if needed
	  (-if-let (year-suffix (alist-get 'year-suffix var-alist))
	      (car (citeproc-rt-add-year-suffix
		    rendered
		    ;; year suffix is empty if already rendered by var just to delete the
		    ;; suppressed date
		    (if (citeproc-style-uses-ys-var style) "" year-suffix)))
	    rendered))))))

(provide 'citeproc-context)

;;; citeproc-context.el ends here
