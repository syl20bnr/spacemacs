;;; citeproc-cite.el --- cite and citation rendering -*- lexical-binding: t; -*-

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

;; Functionality to render citations and the cites they contain. (Terminology
;; from the CSL standard: "citations consist of one or more cites to individual
;; items".)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'queue)
(require 's)

(require 'citeproc-rt)
(require 'citeproc-number)
(require 'citeproc-itemdata)
(require 'citeproc-style)
(require 'citeproc-proc)
(require 'citeproc-formatters)
(require 'citeproc-sort)
(require 'citeproc-subbibs)

(cl-defstruct (citeproc-citation (:constructor citeproc-citation-create))
  "A struct representing a citation.
CITES is a list of alists describing individual cites,
NOTE-INDEX is the note index of the citation if it occurs in a
  note,
MODE is either nil (for the default citation mode) or one
  of the symbols `suppress-author', `textual', `author-only',
  `year-only', `title-only', `bib-entry', `locator-only',
SUPPRESS-AFFIXES is non-nil if the citation affixes should be
  suppressed,
CAPITALIZE-FIRST is non-nil if the first word of the rendered
  citation should be capitalized,
IGNORE-ET-AL is non-nil if et-al settings should be ignored for
  the first cite.
GROUPED is used internally to indicate whether the cites were
  grouped by the csl processor."
  cites note-index mode suppress-affixes capitalize-first
  ignore-et-al grouped)

(defconst citeproc-cite--from-mode-alist
  '((textual . ((suppress-author . t)))
    (suppress-author . ((suppress-author . t)))
    (author-only . ((stop-rendering-at . names)))
    (year-only . ((stop-rendering-at . issued)))
    (title-only . ((stop-rendering-at . title) (bib-entry . t) (use-short-title . t)))
    (bib-entry . ((bib-entry . t)))
    (locator-only . ((locator-only . t))))
  "Alist mapping citation modes to corresponding cite-level
key-value pair representations.")

(defvar citeproc-citation-postprocess-functions nil
  "A list of functions to postprocess rendered citations.
Each function takes a single argument, a rich-text, and returns a
post-processed rich-text value. The functions are applied in the
order they appear in the list.")

(defun citeproc-cite--varlist (cite)
  "Return the varlist belonging to CITE."
  (let* ((itd (alist-get 'itd cite))
	 (item-vv (citeproc-itemdata-varvals itd))
	 ;; OPTIMIZE: Should we do this filtering?
	 (cite-vv
	  (--filter (memq (car it)
			  '(label locator suppress-author suppress-date
				  stop-rendering-at position near-note
				  first-reference-note-number ignore-et-al
				  bib-entry locator-only use-short-title))
		    cite)))
    (nconc cite-vv item-vv)))

(defun citeproc-cite--render (cite style &optional internal-links)
  "Render CITE in STYLE, together with its affixes.
If the prefix or suffix in CITE don't contain trailing and
leading spaces then they are added. If the optional
INTERNAL-LINKS is `bib-links' then link cites to the bibliography
regardless of the style type, if `no-links' then don't add
internal links, if nil or `auto' then add internal links based on
the style type (cite-cite links for note styles and cite-bib
links else). For legacy reasons, any other value is treated as
`no-links'."
  (-let* ((result nil)
	  ((&alist 'suffix suff
		   'prefix pref
		   'bib-entry bib-entry
		   'locator-only locator-only
		   'stop-rendering-at stop-rendering-at)
	   cite)
	  (rt-pref (citeproc-rt-from-str pref))
	  (plain-pref (citeproc-rt-to-plain rt-pref))
	  (rt-suff (citeproc-rt-from-str suff))
	  (plain-suff (citeproc-rt-to-plain rt-suff))
	  (mode (if bib-entry 'bib 'cite))
	  (varlist (citeproc-cite--varlist cite)))
    ;; Remove cite-number when cite is the full bibliography entry.
    (when (and (eq mode 'bib) (not stop-rendering-at))
      (push '(citation-number) varlist))
    (let ((rendered-varlist
	   (citeproc-render-varlist-in-rt
	    varlist style mode 'display
	    ;; No link-targets for bib-entry based citations.
	    (if (eq mode 'bib) 'no-links internal-links)
	    ;; No external limking for title-only citations, since we link to the
	    ;; corresponding bibliography entry.
	    (eq stop-rendering-at 'title))))
      ;; Locator-only cites require extensive post-processing of full cite.
      (when locator-only
	(setq rendered-varlist (citeproc-rt-locator-w-label rendered-varlist)))
      ;; Title-only cites
      (when (eq stop-rendering-at 'title)
	(when-let* ((cite-no-attr
		     (citeproc-context-int-link-attrval
		      style internal-links 'cite (alist-get 'position varlist)))
		    (cite-no-attr-val (cons cite-no-attr
					    (alist-get 'citation-number varlist))))
	  ;; Add cited-item-no attr to link to the bibliography entry
	  (setf (car rendered-varlist)
		(-snoc (car rendered-varlist) cite-no-attr-val))))
      ;; Add cite prefix and suffix
      (when (s-present-p plain-suff)
	(push (citeproc-rt-from-str suff) result)
	(unless (memql (aref plain-suff 0) '(?, ?\s))
	  (push " " result)))
      (push rendered-varlist result)
      (when (s-present-p plain-pref)
	(unless (= (aref plain-pref (1- (length plain-pref))) ?\s)
	  (push " " result))
	(push rt-pref result))
      (citeproc-rt-join-formatted nil result nil))))

(defun citeproc-cite-or-citegroup--render (c style internal-links top-dl gr-dl ys-dl ac-dl)
  "Render cite or cite-group C with STYLE.
For the INTERNAL-LINKS argument see `citeproc-cite--render'.
TOP-DL is the top-, GR-DL the group-, YS-DL the year-suffix- and
AC-DL the after-collapse-delimiter to use."
  (cond ((and (car c) (memq (car c) '(top group year-suffix-collapsed)))
	 (let ((delimiter (pcase (car c)
			    ('top top-dl)
			    ('group gr-dl)
			    ('year-suffix-collapsed ys-dl))))
	   (cons
	    nil				; empty attribute list
	    (nbutlast			; remove last delimiter
	     (--mapcat (list (citeproc-cite-or-citegroup--render
			      it style internal-links top-dl gr-dl ys-dl ac-dl)
			     (if (and (car it) (memq (car it) '(group year-suffix-collapsed)))
				 ac-dl
			       delimiter))
		       (cdr c))))))
	((eq (car c) 'range)
	 (list nil (citeproc-cite--render (cl-second c) style internal-links)
	       "–" (citeproc-cite--render (cl-third c) style internal-links)))
	(t (citeproc-cite--render c style internal-links))))

(defun citeproc-citation--render (c proc &optional internal-links)
  "Render citation C with CSL processor PROC.
For the optional INTERNAL-LINKS argument see
`citeproc-cite--render'."
  (let* ((style (citeproc-proc-style proc))
	 (punct-in-quote
	  (string= (alist-get 'punctuation-in-quote (citeproc-style-locale-opts style))
		   "true"))
	 (cites (citeproc-citation-cites c))
	 (cite-attrs (citeproc-style-cite-layout-attrs style))
	 (cite-layout-dl (alist-get 'delimiter cite-attrs)))
    ;; Remove delimiters from cite-attrs -- they are rendered 'manually' because of
    ;; the delimiter-after-collapse complications in rendering. Also remove affixes
    ;; if requested.
    (setq cite-attrs
	  (if (citeproc-citation-suppress-affixes c)
	      (--remove (memq (car it) '(delimiter prefix suffix)) cite-attrs)
	    (--remove (eq (car it) 'delimiter) cite-attrs)))
    ;; Generate rendered cites
    (let ((rendered-cites
	   (cond
	    ((citeproc-citation-grouped c)
	     (let ((gr-dl
		    (alist-get 'cite-group-delimiter (citeproc-style-cite-opts style)))
		   (ys-dl
		    (alist-get 'year-suffix-delimiter (citeproc-style-cite-opts style)))
		   (aft-coll-dl
		    (alist-get
		     'after-collapse-delimiter (citeproc-style-cite-opts style))))
	       (cdr (citeproc-cite-or-citegroup--render
		     (cons 'top cites)	; indicate top level input
		     style internal-links cite-layout-dl gr-dl ys-dl aft-coll-dl))))
	    ((cdr cites)
	     (cdr (--mapcat
		   (list cite-layout-dl (citeproc-cite--render it style internal-links))
		   cites)))
	    (t
	     (list (citeproc-cite--render (car cites) style internal-links))))))
      ;; Calculate inner and outer citation attrs (affixes go inside)
      (let* ((non-affixes (--remove (memq (car it) '(prefix suffix delimiter)) cite-attrs))
	     (affixes (--filter (memq (car it) '(prefix suffix)) cite-attrs))
	     (outer-attrs (and affixes non-affixes))
	     (result
	      (citeproc-rt-cull-spaces-puncts
	       (citeproc-rt-finalize
		(citeproc-rt-render-affixes
		 (citeproc-rt-join-formatted (if outer-attrs affixes cite-attrs)
					     rendered-cites nil)
		 t)
		punct-in-quote))))
	;; Add outer (non-affix attrs) if needed
	(when outer-attrs
	  (setq result (list outer-attrs result)))
	;; Prepend author to textual citations
	(when (eq (citeproc-citation-mode c) 'textual)
	  (let* ((first-elt (car cites)) ;; First elt is either a cite or a cite group.
		 ;; If the latter then we need to locate the
		 ;; first cite as the 2nd element of the first
		 ;; cite group.
		 (first-cite (if (eq 'group (car first-elt))
				 (cadr first-elt)
			       first-elt))
		 (author-cite
		  (append '((suppress-author . nil) (stop-rendering-at . names)
			    (prefix) (suffix) (locator))
		   first-cite))
		 (rendered-author (citeproc-cite--render author-cite style 'no-links)))
	    (when (listp rendered-author)
	      (setq result `(nil ,rendered-author " " ,result)))))
	;; Capitalize first
	(when (citeproc-citation-capitalize-first c)
	  (setq result (citeproc-rt-change-case result #'citeproc-s-capitalize-first)))
	;; Run the citation postprocessing hook
	(dolist (fn citeproc-citation-postprocess-functions)
	  (setq result (funcall fn result)))
	result))))

(defun citeproc-cites--collapse-indexed (cites index-getter no-span-pred)
  "Collapse continuously indexed cites in CITES.
INDEX-GETTER is a function from cites to numeric indices,
NO-SPAN-PRED is a predicate that returns non-nil for cites that
cannot be part of a span. Return the collapsed cites list or nil
if no cites were collapsed."
  (let (group-len start-cite prev-index end-cite result)
    (dolist (cite cites)
      (let* ((cur-index (funcall index-getter cite))
	     (no-span-elt (funcall no-span-pred cite))
	     (subsequent (and prev-index (= (1+ prev-index) cur-index))))
	;; Process ending current group
	(when (and group-len (or no-span-elt (not subsequent)))
	  (setq result (nconc (citeproc-cite-range--collapse
			       start-cite end-cite
			       group-len)
			      result)))
	(cond (no-span-elt		; Not only cite-no
	       (push cite result)
	       (setq group-len nil))
	      ((or (not group-len) (not subsequent)) ; New group starts
	       (setq group-len 1
		     start-cite cite
		     prev-index cur-index))
	      (t			; Group continues
	       (cl-incf group-len)
	       (setq end-cite cite
		     prev-index cur-index)))))
    ;; Process the last group
    (when group-len
      (setq result (nconc (citeproc-cite-range--collapse
			   start-cite end-cite group-len)
			  result)))
    (if (/= (length cites) (length result))
	(nreverse result)
      nil)))

(defun citeproc-cite-range--collapse (start-cite end-cite len)
  "Collapse cite span with START-CITE, END-CITE of LEN length.
START-CITE end END-CITE is the first and last rendered cites of
the span."
  (pcase len
    (1 (list start-cite))
    (2 (list end-cite start-cite))
    (_ (list (list 'range start-cite end-cite)))))

(defun citeproc-citation--collapse-num-citeranges (citation)
  "Collapse numbered ranges in CITATION."
  (let* ((cites (citeproc-citation-cites citation))
	 (cites-length (length cites)))
    (when (> cites-length 2)
      (-when-let (collapsed
		  (citeproc-cites--collapse-indexed
		   cites
		   (lambda (x)
		     (string-to-number
		      (alist-get 'citation-number (citeproc-cite--varlist x))))
		   (lambda (x) (alist-get 'locator (citeproc-cite--varlist x)))))
	(setf (citeproc-citation-cites citation) collapsed
	      (citeproc-citation-grouped citation) t)))))

(defun citeproc-cites--collapse-suff-citeranges (cites)
  "Collapse continuously year-suffixed CITES."
  (or (citeproc-cites--collapse-indexed
       cites
       (lambda (x)
	 (string-to-char (alist-get 'year-suffix (citeproc-cite--varlist x) " ")))
       (lambda (_x) nil))
      cites))

(defun citeproc-citation--render-formatted-citation (c proc format &optional internal-links)
  "Render citation C with csl processor PROC in FORMAT.
For the optional INTERNAL-LINKS argument see
`citeproc-cite--render'."
  (let ((fmt (citeproc-formatter-for-format format)))
    (funcall (citeproc-formatter-cite fmt)
	     (funcall (citeproc-formatter-rt fmt)
		      (citeproc-citation--render c proc internal-links)))))

(defun citeproc-citation--sort-cites (citation proc)
  "Sort cites in CITATION for processor PROC."
  (let ((cites (citeproc-citation-cites citation)))
    (when (cdr cites)
      (let* ((style (citeproc-proc-style proc))
	     (sort-orders (citeproc-style-cite-sort-orders style)))
	(setf (citeproc-citation-cites citation)
	      (sort
	       (--map (cons (cons 'key	; add keys to the cites as extra attr
				  (citeproc-sort--render-keys style (citeproc-cite--varlist it) 'cite))
			    it)
		      cites)
	       (lambda (x y) (citeproc-sort--compare-keylists (cdar x) (cdar y) sort-orders))))))))

(defun citeproc-proc-sort-cites (proc)
  "Sort cites in all citations of PROC."
  (when (citeproc-style-cite-sort (citeproc-proc-style proc))
    (dolist (citation (queue-head (citeproc-proc-citations proc)))
      (citeproc-citation--sort-cites citation proc))))

(defun citeproc-proc-apply-citation-modes (proc)
  "Apply mode to the first cite in each citation in PROC."
  (dolist (citation (queue-head (citeproc-proc-citations proc)))
    (let ((mode (citeproc-citation-mode citation))
	  (cites (citeproc-citation-cites citation))
	  (ignore-et-al (citeproc-citation-ignore-et-al citation)))
      (-when-let (mode-rep
		  (alist-get mode citeproc-cite--from-mode-alist))
	(setf (car cites) (nconc (car cites) mode-rep)))
      (when ignore-et-al
	(push '(ignore-et-al . t) (car cites))))))

(defun citeproc-proc-group-and-collapse-cites (proc)
  "Group and collapse cites in all citations of PROC."
  (let* ((cite-opts (citeproc-style-cite-opts (citeproc-proc-style proc)))
	 (group-delim (alist-get 'cite-group-delimiter cite-opts))
	 (collapse-type (alist-get 'collapse cite-opts))
	 (collapse-year-type
	  (when collapse-type
	    (let ((cy (member collapse-type
			      '("year" "year-suffix" "year-suffix-ranged"))))
	      (and cy (car cy))))))
    ;; Collapse (and group) according to collapse type
    (cond ((or group-delim collapse-year-type)
	   ;; Group and possibly collapse
	   (dolist (citation (queue-head (citeproc-proc-citations proc)))
	     (citeproc-citation--group-and-collapse-cites citation proc collapse-type)))
	  ;; Collapse numeric cites
	  ((string= collapse-type "citation-number")
	   (dolist (citation (queue-head (citeproc-proc-citations proc)))
	     (citeproc-citation--collapse-num-citeranges citation))))))

(defun citeproc-citation--group-and-collapse-cites (c proc &optional collapse-type)
  "Divide items in citation C in place into groups for PROC.
Apart from movement necessary for grouping, the relative
positions of cites in C is kept. If optional COLLAPSE-TYPE is
given then collapse the groups accordingly."
  (let ((cites (citeproc-citation-cites c)))
    (when (cdr cites)
      (let (groups)
	(dolist (cite cites)
	  (let ((g-ind
		 ;; Cites are in the same group iff the cdrs of the rendered cite's first
		 ;; name-var are equal. The cdr is taken because we ignore attributes, in
		 ;; particular the cited-item-no attribute which is added when the cite consists
		 ;; entirely of the rendered name var
		 (--find-index (equal (cdr (citeproc-cite--first-namevar-cont cite proc))
				      (cdr (citeproc-cite--first-namevar-cont (car it) proc)))
			       groups)))
	    (if g-ind
		(push cite (nth g-ind groups))
	      (push (list cite) groups))))
	(unless (= (length groups) (length cites))
	  (setf (citeproc-citation-cites c)
		(nreverse
		 (--map (if (cdr it)
			    (cons 'group
				  (pcase collapse-type
				    ("year"
				     (citeproc-citation-group--collapse-year (nreverse it)))
				    ("year-suffix"
				     (citeproc-citation-group--collapse-ys (nreverse it) proc nil))
				    ("year-suffix-ranged"
				     (citeproc-citation-group--collapse-ys (nreverse it) proc t))
				    (_ (nreverse it))))
			  (car it))
			groups))
		(citeproc-citation-grouped c) t))))))

(defun citeproc-citation-group--collapse-year (cites)
  "Collapse year in group CITES."
  (cons (car cites)
	(--map (cons '(suppress-author . t) it)
	       (cdr cites))))

(defun citeproc-citation-group--collapse-ys (cites proc collapse-ranges)
  "Collapse year and suffix in group CITES using PROC.
If optional COLLAPSE-RANGES is non-nil then collapse year-suffix
ranges."
  (let ((first t) (groups (list (list (car cites))))
	prev-datevar-cont prev-locator)
    (dolist (cite cites)
      (let* ((varlist (citeproc-cite--varlist cite))
	     (datevar-cont (cadr (citeproc-cite--first-datevar-cont cite proc)))
	     (locator (alist-get 'locator varlist)))
	(cond (first
	       (setq first nil))
	      ((or prev-locator
		   locator
		   (not (alist-get 'year-suffix varlist))
		   (not (equal datevar-cont prev-datevar-cont)))
	       (push (list (cons '(suppress-author . t)
				 cite))
		     groups))
	      (t
	       (push (cons '(suppress-date . t)
			   (cons '(suppress-author . t)
				 cite))
		     (car groups))))
	(setq prev-datevar-cont datevar-cont
	      prev-locator locator))
      cites)
    (nreverse
     (--map (if (cdr it)
		(cons 'year-suffix-collapsed
		      (if (and collapse-ranges (> (length cites) 2))
			  (citeproc-cites--collapse-suff-citeranges (nreverse it))
			(nreverse it)))
	      (car it))
	    groups))))

(defun citeproc-citations--itd-referred-p (itd citations)
  "Whether ITD is referred to in CITATIONS."
  (let ((cites (--mapcat (citeproc-citation-cites it) citations)))
    (--any-p (eq itd (alist-get 'itd it)) cites)))

(defun citeproc-cite--update-nn-queue (q index nnd)
  "Remove too distant citations from near-notes queue Q.
INDEX is the actual note-index, NND is the near-note-distance."
  (while (and (queue-head q)
	      (< nnd (- index
			(citeproc-citation-note-index (queue-first q)))))
    (queue-dequeue q)))

(defun citeproc-cite--loc-equal-p (s1 s2)
  "Whether locator strings S1 and S2 refer to the same location."
  (if (and (citeproc-lib-numeric-p s1) (citeproc-lib-numeric-p s2))
      (equal (citeproc-number-extract s1) (citeproc-number-extract s2))
    (string= (s-trim s1) (s-trim s2))))

(defvar citeproc-disambiguation-cite-pos 'last
  "Which cite position should be the basis of cite disambiguation.
Possible values are `last', `first' and `subsequent'.")

(defun citeproc-proc-update-positions (proc)
  "Update all position-related fields in PROC."
  (citeproc-proc-delete-occurrence-info proc)
  (let* ((ctns (queue-head (citeproc-proc-citations proc)))
	 (cite-opts (citeproc-style-cite-opts (citeproc-proc-style proc)))
	 (nnd (string-to-number
	       (or (alist-get 'near-note-distance cite-opts)
		   "5")))
	 (near-note-ctns (make-queue))
	 prev-itd prev-loc prev-label)
    (unless (eq citeproc-disambiguation-cite-pos 'last)
      (dolist (itd (hash-table-values
		    (citeproc-proc-itemdata proc)))
	(setf (citeproc-itemdata-disamb-pos itd) citeproc-disambiguation-cite-pos)))
    (dolist (ctn ctns)
      (let* ((note-ind (citeproc-citation-note-index ctn))
	     (cites (citeproc-citation-cites ctn))
	     (single-cite (not (cdr cites))))
	(when note-ind (citeproc-cite--update-nn-queue near-note-ctns note-ind nnd))
	(let (seen-itds)
	  (while cites
	    (let* ((cite (car cites))
		   (itd (alist-get 'itd cite))
		   (locator (alist-get 'locator cite))
		   (label (alist-get 'label cite))
		   (pos (if (citeproc-itemdata-occurred-before itd)
			    (if (eq itd prev-itd)
				(if prev-loc
				    (if locator
					(if (and (citeproc-cite--loc-equal-p prev-loc locator)
						 (string= prev-label label))
					    'ibid
					  'ibid-with-locator)
				      'subsequent)
				  (if locator 'ibid-with-locator 'ibid))
			      'subsequent)
			  'first)))
	      (when (and note-ind
			 (or (citeproc-citations--itd-referred-p itd (queue-head near-note-ctns))
			     (memq itd seen-itds)))
		(setf (alist-get 'near-note (car cites)) t))
	      (setf (alist-get 'position (car cites)) pos
		    prev-itd itd
		    prev-loc locator
		    prev-label label)
	      (when (eq citeproc-disambiguation-cite-pos 'last)
		(citeproc-itd-update-disamb-pos itd pos))
	      (let ((prev-occurrence (citeproc-itemdata-occurred-before itd)))
		(if prev-occurrence
		    (unless (eq t prev-occurrence)
		      (setf (alist-get 'first-reference-note-number (car cites))
			    (number-to-string prev-occurrence)))
		  (setf (citeproc-itemdata-occurred-before itd) (or note-ind t))))
	      (push itd seen-itds)
	      (pop cites))))
	(unless single-cite
	  (setq prev-itd nil prev-loc nil prev-label nil))
	(when note-ind (queue-append near-note-ctns ctn))))))

(defun citeproc-proc-finalize (proc)
  "Finalize processor PROC by sorting and disambiguating items."
  (unless (citeproc-proc-finalized proc)
    (citeproc-proc-process-uncited proc)
    (citeproc-sb-add-subbib-info proc)
    (citeproc-sb-prune-unrendered proc)
    (citeproc-proc-update-sortkeys proc)
    (citeproc-proc-sort-itds proc)
    (citeproc-proc-update-positions proc)
    (citeproc-proc-disamb proc)
    (citeproc-proc-sort-cites proc)
    (citeproc-proc-apply-citation-modes proc)
    (citeproc-proc-group-and-collapse-cites proc)
    (setf (citeproc-proc-finalized proc) t)))

(defun citeproc-cite--first-namevar-cont (cite proc)
  "Return the first raw name-var node of CITE rendered with PROC."
  (citeproc-rt-find-first-node
   (citeproc-itd-rt-cite (alist-get 'itd cite) (citeproc-proc-style proc))
   (lambda (x)
     (and (consp x) (memq (alist-get 'rendered-var (car x))
			  citeproc--name-vars)))))

(defun citeproc-cite--first-datevar-cont (cite proc)
  "Return the first raw date-var node of CITE rendered with PROC."
  (citeproc-rt-find-first-node
   (citeproc-itd-rt-cite (alist-get 'itd cite) (citeproc-proc-style proc))
   (lambda (x)
     (and (consp x) (memq (alist-get 'rendered-var (car x))
			  citeproc--date-vars)))))
(provide 'citeproc-cite)

;;; citeproc-cite.el ends here
