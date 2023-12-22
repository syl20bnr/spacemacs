;;; citeproc.el --- A CSL 1.0.2 Citation Processor -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2023 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>
;; Maintainer: András Simonyi <andras.simonyi@gmail.com>
;; URL: https://github.com/andras-simonyi/citeproc-el
;; Keywords: bib
;; Package-Requires: ((emacs "26") (dash "2.13.0") (s "1.12.0") (f "0.18.0") (queue "0.2") (string-inflection "1.0") (org "9") (parsebib "2.4")(compat "28.1"))
;; Version: 0.9.3

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

;; citeproc-el is a library for rendering citations and bibliographies in styles
;; described in the Citation Style Language (CSL).

;;; Code:

(require 'dash)
(require 'queue)

(require 'citeproc-rt)
(require 'citeproc-locale)
(require 'citeproc-style)
(require 'citeproc-choose)
(require 'citeproc-generic-elements)
(require 'citeproc-context)
(require 'citeproc-itemdata)
(require 'citeproc-proc)
(require 'citeproc-cite)
(require 'citeproc-sort)
(require 'citeproc-formatters)
(require 'citeproc-itemgetters)
(require 'citeproc-subbibs)

;;; Public API

(defun citeproc-create (style it-getter loc-getter &optional loc force-loc)
  "Return a CSL processor for a given STYLE, IT-GETTER and LOC-GETTER.
STYLE is either a path to a CSL style file or a CSL style as a
  string.
IT-GETTER is an item-getter function that takes a list of itemid
  strings as its sole argument and returns an alist in which the
  given itemids are the keys and the values are the parsed csl
  json descriptions of the corresponding bibliography items (keys
  are symbols, arrays and hashes should be represented as lists
  and alists, respecively).
LOC-GETTER is a function that takes a locale string (e.g.
  \"en-GB\") as an argument and returns a corresponding parsed
  CSL locale (as parsed by Emacs's `libxml-parse-xml-region').
Optional LOC is the locale to use if the style doesn't specify a
  default one. Defaults to \"en-US\".
If optional FORCE-LOC is non-nil then use locale LOC even if
  STYLE specifies a different one as default. Defaults to nil."
  (let ((style (citeproc-create-style style loc-getter loc force-loc))
	(names (make-hash-table :test 'equal))
	(itemdata (make-hash-table :test 'equal))
	(citations (make-queue)))
    (citeproc-proc--create :style style :getter it-getter :names names 
			   :itemdata itemdata :citations citations :finalized t)))

(defun citeproc-append-citations (citations proc)
  "Append CITATIONS to the list of citations in PROC.
CITATIONS is a list of `citeproc-citation' structures."
  (let ((itemdata (citeproc-proc-itemdata proc))
	ids)
    ;; Collect new itemids
    (dolist (citation citations)
      (dolist (cite (citeproc-citation-cites citation))
	(push (alist-get 'id cite) ids)))
    (let* ((uniq-ids (delete-dups (nreverse ids))) ; reverse pushed ids
	   (new-ids (--remove (gethash it itemdata) uniq-ids)))
      ;; Add all new items in one pass
      (citeproc-proc-put-items-by-id proc new-ids)
      ;; Add itemdata to the cite structs and add them to the cite queue.
      (dolist (citation citations)
	(setf (citeproc-citation-cites citation)
	      (--map (cons (cons 'itd (gethash (alist-get 'id it) itemdata)) it)
		     (citeproc-citation-cites citation)))
	(queue-append (citeproc-proc-citations proc) citation))
      (setf (citeproc-proc-finalized proc) nil))))

(defun citeproc-add-uncited (itemids proc)
  "Add uncited bib items with ITEMIDS to PROC.
As an extension, an itemid can be the string \"*\" which has the
effect of adding all items available in the itemgetter."
  ;; We simply store the added ids here, real processing is performed when the
  ;; processor is finalized.
  (push itemids (citeproc-proc-uncited proc))
  (setf (citeproc-proc-finalized proc) nil))

(defun citeproc-add-subbib-filters (filters proc)
  "Add sub-bibliography FILTERS to PROC.
FILTERS should be a list of alists containing symbol keys and
string values, each pair describing an atomic condition to be
satisified by the printed entries. The following keys are
supported:
- `type': print only entries of the given type. Type is the
  bib(la)tex entry type if available, otherwise the CSL type is
  used as fallback;
- `nottype': print only entries not of the given type. Type is
  the bib(la)tex entry type if available, otherwise the CSL type
  is used as fallback;
- `csltype', `notcsltype': same as `type' and `nottype' but uses
  the entries' CSL type even if the bib(la)tex type is also
  available;
- `keyword': print only entries with the given keyword;
- `notkeyword': print only entries without the given keyword;
- `filter': print only entries for which the function named by
  the key returns a non-nil value."
  (setf (citeproc-proc-bib-filters proc) filters
	(citeproc-proc-finalized proc) nil))

(defun citeproc-render-citations (proc format &optional internal-links)
  "Render all citations in PROC in the given FORMAT.
Return a list of formatted citations.
  If the optional INTERNAL-LINKS is `bib-links' then link cites
to the bibliography regardless of the style type, if `no-links'
then don't add internal links, if nil or `auto' then add internal
links based on the style type (cite-cite links for note styles
and cite-bib links else). For legacy reasons, any other value is
treated as `no-links'."
  (citeproc-proc-finalize proc)
  (--map (citeproc-citation--render-formatted-citation it proc format internal-links)
	 (queue-head (citeproc-proc-citations proc))))

(defun citeproc-render-bib (proc format &optional internal-links
				 no-external-links bib-formatter-fun)
  "Render a bibliography of items in PROC in FORMAT.
For the optional INTERNAL-LINKS argument see
`citeproc-render-citations'. If the optional NO-EXTERNAL-LINKS is
non-nil then don't generate external links. If the optional
BIB-FORMATTER-FUN is given then it will be used to join the
bibliography items instead of the content of the chosen
formatter's `bib' slot (see `citeproc-formatter' for details).

Returns an error message string if the style of PROC doesn't
contain a bibliography section. Otherwise it returns
a (FORMATTED-BIBLIOGRAPHY . FORMATTING-PARAMETERS) cons cell,
where FORMATTED-BIBLIOGRAPHY is either a single bibliography or a
list of sub-bibliograhies if filters were added to the processor,
and FORMATTING-PARAMETERS is an alist containing the following
formatting parameters keyed to the parameter names as symbols:
- `max-offset' (integer): The width of the widest first field in
  the bibliography, measured in characters.
- `line-spacing' (integer): Vertical line distance specified as a
  multiple of standard line height.
- `entry-spacing' (integer): Vertical distance between
  bibliographic entries, specified as a multiple of standard line
  height.
- `second-field-align' (`flush'or `margin'): The position of
  second-field alignment.
- `hanging-indent' (boolean): Whether the bibliography items should
  be rendered with hanging-indents."
  (if (null (citeproc-style-bib-layout (citeproc-proc-style proc)))
      "[NO BIBLIOGRAPHY LAYOUT IN CSL STYLE]"
    (citeproc-proc-finalize proc)
    (let* ((formatter (citeproc-formatter-for-format format))
	   (rt-formatter (citeproc-formatter-rt formatter))
	   (bib-formatter (or bib-formatter-fun
			      (citeproc-formatter-bib formatter)))
	   (bibitem-formatter (citeproc-formatter-bib-item formatter))
	   (formatter-no-external-links (citeproc-formatter-no-external-links
					 formatter))
	   (style (citeproc-proc-style proc))
	   (bib-opts (citeproc-style-bib-opts style))
	   (punct-in-quote (string= (alist-get 'punctuation-in-quote
					       (citeproc-style-locale-opts style))
				    "true"))
	   (itemdata (citeproc-proc-itemdata proc))
	   (filters (citeproc-proc-bib-filters proc)))
      ;; Render raw bibitems for each itemdata struct and store them in the
      ;; `rawbibitem' slot.
      (maphash (lambda (_ itd)
		 (setf (citeproc-itemdata-rawbibitem itd)
		       (citeproc-rt-finalize
			(citeproc-render-varlist-in-rt
			 (citeproc-itemdata-varvals itd)
			 style 'bib 'display internal-links
			 (or formatter-no-external-links no-external-links))
			punct-in-quote)))
	       itemdata)
      (let* ((raw-bib
	      (if (citeproc-proc-filtered-bib-p proc)
		  ;; There are filters, we need to select and sort the subbibs.
		  (let* ((nr-of-filters (length filters))
		 	 (result (make-list nr-of-filters nil))
			 ;; We store boolean to-be-sorted flags for each sub-bib
			 (to-be-sorted (make-bool-vector nr-of-filters nil))
			 (bib-sort (citeproc-style-bib-sort (citeproc-proc-style proc))))
		    ;; Put the itds into subbib lists.
		    (maphash
		     (lambda (_ itd)
		       (let ((subbib-nos (citeproc-itemdata-subbib-nos itd)))
			 ;; Set to-be-sorted for later subbibs if itemdata
			 ;; occcurs in more than one.
			 (when-let ((later-subbib-nos (cdr subbib-nos)))
			   (dolist (subbib-no later-subbib-nos)
			     (setf (elt to-be-sorted subbib-no) t)))
			 ;; Push the item in all corresponding subbibs.
			 (dolist (subbib-no subbib-nos)
			   (push itd (elt result subbib-no)))))
		     itemdata)
		    ;; Sort the itds in each individual list
		    (setq result
			  (--map-indexed
			   (if (and bib-sort (elt to-be-sorted it-index))
			       ;; Subbib contains earlier item, needs to sorted.
			       (citeproc-sort-itds it (citeproc-style-bib-sort-orders
						       (citeproc-proc-style proc)))
			     ;; No earlier item, sorting on citation-number.
			     (citeproc-sort-itds-on-citnum it))
			   result))
		    ;; Generate the raw bibs.
		    (--map (mapcar #'citeproc-itemdata-rawbibitem it) result))
		;; No filters, so raw-bib is a list containing a single raw bibliograhy.
		(list (mapcar #'citeproc-itemdata-rawbibitem
			      (citeproc-sort-itds-on-citnum (hash-table-values itemdata))))))
	     ;; Perform author-substitution.
	     (substituted
	      (-if-let (subs-auth-subst
			(alist-get 'subsequent-author-substitute bib-opts))
		  (--map (citeproc-rt-subsequent-author-substitute it subs-auth-subst)
			 raw-bib)
		raw-bib))
	     ;; Calculate formatting params.
	     ;; NOTE: This is the only place where we check whether there are
	     ;; bibliography items in the processor, even though the empty case
	     ;; could be handled way more efficiently. 
	     (max-offset (if (and (alist-get 'second-field-align bib-opts)
				  (not (hash-table-empty-p itemdata)))
			     (citeproc-rt-max-offset itemdata)
			   0))
	     (format-params (cons (cons 'max-offset max-offset)
				  (citeproc-style-bib-opts-to-formatting-params bib-opts)))
	     (formatted-bib
	      (--map (funcall bib-formatter
			      (mapcar
			       (lambda (x)
				 (funcall
				  bibitem-formatter
				  (funcall
				   rt-formatter (citeproc-rt-cull-spaces-puncts x))
				  format-params))
			       it)
			      format-params)
		     substituted)))
	;; Generate final return value.
	(cons (if filters formatted-bib (car formatted-bib))
	      format-params)))))

(defun citeproc-clear (proc)
  "Remove all bibliographic and citation data from PROC."
  (clrhash (citeproc-proc-itemdata proc))
  (clrhash (citeproc-proc-names proc))
  (queue-clear (citeproc-proc-citations proc))
  (setf (citeproc-proc-finalized proc) t))

;; For one-off renderings

(defun citeproc-create-style (style locale-getter &optional locale force-locale)
  "Compile style in STYLE into a citeproc-style struct.
STYLE is either a path to a CSL style file, or a style as a
string. LOCALE-GETTER is a getter function for locales, the
optional LOCALE is a locale to prefer. If FORCE-LOCALE is non-nil
then use LOCALE even if the style's default locale is different."
  (-let* (((year-suffix . parsed-style) (citeproc-style-parse style))
	  (default-locale (alist-get 'default-locale (cadr parsed-style)))
	  (preferred-locale (if force-locale locale (or default-locale
							locale
							"en-US")))
	  (act-parsed-locale (funcall locale-getter preferred-locale))
	  (act-locale (alist-get 'lang (cadr act-parsed-locale)))
	  (style (citeproc-create-style-from-locale
		  parsed-style
		  (not (not year-suffix)) act-locale)))
    (citeproc-style--update-locale style act-parsed-locale)
    (citeproc-style--set-opt-defaults style)
    (setf (citeproc-style-locale style) (or locale act-locale))
    style))

;; REVIEW: this should be rethought -- should we apply the specific wrappers as
;; well?
(defun citeproc-render-item (item-data style mode format
				       &optional no-external-links)
  "Render a bibliography item described by ITEM-DATA with STYLE.
ITEM-DATA is the parsed form of a bibliography item description
  in CSL-JSON format,
STYLE is a `citeproc-style' structure,
MODE is one of the symbols `bib' or `cite',
FORMAT is a symbol representing a supported output format.
If the optional NO-EXTERNAL-LINKS is non-nil then don't generate
external links in the item."
  (let ((internal-varlist (--map-when (memq (car it) citeproc--date-vars)
				      (cons (car it)
					    (citeproc-date-parse (cdr it)))
				      item-data)))
    (funcall (citeproc-formatter-rt (citeproc-formatter-for-format format))
	     (citeproc-rt-cull-spaces-puncts
	      (citeproc-rt-finalize
	       (citeproc-render-varlist-in-rt
		internal-varlist style mode 'display 'no-links no-external-links))))))

(provide 'citeproc)

;;; citeproc.el ends here
