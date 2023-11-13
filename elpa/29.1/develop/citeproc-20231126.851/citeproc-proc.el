;;; citeproc-proc.el --- construct and manage citation processors -*- lexical-binding: t; -*-

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

;; Structure type and functions to construct citation processor objects, add or
;; clear stored bibliography items, and disambiguate and finalize the rendering
;; of the stored items.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'queue)
(require 's)

(require 'citeproc-date)
(require 'citeproc-itemdata)
(require 'citeproc-disamb)

(cl-defstruct (citeproc-proc (:constructor citeproc-proc--create))
  "Citation processor struct.
STYLE is a `citeproc-style' struct,
GETTER is a bibliography item getter,
ITEMDATA is a hash table that maps itemids to `citeproc-itemdata'
  structs,
CITATIONS is a queue containing citations,
NAMES is hash table that maps name alists to ids,
FINALIZED is non-nil iff the processor is finalized
  (bibliography items are properly sorted, citation positions are
  updated etc),
UNCITED is a list of lists of uncited itemids to be added during
  finalization,
BIB-FILTERS is a list of filters defining sub-bibliographies."
  style getter itemdata citations names finalized uncited bib-filters)

(defun citeproc-proc--internalize-name (name proc)
  "Find or add name-alist NAME in/to the names stored in PROC.
Return an internalized version which contains the name-id, and is
sorted."
  (let* ((sorted (sort name (lambda (x y)
			      (string< (car x) (car y)))))
	 (names (citeproc-proc-names proc))
	 (val (gethash sorted names)))
    (cons (cons 'name-id
		(or val (puthash sorted (hash-table-count names) names)))
	  sorted)))

(defconst citeproc-proc--nonstd-csl-vars-alist
  '((shortTitle . title-short) (journalAbbreviation . container-title-short))
  "Alist mapping non-standard citeproc.js vars to their standard CSL peers.")

(defun citeproc-proc--internalize-item (proc item)
  "Return the internal form of a CSL json ITEM for PROC."
  (let* (label
	 page-first
	 (result
	  (--map
	   (let* ((orig-var (car it))
		  (var (-if-let (mapped (alist-get orig-var
						   citeproc-proc--nonstd-csl-vars-alist))
			   mapped
			 orig-var))
		  (value (citeproc-proc--parse-csl-var-val (cdr it) var proc)))
	     (pcase var
	       ('page (-when-let (page-first-match (s-match "[[:digit:]]+" value))
			(setq page-first (car page-first-match))))
	       ('label (setq label t)))
	     (cons var value))
	   item)))
    (when page-first (push (cons 'page-first page-first) result))
    (unless label (push (cons 'label "page") result))
    ;; Generate the editor-translator variable if needed
    ;; (required by CSL 1.02)
    (when-let* (((null (alist-get 'editor-translator result)))
		(editor (alist-get 'editor result))
		(translator (alist-get 'translator result))
		((string= (alist-get 'name-id editor)
			  (alist-get 'name-id translator))))
      (push (cons 'editor-translator editor) result))
    result))

(defun citeproc-proc--put-item (proc item itemid &optional uncited)
  "Put parsed csl-json ITEM with ITEMID into PROC.
Return the added itemdata structure."
  (let* ((int-vars (citeproc-proc--internalize-item proc item))
	 (itemdata (citeproc-itemdata-create :varvals int-vars :uncited uncited)))
    (citeproc-proc-put-itd-put itemid itemdata proc)
    (citeproc-itd-setvar itemdata 'citation-number
			 (number-to-string (hash-table-count
					    (citeproc-proc-itemdata proc))))
    (setf (citeproc-proc-finalized proc) nil)
    itemdata))

(defun citeproc-proc-put-item-by-id (proc itemid)
  "Put item with ITEMID into the itemlist of PROC.
Return the itemdata struct that was added."
  (let ((item (cdar (funcall (citeproc-proc-getter proc)
			     (list itemid)))))
    (citeproc-proc--put-item proc
			     (or item `((unprocessed-with-id . ,itemid)))
			     itemid)))

(defun citeproc-proc-put-items-by-id (proc itemids)
  "Add items with ITEMIDS into the itemlist of PROC."
  (let* ((received (funcall (citeproc-proc-getter proc) itemids))
	 ;; OPTIMIZE: putting the received items into the original order could/should be
	 ;; made more efficient
	 (items (--map (cons it (assoc-default it received))
		       itemids)))
    (cl-loop for (itemid . item) in items do
	     (citeproc-proc--put-item proc
				      (or item `((unprocessed-with-id . ,itemid)))
				      itemid))))

(defun citeproc-proc-put-itd-put (id data proc)
  "Put the DATA of item with ID in processor PROC."
  (let ((itemdata (citeproc-proc-itemdata proc)))
    (puthash id data itemdata)))

(defun citeproc-proc-process-uncited (proc)
  "Add uncited items to the itemdata in PROC."
  (when-let ((unciteds (citeproc-proc-uncited proc)))
   (let* ((itemids (cl-delete-duplicates (apply #'append unciteds))))
     (when (member "*" itemids)
       (setq itemids (funcall (citeproc-proc-getter proc) 'itemids)))
     (let* ((itemdata (citeproc-proc-itemdata proc))
	    (new-ids (--remove (gethash it itemdata) itemids))
	    (id-items (funcall (citeproc-proc-getter proc) new-ids)))
       (pcase-dolist (`(,id . ,item) id-items)
	 (citeproc-proc--put-item
	  proc
	  (or item `((unprocessed-with-id . ,id)))
	  id t))))))

(defun citeproc-proc-delete-occurrence-info (proc)
  "Remove all itemdata occurrence info from PROC."
  (maphash (lambda (_ itd)
	     (setf (citeproc-itemdata-occurred-before itd) nil))
	   (citeproc-proc-itemdata proc)))

(defun citeproc-proc--parse-csl-json-name (rep)
  "Parse the json representation REP of a csl name variable."
  (if-let ((literal (alist-get 'literal rep)))
      (list (cons 'family (citeproc-s-smart-apostrophes literal)))
    (let ((filtered (-remove (lambda (x) (eq (car x) 'isInstitution)) rep)))
      (--map (cons
	      (car it)
	      (let ((text-field (cdr it)))
		(if (stringp text-field)
		    (citeproc-s-smart-apostrophes text-field)
		  text-field)))
	     filtered))))

(defun citeproc-proc--parse-csl-var-val (rep var proc)
  "Parse the json representation REP of csl variable VAR.
VAR is a csl variable as symbol;
REP is its value in standard csl json representation as parsed by
  the Emacs `json' library;
PROC is the target citeproc-processor of the internal representation.
Return the PROC-internal representation of REP."
  (cond ((memq var citeproc--name-vars)
	 (--map (citeproc-proc--internalize-name
		 (citeproc-proc--parse-csl-json-name it)
		 proc)
	  rep))
	((memq var citeproc--date-vars)
	 (citeproc-date-parse rep))
	;;FIXME: We handle here the id... do we need it in the itemdata at all?
	((or (memq var citeproc--number-vars) (eq 'id var))
	 (citeproc-s-from-num-or-s rep))
	((stringp rep)
	 (let* ((w-aposts (citeproc-s-smart-apostrophes rep))
		(rt (citeproc-rt-from-str w-aposts)))
	   (if (s-contains-p "\"" rep)
	       (let* ((terms (citeproc-style-terms (citeproc-proc-style proc)))
		      (oq (citeproc-term-text-from-terms "open-quote" terms))
		      (cq (citeproc-term-text-from-terms "close-quote" terms)))
		 (citeproc-rt-change-case
		  rt (lambda (x) (citeproc-s-smart-quotes x oq cq))))
	     rt)))
	(t rep)))

(defun citeproc-proc-disamb (proc)
  "Disambiguate the items stored in PROC."
  (let* ((cite-opts (citeproc-style-cite-opts (citeproc-proc-style proc)))
	 (name (string= "true" (alist-get 'disambiguate-add-names cite-opts)))
	 (given (string= "true" (alist-get 'disambiguate-add-givenname cite-opts)))
	 (yearsuff (string= "true" (alist-get 'disambiguate-add-year-suffix cite-opts))))
    (citeproc-disamb-itds (hash-table-values (citeproc-proc-itemdata proc))
			  (citeproc-proc-style proc)
			  name given yearsuff)))

(defun citeproc-proc-byte-compile (proc)
  "Byte-compile all lambdas in PROC."
  (let* ((style (citeproc-proc-style proc))
	 (bib-sort (citeproc-style-bib-sort style))
	 (cite-sort (citeproc-style-cite-sort style)))
    (setf (citeproc-style-macros style)
	  (--map (cons (car it) (byte-compile (cdr it)))
		 (citeproc-style-macros style))
	  (citeproc-style-cite-layout style)
	  (byte-compile (citeproc-style-cite-layout style))
	  (citeproc-style-bib-layout style)
	  (byte-compile (citeproc-style-bib-layout style)))
    (when bib-sort (setf (citeproc-style-bib-sort style) (byte-compile bib-sort)))
    (when cite-sort (setf (citeproc-style-cite-sort style) (byte-compile cite-sort)))))

(defun citeproc-proc-filtered-bib-p (proc)
  "Return whether PROC has nontrivial filters"
  (let ((filters (citeproc-proc-bib-filters proc)))
    (and filters (not (equal filters '(nil))))))

(provide 'citeproc-proc)

;;; citeproc-proc.el ends here
