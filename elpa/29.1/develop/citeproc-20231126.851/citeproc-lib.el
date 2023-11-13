;;; citeproc-lib.el --- misc functions and variables for citeproc-el -*- lexical-binding: t; -*-

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

;; General utility functions used in citeproc-el.

;;; Code:

(require 'dash)
(require 's)

(defconst citeproc--number-vars
  '(chapter-number citation-number collection-number edition first-reference-note-number
		   issue number number-of-pages number-of-volumes page page-first
		   part-number printing-number section supplement-number version volume)
  "CSL number variables."
  ;; Note: `locator', which is also on the official list, is omitted because
  ;; it's treated specially in the code.
  )

(defconst citeproc--date-vars
  '(accessed available-date event-date issued original-date submitted)
  "CSL date variables.")

(defconst citeproc--name-vars
  '(author chair collection-editor compiler composer container-author contributor
	   curator director editor editorial-director editor-translator executive-producer
	   guest host illustrator interviewer narrator organizer original-author
	   performer producer recipient reviewed-author script-writer series-creator
	   translator)
  "CSL name variables.")

(defconst citeproc--linked-vars
  '(DOI PMCID PMID URL)
  "Variables whose rendered content should be linked.
The ordering is according to priority ")

(defconst citeproc--link-prefix-alist
  '((DOI .  "https://doi.org/")
    (PMID . "https://www.ncbi.nlm.nih.gov/pubmed/")
    (PMCID . "https://www.ncbi.nlm.nih.gov/pmc/articles/"))
  "Alist mapping variable names to uri prefixes.")

(defun citeproc-lib-parse-xml-file (file)
  "Return the parsed xml representation of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun citeproc-lib-remove-xml-comments (tree)
  "Remove comments from xml TREE."
  (let ((head (car tree))
	(attrs (cadr tree))
	(body (cddr tree))
	result)
    (while body
      (pcase (car body)
	((pred atom) (push (pop body) result))
	(`(comment . ,_) (pop body))
	(_ (push (citeproc-lib-remove-xml-comments (pop body)) result))))
    (let ((full-result (cons head (cons attrs (nreverse result)))))
      ;; Handle the problem of the top element added by the libxml parser when
      ;; there is a comment after the xml declaration.
      (if (eq (car full-result) 'top)
	  (caddr full-result)
	full-result))))

(defun citeproc-lib-parse-html-frag (s)
  "Return the parsed representation of html in string S."
  (with-temp-buffer
    (insert s)
    (libxml-parse-html-region (point-min) (point-max))))

(defun citeproc-lib-intern (s)
  "Intern S if it is a string, return nil if it is nil."
  (cond ((not s) nil)
	((stringp s) (intern s))
	(t (error "Function citeproc-lib-intern was called with an argument that is neither string nor nil"))))

(defun citeproc-lib-named-parts-to-alist (e)
  "Collect the attrs of parsed xml element E's enclosed elements.
The attributes are collected into an alist consisting
of (PARTNAME . ATTRS) pairs, where PARTNAME is the value of the
enclosed element's `name' attr"
  (--map (cons (intern (alist-get 'name it))
	       (assq-delete-all 'name it))
	 (mapcar #'cadr (-remove #'stringp (cddr e)))))

(defun citeproc-lib-lex-compare (l1 l2 cmp-fun sort-orders)
  "Whether list L1 lexicographically precedes list L2.
CMP-FUN is a three-valued (1, 0, -1) comparator function,
SORT-ORDERS is a list of sort orders (see the bib- and
cite-sort-orders slots of `citeproc-style' for details). Return t
iff L1 is strictly ordered before L2, nil otherwise."
  (unless sort-orders
    (setq sort-orders (make-list (length l1) t)))
  (let (result)
    (while (and l1 (not result))
      (let ((comp
	     (funcall cmp-fun (pop l1) (pop l2) (not (pop sort-orders)))))
	(unless (= comp 0)
	  (setq result comp))))
    (equal result 1)))

(defun citeproc-lib-splice-into (list tag)
  "Splice elements with car TAG into LIST."
  (let (result)
    (dolist (elt list)
      (if (and (consp elt) (eq tag (car elt)))
	  (dolist (e (cdr elt))
	    (push e result))
	(push elt result)))
    (nreverse result)))

(defun citeproc-lib-add-splice-tag (list tag)
  "Add TAG as car if LIST is not a singleton.
Return the first element if LIST is singleton."
  (if (cdr list) (cons tag list) (car list)))

(defun citeproc-lib-numeric-p (val)
  "Return whether VAL is numeric content.
VAL has numeric content if it is a number or a string containing
numeric content."
  (or (numberp val)
      (and (stringp val)
	   (s-matches-p "\\`[[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\(\\( *\\([,&-]\\|--\\) *\\)?[[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\)?\\'"
			val))))

(defun citeproc-lib-maybe-stop-rendering
    (trigger context result &optional var)
  "Stop rendering if a (`stop-rendering-at'. TRIGGER) pair is present in CONTEXT.
In case of stopping return with RESULT. If the optional VAR
symbol is non-nil then rendering is stopped only if VAR is eq to
TRIGGER."
  (if (and (eq trigger (alist-get 'stop-rendering-at (citeproc-context-vars context)))
	   (or (not var) (eq var trigger))
	   (eq (cdr result) 'present-var))
      (let ((rt-result (car result)))
	(push '(stopped-rendering . t) (car rt-result))
	(throw 'stop-rendering (citeproc-rt-render-affixes rt-result)))
    result))

(provide 'citeproc-lib)

;;; citeproc-lib.el ends here
