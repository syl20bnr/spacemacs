;;; citeproc-bibtex.el --- convert BibTeX entries to CSL -*- lexical-binding: t; -*-

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

;; Convert BibTeX bibliography entries to CSL.

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'bibtex)
(require 'cl-lib)
(require 's)
(require 'org)
(require 'map)
;; Handle the fact that org-bibtex has been renamed to ol-bibtex -- for the time
;; being we support both feature names.
(or (require 'ol-bibtex nil t)
    (require 'org-bibtex))

(require 'citeproc-s)

;; Declare used ol-bibtex variables and functions to silence 'reference to free
;; variable' and 'function is not known to be defined' warnings during
;; compilation.
(defvar org-bibtex-key-property)
(defvar org-bibtex-type-property-name)
(defvar org-bibtex-export-arbitrary-fields)
(defvar org-bibtex-prefix)
(defvar org-bibtex-types)
(declare-function org-bibtex-get "ext:ol-bibtex")

(defconst citeproc-bt--to-csl-types-alist
  '(("article" . "article-journal") ("book" . "book") ("proceedings" . "book")
    ("manual" . "book") ("periodical" . "book") ("booklet" . "pamphlet")
    ("inbook" . "chapter") ("incollection" . "chapter") ("inproceedings" . "paper-conference")
    ("conference" . "paper-conference") ("mastersthesis" . "thesis") ("phdthesis" . "thesis")
    ("techreport" . "report") ("patent" . "patent") ("electronic" . "webpage")
    ("misc" . "article") ("other" . "article") ("standard" . "legislation")
    ("unpublished" . "manuscript") ("online" . "article-journal"))
  "Alist mapping BibTeX item types to CSL item types.")

(defconst citeproc-bt--to-csl-keys-alist
  '(("=key=" . citation-label) ("address" . publisher-place)
    ("booktitle" . container-title) ("journal" . container-title)
    ("chapter" . title) ("location" . event-place) ("series" . collection-title)
    ("keywords" . keyword) ("institution" . publisher) ("school" . publisher)
    ("pages" . page) ("organization" . publisher) ("url" . URL)
    ("doi" . DOI) ("pmid" . PMID) ("pmcid" . PMCID))
  "Alist mapping BibTeX keys to CSL keys with different names.")

(defconst citeproc-bt--mon-to-num-alist
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12))
  "Alist mapping LaTeX abbreviated month names to ordinals.")

(defconst citeproc-bt--pref-to-ucs-alist
  '(("'" . "ACUTE") ("`" . "GRAVE") ("^" . "CIRCUMFLEX") ("~" . "TILDE")
    ("=" . "MACRON") ("." . "WITH DOT ABOVE") ("\"" . "DIAERESIS")
    ("''" . "DIAERESIS") ("H" . "DOUBLE ACUTE") ("r" . "WITH RING ABOVE")
    ("u" . "BREVE") ("c" . "CEDILLA") ("k" . "OGONEK") ("v" . "CARON"))
  "Alist mapping LaTeX prefixes to unicode name endings.")

(defconst citeproc-bt--comm-letter-to-ucs-alist
  '((("`" . "A") . "À")
    (("'" . "A") . "Á")
    (("^" . "A") . "Â")
    (("~" . "A") . "Ã")
    (("\"" . "A") . "Ä")
    (("r" . "A") . "Å")
    (("c" . "C") . "Ç")
    (("v" . "C") . "Č")
    (("'" . "C") . "Ć")
    (("`" . "E") . "È")
    (("'" . "E") . "É")
    (("^" . "E") . "Ê")
    (("\"" . "E") . "Ë")
    (("`" . "I") . "Ì")
    (("'" . "I") . "Í")
    (("^" . "I") . "Î")
    (("\"" . "I") . "Ï")
    (("~" . "N") . "Ñ")
    (("`" . "O") . "Ò")
    (("'" . "O") . "Ó")
    (("^" . "O") . "Ô")
    (("~" . "O") . "Õ")
    (("\"" . "O") . "Ö")
    (("c" . "S") . "Ş")
    (("v" . "S") . "Š")
    (("`" . "U") . "Ù")
    (("'" . "U") . "Ú")
    (("^" . "U") . "Û")
    (("\"" . "U") . "Ü")
    (("'" . "Y") . "Ý")
    (("`" . "a") . "à")
    (("'" . "a") . "á")
    (("^" . "a") . "â")
    (("~" . "a") . "ã")
    (("\"" . "a") . "ä")
    (("r" . "a") . "å")
    (("c" . "c") . "ç")
    (("v" . "c") . "č")
    (("'" . "c") . "ć")
    (("`" . "e") . "è")
    (("'" . "e") . "é")
    (("^" . "e") . "ê")
    (("\"" . "e") . "ë")
    (("`" . "i") . "ì")
    (("'" . "i") . "í")
    (("^" . "i") . "î")
    (("\"" . "i") . "ï")
    (("~" . "n") . "ñ")
    (("`" . "o") . "ò")
    (("'" . "o") . "ó")
    (("^" . "o") . "ô")
    (("~" . "o") . "õ")
    (("\"" . "o") . "ö")
    (("v" . "r") . "ř")
    (("c" . "s") . "ş")
    (("v" . "s") . "š")
    (("`" . "u") . "ù")
    (("'" . "u") . "ú")
    (("^" . "u") . "û")
    (("\"" . "u") . "ü")
    (("'" . "y") . "ý")
    (("\"" . "y") . "ÿ")
    (("H" . "o") . "ő")
    (("H" . "O") . "Ő")
    (("H" . "u") . "ű")
    (("H" . "U") . "Ű")
    (("v" . "z") . "ž")
    (("v" . "Z") . "Ž"))
  "Alist mapping LaTeX (SYMBOL-COMMAND . ASCII-CHAR) pairs to unicode characters.")

(defconst citeproc-bt--to-ucs-alist
  '(("l" . "ł") ("L" . "Ł") ("o" . "ø") ("O" . "Ø") ("AA" . "Å") ("aa" . "å")
    ("AE" . "Æ") ("ae" . "æ") ("ss" . "ß") ("i" . "ı"))
  "Alist mapping LaTeX commands to characters.")

(defun citeproc-bt--to-ucs (ltx char)
  "Return the unicode version of LaTeX command LTX applied to CHAR.
LTX is a one-char LaTeX accenting command (e.g. \"'\"), CHAR is
an ascii character. Return nil if no corresponding unicode
character was found."
  (or (assoc-default (cons ltx char) citeproc-bt--comm-letter-to-ucs-alist)
      ;; If the combination is not in citeproc-bt--comm-letter-to-ucs-alist
      ;; then, as a last resort, we try to assemble the canonical unicode name
      ;; of the requested character and look it up in (usc-names). This process
      ;; can be *very slow* on older Emacs versions in which (usc-names) returns
      ;; an alist!

      ;; NOTE: Because of a nasty interaction bug between `ucs-names' and
      ;; `replace-regexp-in-string' we do the lookup only for Emacs versions
      ;; earlier than 28.
      (when-let* (((version< emacs-version "28" ))
		  (case-name (if (s-lowercase-p char) "SMALL" "CAPITAL"))
		  (combining-name (assoc-default ltx citeproc-bt--pref-to-ucs-alist))
		  (name (concat "LATIN " case-name " LETTER "
				(upcase char) " " combining-name))
		  (char-name (map-elt (ucs-names) name)))
	(char-to-string char-name))))

(defconst citeproc-bt--decode-rx
  (rx (or
       (seq "{\\" (group-n 1 (in "'" "`" "^" "~" "=" "." "\"")) (0+ space)
	    (group-n 2 letter) "}")
       (seq "{\\" (group-n 1 (in "H" "r" "u" "c" "k" "v")) (1+ space)
	    (group-n 2 letter) "}")
       (seq "{\\" (group-n 1 (or "l" "L" "o" "O" "AA" "aa" "ae" "AE" "ss" "i"))
	    (0+ space) "}")
       (seq "\\" (group-n 1 (in "'" "`" "^" "~" "=" "." "\"" "H" "r" "u" "c" "k" "v"))
	    (0+ space) "{" (group-n 2 letter) "}")
       (seq "\\" (group-n 1 (in "H" "r" "u" "c" "k" "v")) (1+ space)
	    (group-n 2 letter))
       (seq "\\" (group-n 1 (in "'" "`" "^" "~" "=" "." "\"")) (0+ space)
	    (group-n 2 letter))
       (seq "\\" (group-n 1 (or "l" "L" "o" "O" "AA" "aa" "ae" "AE" "ss" "i"))
	    word-boundary)))
  "Regular expression matching BibTeX special character commands.")

(defun citeproc-bt--decode (s)
  "Decode a BibTeX encoded string."
  (replace-regexp-in-string
   citeproc-bt--decode-rx
   (lambda (x)
     (let ((command (match-string 1 x))
	   (letter (match-string 2 x)))
       (if letter
	   (or (citeproc-bt--to-ucs command letter) (concat "\\" x))
	 (or (assoc-default command citeproc-bt--to-ucs-alist) x))))
   s t t))

(defun citeproc-bt--decode-buffer ()
  "Decode BibTeX encoded characters in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward citeproc-bt--decode-rx nil t)
    (replace-match
     (let ((replacement (let ((command (match-string 1))
			      (letter (match-string 2)))
			  (if letter
			      (or (citeproc-bt--to-ucs command letter) (concat "\\" (match-string 0)))
			    (assoc-default command citeproc-bt--to-ucs-alist)))))
       replacement))))

(defconst citeproc-bt--command-rx
  (rx "\\" (1+ (any "a-z" "A-Z")) "{"	; \TEX-COMMAND{
      (group (*? anything))		; ARGUMENT
      "}"				; }
      ))

(defconst citeproc-bt--command-wo-arg-rx
  (rx "\\" (1+ (any "a-z" "A-Z")) word-end)) ; \TEX-COMMAND + word-end

(defconst citeproc-bt--braces-rx
  (rx "{" (group (*? anything)) "}")) 	; {TEXT}

(defun citeproc-bt--process-brackets (s &optional lhb rhb)
  "Process LaTeX curly brackets in string S.
Optional LHB and RHB specify what to substitute for left and
right braces when they are not enclosing command arguments.
The default is to remove them."
  (let* ((result s)
	 (match t))
    (while match
      (cond ((string-match citeproc-bt--command-rx result)
	     (setq result (replace-match "\\1" t nil result)
		   match t))
	    ((string-match citeproc-bt--command-wo-arg-rx result)
	     (setq result (replace-match "" t nil result)
		   match t))
	    ((string-match citeproc-bt--braces-rx result)
	     (setq result (replace-match
			   (concat lhb "\\1" rhb)
			   t nil result)
		   match t))
	    (t (setq match nil))))
    result))

(defun citeproc-bt--preprocess-for-decode (s)
  "Preprocess field S before decoding.
Remove flanking dumb quotes from string S and make some
replacements."
  (let ((wo-quotes (if (and (string= (substring s 0 1) "\"")
			    (string= (substring s -1) "\""))
		       (substring s 1 -1) s)))
    (citeproc-s-replace "\\&" "&" wo-quotes)))

(defun citeproc-bt--to-csl (s &optional with-nocase)
  "Convert a BibTeX field S to a CSL one.
If optional WITH-NOCASE is non-nil then convert BibTeX no-case
brackets to the corresponding CSL XML spans."
  (if (> (length s) 0)
      (--> s
	   (citeproc-bt--preprocess-for-decode it)
	   (citeproc-bt--decode it)
	   (citeproc-bt--process-brackets
	    it
	    (when with-nocase "<span class=\"nocase\">")
	    (when with-nocase "</span>"))
	   (citeproc-s-replace-all-seq it '(("\n" . " ") ("~" . " ") ("--" . "–")))
	   (s-trim it))
    s))

(defun citeproc-bt--to-csl-names (n)
  "Return a CSL version of BibTeX names field N."
  (let ((name-fields (s-split "\\band\\b" n)))
    (mapcar
     (lambda (x)
       (let ((trimmed (s-trim x)))
	 (cond
	  ((string= trimmed "") '((family . "")))
	  ;; Presence of equal sign signals extended specification.
	  ((string-match "=" trimmed) (citeproc-bt--ext-desc-to-csl-name trimmed))
	  ;; Brackets indicate corporate entities without name parts.
	  ((and (string= "{" (substring trimmed 0 1))
		(string= "}" (substring trimmed -1)))
	   `((literal . ,(citeproc-bt--to-csl (substring trimmed 1 -1)))))
	  ;; Else standard bib(la)tex name field processing.
	  (t (citeproc-bt--to-csl-name (citeproc-bt--to-csl trimmed))))))
     name-fields)))

(defvar citeproc-bt-dropping-particles
  '("dela" "il" "sen" "z" "ze")
  "List containing dropping particles. Particles whose first word
is not on this list are classified as non-dropping.")

(defun citeproc-bt--parse-family (f)
  "Parse family name tokens F into a csl name-part alist."
  (let (family result particle)
    (-if-let (firsts (butlast f))
	(progn
	  (while (and firsts (s-lowercase-p (car firsts)))
	    (push (pop firsts) particle))
	  (when particle
	    (push (cons (if (member (car particle) citeproc-bt-dropping-particles)
			    'dropping-particle
			  'non-dropping-particle)
			(nreverse particle))
		  result))
	  (setq family (-concat firsts (last f))))
      (setq family f))
    (push `(family . ,family) result)
    result))

(defun citeproc-bt--parse-attr-val-field (f)
  "Parse biblatex key-val field F into an alist."
  (let* ((bracketless (replace-regexp-in-string "[{}]" "" f))
	 (equal-split (split-string bracketless "=" t " "))
	 (first-attr (intern (string-trim (pop equal-split) "[ \"]+" "[ \"]+")))
	 (reversed (nreverse equal-split))
	 (result (list (string-trim (pop reversed) "[ \"]+" "[ \"]+"))))
    (dolist (elt reversed)
      (let ((pos (- (length elt) 2))
	    found)
	(while (and (< 0 pos) (null found))
	  (if (= (aref elt pos) ?,)
	      (setq found t)
	    (cl-decf pos)))
	(unless found (error "Could not parse biblatex key-value list \"%s\"" f))
	(let ((key (intern (s-trim (substring elt (1+ pos)))))
	      (val (string-trim (substring elt 0 pos) "[ \"]+" "[ \"]+")))
	  (push key (car result))
	  (push val result))))
    (push first-attr (car result))
    result))

(defun citeproc-bt--ext-desc-to-csl-name (name)
  "Return a CSL version of extended biblatex description NAME."
  (let* ((parsed (citeproc-bt--parse-attr-val-field name))
	 (dropping (string= (alist-get 'useprefix parsed) "false")))
    (--keep (pcase (car it)
	      ((or 'family 'given 'suffix) it)
	      ('prefix (cons (if dropping 'dropping-particle 'non-dropping-particle)
			     (cdr it)))
	      (_ nil))
	    parsed)))

(defun citeproc-bt--to-csl-name (name)
  "Return a CSL version of BibTeX name string NAME."
  (let* (result
	 family
	 (tokens (-remove #'s-blank-str-p
			  (citeproc-s-slice-by-matches name "\\(,\\|[[:space:]]+\\)")))
	 (parts (-split-on "," tokens)))
    (pcase (length parts)
      ;; No commas in the name
      (1 (let ((name (car parts)))
	   (-if-let (1st-downcased-idx (-find-index #'s-lowercase-p name))
	       (progn (setq family (-slice name 1st-downcased-idx))
		      (when (> 1st-downcased-idx 0)
			(push `(given . ,(-slice name 0 1st-downcased-idx)) result)))
	     (setq family (last name))
	     (when (> (length name) 1)
	       (push `(given . ,(-slice name 0 -1)) result)))))
      ;; A single comma separates family and last name
      (2 (setq family (car parts))
	 (push `(given . ,(cadr parts)) result))
      ;; More than one commas
      (_ (setq family (car parts))
	 (push `(suffix . ,(cadr parts)) result)
	 (push `(given . ,(cl-caddr parts)) result)))
    (setq result (nconc (citeproc-bt--parse-family family) result))
    (--map (cons (car it) (s-join " " (cdr it)))
	   result)))

(defun citeproc-bt--to-csl-date (year month)
  "Return a CSL version of the date given by YEAR and MONTH.
YEAR and MONTH are the values of the corresponding BibTeX fields,
MONTH might be nil."
  (condition-case nil
      (let ((csl-year (string-to-number (car (s-match "[[:digit:]]+" year))))
	    (csl-month (when month
			 (assoc-default (downcase month)
					citeproc-bt--mon-to-num-alist)))
	    date)
	(when csl-year
	  (when csl-month (push csl-month date))
	  (push csl-year date))
	(list (cons 'date-parts (list date))))
    (error
     (error (concat "Couldn't parse year: '%s'"
		    (when month " and month: '%s'")
		    " as a date")
	    year month))))

(defun citeproc-bt-entry-to-csl (b)
  "Return a CSL form of normalized parsed BibTeX entry B."
  (let ((type (assoc-default (downcase (assoc-default "=type=" b))
			     citeproc-bt--to-csl-types-alist))
	result year month)
    (cl-loop for (key . value) in b do
	     (let ((key (downcase key))
		   (value (citeproc-bt--to-csl value)))
	       (-if-let (csl-key (assoc-default key citeproc-bt--to-csl-keys-alist))
		   ;; Vars mapped simply to a differently named CSL var
		   (push (cons csl-key value) result)
		 (pcase key
		   ((or "author" "editor") ; Name vars
		    (push (cons (intern key) (citeproc-bt--to-csl-names value))
			  result))
		   ("=type=" (push (cons 'type type) result))
		   ("number" (push (cons (if (string= type "article-journal") 'issue
					   'number)
					 value)
				   result))
		   ;; Date vars that need further processing below
		   ("year" (setq year value))
		   ("month" (setq month value))
		   ;; Remaining keys are mapped without change
		   (_ (push (cons (intern key) value) result))))))
    (when year
      (push (cons 'issued (citeproc-bt--to-csl-date year month))
	    result))
    result))

;; This function is based on the function `org-bibtex-headline' in org-bibtex,
;; written by Bastien Guerry <bzg@gnu.org>, Carsten Dominik <carsten dot dominik
;; at gmail dot com> and Eric Schulte <schulte dot eric at gmail dot com>.
;; Copyright (C) 2007-2017 Free Software Foundation, Inc.
(defun citeproc-bt-from-org-headline (&optional itemids)
  "Return a (KEY . BIBTEX-ENTRY) pair from the headline at point.
The returned BibTeX entry has the same form as produced by
`bibtex-parse-entry'. Return nil if the headline has no
associated bibtex data. If optional ITEMIDS is given then also
return nil if the entry's key is not in ITEMIDS."
  (letrec ((val (lambda (key lst) (cdr (assoc key lst))))
	   (to (lambda (string) (intern (concat ":" string))))
	   (from (lambda (key) (substring (symbol-name key) 1)))
	   (flatten (lambda (&rest lsts)
		      (apply #'append (mapcar
				       (lambda (e)
					 (if (listp e) (apply flatten e) (list e)))
				       lsts))))
	   (id (org-bibtex-get org-bibtex-key-property))
	   (type (org-bibtex-get org-bibtex-type-property-name)))
    (when (and type (or (null itemids) (member id itemids)))
      `(,id . 
	    (("=type=" . ,type)
	     ,@(remove
		nil
		(if (and org-bibtex-export-arbitrary-fields
			 org-bibtex-prefix)
		    (mapcar
		     (lambda (kv)
		       (let ((key (car kv)) (val0 (cdr kv)))
			 (when (and
				(string-match org-bibtex-prefix key)
				(not (string=
				      (downcase (concat org-bibtex-prefix
							org-bibtex-type-property-name))
				      (downcase key))))
			   (cons (downcase (replace-regexp-in-string
					    org-bibtex-prefix "" key))
				 val0))))
		     (org-entry-properties nil 'standard))
		  (mapcar
		   (lambda (field)
		     (let ((value (or (org-bibtex-get (funcall from field))
				      (and (eq :title field)
					   (nth 4 (org-heading-components))))))
		       (when value (cons (funcall from field) value))))
		   (funcall flatten
			    (funcall val :required (funcall val (funcall to type) org-bibtex-types))
			    (funcall val :optional (funcall val (funcall to type) org-bibtex-types)))))))))))

(provide 'citeproc-bibtex)

;;; citeproc-bibtex.el ends here
