;; citeproc-itemgetters.el --- functions for constructing itemgetters -*- lexical-binding: t; -*-

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

;; Functions for constructing various types of bibliographic itemgetter
;; functions. The returned itemgetter functions can, in turn, be used to create
;; `citeproc-style' and `citeproc-proc' structures.

;;; Code:

(require 'dash)
(require 'org)
;; Handle the fact that org-bibtex has been renamed to ol-bibtex -- for the time
;; being we support both feature names.
(or (require 'ol-bibtex nil t)
    (require 'org-bibtex))
(require 'json)
(require 'bibtex)
(require 'parsebib)

(require 'citeproc-bibtex)
(require 'citeproc-biblatex)

(defun citeproc-itemgetters--parsebib-buffer (entries strings)
  "Parse a BibTeX/biblatex buffer with Parsebib."
  ;; Note: this is needed to support different Parsebib versions in use.
  (cond ((fboundp 'parsebib-parse-buffer)
	 (parsebib-parse-buffer entries strings t t))
	((fboundp 'parsebib-parse-bib-buffer)
	 (parsebib-parse-bib-buffer :entries entries :strings strings
				    :expand-strings t :inheritance t))
	(t (error "No Parsebib buffer parsing function is available"))))

(defun citeproc-hash-itemgetter-from-csl-json (file)
  "Return a hash-based getter for csl json bibliography FILE."
  (let* ((json-array-type 'list)
	 (json-key-type 'symbol)
	 (item-list (json-read-file file))
	 (hash (make-hash-table :test 'equal)))
    (--each item-list
      (puthash (alist-get 'id it) it hash))
    (lambda (itemids) (--map (cons it (gethash it hash))
			     itemids))))

(defun citeproc-itemgetter-from-csl-json (file)
  "Return an item-getter for csl json bibliography FILE."
  (lambda (itemids)
    (let* ((json-array-type 'list)
	   (json-key-type 'symbol)
	   (item-list (json-read-file file))
	   result)
      (dolist (item item-list result)
	(let ((id (alist-get 'id item)))
	  (when (member id itemids)
	    (push (cons id item) result)))))))

(defun citeproc-itemgetter-from-bibtex (file-or-files)
  "Return a getter for BibTeX bibliography FILE-OR-FILES."
  (if (listp file-or-files)
      (lambda (itemids)
	(let (result
	      (remaining-ids (cl-copy-list itemids))
	      (remaining-files file-or-files))
	  (while (and remaining-ids remaining-files)
	    (let ((file (pop remaining-files)))
	     (with-temp-buffer
	       (insert-file-contents file)
	       (goto-char (point-min))
	       (bibtex-set-dialect 'BibTeX t)
	       (bibtex-map-entries
		(lambda (key _beg _end)
		  (when (member key itemids)
		    (push (cons key (citeproc-bt-entry-to-csl (bibtex-parse-entry)))
			  result)
		    (setq remaining-ids (delete key remaining-ids))))))))
	  result))
    (lambda (itemids)
      (let (result)
	(with-temp-buffer
	  (insert-file-contents file-or-files)
	  (goto-char (point-min))
	  (bibtex-set-dialect 'BibTeX t)
	  (bibtex-map-entries
	   (lambda (key _beg _end)
	     (when (member key itemids)
	       (push (cons key (citeproc-bt-entry-to-csl (bibtex-parse-entry)))
		     result)))))
	result))))

(defun citeproc-itemgetter-from-org-bibtex (file-or-files)
  "Return a getter for org-bibtex bibliography FILE-OR-FILES."
  (let ((files (if (listp file-or-files)
		   file-or-files
		 (list file-or-files))))
   (lambda (itemids)
     (let (result)
       (org-map-entries
	(lambda ()
	  (-when-let (key-w-entry (citeproc-bt-from-org-headline itemids))
	    (push (cons (car key-w-entry)
			(citeproc-bt-entry-to-csl (cdr key-w-entry)))
		  result)))
	t files)
       result))))

(defun citeproc-hash-itemgetter-from-any (file-or-files &optional no-sentcase-wo-langid)
  "Return a getter for FILE-OR-FILES in any supported format.
The format is determined on the basis of file extensions.
Supported formats:
- CSL-JSON (.json extension) the recommended native format;
- BibTeX/biblatex (.bib or .bibtex extension),
- org-bibtex (.org extension).
If NO-SENTCASE-WO-LANGID is non-nil then title fields in items
without a `langid' field are not converted to sentence-case."
  (let ((files (if (listp file-or-files)
		   file-or-files
		 (list file-or-files)))
	(cache (make-hash-table :test #'equal))
	(bt-entries (make-hash-table :test #'equal))
	(bt-strings (make-hash-table :test #'equal)))
    (dolist (file files)
      (pcase (file-name-extension file)
        ("json"
         (let ((json-array-type 'list)
               (json-key-type 'symbol))
           (dolist (item (json-read-file file))
             (puthash (cdr (assq 'id item)) item cache))))
        ((and (or "bib" "bibtex") ext)
         (with-temp-buffer
	   (insert-file-contents file)
	   (bibtex-set-dialect (if (string= ext "bib") 'biblatex 'BibTeX) t)
	   (citeproc-itemgetters--parsebib-buffer bt-entries bt-strings)))
	("org"
	 (org-map-entries
	  (lambda ()
	    (-when-let (key-w-entry (citeproc-bt-from-org-headline))
	      (condition-case err
		  (puthash (car key-w-entry) (citeproc-blt-entry-to-csl
					      (cdr key-w-entry))
			   cache)
		(error
		 (user-error
		  "Couldn't parse the bib(la)tex entry with key '%s', the error was: %s" 
		  (car key-w-entry) err)))))
	  t (list file)))
        (ext
         (user-error "Unknown bibliography extension: %S" ext))))
    (maphash
     (lambda (key entry)
       (condition-case err
	   (puthash key (citeproc-blt-entry-to-csl entry nil no-sentcase-wo-langid)
		    cache)
	 (error
	  (user-error
	   "Couldn't parse the bib(la)tex entry with key '%s', the error was: %s"
	   key err))))
     bt-entries)
    (lambda (x)
      (pcase x
	('itemids
	 (hash-table-keys cache))
	((pred listp) (mapcar (lambda (id)
				(cons id (gethash id cache)))
			      x))
	(_ (error "Unsupported citeproc itemgetter retrieval method"))))))

(provide 'citeproc-itemgetters)

;;; citeproc-itemgetters.el ends here
