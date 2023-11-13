;;; org-ref-isbn.el --- utilities for generating bibtex entries from an ISBN  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience

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

;;; Commentary:

;; Provides functions to download bibtex entries from isbn numbers.

;;; Code:
(require 'f)
(require 'org)
(require 'org-ref-core)

(defvar url-http-end-of-headers)

;;* ISBN utility

(defcustom org-ref-isbn-clean-bibtex-entry-hook
  '(oricb-remove-enclosing-brackets
    oricb-clean-author-field
    oricb-remove-period
    oricb-kill-fields
    oricb-replace-field-names)
  "Hook that is run in `org-ref-isbn-clean-bibtex-entry'.
The functions should have no arguments, and operate on the bibtex
entry at point. You can assume point starts at the beginning of the
entry. These functions are wrapped in `save-restriction' and
`save-excursion' so you do not need to save the point position."
  :group 'org-ref-isbn
  :type 'hook)


(defcustom org-ref-isbn-exclude-fields nil
  "List of bibtex fields to kill when new entry is inserted."
  :group 'org-ref-isbn
  :type '(repeat :tag "List of bibtex fields to kill" string))


(defcustom org-ref-isbn-field-name-replacements nil
  "List of bitex field name/replacement pairs.
The entries in this list are cons cells where the car is the field name
and cdr is the replacement name."
  :group 'org-ref-isbn
  :type '(repeat (cons (string :tag "Field name")
		       (string :tag "Field name replacement"))))


(defun oricb-replace-field-names ()
  "Replace bibtex field names defined in
`org-ref-isbn-field-name-replacements'."
  (when org-ref-isbn-field-name-replacements
    (mapcar (lambda (field)
	      (bibtex-beginning-of-entry)
	      (let ((f (bibtex-autokey-get-field (car field))))
		(unless (string= "" f)
		  (goto-char (cadr (bibtex-search-forward-field (car field) t)))
		  (bibtex-kill-field)
		  (bibtex-make-field (cdr field))
		  (backward-char)
		  (insert f))))
	    org-ref-isbn-field-name-replacements)))


(defun oricb-kill-fields ()
  "Kill all bibtex fields defined in `org-ref-isbn-exclude-fields'."
  (when org-ref-isbn-exclude-fields
    (mapcar (lambda (field)
	      (bibtex-beginning-of-entry)
	      (let ((f (bibtex-autokey-get-field field)))
		(unless (string= "" f)
		  (goto-char (cadr (bibtex-search-forward-field field t)))
		  (bibtex-kill-field))))
	    org-ref-isbn-exclude-fields)))


(defun oricb-remove-enclosing-brackets ()
  "Remove enclosing brackets from fields."
  (save-restriction
    (bibtex-narrow-to-entry)
    (while (re-search-forward "\\({\\)\\(\\[\\)\\(.+\\)\\(]\\)\\(}\\)" nil t)
      (replace-match "\\1\\3\\5"))))


(defun oricb-clean-author-field ()
  "Remove extra information from author's field."
  (goto-char (cadr (bibtex-search-forward-field "author" t)))
  (let ((case-fold-search nil))
    (when (re-search-forward "\\({\\)\\(by \\|ed. by \\|edited by \\)" nil t)
      (replace-match "\\1"))))


(defun oricb-remove-period ()
  "Remove period from author's field."
  (goto-char (cadr (bibtex-search-forward-field "author" t)))
  (when (re-search-forward "\\(\\.\\)\\(}\\)" nil t)
    (replace-match "\\2")))


;;;###autoload
(defun org-ref-isbn-clean-bibtex-entry ()
  "Clean a bibtex entry inserted via `isbn-to-bibtex'.
See functions in `org-ref-isbn-clean-bibtex-entry-hook'."
  (interactive)
  (bibtex-beginning-of-entry)
  (mapc (lambda (x)
	  (save-restriction
	    (save-excursion
	      (funcall x))))
	org-ref-isbn-clean-bibtex-entry-hook))


;; I found this on the web. It can be handy, but the bibtex entry has a lot of stuff in it.

;;;###autoload
(defun isbn-to-bibtex-lead (isbn)
  "Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file."
  (interactive "sISBN: ")
  (browse-url
   (format
    "http://lead.to/amazon/en/?key=%s+&si=all&op=bt&bn=&so=sa&ht=us"
    isbn)))


;; Here we get isbn metadata and build a bibtex entry.

;;;###autoload
(defun isbn-to-bibtex (isbn bibfile)
  "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from www.ebook.de."
  (interactive
   (list
    (read-string
     "ISBN: "
     ;; now set initial input
     (cond
      ;; If region is active and it starts with a number, we use it
      ((and  (region-active-p)
             (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ;; if first entry in kill ring starts with a number assume it is an isbn
      ;; and use it as the guess
      ((stringp (car kill-ring))
       (when (s-match "^[0-9]" (car kill-ring))
	 (car kill-ring)))
      ;; type or paste it in
      (t
       nil)))
    (completing-read "Bibfile: " (org-ref-possible-bibfiles))))

  (let* ((url (format "https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s" isbn))
	 (entry))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (when (re-search-forward "@[a-zA-Z]+{.+\\(\n\s+[^\n]+\\)+}$" nil t)
	(setq entry (match-string 0))))

    (if (not entry)
	(message "Nothing found.")
      (find-file bibfile)
      (goto-char (point-max))
      (insert (with-temp-buffer
		(insert (concat entry "\n}"))
		(goto-char (point-min))
		;; [2020-06-06 Sat] I got a report that ottobib returns entries
		;; with ,, in the first line. here if we find one, I eliminate
		;; one of them.
		(when (re-search-forward ",," nil t)
		  (delete-char -1))
		(org-ref-isbn-clean-bibtex-entry)
		(org-ref-clean-bibtex-entry)
		(bibtex-fill-entry)
		(s-trim (buffer-string))))
      (save-buffer))))


(defun isbn-to-bibtex-open-library (isbn bibfile)
  "Retrieve bibtex entry for a book with ISBN using openlibrary.org.

API: https://openlibrary.org/developers/api
"
  (interactive
   (list
    (read-string
     "ISBN: "
     ;; now set initial input
     (cond
      ;; If region is active and it starts with a number, we use it
      ((and  (region-active-p)
             (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ;; if first entry in kill ring starts with a number assume it is an isbn
      ;; and use it as the guess
      ((stringp (car kill-ring))
       (when (s-match "^[0-9]" (car kill-ring))
	 (car kill-ring)))
      ;; type or paste it in
      (t
       nil)))
    (completing-read "Bibfile: " (org-ref-possible-bibfiles))))

  (let* ((url (format "https://openlibrary.org/isbn/%s.json" isbn))
	 (json (with-current-buffer (url-retrieve-synchronously url)
		 (json-read-from-string (string-trim (buffer-substring url-http-end-of-headers (point-max))))))
	 (title (cdr (assoc 'title json)))
	 (publisher (s-join ", " (cdr (assoc 'publishers json))))
	 (year (cdr (assoc 'publish_date  json)))
	 ;; this is a list of urls
	 (author-urls (cdr (assoc 'authors json)))
	 (authors (s-join " and "
			  (cl-loop for aurl across author-urls
				   collect
				   (with-current-buffer (url-retrieve-synchronously
							 (format "https://openlibrary.org%s.json"
								 (cdr (assoc 'key  aurl)))) 
				     (cdr (assoc 'personal_name
						 (json-read-from-string
						  (string-trim (buffer-substring url-http-end-of-headers (point-max)))))))))) 
	 (burl (format "https://openlibrary.org%s" (cdr (assoc 'key json))))
	 (bibtex (format "@Book{,
  author = 	 {%s},
  title = 	 {%s},
  publisher = 	 {%s},
  year = 	 {%s},
  url = {%s}
}"
			 authors
			 title
			 publisher
			 year
			 burl)))

    (with-current-buffer (find-file-noselect bibfile)
      (goto-char (point-max))
      (insert "\n\n")
      (insert bibtex)
      (org-ref-clean-bibtex-entry)
      (save-buffer))))

(provide 'org-ref-isbn)
;;; org-ref-isbn.el ends here
