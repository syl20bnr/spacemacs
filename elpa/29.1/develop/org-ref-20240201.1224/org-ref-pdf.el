;;; org-ref-pdf.el --- Drag-n-drop PDF onto bibtex files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;; This library provides functions to enable drag-n-drop of pdfs onto a bibtex
;; buffer to add bibtex entries to it.
;;
;; [2021-08-29 Sun] I have not found drag-and-drop to be especially reliable,
;; and don't recommend you use it. This code relies 'pdf-tools, which is slow to
;; load for me. pdf-tools is no longer automatically installed, so if you use
;; this, you should install it yourself.

;; TODO: If no DOI is found, figure out a way to do a crossref/google query to
;; get a doi. This needs a reliable title/citation.

;;; Code:
(require 'pdf-tools)

(require 'f)


(eval-when-compile
  (require 'cl-lib))


(defgroup org-ref-pdf nil
  "Customization group for org-ref-pdf"
  :tag "Org Ref PDF"
  :group 'org-ref-pdf)


(defcustom pdftotext-executable
  "pdftotext"
  "Executable for pdftotext. Set if the executable is not on your
path, or you want to use another version."
  :type 'file
  :group 'org-ref-pdf)


(defcustom org-ref-pdf-doi-regex
  "10\\.[0-9]\\{4,9\\}/[-+._;()/:A-Z0-9]+"
  "Regular expression to match DOIs in a pdf converted to text."
  :type 'regexp
  :group 'org-ref-pdf)


(defcustom org-ref-pdf-to-bibtex-function
  'copy-file
  "Function for getting  a pdf to a directory.
Defaults to `copy-file', but could also be `rename-file'."
  :type 'File :group 'org-ref-pdf)


(defun org-ref-extract-doi-from-pdf (pdf)
  "Try to extract a doi from a PDF file.
There may be more than one doi in the file. This function returns
all the ones it finds based on two patterns: doi: up to a quote,
bracket, space or end of line. dx.doi.org/up to a quote, bracket,
space or end of line.

If there is a trailing . we chomp it off. Returns a list of doi
strings, or nil."
  (with-temp-buffer
    (insert (shell-command-to-string (format "%s %s -"
					     pdftotext-executable
					     (shell-quote-argument pdf))))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward org-ref-pdf-doi-regex nil t)
	;; I don't know how to avoid a trailing . on some dois with the
	;; expression above, so if it is there, I chomp it off here.
	(let ((doi (match-string 0)))
	  (when (or (s-ends-with? "." doi) (s-ends-with? ";" doi))
	    (setq doi (substring doi 0 (- (length doi) 1))))
	  (cl-pushnew doi matches :test #'equal)))
      matches)))


(defun org-ref-pdf-doi-candidates (dois)
  "Generate candidate list for a completion source.
Used when multiple dois are found in a pdf file."
  (cl-loop for doi in dois
	   collect
	   (condition-case nil
	       (cons
		(plist-get (doi-utils-get-json-metadata doi) :title)
		doi)
	     (error (cons (format "%s read error" doi) doi)))))


(defun org-ref-bibtex-key-from-doi (doi)
  "Return a bibtex entry's key from a DOI.
BIB is an optional filename to get the entry from."
  (catch 'key
    (cl-loop for bibfile in (if (stringp bibtex-completion-bibliography)
				(list bibtex-completion-bibliography)
			      bibtex-completion-bibliography)
	     do
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name bibfile))
	       (when (search-forward doi)
		 (bibtex-beginning-of-entry)
		 (throw 'key (cdr (assoc "=key=" (bibtex-parse-entry)))))))))


;;;###autoload
(defun org-ref-pdf-to-bibtex ()
  "Add pdf of current buffer to bib file and save pdf. The pdf
should be open in Emacs using the `pdf-tools' package."
  (interactive)
  (when (not (f-ext? (downcase (buffer-file-name)) "pdf"))
    (error "Buffer is not a pdf file"))
  ;; Get doi from pdf of current buffer
  (let* ((dois (org-ref-extract-doi-from-pdf (buffer-file-name)))
         (doi-utils-download-pdf nil)
         (doi (if (= 1 (length dois))
                  (car dois)
                (completing-read "Select DOI: " dois))))
    ;; Add bib entry from doi:
    (doi-utils-add-bibtex-entry-from-doi doi)
    ;; Copy pdf to a directory
    (let ((key (org-ref-bibtex-key-from-doi doi)))
      (funcall org-ref-pdf-to-bibtex-function
	       (buffer-file-name)
               (expand-file-name (format "%s.pdf" key) (org-ref-library-path))))))


;;;###autoload
(defun org-ref-pdf-debug-pdf (pdf-file)
  "Try to debug getting a doi from a pdf.
Opens a buffer with the pdf converted to text, and `occur' on the
variable `org-ref-pdf-doi-regex'."
  (interactive "fPDF: ")
  (switch-to-buffer (get-buffer-create "*org-ref-pdf debug*"))
  (erase-buffer)
  (insert (shell-command-to-string (format "%s %s -"
					   pdftotext-executable
					   (shell-quote-argument pdf-file))))
  (goto-char (point-min))
  (highlight-regexp org-ref-pdf-doi-regex)
  (occur org-ref-pdf-doi-regex)
  (switch-to-buffer-other-window "*Occur*"))


;;;###autoload
(defun org-ref-pdf-crossref-lookup ()
  "Lookup highlighted text in PDFView in CrossRef."
  (interactive)
  (require 'pdf-view)
  (unless (pdf-view-active-region-p)
    (error "The region is not active"))
  (let* ((txt (pdf-view-active-region-text)))
    (pdf-view-deactivate-region)
    (crossref-lookup (mapconcat 'identity txt "	 \n"))))


(provide 'org-ref-pdf)
;;; org-ref-pdf.el ends here
