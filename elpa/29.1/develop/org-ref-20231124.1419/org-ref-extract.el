;;; org-ref-extract.el --- Extract BibTeX from HTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Justus Piater

;; Author: Justus Piater <Justus-dev@Piater.name>
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

;;


(defun org-ref--extract (html-buffer rx num)
  "Return content matched within HTML-BUFFER by RX at parenthesized
sub-expression NUM."
  (with-current-buffer html-buffer
    (goto-char (point-min))
    (if (re-search-forward rx nil t)
	(match-string num)
      nil)))


(defun org-ref--get-pdf (pdf-url)
  "For BibTeX entry at point, if not already present, get PDF, place
it in`bibtex-completion-library-path', and add a corresponding
FILE field to the entry."
  (bibtex-beginning-of-entry)
  (let* ((key (cdr (assoc "=key=" (bibtex-parse-entry))))
         (pdf-file (concat (car bibtex-completion-library-path) key ".pdf")))
    (unless (file-exists-p pdf-file)
      (url-copy-file pdf-url pdf-file)
      (if (org-ref-pdf-p pdf-file)
          (message "%s saved" pdf-file)
        (delete-file pdf-file)
        (message "No pdf was downloaded.")
        (browse-url pdf-url)))
    (when (file-exists-p pdf-file)
      (bibtex-set-field "file" pdf-file)
      (when doi-utils-open-pdf-after-download
	(org-open-file pdf-file)))))


(defun org-ref--extract-entry-from-html
    (html-buffer bibtex pdf-url &rest more-fields)
  "At point, create a BibTeX entry using information extracted
  from the HTML-BUFFER, and kill HTML-BUFFER."
  (bibtex-mode)
  (let ((bibtex (if (consp bibtex)
		    (org-ref--extract html-buffer (car bibtex) (cdr bibtex))
		  bibtex))
	(pdf-url (if (consp pdf-url)
		     (org-ref--extract html-buffer (car pdf-url) (cdr pdf-url))
		   pdf-url))
	(more-fields
	 (mapcar
	  (lambda (field)
	    (cons (car field)
		  (if (consp (cdr field))
		      (org-ref--extract html-buffer (cadr field) (cddr field))
		    (cdr field))))
	  more-fields)))
    (insert bibtex)
    (goto-char (point-min))
    (while (search-forward "{\\n" nil t)
      (replace-match "{"))
    (goto-char (point-min))
    (while (search-forward "\\n" nil t)
      (replace-match "\n"))
    (org-ref-clean-bibtex-entry)
    (dolist (pair more-fields)
      (when (cdr pair)
	(bibtex-set-field (car pair) (cdr pair))))
    (org-ref--get-pdf pdf-url))
  (kill-buffer html-buffer))


(defun org-ref--html-buffer (url)
  "Retrieve resource from URL, decode it, substitute XML entities,
and return the buffer."
  (with-current-buffer (generate-new-buffer "org-ref--html")
    (let ((url-request-method "GET"))
      (url-insert (url-retrieve-synchronously url)))
    (goto-char (point-min))
    (insert (xml-substitute-special (buffer-string)))
    (delete-region (point) (point-max))
    (current-buffer)))


(defun org-ref-extract-from-openreview (id)
  "At point, create a BibTeX entry for the given OpenReview ID."
  (interactive "MOpenReview ID: ")
  (let* ((url (concat "https://openreview.net/forum?id=" id))
	 (html-buffer (org-ref--html-buffer url)))
    (org-ref--extract-entry-from-html
     html-buffer
     '("\"_bibtex\":\"\\(@.+?}\\)\"" . 1)
     (replace-regexp-in-string "forum" "pdf" url)
     '("abstract" .
       ("<meta name=\"citation_abstract\" content=\"\\(.+?\\(\n.*?\\)*?\\)\"/>" . 1))
     '("area" .
       ("\"Please_choose_the_closest_area_that_your_submission_falls_into\":\"\\(.+?\\)\"" . 1))
     '("keywords" . ("Keywords.*?\"note-content-value\">\\(.+?\\)</span>" . 1))
     '("summary" .
       ("\\(Summary\\|TL;DR\\).*?\"note-content-value\">\\(.+?\\)</span>" . 2))
     ;; Should we proactively download supplementary materials too?
     (cons "supp"
	   (if-let ((supp (org-ref--extract
			   html-buffer
			   ">Supplementary Material<.*?href=\"\\([^\"]+\\)" 1)))
	       (concat "https://openreview.net" supp))))))


(defun org-ref-extract-from-pmlr (url)
  "At point, create a BibTeX entry for the given PMLR URL."
  (interactive "MPMLR URL: ")
  (org-ref--extract-entry-from-html
   (org-ref--html-buffer url)
   '("id=\"bibtex\">\n\\(@.+\\(\n.*?\\)+?\\)\n</" . 1)
   '("{\\(http.+\\.pdf\\)}" . 1)
   ;; Should we proactively download supplementary materials too?
   '("supp" . ("href=\"\\(https?://proceedings\\.mlr\\.press/[^\"]+?-supp[^\"]*?\\)\".*?>Supplementary PDF</" . 1))))


(defun org-ref-extract-from-neurips (url)
  "At point, create a BibTeX entry for the given NeurIPS Abstract URL."
  (interactive "MNeurIPS Abstract URL: ")
  (let ((hash (progn (string-match "/\\([0-9a-f]+\\)-" url)
		     (match-string 1 url)))
	(neurips-url "https://proceedings.neurips.cc")
	(html-buffer (org-ref--html-buffer url))
	(bibtex))
    (with-current-buffer html-buffer
      (goto-char (point-min))
      (re-search-forward "href=[\"']\\([^\"']+bibtex[^\"']*\\)[\"']")
      (let ((bibtex-url (match-string 1)))
	(with-temp-buffer
	  (url-insert
	   (url-retrieve-synchronously (concat neurips-url bibtex-url)))
	  (setq bibtex (buffer-string)))))
    (org-ref--extract-entry-from-html
     html-buffer
     bibtex
     (concat neurips-url
	     (org-ref--extract html-buffer
			       "href=[\"']\\([^\"']+-Paper[^\"']*\\)[\"']" 1))
     (cons "url" url)
     '("abstract" . ("<h4>Abstract</h4>[ \n]*?\\(<p>\\)+\\(.+?\\)</p>" . 2))
     ;; Should we proactively download supplementary materials too?
     (cons "supp"
	   (if-let
	       ((supp (org-ref--extract
		       html-buffer
		       "href=[\"']\\([^\"']+-Supplemental[^\"']*\\)[\"']" 1)))
	       (concat neurips-url supp))))))


(defun org-ref-extract-from-cvf (url)
  "At point, create a BibTeX entry for the given CVF HTML URL."
  (interactive "MCVF HTML URL: ")
  (let ((cvf-url "https://openaccess.thecvf.com")
	(html-buffer (org-ref--html-buffer url)))
    (org-ref--extract-entry-from-html
     html-buffer
     '("class=\"bibref[^\"]*\">[ \n]*\\(@.+?\\(\n.*?\\)+?\\)[ \n]*</" . 1)
     (concat cvf-url (org-ref--extract
		      html-buffer "<a href=[\"']\\([^\"']+\\)[\"']>pdf</a>" 1))
     (cons "url" url)
     '("abstract" . ("id=\"abstract\">[ \n]*\\([^<]+\\)[ \n]*</" . 1))
     ;; Should we proactively download supplementary materials too?
     (cons "supp" (concat cvf-url
			  (org-ref--extract html-buffer
					    "href=[\"']\\([^\"']+\\)[\"']>supp</"
					    1))))))


(provide 'org-ref-extract)
;;; org-ref-extract.el ends here
