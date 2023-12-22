;;; org-ref-pubmed.el --- Links and functions for Pubmed and NIH databases  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021  John Kitchin

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

;;* Introduction

;; This document is an experiment at creating a literate program to provide
;; functions for interacting with pubmed databases.

;; This library provides links that go to pubmed resources, e.g.

;; pmcid:PMC3498956
;;
;; pmid:23162369
;;
;; and nihmsid:NIHMS395714
;;
;; See http://www.ncbi.nlm.nih.gov/pmc/about/public-access-info/#p3 for details
;; of these identifiers.
;;
;; For PMID there is one interactive function that inserts a bibtex entry:
;; pubmed-insert-bibtex-from-pmid.

;; This library is complementary to [[./doi-utils.org]].

;;; Code:

;;* PMID (from PubMed) link and functions

;; A PMID is a number that identifies an entry in the Pubmed database.  The PMID
;; is a unique reference number for PubMed citations. The PMID is a distinctly
;; different number from the PMCID and is used only for PubMed records.

(require 'dash)
(require 'org)
(require 'org-ref-utils)

(org-link-set-parameters "pmid"
			 :follow (lambda (link-string) (browse-url (format "http://www.ncbi.nlm.nih.gov/pubmed/%s" link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://www.ncbi.nlm.nih.gov/pmc/articles/mid/%s\">pmid:%s</a>" keyword (or desc keyword))) ; no output for html
				    ((eq format 'latex)
				     ;; write out the latex command
				     (format "\\url{http://www.ncbi.nlm.nih.gov/pmc/articles/mid/%s}{%s}" keyword (or desc keyword))))))

;;** Get MEDLINE metadata

;; We can get bibliographic metadata from a pmid. Here we get the MEDLINE
;; text. The website wraps the data in <pre></pre> tags.

(defun pubmed-get-medline (pmid)
  "Get MEDLINE text for PMID as a string."
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://pubmed.ncbi.nlm.nih.gov/%s/?format=pubmed" pmid))
    (goto-char (point-min))
    (let ((p1 (search-forward "<pre class=\"article-details\" id=\"article-details\">"))
          (p2 (search-forward "</pre>")))
      (replace-regexp-in-string "" "" (buffer-substring (+ 1 p1) (- p2 6))))))

;;** Parse the PMID MEDLINE data

;; We can parse this into a data structure.
(defun pubmed-parse-medline (pmid)
  "Parse the medline text for PMID and return a list of cons cells."
  (let ((data '())
        (p1)
        (p2)
        (tag)
        (value))
    (with-temp-buffer (insert (pubmed-get-medline pmid))
                      (goto-char (point-min))
                      (while (re-search-forward "\\(^[A-Z]\\{2,4\\}\\)\\s-*- "
						nil t)
                        (setq tag (match-string 1))
                        ;; point is at end of the search
                        (setq p1 (point))
                        ;; now go to next tag
                        (re-search-forward "\\(^[A-Z]\\{2,4\\}\\)\\s-*- " nil t)
                        (setq p2 (- (match-beginning 1) 1))
                        (setq value (buffer-substring p1 p2))
                        (setq data (append data (list (cons tag value))))
                        ;; now go back to last tag to get the next one
                        (goto-char p1)))
    data))

;;** PMID to bibtex entry

;; The point of parsing the MEDLINE text is so we can make bibtex entries. We
;; only support Journal articles for now.

(defun pubmed-pmid-to-bibtex (pmid)
  "Convert a PMID to a bibtex entry."
  (let* ((data (pubmed-parse-medline pmid))
         (type (downcase (cdr (assoc "PT" data))))
         (title (cdr (assoc "TI" data)))
         (authors (mapconcat 'cdr
                             (-filter (lambda (x)
                                        (string= (car x) "FAU"))
                                      data)
                             " and "))
         (abstract (cdr (assoc "AB" data)))
         (volume (cdr (assoc "VI" data)))
         (issue (cdr (assoc "IP" data)))
         (journal (cdr (assoc "JT" data)))
         (year (cdr (assoc "DP" data)))
         (pages (cdr (assoc "PG" data)))
         (aid (cdr (assoc "AID" data))))

    (cond
     ((string= type "journal article")
      (concat "@article{,
 author = {" authors "},
 title = {" title "},
 abstract = {" abstract "},
 journal = {" journal "},
 volume = {" volume "},
 number = {" issue "},
 year = {" (car (split-string year)) "},
 pages = {" pages "},
 doi = {" (replace-regexp-in-string " \\[doi\\]" "" (or aid "")) "},
}"))
     (t
      (message "No conversion for type: %s" type)))))

;; And we probably want to be able to insert a bibtex entry

;;;###autoload
(defun pubmed-insert-bibtex-from-pmid (pmid)
  "Insert a bibtex entry at point derived from PMID.
You must clean the entry after insertion."
  (interactive "sPMID: ")
  (insert (pubmed-pmid-to-bibtex pmid)))


;;* Pubmed Central (PMC) link

;; A PMCID starts with PMC and is followed by numbers. The PMCID is a unique
;; reference number or identifier that is assigned to every article that is
;; accepted into PMC. The PMCID is also used by recipients of NIH funding to
;; demonstrate compliance with the NIH Public Access policy. The PMCID can be
;; found in both PMC and PubMed.

;; Here we define a new link. Clicking on it simply opens a webpage to the
;; article.

(org-link-set-parameters "pmcid"
			 :follow (lambda (link-string) (browse-url (format "http://www.ncbi.nlm.nih.gov/pmc/articles/%s" link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://www.ncbi.nlm.nih.gov/pmc/articles/%s\">pmcid:%s</a>" keyword (or desc keyword)))
				    ((eq format 'latex)
				     (format "\\url{http://www.ncbi.nlm.nih.gov/pmc/articles/%s}{%s}" keyword (or desc keyword))))))

;;* NIHMSID

;; The NIHMSID is a preliminary article identifier that applies only to
;; manuscripts deposited through the NIHMS system. The NIHMSID is only valid for
;; compliance reporting for 90 days after the publication date of an
;; article. Once the Web version of the NIHMS submission is approved for
;; inclusion in PMC and the corresponding citation is in PubMed, the article
;; will also be assigned a PMCID.

(org-link-set-parameters "nihmsid"
			 :follow (lambda (link-string) (browse-url (format "http://www.ncbi.nlm.nih.gov/pmc/articles/mid/%s" link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://www.ncbi.nlm.nih.gov/pmc/articles/mid//%s\">nihmsid:%s</a>" keyword (or desc keyword)))
				    ((eq format 'latex)
				     ;; write out the latex command
				     (format "\\url{http://www.ncbi.nlm.nih.gov/pmc/articles/mid/%s}{%s}" keyword (or desc keyword))))))


;;* Searching pubmed

;;;###autoload
(defun pubmed ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser."
  (interactive)
  (browse-url "http://www.ncbi.nlm.nih.gov/pubmed"))


;;;###autoload
(defun pubmed-advanced ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed/advanced in a browser."
  (interactive)
  (browse-url "http://www.ncbi.nlm.nih.gov/pubmed/advanced"))


;;;###autoload
(defun pubmed-simple-search (query)
  "Open QUERY in Pubmed in a browser."
  (interactive "sQuery: ")
  (browse-url
   (format "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s" (url-hexify-string query))))


(org-link-set-parameters "pubmed-search"
			 :follow (lambda (query)
				   "Open QUERY in a `pubmed-simple-search'."
				   (pubmed-simple-search query))
			 :export (lambda (query desc format)
				   (let ((url (format "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s" (url-hexify-string query))))
				     (cond
				      ((eq format 'html)
				       (format "<a href=\"%s\">%s</a>" url (or desc (concat "pubmed-search:" query))))
				      ((eq format 'latex)
				       (format "\\href{%s}{%s}" url (or desc (concat "pubmed-search:" query))))))))

;; ** Pubmed clinical
;; These functions were suggested by Colin Melville <casm40@gmail.com>.
;;;###autoload
(defun pubmed-clinical ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed/clinical in a browser."
  (interactive)
  (browse-url "http://www.ncbi.nlm.nih.gov/pubmed/clinical"))


;;;###autoload
(defun pubmed-clinical-search (query)
  "Open QUERY in pubmed-clinical."
  (interactive "sQuery: ")
  (browse-url
   (format "https://www.ncbi.nlm.nih.gov/pubmed/clinical?term=%s" (url-hexify-string query))))


(org-link-set-parameters "pubmed-clinical"
			 :follow (lambda (query)
				   "Open QUERY in a `pubmed-clinical-search'."
				   (pubmed-clinical-search query))
			 :export (lambda (query desc format)
				   (let ((url (format "http://www.ncbi.nlm.nih.gov/pubmed/clinical?term=%s"
						      (url-hexify-string query))))
				     (cond
				      ((eq format 'html)
				       (format "<a href=\"%s\">%s</a>" url
					       (or desc (concat "pubmed-clinical-search:" query))))
				      ((eq format 'latex)
				       (format "\\href{%s}{%s}" url
					       (or desc (concat "pubmed-clinical-search:" query))))))))


(provide 'org-ref-pubmed)
;;; org-ref-pubmed.el ends here
