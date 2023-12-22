;;; org-ref-scopus.el --- Emacs-lisp interface to the Scopus API  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

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
;;  See http://dev.elsevier.com/index.html for more information about the Scopus API
;;
;; New org-links:
;; eid:2-s2.0-72649092395 with a hydra menu
;; [[scopus-search:alloy Au segregation]]
;; [[scopus-advanced-search:au-id(24176978500)]]

;;; Code:

(require 'org)
(require 'hydra)
(require 'xml)
(require 'org-ref-utils)

(defvar url-request-method)
(defvar url-mime-accept-string)
(defvar url-http-end-of-headers)
(defvar url-request-extra-headers)

(defvar *scopus-api-key* nil
  "Your Scopus API key.
You need to set this in your init files.  Get a key here:
http://dev.elsevier.com/myapikey.html.")


;; (defun scopus-doi-to-xml (doi)
;;   "Return a parsed xml from the Scopus article retrieval api for DOI.
;; This does not always seem to work for the most recent DOIs."
;;   (let* ((url-request-method "GET")
;;	 (url-request-extra-headers  (list (cons "X-ELS-APIKey" *scopus-api-key*)))
;;	 (url (format  "http://api.elsevier.com/content/article/doi/%s" doi))
;;	 (xml))
;;     (setq xml
;;	  (with-current-buffer  (url-retrieve-synchronously url)
;;	    (xml-parse-region url-http-end-of-headers (point-max))))
;;     (if (eq 'service-error (caar xml))
;;	(progn (message-box "%s\n%s\n%s" doi url xml)
;;	       nil)
;;       xml)))

(defun scopus-doi-to-eid (doi)
  "Get a Scopus eid from a DOI.
Requires `*scopus-api-key*' to be defined."
  (unless *scopus-api-key* (error "You must define `*scopus-api-key*'"))
  (let* ((url-request-method "GET")
         (url-mime-accept-string "application/xml")
         (url-request-extra-headers  (list (cons "X-ELS-APIKey" *scopus-api-key*)
                                           '("field" . "eid")))
         (url (format  "http://api.elsevier.com/content/search/scopus?query=doi(%s)" doi))
         (xml (with-current-buffer  (url-retrieve-synchronously url)
                (xml-parse-region url-http-end-of-headers (point-max))))
         (results (car xml))
         (entry (car (xml-get-children results 'entry))))
    (car (xml-node-children (car (xml-get-children entry 'eid))))))


;;;###autoload
(defun scopus-related-by-keyword-url (doi)
  "Return a Scopus url to articles related by keyword for DOI."
  (interactive)
  (unless *scopus-api-key* (error "You must define `*scopus-api-key*'"))
  (let ((eid (scopus-doi-to-eid doi)))
    (when eid (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=key&zone=relatedDocuments" eid))))


;;;###autoload
(defun scopus-related-by-author-url (doi)
  "Return a Scopus url to articles related by author for DOI."
  (interactive)
  (unless *scopus-api-key* (error "You must define `*scopus-api-key*'"))
  (let ((eid (scopus-doi-to-eid doi)))
    (when eid (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=aut&zone=relatedDocuments" eid))))


;;;###autoload
(defun scopus-related-by-references-url (doi)
  "Return a Scopus url to articles related by references for DOI."
  (interactive)
  (unless *scopus-api-key* (error "You must define `*scopus-api-key*'"))
  (let ((eid (scopus-doi-to-eid doi)))
    (when eid (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=ref&zone=relatedDocuments" eid))))


(defun scopus-citing-url (doi)
  "Return a Scopus url to articles citing DOI."
  (format "http://www.scopus.com/results/citedbyresults.url?sort=plf-f&cite=%s&src=s&imp=t&sot=cite&sdt=a&sl=0&origin=recordpage" (scopus-doi-to-eid doi)))


;;;###autoload
(defun scopus-open-eid (eid)
  "Open article with EID in browser."
  (interactive "sEID: ")
  (browse-url (format "http://www.scopus.com/record/display.url?eid=%s&origin=resultslist" eid)))


(defun scopus ()
  "Open http://scopus.com is a browser."
  (browse-url "http://www.scopus.com"))


;;;###autoload
(defun scopus-basic-search (query)
  "Open QUERY as a basic title-abstract-keyword search at scopus.com."
  (interactive "sQuery: ")
  (browse-url
   (format
    "http://www.scopus.com/results/results.url?sort=plf-f&src=s&sot=b&sdt=b&sl=%s&s=TITLE-ABS-KEY%%28%s%%29&origin=searchbasic"
    (length (url-unhex-string (concat "TITLE-ABS-KEY%28" (url-hexify-string query) "%29")))
    (url-hexify-string query))))


;;;###autoload
(defun scopus-advanced-search (query)
  "Open QUERY as an advanced search at scopus.com."
  (interactive "sQuery: ")
  (browse-url
   (format
    "http://www.scopus.com/results/results.url?sort=plf-f&src=s&sot=a&sdt=a&sl=%s&s=%s&origin=searchadvanced"
    (length query)
    (url-hexify-string query))))


;;; Org-mode EID link and an action menu
;; These functions use a global var *hydra-eid*
(defvar *hydra-eid* nil
  "Global variable to pass an EID from an ‘org-mode’ link to a hydra function.")


(defhydra scopus-hydra (:color blue)
  ("o" (scopus-open-eid *hydra-eid*) "Open in Scopus")
  ("a" (browse-url (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=aut&zone=relatedDocuments" *hydra-eid*))
   "Related by author")
  ("k" (browse-url (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=key&zone=relatedDocuments" *hydra-eid*))
   "Related by keyword")
  ("r" (browse-url (format "http://www.scopus.com/search/submit/mlt.url?eid=%s&src=s&all=true&origin=recordpage&method=ref&zone=relatedDocuments" *hydra-eid*))
   "Related by references")
  ("c" (browse-url (format "http://www.scopus.com/results/citedbyresults.url?sort=plf-f&cite=%s&src=s&imp=t&sot=cite&sdt=a&sl=0&origin=recordpage" *hydra-eid*))
   "Citing articles"))


(org-link-set-parameters "eid"
			 :follow (lambda (eid)
				   "Opens the hydra menu."
				   (setq *hydra-eid* eid)
				   (scopus-hydra/body))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\" http://www.scopus.com/record/display.url?eid=%s&origin=resultslist\">%s</a>" keyword (or desc keyword)))
				    ((eq format 'latex)
				     (format "\\href{http://www.scopus.com/record/display.url?eid=%s&origin=resultslist}{%s}"
					     keyword (or desc keyword))))))


(org-link-set-parameters "scopus-search"
			 :follow (lambda (query)
				   (scopus-basic-search query))
			 :export (lambda (query desc format)
				   (let ((url (format
					       "http://www.scopus.com/results/results.url?sort=plf-f&src=s&sot=b&sdt=b&sl=%s&s=TITLE-ABS-KEY%%28%s%%29&origin=searchbasic"
					       (length (url-unhex-string (concat "TITLE-ABS-KEY%28" (url-hexify-string query) "%29")))
					       (url-hexify-string query))))
				     (cond
				      ((eq format 'html)
				       (format "<a href=\"%s\">%s</a>" url (or desc query)))
				      ((eq format 'latex)
				       (format "\\href{%s}{%s}" url (or desc query)))))))

(org-link-set-parameters "scopus-advanced-search"
			 :follow (lambda (query)
				   (scopus-advanced-search query))
			 :export (lambda (query desc format)
				   (let ((url (format
					       "http://www.scopus.com/results/results.url?sort=plf-f&src=s&sot=a&sdt=a&sl=%s&s=%s&origin=searchadvanced"
					       (length (url-hexify-string query))
					       (url-hexify-string query))))
				     (cond
				      ((eq format 'html)
				       (format "<a href=\"%s\">%s</a>" url (or desc query)))
				      ((eq format 'latex)
				       (format "\\href{%s}{%s}" url (or desc query)))))))

(org-link-set-parameters "scopusid"
			 :follow (lambda
				   (link-string)
				   (browse-url
				    (format
				     "http://www.scopus.com/authid/detail.url?origin=AuthorProfile&authorId=%s"
				     link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\href{http://www.scopus.com/authid/detail.url?origin=AuthorProfile&authorId=%s}{%s}"
					     keyword (or desc (concat "scopusid:" keyword))))
				    ((eq format 'html)
				     (format "<a href=\"http://www.scopus.com/authid/detail.url?origin=AuthorProfile&authorId=%s\">%s</a>"
					     keyword (or desc (concat "scopusid:" keyword)))))))


(provide 'org-ref-scopus)
;;; org-ref-scopus.el ends here
