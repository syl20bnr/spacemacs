;;; biblio-crossref.el --- Lookup and import bibliographic entries from CrossRef -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/biblio.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup and download bibliographic records from CrossRef (a very nicely
;; curated metadata engine) using `crossref-lookup'.
;;
;; This file implements a backend for the `biblio' package (which see for more
;; documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)

(defcustom biblio-crossref-user-email-address nil
  "Email address to include in CrossRef queries, or nil.
CrossRef gives priority to queries that include an email address.
See URL `https://github.com/CrossRef/rest-api-doc#etiquette' for
more details."
  :group 'biblio
  :type '(choice (const :tag "Do not include an email address." nil)
                 (string :tag "Include an email address.")))

(defun biblio-crossref--forward-bibtex (metadata forward-to)
  "Forward BibTeX for CrossRef entry METADATA to FORWARD-TO."
  (biblio-doi-forward-bibtex (biblio-alist-get 'doi metadata) forward-to))

(defun biblio-crossref--format-affiliation (affiliation)
  "Format AFFILIATION for CrossRef search results."
  (mapconcat (apply-partially #'biblio-alist-get 'name) affiliation ", "))

(defun biblio-crossref--format-author (author)
  "Format AUTHOR for CrossRef search results."
  (let-alist author
    (biblio-join " "
                 .given .family (biblio-parenthesize (biblio-crossref--format-affiliation .affiliation)))))

(defun biblio-crossref--extract-interesting-fields (item)
  "Prepare a CrossRef search result ITEM for display."
  (let-alist item
    (list (cons 'doi .DOI)
          (cons 'year (let ((year (aref (aref .issued.date-parts 0) 0)))
                        (and (numberp year) (number-to-string year))))
          (cons 'title (biblio-join " "
                         (biblio-join-1 ", " .title)
                         (biblio-parenthesize (biblio-join-1 ", " .subtitle))))
          (cons 'authors (seq-map #'biblio-crossref--format-author .author))
          (cons 'publisher .publisher)
          (cons 'container .container-title)
          (cons 'references (seq-concatenate 'list (list .DOI) .isbn))
          (cons 'type .type)
          (cons 'url .URL))))

(defun biblio-crossref--parse-search-results ()
  "Extract search results from CrossRef response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (unless (string= .status "ok")
      (display-warning 'biblio-crossref "CrossRef query failed"))
    (seq-map #'biblio-crossref--extract-interesting-fields .message.items)))

(defun biblio-crossref--url (query)
  "Create a CrossRef url to look up QUERY."
  (format "https://api.crossref.org/works?query=%s%s"
          (url-encode-url query)
          (if biblio-crossref-user-email-address
              (format "&mailto=%s" (url-encode-url biblio-crossref-user-email-address)) "")))

;;;###autoload
(defun biblio-crossref-backend (command &optional arg &rest more)
  "A CrossRef backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "CrossRef")
    (`prompt "CrossRef query: ")
    (`url (biblio-crossref--url arg))
    (`parse-buffer (biblio-crossref--parse-search-results))
    (`forward-bibtex (biblio-crossref--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-crossref-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-crossref-backend)

;;;###autoload
(defun biblio-crossref-lookup (&optional query)
  "Start a CrossRef search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-crossref-backend query))

;;;###autoload
(defalias 'crossref-lookup 'biblio-crossref-lookup)

(provide 'biblio-crossref)
;;; biblio-crossref.el ends here
