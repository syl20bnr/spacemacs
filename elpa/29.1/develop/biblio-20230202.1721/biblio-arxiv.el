;;; biblio-arxiv.el --- Lookup and import bibliographic entries from arXiv -*- lexical-binding: t -*-

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
;; Lookup and download bibliographic records from arXiv using `arxiv-lookup'.
;; When a DOI is available, the metadata is fetched from the DOI's issuer;
;; otherwise, this package uses arXiv's metadata to generate an entry.
;;
;; This file implements a backend for the for the `biblio' package (which see for more
;; documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)
(require 'timezone)

(defgroup biblio-arxiv nil
  "arXiv support in biblio.el"
  :group 'biblio)

(defcustom biblio-arxiv-bibtex-header "online"
  "Which header to use for BibTeX entries generated from arXiv metadata."
  :group 'biblio
  :type 'string)

(defun biblio-arxiv--build-bibtex-1 (metadata)
  "Create an unformated BibTeX record for METADATA."
  (let-alist metadata
    (format "@%s{NO_KEY,
author = {%s},
title = {{%s}},
year = {%s},
archivePrefix = {arXiv},
eprint = {%s},
primaryClass = {%s}}"
            biblio-arxiv-bibtex-header
            (biblio-join-1 " AND " .authors)
            .title .year .identifier .category)))

(defun biblio-arxiv--build-bibtex (metadata)
  "Create a BibTeX record for METADATA."
  (let-alist metadata
    (message "Auto-generating a BibTeX entry for %S." .id)
    (biblio-format-bibtex (biblio-arxiv--build-bibtex-1 metadata) t)))

(defun biblio-arxiv--forward-bibtex (metadata forward-to)
  "Forward BibTeX for arXiv entry METADATA to FORWARD-TO."
  (let-alist metadata
    (if (seq-empty-p .doi)
        (funcall forward-to (biblio-arxiv--build-bibtex metadata))
      (biblio-doi-forward-bibtex .doi forward-to))))

(defun biblio-arxiv--format-author (author)
  "Format AUTHOR for arXiv search results."
  (when (eq (car-safe author) 'author)
    (let-alist (cdr author)
      (biblio-join " "
        (cadr .name)
        (biblio-parenthesize (cadr .arxiv:affiliation))))))

(defun biblio-arxiv--extract-id (id)
  "Extract identifier from ID, the URL of an arXiv abstract."
  (replace-regexp-in-string "https?://arxiv.org/abs/" "" id))

(defun biblio-arxiv--pdf-url (id)
  "Extract PDF url from ID of an arXiv entry."
  (when id
    (concat "https://arxiv.org/pdf/" id)))

(defun biblio-arxiv--extract-interesting-fields (entry)
  "Prepare an arXiv search result ENTRY for display."
  (let-alist entry
    (let ((id (biblio-arxiv--extract-id (cadr .id))))
      (list (cons 'doi (cadr .arxiv:doi))
            (cons 'identifier id)
            (cons 'year (aref (timezone-parse-date (cadr .published)) 0))
            (cons 'title (cadr .title))
            (cons 'authors (seq-map #'biblio-arxiv--format-author entry))
            (cons 'container (cadr .arxiv:journal_ref))
            (cons 'category
                  (biblio-alist-get 'term (car .arxiv:primary_category)))
            (cons 'references (list (cadr .arxiv:doi) id))
            (cons 'type "eprint")
            (cons 'url (biblio-alist-get 'href (car .link)))
            (cons 'direct-url (biblio-arxiv--pdf-url id))))))

(defun biblio-arxiv--entryp (entry)
  "Check if ENTRY is an arXiv entry."
  (eq (car-safe entry) 'entry))

(defun biblio-arxiv--parse-search-results ()
  "Extract search results from arXiv response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (xml-parse-region (point-min) (point-max))
    (seq-map #'biblio-arxiv--extract-interesting-fields
             (seq-filter #'biblio-arxiv--entryp .feed))))

(defun biblio-arxiv--url (query)
  "Create an arXiv url to look up QUERY."
  (format "https://export.arxiv.org/api/query?search_query=%s"
          (url-encode-url query)))

;;;###autoload
(defun biblio-arxiv-backend (command &optional arg &rest more)
  "A arXiv backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "arXiv")
    (`prompt "arXiv query: ")
    (`url (biblio-arxiv--url arg))
    (`parse-buffer (biblio-arxiv--parse-search-results))
    (`forward-bibtex (biblio-arxiv--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-arxiv-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-arxiv-backend)

;;;###autoload
(defun biblio-arxiv-lookup (&optional query)
  "Start an arXiv search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-arxiv-backend query))

;;;###autoload
(defalias 'arxiv-lookup 'biblio-arxiv-lookup)

(provide 'biblio-arxiv)
;;; biblio-arxiv.el ends here
