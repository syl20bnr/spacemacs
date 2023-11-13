;;; biblio-ieee.el --- Lookup and import bibliographic entries from IEEE -*- lexical-binding: t -*-

;; Copyright (C) 2019  Clément Pit-Claudel

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
;; Lookup and download bibliographic records from IEEE Xplore using
;; `ieee-lookup'.
;;
;; This package uses `biblio-selection-mode', and plugs into the more general
;; `biblio' package (which see for more documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)

(defconst biblio-ieee--api-key "rfwzcsz3f9fkhklhii84xdfz"
  "API key used to query IEEE; for use only with biblio.el!")

(defconst biblio-ieee--api-root-url
  "https://ieeexploreapi.ieee.org/api/v1/search/articles")

(defun biblio-ieee--forward-bibtex (metadata forward-to)
  "Forward BibTeX for IEEE Xplore entry METADATA to FORWARD-TO."
  (biblio-doi-forward-bibtex (biblio-alist-get 'doi metadata) forward-to))

(defun biblio-ieee--format-author (author)
  "Format AUTHOR for IEEE Xplore search results."
  (let-alist author
    (biblio-join " " .full_name (biblio-parenthesize .affiliation))))

(defun biblio-ieee--extract-interesting-fields (item)
  "Prepare a IEEE Xplore search result ITEM for display."
  (let-alist item
    (list (cons 'doi .doi)
          (cons 'year .publication_year)
          (cons 'title .title)
          (cons 'authors (seq-map #'biblio-ieee--format-author .authors.authors))
          (cons 'publisher .publisher)
          (cons 'container .publication_title)
          (cons 'references (list .doi .isbn))
          (cons 'type (or .index_terms.author_terms.terms
                          .index_terms.ieee_terms.terms))
          (cons 'url .abstract_url)
          (cons 'direct-url .pdf_url)
          (cons 'open-access-status .access_type))))

(defun biblio-ieee--parse-search-results ()
  "Extract search results from IEEE Xplore response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (seq-map #'biblio-ieee--extract-interesting-fields .articles)))

(defun biblio-ieee--url (query)
  "Create an IEEE Xplore url to look up QUERY."
  (format "%s?querytext=%s&apikey=%s"
          biblio-ieee--api-root-url
          (url-encode-url query)
          (rot13 biblio-ieee--api-key)))

;;;###autoload
(defun biblio-ieee-backend (command &optional arg &rest more)
  "A IEEE Xplore backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "IEEE Xplore")
    (`prompt "IEEE Xplore query: ")
    (`url (biblio-ieee--url arg))
    (`parse-buffer (biblio-ieee--parse-search-results))
    (`forward-bibtex (biblio-ieee--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-ieee-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-ieee-backend)

;;;###autoload
(defun biblio-ieee-lookup (&optional query)
  "Start a IEEE search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-ieee-backend query))

;;;###autoload
(defalias 'ieee-lookup 'biblio-ieee-lookup)

(provide 'biblio-ieee)
;;; biblio-ieee.el ends here
