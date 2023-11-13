;;; biblio-hal.el --- Lookup and import bibliographic entries from HAL (archives ouvertes) -*- lexical-binding: t -*-

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
;; Lookup and download bibliographic records from HAL using `hal-lookup'.
;;
;; This file implements a backend for the for the `biblio' package (which see for more
;; documentation).

;;; Code:

(require 'biblio-core)

(defun biblio-hal--forward-bibtex (metadata forward-to)
  "Forward BibTeX for HAL entry METADATA to FORWARD-TO."
  (funcall forward-to (biblio-alist-get 'bibtex metadata)))

;; (defun biblio-hal--format-author (author)
;;   "Format AUTHOR for HAL search results."
;;   (pcase author
;;     (`(,author . ,affiliation)
;;      (biblio-join " " author (biblio-parenthesize affiliation)))))

(defun biblio-hal--extract-interesting-fields (item)
  "Prepare a HAL search result ITEM for display."
  (let-alist item
    (list (cons 'doi .doiId_s)
          (cons 'year (aref (timezone-parse-date .producedDate_tdate) 0))
          (cons 'bibtex .label_bibtex)
          (cons 'title (biblio-join " "
                         (biblio-join-1 ", " .title_s)
                         (biblio-parenthesize
                          (biblio-join-1 ", " .subtitle_s))))
          (cons 'authors .authFullName_s)
          ;; Too many institutions? (biblio-parenthesize (biblio-join-1 ", " .structName_s))
          (cons 'publisher .journalPublisher_s)
          (cons 'container .journalTitle_s)
          (cons 'references (biblio-remove-empty
                             (list .doiId_s .halId_s .arxivId_s)))
          (cons 'type .submitType_s)
          (cons 'url .uri_s)
          (cons 'direct-url (car (append .files_s nil))))))

(defun biblio-hal--parse-search-results ()
  "Extract search results from HAL response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (unless .response
      (display-warning 'biblio-hal "HAL query failed"))
    (seq-map #'biblio-hal--extract-interesting-fields .response.docs)))

(defun biblio-hal--url (query)
  "Create a HAL url to look up QUERY."
  (format "https://api.archives-ouvertes.fr/search/?q=%s&wt=%s&fl=%s"
          (url-encode-url query) "json"
          (biblio-join "," ;; Use ‘*’ to show all fields
            "arxivId_s" "halId_s" "doiId_s" ;; "journalIssn_s"
            "title_s" "subtitle_s" "authFullName_s" "structName_s"
            "journalPublisher_s" "submitType_s" ;; "abstract_s"
            ;; "journalTitle_s" "volume_s" "issue_s"  "page_s" "writingDate_s"
            "label_bibtex" "files_s" "uri_s" "producedDate_tdate")))

;;;###autoload
(defun biblio-hal-backend (command &optional arg &rest more)
  "A HAL backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "HAL")
    (`prompt "HAL (archives ouvertes) query: ")
    (`url (biblio-hal--url arg))
    (`parse-buffer (biblio-hal--parse-search-results))
    (`forward-bibtex (biblio-hal--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-hal-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-hal-backend)

;;;###autoload
(defun biblio-hal-lookup (&optional query)
  "Start a HAL search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-hal-backend query))

;;;###autoload
(defalias 'hal-lookup 'biblio-hal-lookup)

(provide 'biblio-hal)
;;; biblio-hal.el ends here
