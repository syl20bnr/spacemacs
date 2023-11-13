;;; biblio-doi.el --- Retrieve BibTeX entries by DOI -*- lexical-binding: t -*-

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
;; Retrieve and insert BibTeX records by DOI using `doi-insert-bibtex'.
;; Information is retrieved from DOI issuing sites of each DOI using the
;; “application/x-bibtex” and “text/bibliography” request types, falling back to
;; CrossCite if unavailable.
;;
;; This package integrates with `biblio-selection-mode', and is part of the more
;; general `biblio' package (which see for more documentation).

(require 'biblio-core)

;;; Code:

(defun biblio-doi--dx-url (doi)
  "Create a doi.org url for DOI."
  (format "https://doi.org/%s" doi))

(defun biblio-doi--crosscite-url (doi)
  "Create a crosscite URL to use as a fallback for DOI.
Not all content providers provide BibTeX formatted entries, so
instead of failing reroute the request through crosscite, which
requests a generic format and crates the BibTeX on its own."
  (format "https://crosscite.org/citeproc/format?doi=%s&style=bibtex&lang=en-US" doi))

(defconst biblio-doi--dx-mime-accept
  ;; “Accept:” header; Zenodo recognizes x-bibtex but not text/bibliography
  "text/bibliography;style=bibtex, application/x-bibtex")

(defun biblio-doi--set-mime-accept ()
  "Set `url-mime-accept-string' before contacting the DOI server."
  ;; Ugly: let-binding or buffer-locally setting `url-mime-accept-string' does
  ;; not work, because `url-http-create-request' can be called from a
  ;; sentinel, or from an entirely new buffer (after a redirection).
  (setq url-mime-accept-string biblio-doi--dx-mime-accept))

(defun biblio-doi--restore-mime-accept ()
  "Restore `url-mime-accept-string'."
  (kill-local-variable 'url-mime-accept-string)
  (setq-default url-mime-accept-string nil))

(defun biblio-doi--insert (bibtex buffer)
  "Insert formatted BIBTEX into BUFFER."
  (with-current-buffer buffer
    (insert bibtex "\n\n")))

(defun biblio-doi--generic-url-callback-1 (errors forward-to)
  "Helper function for `biblio-doi--generic-url-callback'.
ERRORS, FORWARD-TO: see there."
  (funcall forward-to (unless errors (biblio-response-as-utf-8))))

(defun biblio-doi--generic-url-callback (cleanup-fn forward-to)
  "Make an URL-ready callback.
Call CLEANUP-FN in any case, and FORWARD-TO with BibTeX source
or nil depending on whether an error occured.  If error 406
occurs, forward nil; otherwise, signal the error.  This is
essentially a thin wrapper around `biblio-generic-url-callback'."
  (biblio-generic-url-callback
   (lambda (&optional errors)
     "Handle response from BibTeX server."
     (biblio-doi--generic-url-callback-1 errors forward-to))
   cleanup-fn '(http . 406)))

(defun biblio-doi--crosscite-callback (forward-to)
  "Generate a handler for response of CrossCite server.
FORWARD-TO is the callback to call with the results of the search."
  (biblio-doi--generic-url-callback #'ignore forward-to))

(defun biblio-doi--forward-bibtex-crosscite (doi forward-to)
  "Forward BibTeX entry for DOI from CrossCite to FORWARD-TO."
  (biblio-url-retrieve (biblio-doi--crosscite-url doi) (biblio-doi--crosscite-callback forward-to)))

(defun biblio-doi--dx-callback (forward-to)
  "Generate a handler for response of DX server.
FORWARD-TO is the callback to call with the results of the search."
  (biblio-doi--generic-url-callback #'biblio-doi--restore-mime-accept forward-to))

(defun biblio-doi--forward-bibtex-dx (doi forward-to)
  "Forward BibTeX entry for DOI from doi.org to FORWARD-TO."
  (biblio-doi--set-mime-accept)
  (biblio-url-retrieve (biblio-doi--dx-url doi) (biblio-doi--dx-callback forward-to)))

(defun biblio-doi-forward-bibtex (doi forward-to)
  "Pass BibTeX entry for DOI to FORWARD-TO."
  (biblio-doi--forward-bibtex-dx
   doi (lambda (result)
         (if result (funcall forward-to result)
           (biblio-doi--forward-bibtex-crosscite doi forward-to)))))

;;;###autoload
(defun biblio-doi-insert-bibtex (doi)
  "Insert BibTeX entry matching DOI."
  (interactive "MDOI: ")
  (let ((target-buffer (current-buffer)))
    (biblio-doi-forward-bibtex
     (biblio-cleanup-doi doi)
     (lambda (result)
       (biblio-doi--insert
        (biblio-format-bibtex result biblio-bibtex-use-autokey)
        target-buffer)))))

(defalias 'doi-insert-bibtex 'biblio-doi-insert-bibtex)

(provide 'biblio-doi)
;;; biblio-doi.el ends here
