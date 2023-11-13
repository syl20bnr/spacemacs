;;; ebib-biblio.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2022 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It contains
;; functions that integrate [biblio.el] with Ebib.
;;
;; [biblio]: https://github.com/cpitclaudel/biblio.el
;;
;; To use this code, `require' it in your init file.  This adds the key "B" to
;; the index mode map, which fetches a BibTeX entry by DOI
;; (`ebib-biblio-import-doi').  You should also add a key to
;; `biblio-selection-mode-map' and bind it to `ebib-biblio-selection-import':
;;
;; (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import)

;;; Code:

(require 'biblio nil 'noerror)

(defvar ebib--databases)
(defvar ebib--cur-db)
(defvar ebib-index-mode-map)
(declare-function ebib-read-database "ext:ebib.el" (prompt &optional databases))
(declare-function ebib-import-entries "ext:ebib.el" (&optional db))

(declare-function biblio-doi-forward-bibtex "ext:biblio-doi.el" (doi forward-to))
(declare-function biblio-cleanup-doi "ext:biblio-core.el" (doi))
(declare-function biblio-doi--insert "ext:biblio-doi.el" (bibtex buffer))
(declare-function biblio-format-bibtex "ext:biblio-core.el" (bibtex &optional autokey))
(defvar biblio-bibtex-use-autokey)
(declare-function biblio--selection-forward-bibtex "ext:biblio-core.el" (forward-to &optional quit))

;;;###autoload
(defun ebib-biblio-import-doi (doi)
  "Fetch a BibTeX entry from a remote server by its DOI using `biblio.el'.
The entry is stored in the current database."
  (interactive "MDOI: ")
  (biblio-doi-forward-bibtex (biblio-cleanup-doi doi)
                             (lambda (result)
                               (with-temp-buffer
                                 (biblio-doi--insert (biblio-format-bibtex result biblio-bibtex-use-autokey)
                                                     (current-buffer))
                                 (ebib-import-entries ebib--cur-db)))))

(defun ebib-biblio-selection-import-callback (bibtex _entry)
  "Add a BibTeX entry to the current Ebib database.
BIBTEX is the textual representation of the entry, ENTRY is its
metadata."
  (with-temp-buffer
    (insert bibtex)
    (ebib-import-entries ebib--cur-db)))

(defun ebib-biblio-selection-import ()
  "Import the current entry in the `biblio.el' selection buffer into Ebib."
  (interactive)
  (biblio--selection-forward-bibtex #'ebib-biblio-selection-import-callback))

(provide 'ebib-biblio)

;;; ebib-biblio.el ends here
