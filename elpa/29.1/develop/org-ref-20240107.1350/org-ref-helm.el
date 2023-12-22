;;; org-ref-helm.el --- org-ref interface to helm-bibtex -*- lexical-binding: t; -*-

;; Copyright(C) 2014-2022 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((helm-bibtex "0") (org-ref "0"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Adapted from helm-bibtex, mostly so you get the default action to insert a citation

(require 'org-ref-core)
(require 'helm-bibtex)

(defvar helm-bibtex-local-bib)  ; to quiet compiler. 

;;; Code:

(helm-bibtex-helmify-action org-ref-insert-cite-keys org-ref-helm-bibtex-insert-citation)

(defvar org-ref-helm-source-bibtex
  (helm-build-sync-source "BibTeX entries"
    :header-name (lambda (name)
                   (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
	     "Insert citation"            'org-ref-helm-bibtex-insert-citation
	     "Insert reference"           'helm-bibtex-insert-reference
             "Open PDF, URL or DOI"       'helm-bibtex-open-any
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             "Edit notes"                 'helm-bibtex-edit-notes
             "Show entry"                 'helm-bibtex-show-entry
             "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
  "Source for searching in BibTeX files.")

(defvar org-ref-helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . bibtex-completion-fallback-candidates)
    (no-matchplugin)
    (nohighlight)
    (action          . (lambda (candidate) (bibtex-completion-fallback-action candidate helm-pattern))))
  "Source for online look-up.")

;; Helm-bibtex command:

;;;###autoload
(defun org-ref-cite-insert-helm (&optional arg local-bib input)
  "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications)."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
					   (member (cons "=key=" key)
						   (cdr cand)))
                                         candidates))))
    (helm :sources (list org-ref-helm-source-bibtex org-ref-helm-source-fallback-options)
          :full-frame helm-bibtex-full-frame
          :buffer "*helm bibtex*"
          :input input
          :preselect (lambda ()
		       (and preselect
			    (> preselect 0)
			    (helm-next-line preselect)))
          :candidate-number-limit (max 500 (1+ (or preselect 0)))
          :bibtex-candidates candidates
          :bibtex-local-bib local-bib)))


(setq org-ref-insert-cite-function 'org-ref-cite-insert-helm)


(provide 'org-ref-helm)

;;; org-ref-helm.el ends here
