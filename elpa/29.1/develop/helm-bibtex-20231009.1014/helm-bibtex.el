;;; helm-bibtex.el --- A bibliography manager based on Helm

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/tmalsburg/helm-bibtex
;; Version: 2.0.1
;; Package-Requires: ((bibtex-completion "1.0.0") (helm "1.5.5") (cl-lib "0.5") (emacs "24.1"))

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

;; A bibliography manager for Emacs, based on Helm and the
;; bibtex-completion backend.
;;
;; News:
;; - 09/06/2018: Added virtual APA field `author-or-editor` for use in
;;   notes templates.
;; - 02/06/2018: Reload bibliography proactively when bib files are
;;   changed.
;; - 21/10/2017: Added support for multiple PDFs and other file
;;   types.  See `bibtex-completion-pdf-extension' and
;;   `bibtex-completion-find-additional-pdfs' for details.
;; - 10/10/2017: Added support for ~@string~ constants.
;; - 02/10/2017: Date field is used when year is undefined.
;; - 29/09/2017: BibTeX entry, citation macro, or org-bibtex entry at
;;   point, will be pre-selected in helm-bibtex and ivy-bibtex giving
;;   quick access to PDFs and other functions.
;;
;; See NEWS.org for old news.
;;
;; Key features:
;; - Quick access to your bibliography from within Emacs
;; - Powerful search capabilities
;; - Provides instant search results as you type
;; - Tightly integrated with LaTeX authoring, emails, Org mode, etc.
;; - Open the PDFs, URLs, or DOIs associated with an entry
;; - Insert LaTeX cite commands, Ebib links, or Pandoc citations,
;;   BibTeX entries, or plain text references at point, attach PDFs to
;;   emails
;; - Support for note taking
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;; - Imports BibTeX entries from CrossRef and other sources.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Install:

;; Put this file in a directory included in your load path or install
;; helm-bibtex from MELPA (preferred).  Then add the following in your
;; Emacs startup file:
;;
;;     (require 'helm-bibtex)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'helm-bibtex "helm-bibtex" "" t)
;;
;; Requirements are parsebib, helm, s, dash, and f.  The easiest way
;; to install these packages is through MELPA.  Make sure helm is
;; properly configured (see
;; https://github.com/emacs-helm/helm#install-from-emacs-packaging-system).
;;
;; Let helm-bibtex know where it can find your bibliography by setting
;; the variable `bibtex-completion-bibliography'.  See the manual for
;; more details:
;;
;;   https://github.com/tmalsburg/helm-bibtex#minimal-configuration

;;; Usage:

;; You can search entries using the command `helm-bibtex'.  Select an
;; entry and press TAB to access all available actions.  At the end of
;; the list of matches you find some dummy entries that can be used
;; for searching in online databases.  Apart from that, familiarize
;; yourself with Helm.  It's more powerful that you might think.

;;; Code:

(require 'helm)
(require 'helm-net)
(require 'helm-easymenu)
(require 'helm-files)
(require 'bibtex-completion)

;; Silence the byte compiler
(eval-when-compile (defvar helm-bibtex-local-bib))

;; The following allows people to continue using their old helm-bibtex
;; configurations:

(cl-loop
 for var in '("bibliography" "library-path" "pdf-open-function"
              "pdf-symbol" "format-citation-functions" "notes-path"
              "notes-template-multiple-files"
              "notes-template-one-file" "notes-key-pattern"
              "notes-extension" "notes-symbol" "fallback-options"
              "browser-function" "additional-search-fields"
              "no-export-fields" "cite-commands"
              "cite-default-command"
              "cite-prompt-for-optional-arguments"
              "cite-default-as-initial-input" "pdf-field")
 for oldvar = (intern (concat "helm-bibtex-" var))
 for newvar = (intern (concat "bibtex-completion-" var))
 do
 (define-obsolete-variable-alias oldvar newvar "2016-03-20"))

;; Helm-specific configurations:

(defcustom helm-bibtex-full-frame t
  "Non-nil means open `helm-bibtex' using the entire window.
When nil, the window will split below."
  :group 'bibtex-completion
  :type 'boolean)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["BibTeX" helm-bibtex t])

;; Candidate formatter:

;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun helm-bibtex-window-width ()
  "Return the width of the window to pass to `helm-bibtex-candidates-formatter'."
  (1- (window-body-width)))

(defun helm-bibtex-candidates-formatter (candidates _)
  "Format CANDIDATES for display in helm."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   collect (cons (bibtex-completion-format-entry entry width) entry-key)))

;; Warp bibtex-completion actions with some helm-specific code:

(defmacro helm-bibtex-helmify-action (action name)
  "Wrap ACTION in another function NAME.
Then pass the candidates marked in helm to ACTION.  Also uses
`with-helm-current-buffer' such that when ACTION inserts text it
comes out in the right buffer."
  `(defun ,name (_)
     (let ((keys (helm-marked-candidates :with-wildcard t)))
       (with-helm-current-buffer
         (,action keys)))))

(helm-bibtex-helmify-action bibtex-completion-open-pdf helm-bibtex-open-pdf)
(helm-bibtex-helmify-action bibtex-completion-open-url-or-doi helm-bibtex-open-url-or-doi)
(helm-bibtex-helmify-action bibtex-completion-open-any helm-bibtex-open-any)
(helm-bibtex-helmify-action bibtex-completion-insert-citation helm-bibtex-insert-citation)
(helm-bibtex-helmify-action bibtex-completion-insert-reference helm-bibtex-insert-reference)
(helm-bibtex-helmify-action bibtex-completion-insert-key helm-bibtex-insert-key)
(helm-bibtex-helmify-action bibtex-completion-insert-bibtex helm-bibtex-insert-bibtex)
(helm-bibtex-helmify-action bibtex-completion-add-PDF-attachment helm-bibtex-add-PDF-attachment)
(helm-bibtex-helmify-action bibtex-completion-edit-notes helm-bibtex-edit-notes)
(helm-bibtex-helmify-action bibtex-completion-show-entry helm-bibtex-show-entry)
(helm-bibtex-helmify-action bibtex-completion-add-pdf-to-library helm-bibtex-add-pdf-to-library)

;; Helm sources:

(defvar helm-source-bibtex
  (helm-build-sync-source "BibTeX entries"
    :header-name (lambda (name)
                   (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
             "Open PDF, URL or DOI"       'helm-bibtex-open-any
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Insert citation"            'helm-bibtex-insert-citation
             "Insert reference"           'helm-bibtex-insert-reference
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             "Edit notes"                 'helm-bibtex-edit-notes
             "Show entry"                 'helm-bibtex-show-entry
             "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
  "Source for searching in BibTeX files.")

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . bibtex-completion-fallback-candidates)
    (no-matchplugin)
    (nohighlight)
    (action          . (lambda (candidate) (bibtex-completion-fallback-action candidate helm-pattern))))
  "Source for online look-up.")

;; Helm-bibtex command:

;;;###autoload
(defun helm-bibtex (&optional arg local-bib input)
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
    (helm :sources (list helm-source-bibtex helm-source-fallback-options)
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

;;;###autoload
(defun helm-bibtex-with-local-bibliography (&optional arg)
  "Search BibTeX entries with local bibliography.

If none is found the global bibliography is used instead.  With a
prefix ARG the cache is invalidated and the bibliography
reloaded."
  (interactive "P")
  (let* ((local-bib (bibtex-completion-find-local-bibliography))
         (bibtex-completion-bibliography (or local-bib
                                             bibtex-completion-bibliography)))
    (helm-bibtex arg local-bib)))

;;;###autoload
(defun helm-bibtex-with-notes (&optional arg)
  "Search BibTeX entries with notes.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (cl-letf* ((candidates (bibtex-completion-candidates))
             ((symbol-function 'bibtex-completion-candidates)
              (lambda ()
                (seq-filter
                 (lambda (candidate) (assoc "=has-note=" candidate))
                 candidates))))
    (helm-bibtex)))

;; Helm-bibtex follow processor:

(defvar helm-bibtex-follow-actions-alist
  '(("Open PDF, URL or DOI"       . bibtex-completion-open-pdf)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi)
    ("Edit notes"                 . bibtex-completion-edit-notes)
    ("Show entry"                 . bibtex-completion-show-entry)
    ("Add PDF to library"         . bibtex-completion-add-pdf-to-library)))


(defcustom helm-bibtex-follow-full-frame nil
  "Non-nil means open ‘helm-bibtex-follow’ using the entire window.
When nil, the window will split below."
  :group 'bibtex-completion
  :type 'boolean)

;;;###autoload
(defun helm-bibtex-follow (&optional citation args)
  (interactive)
  (let ((candidates (bibtex-completion-candidates))
        (key (if citation
                 (org-element-property :key citation)
               (bibtex-completion-key-at-point))))
    (unless (cl-find-if (lambda (cand)
                          (member (cons "=key=" key)
                                  (cdr cand)))
                        candidates)
      (user-error "Bibtex-completion couldn't find entry with key \"%s\"." key))
    (let ((item-info
           (format "%s\t\t%s"
                   (bibtex-completion-apa-get-value
                    "title" (bibtex-completion-get-entry key))
                   (bibtex-completion-apa-get-value
                    "author" (bibtex-completion-get-entry key)))))
      (helm :sources
            (helm-build-sync-source item-info
              :candidates helm-bibtex-follow-actions-alist
              :action (lambda (x) (funcall x (list key))))
            :full-frame helm-bibtex-follow-full-frame
            :buffer "*helm bibtex follow*"))))


(org-cite-register-processor 'helm-bibtex-org-cite-follow
  :follow 'helm-bibtex-follow)


(provide 'helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bibtex.el ends here
