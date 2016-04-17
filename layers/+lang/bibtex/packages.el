;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Joshua Ellis <josh@jpellis.me>
;; URL: https://github.com/JP-Ellis
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst bibtex-packages
      '(
        auctex
        org
        org-ref
        markdown-mode
        ))

(defun bibtex/post-init-auctex ()
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "ic" 'org-ref-helm-insert-cite-link))

(defun bibtex/post-init-org ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ic" 'org-ref-helm-insert-cite-link))

(defun bibtex/init-org-ref ()
  (use-package org-ref
    :defer t
    :config (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
              ;; Navigation
              "j" 'org-ref-bibtex-next-entry
              "k" 'org-ref-bibtex-previous-entry

              ;; Open
              "b" 'org-ref-open-in-browser
              "n" 'org-ref-open-bibtex-notes
              "p" 'org-ref-open-bibtex-pdf

              ;; Misc
              "h" 'org-ref-bibtex-hydra/body
              "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
              "s" 'org-ref-sort-bibtex-entry

              ;; Lookup utilities
              "la" 'arxiv-add-bibtex-entry
              "lA" 'arxiv-get-pdf-add-bibtex-entry
              "ld" 'doi-utils-add-bibtex-entry-from-doi
              "li" 'isbn-to-bibtex
              "lp" 'pubmed-insert-bibtex-from-pmid)))

(defun bibtex/post-init-markdown-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
    "ic" 'org-ref-helm-insert-cite-link))

;;; packages-config.el ends here
