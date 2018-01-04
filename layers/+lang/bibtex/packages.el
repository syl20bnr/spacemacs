;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Joshua Ellis <josh@jpellis.me>
;; URL: https://github.com/JP-Ellis
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq bibtex-packages
      '(
        auctex
        org
        org-ref
        markdown-mode
        (helm-bibtex :requires helm)
        biblio
        biblio-core
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
    :commands (org-ref-bibtex-next-entry
               org-ref-bibtex-previous-entry
               org-ref-open-in-browser
               org-ref-open-bibtex-notes
               org-ref-open-bibtex-pdf
               org-ref-bibtex-hydra/body
               org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
               org-ref-sort-bibtex-entry
               arxiv-add-bibtex-entry
               arxiv-get-pdf-add-bibtex-entry
               doi-utils-add-bibtex-entry-from-doi
               isbn-to-bibtex
               pubmed-insert-bibtex-from-pmid)
    :init
    (progn
      (evil-define-key 'normal bibtex-mode-map
        (kbd "C-j") 'org-ref-bibtex-next-entry
        (kbd "C-k") 'org-ref-bibtex-previous-entry
        "gj" 'org-ref-bibtex-next-entry
        "gk" 'org-ref-bibtex-previous-entry)

      (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
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
        "lp" 'pubmed-insert-bibtex-from-pmid))))

(defun bibtex/pre-init-org-ref ()
  (add-hook 'org-mode-hook (lambda () (require 'org-ref))))

(defun bibtex/post-init-markdown-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
    "ic" 'org-ref-helm-insert-cite-link))

(defun bibtex/init-helm-bibtex ())
(defun bibtex/init-biblio ())
(defun bibtex/init-biblio-core ())

