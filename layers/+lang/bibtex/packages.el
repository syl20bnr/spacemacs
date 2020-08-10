;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
    (helm-bibtex :requires helm)
    (ivy-bibtex :requires ivy)
    markdown-mode
    org
    org-ref))

(defun bibtex/post-init-auctex ()
  (cond ((configuration-layer/layer-used-p 'helm)
         (spacemacs/set-leader-keys-for-major-mode 'latex-mode
           "ic" 'org-ref-helm-insert-cite-link))
        ((configuration-layer/layer-used-p 'ivy)
         (spacemacs/set-leader-keys-for-major-mode 'latex-mode
           "ic" 'org-ref-ivy-insert-cite-link))))

(defun bibtex/init-helm-bibtex ()
  (use-package helm-bibtex
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
      "m" 'helm-bibtex)))

(defun bibtex/init-ivy-bibtex ()
  (use-package ivy-bibtex
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
      "m" 'ivy-bibtex)))

(defun bibtex/post-init-markdown-mode ()
  (cond ((configuration-layer/layer-used-p 'helm)
         (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
           "ic" 'org-ref-helm-insert-cite-link))
        ((configuration-layer/layer-used-p 'ivy)
         (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
           "ic" 'org-ref-ivy-insert-cite-link))))

(defun bibtex/post-init-org ()
  (cond ((configuration-layer/layer-used-p 'helm)
         (spacemacs/set-leader-keys-for-major-mode 'org-mode
           "ic" 'org-ref-helm-insert-cite-link))
        ((configuration-layer/layer-used-p 'ivy)
         (spacemacs/set-leader-keys-for-major-mode 'org-mode
           "ic" 'org-ref-ivy-insert-cite-link))))

(defun bibtex/init-org-ref ()
  (use-package org-ref
    :defer t
    :commands (org-ref-bibtex-next-entry
               org-ref-bibtex-previous-entry
               org-ref-ivy-insert-cite-link
               org-ref-helm-insert-cite-link
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
      (add-hook 'org-mode-hook (lambda () (require 'org-ref)))

      (evil-define-key 'normal bibtex-mode-map
        (kbd "C-j") 'org-ref-bibtex-next-entry
        (kbd "C-k") 'org-ref-bibtex-previous-entry
        "gj" 'org-ref-bibtex-next-entry
        "gk" 'org-ref-bibtex-previous-entry)

      (spacemacs/declare-prefix-for-mode 'bibtex-mode "ml" "lookup")
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
        "lp" 'pubmed-insert-bibtex-from-pmid))
    :config
    (progn
      ;; override org-ref's default helm completion with ivy
      (when (configuration-layer/layer-used-p 'ivy)
        (require 'org-ref-ivy)))))
