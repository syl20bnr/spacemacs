;;; packages.el --- orgwiki layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Swaroop C H <swaroop@swaroopch.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `orgwiki-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `orgwiki/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `orgwiki/pre-init-PACKAGE' and/or
;;   `orgwiki/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst orgwiki-packages
  '((org-wiki :location (recipe
                         :fetcher github
                         :repo "caiorss/org-wiki"))))


(defun orgwiki/init-org-wiki ()
  (use-package org-wiki
    :init (progn
            (spacemacs/declare-prefix "aow" "orgwiki")
            (spacemacs/set-leader-keys
              ;; (a)sset link
              "aowa" 'spacemacs/orgwiki-insert-asset-link
              ;; open asset (d)irectory
              "aowd" 'org-wiki-asset-open
              ;; (h)ome
              "aowh" 'org-wiki-index
              ;; (i)nsert link
              "aowi" 'org-wiki-insert
              ;; (n)ew page
              "aown" 'org-wiki-make-page)
            (when (configuration-layer/layer-usedp 'helm)
              (spacemacs/set-leader-keys
                ;; (j)ump to page
                "aowj" 'org-wiki-helm)))))

;;; packages.el ends here
