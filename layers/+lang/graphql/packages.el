;;; packages.el --- graphql layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
;; URL: https://github.com/thanhvg
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
;; added to `graphql-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `graphql/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `graphql/pre-init-PACKAGE' and/or
;;   `graphql/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst graphql-packages
  '(company
    prettier-js
    graphql-mode))

(defun graphql/init-graphql-mode ()
  (use-package graphql-mode
    :defer t
    :init
    (add-to-list 'spacemacs-jump-handlers-graphql-mode 'ahs-backward-definition))
    (when (configuration-layer/layer-used-p 'prettier)
     (spacemacs/declare-prefix-for-mode 'graphql-mode "m=" "format"))
    (spacemacs/declare-prefix-for-mode 'graphql-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'graphql-mode
      "s" 'graphql-send-query
      "e" 'graphql-select-endpoint
      "h" 'graphql-edit-headers))

(defun graphql/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-dabbrev
    :modes graphql-mode))

(defun graphql/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'graphql-mode))
