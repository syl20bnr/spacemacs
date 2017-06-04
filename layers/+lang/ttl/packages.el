;;; packages.el --- ttl layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Carl Lange <carl@flax.ie>
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
;; added to `ttl-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ttl/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ttl/pre-init-PACKAGE' and/or
;;   `ttl/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ttl-packages
  '(
    (ttl-mode :location local)
    ))


(defun ttl/init-ttl-mode ()
  "Initialize ttl-mode"
  (use-package ttl-mode))
;;; packages.el ends here
