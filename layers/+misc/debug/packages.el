;;; packages.el --- debug layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <mfulz@olznet.de>
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
;; added to `debug-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `debug/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `debug/pre-init-PACKAGE' and/or
;;   `debug/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq debug-packages
  '(
    gdb-mi
    ))

(defun debug/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))


;;; packages.el ends here
