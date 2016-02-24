;;; packages.el --- graphviz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Peter Hoeg <peter@hoeg.com>
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
;; added to `graphviz-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `graphviz/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `graphviz/pre-init-PACKAGE' and/or
;;   `graphviz/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst graphviz-packages
  '(graphviz-dot-mode)
  "The list of Lisp packages required by the graphviz layer.")

(defun graphviz/init-graphviz-dot-mode ()
  (use-package graphviz-dot-mode
    :commands (graphviz-dot-mode)
    :mode (("\\.diag$"      . graphviz-dot-mode)
           ("\\.blockdiag$" . graphviz-dot-mode)
           ("\\.nwdiag$"    . graphviz-dot-mode)
           ("\\.rackdiag$"  . graphviz-dot-mode)
           ("\\.dot$"       . graphviz-dot-mode)
           ("\\.gv"         . graphviz-dot-mode))))

;;; packages.el ends here
