;;; packages.el --- column-enforce-mode layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Daniel Luna <dancluna@dcl-notebook>
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
;; added to `column-enforce-mode-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `column-enforce-mode/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `column-enforce-mode/pre-init-PACKAGE' and/or
;;   `column-enforce-mode/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst column-enforce-mode-packages
  '(column-enforce-mode))

(defun column-enforce-mode/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :defer t
    :init (add-hook 'prog-mode-hook 'column-enforce-mode)))

;;; packages.el ends here
