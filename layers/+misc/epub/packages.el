;;; packages.el --- epub layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jeremy Dormitzer <jeremy.dormitzer@gmail.com>
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
;; added to `epub-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `epub/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `epub/pre-init-PACKAGE' and/or
;;   `epub/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst epub-packages
  '((nov :location elpa))
  "The list of Lisp packages required by the epub layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun epub/setup-nov-major-mode-keys ()
  (spacemacs/set-leader-keys-for-major-mode 'nov-mode
    "g" 'nov-render-document
    "v" 'nov-view-source
    "V" 'nov-view-content-source
    "m" 'nov-display-metadata
    "n" 'nov-next-document
    "]" 'nov-next-document
    "p" 'nov-previous-document
    "[" 'nov-previous-document
    "t" 'nov-goto-toc))

(defun epub/init-nov ()
  (let ((epub/nov-text-width
         (if (boundp 'epub-nov-text-width) epub-nov-text-width 80)))
    (use-package nov
      :mode ("\\.epub\\'" . nov-mode)
      :init (setq nov-text-width epub/nov-text-width)
      :config (epub/setup-nov-major-mode-keys))))

;;; packages.el ends here
