;;; packages.el --- nlinum layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: codefalling <codefalling@codefallings-MacBook-Pro.local>
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
;; added to `nlinum-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `nlinum/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `nlinum/pre-init-PACKAGE' and/or
;;   `nlinum/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst nlinum-packages
  '(
    nlinum
    (nlinum-relative :location (recipe
                                :fetcher github
                                :repo "CodeFalling/nlinum-relative"
                                ))
    (linum-relative :excluded t)
    )
  "The list of Lisp packages required by the nlinum layer.

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

(defun nlinum/init-nlinum ()
  (use-package nlinum
    :init
    (progn
      ;; replace linum-mode with nlinum-mode
      (advice-add 'linum-mode :around
                  (lambda (orig-fun &rest args) (nlinum-mode args)))

      ;; nlinum-mode format
      (setq nlinum-format "%4d")
      )
    ))

(defun nlinum/init-nlinum-relative ()
  (use-package nlinum-relative
    :commands (nlinum-relative-toggle nlinum-relative-on)
    :init
    (progn
      (when (eq dotspacemacs-line-numbers 'relative)
        (nlinum-relative-setup-evil)
        (add-hook 'nlinum-mode-hook
                  (lambda () (unless (bound-and-true-p nlinum-relative-mode)
                               (nlinum-relative-mode)))))
      (spacemacs/set-leader-keys "tr" 'nlinum-relative-toggle))
    :config
    (progn
      (setq nlinum-relative-current-symbol "->"))))


;;; packages.el ends here
