;;; packages.el --- geiser Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Joseph Cecil & Contributors
;;
;; Author: Joseph Cecil <joe.a.cecil@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq geiser-packages
    '(geiser))

(defun geiser/init-geiser ()
  (use-package geiser
    :defer t
    :init (progn
            (setq geiser-active-implementations '(guile chicken))
            (evil-leader/set-key-for-mode 'scheme-mode
                "mcc" 'geiser-compile-file
                "mcd" 'geiser-compile-definition
                "mcr" 'geiser-compile-region

                "meb" 'geiser-eval-buffer
                "med" 'geiser-eval-definition
                "mee" 'geiser-eval-last-sexp
                "mer" 'geiser-eval-region

                "mha" 'geiser-doc-symbol-at-point
                "mhA" 'geiser-autodoc-show
                "mhh" 'geiser-doc-look-up-manual

                "msi" 'geiser
                "mss" 'switch-to-geiser
                "msq" 'geiser-repl-exit)
            (evilify geiser-doc-mode geiser-doc-mode-map)
            (evilify geiser-repl-mode geiser-repl-mode-map))))
