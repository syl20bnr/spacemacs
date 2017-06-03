;;; packages.el --- Shen Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shen-packages
      '(shen-mode
        ;; inf-shen is not in GNU ELPA, pending FSF copyright paperwork
        (inf-shen :location (recipe :fetcher git
                                    :repo "https://github.com/eschulte/shen-mode.git"
                                    :files ("inf-shen.el")))))

(defun shen/init-shen-mode ()
  (use-package shen-mode
    :defer t
    :mode "\\.shen\\'"
    :config
    (progn
      (evil-leader/set-key-for-mode 'shen-mode
        ;; e - eval
        "mel" 'shen-eval-last-sexp
        "med" 'shen-eval-defun
        "meg" 'shen-eval-defun-and-go
        "mer" 'shen-eval-region
        "met" 'shen-eval-region-and-go

        ;; compile
        "mf"  'shen-compile-file
        "md" 'shen-compile-defun
        "mg" 'shen-compile-defun-and-go

        ;; s - REPL
        "msi" 'inferior-shen
        "mss" 'switch-to-shen
        "msl" 'shen-load-file

        ;; h - help
        "mha" 'shen-show-arglist
        "mhs" 'shen-describe-sym
        "mhf" 'shen-show-function-documentation
        "mhv" 'shen-show-variable-documentation))))

(defun shen/init-inf-shen ()
  (use-package inf-shen
    :defer t))
