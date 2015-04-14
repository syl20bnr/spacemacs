;;; packages.el --- typescript Layer packages File for Spacemacs
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

(defvar typescript-packages '(tss)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar typescript-excluded-packages '()
  "List of packages to exclude.")

(defun typescript/init-tss ()
  "Initialize my package"
  (use-package tss
    :mode ("\\.ts\\'" . typescript-mode)
    :defer t
    :init
    (progn
      (require 'typescript)
      (require 'tss)
      (evil-leader/set-key-for-mode 'typescript-mode
        "mh" 'tss-popup-help
        "md" 'tss-jump-to-definition
        "mcc" 'tss-run-flymake))
    :config
    (progn
      (tss-config-default))))
