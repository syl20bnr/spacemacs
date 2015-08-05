;;; config.el --- Syntax Checking Layer configuration File for Spacemacs
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

;; Variables

(defvar syntax-checking-enable-tooltips t
  "If non nil some feedback are displayed in tooltips.")

(defvar syntax-checking-ispell-program "aspell"
  "External program to use for ispell. Defaults to \"aspell\".")

(defvar syntax-checking-ispell-dictionary "english"
  "Dictionary to use for ispell. Defaults to \"english\".")

;; Command Prefixes

(spacemacs/declare-prefix "S" "spelling")
