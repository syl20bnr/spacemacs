;;; config.el --- Syntax Checking Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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

(defvar syntax-checking-auto-hide-tooltips nil
  "If non-nil and positive number, auto hide tooltips after number of seconds.")

(defvar syntax-checking-enable-by-default t
  "Enable syntax-checking by default.")

(defvar syntax-checking-use-original-bitmaps nil
  "If non-nil, use the original bitmaps from flycheck.")

(defvar syntax-checking-use-standard-error-navigation nil
  "If non-nil hook into emacs standard error navigation")

;; Command Prefixes

