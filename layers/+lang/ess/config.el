;;; config.el --- ESS Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar ess-user-style-def nil
  "If non-nil, add the style to ess-style-alist")
(defvar ess-user-style 'RStudio
  "Use the named style for ess-set-style.  Defaults to 'RStudio")
(defvar ess-enable-smart-equals nil
  "If non-nil smart-equal support is enabled")
(defvar ess-user-R-fontlock nil
  "If non-nil, set ess-R-font-lock-keywords to its value.")
(defvar ess-user-roxy-template nil
  "If non-null, set ess-roxy-template-alist to its value")
