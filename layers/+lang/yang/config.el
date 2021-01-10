;;; config.el --- yang layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (defvar yang-check-using-rules "lint"
;;   "Value indicating which rules to use when checking yang syntax
;;   checking. This value is passed as an double-dash argument to
;;   pyang. As of this writing the following values are supported:

;;     bbf, ieee, ietf, lint, mef

;;   where lint represents RFC6087 rules, and the others correspond
;;   to the respective organization.
;;   ")

(defvar yang-pyang-rules "lint"
  "Rules to use when checking yang syntax. This value is
  prepended with double-dash and passed to pyang. The valid
  values at the time of this writing were as follows:

    bbf, ieee, ietf, mef, and lint

  lint being the default and referring to rules outlined in RFC
  6020.")

(defvar yang-pyang-extra-args nil
  "Any extra arguments to pass to pyang.")
