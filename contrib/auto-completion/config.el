;;; config.el --- Auto-completion configuration File for Spacemacs
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

;; Company -------------------------------------------------------------------

;; not used for now
(defvar auto-completion-enable-company-yasnippet t
  "If non nil enable yasnippet for all company backends.
Not used for now.")

(defvar auto-completion-enable-company-help-tooltip nil
  "If non nil the docstring appears in a tooltip.")

(defvar auto-completion-use-tab-instead-of-enter nil
  "If non nil use tab instead of enter for completion.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")
