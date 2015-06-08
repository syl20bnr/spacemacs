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

(defvar auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar auto-completion-enable-snippets-in-popup nil
  "If non nil show snippets in the auto-completion popup.")

(defvar auto-completion-enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used.")

(defvar auto-completion-enable-help-tooltip nil
  "If non nil the docstring appears in a tooltip.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

(defvar auto-completion-private-snippets-directory nil
  "Configurable private snippets directory.")
