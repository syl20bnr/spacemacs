;;; config.el --- Auto-completion configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Company -------------------------------------------------------------------

(defvar spacemacs-default-company-backends
  '((company-semantic company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defvar-local auto-completion-front-end 'company
  "Which auto-completion front end to use.")

(defvar auto-completion-return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar auto-completion-tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar auto-completion-complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar auto-completion-complete-with-key-sequence-delay 0.1
  "Timeout (seconds) when waiting for the second key of
`auto-completion-complete-with-key-sequence'.")

(defvar auto-completion-minimum-prefix-length 2
  "The minimum prefix length for idle completion.")

(defvar auto-completion-idle-delay 0.2
  "Delay (seconds) before completions are shown.")

(defvar auto-completion-enable-snippets-in-popup nil
  "If non nil show snippets in the auto-completion popup.")

(defvar auto-completion-enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used.")

(defvar auto-completion-enable-help-tooltip nil
  "If non nil the docstring appears in a tooltip.
If set to `manual', help tooltip appears only when invoked
manually.")

(defvar auto-completion-use-company-box nil
  "If non nil company-box is used.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

(defvar auto-completion-private-snippets-directory nil
  "Configurable private snippets directory.")
