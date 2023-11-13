;;; ledger-fonts.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.



;;; Commentary:
;; All of the faces for ledger mode are defined here.

;;; Code:

(require 'ledger-navigate)
(require 'ledger-regex)
(require 'ledger-state)
(require 'ledger-fontify)

(defgroup ledger-faces nil "Ledger mode highlighting" :group 'ledger)

(defface ledger-font-auto-xact-face
  `((t :inherit font-lock-negation-char-face))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-periodic-xact-face
  `((t :inherit font-lock-constant-face))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-xact-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Default face for cleared transaction"
  :group 'ledger-faces)

(defface ledger-font-xact-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending transaction"
  :group 'ledger-faces)

(defface ledger-font-payee-uncleared-face
  `((t :inherit error))
  "Default face for Ledger"
  :group 'ledger-faces)

(defface ledger-font-payee-cleared-face
  `((t :inherit shadow))
  "Default face for cleared (*) payees"
  :group 'ledger-faces)

(defface ledger-font-payee-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) payees"
  :group 'ledger-faces)

(defface ledger-font-xact-highlight-face
  `((t
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit ledger-occur-xact-face))
  "Default face for transaction under point"
  :group 'ledger-faces)

(defface ledger-font-pending-face
  `((t :inherit warning))
  "Default face for pending (!) transactions"
  :group 'ledger-faces)

(defface ledger-font-other-face
  `((t :inherit font-lock-type-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-directive-face
  `((t :inherit font-lock-preprocessor-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-account-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-account-name-face
  `((t :inherit font-lock-variable-name-face))
  "Face for account names in account and alias directives"
  :group 'ledger-faces)

(defface ledger-font-note-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for note subdirectives"
  :group 'ledger-faces)

(defface ledger-font-note-text-face
  `((t :inherit font-lock-doc-face))
  "Face for note subdirective text"
  :group 'ledger-faces)

(defface ledger-font-default-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for default subdirectives"
  :group 'ledger-faces)

(defface ledger-font-price-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-price-date-face
  `((t :inherit default))
  "Face for date and time in price directive"
  :group 'ledger-faces)

(defface ledger-font-price-symbol-face
  `((t :inherit font-lock-constant-face))
  "Face for symbol in price directive"
  :group 'ledger-faces)

(defface ledger-font-price-face
  `((t :inherit default))
  "Face for price in price directive"
  :group 'ledger-faces)

(defface ledger-font-apply-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-apply-account-face
  `((t :inherit default))
  "Face for argument of apply account directive"
  :group 'ledger-faces)

(defface ledger-font-apply-tag-face
  `((t :inherit default))
  "Face for argument of apply tag directive"
  :group 'ledger-faces)

(defface ledger-font-alias-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-alias-definition-face
  `((t :inherit default))
  "Face for aliased account in alias directives"
  :group 'ledger-faces)

(defface ledger-font-assert-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-condition-face
  `((t :inherit default))
  "Default face for check and assert conditions"
  :group 'ledger-faces)

(defface ledger-font-assert-condition-face
  `((t :inherit ledger-font-condition-face))
  "Face for assert conditions"
  :group 'ledger-faces)

(defface ledger-font-bucket-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-bucket-account-face
  `((t :inherit default))
  "Face for bucket directive argument"
  :group 'ledger-faces)

(defface ledger-font-C-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for C directive"
  :group 'ledger-faces)

(defface ledger-font-C-amount-face
  `((t :inherit default))
  "Face for amounts in C directives"
  :group 'ledger-faces)

(defface ledger-font-capture-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-capture-account-face
  `((t :inherit default))
  "Face for account name in capture directives"
  :group 'ledger-faces)

(defface ledger-font-capture-regex-face
  `((t :inherit default))
  "Face for match regex in capture directives"
  :group 'ledger-faces)

(defface ledger-font-check-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-check-condition-face
  `((t :inherit ledger-font-condition-face))
  "Face for check conditions"
  :group 'ledger-faces)

(defface ledger-font-commodity-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-commodity-name-face
  `((t :inherit font-lock-constant-face))
  "Face for commodity name in commodity directives"
  :group 'ledger-faces)

(defface ledger-font-format-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for format subdirective"
  :group 'ledger-faces)

(defface ledger-font-commodity-format-face
  `((t :inherit default))
  "Face for format subdirective argument"
  :group 'ledger-faces)

(defface ledger-font-D-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for D directive"
  :group 'ledger-faces)

(defface ledger-font-define-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-define-name-face
  `((t :inherit font-lock-variable-name-face))
  "Face for variable name in define directive"
  :group 'ledger-faces)

(defface ledger-font-define-body-face
  `((t :inherit default))
  "Face for body in define directive"
  :group 'ledger-faces)

(defface ledger-font-end-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-expr-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-expr-expression-face
  `((t :inherit default))
  "Face for expr and eval expressions"
  :group 'ledger-faces)

(defface ledger-font-fixed-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-fixed-commodity-face
  `((t :inherit font-lock-constant-face))
  "Face for commodity name in fixed directive"
  :group 'ledger-faces)

(defface ledger-font-fixed-price-face
  `((t :inherit default))
  "Face for price in fixed directive"
  :group 'ledger-faces)

(defface ledger-font-include-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-include-filename-face
  `((t :inherit font-lock-string-face))
  "Face for file name in include directives"
  :group 'ledger-faces)

(defface ledger-font-N-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for N directive"
  :group 'ledger-faces)

(defface ledger-font-N-symbol-face
  `((t :inherit default))
  "Face for symbol in N directives")

(defface ledger-font-payee-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-payee-name-face
  `((t :inherit font-lock-function-name-face))
  "Face for payee name in payee directive"
  :group 'ledger-faces)

(defface ledger-font-payee-regex-face
  `((t :inherit font-lock-string-face))
  "Face for payee subdirective regex in account directive"
  :group 'ledger-faces)

(defface ledger-font-uuid-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for uuid subdirectives"
  :group 'ledger-faces)

(defface ledger-font-uuid-face
  `((t :inherit default))
  "Face for uuid in uuid subdirectives"
  :group 'ledger-faces)

(defface ledger-font-tag-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-tag-name-face
  `((t :inherit font-lock-type-face))
  "Face for tag name in tag directive"
  :group 'ledger-faces)

(defface ledger-font-timeclock-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for timeclock I,i,O,o,b,h directives"
  :group 'ledger-faces)

(defface ledger-font-year-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-year-face
  `((t :inherit default))
  "Font for year in year directives"
  :group 'ledger-faces)

(defface ledger-font-posting-account-face
  `((t :inherit ledger-font-default-directive-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-cleared-face
  `((t :inherit ledger-font-posting-account-cleared-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-pending-face
  `((t :inherit ledger-font-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-pending-face
  `((t :inherit ledger-font-posting-account-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-face
  `((t :inherit font-lock-constant-face ))
  "Face for Ledger amounts"
  :group 'ledger-faces)

(defface ledger-font-posting-date-face
  `((t :inherit font-lock-keyword-face))
  "Face for Ledger dates"
  :group 'ledger-faces)

(defface ledger-occur-narrowed-face
  `((t :inherit font-lock-comment-face :invisible t))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face
  `((t :inherit highlight))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defface ledger-font-comment-face
  `((t :inherit font-lock-comment-face))
  "Face for Ledger comments"
  :group 'ledger-faces)

(defface ledger-font-reconciler-uncleared-face
  `((t :inherit ledger-font-payee-uncleared-face))
  "Default face for uncleared transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Default face for cleared (*) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-report-clickable-face
  `((t))
  "Face applied to clickable entries in the report window"
  :group 'ledger-faces)

(defface ledger-font-code-face
  `((t :inherit default))
  "Face for Ledger codes"
  :group 'ledger-faces)

(defun ledger-font-face-by-state (num faces)
  "Choose one of three faces depending on transaction state.
NUM specifies a match group containing the state.
FACES has the form (CLEARED PENDING OTHER).
Return CLEARED if that group specifies a cleared transaction,
PENDING if pending, and OTHER if none of the above."
  (let ((state (save-match-data (ledger-state-from-string (match-string num)))))
    (cond ((eq state 'cleared) (nth 0 faces))
          ((eq state 'pending) (nth 1 faces))
          (t (nth 2 faces)))))

(defun ledger-font-face-by-timeclock-state (num faces)
  "Choose one of two faces depending on a timeclock directive character.
NUM specifies a match group containing the character.
FACES has the form (CLEARED UNCLEARED).
Return CLEARED if the character specifies a cleared transaction,
UNCLEARED otherwise."
  (if (member (match-string num) '("I" "O"))
      (nth 0 faces)
    (nth 1 faces)))

(defun ledger-font-subdirectives (subdirectives)
  "Construct anchored highlighters for subdirectives.

Each element of SUBDIRECTIVES should have the form (MATCHER
SUBEXP-HIGHLIGHTERS…).  The result will be a list of elements of
the form (MATCHER PRE-FORM POST-FORM SUBEXP-HIGHLIGHTERS) with
PRE-FORM and POST-FORM set to appropriate values.

See `font-lock-keywords' for the full description."

  (mapcar (lambda (item)
            `(,(car item)
              (save-excursion
                (save-match-data
                  (ledger-navigate-end-of-xact))
                (point))
              (goto-char (match-end 0))
              ,@(cdr item)))
          subdirectives))

(defvar ledger-font-lock-keywords
  `(("^[;#%|*].*$" . 'ledger-font-comment-face)
    ("^\\(account\\)\\(?:[[:blank:]]\\(.*\\)\\)?$"
     (1 'ledger-font-account-directive-face)
     (2 'ledger-font-account-name-face nil :lax)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(note\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-note-directive-face)
           (2 'ledger-font-note-text-face nil :lax))
          ("^[ \t]+\\(alias\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-alias-directive-face)
           (2 'ledger-font-account-name-face nil :lax))
          ("^[ \t]+\\(payee\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-payee-directive-face)
           (2 'ledger-font-payee-regex-face nil :lax))
          ("^[ \t]+\\(check\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-check-directive-face)
           (2 'ledger-font-check-condition-face nil :lax))
          ("^[ \t]+\\(assert\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-assert-directive-face)
           (2 'ledger-font-assert-condition-face nil :lax))
          ("^[ \t]+\\(eval\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-expr-directive-face)
           (2 'ledger-font-expr-expression-face nil :lax))
          ("^[ \t]+\\(default\\)\\>.*"
           (1 'ledger-font-default-directive-face)))))
    ("^\\(alias\\)\\(?:[[:blank:]]+\\([^=\n]*\\)\\(?:=\\(.*\\)\\)?\\)?$"
     (1 'ledger-font-alias-directive-face)
     (2 'ledger-font-account-name-face nil :lax)
     (3 'ledger-font-alias-definition-face nil :lax))
    (,(concat "^\\(apply\\)\\(?:[[:blank:]]+"
              "\\(?:\\(account\\)\\(?:[[:blank:]]+\\(.*\\)\\)?"
              "\\|\\(tag\\)\\(?:[[:blank:]]+\\(.*\\)\\)?\\)\\)?$")
     (1 'ledger-font-apply-directive-face)
     (2 'ledger-font-apply-directive-face nil :lax)
     (3 'ledger-font-apply-account-face nil :lax)
     (4 'ledger-font-apply-directive-face nil :lax)
     (5 'ledger-font-apply-tag-face nil :lax))
    ("^\\(assert\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-assert-directive-face)
     (2 'ledger-font-assert-condition-face nil :lax))
    ("^\\(bucket\\|A\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-bucket-directive-face)
     (2 'ledger-font-bucket-account-face nil :lax))
    (,(concat "^\\(C\\)"
              "\\(?:[[:blank:]]+\\([^=\n]*?\\)[[:blank:]]*"
              "\\(?:=[[:blank:]]*\\(.*\\)\\)?\\)?$")
     (1 'ledger-font-C-directive-face)
     (2 'ledger-font-C-amount-face nil :lax)
     (3 'ledger-font-C-amount-face nil :lax))
    (,(concat "^\\(capture\\)"
              "\\(?:[[:blank:]]+\\(.*?\\)"
              "\\(?:\\(?:\t\\|[ \t]\\{2,\\}\\)\\(.*\\)\\)?\\)?$")
     (1 'ledger-font-capture-directive-face)
     (2 'ledger-font-capture-account-face nil :lax)
     (3 'ledger-font-capture-regex-face nil :lax))
    ("^\\(check\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-check-directive-face)
     (2 'ledger-font-check-condition-face nil :lax))
    (,(concat "^\\(?:comment\\|test\\)\\>"
              "[^\0]*?\n"
              "end[[:blank:]]+\\(?:comment\\|test\\)\\>.*\n")
     . 'ledger-font-comment-face)
    ("^\\(commodity\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-commodity-directive-face)
     (2 'ledger-font-commodity-name-face nil :lax)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(note\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-note-directive-face)
           (2 'ledger-font-note-text-face nil :lax))
          ("^[ \t]+\\(format\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-format-directive-face)
           (2 'ledger-font-commodity-format-face nil :lax))
          ("^[ \t]+\\(nomarket\\)\\>.*"
           (1 'ledger-font-N-directive-face))
          ("^[ \t]+\\(default\\)\\>.*"
           (1 'ledger-font-default-directive-face)))))
    ("^\\(D\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-D-directive-face)
     (2 'ledger-font-commodity-format-face nil :lax))
    (,(concat "^\\(define\\|def\\)"
              "\\(?:[[:blank:]]+\\([^=\n]*?\\)[[:blank:]]*"
              "\\(?:=[[:blank:]]*\\(.*\\)\\)?\\)?$")
     (1 'ledger-font-define-directive-face)
     (2 'ledger-font-define-name-face nil :lax)
     (3 'ledger-font-define-body-face nil :lax))
    (,(concat "^\\(end\\)"
              "\\(?:[[:blank:]]+\\(apply\\)"
              "\\(?:[[:blank:]]+\\(account\\|tag\\)\\>.*\\)?\\)?$")
     (1 'ledger-font-end-directive-face)
     (2 'ledger-font-end-directive-face nil :lax)
     (3 'ledger-font-end-directive-face nil :lax))
    ("^\\(endfixed\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-end-directive-face)
     (2 'ledger-font-fixed-commodity-face nil :lax))
    ("^\\(expr\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-expr-directive-face)
     (2 'ledger-font-expr-expression-face nil :lax))
    ("^\\(fixed\\)\\(?:[[:blank:]]+\\([^[:blank:]\n]+\\)\\(?:[[:blank:]]+\\(.*\\)\\)?\\)?$"
     (1 'ledger-font-fixed-directive-face)
     (2 'ledger-font-fixed-commodity-face nil :lax)
     (3 'ledger-font-fixed-price-face nil :lax))
    ("^\\(include\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-include-directive-face)
     (2 'ledger-font-include-filename-face nil :lax))
    ("^\\(N\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-N-directive-face)
     (2 'ledger-font-N-symbol-face nil :lax))
    ("^\\(payee\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-payee-directive-face)
     (2 'ledger-font-payee-name-face nil :lax)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(alias\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-alias-directive-face)
           (2 'ledger-font-payee-regex-face nil :lax))
          ("^[ \t]+\\(uuid\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-uuid-directive-face)
           (2 'ledger-font-uuid-face nil :lax)))))
    (,(concat "^\\(P\\)"
              "\\(?:[[:blank:]]+\\([^[:blank:]\n]+"
              "\\(?:[[:blank:]]+[[:digit:]][^[:blank:]\n]*\\)?\\)"
              "\\(?:[[:blank:]]+\\(\".*?\"\\|[^[:blank:]\n]+\\)"
              "\\(?:[[:blank:]]+\\(.*\\)\\)?\\)?\\)?$")
     (1 'ledger-font-price-directive-face)
     (2 'ledger-font-price-date-face nil :lax)
     (3 'ledger-font-price-symbol-face nil :lax)
     (4 'ledger-font-price-face nil :lax))
    ("^\\(tag\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-tag-directive-face)
     (2 'ledger-font-tag-name-face nil :lax)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(check\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-check-directive-face)
           (2 'ledger-font-check-condition-face nil :lax))
          ("^[ \t]+\\(assert\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
           (1 'ledger-font-assert-directive-face)
           (2 'ledger-font-assert-condition-face nil :lax)))))
    (,(concat "^\\([IiOo]\\)"
              "\\(?:[[:blank:]]+\\([^[:blank:]\n]+"
              "\\(?:[[:blank:]]+[^[:blank:]\n]+\\)?\\)"
              "\\(?:[[:blank:]]+\\(.*?\\)"
              "\\(?:\\(?:\t\\|[ \t]\\{2,\\}\\)\\(.*?\\)"
              "\\(?:\\(?:\t\\|[ \t]\\{2,\\}\\)\\(;.*\\)\\)?\\)?\\)?\\)?$")
     (1 'ledger-font-timeclock-directive-face)
     (2 'ledger-font-posting-date-face nil :lax)
     (3 (ledger-font-face-by-timeclock-state 1 '(ledger-font-posting-account-cleared-face
                                                 ledger-font-posting-account-face)) nil :lax)
     (4 (ledger-font-face-by-timeclock-state 1 '(ledger-font-payee-cleared-face
                                                 ledger-font-payee-uncleared-face)) nil :lax)
     (5 'ledger-font-comment-face nil :lax))
    ("^\\([bh]\\)\\>.*$" (1 'ledger-font-timeclock-directive-face))
    ("^\\(year\\|Y\\)\\(?:[[:blank:]]+\\(.*\\)\\)?$"
     (1 'ledger-font-year-directive-face)
     (2 'ledger-font-year-face nil :lax))

    (,(lambda (limit)
        (when ledger-fontify-xact-state-overrides
          (re-search-forward
           (concat "^\\(?:\\([=~]\\)[ \t].*\\|" ; auto/periodic, subexpr 1
                   "[[:digit:]][^ \t\n]*"       ; date
                   "[ \t]+\\([*!]\\)"           ; mark, subexp 2
                   ".*\\)"                      ; rest of header
                   "\\(?:\n[ \t]+.*\\)*"        ; postings
                   )
           limit t)))
     (0 (cond ((equal "=" (match-string 1)) 'ledger-font-auto-xact-face)
              ((equal "~" (match-string 1)) 'ledger-font-periodic-xact-face)
              (t (ledger-font-face-by-state 2 '(ledger-font-xact-cleared-face
                                                ledger-font-xact-pending-face))))))
    (,(concat "^\\(?:\\(\\([=~]\\).*\\)\\|"       ; auto/periodic, subexp 1, 2
              "\\([[:digit:]][^ \t\n]*\\)"        ; date, subexp 3
              ledger-xact-after-date-regex "\\)") ; mark 4, code 5, desc 6, comment 7
     (1 (cond ((equal "=" (match-string 2)) 'ledger-font-auto-xact-face)
              ((equal "~" (match-string 2)) 'ledger-font-periodic-xact-face)
              (t 'ledger-font-default-directive-face))
        nil :lax)
     (3 'ledger-font-posting-date-face nil :lax)
     (5 'ledger-font-code-face nil :lax)
     (6 (ledger-font-face-by-state 4 '(ledger-font-payee-cleared-face
                                       ledger-font-payee-pending-face
                                       ledger-font-payee-uncleared-face))
        nil :lax)
     (7 'ledger-font-comment-face nil :lax)
     ,@(ledger-font-subdirectives
        `(("^[ \t]+\\(;.*\\)"
           (1 'ledger-font-comment-face))
          (,ledger-posting-regex ; state and account 1, state 2, account 3, amount 4, comment 5
           (1 (ledger-font-face-by-state 2 '(ledger-font-posting-account-cleared-face
                                             ledger-font-posting-account-pending-face
                                             ledger-font-posting-account-face))
              nil :lax)
           (4 (ledger-font-face-by-state 2 '(ledger-font-posting-amount-cleared-face
                                             ledger-font-posting-amount-pending-face
                                             ledger-font-posting-amount-face))
              nil :lax)
           (5 'ledger-font-comment-face nil :lax))))))
  "Expressions to highlight in Ledger mode.")



(provide 'ledger-fonts)

;;; ledger-fonts.el ends here
