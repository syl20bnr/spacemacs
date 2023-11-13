;;; flycheck-ledger.el --- Flycheck integration for ledger files  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Homepage: https://github.com/purcell/flycheck-ledger
;; Version: DEV
;; Keywords: convenience languages tools
;; Package-Requires: ((emacs "24.1") (flycheck "0.15"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This flychecker uses the output of "ledger balance" on the current file to
;; find errors such as unbalanced transactions and syntax errors.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-ledger))

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-ledger-zero-accounts nil ledger-zero
  "Whether to check account names, tags, and payees from cleared transactions."
  :type '(repeat string)
  :safe #'flycheck-string-list-p)

(flycheck-define-checker ledger
  "A checker for ledger files, showing unmatched balances and failed checks."
  :command ("ledger"
            (option-flag "--explicit" flycheck-ledger-explicit)
            (option-flag "--pedantic" flycheck-ledger-pedantic)
            (eval (when (eq flycheck-ledger-pedantic 'check-payees) "--check-payees"))
            "-f" source-inplace
            "balance"
            ;; to find non-zero zero accounts:
            "--flat" "--no-total"
            "--balance-format" "%(scrub(display_total))\t\t%(account())\n"
            (eval flycheck-ledger-zero-accounts))
  :error-patterns
  ((error line-start "While parsing file \"" (file-name) "\", line " line ":" (zero-or-more whitespace) "\n"
          (zero-or-more line-start (or "While " "> ") (one-or-more not-newline) "\n" )
          (message (minimal-match (zero-or-more line-start (zero-or-more not-newline) "\n"))
                   "Error: " (one-or-more not-newline) "\n")))
  :error-parser
  (lambda (output checker buffer)
    (let ((pattern-errors (flycheck-parse-with-patterns output checker buffer)))
      (or pattern-errors
          (when (> (length flycheck-ledger-zero-accounts) 0)
            (flycheck-ledger--zero-error-parser output checker buffer)))))
  :verify
  (lambda (checker)
    (let ((has-accounts (> (length flycheck-ledger-zero-accounts) 0)))
      (list
       (flycheck-verification-result-new
        :label "accounts"
        :message (if has-accounts (format "%s" flycheck-ledger-zero-accounts) "none")
        :face 'success))))
  :modes ledger-mode)

(flycheck-def-option-var flycheck-ledger-pedantic () ledger
  "Whether to be pedantic in ledger.

When equal to `check-payees', be pedantic on account name and payees,
When non-nil, be pedantic on account name,
otherwise don't be pedantic."
  :type '(radio (const :tag "Run Ledger normally" nil)
                (const :tag "Check account names (--pedantic)" t)
                (const :tag "Also check payees (--check-payees)" check-payees)))

(flycheck-def-option-var flycheck-ledger-explicit nil ledger
  "Whether to check account names, tags, and payees from cleared transactions."
  :type 'boolean)

(defun flycheck-ledger--zero-last-position-of-account (account buffer)
  "Return (LINE . COL) of last occurrence of ACCOUNT in BUFFER.

Return nil if ACCOUNT can't be found in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (save-excursion
        (goto-char (point-max))
        (when (search-backward account nil t)
          (cons (line-number-at-pos (point))
                (1+ (- (point) (line-beginning-position)))))))))

(defun flycheck-ledger--zero-error-parser (output checker buffer)
  "Return errors found in OUTPUT.

CHECKER is a `flycheck-ledger-zero' checker.

BUFFER is the buffer being checked by flycheck.

Return a list of parsed errors and warnings (as `flycheck-error'
objects)."
  (let ((errors (list))
        (buffer (current-buffer)))
    (save-match-data
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward "^\\(.*\\)\\>\t\t\\<\\(.*\\)$" nil t)
          (let* ((amount (string-trim (match-string-no-properties 1)))
                 (account (string-trim (match-string-no-properties 2)))
                 (message (format "Account %s should have zero value but has %s"
                                  account amount))
                 (position (flycheck-ledger--zero-last-position-of-account account buffer))
                 (line (or (car position) 1))
                 (column (or (cdr position) 0)))
            (push
             (flycheck-error-new-at
              line column 'error message
              :checker checker
              :filename (buffer-file-name buffer) :buffer buffer)
             errors)))))
    errors))

(flycheck-def-option-var flycheck-ledger-zero-accounts nil ledger-zero
  "Whether to check account names, tags, and payees from cleared transactions."
  :type '(repeat string))

(add-to-list 'flycheck-checkers 'ledger)

(provide 'flycheck-ledger)
;;; flycheck-ledger.el ends here
