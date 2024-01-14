;;; ledger-mode.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; Package-Requires: ((emacs "25.1"))

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
;; This Emacs library provides a major mode for editing files in the format used
;; by the `ledger' command-line accounting system.

;; It also provides automated support for some `ledger' workflows, such as
;; reconciling transactions, or running certain reports.

;;; Code:

(require 'ledger-regex)
(require 'org)
(require 'ledger-commodities)
(require 'ledger-complete)
(require 'ledger-context)
(require 'ledger-exec)
(require 'ledger-fonts)
(require 'ledger-fontify)
(require 'ledger-init)
(require 'ledger-navigate)
(require 'ledger-occur)
(require 'ledger-post)
(require 'ledger-reconcile)
(require 'ledger-report)
(require 'ledger-sort)
(require 'ledger-state)
(require 'ledger-test)
(require 'ledger-texi)
(require 'ledger-xact)
(require 'ledger-schedule)
(require 'ledger-check)

(declare-function custom-group-members "cus-edit" (symbol groups-only))

;;; Code:

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defconst ledger-version "3.0"
  "The version of ledger.el currently loaded.")

(defconst ledger-mode-version "4.0.0")

(defun ledger-mode-dump-variable (var)
  "Format VAR for dump to buffer."
  (if var
      (insert (format "         %s: %S\n" (symbol-name var) (eval var)))))

(defun ledger-mode-dump-group (group)
  "Dump GROUP customizations to current buffer."
  (require 'cus-edit)
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
             (insert (format "Group %s:\n" (symbol-name (car member))))
             (ledger-mode-dump-group (car member)))
            ((eq (cadr member) 'custom-variable)
             (ledger-mode-dump-variable (car member)))))))

(defun ledger-mode-dump-configuration ()
  "Dump all customizations."
  (interactive)
  (find-file "ledger-mode-dump")
  (ledger-mode-dump-group 'ledger))

(defun ledger-read-account-with-prompt (prompt)
  "Read an account from the minibuffer with PROMPT."
  (let* ((context (ledger-context-at-point))
         (account (ledger-context-field-value context 'account)))
    (ledger-completing-read-with-default prompt
                                         (when account
                                           (regexp-quote account))
                                         (ledger-accounts-list))))

(defun ledger-read-date (prompt)
  "Return user-supplied date after `PROMPT', defaults to today.
This uses `org-read-date', which see."
  (ledger-format-date (let ((org-read-date-prefer-future nil))
                        (org-read-date nil t nil prompt))))

(defun ledger-get-minibuffer-prompt (prompt default)
  "Return a minibuffer prompt string composing PROMPT and DEFAULT."
  (concat prompt
          (if default
              (concat " (" default "): ")
            ": ")))

(defun ledger-completing-read-with-default (prompt default collection)
  "Return a user-supplied string after PROMPT.
Use the given DEFAULT, while providing completions from COLLECTION."
  (completing-read (ledger-get-minibuffer-prompt prompt default)
                   collection nil nil nil 'ledger-minibuffer-history default))

(defun ledger-read-string-with-default (prompt default)
  "Return user supplied string after PROMPT, or DEFAULT."
  (read-string (ledger-get-minibuffer-prompt prompt default)
               nil 'ledger-minibuffer-history default))

(defun ledger-display-balance-at-point (&optional arg)
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled.

With ARG (\\[universal-argument]) ask for the target commodity and convert
the balance into that."
  (interactive "P")
  (let* ((account (ledger-read-account-with-prompt "Account balance to show"))
         (target-commodity (when arg (ledger-read-commodity-with-prompt "Target commodity: ")))
         (buffer (find-file-noselect (ledger-master-file)))
         (balance (with-temp-buffer
                    (apply 'ledger-exec-ledger buffer (current-buffer) "cleared" account
                           (when target-commodity (list "-X" target-commodity)))
                    (if (> (buffer-size) 0)
                        (buffer-substring-no-properties (point-min) (1- (point-max)))
                      (concat account " is empty.")))))
    (when balance
      (message balance))))

(defun ledger-display-ledger-stats ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((buffer (find-file-noselect (ledger-master-file)))
         (balance (with-temp-buffer
                    (ledger-exec-ledger buffer (current-buffer) "stats")
                    (buffer-substring-no-properties (point-min) (1- (point-max))))))
    (when balance
      (message balance))))

(defvar ledger-mode-abbrev-table)

(defvar ledger-date-string-today (ledger-format-date))

(defun ledger-remove-effective-date ()
  "Remove the effective date from a transaction or posting."
  (interactive)
  (let ((context (car (ledger-context-at-point))))
    (save-excursion
      (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (beginning-of-line)
        (cond ((eq 'xact context)
               (re-search-forward ledger-iso-date-regexp)
               (when (= (char-after) ?=)
                 (let ((eq-pos (point)))
                   (delete-region
                    eq-pos
                    (re-search-forward ledger-iso-date-regexp)))))
              ((eq 'acct-transaction context)
               ;; Match "; [=date]" & delete string
               (when (re-search-forward
                      (concat ledger-comment-regex
                              "\\[=" ledger-iso-date-regexp "\\]")
                      nil 'noerr)
                 (replace-match ""))))))))

(defun ledger-insert-effective-date (&optional date)
  "Insert effective date `DATE' to the transaction or posting.

If `DATE' is nil, prompt the user a date.

Replace the current effective date if there's one in the same
line.

With a prefix argument, remove the effective date."
  (interactive)
  (if (and (listp current-prefix-arg)
           (= 4 (prefix-numeric-value current-prefix-arg)))
      (ledger-remove-effective-date)
    (let* ((context (car (ledger-context-at-point)))
           (date-string (or date (ledger-read-date "Effective date: "))))
      (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (cond
         ((eq 'xact context)
          (beginning-of-line)
          (re-search-forward ledger-iso-date-regexp)
          (when (= (char-after) ?=)
            (ledger-remove-effective-date))
          (insert "=" date-string))
         ((eq 'acct-transaction context)
          (end-of-line)
          (ledger-remove-effective-date)
          (insert "  ; [=" date-string "]")))))))

(defun ledger-mode-remove-extra-lines ()
  "Get rid of multiple empty lines."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\\(\n\\)+" nil t)
    (replace-match "\n\n")))

(defun ledger-mode-clean-buffer ()
  "Indent, remove multiple line feeds and sort the buffer."
  (interactive)
  (let ((start (point-min-marker))
        (end (point-max-marker))
        (distance-in-xact (- (point) (ledger-navigate-beginning-of-xact))))
    (beginning-of-line)
    (let ((target (buffer-substring (point) (progn
                                              (end-of-line)
                                              (point)))))
      (goto-char start)
      (untabify start end)
      (ledger-sort-buffer)
      (ledger-post-align-postings start end)
      (ledger-mode-remove-extra-lines)
      (goto-char start)
      (search-forward target)
      (beginning-of-line)
      (forward-char distance-in-xact))))

(defvar ledger-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table in use in `ledger-mode' buffers.")

(defvar ledger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'ledger-add-transaction)
    (define-key map (kbd "C-c C-b") #'ledger-post-edit-amount)
    (define-key map (kbd "C-c C-c") #'ledger-toggle-current)
    (define-key map (kbd "C-c C-d") #'ledger-delete-current-transaction)
    (define-key map (kbd "C-c C-e") #'ledger-toggle-current-transaction)
    (define-key map (kbd "C-c C-f") #'ledger-occur)
    (define-key map (kbd "C-c C-k") #'ledger-copy-transaction-at-point)
    (define-key map (kbd "C-c C-r") #'ledger-reconcile)
    (define-key map (kbd "C-c C-s") #'ledger-sort-region)
    (define-key map (kbd "C-c C-t") #'ledger-insert-effective-date)
    (define-key map (kbd "C-c C-u") #'ledger-schedule-upcoming)
    (define-key map (kbd "C-c C-p") #'ledger-display-balance-at-point)
    (define-key map (kbd "C-c C-l") #'ledger-display-ledger-stats)
    (define-key map (kbd "C-c C-q") #'ledger-post-align-xact)

    (define-key map (kbd "C-TAB") #'ledger-post-align-xact)
    (define-key map (kbd "C-c TAB") #'ledger-fully-complete-xact)
    (define-key map (kbd "C-c C-i") #'ledger-fully-complete-xact)

    (define-key map (kbd "C-c C-o C-a") #'ledger-report-redo)
    (define-key map (kbd "C-c C-o C-e") #'ledger-report-edit-report)
    (define-key map (kbd "C-c C-o C-g") #'ledger-report-goto)
    (define-key map (kbd "C-c C-o C-k") #'ledger-report-quit)
    (define-key map (kbd "C-c C-o C-r") #'ledger-report)
    (define-key map (kbd "C-c C-o C-s") #'ledger-report-save)

    (define-key map (kbd "M-p") #'ledger-navigate-prev-xact-or-directive)
    (define-key map (kbd "M-n") #'ledger-navigate-next-xact-or-directive)
    (define-key map (kbd "M-q") #'ledger-post-align-dwim)

    ;; Reset the `text-mode' override of this standard binding
    (define-key map (kbd "C-M-i") 'completion-at-point)
    map)
  "Keymap for `ledger-mode'.")

(easy-menu-define ledger-mode-menu ledger-mode-map
  "Ledger menu"
  '("Ledger"
    ["Narrow to REGEX" ledger-occur]
    ["Show all transactions" ledger-occur-mode ledger-occur-mode]
    ["Ledger Statistics" ledger-display-ledger-stats ledger-works]
    "---"
    ["Show upcoming transactions" ledger-schedule-upcoming]
    ["Add Transaction (ledger xact)" ledger-add-transaction ledger-works]
    ["Complete Transaction" ledger-fully-complete-xact]
    ["Delete Transaction" ledger-delete-current-transaction]
    "---"
    ["Calc on Amount" ledger-post-edit-amount]
    "---"
    ["Check Balance" ledger-display-balance-at-point ledger-works]
    ["Reconcile Account" ledger-reconcile ledger-works]
    "---"
    ["Toggle Current Transaction" ledger-toggle-current-transaction]
    ["Toggle Current Posting" ledger-toggle-current]
    ["Copy Trans at Point" ledger-copy-transaction-at-point]
    "---"
    ["Clean-up Buffer" ledger-mode-clean-buffer]
    ["Check Buffer" ledger-check-buffer ledger-works]
    ["Align Region" ledger-post-align-postings mark-active]
    ["Align Xact" ledger-post-align-xact]
    ["Sort Region" ledger-sort-region mark-active]
    ["Sort Buffer" ledger-sort-buffer]
    ["Mark Sort Beginning" ledger-sort-insert-start-mark]
    ["Mark Sort End" ledger-sort-insert-end-mark]
    ["Set effective date" ledger-insert-effective-date]
    "---"
    ["Customize Ledger Mode" (lambda () (interactive) (customize-group 'ledger))]
    "---"
    ["Run Report" ledger-report ledger-works]
    ["Goto Report" ledger-report-goto ledger-works]
    ["Re-run Report" ledger-report-redo ledger-works]
    ["Save Report" ledger-report-save ledger-works]
    ["Edit Report" ledger-report-edit-report ledger-works]
    ["Quit Report" ledger-report-quit ledger-works]))

;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (ledger-check-version)
  (setq font-lock-defaults
        '(ledger-font-lock-keywords t nil nil nil))
  (add-hook 'font-lock-extend-region-functions 'ledger-fontify-extend-region)
  (add-hook 'completion-at-point-functions #'ledger-complete-at-point nil t)
  (add-hook 'after-save-hook 'ledger-report-redo nil t)

  (add-hook 'post-command-hook 'ledger-highlight-xact-under-point nil t)

  (ledger-init-load-init-file)
  (setq-local comment-start ";")
  (setq-local indent-line-function #'ledger-indent-line)
  (setq-local indent-region-function 'ledger-post-align-postings)
  (setq-local beginning-of-defun-function #'ledger-navigate-beginning-of-xact)
  (setq-local end-of-defun-function #'ledger-navigate-end-of-xact))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(provide 'ledger-mode)

;;; ledger-mode.el ends here
