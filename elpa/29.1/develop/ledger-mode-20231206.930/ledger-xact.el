;;; ledger-xact.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Utilities for running ledger synchronously.

;;; Code:

(require 'eshell)
(require 'ledger-regex)
(require 'ledger-navigate)
(require 'ledger-exec)
(require 'ledger-post)
(declare-function ledger-read-date "ledger-mode" (prompt))
(declare-function ledger-format-date "ledger-init" (&optional date))

;; TODO: This file depends on code in ledger-mode.el, which depends on this.

(defcustom ledger-highlight-xact-under-point t
  "If t highlight xact under point."
  :type 'boolean
  :group 'ledger)

(defcustom ledger-add-transaction-prompt-for-text t
  "When non-nil, use ledger xact to format transaction.
When nil, `ledger-add-transaction' will not prompt twice."
  :type 'boolean
  :package-version '(ledger-mode . "4.0.1")
  :group 'ledger)

(defvar-local ledger-xact-highlight-overlay (list))

(defun ledger-highlight-make-overlay ()
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'font-lock-face 'ledger-font-xact-highlight-face)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun ledger-highlight-xact-under-point ()
  "Move the highlight overlay to the current transaction."
  (when ledger-highlight-xact-under-point
    (unless ledger-xact-highlight-overlay
      (setq ledger-xact-highlight-overlay (ledger-highlight-make-overlay)))
    (let ((exts (ledger-navigate-find-element-extents (point))))
      (let ((b (car exts))
            (e (cadr exts))
            (p (point)))
        (if (and (> (- e b) 1)            ; not an empty line
                 (<= p e) (>= p b)        ; point is within the boundaries
                 (not (region-active-p))) ; no active region
            (move-overlay ledger-xact-highlight-overlay b (+ 1 e))
          (move-overlay ledger-xact-highlight-overlay 1 1))))))

(defun ledger-xact-context ()
  "Return the context of the transaction containing point or nil."
  (let ((i 0))
    (while (eq (ledger-context-line-type (ledger-context-other-line i)) 'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (ledger-context-other-line i)))
      (if (eq (ledger-context-line-type context-info) 'xact)
          context-info
        nil))))

(defun ledger-xact-payee ()
  "Return the payee of the transaction containing point or nil."
  (let ((xact-context (ledger-xact-context)))
    (if xact-context
        (ledger-context-field-value xact-context 'payee)
      nil)))

(defun ledger-xact-date ()
  "Return the date of the transaction containing point or nil."
  (let ((xact-context (ledger-xact-context)))
    (if xact-context
        (ledger-context-field-value xact-context 'date)
      nil)))

(defun ledger-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  ;; TODO: assert listp, or support when both are strings
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun ledger-xact-find-slot (moment)
  "Find the right place in the buffer for a transaction at MOMENT.
MOMENT is an encoded date"
  (let (last-xact-start)
    (catch 'found
      (ledger-xact-iterate-transactions
       (function
        (lambda (start date _mark _desc)
          (setq last-xact-start start)
          (if (ledger-time-less-p moment date)
              (throw 'found t))))))
    (when (and (eobp) last-xact-start)
      (let ((end (cadr (ledger-navigate-find-xact-extents last-xact-start))))
        (goto-char end)
        (insert "\n")
        (forward-line)))))

(defun ledger-xact-iterate-transactions (callback)
  "Iterate through each transaction call CALLBACK for each."
  (goto-char (point-min))
  (let* ((now (current-time))
         (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at ledger-iterate-regexp)
        (let ((found-y-p (match-string 1)))
          (if found-y-p
              (setq current-year (string-to-number found-y-p)) ;; a Y directive was found
            (let ((start (match-beginning 0))
                  (year (match-string (+ ledger-regex-iterate-group-actual-date 1)))
                  (month (string-to-number (match-string (+ ledger-regex-iterate-group-actual-date 2))))
                  (day (string-to-number (match-string (+ ledger-regex-iterate-group-actual-date 3))))
                  (state (match-string ledger-regex-iterate-group-state))
                  (payee (match-string ledger-regex-iterate-group-payee)))
              (if (and year (> (length year) 0))
                  (setq year (string-to-number year)))
              (funcall callback start
                       (encode-time 0 0 0 day month
                                    (or year current-year))
                       state payee)))))
      (forward-line))))

(defcustom ledger-copy-transaction-insert-blank-line-after nil
  "When non-nil, insert a blank line after `ledger-copy-transaction-at-point'."
  :type 'boolean
  :group 'ledger)

(defun ledger-copy-transaction-at-point (date)
  "Ask for a new DATE and copy the transaction under point to that date.
Leave point on the first amount."
  (interactive  (list
                 (ledger-read-date "Copy to date: ")))
  (let* ((extents (ledger-navigate-find-xact-extents (point)))
         (transaction (buffer-substring-no-properties (car extents) (cadr extents)))
         (encoded-date (ledger-parse-iso-date date)))
    (push-mark)
    (ledger-xact-find-slot encoded-date)
    (insert transaction
            (if (and ledger-copy-transaction-insert-blank-line-after (not (eobp)))
                "\n\n"
              "\n"))
    (beginning-of-line -1)
    (ledger-navigate-beginning-of-xact)
    (re-search-forward ledger-iso-date-regexp)
    (replace-match date)
    (ledger-next-amount)
    (if (re-search-forward "[-0-9]")
        (goto-char (match-beginning 0)))))

(defun ledger-delete-current-transaction (pos)
  "Delete the transaction surrounding POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (delete-region (car bounds) (cadr bounds)))
  (delete-blank-lines))

(defvar ledger-add-transaction-last-date nil
  "Last date entered using `ledger-read-transaction'.")

(defun ledger-read-transaction ()
  "Read the text of a transaction, which is at least the current date."
  (let ((date (ledger-read-date "Date: ")))
    (concat date " "
            (when ledger-add-transaction-prompt-for-text
              (read-string (concat "xact " date ": ") nil 'ledger-minibuffer-history)))))

(defun ledger-parse-iso-date (date)
  "Try to parse DATE using `ledger-iso-date-regexp' and return a time value or nil."
  (save-match-data
    (when (string-match ledger-iso-date-regexp date)
      (encode-time 0 0 0 (string-to-number (match-string 4 date))
                   (string-to-number (match-string 3 date))
                   (string-to-number (match-string 2 date))))))

(defun ledger-add-transaction (transaction-text &optional insert-at-point)
  "Use ledger xact TRANSACTION-TEXT to add a transaction to the buffer.
If INSERT-AT-POINT is non-nil insert the transaction there,
otherwise call `ledger-xact-find-slot' to insert it at the
correct chronological place in the buffer.  Interactively, the
date is requested via `ledger-read-date'."
  (interactive (list (ledger-read-transaction)))
  (let* ((args (with-temp-buffer
                 (insert transaction-text)
                 (eshell-parse-arguments (point-min) (point-max))))
         (ledger-buf (current-buffer))
         (separator "\n"))
    (unless insert-at-point
      (let* ((date (car args))
             (parsed-date (ledger-parse-iso-date date)))
        (setq ledger-add-transaction-last-date parsed-date)
        (push-mark)
        ;; TODO: what about when it can't be parsed?
        (ledger-xact-find-slot (or parsed-date date))
        (when (looking-at "\n*\\'")
          (setq separator ""))))
    (if (> (length args) 1)
        (save-excursion
          (insert
           (with-temp-buffer
             (apply #'ledger-exec-ledger ledger-buf (current-buffer) "xact"
                    (mapcar 'eval args))
             (goto-char (point-min))
             (ledger-post-align-postings (point-min) (point-max))
             (buffer-string))
           separator))
      (progn
        (insert (car args) " ")
        (save-excursion (insert "\n" separator))))))

(provide 'ledger-xact)

;;; ledger-xact.el ends here
