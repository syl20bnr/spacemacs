;;; ledger-state.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Utilities for dealing with transaction and posting status.

;;; Code:
(require 'ledger-navigate)
(require 'ledger-context)

(defcustom ledger-clear-whole-transactions nil
  "If non-nil, clear whole transactions, not individual postings."
  :type 'boolean
  :group 'ledger)

(defun ledger-transaction-state ()
  "Return the state of the transaction at point."
  (save-excursion
    (when (or (looking-at "^[0-9]")
              (re-search-backward "^[0-9]" nil t))
      (skip-chars-forward "0-9./=\\-")
      (skip-syntax-forward " ")
      (cond ((looking-at "!\\s-*") 'pending)
            ((looking-at "\\*\\s-*") 'cleared)
            (t nil)))))

(defun ledger-posting-state ()
  "Return the state of the posting."
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-syntax-forward " ")
    (cond ((looking-at "!\\s-*") 'pending)
          ((looking-at "\\*\\s-*") 'cleared)
          (t (ledger-transaction-state)))))

(defun ledger-char-from-state (state)
  "Return the char representation of STATE."
  (if state
      (if (eq state 'pending)
          "!"
        "*")
    ""))

(defun ledger-state-from-char (state-char)
  "Get state from STATE-CHAR."
  (cond ((eql state-char ?\!) 'pending)
        ((eql state-char ?\*) 'cleared)
        ((eql state-char ?\;) 'comment)
        (t nil)))


(defun ledger-state-from-string (state-string)
  "Get state from STATE-STRING."
  (when state-string
    (cond
     ((string-match "!" state-string) 'pending)
     ((string-match "\\*" state-string) 'cleared)
     ((string-match ";" state-string) 'comment)
     (t nil))))

(defun ledger-toggle-current-posting (&optional style)
  "Toggle the cleared status of the transaction under point.
Optional argument STYLE may be `pending' or `cleared', depending
on which type of status the caller wishes to indicate (default is
`cleared').  Returns the new status as `pending' `cleared' or nil.
This function is rather complicated because it must preserve both
the overall formatting of the ledger xact, as well as ensuring
that the most minimal display format is used.  This could be
achieved more certainly by passing the xact to ledger for
formatting, but doing so causes inline math expressions to be
dropped."
  (interactive)
  (let ((bounds (ledger-navigate-find-xact-extents (point)))
        new-status cur-status)
    ;; Uncompact the xact, to make it easier to toggle the
    ;; transaction
    (save-excursion  ;; this excursion checks state of entire
      ;; transaction and unclears if marked
      (goto-char (car bounds))  ;; beginning of xact
      (skip-chars-forward "0-9./=\\-") ;; skip the date
      (skip-chars-forward " \t") ;; skip the white space after the date
      (setq cur-status (and (member (char-after) '(?\* ?\!))
                            (ledger-state-from-char (char-after))))
      ;;if cur-status if !, or * then delete the marker
      (when cur-status
        (let ((here (point)))
          (skip-chars-forward "*! ")
          (let ((width (- (point) here)))
            (when (> width 0)
              (delete-region here (point))
              (if (search-forward "  " (line-end-position) t)
                  (insert (make-string width ? ))))))
        (forward-line)
        ;; Shift the cleared/pending status to the postings
        (while (looking-at "[ \t]")
          (skip-chars-forward " \t")
          (when (not (eq (ledger-state-from-char (char-after)) 'comment))
            (insert (ledger-char-from-state cur-status) " ")
            (if (and (search-forward "  " (line-end-position) t)
                     (looking-at "  "))
                (delete-char 2)))
          (forward-line))
        (setq new-status nil)))

    ;;this excursion toggles the posting status
    (save-excursion
      (setq inhibit-modification-hooks t)

      (goto-char (line-beginning-position))
      (when (looking-at "[ \t]")
        (skip-chars-forward " \t")
        (let ((here (point))
              (cur-status (ledger-state-from-char (char-after))))
          (skip-chars-forward "*! ")
          (let ((width (- (point) here)))
            (when (> width 0)
              (delete-region here (point))
              (save-excursion
                (if (search-forward "  " (line-end-position) t)
                    (insert (make-string width ? ))))))
          (let (inserted)
            (if cur-status
                (if (and style (eq style 'cleared))
                    (progn
                      (insert  "* ")
                      (setq inserted 'cleared)))
              (if (and style (eq style 'pending))
                  (progn
                    (insert  "! ")
                    (setq inserted 'pending))
                (progn
                  (insert  "* ")
                  (setq inserted 'cleared))))
            (if (and inserted
                     (re-search-forward "\\(\t\\| [ \t]\\)"
                                        (line-end-position) t))
                (cond
                 ((looking-at "\t")
                  (delete-char 1))
                 ((looking-at " [ \t]")
                  (delete-char 2))
                 ((looking-at " ")
                  (delete-char 1))))
            (setq new-status inserted))))
      (setq inhibit-modification-hooks nil))

    ;; This excursion cleans up the xact so that it displays
    ;; minimally.  This means that if all posts are cleared, remove
    ;; the marks and clear the entire transaction.
    (save-excursion
      (goto-char (car bounds))
      (forward-line)
      (let ((first t)
            (state nil)
            (hetero nil))
        (while (and (not hetero) (looking-at "[ \t]"))
          (skip-chars-forward " \t")
          (let ((cur-status (ledger-state-from-char (char-after))))
            (if (not (eq cur-status 'comment))
                (if first
                    (setq state cur-status
                          first nil)
                  (if (not (eq state cur-status))
                      (setq hetero t)))))
          (forward-line))
        (when (and (not hetero) (not (eq state nil)))
          (goto-char (car bounds))
          (forward-line)
          (while (looking-at "[ \t]")
            (skip-chars-forward " \t")
            (let ((here (point)))
              (skip-chars-forward "*! ")
              (let ((width (- (point) here)))
                (when (> width 0)
                  (delete-region here (point))
                  (if (re-search-forward "\\(\t\\| [ \t]\\)"
                                         (line-end-position) t)
                      (insert (make-string width ? ))))))
            (forward-line))
          (goto-char (car bounds))
          (skip-chars-forward "0-9./=\\-") ;; Skip the date
          (skip-chars-forward " \t") ;; Skip the white space
          (insert (ledger-char-from-state state) " ")
          (setq new-status state)
          (if (re-search-forward "\\(\t\\| [ \t]\\)"
                                 (line-end-position) t)
              (cond
               ((looking-at "\t")
                (delete-char 1))
               ((looking-at " [ \t]")
                (delete-char 2))
               ((looking-at " ")
                (delete-char 1)))))))
    new-status))

(defun ledger-toggle-current (&optional style)
  "Toggle the current thing at point with optional STYLE."
  (interactive)
  (if (or ledger-clear-whole-transactions
          (eq 'transaction (ledger-thing-at-point)))
      (progn
        (save-excursion
          (forward-line)
          (goto-char (line-beginning-position))
          (while (and (not (eolp))
                      (save-excursion
                        (not (eq 'transaction (ledger-thing-at-point)))))
            (if (looking-at "\\s-+[*!]")
                (ledger-toggle-current-posting style))
            (forward-line)
            (goto-char (line-beginning-position))))
        (ledger-toggle-current-transaction style))
    (ledger-toggle-current-posting style)))

(defun ledger-toggle-current-transaction (&optional style)
  "Toggle the transaction at point using optional STYLE."
  (interactive)
  (save-excursion
    (when (or (looking-at "^[0-9]")
              (re-search-backward "^[0-9]" nil t))
      (skip-chars-forward "0-9./=\\-")
      (delete-horizontal-space)
      (if (or (eq (ledger-state-from-char (char-after)) 'pending)
              (eq (ledger-state-from-char (char-after)) 'cleared))
          (progn
            (delete-char 1)
            (when (and style (eq style 'cleared))
              (insert " *")
              'cleared))
        (if (and style (eq style 'pending))
            (progn
              (insert " ! ")
              'pending)
          (progn
            (insert " * ")
            'cleared))))))

(provide 'ledger-state)

;;; ledger-state.el ends here
