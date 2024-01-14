;;; ledger-sort.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;;

;;; Code:
(require 'ledger-regex)
(require 'ledger-navigate)

(defun ledger-sort-find-start ()
  "Find the beginning of a sort region."
  (when (re-search-forward ";.*Ledger-mode:.*Start sort" nil t)
    (match-end 0)))

(defun ledger-sort-find-end ()
  "Find the end of a sort region."
  (when (re-search-forward ";.*Ledger-mode:.*End sort" nil t)
    (match-end 0)))

(defun ledger-sort-insert-start-mark ()
  "Insert a marker to start a sort region."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (ledger-sort-find-start)
      (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: Start sort\n\n"))

(defun ledger-sort-insert-end-mark ()
  "Insert a marker to end a sort region."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (ledger-sort-find-end)
      (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: End sort\n\n"))

(defun ledger-sort-startkey ()
  "Return the date portion of the current line, for use in sorting."
  (buffer-substring-no-properties (point) (+ 10 (point))))

(defun ledger-sort-region (beg end)
  "Sort the region from BEG to END in chronological order."
  (interactive "r") ;; load beg and end from point and mark
  ;; automagically
  (let* ((new-beg beg)
         (new-end end)
         (bounds (ledger-navigate-find-xact-extents (point)))
         (point-delta (- (point) (car bounds)))
         (target-xact (buffer-substring (car bounds) (cadr bounds)))
         (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
        (goto-char beg)
        ;; make sure beg of region is at the beginning of a line
        (beginning-of-line)
        ;; make sure point is at the beginning of a xact
        (unless (looking-at ledger-payee-any-status-regex)
          (ledger-navigate-next-xact))
        (setq new-beg (point))
        (goto-char end)
        (ledger-navigate-next-xact)
        ;; make sure end of region is at the beginning of next record
        ;; after the region
        (setq new-end (point))
        (narrow-to-region new-beg new-end)
        (goto-char new-beg)

        (let ((inhibit-field-text-motion t))
          (sort-subr
           nil
           'ledger-navigate-next-xact
           'ledger-navigate-end-of-xact
           'ledger-sort-startkey))))

    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))))

(defun ledger-sort-buffer ()
  "Sort the entire buffer."
  (interactive)
  (let (sort-start
        sort-end)
    (save-excursion
      (goto-char (point-min))
      (setq sort-start (ledger-sort-find-start)
            sort-end (ledger-sort-find-end)))
    (ledger-sort-region (or sort-start (point-min))
                        (or sort-end (point-max)))))

(provide 'ledger-sort)

;;; ledger-sort.el ends here
