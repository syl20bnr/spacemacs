;;; ledger-navigate.el --- Provide navigation services through the ledger buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015 Craig Earls (enderw88 AT gmail DOT com)

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
(require 'ledger-context)

(defun ledger-navigate-next-xact ()
  "Move point to beginning of next xact."
  ;; make sure we actually move to the next xact, even if we are the
  ;; beginning of one now.
  (if (looking-at ledger-payee-any-status-regex)
      (forward-line))
  (if (re-search-forward  ledger-payee-any-status-regex nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun ledger-navigate-start-xact-or-directive-p ()
  "Return t if at the beginning of an empty or all-whitespace line."
  (not (looking-at "[ \t]\\|\\(^$\\)")))

(defun ledger-navigate-next-xact-or-directive ()
  "Move to the beginning of the next xact or directive."
  (interactive)
  (beginning-of-line)
  (if (ledger-navigate-start-xact-or-directive-p) ; if we are the start of an xact, move forward to the next xact
      (progn
        (forward-line)
        (if (not (ledger-navigate-start-xact-or-directive-p)) ; we have moved forward and are not at another xact, recurse forward
            (ledger-navigate-next-xact-or-directive)))
    (while (not (or (eobp)  ; we didn't start off at the beginning of an xact
                    (ledger-navigate-start-xact-or-directive-p)))
      (forward-line))))

(defun ledger-navigate-prev-xact-or-directive ()
  "Move point to beginning of previous xact."
  (interactive)
  (let ((context (car (ledger-context-at-point))))
    (when (equal context 'acct-transaction)
      (ledger-navigate-beginning-of-xact))
    (beginning-of-line)
    (re-search-backward "^[[:graph:]]" nil t)))

(defun ledger-navigate-beginning-of-xact ()
  "Move point to the beginning of the current xact."
  (interactive)
  ;; need to start at the beginning of a line in case we are in the first line of an xact already.
  (beginning-of-line)
  (let ((sreg (concat "^[=~[:digit:]]")))
    (unless (looking-at sreg)
      (re-search-backward sreg nil t)
      (beginning-of-line)))
  (point))

(defun ledger-navigate-end-of-xact ()
  "Move point to end of xact."
  (interactive)
  (ledger-navigate-next-xact-or-directive)
  (re-search-backward ".$")
  (end-of-line)
  (point))

(defun ledger-navigate-to-line (line-number)
  "Rapidly move point to line LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun ledger-navigate-find-xact-extents (pos)
  "Return list containing point for beginning and end of xact containing POS.
Requires empty line separating xacts."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (ledger-navigate-beginning-of-xact)
          (ledger-navigate-end-of-xact))))

(defun ledger-navigate-skip-lines-backwards (re)
  "Move backwards if necessary until the line beginning does not match RE."
  (beginning-of-line)
  (while (and (looking-at-p re)
              (zerop (forward-line -1)))))

(defun ledger-navigate-skip-lines-forwards (re)
  "Move forwards if necessary until the line beginning does not match RE."
  (beginning-of-line)
  (while (and (looking-at-p re)
              (zerop (forward-line 1)))))

(defun ledger-navigate-find-directive-extents (pos)
  "Return the extents of the directive at POS."
  (goto-char pos)
  (let ((begin (progn (ledger-navigate-skip-lines-backwards "[ \t]\\|end[[:blank:]]+\\(?:comment\\|test\\)")
                      (point)))
        (end (progn (forward-line 1)
                    (ledger-navigate-skip-lines-forwards "[ \t]")
                    (1- (point))))
        (comment-re " *;"))
    ;; handle block comments here
    (goto-char begin)
    (cond
     ((looking-at comment-re)
      (progn
        (ledger-navigate-skip-lines-backwards comment-re)
        ;; We are either at the beginning of the buffer, or we found
        ;; a line outside the comment, or both.  If we are outside
        ;; the comment then we need to move forward a line.
        (unless (looking-at comment-re)
          (forward-line 1)
          (beginning-of-line))
        (setq begin (point))
        (goto-char pos)
        (ledger-navigate-skip-lines-forwards comment-re)
        (setq end (point))))
     ((looking-at "\\(?:comment\\|test\\)\\>")
      (setq end (or (save-match-data
                      (re-search-forward "^end[[:blank:]]+\\(?:comment\\|test\\)\\_>"))
                    (point-max)))))
    (list begin end)))

(defun ledger-navigate-block-comment (pos)
  "Move past the block comment at POS, and return its extents."
  (interactive "d")
  (goto-char pos)
  (let ((begin (progn (beginning-of-line)
                      (point)))
        (end (progn (end-of-line)
                    (point)))
        (comment-re " *;"))
    ;; handle block comments here
    (beginning-of-line)
    (if (looking-at comment-re)
        (progn
          (ledger-navigate-skip-lines-backwards comment-re)
          (setq begin (point))
          (goto-char pos)
          (beginning-of-line)
          (ledger-navigate-skip-lines-forwards comment-re)
          (setq end (point))))
    (list begin end)))


(defun ledger-navigate-find-element-extents (pos)
  "Return list containing beginning and end of the entity surrounding POS."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (ledger-navigate-skip-lines-backwards "[ \t]\\|end[[:blank:]]+\\(?:comment\\|test\\)\\_>")
    (if (looking-at "[=~0-9\\[]")
        (ledger-navigate-find-xact-extents pos)
      (ledger-navigate-find-directive-extents pos))))

(defun ledger-navigate-next-uncleared ()
  "Move point to the next uncleared transaction."
  (interactive)
  (when (looking-at ledger-payee-uncleared-regex)
    (forward-line))
  (if (re-search-forward ledger-payee-uncleared-regex nil t)
      (progn (beginning-of-line)
             (point))
    (user-error "No next uncleared transactions")))

(defun ledger-navigate-previous-uncleared ()
  "Move point to the previous uncleared transaction."
  (interactive)
  (when (equal (car (ledger-context-at-point)) 'acct-transaction)
    (ledger-navigate-beginning-of-xact))
  (if (re-search-backward ledger-payee-uncleared-regex nil t)
      (progn (beginning-of-line)
             (point))
    (user-error "No previous uncleared transactions")))


(provide 'ledger-navigate)

;;; ledger-navigate.el ends here
