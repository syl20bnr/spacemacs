;;; ledger-occur.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Provide buffer narrowing to ledger mode.  Adapted from original loccur
;; mode by Alexey Veretennikov <alexey dot veretennikov at gmail dot
;; com>
;;
;; Adapted to ledger mode by Craig Earls <enderww at gmail dot
;; com>

;;; Code:

(require 'cl-lib)
(require 'ledger-navigate)

(defconst ledger-occur-overlay-property-name 'ledger-occur-custom-buffer-grep)

(defcustom ledger-occur-use-face-shown t
  "If non-nil, use a custom face for xacts shown in `ledger-occur' mode.
This uses `ledger-occur-xact-face'."
  :type 'boolean
  :group 'ledger)
(make-variable-buffer-local 'ledger-occur-use-face-shown)


(defvar ledger-occur-history nil
  "History of previously searched expressions for the prompt.")

(defvar ledger-occur-current-regex nil
  "Pattern currently applied to narrow the buffer.")
(make-variable-buffer-local 'ledger-occur-current-regex)

(defvar ledger-occur-mode-map (make-sparse-keymap))

(define-minor-mode ledger-occur-mode
  "A minor mode which display only transactions matching a pattern.
The pattern is given by `ledger-occur-current-regex'."
  :init-value nil
  :lighter (:eval (format " Ledger-Narrow(%s)" ledger-occur-current-regex))
  :keymap ledger-occur-mode-map
  (if (and ledger-occur-current-regex ledger-occur-mode)
      (ledger-occur-refresh)
    (ledger-occur-remove-overlays)
    (message "Showing all transactions")))

(define-key ledger-occur-mode-map (kbd "C-c C-g") #'ledger-occur-refresh)
(define-key ledger-occur-mode-map (kbd "C-c C-f") #'ledger-occur-mode)

(defun ledger-occur-refresh ()
  "Re-apply the current narrowing expression."
  (interactive)
  (let ((matches (ledger-occur-compress-matches
                  (ledger-occur-find-matches ledger-occur-current-regex))))
    (if matches
        (ledger-occur-create-overlays matches)
      (message "No matches found for '%s'" ledger-occur-current-regex)
      (ledger-occur-mode -1))))

(defun ledger-occur (regex)
  "Show only transactions in the current buffer which match REGEX.

This command hides all xact in the current buffer except those
matching REGEX.  If REGEX is nil or empty, turn off any narrowing
currently active."
  (interactive
   (list (read-regexp "Regexp" (ledger-occur-prompt) 'ledger-occur-history)))
  (if (or (null regex)
          (zerop (length regex)))  ; empty regex, or already have narrowed, clear narrowing
      (ledger-occur-mode -1)
    (setq ledger-occur-current-regex regex)
    (ledger-occur-mode 1)))

(defun ledger-occur-prompt ()
  "Return the default value of the prompt.

   Default value for prompt is a current word or active
   region(selection), if its size is 1 line"
  (if (use-region-p)
      (let ((pos1 (region-beginning))
            (pos2 (region-end)))
        ;; Check if the start and the of an active region is on
        ;; the same line
        (if (= (line-number-at-pos pos1)
               (line-number-at-pos pos2))
            (buffer-substring-no-properties pos1 pos2)))
    (current-word)))


(defun ledger-occur-make-visible-overlay (beg end)
  (let ((ovl (make-overlay beg end (current-buffer))))
    (overlay-put ovl ledger-occur-overlay-property-name t)
    (when ledger-occur-use-face-shown
      (overlay-put ovl 'font-lock-face 'ledger-occur-xact-face))))

(defun ledger-occur-make-invisible-overlay (beg end)
  (let ((ovl (make-overlay beg end (current-buffer))))
    (overlay-put ovl ledger-occur-overlay-property-name t)
    (overlay-put ovl 'invisible t)))

(defun ledger-occur-create-overlays (ovl-bounds)
  "Create the overlays for the visible transactions.
Argument OVL-BOUNDS contains bounds for the transactions to be left visible."
  (let* ((beg (caar ovl-bounds))
         (end (cl-cadar ovl-bounds)))
    (ledger-occur-remove-overlays)
    (ledger-occur-make-invisible-overlay (point-min) (1- beg))
    (dolist (visible (cdr ovl-bounds))
      (ledger-occur-make-visible-overlay beg end)
      (ledger-occur-make-invisible-overlay (1+ end) (1- (car visible)))
      (setq beg (car visible))
      (setq end (cadr visible)))
    (ledger-occur-make-invisible-overlay (1+ end) (point-max))))

(defun ledger-occur-remove-overlays ()
  "Remove the transaction hiding overlays."
  (interactive)
  (remove-overlays (point-min)
                   (point-max) ledger-occur-overlay-property-name t))

(defun ledger-occur-find-matches (regex)
  "Return a list of bounds for transactions matching REGEX."
  (save-excursion
    (goto-char (point-min))
    ;; Set initial values for variables
    (let (endpoint lines bounds)
      ;; Search loop
      (while (not (eobp))
        ;; if something found
        (when (setq endpoint (re-search-forward regex nil 'end))
          (setq bounds (ledger-navigate-find-element-extents endpoint))
          (push bounds lines)
          ;; move to the end of the xact, no need to search inside it more
          (goto-char (cadr bounds))))
      (nreverse lines))))

(defun ledger-occur-compress-matches (buffer-matches)
  "identify sequential xacts to reduce number of overlays required"
  (if buffer-matches
      (let ((points (list))
            (current-beginning (caar buffer-matches))
            (current-end (cl-cadar buffer-matches)))
        (dolist (match (cdr buffer-matches))
          (if (< (- (car match) current-end) 2)
              (setq current-end (cadr match))
            (push (list current-beginning current-end) points)
            (setq current-beginning (car match))
            (setq current-end (cadr match))))
        (nreverse (push (list current-beginning current-end) points)))))

(provide 'ledger-occur)

;;; ledger-occur.el ends here
