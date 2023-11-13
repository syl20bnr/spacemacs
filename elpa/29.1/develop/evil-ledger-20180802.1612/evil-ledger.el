;;; evil-ledger.el --- Make `ledger-mode' more `evil'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; URL: https://github.com/atheriel/evil-ledger
;; Keywords: convenience evil languages ledger vim-emulation
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (evil "1.2.12") (ledger-mode "0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides `evil-ledger-mode', which provides motions for
;; `ledger-mode' using Evil.

;;; Code:

(require 'evil)
(require 'ledger-mode)
(require 'ledger-report)

(defgroup evil-ledger nil
  "Minor mode for more evil in `ledger-mode'."
  :group 'evil)

(defcustom evil-ledger-sort-key nil
  "When non-nil, specifies the keybinding in `evil-visual-state'
for `evil-ledger-sort'."
  :type 'str
  :group 'evil-ledger)

;;;; Motions

(evil-define-motion evil-ledger-forward-xact (&optional count)
  "Moves the cursor COUNT transactions forwards."
  (evil-motion-loop (nil (or count 1))
    (ledger-navigate-next-xact-or-directive)))

(evil-define-motion evil-ledger-backward-xact (&optional count)
  "Moves the cursor COUNT transactions backwards."
  (evil-motion-loop (nil (or count 1))
    (ledger-navigate-prev-xact-or-directive)))

;;;; Operators

(evil-define-operator evil-ledger-align (&optional beg end)
  "Align all postings in the region."
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (when (not beg)
    (ledger-post-align-xact beg))
  (ledger-post-align-postings beg end))

(evil-define-operator evil-ledger-sort (&optional beg end)
  "Sort all postings in the region by date."
  :keep-visual nil
  :move-point t
  (interactive "<r>")
  (when (and beg end)
    (ledger-sort-region beg end)))

;;;; Text Objects

(defsubst evil-ledger--xact-begin-near (&optional point)
  "Return the buffer position of the transaction beginning nearest POINT."
 (save-excursion
   (when point (goto-char point))
   (ledger-navigate-beginning-of-xact)
   (point)))

(evil-define-text-object evil-ledger-inner-xact (count &optional beg end _)
  "Select inside COUNT transactions at point."
  (let* ((begin (evil-ledger--xact-begin-near beg))
         (xend (save-excursion
                 (when end (goto-char end))
                 (ledger-navigate-end-of-xact)
                 (point)))
         ;; If the selection (or point) is inside a transaction, expand to
         ;; select that transaction before moving on to any others.
         (steps (if (or (and beg end (= (1+ beg) end))
                        (and beg end (= begin beg) (> xend end))
                        (and (null beg) (null end)))
                    (1- (or count 1))
                  (or count 1)))
         (end (save-excursion
                (when end (goto-char end))
                (evil-motion-loop (nil steps)
                  (ledger-navigate-next-xact-or-directive))
                (ledger-navigate-end-of-xact)
                (point))))
    (evil-range begin end)))

(evil-define-text-object evil-ledger-outer-xact (count &optional beg end _)
  "Select around COUNT transactions at point."
  (let ((begin (evil-ledger--xact-begin-near beg))
        (end (save-excursion
               (when end (goto-char end))
               (evil-motion-loop (nil (or count 1))
                 (ledger-navigate-next-xact-or-directive))
               (point))))
    (evil-range begin end)))

;;;; Minor Mode

(defvar evil-ledger-mode-map (make-sparse-keymap)
  "Keymap for command `evil-ledger-mode'.

\\{evil-ledger-mode-map}")

(defvar evil-ledger--mode-map-initialized nil)

(defun evil-ledger--mode-map-initialize ()
  "Add keys to `evil-ledger-mode-map'."
  (dolist (state '(normal motion visual))
    (evil-define-key* state evil-ledger-mode-map
      (kbd "gj") 'evil-ledger-forward-xact
      (kbd "gk") 'evil-ledger-backward-xact))
  (evil-define-key* 'visual evil-ledger-mode-map
    (kbd "=") #'evil-ledger-align)
  (when evil-ledger-sort-key
    (evil-define-key* 'visual evil-ledger-mode-map
      (kbd evil-ledger-sort-key) #'evil-ledger-sort))
  (setq evil-ledger--mode-map-initialized t))

;;;###autoload
(define-minor-mode evil-ledger-mode
  "Minor mode for more evil in `ledger-mode'.

The following keys are available in `evil-ledger-mode':

\\{evil-ledger-mode-map}"
  :lighter " EvilLedger"
  :keymap evil-ledger-mode-map
  :group 'evil-ledger
  (unless evil-ledger--mode-map-initialized
    (evil-ledger--mode-map-initialize))
  (define-key evil-visual-state-local-map (kbd "ix") 'evil-ledger-inner-xact)
  (define-key evil-operator-state-local-map (kbd "ix") 'evil-ledger-inner-xact)
  (define-key evil-visual-state-local-map (kbd "ax") 'evil-ledger-outer-xact)
  (define-key evil-operator-state-local-map (kbd "ax") 'evil-ledger-outer-xact)
  (evil-normalize-keymaps))

;;;; Report Mode Modification

(add-to-list 'evil-motion-state-modes 'ledger-report-mode)
(evil-define-key* 'motion ledger-report-mode-map
                  "e" #'ledger-report-edit-report
                  "gd" #'ledger-report-visit-source
                  "gr" #'ledger-report-redo)

(provide 'evil-ledger)

;; Local Variables:
;; coding: utf-8
;; End:

;;; evil-ledger.el ends here
