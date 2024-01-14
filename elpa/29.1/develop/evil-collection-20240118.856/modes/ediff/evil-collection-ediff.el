;;; evil-collection-ediff.el --- Evil bindings for ediff -*- lexical-binding: t -*-
;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Homepage: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (evil "1.2.3"))
;; Keywords: evil, ediff, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make ediff a little evil. This configures ediff to be friendlier to users
;; of vim-like keybindings. Consult the help buffer (=?=) for more info.

;; Here's a table describing the bindings

;; | Command                     | Original Binding | Evil-ediff  |
;; |-----------------------------+------------------+-------------|
;; | ediff-next-difference       | n,SPC            | C-j,n,SPC   |
;; | ediff-previous-difference   | p,DEL            | C-k,N,p,DEL |
;; | ediff-jump-to-difference    | j                | d           |
;; | jump to first difference    | 1j               | gg (or 1d)  |
;; | jump to last difference     | N/A              | G           |
;; | copy region A to region B   | a                | a,l         |
;; | copy region B to region A   | b                | b,h         |
;; | scroll down 1 line          | C-u 1 v          | j           |
;; | scroll up 1 line            | C-u 1 V          | k           |
;; | scroll down half page       | v,C-v            | C-d,v,C-v   |
;; | scroll up half page         | V,M-v            | C-u,V,M-v   |
;; | scroll left                 | >                | zh          |
;; | scroll right                | <                | zl          |
;; | toggle highlighting         | h                | H           |
;; | ediff-suspend               | z                | C-z         |

;; Not implemented yet
;; | restore old diff            | ra,rb,rc         | u           |

;;; Code:

(require 'evil-collection)
(require 'ediff nil t)

(defconst evil-collection-ediff-maps '(ediff-mode-map))

(defvar evil-collection-ediff-initial-state-backup (evil-initial-state 'ediff-mode))
(defvar evil-collection-ediff-long-help-message-compare2-backup ediff-long-help-message-compare2)
(defvar evil-collection-ediff-long-help-message-compare3-backup  ediff-long-help-message-compare3)
(defvar evil-collection-ediff-long-help-message-narrow2-backup  ediff-long-help-message-narrow2)
(defvar evil-collection-ediff-long-help-message-word-backup  ediff-long-help-message-word-mode)
(defvar evil-collection-ediff-long-help-message-merge-backup  ediff-long-help-message-merge)
(defvar evil-collection-ediff-long-help-message-head-backup  ediff-long-help-message-head)
(defvar evil-collection-ediff-long-help-message-tail-backup  ediff-long-help-message-tail)

(defvar evil-collection-ediff-help-changed nil)

(defun evil-collection-ediff-adjust-help ()
  "Adjust long help messages to reflect evil-ediff bindings."
  (unless evil-collection-ediff-help-changed
    (dolist (msg '(ediff-long-help-message-compare2
                   ediff-long-help-message-compare3
                   ediff-long-help-message-narrow2
                   ediff-long-help-message-word-mode
                   ediff-long-help-message-merge
                   ediff-long-help-message-head
                   ediff-long-help-message-tail))
      (dolist (chng '( ;;("^" . "  ")
                      ("p,DEL -previous diff " . "k,N,p -previous diff ")
                      ("n,SPC -next diff     " . "  j,n -next diff     ")
                      ("    j -jump to diff  " . "    d -jump to diff  ")
                      ("    h -highlighting  " . "    H -highlighting  ")
                      ("  v/V -scroll up/dn  " . "C-u/d -scroll up/dn  ")
                      ("  </> -scroll lt/rt  " . "zh/zl -scroll lt/rt  ")
                      ("  z/q -suspend/quit"   . "C-z/q -suspend/quit")))
        (setf (symbol-value msg)
              (replace-regexp-in-string (car chng) (cdr chng) (symbol-value msg))))))
  (setq evil-collection-ediff-help-changed t))

(defun evil-collection-ediff-scroll-left (&optional arg)
  "Scroll left."
  (interactive "P")
  (let ((last-command-event ?>))
    (ediff-scroll-horizontally arg)))

(defun evil-collection-ediff-scroll-right (&optional arg)
  "Scroll right."
  (interactive "P")
  (let ((last-command-event ?<))
    (ediff-scroll-horizontally arg)))

(defun evil-collection-ediff-scroll-up (&optional arg)
  "Scroll up by half of a page."
  (interactive "P")
  (let ((last-command-event ?V))
    (ediff-scroll-vertically arg)))

(defun evil-collection-ediff-scroll-down (&optional arg)
  "Scroll down by half of a page."
  (interactive "P")
  (let ((last-command-event ?v))
    (ediff-scroll-vertically arg)))

(defun evil-collection-ediff-scroll-down-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?v))
    (ediff-scroll-vertically 1)))

(defun evil-collection-ediff-scroll-up-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?V))
    (ediff-scroll-vertically 1)))

(defun evil-collection-ediff-first-difference ()
  "Jump to first difference."
  (interactive)
  (ediff-jump-to-difference 1))

(defun evil-collection-ediff-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference ediff-number-of-differences))

;; (defun evil-collection-ediff-restore-diff ()
;;   "Restore the copy of current region."
;;   (interactive)
;;   (ediff-restore-diff nil ?a)
;;   (ediff-restore-diff nil ?b))

(defvar evil-collection-ediff-bindings
  '(("d"    . ediff-jump-to-difference)
    ("H"    . ediff-toggle-hilit)
    ("\C-e" . evil-collection-ediff-scroll-down-1)
    ("\C-y" . evil-collection-ediff-scroll-up-1)
    ("j"    . ediff-next-difference)
    ("k"    . ediff-previous-difference)
    ("N"    . ediff-previous-difference)
    ("gg"   . evil-collection-ediff-first-difference)
    ("G"    . evil-collection-ediff-last-difference)
    ("\C-d" . evil-collection-ediff-scroll-down)
    ("\C-u" . evil-collection-ediff-scroll-up)
    ("\C-z" . ediff-suspend)
    ("z"    . nil)
    ("zl"   . evil-collection-ediff-scroll-right)
    ("zh"   . evil-collection-ediff-scroll-left)
    ;; Not working yet
    ;; ("u"    . evil-collection-ediff-restore-diff)
    )
  "A list of bindings changed/added in evil-ediff.")

(defun evil-collection-ediff-startup-hook ()
  "Place evil-ediff bindings in `ediff-mode-map'."
  (evil-make-overriding-map ediff-mode-map 'normal)
  (dolist (entry evil-collection-ediff-bindings)
    (define-key ediff-mode-map (car entry) (cdr entry)))
  (unless (or ediff-3way-comparison-job
              (eq ediff-split-window-function 'split-window-vertically))
    (define-key ediff-mode-map "l" 'ediff-copy-A-to-B)
    (define-key ediff-mode-map "h" 'ediff-copy-B-to-A))
  (evil-normalize-keymaps)
  nil)

;;;###autoload
(defun evil-collection-ediff-setup ()
  "Initialize evil-ediff."
  (interactive)
  (evil-set-initial-state 'ediff-mode 'normal)
  (add-hook 'ediff-keymap-setup-hook 'evil-collection-ediff-startup-hook)
  (evil-collection-ediff-adjust-help))

(defun evil-collection-ediff-revert ()
  "Revert changes made by evil-ediff."
  (interactive)
  (evil-set-initial-state 'ediff-mode evil-collection-ediff-initial-state-backup)
  (unless evil-collection-ediff-help-changed
    (dolist (msg
             '((ediff-long-help-message-compare2 . ediff-long-help-message-compare2-backup)
               (ediff-long-help-message-compare3 . ediff-long-help-message-compare3-backup)
               (ediff-long-help-message-narrow2 . ediff-long-help-message-narrow2-backup)
               (ediff-long-help-message-word-mode . ediff-long-help-message-word-mode-backup)
               (ediff-long-help-message-merge . ediff-long-help-message-merge-backup)
               (ediff-long-help-message-head . ediff-long-help-message-head-backup)
               (ediff-long-help-message-tail . ediff-long-help-message-tail-backup)))
      (setf (symbol-value (car msg)) (symbol-value (cdr msg)))))
  (setq evil-collection-ediff-help-changed nil)
  (remove-hook 'ediff-keymap-setup-hook 'evil-collection-ediff-startup-hook))

(provide 'evil-collection-ediff)
;;; evil-collection-ediff.el ends here
