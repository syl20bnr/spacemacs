;;; evil-nerd-commenter-sdk.el --- SDK used by other files  -*- lexical-binding: t -*-

;; Author: Chen Bin

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; SDK used internally

;;; Code:

(require 'evil nil t)
(defvar evil-state)

(defun evilnc--check-fonts (fonts-under-cursor fonts-list)
  "Check whether FONTS-UNDER-CURSOR among FONTS-LIST."
  (delq nil
        (mapcar #'(lambda (f)
                    ;; learn this trick from flyspell
                    (member f fonts-list))
                fonts-under-cursor)))

(defun evilnc-web-mode-comment-p (&optional pos)
  "Check whether the code at POS is comment.
`web-mode' removes its API, so create our own."
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property pos 'part-token) 'comment)))))

(defun evilnc-fonts-at-point (pos)
  "Get font faces at POS."
  (let* ((fontfaces (if (> pos 0) (get-text-property pos 'face))))
    (if (listp fontfaces) fontfaces (list fontfaces))))

(defun evilnc-pure-comment-p (pos)
  "Check character at POS is pure comment."
  (or (and (eq major-mode 'web-mode)
           (evilnc-web-mode-comment-p pos))
      (evilnc--check-fonts (evilnc-fonts-at-point pos)
                           '(font-lock-comment-face
                             tree-sitter-hl-face:comment
                             font-lock-comment-delimiter-face))))

(defmacro evilnc-get-char (position)
  "Get character at POSITION."
  `(char-after ,position))

(defmacro evilnc-whitespace-p (position)
  "Character at POSITION is white space."
  `(member (evilnc-get-char ,position) '(32 9)))

(defmacro evilnc-line-end-p (position)
  "Character at POSITION is line end."
  `(member (evilnc-get-char ,position) '(10 11)))

(defun evilnc-comment-p (pos)
  "Check whether the code at POS is comment by comparing font face.
Please note the white spaces out of comment is treated as comment,
or else we can't select multiple lines comment."
  (cond
   ((or (< pos (point-min)) (> pos (point-max)))
    nil)
   ((not (evilnc-fonts-at-point pos))
    ;; character under cursor is SPACE or TAB
    ;; and out of comment
    (evilnc-whitespace-p pos))
   (t
    (evilnc-pure-comment-p pos))))

(defun evilnc-comment-delimiter-p (position)
  "Is character at POSITION a comment delimiter?"
  (let* ((fontfaces (evilnc-fonts-at-point position)))
    (and fontfaces
         (evilnc--check-fonts fontfaces
                              '(font-lock-comment-delimiter-face)))))

(defun evilnc-sdk-inside-one-line-p (start end)
  "Test START and END is inside one line."
  (and (<= (line-beginning-position) start)
       (<= end (line-end-position))))

(defun evilnc-sdk-cur-line (&optional end)
  "String from line beginning to END or line end."
  (buffer-substring-no-properties (line-beginning-position)
                                  (or end (line-end-position))))

(defun evilnc-sdk-expand-to-contain-whole-lines (start end)
  "Expand region between START and END so the region contain whole lines.
Return new range like '(region_begin . region_end)."
  (save-excursion
    ;; Another work around for evil-visual-line bug:
    ;; In `evil-mode', if we use hotkey V or `evil-visual-line' to select line,
    ;; the (line-beginning-position) of the line which is after the last selected
    ;; line is always (region-end)! Don't know why.
    (when (and (> end start)
               (save-excursion
                 (goto-char end)
                 (= end (line-beginning-position)))
               (boundp 'evil-state)
               (eq evil-state 'visual))
      (setq end (1- end)))

    (goto-char start)
    (setq start (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position)))
  (cons start end))

(provide 'evil-nerd-commenter-sdk)
;;; evil-nerd-commenter-sdk.el ends here
