;;; evil-lion.el --- Evil align operator, port of vim-lion -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-lion
;; Package-Requires: ((emacs "24.3") (evil "1.0.0"))
;; Version: 0.0.2
;; Keywords: emulations, evil, vim

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Evil align operator, port of vim-lion by Tom McDonald (https://github.com/tommcdo/vim-lion)
;;
;; Usage:
;;
;; (evil-lion-install)
;;
;; The above call will install "gl" evil operator, which is used as:
;;   gl TEXT-OBJECT SEPARATOR
;; for example,
;;   gl ip =
;; will align the paragraph on = signs
;;
;;; Code:

(require 'evil)

(defgroup evil-lion nil
  "Align operator for Evil."
  :prefix "evil-lion"
  :group 'evil)

(defcustom evil-lion-left-align-key (kbd "g l")
  "Default binding for ‘evil-lion-left’.

Must be set before the minor mode is enabled."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-lion)

(defcustom evil-lion-right-align-key (kbd "g L")
  "Default binding for ‘evil-lion-right’.

Must be set before the minor mode is enabled."

  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-lion)

(defcustom evil-lion-squeeze-spaces t
  "If non-nil, aligning will remove extra spaces."
  :group 'evil-lion
  :type  'boolean)

;;;###autoload(autoload 'evil-lion-left "evil-lion" nil t)
(evil-define-operator evil-lion-left (count beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the left of the found CHAR.

If CHAR is \"/\" the user is prompted interactively for a regular
expression instead of a single character"
  :move-point nil
  :type line
  (interactive "<c><r>c")
  (evil-lion--align beg end count 'left char))

;;;###autoload(autoload 'evil-lion-right "evil-lion" nil t)
(evil-define-operator evil-lion-right (count beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the right of the found CHAR.

If CHAR is \"/\" the user is prompted interactively for a regular
expression instead of a single character"
  :move-point nil
  :type line
  (interactive "<c><r>c")
  (evil-lion--align beg end count 'right char))

(defun evil-lion--align (beg end count type char)
  "Align the region b/w BEG and END.

If COUNT is 1, alignment will be done on the first match only.
TYPE can be either 'left or 'right.
CHAR is the character to align with."
  (cond ((eq char ?\r)
         (evil-lion--plain-align beg end))
        ((evil-lion--valid-char-p char)
         (let ((regex (evil-lion--maybe-read-regex char)))
           (evil-lion--align-region type count beg end regex)))))

(defun evil-lion--plain-align (beg end)
  "Aligh with rules defined by the major mode.

BEG and END specify the region."
  (let ((indent-tabs-mode nil))
    (align beg end)))

(defun evil-lion--valid-char-p (char)
  "Return nil if the CHAR is invalid align character, e.g. DEL."
  (not (memq char '(?\e ?\d ?\b)))) ;; ESC, DEL, BS

(defvar evil-lion--user-regex-history ()
  "List of the user-supplied regexes.")

(defun evil-lion--maybe-read-regex (char)
  "If CHAR is \"/\", ask the user for a regex. Otherwise regexp-quote CHAR."
  (if (eq char ?/)
      (let* ((default (or (car-safe evil-lion--user-regex-history) "/"))
             (regex (read-string (format "Pattern [%s]: " default)
                                 nil
                                 'evil-lion--user-regex-history
                                 default)))
        (progn
          (push regex evil-lion--user-regex-history)
          (delete-dups evil-lion--user-regex-history)
          regex))
    (regexp-quote (format  "%c" char))))

(declare-function align-region "align")
(defun evil-lion--align-region (type count beg end regex)
  "Build input for (align-region) and call it.

TYPE can be either 'left or 'right.
If COUNT is 1, the alignment will be performed on the first occurance
only.
BEG and END specify the retion to align.
REGEX is the regex to align by."
  (when (> (length regex) 0)

    (when (and count (> count 1))
      (user-error "Only COUNT `1' is supported at the moment"))

    (save-restriction
      (narrow-to-region beg end)

      ;; squeeze spaces if configured to do so
      (when evil-lion-squeeze-spaces
        (evil-lion--squeeze-spaces type count (point-min) (point-max) regex))

      ;; prepare input for align-region and call it
      (let* ((indent-tabs-mode nil)
             (regexp
              (if (eq type 'left) (concat "\\(\\)" regex) (concat regex "\\(\\)")))
             (spacing 0)
             (repeat
              (if (eq count 1) nil t))
             (group 1)
             (rule
              (list (list nil (cons 'regexp regexp)
                          (cons 'group group)
                          (cons 'spacing spacing)
                          (cons 'repeat repeat)))))
        ;; if align-region isn't loaded, load it
        (unless (fboundp 'align-region)
          (require 'align))
        (align-region (point-min) (point-max) 'entire rule nil nil)))))

(defun evil-lion--squeeze-spaces (type count beg end regex)
  "Replace multiple spaces with one space in the given region.

Each of the lines in the given region are processed, this function
performs line-wise operation, it doesn't strictly follow the given
region boundary.

TYPE can either be 'left or right.
If COUNT is 1, spaces will be squeezed on the first match only.
BEG and END specify the region.
REGEX is the regex that must follow or preceed the spaces."
  (save-excursion
    (let ((line-count (count-lines beg end)))
      (goto-char beg)
      (dotimes (_var line-count)
        (evil-lion--squeeze-spaces-on-current-line type count regex)
        (forward-line 1)))))

(defun evil-lion--squeeze-spaces-on-current-line (type count regex)
  "Replace multiple spaces with one space on the current line.

TYPE can either be 'left or right.
If COUNT is 1, spaces will be squeezed on the first match only.
For TYPE 'left, spaces will be squeezed only if the REGEX matches
after the spaces.
For TYPE 'right, spaces will be squeezed only if the REGEX matches
before the spaces."
  (beginning-of-line)
  ;; look for 2 or more spaces
  (let ((continue-loop t)
        (spaces-regex "\\([ ]\\{2,\\}\\)")) ;; match 2 or more spaces
    (while (and (re-search-forward regex (point-at-eol) t) continue-loop)
      (when (save-excursion (or
                             ;; for type 'right, match spaces after the regex
                             (and (eq type 'right) (looking-at spaces-regex))
                             ;; for type 'left, match spaces before the regex
                             (and (eq type 'left) (goto-char (match-beginning 0)) (looking-back spaces-regex (line-beginning-position)))))
        (replace-match " "))
      ;; if COUNT is 1, exit the loop after the first match of REGEX
      (when (eq count 1)
        (setq continue-loop nil)))))


(defun evil-lion--bind-keys (mode)
  "Bind keys for the given minor MODE."
  (when evil-lion-left-align-key
    (evil-define-minor-mode-key 'normal mode evil-lion-left-align-key 'evil-lion-left)
    (evil-define-minor-mode-key 'visual mode evil-lion-left-align-key 'evil-lion-left))
  (when evil-lion-right-align-key
    (evil-define-minor-mode-key 'normal mode evil-lion-right-align-key 'evil-lion-right)
    (evil-define-minor-mode-key 'visual mode evil-lion-right-align-key 'evil-lion-right)))

;;;###autoload
(define-minor-mode evil-lion-mode
  "evil-lion global mode, defines align operators 'gl' and 'gL'.

  Align with `gl MOTION CHAR` or right-align with `gL MOTION CHAR`.

  If CHAR is `/` you will be prompted for a regular expression instead
  of a plain character.

  If CHAR is `RET` alignment will be performed with align.el's rules
  specific for the current major mode."
  :global t
  (evil-lion--bind-keys 'evil-lion-mode))

(provide 'evil-lion)

;;; evil-lion.el ends here
