;;; evil-collection-vterm.el --- Bindings for `vterm' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen
;; Copyright (C) 2020 Alex Griffin <a@ajgrf.com>

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, vterm, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `vterm'.

;;; Code:
(require 'evil-collection)
(require 'vterm nil t)

(defconst evil-collection-vterm-maps '(vterm-mode-map))

(declare-function vterm-goto-char "vterm")
(declare-function vterm--get-prompt-point "vterm")
(declare-function vterm--get-end-of-line "vterm")
(declare-function vterm-delete-region "vterm")

(defvar vterm--process)

(defcustom evil-collection-vterm-move-cursor-back nil
  "Whether the cursor is moved backwards when exiting insert state."
  :type 'boolean
  :group 'vterm)

(defun evil-collection-vterm-escape-stay ()
  "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but
it is not appropriate in some cases like terminals."
  (setq-local evil-move-cursor-back
              evil-collection-vterm-move-cursor-back))

(defvar-local evil-collection-vterm-send-escape-to-vterm-p nil
  "Track whether or not we send ESC to `vterm' or `emacs'.")

(defun evil-collection-vterm-toggle-send-escape ()
  "Toggle where ESC is sent between `vterm' and `emacs'.

This is needed for programs that use ESC, e.g. vim or an ssh'd emacs that
also uses `evil-mode'."
  (interactive)
  (if evil-collection-vterm-send-escape-to-vterm-p
      (evil-collection-define-key 'insert 'vterm-mode-map (kbd "<escape>")
        (lookup-key evil-insert-state-map (kbd "<escape>")))
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "<escape>") 'vterm--self-insert))
  (setq evil-collection-vterm-send-escape-to-vterm-p
        (not evil-collection-vterm-send-escape-to-vterm-p))
  (message (format "Sending ESC to %s."
                   (if evil-collection-vterm-send-escape-to-vterm-p
                       "vterm"
                     "emacs"))))

(declare-function vterm-cursor-in-command-buffer-p "vterm")
(declare-function vterm-beginning-of-line "vterm")

(evil-define-motion evil-collection-vterm-first-non-blank ()
  "Move the cursor to the first non-blank character
after the prompt."
  :type exclusive
  (if (vterm-cursor-in-command-buffer-p (point))
      (vterm-beginning-of-line)
    (evil-first-non-blank)))

(defun evil-collection-vterm-insert ()
  "Insert character before cursor."
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun evil-collection-vterm-insert-line ()
  "Insert character at beginning of prompt."
  (interactive)
  (vterm-goto-char (vterm--get-prompt-point))
  (call-interactively #'evil-insert))

(defun evil-collection-vterm-append ()
  "Append character after cursor."
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-append))

(defun evil-collection-vterm-append-line ()
  "Append character at end-of-line."
  (interactive)
  (vterm-goto-char (vterm--get-end-of-line))
  (call-interactively #'evil-append))

(declare-function vterm-yank "vterm")

(defun evil-collection-vterm-paste-after (&optional arg)
  (interactive "P")
  (vterm-goto-char (+ 1 (point)))
  (call-interactively #'vterm-yank arg))

(declare-function vterm-reset-cursor-point "vterm")

(evil-define-operator evil-collection-vterm-delete (beg end type register yank-handler)
  "Modification of evil-delete to work in vterm buffer.
Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (let* ((beg (max (or beg (point)) (vterm--get-prompt-point)))
         (end (min (or end beg) (vterm--get-end-of-line))))
    (unless register
      (let ((text (filter-buffer-substring beg end)))
        (unless (string-match-p "\n" text)
          ;; set the small delete register
          (evil-set-register ?- text))))
    (let ((evil-was-yanked-without-register nil))
      (evil-yank beg end type register yank-handler))
    (cond
     ((eq type 'block)
      (evil-apply-on-block #'vterm-delete-region beg end nil))
     ((and (eq type 'line)
           (= end (point-max))
           (or (= beg end)
               (/= (char-before end) ?\n))
           (/= beg (point-min))
           (=  (char-before beg) ?\n))
      (vterm-delete-region (1- beg) end))
     (t
      (vterm-delete-region beg end)))
    ;; place cursor on beginning of line
    (when (and (called-interactively-p 'any)
               (eq type 'line))
      (vterm-reset-cursor-point))))

(evil-define-operator evil-collection-vterm-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (interactive "<R><x>")
  (evil-collection-vterm-delete beg end type register))

(evil-define-operator evil-collection-vterm-delete-char (beg end type register)
  "Delete current character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-collection-vterm-delete beg end type register))

(evil-define-operator evil-collection-vterm-delete-line (beg end type register yank-handler)
  "Modification of evil-delete line to work in vterm bufer. Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg))
         (visual-line-mode (and evil-respect-visual-line-mode
                                visual-line-mode))
         (line-end (if visual-line-mode
                       (save-excursion
                         (end-of-visual-line)
                         (point))
                     (line-end-position))))
    (when (evil-visual-state-p)
      (unless (memq type '(line screen-line block))
        (let ((range (evil-expand beg end
                                  (if visual-line-mode
                                      'screen-line
                                    'line))))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-collection-vterm-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-collection-vterm-delete beg end 'block register yank-handler)))
     ((memq type '(line screen-line))
      (evil-collection-vterm-delete beg end type register yank-handler))
     (t
      (evil-collection-vterm-delete beg line-end type register yank-handler)))))

(evil-define-operator evil-collection-vterm-change (beg end type register yank-handler)
  (evil-collection-vterm-delete beg end type register yank-handler)
  (evil-collection-vterm-insert))

(evil-define-operator evil-collection-vterm-change-line (beg end type register yank-handler)
  :motion evil-end-of-line-or-visual-line
  (evil-collection-vterm-delete-line beg end type register yank-handler)
  (evil-collection-vterm-insert))

(evil-define-operator evil-collection-vterm-substitute (beg end type register)
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-collection-vterm-change beg end type register))

(evil-define-operator evil-collection-vterm-substitute-line (beg end register yank-handler)
  :motion evil-line-or-visual-line
  :type line
  (interactive "<r><x>")
  (evil-collection-vterm-change beg end 'line register yank-handler))

(evil-define-motion evil-collection-vterm-next-line (count)
  "Move the cursor COUNT lines down.
But don't allow the cursor to move bellow the last prompt line."
  :type line
  ;; This successfully prevents the `j' button from moving to an empty line
  ;; bellow the last prompt. However, it still can be bugged for example by
  ;; going to the one line above the last prompt and doing `10j'.
  (when (> (count-words (point) (point-max)) 0)
    (evil-next-line count)))

;;;###autoload
(defun evil-collection-vterm-setup ()
  "Set up `evil' bindings for `vterm'."
  (evil-set-initial-state 'vterm-mode 'insert)

  (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

  ;; Open to a better binding...
  (evil-collection-define-key '(normal insert) 'vterm-mode-map
    (kbd "C-c C-z") 'evil-collection-vterm-toggle-send-escape)

  ;; Evil has some "C-" bindings in insert state that shadow regular terminal
  ;; bindings. Don't raw-send "C-c" (prefix key) nor "C-h" (help prefix).
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-a") 'vterm--self-insert
    (kbd "C-b") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-d") 'vterm--self-insert
    (kbd "C-e") 'vterm--self-insert
    (kbd "C-f") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-k") 'vterm--self-insert
    (kbd "C-l") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-n") 'vterm--self-insert
    (kbd "C-o") 'vterm--self-insert
    (kbd "C-p") 'vterm--self-insert
    (kbd "C-q") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-r") 'vterm--self-insert
    (kbd "C-s") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-t") 'vterm--self-insert
    (kbd "C-u") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-v") 'vterm--self-insert     ; Should not be necessary.
    (kbd "C-w") 'vterm--self-insert
    (kbd "C-y") 'vterm--self-insert
    (kbd "C-z") 'vterm--self-insert
    (kbd "<delete>") 'vterm-send-delete)

  (evil-collection-define-key 'normal 'vterm-mode-map
    "[[" 'vterm-previous-prompt
    "]]" 'vterm-next-prompt
    "p" 'evil-collection-vterm-paste-after
    "P" 'vterm-yank
    "a" 'evil-collection-vterm-append
    "A" 'evil-collection-vterm-append-line
    "d" 'evil-collection-vterm-delete
    "D" 'evil-collection-vterm-delete-line
    "x" 'evil-collection-vterm-delete-char
    "X" 'evil-collection-vterm-delete-backward-char
    (kbd "RET") 'vterm-send-return
    "^" 'evil-collection-vterm-first-non-blank
    "i" 'evil-collection-vterm-insert
    "I" 'evil-collection-vterm-insert-line
    "u" 'vterm-undo
    "c" 'evil-collection-vterm-change
    "C" 'evil-collection-vterm-change-line
    "s" 'evil-collection-vterm-substitute
    "S" 'evil-collection-vterm-substitute-line
    "j" 'evil-collection-vterm-next-line
    "G" 'vterm-reset-cursor-point)

  (evil-collection-define-key 'visual 'vterm-mode-map
    "d" 'evil-collection-vterm-delete
    "x" 'evil-collection-vterm-delete-backward-char))

(provide 'evil-collection-vterm)
;;; evil-collection-vterm.el ends here
