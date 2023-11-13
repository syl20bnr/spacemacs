;;; evil-command-window.el --- Evil command-line window  -*- lexical-binding: t -*-
;; Author: Emanuel Evans <emanuel.evans at gmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides an implementation of the Vim command-line window for
;; editing and repeating past Ex commands and searches.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-ex)
(require 'evil-search)

(defvar evil-search-module)

(defvar evil-command-window-current-buffer nil
  "The buffer from which the command-line window was called.")

(defvar evil-command-window-execute-fn nil
  "The command to execute when exiting the command-line window.")

(defvar evil--command-window-prompt nil
  "The key for the command that opened the command-line window (:, /, or ?).")

(define-derived-mode evil-command-window-mode fundamental-mode "Evil-cmd"
  "Major mode for the Evil command-line window."
  (add-hook 'after-change-functions #'evil--command-window-draw-prefix nil t)
  (auto-fill-mode 0))

(defun evil-command-window (history prompt execute-fn)
  "Open a command-line window for HISTORY with PROMPT and EXECUTE-FN.
HISTORY should be a list of commands.  PROMPT should be the
command-line prompt (one of \":\", \"/\" or \"?\").  EXECUTE-FN should
be a unary function to execute on the result that the user selects.

If called interactively, edit this minibuffer argument."
  (interactive
   (list (cons (minibuffer-contents) (minibuffer-history-value))
         (or (minibuffer-prompt) (user-error "Minibuffer is inactive"))
         #'evil--command-window-minibuffer-execute))
  (when (derived-mode-p 'evil-command-window-mode)
    (user-error "Command-line window is already open"))
  (when (evil-ex-p) (evil-ex-teardown))
  (let ((previous-buffer (current-buffer))
        (buffer (get-buffer-create "*Command Line*")))
    (with-current-buffer buffer
      (erase-buffer)
      (evil-command-window-mode)
      (setq-local evil-command-window-current-buffer previous-buffer)
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil--command-window-prompt prompt)
      (evil--command-window-insert-commands history))

    (let* ((action
            `((display-buffer-reuse-window display-buffer-at-bottom)
              ,@(unless (zerop evil-command-window-height)
                  `((window-height body-lines . ,evil-command-window-height)
                    (preserve-size nil . t)))
              (dedicated . t)))
           (window (display-buffer buffer action))
           (delete-window-fun
            (lambda (window)
              (set-window-parameter window 'delete-window nil)
              (delete-window window)
              (switch-to-minibuffer))))
      (when (minibufferp)
        (set-window-parameter window 'delete-window delete-window-fun))
      (select-window window)))
  (goto-char (point-max))
  (unless (bobp) (backward-char) (evil-adjust-cursor)))

(defun evil--command-window-draw-prefix (beg end _old-len)
  "Display `evil--command-window-prompt' as a prefix of the changed lines."
  (let ((prefix (propertize evil--command-window-prompt
                            'font-lock-face 'minibuffer-prompt)))
    (put-text-property beg end 'line-prefix prefix)))

(defun evil--command-window-insert-commands (history)
  "Insert the commands in HISTORY."
  (let ((inhibit-modification-hooks t))
    (dolist (cmd (reverse history)) (insert cmd "\n"))
    (evil--command-window-draw-prefix (point-min) (point-max) nil)))

(defun evil-command-window-execute ()
  "Execute the command on the current line in the appropriate buffer.
The local variable `evil-command-window-execute-fn' determines which
function to execute."
  (interactive)
  (let ((result (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
        (original-buffer evil-command-window-current-buffer)
        (execute-fn evil-command-window-execute-fn))
    (let ((ignore-window-parameters t))
      (ignore-errors (kill-buffer-and-window)))
    (unless (buffer-live-p original-buffer)
      (user-error "Originating buffer is no longer active"))
    (let ((window (get-buffer-window original-buffer)))
      (when window (select-window window)))
    (with-current-buffer original-buffer (funcall execute-fn result))))

(defun evil--command-window-minibuffer-execute (result)
  "Terminate this minibuffer argument with RESULT."
  (delete-minibuffer-contents)
  (insert result)
  (exit-minibuffer))

(defun evil-command-window-ex (&optional current-command)
  "Open a command-line window for editing and executing Ex commands.
If CURRENT-COMMAND is present, it will be inserted under the cursor as
the current command to be edited."
  (interactive)
  (evil-command-window (cons (or current-command "") evil-ex-history)
                       ":"
                       #'evil-command-window-ex-execute))

(define-obsolete-function-alias
  'evil-ex-command-window #'evil-command-window "1.15.0"
  "Start command window with Ex history and current minibuffer content.")

(define-obsolete-function-alias
  'evil-ex-search-command-window #'evil-command-window "1.15.0"
  "Start command window with search history and current minibuffer content.")

(defun evil-command-window-ex-execute (result)
  "Execute RESULT as an Ex command."
  (unless (string-match-p "\\`[ \t\n\r]*\\'" result)
    (unless (equal result (car evil-ex-history))
      (push result evil-ex-history))
    (evil-ex-execute result)))

(defun evil--command-window-search (forward)
  "Open a command-line window for searches."
  (evil-command-window
   (cons "" (cond ((eq evil-search-module 'evil-search)
                   evil-ex-search-history)
                  (forward evil-search-forward-history)
                  (t evil-search-backward-history)))
   (evil-search-prompt forward)
   (lambda (result) (evil-command-window-search-execute result forward))))

(defun evil-command-window-search-forward ()
  "Open a command-line window for forward searches."
  (interactive)
  (evil--command-window-search t))

(defun evil-command-window-search-backward ()
  "Open a command-line window for backward searches."
  (interactive)
  (evil--command-window-search nil))

(defun evil-command-window-search-execute (result forward)
  "Search for RESULT using FORWARD to determine direction."
  (unless (string= result "")
    (if (eq evil-search-module 'evil-search)
        (progn
          (setq evil-ex-search-pattern (evil-ex-make-search-pattern result)
                evil-ex-search-direction (if forward 'forward 'backward))
          (unless (equal result (car evil-ex-search-history))
            (push result evil-ex-search-history))
          (evil-ex-search))
      (evil-push-search-history result forward)
      (evil-search result forward evil-regexp-search))))

(provide 'evil-command-window)

;;; evil-command-window.el ends here
