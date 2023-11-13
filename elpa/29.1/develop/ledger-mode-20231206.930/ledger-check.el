;;; ledger-check.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Craig Earls (enderw88 AT gmail DOT com)

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
;;  Provide secial mode to correct errors in ledger when running with --strict and --explicit
;;
;; Adapted to ledger mode by Craig Earls <enderw88 at gmail dot com>

;;; Code:

(require 'easymenu)
(require 'ledger-navigate)
(require 'ledger-report) ; for ledger-master-file


(defvar ledger-check-buffer-name "*Ledger Check*")
(defvar ledger-original-window-cfg nil)




(defvar ledger-check-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'ledger-report-visit-source)
    (define-key map [?q] 'ledger-check-quit)
    map)
  "Keymap for `ledger-check-mode'.")

(easy-menu-define ledger-check-mode-menu ledger-check-mode-map
  "Ledger check menu"
  '("Check"
    ;; ["Re-run Check" ledger-check-redo]
    "---"
    ["Visit Source" ledger-report-visit-source]
    "---"
    ["Quit" ledger-check-quit]
    ))

(define-derived-mode ledger-check-mode text-mode "Ledger-Check"
  "A mode for viewing ledger errors and warnings.")


(defun ledger-do-check ()
  "Run a check command ."
  (goto-char (point-min))
  (let ((data-pos (point))
        (have-warnings nil))
    (shell-command
     ;;  ledger balance command will just return empty if you give it
     ;;  an account name that doesn't exist.  I will assume that no
     ;;  one will ever have an account named "e342asd2131".  If
     ;;  someones does, this will probably still work for them.
     ;;  I should only highlight error and warning lines.
     "ledger bal e342asd2131 --strict --explicit "
     t nil)
    (goto-char data-pos)

    ;; format check report to make it navigate the file

    (while (re-search-forward "^.*: \"\\(.*\\)\", line \\([0-9]+\\)" nil t)
      (let ((file (match-string 1))
            (line (string-to-number (match-string 2))))
        (when file
          (set-text-properties (line-beginning-position) (line-end-position)
                               (list 'ledger-source (cons file (save-window-excursion
                                                                 (save-excursion
                                                                   (find-file file)
                                                                   (widen)
                                                                   (ledger-navigate-to-line line)
                                                                   (point-marker))))))
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'font-lock-face 'ledger-font-report-clickable-face))
          (setq have-warnings 'true)
          (end-of-line))))
    (if (not have-warnings)
        (insert "No errors or warnings reported."))))

(defun ledger-check-goto ()
  "Goto the ledger check buffer."
  (interactive)
  (let ((rbuf (get-buffer ledger-check-buffer-name)))
    (if (not rbuf)
        (error "There is no ledger check buffer"))
    (pop-to-buffer rbuf)
    (shrink-window-if-larger-than-buffer)))

(defun ledger-check-quit ()
  "Quit the ledger check buffer."
  (interactive)
  (ledger-check-goto)
  (set-window-configuration ledger-original-window-cfg)
  (kill-buffer (get-buffer ledger-check-buffer-name)))

(defun ledger-check-buffer ()
  "Check the current buffer for errors.

Runs ledger with --explicit and --strict report errors and assist
with fixing them.

The output buffer will be in `ledger-check-mode', which defines
commands for navigating the buffer to the errors found, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
                (y-or-n-p "Buffer modified, save it? "))
       (save-buffer))))
  (let ((_buf (find-file-noselect (ledger-master-file)))
        (cbuf (get-buffer ledger-check-buffer-name))
        (wcfg (current-window-configuration)))
    (if cbuf
        (kill-buffer cbuf))
    (with-current-buffer
        (pop-to-buffer (get-buffer-create ledger-check-buffer-name))
      (ledger-check-mode)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (ledger-do-check)
      (shrink-window-if-larger-than-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (message "q to quit; r to redo; k to kill"))))


(provide 'ledger-check)

;;; ledger-check.el ends here
