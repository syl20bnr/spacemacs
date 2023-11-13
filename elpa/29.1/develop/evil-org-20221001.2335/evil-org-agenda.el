;;; evil-org-agenda.el --- evil keybindings for org-agenda-mode

;; Copyright (C) 2012-2017 by Somelauw
;; Maintainer: Somelauw
;; Original-author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org-mode.git
;; Git-Repository: git://github.com/Somelauw/evil-org-mode.git
;; Created: 2012-06-14
;; Forked-since: 2017-02-12
;; Version: 0.9.6
;; Package-Requires: ((emacs "24.4") (evil "1.0") (org "8.0.0"))
;; Keywords: evil vim-emulation org-mode key-bindings presets

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Minimal config
;; (evil-org-agenda-set-keys)

;;; Commentary:
;;
;; Known Bugs:
;; See, https://github.com/Somelauw/evil-org-mode/issues
;;
;;; Code:

(require 'evil)
(require 'org)

(defun evil-org-agenda-set-keys ()
  "Set motion state keys for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'motion)

  ;; Horizontal movements have little use, thus we can override "f" and "t".
  ;; "w", "b", "e", "ge" and their upcase counterparts are preserved.
  (evil-define-key 'motion org-agenda-mode-map
    ;; Unused keys: D, X

    ;; open
    (kbd "<tab>") 'org-agenda-goto
    (kbd "S-<return>") 'org-agenda-goto
    (kbd "g TAB") 'org-agenda-goto
    (kbd "RET") 'org-agenda-switch-to
    (kbd "M-RET") 'org-agenda-recenter

    (kbd "SPC") 'org-agenda-show-and-scroll-up
    (kbd "<delete>") 'org-agenda-show-scroll-down
    (kbd "<backspace>") 'org-agenda-show-scroll-down

    ;; motion
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "gj" 'org-agenda-next-item
    "gk" 'org-agenda-previous-item
    "gH" 'evil-window-top
    "gM" 'evil-window-middle
    "gL" 'evil-window-bottom
    (kbd "C-j") 'org-agenda-next-item
    (kbd "C-k") 'org-agenda-previous-item
    (kbd "[[") 'org-agenda-earlier
    (kbd "]]") 'org-agenda-later

    ;; manipulation
    ;; We follow standard org-mode bindings (not org-agenda bindings):
    ;; <HJKL> change todo items and priorities.
    ;; M-<jk> drag lines.
    ;; M-<hl> cannot demote/promote, we use it for "do-date".
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "H" 'org-agenda-do-date-earlier
    "L" 'org-agenda-do-date-later
    "t" 'org-agenda-todo
    (kbd "M-j") 'org-agenda-drag-line-forward
    (kbd "M-k") 'org-agenda-drag-line-backward
    (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
    (kbd "C-S-l") 'org-agenda-todo-nextset ; Original binding "C-S-<right>"

    ;; undo
    "u" 'org-agenda-undo

    ;; actions
    "dd" 'org-agenda-kill
    "dA" 'org-agenda-archive
    "da" 'org-agenda-archive-default-with-confirmation
    "ct" 'org-agenda-set-tags
    "ce" 'org-agenda-set-effort
    "cT" 'org-timer-set-timer
    "i" 'org-agenda-diary-entry
    "a" 'org-agenda-add-note
    "A" 'org-agenda-append-agenda
    "C" 'org-agenda-capture

    ;; mark
    "m" 'org-agenda-bulk-toggle
    "~" 'org-agenda-bulk-toggle-all
    "*" 'org-agenda-bulk-mark-all
    "%" 'org-agenda-bulk-mark-regexp
    "M" 'org-agenda-bulk-unmark-all
    "x" 'org-agenda-bulk-action

    ;; refresh
    "gr" 'org-agenda-redo
    "gR" 'org-agenda-redo-all

    ;; quit
    "ZQ" 'org-agenda-exit
    "ZZ" 'org-agenda-quit

    ;; display
    ;; "Dispatch" can prefix the following:
    ;; 'org-agenda-toggle-deadlines
    ;; 'org-agenda-toggle-diary
    ;; 'org-agenda-follow-mode
    ;; 'org-agenda-log-mode
    ;; 'org-agenda-entry-text-mode
    ;; 'org-agenda-toggle-time-grid
    ;; 'org-agenda-day-view
    ;; 'org-agenda-week-view
    ;; 'org-agenda-year-view
    "gD" 'org-agenda-view-mode-dispatch
    "ZD" 'org-agenda-dim-blocked-tasks

    ;; filter
    "sc" 'org-agenda-filter-by-category
    "sr" 'org-agenda-filter-by-regexp
    "se" 'org-agenda-filter-by-effort
    "st" 'org-agenda-filter-by-tag
    "s^" 'org-agenda-filter-by-top-headline
    "ss" 'org-agenda-limit-interactively
    "S" 'org-agenda-filter-remove-all

    ;; clock
    "I" 'org-agenda-clock-in ; Original binding
    "O" 'org-agenda-clock-out ; Original binding
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode

    ;; go and show
    "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
    "gc" 'org-agenda-goto-calendar
    "gC" 'org-agenda-convert-date
    "gd" 'org-agenda-goto-date
    "gh" 'org-agenda-holidays
    "gm" 'org-agenda-phases-of-moon
    "gs" 'org-agenda-sunrise-sunset
    "gt" 'org-agenda-show-tags

    "p" 'org-agenda-date-prompt
    "P" 'org-agenda-show-the-flagging-note

    ;; 'org-save-all-org-buffers ; Original binding "C-x C-s"

    ;; Others
    "+" 'org-agenda-manipulate-query-add
    "-" 'org-agenda-manipulate-query-subtract))

(provide 'evil-org-agenda)
;;; evil-org-agenda.el ends here
