;;; evil-collection-calendar.el --- Evil bindings for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, calendar, tools

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
;; Evil bindings for the calendar.

;;; Code:
(require 'calendar)
(require 'evil-collection)

(defconst evil-collection-calendar-maps '(calendar-mode-map))

(defun evil-collection-calendar-setup-org-bindings ()
  "Bind Org functions to Calendar keymap."
  (require 'org)
  (require 'org-agenda)
  (defvar org-calendar-to-agenda-key)
  (defvar org-agenda-diary-file)
  (defvar org-calendar-insert-diary-entry-key)

  (let ((key (pcase org-calendar-to-agenda-key
               ((and key (pred stringp)) key)
               ;; The default value of `org-calendar-to-agenda-key' is [?c] in Org 9.1
               ;; We do make a minimal compatibility support for vector of integers.
               ((and key (pred vectorp)) (mapconcat (lambda (x) (format "%c" x)) key ""))
               ((and (pred symbolp) 'default) "c")
               (_ ""))))
    (evil-collection-define-key 'normal 'calendar-mode-map
      (kbd key) 'org-calendar-goto-agenda))

  ;; Similar to `org-calendar-to-agenda-key'.
  (unless (eq org-agenda-diary-file 'diary-file)
    (let ((key (pcase org-calendar-insert-diary-entry-key
                 ((and key (pred stringp)) key)
                 ((and key (pred vectorp)) (mapconcat (lambda (x) (format "%c" x)) key ""))
                 ;; NOTE Consistent behavior with `org-calendar-to-agenda-key'
                 ;; although it's not supported in Org officially.
                 ((and (pred symbolp) 'default) "i")
                 (_ ""))))
      (evil-collection-define-key 'normal 'calendar-mode-map
        (kbd key) 'org-agenda-diary-entry))))

;; Otherwise it will load tons of org stuff at startup.
(defvar evil-collection-calendar-want-org-bindings)
(when evil-collection-calendar-want-org-bindings
  (add-hook 'calendar-mode-hook #'evil-collection-calendar-setup-org-bindings))

;;;###autoload
(defun evil-collection-calendar-setup ()
  "Set up `evil' bindings for `calendar'."
  (evil-set-initial-state 'calendar-mode 'normal)
  (evil-collection-define-key 'normal 'calendar-mode-map
    ;; motion
    "h" 'calendar-backward-day
    "j" 'calendar-forward-week
    "k" 'calendar-backward-week
    "l" 'calendar-forward-day
    "0" 'calendar-beginning-of-week
    "^" 'calendar-beginning-of-week
    "$" 'calendar-end-of-week
    "[[" 'calendar-backward-year
    "]]" 'calendar-forward-year
    (kbd "M-<") 'calendar-beginning-of-year
    (kbd "M->") 'calendar-end-of-year
    "(" 'calendar-beginning-of-month
    ")" 'calendar-end-of-month
    (kbd "SPC") 'scroll-other-window
    (kbd "S-SPC") 'scroll-other-window-down
    (kbd "<delete>") 'scroll-other-window-down
    "<" 'calendar-scroll-right
    ">" 'calendar-scroll-left
    (kbd "C-b") 'calendar-scroll-right-three-months
    (kbd "C-f") 'calendar-scroll-left-three-months
    "{" 'calendar-backward-month
    "}" 'calendar-forward-month
    (kbd "C-k") 'calendar-backward-month
    (kbd "C-j") 'calendar-forward-month
    "gk" 'calendar-backward-month
    "gj" 'calendar-forward-month

    ;; visual
    "v" 'calendar-set-mark

    ;; goto
    "." 'calendar-goto-today
    "o" 'calendar-other-month
    "gd" 'calendar-goto-date ; "gd" in evil-org-agenda, "gd" in Emacs.
    "gD" 'calendar-other-month

    ;; diary
    "D" 'diary-view-other-diary-entries
    "d" 'diary-view-entries
    "m" 'diary-mark-entries
    "s" 'diary-show-all-entries

    ;; appointment
    "Aa" 'appt-add
    "Ad" 'appt-delete

    "u" 'calendar-unmark
    "x" 'calendar-mark-holidays

    ;; show
    "gm" 'calendar-lunar-phases ; "gm" in evil-org-agenda. TODO: Shadows calendar-mayan.
    "gs" 'calendar-sunrise-sunset ; "gs" in evil-org-agenda
    "gh" 'calendar-list-holidays ; "gh" in evil-org-agenda. TODO: Shadows calendar-hebrew.
    "gc" 'org-calendar-goto-agenda ; "gc" in evil-org-agenda. TODO: Shadows calendar-iso.
    "a" 'calendar-list-holidays
    "r" 'calendar-cursor-holidays

    ;; refresh
    "gr" 'calendar-redraw

    "g?" 'calendar-goto-info-node
    "?" 'calendar-goto-info-node ; Search is not very useful.
    (kbd "M-=") 'calendar-count-days-region

    ;; quit
    "q" 'calendar-exit
    "ZQ" 'evil-quit
    "ZZ" 'calendar-exit))

(provide 'evil-collection-calendar)
;;; evil-collection-calendar.el ends here
