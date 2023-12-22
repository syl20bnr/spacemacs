;;; evil-collection-dashboard.el --- Evil bindings for Dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, dashboard, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil bindings for Dashboard.

;;; Code:

(require 'evil-collection)
(require 'dashboard nil t)

(defvar dashboard-mode-map)

(defconst evil-collection-dashboard-maps '(dashboard-mode-map))

(defun evil-collection-dashboard-setup-jump-commands ()
  "Set up bindings for jump commands in Dashboard."
  (evil-collection-define-key 'normal 'dashboard-mode-map
    "r" (symbol-function (lookup-key dashboard-mode-map "r")) ; recents
    "m" (symbol-function (lookup-key dashboard-mode-map "m")) ; bookmarks
    "p" (symbol-function (lookup-key dashboard-mode-map "p")) ; projects
    "a" (symbol-function (lookup-key dashboard-mode-map "a")) ; agenda
    ;; - Dashboard inserts shortcut hints in its buffer, so it's
    ;; hard to differ from the default.
    ;;
    ;; - "registers" isn't shown in Dashboard by default; those who
    ;; added it would have to choose between the widget and losing
    ;; `evil-forward-word-end'. That's probably still better than
    ;; having a shortcut hint that isn't correct.
    "e" (symbol-function (lookup-key dashboard-mode-map "e")))) ; registers

;;;###autoload
(defun evil-collection-dashboard-setup ()
  "Set up Evil bindings for Dashboard."
  (evil-collection-set-readonly-bindings 'dashboard-mode-map)
  (evil-collection-define-key 'normal 'dashboard-mode-map
    ;; Movement
    "j" 'dashboard-next-line
    "k" 'dashboard-previous-line
    ;; Like `evil-collection-outline.el'.
    ;; These don't support COUNT, so it's probably better to leave [[
    ;; and ]] alone.
    "gj" 'dashboard-next-section
    "gk" 'dashboard-previous-section

    ;; Enabling this binding makes RET insert a newline.
    ;; Default `evil-ret' works better.
    ;; (kbd "RET") 'dashboard-return

    ;; Other commands
    [tab] 'widget-forward
    [backtab] 'widget-backward
    [down-mouse-1] 'widget-button-click)

  ;; Jump commands
  ;;
  ;; I had to use an advice as dashboard.el defines these bindings
  ;; after the map has been defined, as unnamed functions bound to a
  ;; local variable. See
  ;; https://github.com/emacs-dashboard/emacs-dashboard/blob/f15d3e2e/dashboard-widgets.el#L308
  ;; for what I mean.
  ;;
  ;; Despite the name, this function is what sets up the dashboard
  ;; buffer itself.
  (advice-add 'dashboard-insert-startupify-lists :after
              'evil-collection-dashboard-setup-jump-commands))

(provide 'evil-collection-dashboard)

;;; evil-collection-dashboard.el ends here
