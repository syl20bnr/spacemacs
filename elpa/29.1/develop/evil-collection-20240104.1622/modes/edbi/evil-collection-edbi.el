;;; evil-collection-edbi.el --- Evil bindings for EDBI -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, database, tools

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
;; Evil bindings for EDBI.

;;; Code:
(require 'evil-collection)
(require 'edbi nil t)

(defvar ctbl:table-mode-map)
(defvar edbi:dbview-keymap)
(declare-function ctbl:define-keymap "ctable")
(declare-function epc:add-keymap "epc")

(defconst evil-collection-edbi-maps '(edbi:dbview-query-result-keymap
                                      edbi:sql-mode-map))

(defun evil-collection-edbi-dbview-setup ()
  "The `ctbl:table-mode-map' and `edbi:dbview-keymap' are stored
as local maps and are not bound to any mode.  As such, the only
way to add Evil bindings to the dbview is to overwrite the
default bindings."
  (setq ctbl:table-mode-map
        (ctbl:define-keymap
         '(
           ("k" . ctbl:navi-move-up)
           ("j" . ctbl:navi-move-down)
           ("h" . ctbl:navi-move-left)
           ("l" . ctbl:navi-move-right)

           ("gc" . ctbl:navi-jump-to-column)

           ("$" . ctbl:navi-move-right-most)
           ("0" . ctbl:navi-move-left-most)

           ("gr" . ctbl:action-update-buffer)

           ("g?" . ctbl:describe-bindings)

           ([mouse-1] . ctbl:navi-on-click)
           ("C-m" . ctbl:navi-on-click)
           ("RET" . ctbl:navi-on-click))))

  (setq edbi:dbview-keymap
        (epc:add-keymap
         ctbl:table-mode-map
         '(("gr" . edbi:dbview-update-command)
           ("SPC" . edbi:dbview-show-tabledef-command)
           ("c" . edbi:dbview-query-editor-command)
           ("C" . edbi:dbview-query-editor-new-command)
           ("C-m" . edbi:dbview-show-table-data-command)
           ("q" . edbi:dbview-quit-command)))))

;;;###autoload
(defun evil-collection-edbi-setup ()
  "Set up `evil' bindings for EDBI."
  (evil-collection-edbi-dbview-setup)
  (evil-collection-define-key 'normal 'edbi:dbview-query-result-keymap
    "0" 'ctbl:navi-move-left-most
    "$" 'ctbl:navi-move-right-most

    "h" 'ctbl:navi-move-left
    "j" 'ctbl:navi-move-down
    "k" 'ctbl:navi-move-up
    "l" 'ctbl:navi-move-right

    "gc" 'ctbl:navi-jump-to-column

    "g?" 'ctbl:describe-bindings

    ;; motion
    "gr" 'ctbl:action-update-buffer

    ;; quit
    "q" 'edbi:dbview-query-result-quit-command
    "ZQ" 'evil-quit
    "ZZ" 'quit-window)

  (evil-collection-define-key 'normal 'edbi:sql-mode-map
    "q" 'edbi:dbview-query-editor-quit-command
    (kbd "C-n") 'edbi:dbview-query-editor-history-forward-command
    (kbd "C-p") 'edbi:dbview-query-editor-history-back-command))

(provide 'evil-collection-edbi)
;;; evil-collection-edbi.el ends here
