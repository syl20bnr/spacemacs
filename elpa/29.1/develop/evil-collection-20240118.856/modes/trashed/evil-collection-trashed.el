;;; evil-collection-trashed.el --- Evil bindings for trashed.el -*- lexical-binding: t -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.3
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, files, convenience, tools

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
;; Evil bindings for `trashed'.

;;; Code:
(require 'evil-collection)
(require 'trashed nil t)

(defconst evil-collection-trashed-maps '(trashed-mode-map))

;;;###autoload
(defun evil-collection-trashed-setup ()
  "Set up `evil' bindings for `trashed'."
  (evil-collection-set-readonly-bindings 'trashed-mode-map)
  (evil-collection-define-key 'normal 'trashed-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'trashed-forward-column
    (kbd "<backtab>") 'trashed-backward-column

    (kbd "RET") 'trashed-find-file
    (kbd "S-RET") 'trashed-find-file-other-window
    (kbd "go") 'trashed-find-file-other-window
    (kbd "gO") 'trashed-display-file
    (kbd "gw") 'trashed-browse-url-of-file
    (kbd "M-RET") 'trashed-display-file

    "S" 'tabulated-list-sort

    "m" 'trashed-mark
    "M" 'trashed-mark-all
    "u" 'trashed-unmark
    "U" 'trashed-unmark-all
    "~" 'trashed-toggle-marks

    "$ m" 'trashed-mark-files-by-date
    "$ u" 'trashed-unmark-files-by-date
    "$ d" 'trashed-flag-delete-files-by-date
    "$ r" 'trashed-flag-restore-files-by-date

    "% m" 'trashed-mark-files-regexp
    "% u" 'trashed-unmark-files-regexp
    "% d" 'trashed-flag-delete-files-regexp
    "% r" 'trashed-flag-restore-files-regexp

    "d" 'trashed-flag-delete
    "r" 'trashed-flag-restore

    "zb" 'trashed-flag-backup-files
    "za" 'trashed-flag-auto-save-files

    "x" 'trashed-do-execute
    "D" 'trashed-do-delete
    "R" 'trashed-do-restore

    "v" 'trashed-view-file

    "g?" 'describe-mode
    "gr" 'revert-buffer))

(provide 'evil-collection-trashed)
;;; evil-collection-trashed.el ends here
