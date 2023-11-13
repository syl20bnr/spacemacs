;;; evil-collection-p4.el --- Evil bindings for P4 -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, p4, tools

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
;; Evil bindings for P4.

;;; Code:
(require 'p4 nil t)
(require 'evil-collection)

(defvar p4-basic-mode-map)
(defvar p4-basic-list-mode-map)
(defvar p4-opened-list-mode-map)
(defvar p4-status-list-mode-map)
(defvar p4-filelog-mode-map)
(defvar p4-diff-mode-map)
(defvar p4-annotate-mode-map)

(defconst evil-collection-p4-maps '(p4-basic-mode-map
                                    p4-basic-list-mode-map
                                    p4-opened-list-mode-map
                                    p4-status-list-mode-map
                                    p4-filelog-mode-map
                                    p4-diff-mode-map
                                    p4-annotate-mode-map))

;;;###autoload
(defun evil-collection-p4-setup ()
  "Set up `evil' bindings for `p4'."
  (evil-set-initial-state 'p4-basic-mode 'normal)
  (evil-set-initial-state 'p4-basic-list-mode 'normal)
  (evil-set-initial-state 'p4-opened-list-mode-map 'normal)
  (evil-set-initial-state 'p4-status-list-mode-map 'normal)
  (evil-set-initial-state 'p4-filelog-mode-map 'normal)
  (evil-set-initial-state 'p4-diff-mode-map 'normal)
  (evil-set-initial-state 'p4-annotate-mode-map 'normal)

  (evil-collection-define-key 'normal 'p4-basic-mode-map
    [mouse-1] 'p4-buffer-mouse-clicked
    "k" 'evil-previous-line
    "j" 'evil-next-line
    (kbd "C-j") 'p4-forward-active-link
    (kbd "C-k") 'p4-backward-active-link
    (kbd "RET") 'p4-buffer-commands
    "q" 'quit-window
    "gr" 'revert-buffer
    "]]" 'p4-scroll-down-1-window
    "[[" 'p4-scroll-up-1-window
    "gg" 'p4-top-of-buffer
    "G" 'p4-bottom-of-buffer
    "=" 'delete-other-windows)

  (evil-collection-define-key 'normal 'p4-basic-list-mode-map
    (kbd "RET") 'p4-basic-list-activate)

  (evil-collection-define-key 'normal 'p4-opened-list-mode-map
    "R" 'p4-revert
    "T" 'p4-opened-list-type
    "C" 'p4-opened-list-change)

  (evil-collection-define-key 'normal 'p4-status-list-mode-map
    (kbd "RET") 'p4-status-list-activate)

  (evil-collection-define-key 'normal 'p4-filelog-mode-map
    "d" 'p4-diff2
    "gf" 'p4-find-file-other-window
    "S" 'p4-filelog-short-format
    "L" 'p4-filelog-long-format
    " " 'p4-scroll-up-1-window
    "gg" 'p4-top-of-buffer
    "G" 'p4-bottom-of-buffer
    "=" 'p4-delete-other-windows
    "]]" 'p4-goto-next-change
    "[[" 'p4-goto-prev-change
    (kbd "C-j") 'p4-goto-next-change
    (kbd "C-k") 'p4-goto-prev-change)

  (evil-collection-define-key 'normal 'p4-diff-mode-map
    "]]" 'diff-hunk-next
    (kbd "C-j") 'diff-hunk-next
    (kbd "M-j") 'diff-file-next
    "[[" 'diff-hunk-prev
    (kbd "C-k") 'diff-hunk-prev
    (kbd "M-k") 'diff-file-prev
    "\t" 'diff-hunk-next
    [backtab] 'diff-hunk-prev
    "}" 'diff-file-next
    "{" 'diff-file-prev
    "\C-m" 'p4-buffer-commands
    [mouse-2] 'p4-buffer-commands
    "gb" 'p4-buffer-commands)

  (evil-collection-define-key 'normal 'p4-annotate-mode-map
    "]]" 'p4-next-change-rev-line
    "[[" 'p4-prev-change-rev-line
    (kbd "C-j") 'p4-next-change-rev-line
    (kbd "C-k") 'p4-prev-change-rev-line
    "L" 'p4-toggle-line-wrap))

(provide 'evil-collection-p4)
;;; evil-collection-p4.el ends here
