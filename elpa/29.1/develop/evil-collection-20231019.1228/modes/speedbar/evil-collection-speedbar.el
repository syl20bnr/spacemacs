;;; evil-collection-speedbar.el --- Evil bindings for speedbar -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, speedbar, tools

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
;; Evil bindings for speedbar.

;;; Code:

(require 'evil-collection)
(require 'speedbar)

(defconst evil-collection-speedbar-maps '(speedbar-mode-map
                                          speedbar-file-key-map
                                          speedbar-buffers-key-map))

(defun evil-collection-speedbar-switch-to-previous-expansion-list ()
  "Back to previously used expansion list."
  (interactive)
  (speedbar-change-initial-expansion-list speedbar-previously-used-expansion-list-name))

(defun evil-collection-speedbar-switch-to-quick-buffers ()
  "Switch to quick-buffers expansion list."
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))

(defun evil-collection-speedbar-switch-to-files ()
  "Switch to files expansion list."
  (interactive)
  (speedbar-change-initial-expansion-list "files"))

;;;###autoload
(defun evil-collection-speedbar-setup ()
  "Set up `evil' bindings for `speedbar'."
  (evil-set-initial-state 'speedbar-mode 'normal)
  (evil-collection-define-key 'normal 'speedbar-mode-map
    "j" 'speedbar-next
    "k" 'speedbar-prev
    "gr" 'speedbar-refresh
    "gg" 'evil-goto-first-line
    "G"  'evil-goto-line

    ;; Basic tree features
    (kbd "RET") 'speedbar-edit-line
    (kbd "TAB") 'speedbar-toggle-line-expansion

    ;; Fold
    "zo" 'speedbar-expand-line
    "zO" 'speedbar-expand-line-descendants
    "zc" 'speedbar-contract-line
    "zC" 'speedbar-contract-line-descendants

    ;; Toggle
    "a" 'speedbar-toggle-show-all-files
    "t" 'speedbar-toggle-updates
    "i" 'speedbar-toggle-images
    "o" 'speedbar-toggle-sorting

    ;; Navigation
    (kbd "M-j") 'speedbar-restricted-next
    (kbd "M-k") 'speedbar-restricted-prev
    "]]" 'speedbar-forward-list
    "[[" 'speedbar-backward-list
    (kbd "C-p") 'evil-collection-speedbar-switch-to-previous-expansion-list
    (kbd "C-n") 'evil-collection-speedbar-switch-to-previous-expansion-list
    "b" 'evil-collection-speedbar-switch-to-quick-buffers
    "f" 'evil-collection-speedbar-switch-to-files
    "J" 'speedbar-change-initial-expansion-list)

  (evil-collection-define-key 'normal 'speedbar-file-key-map
    ;; File based commands
    "B" 'speedbar-item-byte-compile
    "C" 'speedbar-item-copy
    "D" 'speedbar-item-delete
    "I" 'speedbar-item-info
    "K" 'speedbar-item-object-delete
    "L" 'speedbar-item-load
    "R" 'speedbar-item-rename
    "+" 'speedbar-create-directory
    "^" 'speedbar-up-directory
    "-" 'speedbar-up-directory)

  (evil-collection-define-key 'normal 'speedbar-buffers-key-map
    ;; Buffer specific keybindings
    "K" 'speedbar-buffer-kill-buffer
    "gr" 'speedbar-buffer-revert-buffer))

(provide 'evil-collection-speedbar)
;;; evil-collection-speedbar.el ends here
