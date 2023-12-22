;;; evil-collection-newsticker.el --- Evil bindings for newsticker -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, newsticker, tools

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
;; Evil bindings for newsticker.

;;; Code:
(require 'evil-collection)
(require 'newsticker)

(defconst evil-collection-newsticker-maps '(newsticker-mode-map))

;;;###autoload
(defun evil-collection-newsticker-setup ()
  "Set up `evil' bindings for `newsticker'."
  ;; plainview
  (evil-set-initial-state 'newsticker-mode 'normal)
  (evil-collection-define-key 'normal 'newsticker-mode-map
    ;; move
    "gk" 'newsticker-previous-feed
    "gj" 'newsticker-next-feed

    ;; mark
    "r" 'newsticker-mark-item-at-point-as-read
    "i" 'newsticker-mark-item-at-point-as-immortal

    ;; show/hide
    "o" 'newsticker-show-old-items
    "O" 'newsticker-hide-old-items

    ;; refresh
    "gr" 'newsticker-buffer-force-update
    "gR" 'newsticker-get-all-news

    ;; quit
    "q" 'newsticker-close-buffer)

  ;; treeview
  (evil-set-initial-state 'newsticker-treeview-list-mode 'normal)
  (evil-collection-define-key 'normal 'newsticker-treeview-list-mode-map
    [down-mouse-3] 'newsticker-treeview-list-menu)

  (evil-set-initial-state 'newsticker-treeview-item-mode 'normal)
  (evil-collection-define-key 'normal 'newsticker-treeview-item-mode-map
    [down-mouse-3] 'newsticker-treeview-item-menu)

  (evil-set-initial-state 'newsticker-treeview-mode 'normal)
  (evil-collection-define-key 'normal 'newsticker-treeview-mode-map
    ;; move
    "[[" 'newsticker-treeview-prev-feed
    "]]" 'newsticker-treeview-next-feed
    ;; The items in Newsticker List buffer has a particular keymap by text
    ;; property, where CR (C-m) and LF (C-j) are bound to
    ;; `newsticker-treeview-show-item'. According to the keymap precedence page,
    ;; the text property based keymap has a higher priority.
    ;;
    ;; Eval (info "(elisp)Searching Keymaps") if you have interests.
    ;;
    ;; Use M-j/M-k instead.
    (kbd "M-j") 'newsticker-treeview-next-item
    (kbd "M-k") 'newsticker-treeview-prev-item
    "gj" 'newsticker-treeview-next-new-or-immortal-item
    "gk" 'newsticker-treeview-prev-new-or-immortal-item
    (kbd "SPC") 'newsticker-treeview-next-page
    "J" 'newsticker-treeview-jump

    ;; mark
    "o" 'newsticker-treeview-mark-item-old
    "O" 'newsticker-treeview-mark-list-items-old
    "i" 'newsticker-treeview-toggle-item-immortal

    "b" 'newsticker-treeview-browse-url-item
    "B" 'newsticker-treeview-browse-url

    ;; group
    "A" 'newsticker-group-add-group
    "D" 'newsticker-group-delete-group
    "R" 'newsticker-group-rename-group
    "S" 'newsticker-treeview-save
    (kbd "M-m") 'newsticker-group-move-feed
    (kbd "<M-down>") 'newsticker-group-shift-feed-down
    (kbd "<M-up>") 'newsticker-group-shift-feed-up
    (kbd "<M-S-down>") 'newsticker-group-shift-group-down
    (kbd "<M-S-up>") 'newsticker-group-shift-group-up

    ;; refresh
    "gr" 'newsticker-treeview-update
    "gR" 'newsticker-get-all-news

    ;; quit
    "q" 'newsticker-treeview-quit))

(provide 'evil-collection-newsticker)
;;; evil-collection-newsticker.el ends here
