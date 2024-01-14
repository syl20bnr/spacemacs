;;; evil-collection-eww.el --- Evil bindings for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, eww, tools

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
;; Evil bindings for EWW.

;;; Code:
(require 'eww)
(require 'evil-collection)

(defvar evil-collection-eww-maps '(eww-mode-map
                                   eww-history-mode-map
                                   eww-buffers-mode-map
                                   eww-bookmark-mode-map))

;;;###autoload
(defun evil-collection-eww-setup ()
  "Set up `evil' bindings for `eww'."

  (evil-set-initial-state 'eww-mode 'normal)
  (evil-collection-define-key 'normal 'eww-mode-map
    "^" 'eww-up-url
    "u" 'eww-up-url
    "U" 'eww-top-url
    (kbd "DEL") 'eww-back-url
    "H" 'eww-back-url
    "L" 'eww-forward-url

    "gf" 'eww-view-source               ; Like qutebrowser.

    "&" 'eww-browse-with-external-browser
    "gc" 'url-cookie-list
    "zd" 'eww-toggle-paragraph-direction
    "ze" 'eww-set-character-encoding
    "zf" 'eww-toggle-fonts
    "d" 'eww-download
    "m" 'eww-add-bookmark
    "R" 'eww-readable                   ; Default binding.
    "r" 'eww-readable

    "]]" 'eww-next-url
    "[[" 'eww-previous-url
    "gj" 'eww-next-url
    "gk" 'eww-previous-url

    ;; open
    (kbd "S-<return>") 'eww-browse-with-external-browser
    "go" 'eww-browse-with-external-browser
    "o" 'eww                            ; Like qutebrowser.

    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'shr-next-link
    (kbd "<backtab>") 'shr-previous-link

    ;; bookmarks
    "gb" 'eww-list-bookmarks

    "gh" 'eww-list-histories
    "gt" 'eww-list-buffers              ; Like dwb, qutebrowser.

    ;; refresh
    "gr" 'eww-reload

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window)

  (evil-collection-define-operator-key 'yank 'eww-mode-map
    "u" 'eww-copy-page-url)

  (evil-collection-set-readonly-bindings 'eww-history-mode-map)
  (evil-set-initial-state 'eww-history-mode 'normal)
  (evil-collection-define-key 'normal 'eww-history-mode-map
    (kbd "RET") 'eww-history-browse
    ;; refresh
    "gr" 'revert-buffer)

  (evil-collection-set-readonly-bindings 'eww-buffers-mode-map)
  (evil-set-initial-state 'eww-buffers-mode 'normal)
  (evil-collection-define-key 'normal 'eww-buffers-mode-map
    "D" 'eww-buffer-kill
    (kbd "RET") 'eww-buffer-select
    "]]" 'eww-buffer-show-next
    "[[" 'eww-buffer-show-previous
    "gj" 'eww-buffer-show-next
    "gk" 'eww-buffer-show-previous
    ;; refresh
    "gr" 'revert-buffer)

  (evil-collection-set-readonly-bindings 'eww-bookmark-mode-map)
  (evil-set-initial-state 'eww-bookmark-mode 'normal)
  (evil-collection-define-key 'normal 'eww-bookmark-mode-map
    "D" 'eww-bookmark-kill
    "P" 'eww-bookmark-yank

    (kbd "RET") 'eww-bookmark-browse
    ;; refresh
    "gr" 'revert-buffer)

  (evil-collection-define-operator-key 'yank 'eww-bookmark-mode-map
    "u" 'eww-copy-page-url))

(provide 'evil-collection-eww)
;;; evil-collection-eww.el ends here
