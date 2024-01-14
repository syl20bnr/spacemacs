;;; evil-collection-w3m.el --- Evil bindings for w3m -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, w3m, tools

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
;; Evil bindings for `w3m'.

;;; Code:
(require 'evil-collection)
(require 'w3m nil t)

(defconst evil-collection-w3m-maps '(w3m-mode-map))

;;;###autoload
(defun evil-collection-w3m-setup ()
  "Set up `evil' bindings for `w3m'."
  (evil-set-initial-state 'w3m-mode 'normal)
  (evil-collection-define-key 'normal 'w3m-mode-map
    (kbd "S-SPC") 'w3m-scroll-down-or-previous-url
    (kbd "SPC") 'w3m-scroll-up-or-next-url

    (kbd "RET") 'w3m-view-this-url
    "o" 'w3m-goto-url
    "O" 'w3m-goto-url-new-session

    "]]" 'w3m-next-form
    "[[" 'w3m-previous-form

    "H" 'w3m-view-previous-page
    "L" 'w3m-view-next-page
    "^" 'w3m-view-parent-page

    "gf" 'w3m-view-source
    "gh" 'w3m-view-header

    "d" 'w3m-download-this-url
    "M-d" 'w3m-download

    "I" 'w3m-view-image
    "M-i" 'w3m-save-image
    "zi" 'w3m-toggle-inline-image
    "zI" 'w3m-toggle-inline-images
    "M-T" 'w3m-turnoff-inline-images

    ;; bookmarks
    "gb" 'w3m-bookmark-view
    "gB" 'w3m-bookmark-view-new-session
    "a" 'w3m-bookmark-add-current-url

    ;; refresh
    "gr" 'w3m-reload-this-page
    "gR" 'w3m-reload-all-pages

    ;; quit
    "q" 'w3m-close-window
    "ZQ" 'w3m-quit
    "ZZ" 'quit-window)

  (evil-collection-define-operator-key 'yank 'w3m-mode-map
    ;; yt
    "t" 'w3m-print-this-url
    ;; yu
    "u" 'w3m-print-current-url))

(provide 'evil-collection-w3m)
;;; evil-collection-w3m.el ends here
