;;; evil-collection-tab-bar.el --- Evil bindings for tab-bar -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, tab-bar, tools

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
;; Evil bindings for tab-bar.

;;; Code:

(require 'evil-collection)
(require 'tab-bar nil t)

(defconst evil-collection-tab-bar-maps '(tab-switcher-mode-map))

(defun evil-collection-tab-bar-setup ()
  "Set up `evil' bindings to `tab-bar'."
  (evil-set-initial-state 'tab-switcher-mode 'normal)
  (evil-collection-define-key 'normal 'tab-switcher-mode-map
    ;; Movement
    "j" 'tab-switcher-next-line
    "k" 'tab-switcher-prev-line
    "gj" 'tab-switcher-next-line
    "gk" 'tab-switcher-prev-line

    ;; Mark
    "d" 'tab-switcher-delete
    (kbd "RET") 'tab-switcher-select
    "u" 'tab-switcher-unmark
    "x" 'tab-switcher-execute

    ;; Quit
    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window))

(provide 'evil-collection-tab-bar)

;;; evil-collection-tab-bar.el ends here
