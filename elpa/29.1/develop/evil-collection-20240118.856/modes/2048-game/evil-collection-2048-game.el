;;; evil-collection-2048-game.el --- Evil bindings for `2048-game' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, games

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
;; Evil bindings for `2048-game'.

;;; Code:
(require 'evil-collection)
(require '2048-game nil t)

(defconst evil-collection-2048-game-maps '(2048-mode-map))

;;;###autoload
(defun evil-collection-2048-game-setup ()
  "Set up `evil' bindings for `2048-game'."
  (evil-set-initial-state '2048-mode 'normal)
  (evil-collection-define-key 'normal '2048-mode-map
    "h" '2048-left
    "j" '2048-down
    "k" '2048-up
    "l" '2048-right
    "r" '2048-random-move
    (kbd "<up>") '2048-up
    (kbd "<left>") '2048-left
    (kbd "<right>") '2048-right
    (kbd "<down>") '2048-down
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-collection-2048-game)
;;; evil-collection-2048-game.el ends here
