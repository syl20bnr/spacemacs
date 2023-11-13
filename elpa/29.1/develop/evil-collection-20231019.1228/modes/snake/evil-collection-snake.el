;;; evil-collection-snake.el --- Bindings for `snake' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen
;; Copyright (C) 2022 Farzin Firouzi

;; Author: Farzin Firouzi <farzineff@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Bindings for `snake'.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-snake-maps '(snake-mode-map))

;;;###autoload
(defun evil-collection-snake-setup ()
  "Set up `evil' bindings for `snake'."
  (evil-set-initial-state 'snake-mode 'normal)
  (evil-collection-define-key 'normal 'snake-mode-map
    "h" 'snake-move-left
    "l" 'snake-move-right
    "j" 'snake-move-down
    "k" 'snake-move-up
    "gr" 'snake-start-game
    "p" 'snake-pause-game
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'snake-end-game))

(provide 'evil-collection-snake)
;;; evil-collection-snake.el ends here
