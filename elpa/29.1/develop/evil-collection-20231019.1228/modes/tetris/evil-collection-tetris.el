;;; evil-collection-tetris.el --- Bindings for `tetris' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: MaxSt
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `tetris'.

;;; Code:
(require 'evil-collection)
(require 'tetris nil t)

(defconst evil-collection-tetris-maps '(tetris-mode-map))

;;;###autoload
(defun evil-collection-tetris-setup ()
  "Set up `evil' bindings for `tetris'."
  (evil-set-initial-state 'tetris-mode 'normal)
  (evil-collection-define-key 'normal 'tetris-mode-map
    "h" 'tetris-move-left
    "l" 'tetris-move-right
    "k" 'tetris-rotate-next
    "K" 'tetris-rotate-prev
    "j" 'tetris-move-down
    "J" 'tetris-move-bottom
    "G" 'tetris-move-bottom
    "gr" 'tetris-start-game
    (kbd "TAB") 'tetris-move-bottom
    "p" 'tetris-pause-game
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'tetris-end-game))

(provide 'evil-collection-tetris)
;;; evil-collection-tetris.el ends here
