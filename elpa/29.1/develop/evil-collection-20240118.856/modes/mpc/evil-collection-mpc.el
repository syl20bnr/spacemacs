;;; evil-collection-mpc.el --- Bindings for `mpc-mode' -*- lexical-binding: t -*-
;; Copyright (C) 2021, 2022 pspiagicw

;; Author: pspiagicw <pspiagicw@gmail.com>
;; Maintainer: pspiagicw <pspiagicw@gmail.com>
;; pspiagicw <pspiagicw@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, mpc, processes , mpd

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
;;; Bindings for `mpc-mode'.

;;; Code:
(require 'mpc)
(require 'evil-collection)

(defconst evil-collection-mpc-mode-maps '(mpc-mode-map))

(defun evil-collection-mpc-move-down ()
  "Move the cursor down along with selecting the element."
  (interactive)
  (evil-next-visual-line)
  (mpc-select))

(defun evil-collection-mpc-move-up ()
  "Move the cursor up along with selecting the element."
  (interactive)
  (evil-previous-visual-line)
  (mpc-select))

;;;###autoload
(defun evil-collection-mpc-setup ()
  "Setup up `evil' bindings for `mpc-mode'."
  (evil-collection-define-key 'normal 'mpc-mode-map
    "C-j" 'evil-collection-mpc-move-down
    "C-k" 'evil-collection-mpc-move-up
    "t" 'mpc-toggle-play
    "r" 'mpc-toggle-repeat
    "s" 'mpc-toggle-shuffle
    "c" 'mpc-toggle-consume
    "p" 'mpc-playlist
    "a" 'mpc-playlist-add
    ">" 'mpc-next
    "<" 'mpc-prev
    "x" 'mpc-play-at-point
    "RET" 'mpc-select))

(provide 'evil-collection-mpc)
;;; evil-collection-mpc.el ends here
