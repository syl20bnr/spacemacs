;;; funcs.el --- Games Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.




(defun spacemacs/tetris-quit-game ()
  "Correctly quit tetris by killng the game buffer."
  (interactive)
  (tetris-pause-game)
  (if (yes-or-no-p "Do you really want to quit ? ")
      (progn
        (tetris-end-game)
        (kill-buffer "*Tetris*"))
    (tetris-pause-game)))



(defun spacemacs/games-start-typit-beginner ()
  "Start `typit' game in beginner difficulty."
  (interactive)
  (spacemacs//games-start-typit 'basic))

(defun spacemacs/games-start-typit-expert ()
  "Start `typit' game in expert difficulty."
  (interactive)
  (spacemacs//games-start-typit 'advanced))

(defun spacemacs//games-start-typit (type)
  "Start a `typit' game with TYPE difficulty."
  (with-current-buffer (get-buffer-create "*typit*")
    (let ((evil-escape-inhibit t)
          (golden-ratio-mode nil))
      (evil-insert-state)
      (funcall (intern (format "typit-%S-test" type))))))


