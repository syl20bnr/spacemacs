;;; funcs.el --- Games Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3



(defun space-macs/tetris-quit-game ()
  "Correctly quit tetris by killng the game buffer."
  (interactive)
  (tetris-pause-game)
  (if (yes-or-no-p "Do you really want to quit ? ")
      (progn
        (tetris-end-game)
        (kill-buffer "*Tetris*"))
    (tetris-pause-game)))



(defun space-macs/games-start-typit-beginner ()
  "Start `typit' game in beginner difficulty."
  (interactive)
  (space-macs//games-start-typit 'basic))

(defun space-macs/games-start-typit-expert ()
  "Start `typit' game in expert difficulty."
  (interactive)
  (space-macs//games-start-typit 'advanced))

(defun space-macs//games-start-typit (type)
  "Start a `typit' game with TYPE difficulty."
  (with-current-buffer (get-buffer-create "*typit*")
    (let ((evil-escape-inhibit t)
          (golden-ratio-mode nil))
      (evil-insert-state)
      (funcall (intern (format "typit-%S-test" type))))))




