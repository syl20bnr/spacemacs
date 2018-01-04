;;; funcs.el --- Games Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



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


