;;; extensions.el --- Games Layer Extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq games-post-extensions
      '(
        emacs-builtin-tetris
        helm-games
        ))

(defun games/init-emacs-builtin-tetris ()
  (use-package tetris
    :defer t
    :init
    (progn
      (push '("Tetris" . (tetris :quit spacemacs/tetris-quit-game
                                 :reset tetris-start-game)) helm-games-list)
      (setq tetris-score-file (concat spacemacs-games-cache-directory
                                      "tetris-scores.txt"))
      (defun spacemacs/tetris-quit-game ()
        "Correctly quit tetris by killng the game buffer."
        (interactive)
        (tetris-pause-game)
        (if (yes-or-no-p "Do you really want to quit ? ")
            (progn
              (tetris-end-game)
              (kill-buffer "*Tetris*"))
          (tetris-pause-game))))
    :config
    (progn
      (evilified-state-evilify tetris-mode tetris-mode-map
        "h" 'tetris-move-left
        "i" 'tetris-rotate-prev
        "j" 'tetris-move-bottom
        "k" 'tetris-rotate-next
        "l" 'tetris-move-right
        "q" 'spacemacs/tetris-quit-game))))

(defun games/init-helm-games ()
  (use-package helm-games
    :commands helm-games
    :init (spacemacs/set-leader-keys "aG" 'helm-games)))
