;;; packages.el --- games Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq games-packages
      '(
        2048-game
        (helm-games :location local)
        pacmacs
        (tetris :location built-in)
        ))

(defun games/init-2048-game ()
  (use-package 2048-mode
    :defer t
    :init
    (progn
      (push '("2048" . (2048-game :quit (kill-buffer-ask (get-buffer "2048"))
                                  :reset 2048-init)) helm-games-list)
      (evilified-state-evilify 2048-mode 2048-mode-map
        "j" '2048-down
        "k" '2048-up
        "h" '2048-left
        "l" '2048-right))))

(when (configuration-layer/layer-usedp 'spacemacs-helm)
  (defun games/init-helm-games ()
    (use-package helm-games
      :commands helm-games
      :init (spacemacs/set-leader-keys "aG" 'helm-games))))

(defun games/init-pacmacs ()
  (use-package pacmacs
    :defer t
    :init
    (push '("pacmacs" . (pacmacs-start :quit (kill-buffer-ask (get-buffer "*Pacmacs*"))
                                :reset pacmacs-start)) helm-games-list)
    (evilified-state-evilify pacmacs-mode pacmacs-mode-map
      "h" 'pacmacs-left
      "j" 'pacmacs-down
      "k" 'pacmacs-up
      "l" 'pacmacs-right)))

(defun games/init-tetris ()
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
