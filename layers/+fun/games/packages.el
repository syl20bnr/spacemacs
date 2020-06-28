;;; packages.el --- games Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
        (helm-games :location local
                    :requires helm)
        pacmacs
        (tetris :location built-in)
        sudoku
        typit
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

(defun games/init-helm-games ()
  (use-package helm-games
    :commands helm-games
    :init
    (progn
      (spacemacs/declare-prefix "afg" "games")
      (spacemacs/set-leader-keys "afg" 'helm-games))))

(defun games/init-pacmacs ()
  (use-package pacmacs
    :defer t
    :init
    (push '("pacmacs" . (pacmacs-start
                         :quit (kill-buffer-ask (get-buffer "*Pacmacs*"))
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
                                      "tetris-scores.txt")))
    :config
    (progn
      (evilified-state-evilify tetris-mode tetris-mode-map
        "h" 'tetris-move-left
        "i" 'tetris-rotate-prev
        "j" 'tetris-move-bottom
        "k" 'tetris-rotate-next
        "l" 'tetris-move-right
        "q" 'spacemacs/tetris-quit-game))))

(defun games/init-sudoku ()
  (use-package sudoku
    :defer t
    :init
    (progn
      (push '("sudoku" . (sudoku :quit (kill-buffer-ask (get-buffer "*Sudoku*"))
                                 :reset sudoku-restart)) helm-games-list)
      (evilified-state-evilify sudoku-mode sudoku-mode-map
        ;; Movement
        "j" 'sudoku-move-point-down
        "J" 'sudoku-move-point-downmost
        "k" 'sudoku-move-point-up
        "K" 'sudoku-move-point-upmost
        "h" 'sudoku-move-point-left
        "H" 'sudoku-move-point-leftmost
        "l" 'sudoku-move-point-right
        "L" 'sudoku-move-point-rightmost

        ;; Start/quit/print game
        "N" 'sudoku
        "q" 'sudoku-quit
        "Q" 'sudoku-quit-immediately
        "P" 'sudoku-print

        ;; Undo/redo
        "u"    'sudoku-undo
        "\C-r" 'sudoku-redo

        ;; Inserting values
        "1" 'sudoku-change-point
        "2" 'sudoku-change-point
        "3" 'sudoku-change-point
        "4" 'sudoku-change-point
        "5" 'sudoku-change-point
        "6" 'sudoku-change-point
        "7" 'sudoku-change-point
        "8" 'sudoku-change-point
        "9" 'sudoku-change-point)

      (spacemacs/set-leader-keys-for-major-mode 'sudoku-mode
        "c" 'sudoku-comment-puzzle
        "h" 'sudoku-hint
        "l" 'sudoku-load-puzzle
        "L" 'sudoku-load-puzzle-collection
        "s" 'sudoku-save-puzzle))))

(defun games/init-typit ()
  (use-package typit
    :defer t
    :init
    (progn
      (push '("typit (beginner)" .
              (spacemacs/games-start-typit-beginner
               :quit (kill-buffer-ask (get-buffer "*typit*"))
               :reset spacemacs/games-start-typit-beginner))
            helm-games-list)
      (push '("typit (expert)" .
              (spacemacs/games-start-typit-expert
               :quit (kill-buffer-ask (get-buffer "*typit*"))
               :reset spacemacs/games-start-typit-expert))
            helm-games-list))))
