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

(defvar games-packages '(
                         2048-game
                         pacmacs
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
