;;; packages.el --- 2048-game Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar 2048-game-packages '(2048-game))

(defun 2048-game/init-2048-game ()
  (use-package 2048-mode
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "a2" '2048-game)
      (evilify 2048-mode 2048-mode-map
               "j" '2048-down
               "k" '2048-up
               "h" '2048-left
               "l" '2048-right))))
