;;; packages.el --- Prodigy Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq prodigy-packages '(prodigy))

(defun prodigy/init-prodigy ()
  (use-package prodigy
    :init
    (space-macs/set-leader-keys "atp" 'prodigy)
    :config
    (progn
      (evilified-state-evilify prodigy-mode prodigy-mode-map
        "c" 'prodigy-view-clear-buffer
        "h" 'prodigy-first
        "j" 'prodigy-next
        "k" 'prodigy-prev
        "l" 'prodigy-last
        "H" 'prodigy-display-process
        "J" 'prodigy-next-with-status
        "K" 'prodigy-prev-with-status
        "L" 'prodigy-start
        "d" 'prodigy-jump-file-manager
        "g" 'prodigy-jump-magit
        "Y" 'prodigy-copy-cmd
        "R" 'revert-buffer)
      (evilified-state-evilify prodigy-view-mode prodigy-view-mode-map
        "gf" 'find-file-at-point
        "q" 'quit-window))))


