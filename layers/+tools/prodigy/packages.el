;;; packages.el --- Prodigy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq prodigy-packages '(prodigy))

(defun prodigy/init-prodigy ()
  (use-package prodigy
    :init
    (spacemacs/set-leader-keys "aS" 'prodigy)
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
