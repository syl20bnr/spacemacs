;;; packages.el --- Evernote Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(setq evernote-packages
  '(
    geeknote
    ))

(defun evernote/init-geeknote ()
  "Initializes geeknote and adds keybindings for its exposed functionalities."
  (use-package geeknote
    :commands (geeknote-create
               geeknote-edit
               geeknote-find
               geeknote-show
               geeknote-remove
               geeknote-move)
    :init
    (progn
      (space-macs/declare-prefix "awe" "applications-evernote")
      (space-macs/set-leader-keys
        "awec" 'geeknote-create
        "awee" 'geeknote-edit
        "awef" 'geeknote-find
        "awes" 'geeknote-show
        "awer" 'geeknote-remove
        "awem" 'geeknote-move))))


