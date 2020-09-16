;;; packages.el --- Evernote Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
      (spacemacs/declare-prefix "awe" "applications-evernote")
      (spacemacs/set-leader-keys
        "awec" 'geeknote-create
        "awee" 'geeknote-edit
        "awef" 'geeknote-find
        "awes" 'geeknote-show
        "awer" 'geeknote-remove
        "awem" 'geeknote-move))))
