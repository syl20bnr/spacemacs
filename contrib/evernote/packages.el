;;; packages.el --- Evernote Layer packages File for Spacemacs
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
(defvar evernote-packages
  '(
    geeknote
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar evernote-excluded-packages '()
  "List of packages to exclude.")

(defun evernote/init-geeknote ()
  "Initializes geeknote and adds keybindings for its exposed functionalities."
  (use-package geeknote
    :commands (geeknote-create
               geeknote-edit
               geeknote-find
               geeknote-show
               geeknote-remove
               )
    :init
    (progn
      (evil-leader/set-key
        "aec" 'geeknote-create
        "aee" 'geeknote-edit
        "aef" 'geeknote-find
        "aes" 'geeknote-show
        "aer" 'geeknote-remove))))
