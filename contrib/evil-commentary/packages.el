;;; packages.el --- evil-commentary Layer packages File for Spacemacs
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

(defvar evil-commentary-packages '(evil-commentary)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar evil-commentary-excluded-packages '(evil-nerd-commenter)
  "List of packages to exclude.")

(defun evil-commentary/init-evil-commentary ()
  "Initialize evil-commentary."
  (use-package evil-commentary
    :defer t
    :init (evil-commentary-mode t)
    :config
    (spacemacs|hide-lighter 'evil-commentary-mode)))
