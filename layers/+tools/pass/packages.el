;;; packages.el --- Passwords Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pass-packages '(password-store))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "Ap" "pass")
      (evil-leader/set-key
        "Apy" 'password-store-copy
        "Apg" 'password-store-generate
        "Api" 'password-store-insert
        "Apc" 'password-store-edit
        "Apr" 'password-store-rename
        "Apd" 'password-store-remove
        "ApD" 'password-store-clear
        "ApI" 'password-store-init
        "Apw" 'password-store-url
        "Ap?" 'spacemacs/pass-describe
        "ApY" 'spacemacs/pass-copy-and-describe))))
