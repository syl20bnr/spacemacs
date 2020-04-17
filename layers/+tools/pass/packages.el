;;; packages.el --- Passwords Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pass-packages
      '(
        (ivy-pass :requires ivy)
        (helm-pass :requires helm)
        password-store
        ))

(defun pass/init-helm-pass ()
  (use-package helm-pass
    :defer t
    :init (spacemacs/set-leader-keys "aa/" 'helm-pass)))

(defun pass/init-ivy-pass ()
  (use-package ivy-pass
    :defer t
    :init (spacemacs/set-leader-keys "aa/" 'ivy-pass)))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aa" "pass")
      (spacemacs/set-leader-keys
        "aay" 'password-store-copy
        "aag" 'password-store-generate
        "aai" 'password-store-insert
        "aac" 'password-store-edit
        "aar" 'password-store-rename
        "aad" 'password-store-remove
        "aaD" 'password-store-clear
        "aaI" 'password-store-init
        "aaw" 'password-store-url
        "aa?" 'spacemacs/pass-describe
        "aaY" 'spacemacs/pass-copy-and-describe))))
