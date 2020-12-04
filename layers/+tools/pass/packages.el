;;; packages.el --- Passwords Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq pass-packages
      '(
        (ivy-pass :requires ivy)
        (helm-pass :requires helm)
        auth-source-pass
        password-store
        password-store-otp
        ))

(defun pass/init-helm-pass ()
  (use-package helm-pass
    :defer t
    :init (space-macs/set-leader-keys "atP/" 'helm-pass)))

(defun pass/init-ivy-pass ()
  (use-package ivy-pass
    :defer t
    :init (space-macs/set-leader-keys "atP/" 'ivy-pass)))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (progn
      (space-macs/declare-prefix "atP" "pass")
      (space-macs/set-leader-keys
        "atPy" 'password-store-copy
        "atPg" 'password-store-generate
        "atPi" 'password-store-insert
        "atPc" 'password-store-edit
        "atPr" 'password-store-rename
        "atPd" 'password-store-remove
        "atPD" 'password-store-clear
        "atPI" 'password-store-init
        "atPw" 'password-store-url
        "atP?" 'space-macs/pass-describe
        "atPY" 'space-macs/pass-copy-and-describe))))

(defun pass/init-password-store-otp ()
  (use-package password-store-otp
    :defer t
    :init
    (progn
      (space-macs/declare-prefix "atPo" "otp")
      (space-macs/set-leader-keys
        "atPoy" 'password-store-otp-token-copy
        "atPoY" 'password-store-otp-uri-copy
        "atPoi" 'password-store-otp-insert
        "atPoa" 'password-store-otp-append
        "atPoA" 'password-store-otp-append-from-image))))

(defun pass/init-auth-source-pass ()
  (use-package auth-source-pass
    :after auth-source
    :config
    (auth-source-pass-enable)))


