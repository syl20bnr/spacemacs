;;; packages.el --- Passwords Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst pass-packages
  '(
    (ivy-pass :requires ivy)
    (helm-pass :requires helm)
    auth-source-pass
    password-store
    password-store-otp))

(defun pass/init-helm-pass ()
  (use-package helm-pass
    :defer t
    :init (spacemacs/set-leader-keys "atP/" 'helm-pass)))

(defun pass/init-ivy-pass ()
  (use-package ivy-pass
    :defer t
    :init (spacemacs/set-leader-keys "atP/" 'ivy-pass)))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (spacemacs/declare-prefix "atP" "pass")
    (spacemacs/set-leader-keys
      "atPy" 'password-store-copy
      "atPg" 'password-store-generate
      "atPi" 'password-store-insert
      "atPc" 'password-store-edit
      "atPr" 'password-store-rename
      "atPd" 'password-store-remove
      "atPD" 'password-store-clear
      "atPI" 'password-store-init
      "atPw" 'password-store-url
      "atP?" 'spacemacs/pass-describe
      "atPY" 'spacemacs/pass-copy-and-describe)))

(defun pass/init-password-store-otp ()
  (use-package password-store-otp
    :defer t
    :init
    (spacemacs/declare-prefix "atPo" "otp")
    (spacemacs/set-leader-keys
      "atPoy" 'password-store-otp-token-copy
      "atPoY" 'password-store-otp-uri-copy
      "atPoi" 'password-store-otp-insert
      "atPoa" 'password-store-otp-append
      "atPoA" 'password-store-otp-append-from-image)))

(defun pass/init-auth-source-pass ()
  (use-package auth-source-pass
    :after auth-source
    :config
    (auth-source-pass-enable)))
