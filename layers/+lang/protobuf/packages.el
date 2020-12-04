;;; packages.el --- Protocol Buffers Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Amol Mandhane <https://github.com/amol-mandhane>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst protobuf-packages
  '(
    flycheck
    protobuf-mode
    ))

(defun protobuf/post-init-flycheck ()
  (space-macs/enable-flycheck 'protobuf-mode))

(defun protobuf/init-protobuf-mode ()
  (use-package protobuf-mode
    :init (add-hook 'protobuf-mode-hook 'space-macs//setup-protobuf-imenu)))


