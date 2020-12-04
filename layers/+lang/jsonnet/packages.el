;;; packages.el --- jsonnet layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Liz <liz@kazkaan>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq jsonnet-packages
      '(
        jsonnet-mode
        flycheck
        ))

(defun jsonnet/post-init-flycheck ()
  (space-macs/enable-flycheck 'jsonnet-mode))

(defun jsonnet/init-jsonnet-mode ()
  (use-package jsonnet-mode
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys-for-major-mode 'jsonnet-mode
        "=" 'jsonnet-reformat-buffer
        "gg" 'jsonnet-jump
        "eb" 'jsonnet-eval-buffer))))


