;;; packages.el --- jsonnet layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Liz <liz@kazkaan>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq jsonnet-packages
      '(
        jsonnet-mode
        flycheck
        ))

(defun jsonnet/post-init-flycheck ()
  (spacemacs/enable-flycheck 'jsonnet-mode))

(defun jsonnet/init-jsonnet-mode ()
  (use-package jsonnet-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'jsonnet-mode
        "=" 'jsonnet-reformat-buffer
        "gg" 'jsonnet-jump
        "eb" 'jsonnet-eval-buffer))))
