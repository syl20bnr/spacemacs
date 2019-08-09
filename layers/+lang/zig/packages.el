;;; packages.el --- zig layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <mijoharas@archlinux>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst zig-packages
  '(
    zig-mode
    )
  "The list of Lisp packages required by the zig layer.")

(defun zig/init-zig-mode ()
  (use-package zig-mode
    :defer t
    :init
    (progn
      ;; config goes here.
      )))

;;; packages.el ends here
