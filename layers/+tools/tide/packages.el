;;; packages.el --- Tide Layer packages file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst tide-packages
  '(tide)
  "The list of Lisp packages required by the tide layer.")

(defun tide/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :config
    (space-macs//tide-setup-bindings)
    (add-hook 'tide-mode-hook #'space-macs//tide-setup-jump-handle)))


