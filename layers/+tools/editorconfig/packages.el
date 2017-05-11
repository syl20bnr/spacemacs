;;; packages.el --- editorconfig Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Vladimir Kochnev <hashtable@yandex.ru>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq editorconfig-packages '(editorconfig))

(defun editorconfig/init-editorconfig ()
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode t)
    :init
    (progn
      (spacemacs|diminish editorconfig-mode " Ⓔ" " E"))))
