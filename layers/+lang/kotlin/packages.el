;;; packages.el --- Kotlin Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Laurent Pireyn <laurent.pireyn@pisolutions.eu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq kotlin-packages
      '(
        kotlin-mode
        ))

(defun kotlin/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t))
