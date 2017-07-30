;;; packages.el --- snoopy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Tim Jäger <jger.tm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst snoopy-packages
  '(snoopy))

(defun snoopy/init-snoopy ()
  (use-package snoopy-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'snoopy-mode))))
