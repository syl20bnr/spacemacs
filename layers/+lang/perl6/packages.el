;;; packages.el --- perl6 layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  Bahtiar `kalkin-`''Gadimov <bahtiar@gadimov.de>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst perl6-packages
  '(
    company
    evil
    flycheck
    (flycheck-perl6 :requires flycheck)
    raku-mode
    ))

(defun perl6/post-init-company ()
  (space-macs|add-company-backends
    :backends company-capf
    :modes perl6-mode))

(defun perl6/post-init-evil ()
  (add-to-list 'space-macs-jump-handlers-perl6-mode 'evil-jump-to-tag))

(defun perl6/post-init-flycheck ()
  (space-macs/enable-flycheck 'perl6-mode))

(defun perl6/init-flycheck-perl6 ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-perl6)))

(defun perl6/init-raku-mode()
  (use-package raku-mode
    :defer t
    :mode (("/perl6/site/sources/" . perl6-mode))))


