;;; packages.el --- perl6 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  Bahtiar `kalkin-`''Gadimov <bahtiar@gadimov.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst perl6-packages
  '(
    company
    evil
    flycheck
    (flycheck-perl6 :requires flycheck)
    ;; Not available in MELPA for now
    ;; TODO check progress on issue: https://github.com/melpa/melpa/issues/5261
    (perl6-mode :location (recipe :fetcher github
                                  :repo "perl6/perl6-mode"))
    ))

(defun perl6/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes perl6-mode))

(defun perl6/post-init-evil ()
  (add-to-list 'spacemacs-jump-handlers-perl6-mode 'evil-jump-to-tag))

(defun perl6/post-init-flycheck ()
  (spacemacs/enable-flycheck 'perl6-mode))

(defun perl6/init-flycheck-perl6 ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-perl6)))

(defun perl6/init-perl6-mode()
  (use-package perl6-mode
    :defer t
    :mode (("/perl6/site/sources/" . perl6-mode))))
