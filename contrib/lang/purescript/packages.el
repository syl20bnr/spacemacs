;;; packages.el --- Purescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Ryan L. Bell & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defvar purescript-packages
  '(
    purescript-mode
    psci))

(defun purescript/init-purescript-mode ()
  (use-package purescript-mode
    :defer t
    :config
    (add-hook 'purescript-mode-hook
                      (lambda () (turn-on-purescript-indentation)))))

(defun purescript/init-psci ()
  (use-package psci
    :defer t
    :init
    (add-hook 'purescript-mode-hook 'inferior-psci-mode)))
