;;; packages.el --- semantic Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq semantic-packages
      '(
        (semantic :location built-in)
        srefactor
        stickyfunc-enhance
        ))

(defun semantic/init-semantic ()
  (use-package semantic
    :defer t
    :config (add-to-list 'semantic-default-submodes
                         'global-semantic-idle-summary-mode)))

(defun semantic/init-srefactor ()
  (use-package srefactor :defer t))

(defun semantic/pre-init-stickyfunc-enhance ()
  (spacemacs|use-package-add-hook semantic
    :post-init (add-to-list 'semantic-default-submodes
                            'global-semantic-stickyfunc-mode)))

(defun semantic/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (progn
      (spacemacs|add-toggle semantic-stickyfunc
        :mode semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc."
        :evil-leader "TS")
      (spacemacs|add-toggle semantic-stickyfunc-globally
        :mode global-semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc globally."
        :evil-leader "T C-S"))))

(defun spacemacs//disable-semantic-idle-summary-mode ()
  (semantic-idle-summary-mode 0))
