;;; packages.el --- semantic Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
  (space-macs|use-package-add-hook semantic
    :post-init (add-to-list 'semantic-default-submodes
                            'global-semantic-stickyfunc-mode)))

(defun semantic/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (progn
      (space-macs|add-toggle semantic-stickyfunc
        :mode semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc."
        :evil-leader "TS")
      (space-macs|add-toggle semantic-stickyfunc-globally
        :mode global-semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc globally."
        :evil-leader "T C-S"))))

(defun space-macs//disable-semantic-idle-summary-mode ()
  (semantic-idle-summary-mode 0))


