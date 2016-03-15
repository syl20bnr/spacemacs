;;; packages.el --- semantic Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    :config
    (progn
      (add-to-list 'semantic-default-submodes
                   'global-semantic-stickyfunc-mode)
      (add-to-list 'semantic-default-submodes
                   'global-semantic-idle-summary-mode))))

(defun semantic/init-srefactor ()
  (use-package srefactor
    :defer t
    :init
    (progn
      (defun spacemacs/lazy-load-srefactor ()
        "Lazy load the package."
        (require 'srefactor)
        ;; currently, evil-mode overrides key mapping of srefactor menu
        ;; must expplicity enable evil-emacs-state. This is ok since
        ;; srefactor supports j,k,/ and ? commands when Evil is
        ;; available
        (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state)))))

(defun semantic/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (defun spacemacs/lazy-load-stickyfunc-enhance ()
      "Lazy load the package."
      (require 'stickyfunc-enhance))))
