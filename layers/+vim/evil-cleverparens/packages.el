;;; packages.el --- evil-cleverparens Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq evil-cleverparens-packages
      '(evil-cleverparens))

(defun evil-cleverparens/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t
    :diminish evil-cleverparens-mode
    :init
    (progn
      (setq evil-cleverparens-use-regular-insert t)
      (spacemacs|add-toggle evil-cleverparens
        :mode evil-cleverparens-mode
        :documentation "Enable evil-cleverparens."))))
