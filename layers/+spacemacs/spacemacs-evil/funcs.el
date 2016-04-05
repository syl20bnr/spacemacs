;;; packages.el --- Spacemacs Evil Layer Functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//set-evil-iedit-default-state (style)
  (setq evil-iedit-state-default-state
        (if (eq 'emacs style) 'emacs 'normal)))
