;;; packages.el --- seeing-is-believing Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Brandon Conway & Contributors
;;
;; Author: Brandon Conway <brandoncc@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq seeing-is-believing-packages
    '(
       seeing-is-believing
      ))


(defun seeing-is-believing/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :defer t
    :init
    (progn
      (require 'seeing-is-believing)
      (add-hook 'ruby-mode-hook 'seeing-is-believing))
    :config
    (progn
      (evil-leader/set-key-for-mode 'ruby-mode "msb" 'seeing-is-believing-run)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msb" 'seeing-is-believing-run)
      (evil-leader/set-key-for-mode 'ruby-mode "msc" 'seeing-is-believing-clear)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msc" 'seeing-is-believing-clear))
    )
  )
