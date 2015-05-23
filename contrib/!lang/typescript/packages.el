;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Chris Bowdon & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-packages '(tss))

(defun typescript/init-tss ()
  "Initialize my package"
  (use-package tss
    :defer t
    :mode ("\\.ts\\'" . typescript-mode)
    :init (evil-leader/set-key-for-mode 'typescript-mode
            "mgg" 'tss-jump-to-definition
            "mhh" 'tss-popup-help)
    :config (tss-config-default)))
