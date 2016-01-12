;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    :init (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
            "gg" 'tss-jump-to-definition
            "hh" 'tss-popup-help)
    :config (tss-config-default)))
