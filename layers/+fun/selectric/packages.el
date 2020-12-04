;;; packages.el --- selectric Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Gergely Nagy
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq selectric-packages '(selectric-mode))

(defun selectric/init-selectric-mode ()
  (use-package selectric-mode
    ;; at the moment of adding this layer, selectric-mode is not
    ;; autoloaded.
    :commands selectric-mode
    :init (space-macs/set-leader-keys "C-t t" 'selectric-mode)
    :config (space-macs|diminish selectric-mode "â™¬" "Tw")))


