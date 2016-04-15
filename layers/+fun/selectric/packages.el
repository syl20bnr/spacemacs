;;; packages.el --- selectric Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Gergely Nagy
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq selectric-packages '(selectric-mode))

(defun selectric/init-selectric-mode ()
  (use-package selectric-mode
    ;; at the moment of adding this layer, selectric-mode is not
    ;; autoloaded.
    :commands selectric-mode
    :init (spacemacs/set-leader-keys "C-t t" 'selectric-mode)
    :config (spacemacs|diminish selectric-mode "♬" "Tw")))
