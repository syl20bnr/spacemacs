;;; keybindings.el --- Vinegar Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key evil-normal-state-map (kbd "-") 'dired-jump)

(add-hook 'dired-mode-hook 'vinegar/dired-setup)
