;;; packages.el --- evil-commentary Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq evil-commentary-packages
      '(evil-commentary
        (evil-nerd-commenter :excluded t)
        ))

(defun evil-commentary/init-evil-commentary ()
  (use-package evil-commentary
    :init
    (progn
      (evil-commentary-mode)
      (space-macs/set-leader-keys ";" 'evil-commentary))
    :config (space-macs|hide-lighter evil-commentary-mode)))


