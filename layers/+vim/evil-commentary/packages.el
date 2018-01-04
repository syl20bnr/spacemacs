;;; packages.el --- evil-commentary Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
      (spacemacs/set-leader-keys ";" 'evil-commentary))
    :config (spacemacs|hide-lighter evil-commentary-mode)))
