;;; packages.el --- mu4e Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mu4e-packages
      '((mu4e :location built-in)))

(defun mu4e/init-mu4e ()
  (use-package mu4e
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (spacemacs/set-leader-keys "a M" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new))
    :config
    (progn
      (evilified-state-evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "j") 'mu4e~headers-jump-to-maildir)
      (evilified-state-evilify-map mu4e-headers-mode-map
        :mode mu4e-headers-mode)
      (evilified-state-evilify-map mu4e-view-mode-map
        :mode mu4e-view-mode)

      (setq mu4e-completing-read-function 'helm--completing-read-default)

      (add-to-list 'mu4e-view-actions
                   '("View in browser" . mu4e-action-view-in-browser) t)

      (when mu4e-account-alist
        (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account)
        (add-hook 'message-sent-hook 'mu4e/mail-account-reset)))))
