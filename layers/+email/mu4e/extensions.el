;;; extensions.el --- mu4e Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mu4e-pre-extensions
      '(mu4e))

(setq mu4e-post-extensions
      '())

(defvar mu4e-account-alist nil
  "Account alist for custom multi-account compose.")

(defun mu4e/init-mu4e ()
  (use-package mu4e
    :init
    (progn
      (evil-leader/set-key "a M" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new))
    :config
    (progn
      (spacemacs|evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "j") 'mu4e~headers-jump-to-maildir)
      (spacemacs|evilify-map mu4e-headers-mode-map :mode mu4e-headers-mode)
      (spacemacs|evilify-map mu4e-view-mode-map :mode mu4e-view-mode)

      (setq mu4e-completing-read-function 'helm--completing-read-default)

      (add-to-list 'mu4e-view-actions
                   '("View in browser" . mu4e-action-view-in-browser) t)
      (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account)
      (add-hook 'message-sent-hook 'mu4e/mail-account-reset))))
