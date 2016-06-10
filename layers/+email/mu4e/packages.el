;;; packages.el --- mu4e Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mu4e-packages
      '(
        (mu4e :location site)
        mu4e-alert
        mu4e-maildirs-extension
        (helm-mu :requires helm)
        org
        persp-mode
        evil-mu4e
        ))

(defun mu4e/post-init-persp-mode ()
  (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
    :binding mu4e-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           mu4e-spacemacs-layout-name)))
      (spacemacs/add-to-hooks 'spacemacs-layouts/add-mu4e-buffer-to-persp
                       '(mu4e-main-mode-hook
                         mu4e-headers-mode-hook
                         mu4e-view-mode-hook
                         mu4e-compose-mode-hook))
      (call-interactively 'mu4e)
      (call-interactively 'mu4e-update-index))))

(defun mu4e/init-mu4e ()
  (use-package mu4e
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (spacemacs/set-leader-keys "a M" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new)
      (setq mu4e-completing-read-function 'completing-read
            mu4e-use-fancy-chars 't
            mu4e-view-show-images 't
            message-kill-buffer-on-exit 't)
      (let ((dir "~/Downloads"))
        (when (file-directory-p dir)
          (setq mu4e-attachment-dir dir))))

    :config
    (progn
      (when mu4e-enable-async-operations
        (require 'smtpmail-async)
        (setq send-mail-function         'async-smtpmail-send-it
              message-send-mail-function 'async-smtpmail-send-it))

      (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))

      (add-to-list 'mu4e-view-actions
                   '("View in browser" . mu4e-action-view-in-browser) t)

      (add-hook 'mu4e-compose-mode-hook
                (lambda () (use-hard-newlines t 'guess))))))

(defun mu4e/init-mu4e-alert ()
  (use-package mu4e-alert
    :defer t
    :init (with-eval-after-load 'mu4e
            (when mu4e-enable-notifications
              (mu4e-alert-enable-notifications))
            (when mu4e-enable-mode-line
              (mu4e-alert-enable-mode-line-display)))))

(defun mu4e/init-helm-mu ()
  (use-package helm-mu
    :defer t
    :init (dolist (m '(mu4e-main-mode-hook
                       mu4e-headers-mode-hook
                       mu4e-view-mode-hook
                       mu4e-compose-mode-hook))
            (spacemacs/set-leader-keys-for-major-mode m
              "S" 'helm-mu
              "/" 'helm-mu
              "C" 'helm-mu-contacts))))

(defun mu4e/init-mu4e-maildirs-extension ()
  "If mu4e-use-maildirs-extension is non-nil, set
mu4e-use-maildirs-extension-load to be evaluated after mu4e has been loaded."
  (use-package mu4e-maildirs-extension
    :if mu4e-use-maildirs-extension
    :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load))))

(defun mu4e/pre-init-org ()
  ;; load org-mu4e when org is actually loaded
  (with-eval-after-load 'org (require 'org-mu4e nil 'noerror)))

(defun mu4e/init-evil-mu4e()
  (use-package evil-mu4e
    :after mu4e))
