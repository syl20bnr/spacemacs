;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq chrome-packages '(
                        edit-server
                        gmail-message-mode
                        flymd
                        ))

(defun chrome/init-edit-server ()
  (use-package edit-server
    :init (edit-server-start)
    :config (setq edit-server-default-major-mode 'markdown-mode)))

(defun chrome/init-gmail-message-mode ()
  (use-package gmail-message-mode
    :defer t))

(defun chrome/init-flymd ()
  (use-package flymd
    :defer t
    :init (setq flymd-browser-open-function
                'spacemacs//flymd-browser-function)))
