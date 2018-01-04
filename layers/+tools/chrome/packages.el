;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
                        markdown-mode
                        ))

(defun chrome/init-edit-server ()
  (use-package edit-server
    :init (edit-server-start)
    :config (setq edit-server-default-major-mode 'markdown-mode)))

(defun chrome/init-gmail-message-mode ()
  (use-package gmail-message-mode
    :defer t
    :config
    (when (configuration-layer/layer-used-p 'markdown)
      (spacemacs/set-markdown-keybindings
       'gmail-message-client-mode gmail-message-client-mode-map))))

(defun chrome/init-flymd ()
  (use-package flymd
    :defer t
    :init (setq flymd-browser-open-function
                'spacemacs//flymd-browser-function)))

(defun chrome/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :pre-config
    (when (configuration-layer/package-used-p 'gmail-message-mode)
      (add-to-list 'markdown--key-bindings-modes 'gmail-message-client-mode))))
