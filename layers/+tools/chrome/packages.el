;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    :init
    (progn
      (edit-server-start))
    :config
    (progn
      (setq edit-server-default-major-mode 'markdown-mode))
    ))

(defun chrome/init-gmail-message-mode ( )
  (use-package gmail-message-mode))

(defun chrome/init-flymd ()
  (use-package flymd
    :defer t
    :init
    (progn
      (defun my-flymd-browser-function (url)
        (let ((process-environment (browse-url-process-environment)))
          (apply 'start-process
                 (concat "google-chrome " url) nil
                 chrome-exec-path
                 (list "--allow-file-access-from-files" url))))
      (setq flymd-browser-open-function 'my-flymd-browser-function)
      )))
