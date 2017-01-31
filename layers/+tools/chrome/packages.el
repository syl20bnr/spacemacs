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
      (defun start-browser(browser url)
        (let ((process-environment (browse-url-process-environment)))
          (apply 'start-process
                 (concat "flymd" url) nil
                 browser
                 (list "--allow-file-access-from-files" url))))
      (defun my-flymd-browser-function (url)
        (let ((browser-path
               (cond
                ((executable-find "chromium") (executable-find "chromium"))
                ((executable-find "google-chrome") (executable-find "goolge-chrome"))
                ((executable-find "goolge-chrome-stable") (executable-find "goolge-chrome-stable"))
                (chrome-exec-path chrome-exec-path)
                (t (message "no useful browser")))))
          (start-browser browser-path url)))
      (setq flymd-browser-open-function 'my-flymd-browser-function)
      )))
