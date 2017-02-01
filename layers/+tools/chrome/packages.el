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
                 "flymd" nil
                 browser
                 (list "--new-window" "--allow-file-access-from-files" url))))

      (defun my-flymd-browser-function (url)
               (cond
                (chrome-exec-path (start-browser chrome-exec-path url))
                ((executable-find "chromium") (start-browser (executable-find "chromium") url))
                ((executable-find "google-chrome") (start-browser (executable-find "google-chrome") url))
                ((executable-find "google-chrome-stable") (start-browser (executable-find "google-chrome-stable") url))
                (t (message "no useful browser"))))

      (setq flymd-browser-open-function 'my-flymd-browser-function)
      )))
