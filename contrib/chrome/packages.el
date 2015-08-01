;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
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
