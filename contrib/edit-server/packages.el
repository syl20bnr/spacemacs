;;; packages.el --- edit-server Layer packages File for Spacemacs
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

(setq edit-server-packages '(edit-server))

(setq edit-server-excluded-packages '())

(defun edit-server/init-edit-server ()
  (use-package edit-server
    :config
    (edit-server-start)))
