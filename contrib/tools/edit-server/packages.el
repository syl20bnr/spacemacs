;;; packages.el --- edit-server Layer packages File for Spacemacs
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

(setq edit-server-packages '(edit-server))

(defun edit-server/init-edit-server ()
  (use-package edit-server
    :init (eval-after-load 'edit-server
            '(edit-server-start))))
