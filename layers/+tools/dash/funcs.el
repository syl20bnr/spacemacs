;;; funcs.el --- Dash Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (setq helm-dash-docsets-path path
        helm-dash-common-docsets (helm-dash-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length helm-dash-common-docsets) path)))
