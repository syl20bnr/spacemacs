;;; funcs.el --- Dash Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (when (not (string-blank-p path))
    (setq dash-docs-docsets-path (expand-file-name path)))
  (setq dash-docs-common-docsets (dash-docs-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length dash-docs-common-docsets) path)))
