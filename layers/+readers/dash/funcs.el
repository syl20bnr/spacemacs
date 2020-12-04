;;; funcs.el --- Dash Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (when (not (string-blank-p path))
    (setq dash-docs-docsets-path (expand-file-name path)))
  (setq dash-docs-common-docsets (dash-docs-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length dash-docs-common-docsets) path)))


