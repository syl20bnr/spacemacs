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
  (if (not (string-blank-p path))
      (setq helm-dash-docsets-path path))
  (setq helm-dash-common-docsets (helm-dash-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length helm-dash-common-docsets) path)))

(defun counsel-dash-at-point ()
  "Counsel dash with selected point"
  (interactive)
  (counsel-dash
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (substring-no-properties (or (thing-at-point 'symbol) "")))))
