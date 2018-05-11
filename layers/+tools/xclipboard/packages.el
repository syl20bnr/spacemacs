;;; packages.el --- xclipboard layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst xclipboard-packages '())

(defun xclipboard/init-xclipboard ()
  (use-package xclipboard))
