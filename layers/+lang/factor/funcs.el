;;; funcs.el --- Factor Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: timor <timor.dd@googlemail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun factor//fuel-stack-effect ()
  "Small wrapper around factors stack effect help. If region is
  active, use that, otherwise use sexp under point."
  (interactive)
  (if (region-active-p)
      (call-interactively 'fuel-stack-effect-region)
    (call-interactively 'fuel-stack-effect-sexp)))
