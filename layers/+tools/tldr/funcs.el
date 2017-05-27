;;; funcs.el --- Dash Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun tldr-at-point ()
  "Lookup thing at point in tldr pages."
  (interactive)
  (let ((tldr-use-word-at-point t)) (tldr)))
