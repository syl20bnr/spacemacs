;;; packages.el --- prettier Layer packages file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq prettier-packages '(prettier-js))

(defun prettier/init-prettier-js ()
  (use-package prettier-js
    :commands prettier-js
    :init
    (dolist (mode space-macs--prettier-modes)
      (space-macs/set-leader-keys-for-major-mode mode
        "==" #'prettier-js))))


