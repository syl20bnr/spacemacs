;;; packages.el --- pass Layer packages File for Spacemacs
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

(setq pass-packages
    '(
      (pass :location local)
      ))

;; List of packages to exclude.
(setq pass-excluded-packages '())

(defun pass/init-pass ()
  "Initialize pass"
  (use-package pass))
