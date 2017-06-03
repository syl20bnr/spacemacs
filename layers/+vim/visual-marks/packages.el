;;; packages.el --- visual-marks Layer packages File for Spacemacs
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

(setq visual-marks-packages
    '(
      evil-visual-mark-mode
      ))

(setq visual-marks-excluded-packages '())

(defun visual-marks/init-evil-visual-mark-mode ())
