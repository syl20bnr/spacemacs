;;; packages.el --- Nim Configuration Layer for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|defvar-company-backends nim-mode)
(spacemacs|defvar-company-backends nimscript-mode)

(spacemacs|define-jump-handlers nim-mode)
