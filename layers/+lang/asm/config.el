;;; config.el --- Asm Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables
(spacemacs|defvar-company-backends asm-mode)
(spacemacs|defvar-company-backends nasm-mode)

(spacemacs|define-jump-handlers asm-mode)
