;;; config.el --- Shell Scripts Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables
(spacemacs|defvar-company-backends sh-mode)
(spacemacs|defvar-company-backends fish-mode)

(spacemacs|define-jump-handlers sh-mode)
