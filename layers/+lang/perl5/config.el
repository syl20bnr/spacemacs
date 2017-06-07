;;; config.el --- Perl5 Layer Configuration File for Spacemacs
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

(defalias 'perl-mode 'cperl-mode)

(spacemacs|defvar-company-backends cperl-mode)

(spacemacs|define-jump-handlers cperl-mode)
