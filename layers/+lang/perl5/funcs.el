;;; funcs.el --- Perl5 Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun spacemacs//perl5-smartparens-enable ()
  (define-key cperl-mode-map "{" nil))

(defun spacemacs//perl5-spartparens-disable ()
  (define-key cperl-mode-map "{" 'cperl-electric-lbrace))
