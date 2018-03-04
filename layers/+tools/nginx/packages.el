;;; packages.el --- nginx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Nathaniel Waisbrot <code@waisbrot.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst nginx-packages '(nginx-mode))

(defun nginx/init-nginx-mode ()
  (use-package nginx-mode :defer t))
