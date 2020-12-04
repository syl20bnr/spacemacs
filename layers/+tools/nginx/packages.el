;;; packages.el --- nginx layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Nathaniel Waisbrot <code@waisbrot.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst nginx-packages '(nginx-mode))

(defun nginx/init-nginx-mode ()
  (use-package nginx-mode :defer t))


