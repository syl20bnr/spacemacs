;;; packages.el --- web-beautify Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq web-beautify-packages '(web-beautify))

(defun web-beautify/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (dolist (x space-macs--web-beautify-modes)
      (space-macs/set-leader-keys-for-major-mode (car x) "==" (cdr x)))))


