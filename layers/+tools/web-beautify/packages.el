;;; packages.el --- web-beautify Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq web-beautify-packages '(web-beautify))

(defun web-beautify/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (dolist (x spacemacs--web-beautify-modes)
      (spacemacs/set-leader-keys-for-major-mode (car x) "==" (cdr x)))))
