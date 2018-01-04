;;; packages.el --- JR Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst jr-packages
  '(
    (jr-mode :location local)
    ))

(defun jr/init-jr-mode ()
  (use-package jr-mode
    :commands jr-mode
    :mode "\\.jr\\'"))
