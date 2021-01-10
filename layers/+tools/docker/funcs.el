;;; funcs.el --- docker Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//docker-dockerfile-setup-backend ()
  "Conditionally setup docker backend."
  (pcase docker-dockerfile-backend
    (`lsp (lsp))))
