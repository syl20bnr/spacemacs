;;; funcs.el --- docker Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//docker-dockerfile-setup-backend ()
  "Conditionally setup docker backend."
  (pcase docker-dockerfile-backend
    (`lsp (lsp))))


