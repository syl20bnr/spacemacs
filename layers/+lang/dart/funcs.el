;;; funcs.el --- Dart Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend

(defun space-macs//dart-setup-backend ()
  (pcase dart-backend
    (`analyzer (dart-server))
    (`lsp (lsp))))


