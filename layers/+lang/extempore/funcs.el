;;; funcs.el --- Extempore Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//extempore-setup-eldoc ()
  (setq-local eldoc-documentation-function 'extempore-eldoc-documentation-function)
  (eldoc-mode))


