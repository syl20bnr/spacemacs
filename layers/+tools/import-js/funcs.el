;;; funcs.el --- import-js Layer packages file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/import-js-fix ()
  (interactive)
  (import-js-fix)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

(defun space-macs/import-js-import ()
  (interactive)
  (import-js-import)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))


