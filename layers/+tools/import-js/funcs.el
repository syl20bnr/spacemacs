;;; funcs.el --- import-js Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/import-js-fix ()
  (interactive)
  (import-js-fix)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

(defun spacemacs/import-js-import ()
  (interactive)
  (import-js-import)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
