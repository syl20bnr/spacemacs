;;; funcs.el --- Syntax Checking Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/add-flycheck-hook (mode &optional target)
  "Enable flycheck for the given MODE, if
`syntax-checking-enable-by-default' is true."
  (when syntax-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook 'flycheck-mode))))
