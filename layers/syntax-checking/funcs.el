;;; funcs.el --- Syntax Checking Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/add-flycheck-hook (hook)
  "Add flycheck to the given HOOK, if
`syntax-checking-enable-by-default' is true."
  (when syntax-checking-enable-by-default
    (add-hook hook 'flycheck-mode)))
