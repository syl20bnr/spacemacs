;;; funcs.el --- eslint-fix layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Holm <holmi09@holmi09mac132>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun eslint-fix-hook ()
  "add an after save hook to run eslint-fix"
  (add-hook 'after-save-hook 'eslint-fix nil t))
