;;; funcs.el --- xclipboard layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/xclipboard-cliphist-paste-item-rectangle ()
  (interactive)
  (cliphist-paste-item 1))
