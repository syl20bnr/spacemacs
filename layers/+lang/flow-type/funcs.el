;;; funcs.el --- flow-type layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Mike Holm <coldpour@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun flow-type-eldoc-hook ()
  (set (make-local-variable 'eldoc-documentation-function) 'flow-minor-get-type-at-pos))
