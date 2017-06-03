;;; packages.el --- replace-text Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq replace-text-packages
    '(visual-regexp-steroids))

(defun replace-text/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init
    (spacemacs/declare-prefix "xr" "replace")
    (spacemacs/set-leader-keys
      "xrr" 'vr/replace
      "xrq" 'vr/query-replace)))
