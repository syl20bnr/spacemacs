;;; packages.el --- sr-speedbar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: hujianxin <hujianxincn@163.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq sr-speedbar-packages
      '(sr-speedbar))
(defun sr-speedbar/init-sr-speedbar ()
  (use-package sr-speedbar
    :init
    (progn
      (setq sr-speedbar-width 30)
      (setq sr-speedbar-auto-refresh t)
      (setq sr-speedbar-right-side nil)
      (evil-leader/set-key
        "afo" 'sr-speedbar-open
        "afc" 'sr-speedbar-close
        "afn" 'sr-speedbar-refresh-turn-on
        "aff" 'sr-speedbar-refresh-turn-off))))
