;;; packages.el --- sr-speedbar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: wsk <tshemeng@live.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq sr-speedbar-packages
  '(sr-speedbar))

(defun sr-speedbar/post-init-sr-speedbar ()
  (defun spacemacs/sr-speedbar-show-or-hide ()
    (interactive)
    (cond ((sr-speedbar-exist-p) (kill-buffer speedbar-buffer))
          (t (sr-speedbar-open) (linum-mode -1) (speedbar-refresh)))))

(defun sr-speedbar/init-sr-speedbar ()
  (use-package sr-speedbar
    :init
    (setq sr-speedbar-width 30)
    (setq sr-speedbar-right-side nil)))
