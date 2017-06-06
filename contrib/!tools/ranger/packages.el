;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Rich Alesi
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ranger-packages
  '(ranger))

(setq ranger-excluded-packages '())

(defun ranger/init-ranger ()
  (use-package ranger
    :defer t
    :init
    (evil-leader/set-key "ar" 'ranger)))
