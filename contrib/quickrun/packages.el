;;; packages.el --- Finance Layer packages File for Spacemacs
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

(defvar quickrun-packages
  '(
    quickrun
    )
  )

(defun quickrun/init-quickrun ()
  (use-package quickrun
    :init
    (progn
      (require 'quickrun)
      (evil-leader/set-key "aq"   'quickrun))))
