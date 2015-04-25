;;; packages.el --- Perforce Layer packages File for Spacemacs
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

(setq perforce-packages '(p4))

(defun perforce/init-p4 ()
  (use-package p4
    :commands (p4-add
               p4-delete
               p4-describe
               p4-edit
               p4-revert)
    :init
    (evil-leader/set-key
      "p4a" 'p4-add
      "p4d" 'p4-delete
      "p4D" 'p4-describe
      "p4e" 'p4-edit
      "p4R" 'p4-revert
      "p4r" 'p4-rename
      "p4S" 'p4-submit)))
