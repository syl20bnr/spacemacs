;;; packages.el --- Template Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq templates-packages
      '(yatemplate))

(defun templates/init-yatemplate ()
  (use-package yatemplate
    :init
    (progn
      (setq yatemplate-dir
            (or templates-private-directory
                (concat configuration-layer-private-directory
                        "templates")))
      (unless templates-use-defaults
        (setq auto-insert-alist nil)))
    :config
    (progn
      (setq yatemplate-dir (concat configuration-layer-private-directory "templates"))
      (yatemplate-fill-alist)
      (auto-insert-mode +1))))
