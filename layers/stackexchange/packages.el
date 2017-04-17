;;; packages.el --- stackexchange Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Robert O'Connor & Contributors
;;
;; Author: Robert O'Connor <rob@oconnor.ninja>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq stackexchange-packages
    '(
      sx
      ))
(defun stackexchange/init-sx ()
  (use-package sx
    :defer t))
