;;; packages.el --- org-present Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defvar org-present-packages
  '(
    org-present
    ))

;; List of packages to exclude.
(defvar org-present-excluded-packages '())

;; For each package, define a function org-present/init-<package-org-present>
;;
;; (defun org-present/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun org-present/init-org-present ()
  "Initialize org-present package"
  (use-package org-present
    :defer t))
