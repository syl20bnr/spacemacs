;;; packages.el --- ibuffer Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Aleksandr Guljajev & Contributors
;;
;; Author: Sylvain Benner <aleksandr.guljajev@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defvar ibuffer-packages
  '(
    cl
    ibuffer
    ))

;; List of packages to exclude.
(defvar ibuffer-excluded-packages '())

;; For each package, define a function ibuffer/init-<package-ibuffer>
;;
(defun ibuffer/init-ibuffer()
  (use-package ibuffer
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "B" 'ibuffer)
      (if ibuffer-group-by-major-mode
          (add-hook 'ibuffer-hook 'ibuffer/create-buffs-group))
      ))
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
