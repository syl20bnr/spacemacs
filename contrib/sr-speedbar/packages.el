;;; packages.el --- sr-speedbar Layer packages File for Spacemacs
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
(setq sr-speedbar-packages
    '(
      ;; package sr-speedbars go here
      sr-speedbar
      ))

;; List of packages to exclude.
(setq sr-speedbar-excluded-packages '())

;; For each package, define a function sr-speedbar/init-<package-sr-speedbar>
;;
(defun sr-speedbar/init-sr-speedbar ()
  (use-package sr-speedbar
    :init
    (setq sr-speedbar-width 30)
    (setq sr-speedbar-right-side nil)
    (define-key global-map (kbd "<f2>") 'sr-speedbar-show-or-hide)))

(defun sr-speedbar/post-init-sr-speedbar ()
  (defun sr-speedbar-show-or-hide ()
    (interactive)
    (cond ((sr-speedbar-exist-p) (kill-buffer speedbar-buffer))
          (t (sr-speedbar-open) (speedbar-refresh)))))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
