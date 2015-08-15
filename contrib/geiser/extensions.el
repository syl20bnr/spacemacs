;;; extensions.el --- geiser Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Joseph Cecil & Contributors
;;
;; Author: Joseph Cecil <joe.a.cecil@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq geiser-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq geiser-post-extensions
      '(
        ;; post extension names go here
        ))

;; For each extension, define a function geiser/init-<extension-name>
;;
;; (defun geiser/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
