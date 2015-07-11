;;; packages.el --- Scheme Layer packages File for Spacemacs
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

(setq scheme-packages
      '(geiser))

(defun scheme/post-init-company ()
  ;; Geiser provides completion as long as company mode is loaded.
  (add-hook 'scheme-mode-hook 'company-mode))

(defun scheme/init-geiser ()
  (use-package geiser
    :commands run-geiser
    :config
    (progn
        )
    ))
