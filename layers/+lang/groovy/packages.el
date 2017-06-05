;;; packages.el --- groovy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Robert O'Connor <robby.oconnor@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq groovy-packages
      '(
        company
      groovy-mode
      emacs-eclim
      ))

(setq groovy-excluded-packages '())

(defun groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :config
    (progn
      (add-hook 'groovy-mode-hook (lambda ()
                  (interactive)
                  (progn
                    (eclim-mode t)))))))
