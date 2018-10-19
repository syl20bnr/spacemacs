;;; packages.el --- clipboard layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Authors: Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst clipboard-packages
  '(
    (cliphist :requires ivy)
    ))

(defun clipboard/init-cliphist ()
  (use-package cliphist
    :init (spacemacs/set-leader-keys
            "xP" 'cliphist-paste-item
            "xR" (defun spacemacs/cliphist-paste-item-rectangle ()
                   (interactive)
                   (cliphist-paste-item 1))
            "xs" 'cliphist-select-item)
    :config (setq cliphist-cc-kill-ring t)))
