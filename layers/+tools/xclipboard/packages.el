;;; packages.el --- xclipboard layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;;          Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst xclipboard-packages
  '(
    (space-macs-xclipboard :location local)
    (cliphist :requires ivy
              :toggle xclipboard-enable-cliphist)
    ))

(defun xclipboard/init-cliphist ()
  (use-package cliphist
    :init (space-macs/set-leader-keys
            "xP" 'cliphist-paste-item
            "xR" 'space-macs/xclipboard-cliphist-paste-item-rectangle
            "xs" 'cliphist-select-item)
    :config (setq cliphist-cc-kill-ring t)))

(defun xclipboard/init-space-macs-xclipboard ()
  (use-package space-macs-xclipboard
    :init (space-macs/set-leader-keys
            "xp" 'space-macs/xclipboard-paste
            "xy" 'space-macs/xclipboard-copy)))


