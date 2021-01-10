;;; packages.el --- xclipboard layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;;          Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst xclipboard-packages
  '(
    (spacemacs-xclipboard :location local)
    (cliphist :requires ivy
              :toggle xclipboard-enable-cliphist)
    ))

(defun xclipboard/init-cliphist ()
  (use-package cliphist
    :init (spacemacs/set-leader-keys
            "xP" 'cliphist-paste-item
            "xR" 'spacemacs/xclipboard-cliphist-paste-item-rectangle
            "xs" 'cliphist-select-item)
    :config (setq cliphist-cc-kill-ring t)))

(defun xclipboard/init-spacemacs-xclipboard ()
  (use-package spacemacs-xclipboard
    :init (spacemacs/set-leader-keys
            "xp" 'spacemacs/xclipboard-paste
            "xy" 'spacemacs/xclipboard-copy)))
