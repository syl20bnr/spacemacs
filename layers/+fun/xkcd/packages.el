;;; packages.el --- xkcd Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq xkcd-packages '(xkcd))

(defun xkcd/init-xkcd ()
  (use-package xkcd-mode
    :defer t
    :init
    (progn
      (setq xkcd-cache-dir (concat space-macs-cache-directory "xkcd/"))
      (when (not (file-directory-p xkcd-cache-dir))
        (make-directory xkcd-cache-dir))
      (space-macs/set-leader-keys
        "afx" 'xkcd)
      (evilified-state-evilify xkcd-mode xkcd-mode-map
        "h" 'xkcd-prev
        "j" 'xkcd-next
        "k" 'xkcd-prev
        "l" 'xkcd-next))))


