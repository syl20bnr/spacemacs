;;; packages.el --- floobits Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Rodolfo Hansen <rhansen@kitsd.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq floobits-packages
  '(
    floobits
    ))

(defun floobits/init-floobits ()
  (use-package floobits
    :defer t
    :init
    (progn
      (space-macs/declare-prefix "P" "PP/floobits")

      (defun space-macs/floobits-rclocation ()
        "Return the absolute path to the floobits dotfile."
        (concat user-home-directory ".floorc.json"))

      (defun space-macs/floobits-load-rcfile ()
        "Load ~/.floobitsrc if it exists."
        (let ((floobitsrc (space-macs/floobits-rclocation)))
          (if (file-exists-p floobitsrc) (load floobitsrc))))

      (space-macs/set-leader-keys
        "Pc" 'floobits-clear-highlights
        "Pd" 'space-macs/floobits-load-rcfile
        "Pf" 'floobits-follow-user
        "Pj" 'floobits-join-workspace
        "Pl" 'floobits-leave-workspace
        "PR" 'floobits-share-dir-private
        "Ps" 'floobits-summon
        "Pt" 'floobits-follow-mode-toggle
        "PU" 'floobits-share-dir-public))))


