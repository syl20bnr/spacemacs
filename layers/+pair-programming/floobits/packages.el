;;; packages.el --- floobits Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Rodolfo Hansen <rhansen@kitsd.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
      (spacemacs/declare-prefix "P" "PP/floobits")

      (defun spacemacs/floobits-rclocation ()
        "Return the absolute path to the floobits dotfile."
        (concat user-home-directory ".floorc.json"))

      (defun spacemacs/floobits-load-rcfile ()
        "Load ~/.floobitsrc if it exists."
        (let ((floobitsrc (spacemacs/floobits-rclocation)))
          (if (file-exists-p floobitsrc) (load floobitsrc))))

      (spacemacs/set-leader-keys
        "Pc" 'floobits-clear-highlights
        "Pd" 'spacemacs/floobits-load-rcfile
        "Pf" 'floobits-follow-user
        "Pj" 'floobits-join-workspace
        "Pl" 'floobits-leave-workspace
        "PR" 'floobits-share-dir-private
        "Ps" 'floobits-summon
        "Pt" 'floobits-follow-mode-toggle
        "PU" 'floobits-share-dir-public))))
