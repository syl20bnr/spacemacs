;;; packages.el --- floobits Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Rodolfo Hansen & Contributors
;;
;; Author: Rodolfo Hansen <rhansen@kitsd.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar floobits-packages
  '(
    floobits
    )
  "List of all packages to instal and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun floobits/rclocation ()
  "Return the absolute path to the floobits dotfile."
  (concat user-home-directory ".floorc.json"))


(defun floobits/load-rcfile ()
  "Load ~/.floobitsrc if it exists."
  (let ((floobitsrc (floobits/rclocation)))
    (if (file-exists-p floobitsrc) (load floobitsrc))))

(defun floobits/init-floobits ()
  (use-package floobits
    :defer t
    :init
    (evil-leader/set-key
      "Pc" 'floobits-clear-highlights
      "Pd" 'floobits/load-rcfile
      "Pf" 'floobits-follow-user
      "Pj" 'floobits-join-workspace
      "Pl" 'floobits-leave-workspace
      "Ps" 'floobits-summon
      "Pt" 'floobits-follow-mode-toggle
      "PR" 'floobits-share-dir-private
      "PU" 'floobits-share-dir-public)
    )
  )
