;;; packages.el --- pkgbuild layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Romain Gautier <romain.gautier@nimamoh.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pkgbuild-packages '(pkgbuild-mode))

(defun pkgbuild/init-pkgbuild-mode ()
  (use-package pkgbuild-mode
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'pkgbuild-mode
        "b" 'pkgbuild-makepkg
        "s" 'pkgbuild-update-sums-line
        "t" 'pkgbuild-tar
        "i" 'pkgbuild-increase-release-tag))))
