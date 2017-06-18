;;; packages.el --- Makepkg Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sven Fischer <sven@leiderfischer.de>
;; URL: https://github.com/syl20bnr/spacemacs
;; Salt mode URL: https://github.com/juergenhoetzel/pkgbuild-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq makepkg-packages '(pkgbuild-mode))

(defun makepkg/init-pkgbuild-mode ()
  (use-package pkgbuild-mode
    :mode ("/PKGBUILD\\'" . pkgbuild-mode)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'pkgbuild-mode
        "r" 'pkgbuild-increase-release-tag
        "b" 'pkgbuild-makepkg
        "a" 'pkgbuild-tar
        "u" 'pkgbuild-browse-url
        "m" 'pkgbuild-update-sums-line
        "e" 'pkgbuild-etags)
      )))

