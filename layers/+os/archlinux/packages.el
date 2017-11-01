;;; packages.el --- archlinux layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Piotr Jaszczyk <https://github.com/jaszczur>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst archlinux-packages
  '((pkgbuild-mode :location (recipe
                             :fetcher github
                             :repo "juergenhoetzel/pkgbuild-mode"))))

(defun  archlinux/init-pkgbuild-mode ()
  (require 'pkgbuild-mode)

  (setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

  (spacemacs/set-leader-keys-for-major-mode 'pkgbuild-mode
    "r" 'pkgbuild-increase-release-tag
    "b" 'pkgbuild-makepkg
    "a" 'pkgbuild-tar
    "u" 'pkgbuild-browse-url
    "m" 'pkgbuild-update-sums-line
    "e" 'pkgbuild-etags))

;;; packages.el ends here
