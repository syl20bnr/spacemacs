;;; packages.el --- ebuild layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Kai Wohlfahrt & Contributors
;;
;; Author:  <https://github.com/kwohlfahrt>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ebuild/init-ebuild-mode ()
  (use-package ebuild-mode
    :defer t
    :mode ("\\.\\(ebuild\\|eclass\\)" . ebuild-mode)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'ebuild-mode
        "n" 'ebuild-mode-insert-skeleton
        "k" 'ebuild-mode-keyword
        "e" 'ebuild-run-command
        "a" 'ebuild-run-echangelog))))

(defconst ebuild-packages
  '((ebuild-mode :location (recipe :fetcher github
                                   :repo "emacsmirror/ebuild-mode"))))
