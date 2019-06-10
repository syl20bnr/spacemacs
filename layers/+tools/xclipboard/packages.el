;;; packages.el --- xclipboard layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst xclipboard-packages
  '(
    (spacemacs-xclipboard :location local)
    ))

(defun xclipboard/init-spacemacs-xclipboard ()
  (use-package spacemacs-xclipboard
    :init (spacemacs/set-leader-keys
            "xp" 'spacemacs/xclipboard-paste
            "xy" 'spacemacs/xclipboard-copy)))
