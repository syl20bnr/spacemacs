;;; packages.el --- prettier Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq prettier-packages '(prettier-js))

(defun prettier/init-prettier-js ()
  (use-package prettier-js
    :commands prettier-js
    :init
    (dolist (mode spacemacs--prettier-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "==" #'prettier-js))))
