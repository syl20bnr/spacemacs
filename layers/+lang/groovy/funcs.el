;;; funcs.el --- Groovy layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (configuration-layer/package-usedp 'gradle-mode)
  (defun groovy/gradle-clean-build ()
    "Execute gradle clean build command."
    (interactive)
    (gradle-execute "clean build"))

  (defun groovy/gradle-test-buffer ()
    "Execute gradle test command against buffer tests."
    (interactive)
    (gradle-single-test (file-name-base (buffer-file-name)))))
