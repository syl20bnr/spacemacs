;;; packages.el --- android-logcat layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jiejing Zhang <kzjeef@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `android-logcat-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `android-logcat/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `android-logcat/pre-init-PACKAGE' and/or
;;   `android-logcat/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst android-logcat-packages
  '(
    (logcat :location
            (recipe
             :fetcher github
             :repo "dcolascione/logcat-mode"))
    ))

(defun android-logcat/init-logcat()
  (use-package logcat
    :init
    :config
    (progn   (add-to-list 'auto-mode-alist '("\\.log" . logcat-mode)))
  ))

;;; packages.el ends here
