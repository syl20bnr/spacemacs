;;; core-compilation.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-compiled-files
  '(;; Built-in libs that we changed
    "core/libs/forks/load-env-vars.el"
    ;; Rest of built-in libs.
    "core/libs/dash.el"
    "core/libs/ht.el"
    "core/libs/ido-vertical-mode.el"
    "core/libs/package-build-badges.el"
    "core/libs/package-build.el"
    "core/libs/package-recipe-mode.el"
    "core/libs/page-break-lines.el"
    "core/libs/quelpa.el"
    "core/libs/spinner.el")
  "List of Spacemacs files that should be compiled.
File paths are relative to the `user-emacs-directory'.")

(defun spacemacs//ensure-compilation (files)
  "Make sure that the .el FILES are compiled."
  (dolist (file files)
    (let ((fbn (file-name-sans-extension (file-truename file))))
     (unless (file-exists-p (concat fbn ".elc"))
       (byte-compile-file (concat fbn ".el"))))))

(provide 'core-compilation)
