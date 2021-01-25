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

(require 'cl-lib)
(require 'subr-x)

(defconst spacemacs--compiled-files
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
File paths are relative to the `spacemacs-start-directory'.")

(defun spacemacs//ensure-byte-compilation (files)
  "Make sure that elisp FILES are byte-compiled."
  (dolist (file files)
    (let ((fbp (file-name-sans-extension (file-truename file))))
      (unless (file-exists-p (concat fbp ".elc"))
        (byte-compile-file (concat fbp ".el"))))))

(defun spacemacs//remove-byte-compiled-files (files)
  "Remove .elc files corresponding to the source FILES."
  (dolist (file files)
    (thread-last file
      (file-truename)
      (file-name-sans-extension)
      (format "%s.elc")
      (delete-file))))

(defun spacemacs//remove-byte-compiled-if-stale (files)
  "Remove all .elc files corresponding to the source FILES if any is stale."
  (when (cl-dolist (file files)
          (let* ((file-el (file-truename file))
                 (file-elc (thread-last file-el
                             (file-name-sans-extension)
                             (format "%s.elc"))))
            (when (file-newer-than-file-p file-el file-elc)
              (cl-return t))))
    (spacemacs//remove-byte-compiled-files files)))

(provide 'core-compilation)
