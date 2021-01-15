;;; core-compilation.el --- Spacemacs Core File -*- no-byte-compile: t -*-
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

(defvar spacemacs--last-emacs-version ""
  "This variable is set during Emacs initialization to its version.")
(defconst spacemacs--last-emacs-version-file
  (expand-file-name (concat spacemacs-cache-directory "last-emacs-version"))
  "File that sets `spacemacs--last-emacs-version' variable.")

(defconst spacemacs--compiled-files
  '(;; Built-in libs that we changed
    "core/libs/forks/load-env-vars.el"
    "core/libs/forks/spacemacs-ht.el"
    ;; Rest of built-in libs.
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
    (let ((file-elc (thread-last file
                      (file-truename)
                      (file-name-sans-extension)
                      (format "%s.elc"))))
      (when (file-exists-p file-elc)
        (delete-file file-elc)))))

(defun spacemacs//contains-newer-than-byte-compiled-p (files)
  "Return true if any file in FILES is newer than its byte-compiled version."
  (cl-dolist (file files)
    (let* ((file-el (file-truename file))
           (file-elc (thread-last file-el
                       (file-name-sans-extension)
                       (format "%s.elc"))))
      (when (file-newer-than-file-p file-el file-elc)
        (cl-return t)))))

(defun spacemacs//update-last-emacs-version ()
  "Update `spacemacs--last-emacs-version' and its saved value."
  (with-temp-file spacemacs--last-emacs-version-file
    (insert (format "(setq spacemacs--last-emacs-version %S)"
                    (setq spacemacs--last-emacs-version emacs-version)))
    (make-directory (file-name-directory spacemacs--last-emacs-version-file)
                    t)))

(provide 'core-compilation)
