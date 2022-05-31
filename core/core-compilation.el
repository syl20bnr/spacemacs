;;; core-compilation.el --- Spacemacs Core File -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
    "core/libs/package-recipe.el"
    "core/libs/page-break-lines.el"
    "core/libs/validate.el"
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

(defun spacemacs//remove-byte-compiled-files-in-dir (dir)
  "Remove all .elc files in DIR directory."
  (dolist (elc (directory-files-recursively dir "\\.elc$"))
    (when (file-exists-p elc)
      (delete-file elc))))

(defun spacemacs//dir-contains-stale-byte-compiled-files-p (dir)
  "Returns true if any .elc file in DIR directory is stale or orphaned."
  (cl-dolist (elc (directory-files-recursively dir "\\.elc$"))
    (let ((el (substring elc 0 -1)))
      (unless (and (file-exists-p el)
                   (file-newer-than-file-p elc el))
        (cl-return t)))))

(defun spacemacs//update-last-emacs-version ()
  "Update `spacemacs--last-emacs-version' and its saved value."
  (with-temp-file spacemacs--last-emacs-version-file
    (insert (format "(setq spacemacs--last-emacs-version %S)"
                    (setq spacemacs--last-emacs-version emacs-version)))
    (make-directory (file-name-directory spacemacs--last-emacs-version-file)
                    t)))

(provide 'core-compilation)
