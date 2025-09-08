;;; core-compilation.el --- Spacemacs Core File -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

;; Required libraries for various utilities and compatibility
(require 'cl-lib)      ;; Common Lisp extensions for Emacs Lisp
(require 'subr-x)      ;; Miscellaneous extensions (e.g., string manipulation)
(require 'bytecomp)    ;; Byte-compilation utilities

;; Variable to store the last Emacs version used by Spacemacs.
(defvar spacemacs--last-emacs-version ""
  "This variable is set during Emacs initialization to its version.")

;; Path to the file where the last Emacs version is persisted.
(defconst spacemacs--last-emacs-version-file
  (expand-file-name (concat spacemacs-cache-directory "last-emacs-version"))
  "File that sets `spacemacs--last-emacs-version' variable.")

;; Utility function to remove all byte-compiled files (.elc) in a directory.
(defun spacemacs//remove-byte-compiled-files-in-dir (dir)
  "Remove all .elc files in DIR directory.
This is useful for cleaning up stale or orphaned byte-compiled files
when source files have changed or been removed."
  (dolist (elc (directory-files-recursively dir "\\.elc\\(\\.gz\\)?$"))
    (when (file-exists-p elc)
      (delete-file elc))))

;; Hash table to track the byte-compile status of directories.
(defvar spacemacs--dir-byte-compile-status
  (make-hash-table :test 'equal)
  "The hash table to store each directory byte compile state.
nil for un-initialized, -1 for stale or orphaned *.elc,
0 for no *.elc, 1 for *.elc corresponding to *.el.")

;; Function to determine the byte-compile state of a directory.
(cl-defun spacemacs//dir-byte-compile-state (dir &optional update)
  "Get the directory byte-compile state.
When UPDATE is t, force update the state.
States:
- nil: uninitialized
- -1: stale or orphaned *.elc files (missing or outdated source)
- 0: no *.elc files present
- 1: *.elc files are up-to-date with *.el files"
  (let ((state (gethash dir spacemacs--dir-byte-compile-status)))
    ;; Return cached state unless update is requested
    (when (and (not update) state)
      (cl-return-from spacemacs//dir-byte-compile-state state))
    (setq state nil)
    (remhash dir spacemacs--dir-byte-compile-status)
    ;; Prepare list of possible file suffixes (.el, .el.gz, .elc, .elc.gz)
    (let ((afiles '())
          (slist (mapcan
                  (lambda (x)
                    (mapcar (lambda (y) (concat x y)) load-file-rep-suffixes))
                  (list ".el" (byte-compile-dest-file ".el")))))
      ;; Scan directory for all .el and .elc files
      (cl-dolist (file (directory-files-recursively dir "\\.elc?\\(\\.gz\\)?$"))
        (let* ((name (file-name-sans-extension file))
               (paths (alist-get name afiles nil nil 'equal)))
          ;; Initialize paths for each base filename
          (unless paths
            (setq paths (list nil nil nil nil))
            (push (cons name paths) afiles))
          ;; Assign file to correct slot based on suffix
          (if-let* ((idx (cl-loop for i from 0
                                  for s in slist
                                  until (string-suffix-p s file)
                                  finally return i)))
              (setf (nth idx paths) file))))
      ;; For each file group, determine state
      (cl-dolist (item (mapcar 'cdr afiles))
        (let ((el (or (nth 0 item) (nth 1 item)))   ;; .el or .el.gz file
              (elc (or (nth 2 item) (nth 3 item)))) ;; .elc or .elc.gz file
          (pcase nil
            ((guard (null el))            ;; Source file missing
             (puthash dir -1 spacemacs--dir-byte-compile-status)
             (cl-return-from spacemacs//dir-byte-compile-state -1))
            ((guard (null elc))           ;; Compiled file missing
             (when (null state)
               (setq state 0)))
            ((guard (file-newer-than-file-p el elc)) ;; Compiled file outdated
             (puthash dir -1 spacemacs--dir-byte-compile-status)
             (cl-return-from spacemacs//dir-byte-compile-state -1))
            (_
             (setq state 1)))))
      ;; Cache and return the determined state
      (puthash dir state spacemacs--dir-byte-compile-status)
      state)))

;; Function to update and persist the last Emacs version used.
(defun spacemacs//update-last-emacs-version ()
  "Update `spacemacs--last-emacs-version' and its saved value.
This is used to detect when Emacs has been upgraded, so caches and
byte-compiled files can be refreshed if needed."
  (with-temp-file spacemacs--last-emacs-version-file
    (insert (format ";;; -*- lexical-binding: nil; -*-\n(setq spacemacs--last-emacs-version %S)"
                    (setq spacemacs--last-emacs-version emacs-version)))

    ;; Ensure the directory exists before creating the temp file.
    ;; Without this block will fail when trying to write the file.
    (make-directory (file-name-directory spacemacs--last-emacs-version-file)
                    t)))

;; Make this module available for require
(provide 'core-compilation)
