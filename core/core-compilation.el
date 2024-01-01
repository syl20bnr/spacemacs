;;; core-compilation.el --- Spacemacs Core File -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
(require 'bytecomp)

(defvar spacemacs--last-emacs-version ""
  "This variable is set during Emacs initialization to its version.")
(defconst spacemacs--last-emacs-version-file
  (expand-file-name (concat spacemacs-cache-directory "last-emacs-version"))
  "File that sets `spacemacs--last-emacs-version' variable.")

(defun spacemacs//remove-byte-compiled-files-in-dir (dir)
  "Remove all .elc files in DIR directory."
  (dolist (elc (directory-files-recursively dir "\\.elc\\(\\.gz\\)?$"))
    (when (file-exists-p elc)
      (delete-file elc))))

(defvar spacemacs--dir-byte-compile-status
  (make-hash-table :test 'equal)
  "The hash table to store each directory byte compile state.
nil for un-initialized, -1 for stale or orphaned *.elc,
0 for no *.elc, 1 for *.elc corresponding to *.el.")

(cl-defun spacemacs//dir-byte-compile-state (dir &optional update)
  "Get the directory byte-compile state.
When the UPDATE is t, it will force update the state."
  (let ((state (gethash dir spacemacs--dir-byte-compile-status)))
    (when (and (not update) state)
      (cl-return-from spacemacs//dir-byte-compile-state state))
    (setq state nil)
    (remhash dir spacemacs--dir-byte-compile-status)
    (let ((afiles '())
          (slist (mapcan
                  (lambda (x)
                    (mapcar (lambda (y) (concat x y)) load-file-rep-suffixes))
                  (list ".el" (byte-compile-dest-file ".el")))))
      (cl-dolist (file (directory-files-recursively dir "\\.elc?\\(\\.gz\\)?$"))
        (let* ((name (file-name-sans-extension file))
               (paths (alist-get name afiles nil nil 'equal)))
          (unless paths
            (setq paths (list nil nil nil nil))
            (push (cons name paths) afiles))
          (if-let ((idx (cl-loop for i from 0
                                 for s in slist
                                 until (string-suffix-p s file)
                                 finally return i)))
              (setf (nth idx paths) file))))
      (cl-dolist (item (mapcar 'cdr afiles))
        (let ((el (or (nth 0 item) (nth 1 item)))   ; .el or .el.gz file
              (elc (or (nth 2 item) (nth 3 item)))) ; .elc or .elc.gz file
          (pcase nil
            ((guard (null el))            ; *.el not exists
             (puthash dir -1 spacemacs--dir-byte-compile-status)
             (cl-return-from spacemacs//dir-byte-compile-state -1))
            ((guard (null elc))           ; *.elc not exists
             (when (null state)
               (setq state 0)))
            ((guard (file-newer-than-file-p el elc)) ; *.elc is older
             (puthash dir -1 spacemacs--dir-byte-compile-status)
             (cl-return-from spacemacs//dir-byte-compile-state -1))
            (_
             (setq state 1)))))
      (puthash dir state spacemacs--dir-byte-compile-status)
      state)))

(defun spacemacs//update-last-emacs-version ()
  "Update `spacemacs--last-emacs-version' and its saved value."
  (with-temp-file spacemacs--last-emacs-version-file
    (insert (format "(setq spacemacs--last-emacs-version %S)"
                    (setq spacemacs--last-emacs-version emacs-version)))
    (make-directory (file-name-directory spacemacs--last-emacs-version-file)
                    t)))

(provide 'core-compilation)
