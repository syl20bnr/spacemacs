;;; magit-sparse-checkout.el --- Sparse checkout support for Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an interface to the `git sparse-checkout'
;; command.  It's been possible to define sparse checkouts since Git
;; v1.7.0 by adding patterns to $GIT_DIR/info/sparse-checkout and
;; calling `git read-tree -mu HEAD' to update the index and working
;; tree.  However, Git v2.25 introduced the `git sparse-checkout'
;; command along with "cone mode", which restricts the possible
;; patterns to directories to provide better performance.
;;
;; The goal of this library is to support the `git sparse-checkout'
;; command operating in cone mode.

;;; Code:

(require 'magit)

;;; Utilities

(defun magit-sparse-checkout-enabled-p ()
  "Return non-nil if working tree is a sparse checkout."
  (magit-get-boolean "core.sparsecheckout"))

(defun magit-sparse-checkout--assert-version ()
  ;; Older versions of Git have the ability to define sparse checkout
  ;; patterns in .git/info/sparse-checkout, but the sparse-checkout
  ;; command isn't available until 2.25.0.
  (when (magit-git-version< "2.25.0")
    (user-error "`git sparse-checkout' not available until Git v2.25")))

(defun magit-sparse-checkout--auto-enable ()
  (if (magit-sparse-checkout-enabled-p)
      (unless (magit-get-boolean "core.sparsecheckoutcone")
        (user-error
         "Magit's sparse checkout functionality requires cone mode"))
    ;; Note: Don't use `magit-sparse-checkout-enable' because it's
    ;; asynchronous.
    (magit-run-git "sparse-checkout" "init" "--cone")))

(defun magit-sparse-checkout-directories ()
  "Return directories that are recursively included in the sparse checkout.
See the `git sparse-checkout' manpage for details about
\"recursive\" versus \"parent\" directories in cone mode."
  (and (magit-get-boolean "core.sparsecheckoutcone")
       (mapcar #'file-name-as-directory
               (magit-git-lines "sparse-checkout" "list"))))

;;; Commands

;;;###autoload (autoload 'magit-sparse-checkout "magit-sparse-checkout" nil t)
(transient-define-prefix magit-sparse-checkout ()
  "Create and manage sparse checkouts."
  :man-page "git-sparse-checkout"
  ["Arguments for enabling"
   :if-not magit-sparse-checkout-enabled-p
   ("-i" "Use sparse index" "--sparse-index")]
  ["Actions"
   [:if-not magit-sparse-checkout-enabled-p
    ("e" "Enable sparse checkout" magit-sparse-checkout-enable)]
   [:if magit-sparse-checkout-enabled-p
    ("d" "Disable sparse checkout" magit-sparse-checkout-disable)
    ("r" "Reapply rules" magit-sparse-checkout-reapply)]
   [("s" "Set directories" magit-sparse-checkout-set)
    ("a" "Add directories" magit-sparse-checkout-add)]])

;;;###autoload
(defun magit-sparse-checkout-enable (&optional args)
  "Convert the working tree to a sparse checkout."
  (interactive (list (transient-args 'magit-sparse-checkout)))
  (magit-sparse-checkout--assert-version)
  (magit-run-git-async "sparse-checkout" "init" "--cone" args))

;;;###autoload
(defun magit-sparse-checkout-set (directories)
  "Restrict working tree to DIRECTORIES.
To extend rather than override the currently configured
directories, call `magit-sparse-checkout-add' instead."
  (interactive
   (list (magit-completing-read-multiple
          "Include these directories: "
          ;; Note: Given that the appeal of sparse checkouts is
          ;; dealing with very large trees, listing all subdirectories
          ;; may need to be reconsidered.
          (magit-revision-directories "HEAD"))))
  (magit-sparse-checkout--assert-version)
  (magit-sparse-checkout--auto-enable)
  (magit-run-git-async "sparse-checkout" "set" directories))

;;;###autoload
(defun magit-sparse-checkout-add (directories)
  "Add DIRECTORIES to the working tree.
To override rather than extend the currently configured
directories, call `magit-sparse-checkout-set' instead."
  (interactive
   (list (magit-completing-read-multiple
          "Add these directories: "
          ;; Same performance note as in `magit-sparse-checkout-set',
          ;; but even more so given the additional processing.
          (seq-remove
           (let ((re (concat
                      "\\`"
                      (regexp-opt (magit-sparse-checkout-directories)))))
             (lambda (d) (string-match-p re d)))
           (magit-revision-directories "HEAD")))))
  (magit-sparse-checkout--assert-version)
  (magit-sparse-checkout--auto-enable)
  (magit-run-git-async "sparse-checkout" "add" directories))

;;;###autoload
(defun magit-sparse-checkout-reapply ()
  "Reapply the sparse checkout rules to the working tree.
Some operations such as merging or rebasing may need to check out
files that aren't included in the sparse checkout.  Call this
command to reset to the sparse checkout state."
  (interactive)
  (magit-sparse-checkout--assert-version)
  (magit-run-git-async "sparse-checkout" "reapply"))

;;;###autoload
(defun magit-sparse-checkout-disable ()
  "Convert sparse checkout to full checkout.
Note that disabling the sparse checkout does not clear the
configured directories.  Call `magit-sparse-checkout-enable' to
restore the previous sparse checkout."
  (interactive)
  (magit-sparse-checkout--assert-version)
  (magit-run-git-async "sparse-checkout" "disable"))

;;; Miscellaneous

(defun magit-sparse-checkout-insert-header ()
  "Insert header line with sparse checkout information.
This header is not inserted by default.  To enable it, add it to
`magit-status-headers-hook'."
  (when (magit-sparse-checkout-enabled-p)
    (insert (propertize (format "%-10s" "Sparse! ")
                        'font-lock-face 'magit-section-heading))
    (insert
     (let ((dirs (magit-sparse-checkout-directories)))
       (pcase (length dirs)
         (0 "top-level directory")
         (1 (car dirs))
         (n (format "%d directories" n)))))
    (insert ?\n)))

;;; _
(provide 'magit-sparse-checkout)
;;; magit-sparse-checkout.el ends here
