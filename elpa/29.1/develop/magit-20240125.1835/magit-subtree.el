;;; magit-subtree.el --- Subtree support for Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
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

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-subtree "magit-subtree" nil t)
(transient-define-prefix magit-subtree ()
  "Import or export subtrees."
  :man-page "git-subtree"
  ["Actions"
   ("i" "Import" magit-subtree-import)
   ("e" "Export" magit-subtree-export)])

;;;###autoload (autoload 'magit-subtree-import "magit-subtree" nil t)
(transient-define-prefix magit-subtree-import ()
  "Import subtrees."
  :man-page "git-subtree"
  ["Arguments"
   (magit-subtree:--prefix)
   (magit-subtree:--message)
   ("-s" "Squash" "--squash")]
  ["Actions"
   [("a" "Add"        magit-subtree-add)
    ("c" "Add commit" magit-subtree-add-commit)]
   [("m" "Merge"      magit-subtree-merge)
    ("f" "Pull"       magit-subtree-pull)]])

;;;###autoload (autoload 'magit-subtree-export "magit-subtree" nil t)
(transient-define-prefix magit-subtree-export ()
  "Export subtrees."
  :man-page "git-subtree"
  ["Arguments"
   (magit-subtree:--prefix)
   (magit-subtree:--annotate)
   (magit-subtree:--branch)
   (magit-subtree:--onto)
   ("-i" "Ignore joins" "--ignore-joins")
   ("-j" "Rejoin"       "--rejoin")]
  ["Actions"
   ("p" "Push"          magit-subtree-push)
   ("s" "Split"         magit-subtree-split)])

(transient-define-argument magit-subtree:--prefix ()
  :description "Prefix"
  :class 'transient-option
  :shortarg "-P"
  :argument "--prefix="
  :reader #'magit-subtree-read-prefix)

(defun magit-subtree-read-prefix (prompt &optional default _history)
  (let* ((insert-default-directory nil)
         (topdir (magit-toplevel))
         (prefix (read-directory-name (concat prompt ": ") topdir default)))
    (if (file-name-absolute-p prefix)
        ;; At least `ido-mode's variant is not compatible.
        (if (string-prefix-p topdir prefix)
            (file-relative-name prefix topdir)
          (user-error "%s isn't inside the repository at %s" prefix topdir))
      prefix)))

(transient-define-argument magit-subtree:--message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(transient-define-argument magit-subtree:--annotate ()
  :description "Annotate"
  :class 'transient-option
  :key "-a"
  :argument "--annotate=")

(transient-define-argument magit-subtree:--branch ()
  :description "Branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--branch=")

(transient-define-argument magit-subtree:--onto ()
  :description "Onto"
  :class 'transient-option
  :key "-o"
  :argument "--onto="
  :reader #'magit-transient-read-revision)

(defun magit-subtree-prefix (transient prompt)
  (if-let ((arg (--first (string-prefix-p "--prefix=" it)
                         (transient-args transient))))
      (substring arg 9)
    (magit-subtree-read-prefix prompt)))

(defun magit-subtree-arguments (transient)
  (--remove (string-prefix-p "--prefix=" it)
            (transient-args transient)))

(defun magit-git-subtree (subcmd prefix &rest args)
  (magit-run-git-async "subtree" subcmd (concat "--prefix=" prefix) args))

;;;###autoload
(defun magit-subtree-add (prefix repository ref args)
  "Add REF from REPOSITORY as a new subtree at PREFIX."
  (interactive
   (cons (magit-subtree-prefix 'magit-subtree-import "Add subtree")
         (let ((remote (magit-read-remote-or-url "From repository")))
           (list remote
                 (magit-read-refspec "Ref" remote)
                 (magit-subtree-arguments 'magit-subtree-import)))))
  (magit-git-subtree "add" prefix args repository ref))

;;;###autoload
(defun magit-subtree-add-commit (prefix commit args)
  "Add COMMIT as a new subtree at PREFIX."
  (interactive
   (list (magit-subtree-prefix 'magit-subtree-import "Add subtree")
         (magit-read-string-ns "Commit")
         (magit-subtree-arguments 'magit-subtree-import)))
  (magit-git-subtree "add" prefix args commit))

;;;###autoload
(defun magit-subtree-merge (prefix commit args)
  "Merge COMMIT into the PREFIX subtree."
  (interactive
   (list (magit-subtree-prefix 'magit-subtree-import "Merge into subtree")
         (magit-read-string-ns "Commit")
         (magit-subtree-arguments 'magit-subtree-import)))
  (magit-git-subtree "merge" prefix args commit))

;;;###autoload
(defun magit-subtree-pull (prefix repository ref args)
  "Pull REF from REPOSITORY into the PREFIX subtree."
  (interactive
   (cons (magit-subtree-prefix 'magit-subtree-import "Pull into subtree")
         (let ((remote (magit-read-remote-or-url "From repository")))
           (list remote
                 (magit-read-refspec "Ref" remote)
                 (magit-subtree-arguments 'magit-subtree-import)))))
  (magit-git-subtree "pull" prefix args repository ref))

;;;###autoload
(defun magit-subtree-push (prefix repository ref args)
  "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY."
  (interactive (list (magit-subtree-prefix 'magit-subtree-export "Push subtree")
                     (magit-read-remote-or-url "To repository")
                     (magit-read-string-ns "To reference")
                     (magit-subtree-arguments 'magit-subtree-export)))
  (magit-git-subtree "push" prefix args repository ref))

;;;###autoload
(defun magit-subtree-split (prefix commit args)
  "Extract the history of the subtree PREFIX."
  (interactive (list (magit-subtree-prefix 'magit-subtree-export "Split subtree")
                     (magit-read-string-ns "Commit")
                     (magit-subtree-arguments 'magit-subtree-export)))
  (magit-git-subtree "split" prefix args commit))

;;; _
(provide 'magit-subtree)
;;; magit-subtree.el ends here
