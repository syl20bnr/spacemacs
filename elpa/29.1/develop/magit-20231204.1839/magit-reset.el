;;; magit-reset.el --- Reset functionality  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

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

;;; Commentary:

;; This library implements reset commands.

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-reset "magit" nil t)
(transient-define-prefix magit-reset ()
  "Reset the `HEAD', index and/or worktree to a previous state."
  :man-page "git-reset"
  [["Reset"
    ("b" "branch" magit-branch-reset)
    ("f" "file"   magit-file-checkout)]
   ["Reset this"
    ("m" "mixed    (HEAD and index)" magit-reset-mixed)
    ("s" "soft     (HEAD only)"      magit-reset-soft)
    ("h" "hard     (HEAD, index and worktree)" magit-reset-hard)
    ("k" "keep     (HEAD and index, keeping uncommitted)" magit-reset-keep)
    ("i" "index    (only)"           magit-reset-index)
    ("w" "worktree (only)"           magit-reset-worktree)]])

;;;###autoload
(defun magit-reset-mixed (commit)
  "Reset the `HEAD' and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-reset-read-branch-or-commit "Reset %s to")))
  (magit-reset-internal "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the `HEAD' to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (magit-reset-read-branch-or-commit "Soft reset %s to")))
  (magit-reset-internal "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the `HEAD', index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (magit-reset-read-branch-or-commit
                      (concat (magit--propertize-face "Hard" 'bold)
                              " reset %s to"))))
  (magit-reset-internal "--hard" commit))

;;;###autoload
(defun magit-reset-keep (commit)
  "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.
\n(git reset --keep REVISION)"
  (interactive (list (magit-reset-read-branch-or-commit "Reset %s to")))
  (magit-reset-internal "--keep" commit))

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.
\n(git reset COMMIT .)"
  (interactive (list (magit-read-branch-or-commit "Reset index to")))
  (magit-reset-internal nil commit "."))

;;;###autoload
(defun magit-reset-worktree (commit)
  "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is."
  (interactive (list (magit-read-branch-or-commit "Reset worktree to")))
  (magit-wip-commit-before-change nil " before reset")
  (magit-with-temp-index commit nil
    (magit-call-git "checkout-index" "--all" "--force"))
  (magit-wip-commit-after-apply nil " after reset")
  (magit-refresh))

;;;###autoload
(defun magit-reset-quickly (commit &optional hard)
  "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (magit-reset-read-branch-or-commit
                      (if current-prefix-arg
                          (concat (magit--propertize-face "Hard" 'bold)
                                  " reset %s to")
                        "Reset %s to"))
                     current-prefix-arg))
  (magit-reset-internal (if hard "--hard" "--mixed") commit))

(defun magit-reset-read-branch-or-commit (prompt)
  "Prompt for and return a ref to reset HEAD to.

PROMPT is a format string, where either the current branch name
or \"detached head\" will be substituted for %s."
  (magit-read-branch-or-commit
   (format prompt (or (magit-get-current-branch) "detached head"))))

(defun magit-reset-internal (arg commit &optional path)
  (when (and (not (member arg '("--hard" nil)))
             (equal (magit-rev-parse commit)
                    (magit-rev-parse "HEAD~")))
    (with-temp-buffer
      (magit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (let ((cmd (if (and (equal commit "HEAD") (not arg)) "unstage" "reset")))
    (magit-wip-commit-before-change nil (concat " before " cmd))
    (magit-run-git "reset" arg commit "--" path)
    (when (equal cmd "unstage")
      (magit-wip-commit-after-apply nil " after unstage"))))

;;; _
(provide 'magit-reset)
;;; magit-reset.el ends here
