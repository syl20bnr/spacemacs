;;; magit-wip.el --- Commit snapshots to work-in-progress refs  -*- lexical-binding:t -*-

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

;; This library defines tree global modes which automatically commit
;; snapshots to branch-specific work-in-progress refs before and after
;; making changes, and two commands which can be used to do so on
;; demand.

;;; Code:

(require 'magit-core)
(require 'magit-log)

;;; Options

(defgroup magit-wip nil
  "Automatically commit to work-in-progress refs."
  :link '(info-link "(magit)Wip Modes")
  :group 'magit-modes
  :group 'magit-essentials)

(defgroup magit-wip-legacy nil
  "It is better to not use these modes individually."
  :link '(info-link "(magit)Legacy Wip Modes")
  :group 'magit-wip)

(defcustom magit-wip-mode-lighter " Wip"
  "Lighter for Magit-Wip mode."
  :package-version '(magit . "2.90.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-after-save-local-mode-lighter ""
  "Lighter for Magit-Wip-After-Save-Local mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip-legacy
  :type 'string)

(defcustom magit-wip-after-apply-mode-lighter ""
  "Lighter for Magit-Wip-After-Apply mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip-legacy
  :type 'string)

(defcustom magit-wip-before-change-mode-lighter ""
  "Lighter for Magit-Wip-Before-Change mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip-legacy
  :type 'string)

(defcustom magit-wip-initial-backup-mode-lighter ""
  "Lighter for Magit-Wip-Initial Backup mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip-legacy
  :type 'string)

(defcustom magit-wip-merge-branch nil
  "Whether to merge the current branch into its wip ref.

If non-nil and the current branch has new commits, then it is
merged into the wip ref before creating a new wip commit.  This
makes it easier to inspect wip history and the wip commits are
never garbage collected.

If nil and the current branch has new commits, then the wip ref
is reset to the tip of the branch before creating a new wip
commit.  With this setting wip commits are eventually garbage
collected.  This is currently the default."
  :package-version '(magit . "2.90.0")
  :group 'magit-wip
  :type 'boolean)

(defcustom magit-wip-namespace "refs/wip/"
  "Namespace used for work-in-progress refs.
The wip refs are named \"<namespace/>index/<branchref>\"
and \"<namespace/>wtree/<branchref>\".  When snapshots
are created while the `HEAD' is detached then \"HEAD\"
is used as `branch-ref'."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

;;; Modes

(defvar magit--wip-activation-cache nil)
(defvar magit--wip-inhibit-autosave nil)

;;;###autoload
(define-minor-mode magit-wip-mode
  "Save uncommitted changes to work-in-progress refs.

Whenever appropriate (i.e., when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged."
  :package-version '(magit . "2.90.0")
  :lighter magit-wip-mode-lighter
  :global t
  (let ((arg (if magit-wip-mode 1 -1)))
    (let ((magit--wip-activation-cache (list t)))
      (magit-wip-after-save-mode arg))
    (magit-wip-after-apply-mode arg)
    (magit-wip-before-change-mode arg)
    (magit-wip-initial-backup-mode arg)))

(define-minor-mode magit-wip-after-save-local-mode
  "After saving, also commit to a worktree work-in-progress ref.

After saving the current file-visiting buffer this mode also
commits the changes to the worktree work-in-progress ref for
the current branch.

This mode should be enabled globally by turning on the globalized
variant `magit-wip-after-save-mode'."
  :package-version '(magit . "2.1.0")
  :lighter magit-wip-after-save-local-mode-lighter
  (if magit-wip-after-save-local-mode
      (if (and buffer-file-name (magit-inside-worktree-p t))
          (add-hook 'after-save-hook #'magit-wip-commit-buffer-file t t)
        (setq magit-wip-after-save-local-mode nil)
        (user-error "Need a worktree and a file"))
    (remove-hook 'after-save-hook #'magit-wip-commit-buffer-file t)))

(defun magit-wip-after-save-local-mode-turn-on ()
  (when (and buffer-file-name
             (if magit--wip-activation-cache
                 (if-let ((elt (assoc default-directory
                                      magit--wip-activation-cache)))
                     (and-let* ((top (cadr elt)))
                       (member (file-relative-name buffer-file-name top)
                               (cddr elt)))
                   (if-let ((top (magit-toplevel)))
                       (let (files)
                         (if-let ((elt (assoc top magit--wip-activation-cache)))
                             (setq files (cddr elt))
                           (progn
                             (setq files (let ((default-directory top))
                                           (magit-tracked-files)))
                             (push `(,top ,top ,@files)
                                   magit--wip-activation-cache)
                             (unless (eq default-directory top)
                               (push `(,default-directory ,top ,@files)
                                     magit--wip-activation-cache))))
                         (member (file-relative-name buffer-file-name) files))
                     (push (list default-directory nil)
                           magit--wip-activation-cache)
                     nil))
               (and (magit-inside-worktree-p t)
                    (magit-file-tracked-p buffer-file-name))))
    (magit-wip-after-save-local-mode)))

;;;###autoload
(define-globalized-minor-mode magit-wip-after-save-mode
  magit-wip-after-save-local-mode magit-wip-after-save-local-mode-turn-on
  :package-version '(magit . "2.1.0")
  :group 'magit-wip)

(defun magit-wip-commit-buffer-file (&optional msg)
  "Commit visited file to a worktree work-in-progress ref.

Also see `magit-wip-after-save-mode' which calls this function
automatically whenever a buffer visiting a tracked file is saved."
  (interactive (list "wip-save %s after save"))
  (unless magit--wip-inhibit-autosave
    (when-let ((ref (magit-wip-get-ref)))
      (magit-with-toplevel
        (let ((file (file-relative-name buffer-file-name)))
          (magit-wip-commit-worktree
           ref (list file)
           (format (or msg "autosave %s after save") file)))))))

;;;###autoload
(define-minor-mode magit-wip-after-apply-mode
  "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :lighter magit-wip-after-apply-mode-lighter
  :global t)

(defun magit-wip-commit-after-apply (&optional files msg)
  (when magit-wip-after-apply-mode
    (magit-wip-commit files msg)))

;;;###autoload
(define-minor-mode magit-wip-before-change-mode
  "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :lighter magit-wip-before-change-mode-lighter
  :global t)

(defun magit-wip-commit-before-change (&optional files msg)
  (when magit-wip-before-change-mode
    (magit-with-toplevel
      (magit-wip-commit files msg))))

(define-minor-mode magit-wip-initial-backup-mode
  "Before saving a buffer for the first time, commit to a wip ref."
  :package-version '(magit . "2.90.0")
  :group 'magit-wip
  :lighter magit-wip-initial-backup-mode-lighter
  :global t
  (if magit-wip-initial-backup-mode
      (add-hook  'before-save-hook #'magit-wip-commit-initial-backup)
    (remove-hook 'before-save-hook #'magit-wip-commit-initial-backup)))

(defun magit--any-wip-mode-enabled-p ()
  "Return non-nil if any global wip mode is enabled."
  (or magit-wip-mode
      magit-wip-after-save-mode
      magit-wip-after-apply-mode
      magit-wip-before-change-mode
      magit-wip-initial-backup-mode))

(defvar-local magit-wip-buffer-backed-up nil)
(put 'magit-wip-buffer-backed-up 'permanent-local t)

;;;###autoload
(defun magit-wip-commit-initial-backup ()
  "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository."
  (when (and (not magit-wip-buffer-backed-up)
             buffer-file-name
             (magit-inside-worktree-p t)
             (magit-file-tracked-p buffer-file-name))
    (let ((magit-save-repository-buffers nil))
      (magit-wip-commit-buffer-file "autosave %s before save"))
    (setq magit-wip-buffer-backed-up t)))

;;; Core

(defun magit-wip-commit (&optional files msg)
  "Commit all tracked files to the work-in-progress refs.

Interactively, commit all changes to all tracked files using
a generic commit message.  With a prefix-argument the commit
message is read in the minibuffer.

Non-interactively, only commit changes to FILES using MSG as
commit message."
  (interactive (list nil (if current-prefix-arg
                             (magit-read-string "Wip commit message")
                           "wip-save tracked files")))
  (when-let ((ref (magit-wip-get-ref)))
    (magit-wip-commit-index ref files msg)
    (magit-wip-commit-worktree ref files msg)))

(defun magit-wip-commit-index (ref files msg)
  (let* ((wipref (magit--wip-index-ref ref))
         (parent (magit-wip-get-parent ref wipref))
         (tree   (magit-git-string "write-tree")))
    (magit-wip-update-wipref ref wipref tree parent files msg "index")))

(defun magit-wip-commit-worktree (ref files msg)
  (when (or (not files)
            ;; `update-index' will either ignore (before Git v2.32.0)
            ;; or fail when passed directories (relevant for the
            ;; untracked files code paths).
            (setq files (seq-remove #'file-directory-p files)))
    (let* ((wipref (magit--wip-wtree-ref ref))
           (parent (magit-wip-get-parent ref wipref))
           (tree (magit-with-temp-index parent (list "--reset" "-i")
                   (if files
                       ;; Note: `update-index' is used instead of `add'
                       ;; because `add' will fail if a file is already
                       ;; deleted in the temporary index.
                       (magit-call-git
                        "update-index" "--add" "--remove"
                        (and (magit-git-version>= "2.25.0")
                             "--ignore-skip-worktree-entries")
                        "--" files)
                     (magit-with-toplevel
                       (magit-call-git "add" "-u" ".")))
                   (magit-git-string "write-tree"))))
      (magit-wip-update-wipref ref wipref tree parent files msg "worktree"))))

(defun magit-wip-update-wipref (ref wipref tree parent files msg start-msg)
  (cond
   ((and (not (equal parent wipref))
         (or (not magit-wip-merge-branch)
             (not (magit-rev-verify wipref))))
    (setq start-msg (concat "start autosaving " start-msg))
    (magit-update-ref wipref start-msg
                      (magit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" parent "-m" start-msg
                                        (concat parent "^{tree}")))
    (setq parent wipref))
   ((and magit-wip-merge-branch
         (or (not (magit-rev-ancestor-p ref wipref))
             (not (magit-rev-ancestor-p
                   (concat (magit-git-string "log" "--format=%H"
                                             "-1" "--merges" wipref)
                           "^2")
                   ref))))
    (setq start-msg (format "merge %s into %s" ref start-msg))
    (magit-update-ref wipref start-msg
                      (magit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" wipref "-p" ref
                                        "-m" start-msg
                                        (concat ref "^{tree}")))
    (setq parent wipref)))
  (when (magit-git-failure "diff-tree" "--quiet" parent tree "--" files)
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (let ((len (length files)))
        (setq msg (concat
                   (cond ((= len 0) "autosave tracked files")
                         ((> len 1) (format "autosave %s files" len))
                         (t (concat "autosave "
                                    (file-relative-name (car files)
                                                        (magit-toplevel)))))
                   msg))))
    (magit-update-ref wipref msg
                      (magit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" parent "-m" msg tree))))

(defun magit-wip-get-ref ()
  (let ((ref (or (magit-git-string "symbolic-ref" "HEAD") "HEAD")))
    (and (magit-rev-verify ref)
         ref)))

(defun magit-wip-get-parent (ref wipref)
  (if (and (magit-rev-verify wipref)
           (equal (magit-git-string "merge-base" wipref ref)
                  (magit-rev-verify ref)))
      wipref
    ref))

(defun magit--wip-index-ref (&optional ref)
  (magit--wip-ref "index/" ref))

(defun magit--wip-wtree-ref (&optional ref)
  (magit--wip-ref "wtree/" ref))

(defun magit--wip-ref (namespace &optional ref)
  (concat magit-wip-namespace namespace
          (or (and ref (string-prefix-p "refs/" ref) ref)
              (and-let* ((branch (and (not (equal ref "HEAD"))
                                      (or ref (magit-get-current-branch)))))
                (concat "refs/heads/" branch))
              "HEAD")))

(defun magit-wip-maybe-add-commit-hook ()
  (when (and magit-wip-merge-branch
             (magit-wip-any-enabled-p))
    (add-hook 'git-commit-post-finish-hook #'magit-wip-commit nil t)))

(defun magit-wip-any-enabled-p ()
  (or magit-wip-mode
      magit-wip-after-save-local-mode
      magit-wip-after-save-mode
      magit-wip-after-apply-mode
      magit-wip-before-change-mode
      magit-wip-initial-backup-mode))

;;; Log

(defun magit-wip-log-index (args files)
  "Show log for the index wip ref of the current branch."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list (magit--wip-index-ref)) args files))

(defun magit-wip-log-worktree (args files)
  "Show log for the worktree wip ref of the current branch."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list (magit--wip-wtree-ref)) args files))

(defun magit-wip-log-current (branch args files count)
  "Show log for the current branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (or (magit-get-current-branch) "HEAD"))
          (magit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (magit-wip-log branch args files count))

(defun magit-wip-log (branch args files count)
  "Show log for a branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (magit-completing-read
                 "Log branch and its wip refs"
                 (nconc (magit-list-local-branch-names)
                        (list "HEAD"))
                 nil t nil 'magit-revision-history
                 (or (magit-branch-at-point)
                     (magit-get-current-branch)
                     "HEAD")))
          (magit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (magit-log-setup-buffer (nconc (list branch)
                                 (magit-wip-log-get-tips
                                  (magit--wip-wtree-ref branch)
                                  (abs count))
                                 (and (>= count 0)
                                      (magit-wip-log-get-tips
                                       (magit--wip-index-ref branch)
                                       (abs count))))
                          args files))

(defun magit-wip-log-get-tips (wipref count)
  (and-let* ((reflog (magit-git-lines "reflog" wipref)))
    (let (tips)
      (while (and reflog (> count 1))
        ;; "start autosaving ..." is the current message, but it used
        ;; to be "restart autosaving ...", and those messages may
        ;; still be around (e.g., if gc.reflogExpire is to "never").
        (setq reflog (cl-member "^[^ ]+ [^:]+: \\(?:re\\)?start autosaving"
                                reflog :test #'string-match-p))
        (when (and (cadr reflog)
                   (string-match "^[^ ]+ \\([^:]+\\)" (cadr reflog)))
          (push (match-string 1 (cadr reflog)) tips))
        (setq reflog (cddr reflog))
        (cl-decf count))
      (cons wipref (nreverse tips)))))

;;; _
(provide 'magit-wip)
;;; magit-wip.el ends here
