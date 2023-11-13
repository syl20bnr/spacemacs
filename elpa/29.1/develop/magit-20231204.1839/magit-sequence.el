;;; magit-sequence.el --- History manipulation in Magit  -*- lexical-binding:t -*-

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

;; Support for Git commands that replay commits and help the user make
;; changes along the way.  Supports `cherry-pick', `revert', `rebase',
;; `rebase--interactive' and `am'.

;;; Code:

(require 'magit)

;; For `magit-rebase--todo'.
(declare-function git-rebase-current-line "git-rebase" ())
(eval-when-compile
  (cl-pushnew 'action-type eieio--known-slot-names)
  (cl-pushnew 'action eieio--known-slot-names)
  (cl-pushnew 'action-options eieio--known-slot-names)
  (cl-pushnew 'target eieio--known-slot-names))

;;; Options
;;;; Faces

(defface magit-sequence-pick
  '((t :inherit default))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-stop
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :foreground "DarkSeaGreen2"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-part
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background dark))  :foreground "LightGoldenrod2"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-head
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-drop
  '((((class color) (background light)) :foreground "IndianRed")
    (((class color) (background dark))  :foreground "IndianRed"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-done
  '((t :inherit magit-hash))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-onto
  '((t :inherit magit-sequence-done))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-exec
  '((t :inherit magit-hash))
  "Face used in sequence sections."
  :group 'magit-faces)

;;; Common

;;;###autoload
(defun magit-sequencer-continue ()
  "Resume the current cherry-pick or revert sequence."
  (interactive)
  (cond
   ((not (magit-sequencer-in-progress-p))
    (user-error "No cherry-pick or revert in progress"))
   ((magit-anything-unmerged-p)
    (user-error "Cannot continue due to unresolved conflicts"))
   ((magit-run-git-sequencer
     (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))))

;;;###autoload
(defun magit-sequencer-skip ()
  "Skip the stopped at commit during a cherry-pick or revert sequence."
  (interactive)
  (unless (magit-sequencer-in-progress-p)
    (user-error "No cherry-pick or revert in progress"))
  (magit-call-git "reset" "--hard")
  (magit-sequencer-continue))

;;;###autoload
(defun magit-sequencer-abort ()
  "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started."
  (interactive)
  (cond
   ((not (magit-sequencer-in-progress-p))
    (user-error "No cherry-pick or revert in progress"))
   ((magit-revert-in-progress-p)
    (magit-confirm 'abort-revert "Really abort revert")
    (magit-run-git-sequencer "revert" "--abort"))
   ((magit-confirm 'abort-cherry-pick "Really abort cherry-pick")
    (magit-run-git-sequencer "cherry-pick" "--abort"))))

(defun magit-sequencer-in-progress-p ()
  (or (magit-cherry-pick-in-progress-p)
      (magit-revert-in-progress-p)))

;;; Cherry-Pick

(defvar magit-perl-executable "perl"
  "The Perl executable.")

;;;###autoload (autoload 'magit-cherry-pick "magit-sequence" nil t)
(transient-define-prefix magit-cherry-pick ()
  "Apply or transplant commits."
  :man-page "git-cherry-pick"
  :value '("--ff")
  :incompatible '(("--ff" "-x"))
  ["Arguments"
   :if-not magit-sequencer-in-progress-p
   (magit-cherry-pick:--mainline)
   ("=s" magit-merge:--strategy)
   ("-F" "Attempt fast-forward"               "--ff")
   ("-x" "Reference cherry in commit message" "-x")
   ("-e" "Edit commit messages"               ("-e" "--edit"))
   ("-s" "Add Signed-off-by lines"            ("-s" "--signoff"))
   (5 magit:--gpg-sign)]
  [:if-not magit-sequencer-in-progress-p
   ["Apply here"
    ("A" "Pick"    magit-cherry-copy)
    ("a" "Apply"   magit-cherry-apply)
    ("h" "Harvest" magit-cherry-harvest)
    ("m" "Squash"  magit-merge-squash)]
   ["Apply elsewhere"
    ("d" "Donate"  magit-cherry-donate)
    ("n" "Spinout" magit-cherry-spinout)
    ("s" "Spinoff" magit-cherry-spinoff)]]
  ["Actions"
   :if magit-sequencer-in-progress-p
   ("A" "Continue" magit-sequencer-continue)
   ("s" "Skip"     magit-sequencer-skip)
   ("a" "Abort"    magit-sequencer-abort)])

(transient-define-argument magit-cherry-pick:--mainline ()
  :description "Replay merge relative to parent"
  :class 'transient-option
  :shortarg "-m"
  :argument "--mainline="
  :reader #'transient-read-number-N+)

(defun magit-cherry-pick-read-args (prompt)
  (list (or (nreverse (magit-region-values 'commit))
            (magit-read-other-branch-or-commit prompt))
        (transient-args 'magit-cherry-pick)))

(defun magit--cherry-move-read-args (verb away fn &optional allow-detached)
  (declare (indent defun))
  (let ((commits (or (nreverse (magit-region-values 'commit))
                     (list (funcall (if away
                                        #'magit-read-branch-or-commit
                                      #'magit-read-other-branch-or-commit)
                                    (format "%s cherry" (capitalize verb))))))
        (current (or (magit-get-current-branch)
                     (and allow-detached (magit-rev-parse "HEAD")))))
    (unless current
      (user-error "Cannot %s cherries while HEAD is detached" verb))
    (let ((reachable (magit-rev-ancestor-p (car commits) current))
          (msg "Cannot %s cherries that %s reachable from HEAD"))
      (pcase (list away reachable)
        ('(nil t) (user-error msg verb "are"))
        ('(t nil) (user-error msg verb "are not"))))
    `(,commits
      ,@(funcall fn commits)
      ,(transient-args 'magit-cherry-pick))))

(defun magit--cherry-spinoff-read-args (verb)
  (magit--cherry-move-read-args verb t
    (lambda (commits)
      (magit-branch-read-args
       (format "Create branch from %s cherries" (length commits))
       (magit-get-upstream-branch)))))

;;;###autoload
(defun magit-cherry-copy (commits &optional args)
  "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Cherry-pick"))
  (magit--cherry-pick commits args))

;;;###autoload
(defun magit-cherry-apply (commits &optional args)
  "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Apply changes from commit"))
  (magit--cherry-pick commits (cons "--no-commit" (remove "--ff" args))))

;;;###autoload
(defun magit-cherry-harvest (commits branch &optional args)
  "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive
   (magit--cherry-move-read-args "harvest" nil
     (lambda (commits)
       (list (let ((branches (magit-list-containing-branches (car commits))))
               (pcase (length branches)
                 (0 nil)
                 (1 (car branches))
                 (_ (magit-completing-read
                     (let ((len (length commits)))
                       (if (= len 1)
                           "Remove 1 cherry from branch"
                         (format "Remove %s cherries from branch" len)))
                     branches nil t))))))))
  (magit--cherry-move commits branch (magit-get-current-branch) args nil t))

;;;###autoload
(defun magit-cherry-donate (commits branch &optional args)
  "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially."
  (interactive
   (magit--cherry-move-read-args "donate" t
     (lambda (commits)
       (list (magit-read-other-branch
              (let ((len (length commits)))
                (if (= len 1)
                    "Move 1 cherry to branch"
                  (format "Move %s cherries to branch" len))))))
     'allow-detached))
  (magit--cherry-move commits
                      (or (magit-get-current-branch)
                          (magit-rev-parse "HEAD"))
                      branch args))

;;;###autoload
(defun magit-cherry-spinout (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive (magit--cherry-spinoff-read-args "spinout"))
  (magit--cherry-move commits (magit-get-current-branch) branch args
                      start-point))

;;;###autoload
(defun magit-cherry-spinoff (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually."
  (interactive (magit--cherry-spinoff-read-args "spinoff"))
  (magit--cherry-move commits (magit-get-current-branch) branch args
                      start-point t))

(defun magit--cherry-move (commits src dst args
                                   &optional start-point checkout-dst)
  (let ((current (magit-get-current-branch)))
    (unless (magit-branch-p dst)
      (let ((magit-process-raise-error t))
        (magit-call-git "branch" dst start-point))
      (when-let ((upstream (magit-get-indirect-upstream-branch start-point)))
        (magit-call-git "branch" "--set-upstream-to" upstream dst)))
    (unless (equal dst current)
      (let ((magit-process-raise-error t))
        (magit-call-git "checkout" dst)))
    (if (not src) ; harvest only
        (magit--cherry-pick commits args)
      (let ((tip (car (last commits)))
            (keep (concat (car commits) "^")))
        (magit--cherry-pick commits args)
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (> (process-exit-status process) 0)
                 (magit-process-sentinel process event)
               (process-put process 'inhibit-refresh t)
               (magit-process-sentinel process event)
               (cond
                ((magit-rev-equal tip src)
                 (magit-call-git "update-ref"
                                 "-m" (format "reset: moving to %s" keep)
                                 (magit-ref-fullname src)
                                 keep tip)
                 (if (not checkout-dst)
                     (magit-run-git "checkout" src)
                   (magit-refresh)))
                (t
                 (magit-git "checkout" src)
                 (with-environment-variables
                     (("GIT_SEQUENCE_EDITOR"
                       (format "%s -i -ne '/^pick (%s)/ or print'"
                               magit-perl-executable
                               (mapconcat #'magit-rev-abbrev commits "|"))))
                   (magit-run-git-sequencer "rebase" "-i" keep))
                 (when checkout-dst
                   (set-process-sentinel
                    magit-this-process
                    (lambda (process event)
                      (when (memq (process-status process) '(exit signal))
                        (if (> (process-exit-status process) 0)
                            (magit-process-sentinel process event)
                          (process-put process 'inhibit-refresh t)
                          (magit-process-sentinel process event)
                          (magit-run-git "checkout" dst))))))))))))))))

(defun magit--cherry-pick (commits args &optional revert)
  (let ((command (if revert "revert" "cherry-pick")))
    (when (stringp commits)
      (setq commits (if (string-search ".." commits)
                        (split-string commits "\\.\\.")
                      (list commits))))
    (magit-run-git-sequencer
     (if revert "revert" "cherry-pick")
     (let ((merges (seq-filter #'magit-merge-commit-p commits)))
       (cond
        ((not merges)
         (--remove (string-prefix-p "--mainline=" it) args))
        ((cl-set-difference commits merges :test #'equal)
         (user-error "Cannot %s merge and non-merge commits at once"
                     command))
        ((--first (string-prefix-p "--mainline=" it) args)
         args)
        (t
         (cons (format "--mainline=%s"
                       (read-number "Replay merges relative to parent: "))
               args))))
     commits)))

(defun magit-cherry-pick-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (let ((dir (magit-gitdir)))
    (or (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" dir))
        ;; And CHERRY_PICK_HEAD does not exist when a conflict happens
        ;; while picking a series of commits with --no-commit.
        (and-let* ((line (magit-file-line
                          (expand-file-name "sequencer/todo" dir))))
          (string-prefix-p "pick" line)))))

;;; Revert

;;;###autoload (autoload 'magit-revert "magit-sequence" nil t)
(transient-define-prefix magit-revert ()
  "Revert existing commits, with or without creating new commits."
  :man-page "git-revert"
  :value '("--edit")
  ["Arguments"
   :if-not magit-sequencer-in-progress-p
   (magit-cherry-pick:--mainline)
   ("-e" "Edit commit message"       ("-e" "--edit"))
   ("-E" "Don't edit commit message" "--no-edit")
   ("=s" magit-merge:--strategy)
   ("-s" "Add Signed-off-by lines"   ("-s" "--signoff"))
   (5 magit:--gpg-sign)]
  ["Actions"
   :if-not magit-sequencer-in-progress-p
   ("V" "Revert commit(s)" magit-revert-and-commit)
   ("v" "Revert changes"   magit-revert-no-commit)]
  ["Actions"
   :if magit-sequencer-in-progress-p
   ("V" "Continue" magit-sequencer-continue)
   ("s" "Skip"     magit-sequencer-skip)
   ("a" "Abort"    magit-sequencer-abort)])

(defun magit-revert-read-args (prompt)
  (list (or (magit-region-values 'commit)
            (magit-read-branch-or-commit prompt))
        (transient-args 'magit-revert)))

;;;###autoload
(defun magit-revert-and-commit (commit &optional args)
  "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert commit"))
  (magit--cherry-pick commit args t))

;;;###autoload
(defun magit-revert-no-commit (commit &optional args)
  "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert changes"))
  (magit--cherry-pick commit (cons "--no-commit" args) t))

(defun magit-revert-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (let ((dir (magit-gitdir)))
    (or (file-exists-p (expand-file-name "REVERT_HEAD" dir))
        ;; And REVERT_HEAD does not exist when a conflict happens
        ;; while reverting a series of commits with --no-commit.
        (and-let* ((line (magit-file-line
                          (expand-file-name "sequencer/todo" dir))))
          (string-prefix-p "revert" line)))))

;;; Patch

;;;###autoload (autoload 'magit-am "magit-sequence" nil t)
(transient-define-prefix magit-am ()
  "Apply patches received by email."
  :man-page "git-am"
  :value '("--3way")
  ["Arguments"
   :if-not magit-am-in-progress-p
   ("-3" "Fall back on 3way merge"           ("-3" "--3way"))
   (magit-apply:-p)
   ("-c" "Remove text before scissors line"  ("-c" "--scissors"))
   ("-k" "Inhibit removal of email cruft"    ("-k" "--keep"))
   ("-b" "Limit removal of email cruft"      "--keep-non-patch")
   ("-d" "Use author date as committer date" "--committer-date-is-author-date")
   ("-t" "Use current time as author date"   "--ignore-date")
   ("-s" "Add Signed-off-by lines"           ("-s" "--signoff"))
   (5 magit:--gpg-sign)]
  ["Apply"
   :if-not magit-am-in-progress-p
   ("m" "maildir"     magit-am-apply-maildir)
   ("w" "patches"     magit-am-apply-patches)
   ("a" "plain patch" magit-patch-apply)]
  ["Actions"
   :if magit-am-in-progress-p
   ("w" "Continue" magit-am-continue)
   ("s" "Skip"     magit-am-skip)
   ("a" "Abort"    magit-am-abort)])

(defun magit-am-arguments ()
  (transient-args 'magit-am))

(transient-define-argument magit-apply:-p ()
  :description "Remove leading slashes from paths"
  :class 'transient-option
  :argument "-p"
  :allow-empty t
  :reader #'transient-read-number-N+)

;;;###autoload
(defun magit-am-apply-patches (&optional files args)
  "Apply the patches FILES."
  (interactive (list (or (magit-region-values 'file)
                         (list (let ((default (magit-file-at-point)))
                                 (read-file-name
                                  (if default
                                      (format "Apply patch (%s): " default)
                                    "Apply patch: ")
                                  nil default))))
                     (magit-am-arguments)))
  (magit-run-git-sequencer "am" args "--"
                           (--map (magit-convert-filename-for-git
                                   (expand-file-name it))
                                  files)))

;;;###autoload
(defun magit-am-apply-maildir (&optional maildir args)
  "Apply the patches from MAILDIR."
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     (magit-am-arguments)))
  (magit-run-git-sequencer "am" args (magit-convert-filename-for-git
                                      (expand-file-name maildir))))

;;;###autoload
(defun magit-am-continue ()
  "Resume the current patch applying sequence."
  (interactive)
  (cond
   ((not (magit-am-in-progress-p))
    (user-error "Not applying any patches"))
   ((magit-anything-unstaged-p t)
    (user-error "Cannot continue due to unstaged changes"))
   ((magit-run-git-sequencer "am" "--continue"))))

;;;###autoload
(defun magit-am-skip ()
  "Skip the stopped at patch during a patch applying sequence."
  (interactive)
  (unless (magit-am-in-progress-p)
    (user-error "Not applying any patches"))
  (magit-run-git-sequencer "am" "--skip"))

;;;###autoload
(defun magit-am-abort ()
  "Abort the current patch applying sequence.
This discards all changes made since the sequence started."
  (interactive)
  (unless (magit-am-in-progress-p)
    (user-error "Not applying any patches"))
  (magit-run-git "am" "--abort"))

(defun magit-am-in-progress-p ()
  (file-exists-p (expand-file-name "rebase-apply/applying" (magit-gitdir))))

;;; Rebase

;;;###autoload (autoload 'magit-rebase "magit-sequence" nil t)
(transient-define-prefix magit-rebase ()
  "Transplant commits and/or modify existing commits."
  :man-page "git-rebase"
  :value '("--autostash")
  ["Arguments"
   :if-not magit-rebase-in-progress-p
   ("-k" "Keep empty commits"       "--keep-empty")
   ("-p" "Preserve merges"          ("-p" "--preserve-merges")
    :if (lambda () (magit-git-version< "2.33.0")))
   ("-r" "Rebase merges"            ("-r" "--rebase-merges=")
    magit-rebase-merges-select-mode
    :if (lambda () (magit-git-version>= "2.18.0")))
   ("-u" "Update branches"          "--update-refs"
    :if (lambda () (magit-git-version>= "2.38.0")))
   (7 magit-merge:--strategy)
   (7 magit-merge:--strategy-option)
   (7 "=X" magit-diff:--diff-algorithm :argument "-Xdiff-algorithm=")
   (7 "-f" "Force rebase"           ("-f" "--force-rebase"))
   ("-d" "Use author date as committer date" "--committer-date-is-author-date")
   ("-t" "Use current time as author date"   "--ignore-date")
   ("-a" "Autosquash"               "--autosquash")
   ("-A" "Autostash"                "--autostash")
   ("-i" "Interactive"              ("-i" "--interactive"))
   ("-h" "Disable hooks"            "--no-verify")
   (7 magit-rebase:--exec)
   (5 magit:--gpg-sign)]
  [:if-not magit-rebase-in-progress-p
   :description (lambda ()
                  (format (propertize "Rebase %s onto" 'face 'transient-heading)
                          (propertize (or (magit-get-current-branch) "HEAD")
                                      'face 'magit-branch-local)))
   ("p" magit-rebase-onto-pushremote)
   ("u" magit-rebase-onto-upstream)
   ("e" "elsewhere" magit-rebase-branch)]
  ["Rebase"
   :if-not magit-rebase-in-progress-p
   [("i" "interactively"      magit-rebase-interactive)
    ("s" "a subset"           magit-rebase-subset)]
   [("m" "to modify a commit" magit-rebase-edit-commit)
    ("w" "to reword a commit" magit-rebase-reword-commit)
    ("k" "to remove a commit" magit-rebase-remove-commit)
    ("f" "to autosquash"      magit-rebase-autosquash)
    (6 "t" "to change dates"  magit-reshelve-since)]]
  ["Actions"
   :if magit-rebase-in-progress-p
   ("r" "Continue" magit-rebase-continue)
   ("s" "Skip"     magit-rebase-skip)
   ("e" "Edit"     magit-rebase-edit)
   ("a" "Abort"    magit-rebase-abort)])

(transient-define-argument magit-rebase:--exec ()
  :description "Run command after commits"
  :class 'transient-option
  :shortarg "-x"
  :argument "--exec="
  :reader #'read-shell-command)

(defun magit-rebase-merges-select-mode (&rest _ignore)
  (magit-read-char-case nil t
    (?n "[n]o-rebase-cousins" "no-rebase-cousins")
    (?r "[r]ebase-cousins" "rebase-cousins")))

(defun magit-rebase-arguments ()
  (transient-args 'magit-rebase))

(defun magit-git-rebase (target args)
  (magit-run-git-sequencer "rebase" args target))

;;;###autoload (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t)
(transient-define-suffix magit-rebase-onto-pushremote (args)
  "Rebase the current branch onto its push-remote branch.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :if #'magit-get-current-branch
  :description #'magit-pull--pushbranch-description
  (interactive (list (magit-rebase-arguments)))
  (pcase-let ((`(,branch ,remote)
               (magit--select-push-remote "rebase onto that")))
    (magit-git-rebase (concat remote "/" branch) args)))

;;;###autoload (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t)
(transient-define-suffix magit-rebase-onto-upstream (args)
  "Rebase the current branch onto its upstream branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'magit-get-current-branch
  :description #'magit-rebase--upstream-description
  (interactive (list (magit-rebase-arguments)))
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "No branch is checked out")))
         (upstream (magit-get-upstream-branch branch)))
    (when (or current-prefix-arg (not upstream))
      (setq upstream
            (magit-read-upstream-branch
             branch (format "Set upstream of %s and rebase onto that" branch)))
      (magit-set-upstream-branch branch upstream))
    (magit-git-rebase upstream args)))

(defun magit-rebase--upstream-description ()
  (and-let* ((branch (magit-get-current-branch)))
    (or (magit-get-upstream-branch branch)
        (let ((remote (magit-get "branch" branch "remote"))
              (merge  (magit-get "branch" branch "merge"))
              (u (magit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((magit--unnamed-upstream-p remote merge)
            (concat u ", replacing unnamed"))
           ((magit--valid-upstream-p remote merge)
            (concat u ", replacing non-existent"))
           ((or remote merge)
            (concat u ", replacing invalid"))
           (t
            (concat u ", setting that")))))))

;;;###autoload
(defun magit-rebase-branch (target args)
  "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased."
  (interactive (list (magit-read-other-branch-or-commit "Rebase onto")
                     (magit-rebase-arguments)))
  (message "Rebasing...")
  (magit-git-rebase target args)
  (message "Rebasing...done"))

;;;###autoload
(defun magit-rebase-subset (newbase start args)
  "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits."
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase subset onto" nil
                      (magit-get-upstream-branch))
                     nil
                     (magit-rebase-arguments)))
  (if start
      (progn (message "Rebasing...")
             (magit-run-git-sequencer "rebase" "--onto" newbase start args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-subset ,newbase (concat commit "^") (list ,@args)))
      (concat "Type %p on a commit to rebase it "
              "and commits above it onto " newbase ","))))

(defvar magit-rebase-interactive-include-selected t)

(defun magit-rebase-interactive-1
    (commit args message &optional editor delay-edit-confirm noassert confirm)
  (declare (indent 2))
  (when commit
    (if (eq commit :merge-base)
        (setq commit
              (and-let* ((upstream (magit-get-upstream-branch)))
                (magit-git-string "merge-base" upstream "HEAD")))
      (unless (magit-rev-ancestor-p commit "HEAD")
        (user-error "%s isn't an ancestor of HEAD" commit))
      (if (magit-commit-parents commit)
          (when (or (not (eq this-command 'magit-rebase-interactive))
                    magit-rebase-interactive-include-selected)
            (setq commit (concat commit "^")))
        (setq args (cons "--root" args)))))
  (when (and commit (not noassert))
    (setq commit (magit-rebase-interactive-assert
                  commit delay-edit-confirm
                  (--some (string-prefix-p "--rebase-merges" it) args))))
  (if (and commit (not confirm))
      (let ((process-environment process-environment))
        (when editor
          (push (concat "GIT_SEQUENCE_EDITOR="
                        (if (functionp editor)
                            (funcall editor commit)
                          editor))
                process-environment))
        (magit-run-git-sequencer "rebase" "-i" args
                                 (and (not (member "--root" args)) commit)))
    (magit-log-select
      `(lambda (commit)
         ;; In some cases (currently just magit-rebase-remove-commit), "-c
         ;; commentChar=#" is added to the global arguments for git.  Ensure
         ;; that the same happens when we chose the commit via
         ;; magit-log-select, below.
         (let ((magit-git-global-arguments (list ,@magit-git-global-arguments)))
           (magit-rebase-interactive-1 commit (list ,@args)
             ,message ,editor ,delay-edit-confirm ,noassert)))
      message)))

(defvar magit--rebase-published-symbol nil)
(defvar magit--rebase-public-edit-confirmed nil)

(defun magit-rebase-interactive-assert
    (since &optional delay-edit-confirm rebase-merges)
  (let* ((commit (magit-rebase--target-commit since))
         (branches (magit-list-publishing-branches commit)))
    (setq magit--rebase-public-edit-confirmed
          (delete (magit-toplevel) magit--rebase-public-edit-confirmed))
    (when (and branches
               (or (not delay-edit-confirm)
                   ;; The user might have stopped at a published commit
                   ;; merely to add new commits *after* it.  Try not to
                   ;; ask users whether they really want to edit public
                   ;; commits, when they don't actually intend to do so.
                   (not (--all-p (magit-rev-equal it commit) branches))))
      (let ((m1 "Some of these commits have already been published to ")
            (m2 ".\nDo you really want to modify them"))
        (magit-confirm (or magit--rebase-published-symbol 'rebase-published)
          (concat m1 "%s" m2)
          (concat m1 "%d public branches" m2)
          nil branches))
      (push (magit-toplevel) magit--rebase-public-edit-confirmed)))
  (if (and (magit-git-lines "rev-list" "--merges" (concat since "..HEAD"))
           (not rebase-merges))
      (magit-read-char-case "Proceed despite merge in rebase range?  " nil
        (?c "[c]ontinue" since)
        (?s "[s]elect other" nil)
        (?a "[a]bort" (user-error "Quit")))
    since))

(defun magit-rebase--target-commit (since)
  (if (string-suffix-p "^" since)
      ;; If SINCE is "REV^", then the user selected
      ;; "REV", which is the first commit that will
      ;; be replaced.  (from^..to] <=> [from..to]
      (substring since 0 -1)
    ;; The "--root" argument is being used.
    since))

;;;###autoload
(defun magit-rebase-interactive (commit args)
  "Start an interactive rebase sequence."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to rebase it and all commits above it,"
    nil t))

;;;###autoload
(defun magit-rebase-autosquash (args)
  "Combine squash and fixup commits with their intended targets."
  (interactive (list (magit-rebase-arguments)))
  (magit-rebase-interactive-1 :merge-base
      (nconc (list "--autosquash" "--keep-empty") args)
    "Type %p on a commit to squash into it and then rebase as necessary,"
    "true" nil t))

;;;###autoload
(defun magit-rebase-edit-commit (commit args)
  "Edit a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to edit it,"
    (apply-partially #'magit-rebase--perl-editor 'edit)
    t))

;;;###autoload
(defun magit-rebase-reword-commit (commit args)
  "Reword a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to reword its message,"
    (apply-partially #'magit-rebase--perl-editor 'reword)))

;;;###autoload
(defun magit-rebase-remove-commit (commit args)
  "Remove a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  ;; magit-rebase--perl-editor assumes that the comment character is "#".
  (let ((magit-git-global-arguments
         (nconc (list "-c" "core.commentChar=#")
                magit-git-global-arguments)))
    (magit-rebase-interactive-1 commit args
      "Type %p on a commit to remove it,"
      (apply-partially #'magit-rebase--perl-editor 'remove)
      nil nil t)))

(defun magit-rebase--perl-editor (action since)
  (let ((commit (magit-rev-abbrev (magit-rebase--target-commit since))))
    (format "%s -i -p -e '++$x if not $x and s/^pick %s/%s %s/'"
            magit-perl-executable
            commit
            (cl-case action
              (edit   "edit")
              (remove "noop\n# pick")
              (reword "reword")
              (t      (error "unknown action: %s" action)))
            commit)))

;;;###autoload
(defun magit-rebase-continue (&optional noedit)
  "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is."
  (interactive "P")
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p t)
          (user-error "Cannot continue rebase with unstaged changes")
        (let ((dir (magit-gitdir)))
          (when (and (magit-anything-staged-p)
                     (file-exists-p (expand-file-name "rebase-merge" dir))
                     (not (member (magit-toplevel)
                                  magit--rebase-public-edit-confirmed)))
            (magit-commit-amend-assert
             (magit-file-line
              (expand-file-name "rebase-merge/orig-head" dir)))))
        (if noedit
            (with-environment-variables (("GIT_EDITOR" "true"))
              (magit-run-git-async (magit--rebase-resume-command) "--continue")
              (set-process-sentinel magit-this-process
                                    #'magit-sequencer-process-sentinel)
              magit-this-process)
          (magit-run-git-sequencer (magit--rebase-resume-command) "--continue")))
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-run-git-sequencer (magit--rebase-resume-command) "--skip"))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-run-git-sequencer "rebase" "--edit-todo"))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-confirm 'abort-rebase "Abort this rebase")
  (magit-run-git (magit--rebase-resume-command) "--abort"))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (let ((dir (magit-gitdir)))
    (or (file-exists-p (expand-file-name "rebase-merge" dir))
        (file-exists-p (expand-file-name "rebase-apply/onto" dir)))))

(defun magit--rebase-resume-command ()
  (if (file-exists-p (expand-file-name "rebase-recursive" (magit-gitdir)))
      "rbr"
    "rebase"))

(defun magit-rebase--get-state-lines (file)
  (and (magit-rebase-in-progress-p)
       (let ((dir (magit-gitdir)))
         (magit-file-line
          (expand-file-name
           (concat (if (file-directory-p (expand-file-name "rebase-merge" dir))
                       "rebase-merge/"
                     "rebase-apply/")
                   file)
           dir)))))

;;; Sections

(defun magit-insert-sequencer-sequence ()
  "Insert section for the on-going cherry-pick or revert sequence.
If no such sequence is in progress, do nothing."
  (let ((picking (magit-cherry-pick-in-progress-p)))
    (when (or picking (magit-revert-in-progress-p))
      (let ((dir (magit-gitdir)))
        (magit-insert-section (sequence)
          (magit-insert-heading (if picking "Cherry Picking" "Reverting"))
          (when-let ((lines (cdr (magit-file-lines
                                  (expand-file-name "sequencer/todo" dir)))))
            (dolist (line (nreverse lines))
              (when (string-match
                     "^\\(pick\\|revert\\) \\([^ ]+\\) \\(.*\\)$" line)
                (magit-bind-match-strings (cmd hash msg) line
                  (magit-insert-section (commit hash)
                    (insert (propertize cmd 'font-lock-face 'magit-sequence-pick)
                            " " (propertize hash 'font-lock-face 'magit-hash)
                            " " msg "\n"))))))
          (magit-sequence-insert-sequence
           (magit-file-line
            (expand-file-name (if picking "CHERRY_PICK_HEAD" "REVERT_HEAD")
                              dir))
           (magit-file-line (expand-file-name "sequencer/head" dir)))
          (insert "\n"))))))

(defun magit-insert-am-sequence ()
  "Insert section for the on-going patch applying sequence.
If no such sequence is in progress, do nothing."
  (when (magit-am-in-progress-p)
    (magit-insert-section (rebase-sequence)
      (magit-insert-heading "Applying patches")
      (let* ((patches (nreverse (magit-rebase-patches)))
             (dir (expand-file-name "rebase-apply" (magit-gitdir)))
             (i (string-to-number
                 (magit-file-line (expand-file-name "last" dir))))
             (cur (string-to-number
                   (magit-file-line (expand-file-name "next" dir))))
             patch commit)
        (while (and patches (>= i cur))
          (setq patch (pop patches))
          (setq commit (magit-commit-p
                        (cadr (split-string (magit-file-line patch)))))
          (cond ((and commit (= i cur))
                 (magit-sequence-insert-commit
                  "stop" commit 'magit-sequence-stop))
                ((= i cur)
                 (magit-sequence-insert-am-patch
                  "stop" patch 'magit-sequence-stop))
                (commit
                 (magit-sequence-insert-commit
                  "pick" commit 'magit-sequence-pick))
                (t
                 (magit-sequence-insert-am-patch
                  "pick" patch 'magit-sequence-pick)))
          (cl-decf i)))
      (magit-sequence-insert-sequence nil "ORIG_HEAD")
      (insert ?\n))))

(defun magit-sequence-insert-am-patch (type patch face)
  (magit-insert-section (file patch)
    (let ((title
           (with-temp-buffer
             (insert-file-contents patch nil nil 4096)
             (unless (re-search-forward "^Subject: " nil t)
               (goto-char (point-min)))
             (buffer-substring (point) (line-end-position)))))
      (insert (propertize type 'font-lock-face face)
              ?\s (propertize (file-name-nondirectory patch)
                              'font-lock-face 'magit-hash)
              ?\s title
              ?\n))))

(defun magit-insert-rebase-sequence ()
  "Insert section for the on-going rebase sequence.
If no such sequence is in progress, do nothing."
  (when (magit-rebase-in-progress-p)
    (let* ((gitdir (magit-gitdir))
           (interactive
            (file-directory-p (expand-file-name "rebase-merge" gitdir)))
           (dir  (if interactive "rebase-merge/" "rebase-apply/"))
           (name (thread-first (concat dir "head-name")
                   (expand-file-name gitdir)
                   magit-file-line))
           (onto (thread-first (concat dir "onto")
                   (expand-file-name gitdir)
                   magit-file-line))
           (onto (or (magit-rev-name onto name)
                     (magit-rev-name onto "refs/heads/*") onto))
           (name (or (magit-rev-name name "refs/heads/*") name)))
      (magit-insert-section (rebase-sequence)
        (magit-insert-heading (format "Rebasing %s onto %s" name onto))
        (if interactive
            (magit-rebase-insert-merge-sequence onto)
          (magit-rebase-insert-apply-sequence onto))
        (insert ?\n)))))

(defun magit-rebase--todo ()
  "Return `git-rebase-action' instances for remaining rebase actions.
These are ordered in that the same way they'll be sorted in the
status buffer (i.e., the reverse of how they will be applied)."
  (let ((comment-start (or (magit-get "core.commentChar") "#"))
        lines)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "rebase-merge/git-rebase-todo" (magit-gitdir)))
      (while (not (eobp))
        (let ((ln (git-rebase-current-line)))
          (when (oref ln action-type)
            (push ln lines)))
        (forward-line)))
    lines))

(defun magit-rebase-insert-merge-sequence (onto)
  (dolist (line (magit-rebase--todo))
    (with-slots (action-type action action-options target) line
      (pcase action-type
        ('commit
         (magit-sequence-insert-commit action target 'magit-sequence-pick))
        ((or (or `exec `label)
             (and `merge (guard (not action-options))))
         (insert (propertize action 'font-lock-face 'magit-sequence-onto) "\s"
                 (propertize target 'font-lock-face 'git-rebase-label) "\n"))
        ('merge
         (if-let ((hash (and (string-match "-[cC] \\([^ ]+\\)" action-options)
                             (match-string 1 action-options))))
             (magit-insert-section (commit hash)
               (magit-insert-heading
                 (propertize "merge" 'font-lock-face 'magit-sequence-pick)
                 "\s"
                 (magit-format-rev-summary hash) "\n"))
           (error "failed to parse merge message hash"))))))
  (let ((dir (magit-gitdir)))
    (magit-sequence-insert-sequence
     (magit-file-line (expand-file-name "rebase-merge/stopped-sha" dir))
     onto
     (and-let* ((lines (magit-file-lines
                        (expand-file-name "rebase-merge/done" dir))))
       (cadr (split-string (car (last lines))))))))

(defun magit-rebase-insert-apply-sequence (onto)
  (let* ((dir (magit-gitdir))
         (rewritten
          (--map (car (split-string it))
                 (magit-file-lines
                  (expand-file-name "rebase-apply/rewritten" dir))))
         (stop (magit-file-line
                (expand-file-name "rebase-apply/original-commit" dir))))
    (dolist (patch (nreverse (cdr (magit-rebase-patches))))
      (let ((hash (cadr (split-string (magit-file-line patch)))))
        (unless (or (member hash rewritten)
                    (equal hash stop))
          (magit-sequence-insert-commit "pick" hash 'magit-sequence-pick))))
    (magit-sequence-insert-sequence
     (magit-file-line (expand-file-name "rebase-apply/original-commit" dir))
     onto)))

(defun magit-rebase-patches ()
  (directory-files (expand-file-name "rebase-apply" (magit-gitdir))
                   t "\\`[0-9]\\{4\\}\\'"))

(defun magit-sequence-insert-sequence (stop onto &optional orig)
  (let ((head (magit-rev-parse "HEAD")) done)
    (setq onto (if onto (magit-rev-parse onto) head))
    (setq done (magit-git-lines "log" "--format=%H" (concat onto "..HEAD")))
    (when (and stop (not (member (magit-rev-parse stop) done)))
      (let ((id (magit-patch-id stop)))
        (if-let ((matched (--first (equal (magit-patch-id it) id) done)))
            (setq stop matched)
          (cond
           ((--first (magit-rev-equal it stop) done)
            ;; The commit's testament has been executed.
            (magit-sequence-insert-commit "void" stop 'magit-sequence-drop))
           ;; The faith of the commit is still undecided...
           ((magit-anything-unmerged-p)
            ;; ...and time travel isn't for the faint of heart.
            (magit-sequence-insert-commit "join" stop 'magit-sequence-part))
           ((magit-anything-modified-p t)
            ;; ...and the dust hasn't settled yet...
            (magit-sequence-insert-commit
             (let* ((magit--refresh-cache nil)
                    (staged   (magit-commit-tree "oO" nil "HEAD"))
                    (unstaged (magit-commit-worktree "oO" "--reset")))
               (cond
                ;; ...but we could end up at the same tree just by committing.
                ((or (magit-rev-equal staged   stop)
                     (magit-rev-equal unstaged stop))
                 "goal")
                ;; ...but the changes are still there, untainted.
                ((or (equal (magit-patch-id staged)   id)
                     (equal (magit-patch-id unstaged) id))
                 "same")
                ;; ...and some changes are gone and/or others were added.
                (t "work")))
             stop 'magit-sequence-part))
           ;; The commit is definitely gone...
           ((--first (magit-rev-equal it stop) done)
            ;; ...but all of its changes are still in effect.
            (magit-sequence-insert-commit "poof" stop 'magit-sequence-drop))
           (t
            ;; ...and some changes are gone and/or other changes were added.
            (magit-sequence-insert-commit "gone" stop 'magit-sequence-drop)))
          (setq stop nil))))
    (dolist (rev done)
      (apply #'magit-sequence-insert-commit
             (cond ((equal rev stop)
                    ;; ...but its reincarnation lives on.
                    ;; Or it didn't die in the first place.
                    (list (if (and (equal rev head)
                                   (equal (magit-patch-id rev)
                                          (magit-patch-id orig)))
                              "stop" ; We haven't done anything yet.
                            "like")  ; There are new commits.
                          rev (if (equal rev head)
                                  'magit-sequence-head
                                'magit-sequence-stop)))
                   ((equal rev head)
                    (list "done" rev 'magit-sequence-head))
                   (t
                    (list "done" rev 'magit-sequence-done)))))
    (magit-sequence-insert-commit "onto" onto
                                  (if (equal onto head)
                                      'magit-sequence-head
                                    'magit-sequence-onto))))

(defun magit-sequence-insert-commit (type hash face)
  (magit-insert-section (commit hash)
    (magit-insert-heading
      (propertize type 'font-lock-face face)    "\s"
      (magit-format-rev-summary hash) "\n")))

;;; _
(provide 'magit-sequence)
;;; magit-sequence.el ends here
