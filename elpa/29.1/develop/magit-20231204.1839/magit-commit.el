;;; magit-commit.el --- Create Git commits  -*- lexical-binding:t -*-

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

;; This library implements commands for creating Git commits.  These
;; commands just initiate the commit, support for writing the commit
;; messages is implemented in `git-commit.el'.

;;; Code:

(require 'magit)
(require 'magit-sequence)

;;; Options

(defcustom magit-commit-ask-to-stage 'verbose
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type '(choice (const :tag "Ask" t)
                 (const :tag "Ask showing diff" verbose)
                 (const :tag "Stage without confirmation" stage)
                 (const :tag "Don't ask" nil)))

(defcustom magit-commit-show-diff t
  "Whether the relevant diff is automatically shown when committing."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-extend-override-date t
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date t
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-post-commit-hook nil
  "Hook run after creating a commit without the user editing a message.

This hook is run by `magit-refresh' if `this-command' is a member
of `magit-post-commit-hook-commands'.  This only includes commands
named `magit-commit-*' that do *not* require that the user edits
the commit message in a buffer and then finishes by pressing
\\<with-editor-mode-map>\\[with-editor-finish].

Also see `git-commit-post-finish-hook'."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'hook)

(defcustom magit-commit-diff-inhibit-same-window nil
  "Whether to inhibit use of same window when showing diff while committing.

When writing a commit, then a diff of the changes to be committed
is automatically shown.  The idea is that the diff is shown in a
different window of the same frame and for most users that just
works.  In other words most users can completely ignore this
option because its value doesn't make a difference for them.

However for users who configured Emacs to never create a new
window even when the package explicitly tries to do so, then
displaying two new buffers necessarily means that the first is
immediately replaced by the second.  In our case the message
buffer is immediately replaced by the diff buffer, which is of
course highly undesirable.

A workaround is to suppress this user configuration in this
particular case.  Users have to explicitly opt-in by toggling
this option.  We cannot enable the workaround unconditionally
because that again causes issues for other users: if the frame
is too tiny or the relevant settings too aggressive, then the
diff buffer would end up being displayed in a new frame.

Also see https://github.com/magit/magit/issues/4132."
  :package-version '(magit . "3.3.0")
  :group 'magit-commands
  :type 'boolean)

;;; Popup

;;;###autoload (autoload 'magit-commit "magit-commit" nil t)
(transient-define-prefix magit-commit ()
  "Create a new commit or replace an existing commit."
  :info-manual "(magit)Initiating a Commit"
  :man-page "git-commit"
  ["Arguments"
   ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
   ("-e" "Allow empty commit"                     "--allow-empty")
   ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
   ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ("-R" "Claim authorship and reset author date" "--reset-author")
   (magit:--author :description "Override the author")
   (7 "-D" "Override the author date" "--date=" transient-read-date)
   ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
   (5 magit:--gpg-sign)
   (magit-commit:--reuse-message)]
  [["Create"
    ("c" "Commit"         magit-commit-create)]
   ["Edit HEAD"
    ("e" "Extend"         magit-commit-extend)
    ("w" "Reword"         magit-commit-reword)
    ("a" "Amend"          magit-commit-amend)
    (6 "n" "Reshelve"     magit-commit-reshelve)]
   ["Edit"
    ("f" "Fixup"          magit-commit-fixup)
    ("s" "Squash"         magit-commit-squash)
    ("A" "Augment"        magit-commit-augment)
    (6 "x" "Absorb changes" magit-commit-autofixup)
    (6 "X" "Absorb modules" magit-commit-absorb-modules)]
   [""
    ("F" "Instant fixup"  magit-commit-instant-fixup)
    ("S" "Instant squash" magit-commit-instant-squash)]]
  (interactive)
  (if-let ((buffer (magit-commit-message-buffer)))
      (switch-to-buffer buffer)
    (transient-setup 'magit-commit)))

(defun magit-commit-arguments nil
  (transient-args 'magit-commit))

(transient-define-argument magit-commit:--reuse-message ()
  :description "Reuse commit message"
  :class 'transient-option
  :shortarg "-C"
  :argument "--reuse-message="
  :reader #'magit-read-reuse-message
  :history-key 'magit-revision-history)

(defun magit-read-reuse-message (prompt &optional default history)
  (magit-completing-read prompt (magit-list-refnames)
                         nil nil nil history
                         (or default
                             (and (magit-rev-verify "ORIG_HEAD")
                                  "ORIG_HEAD"))))

;;; Commands

;;;###autoload
(defun magit-commit-create (&optional args)
  "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (magit-commit-arguments)))
                 (list (magit-commit-arguments))))
  (cond ((member "--all" args)
         (setq this-command 'magit-commit--all))
        ((member "--allow-empty" args)
         (setq this-command 'magit-commit--allow-empty)))
  (when (setq args (magit-commit-assert args))
    (let ((default-directory (magit-toplevel)))
      (magit-run-git-with-editor "commit" args))))

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list (magit-commit-arguments)))
  (magit-commit-amend-assert)
  (magit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun magit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  \n(git commit
--amend --no-edit)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-extend-override-date)
                       magit-commit-extend-override-date)))
  (when (setq args (magit-commit-assert args))
    (magit-commit-amend-assert)
    (if override-date
        (magit-run-git-with-editor "commit" "--amend" "--no-edit" args)
      (with-environment-variables
          (("GIT_COMMITTER_DATE" (magit-rev-format "%cD")))
        (magit-run-git-with-editor "commit" "--amend" "--no-edit" args)))))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (magit-commit-amend-assert)
  (cl-pushnew "--allow-empty" args :test #'equal)
  (if override-date
      (magit-run-git-with-editor "commit" "--amend" "--only" args)
    (with-environment-variables
        (("GIT_COMMITTER_DATE" (magit-rev-format "%cD")))
      (magit-run-git-with-editor "commit" "--amend" "--only" args))))

;;;###autoload
(defun magit-commit-fixup (&optional commit args)
  "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--fixup" commit args))

;;;###autoload
(defun magit-commit-squash (&optional commit args)
  "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args))

;;;###autoload
(defun magit-commit-augment (&optional commit args)
  "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args nil t))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--fixup" commit args t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit args)
  "Create a squash commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args t))

(defun magit-commit-squash-internal
    (option commit &optional args rebase edit confirmed)
  (when-let ((args (magit-commit-assert args (not edit))))
    (when commit
      (when (and rebase (not (magit-rev-ancestor-p commit "HEAD")))
        (magit-read-char-case
            (format "%s isn't an ancestor of HEAD.  " commit) nil
          (?c "[c]reate without rebasing" (setq rebase nil))
          (?s "[s]elect other"            (setq commit nil))
          (?a "[a]bort"                   (user-error "Quit")))))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          magit-commit-squash-confirm))))
        (let ((magit-commit-show-diff nil))
          (push (concat option "=" commit) args)
          (unless edit
            (push "--no-edit" args))
          (if rebase
              (magit-with-editor
                (magit-call-git
                 "commit" "--no-gpg-sign"
                 (seq-remove (apply-partially #'string-prefix-p "--gpg-sign=")
                             args)))
            (magit-run-git-with-editor "commit" args))
          t) ; The commit was created; used by below lambda.
      (let ((winconf (and magit-commit-show-diff
                          (current-window-configuration))))
        (magit-log-select
          (lambda (commit)
            (when (and (magit-commit-squash-internal option commit args
                                                     rebase edit t)
                       rebase)
              (magit-commit-amend-assert commit)
              (magit-rebase-interactive-1 commit
                  (list "--autosquash" "--autostash" "--keep-empty")
                "" "true" nil t))
            (when winconf
              (set-window-configuration winconf)))
          (format "Type %%p on a commit to %s into it,"
                  (substring option 2))
          nil nil nil commit))
      (when magit-commit-show-diff
        (let ((magit-display-buffer-noselect t))
          (apply #'magit-diff-staged nil (magit-diff-arguments)))))))

(defun magit-commit-amend-assert (&optional commit)
  (when-let ((branches (magit-list-publishing-branches commit)))
    (let ((m1 "This commit has already been published to ")
          (m2 ".\nDo you really want to modify it"))
      (magit-confirm 'amend-published
        (concat m1 "%s" m2)
        (concat m1 "%d public branches" m2)
        nil branches))))

(defun magit-commit-assert (args &optional strict)
  (cond
   ((or (magit-anything-staged-p)
        (and (magit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args)
                 (member "--reset-author" args)
                 (member "--signoff" args)
                 (transient-arg-value "--author=" args)
                 (transient-arg-value "--date=" args))))
    (or args (list "--")))
   ((and (magit-rebase-in-progress-p)
         (not (magit-anything-unstaged-p))
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (setq this-command #'magit-rebase-continue)
    (magit-run-git-sequencer "rebase" "--continue")
    nil)
   ((file-exists-p (expand-file-name "MERGE_MSG" (magit-gitdir)))
    (cond ((magit-anything-unmerged-p)
           (user-error "Unresolved conflicts"))
          ((and (magit-anything-unstaged-p)
                (not (y-or-n-p
                      "Proceed with merge despite unstaged changes? ")))
           (user-error "Abort"))
          ((or args (list "--")))))
   ((not (magit-anything-unstaged-p))
    (user-error "Nothing staged (or unstaged)"))
   (magit-commit-ask-to-stage
    (when (eq magit-commit-ask-to-stage 'verbose)
      (magit-diff-unstaged))
    (prog1 (when (or (eq magit-commit-ask-to-stage 'stage)
                     (y-or-n-p
                      "Nothing staged.  Commit all uncommitted changes? "))
             (setq this-command 'magit-commit--all)
             (cons "--all" (or args (list "--"))))
      (when (and (eq magit-commit-ask-to-stage 'verbose)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

(defvar magit--reshelve-history nil)

;;;###autoload
(defun magit-commit-reshelve (date update-author &optional args)
  "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changed, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil."
  (interactive
   (let ((update-author (and (magit-rev-author-p "HEAD")
                             (not current-prefix-arg))))
     (push (magit-rev-format (if update-author "%ad" "%cd") "HEAD"
                             (concat "--date=format:%F %T %z"))
           magit--reshelve-history)
     (list (read-string (if update-author
                            "Change author and committer dates to: "
                          "Change committer date to: ")
                        (cons (format-time-string "%F %T %z") 17)
                        'magit--reshelve-history)
           update-author
           (magit-commit-arguments))))
  (with-environment-variables (("GIT_COMMITTER_DATE" date))
    (magit-run-git "commit" "--amend" "--no-edit"
                   (and update-author (concat "--date=" date))
                   args)))

;;;###autoload
(defun magit-commit-absorb-modules (phase commit)
  "Spread modified modules across recent commits."
  (interactive (list 'select (magit-get-upstream-branch)))
  (let ((modules (magit-list-modified-modules)))
    (unless modules
      (user-error "There are no modified modules that could be absorbed"))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn
          (dolist (module modules)
            (when-let ((msg (magit-git-string
                             "log" "-1" "--format=%s"
                             (concat commit "..") "--" module)))
              (magit-git "commit" "-m" (concat "fixup! " msg)
                         "--only" "--" module)))
          (magit-refresh)
          t)
      (magit-log-select
        (lambda (commit)
          (magit-commit-absorb-modules 'run commit))
        nil nil nil nil commit))))

;;;###autoload (autoload 'magit-commit-absorb "magit-commit" nil t)
(transient-define-prefix magit-commit-absorb (phase commit args)
  "Spread staged changes across recent commits.
With a prefix argument use a transient command to select infix
arguments.  This command requires git-absorb executable, which
is available from https://github.com/tummychow/git-absorb.
See `magit-commit-autofixup' for an alternative implementation."
  ["Arguments"
   ("-f" "Skip safety checks"       ("-f" "--force"))
   ("-v" "Display more output"      ("-v" "--verbose"))]
  ["Actions"
   ("x"  "Absorb" magit-commit-absorb)]
  (interactive (if current-prefix-arg
                   (list 'transient nil nil)
                 (list 'select
                       (magit-get-upstream-branch)
                       (transient-args 'magit-commit-absorb))))
  (if (eq phase 'transient)
      (transient-setup 'magit-commit-absorb)
    (unless (magit-git-executable-find "git-absorb")
      (user-error "This command requires the git-absorb executable, which %s"
                  "is available from https://github.com/tummychow/git-absorb"))
    (unless (magit-anything-staged-p)
      (if (magit-anything-unstaged-p)
          (if (y-or-n-p "Nothing staged.  Absorb all unstaged changes? ")
              (magit-with-toplevel
                (magit-run-git "add" "-u" "."))
            (user-error "Abort"))
        (user-error "There are no changes that could be absorbed")))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn (magit-run-git-async "absorb" "-v" args "-b" commit) t)
      (magit-log-select
        (lambda (commit)
          (with-no-warnings ; about non-interactive use
            (magit-commit-absorb 'run commit args)))
        nil nil nil nil commit))))

;;;###autoload (autoload 'magit-commit-autofixup "magit-commit" nil t)
(transient-define-prefix magit-commit-autofixup (phase commit args)
  "Spread staged or unstaged changes across recent commits.

If there are any staged then spread only those, otherwise
spread all unstaged changes. With a prefix argument use a
transient command to select infix arguments.

This command requires the git-autofixup script, which is
available from https://github.com/torbiak/git-autofixup.
See `magit-commit-absorb' for an alternative implementation."
  ["Arguments"
   (magit-autofixup:--context)
   (magit-autofixup:--strict)]
  ["Actions"
   ("x"  "Absorb" magit-commit-autofixup)]
  (interactive (if current-prefix-arg
                   (list 'transient nil nil)
                 (list 'select
                       (magit-get-upstream-branch)
                       (transient-args 'magit-commit-autofixup))))
  (if (eq phase 'transient)
      (transient-setup 'magit-commit-autofixup)
    (unless (magit-git-executable-find "git-autofixup")
      (user-error "This command requires the git-autofixup script, which %s"
                  "is available from https://github.com/torbiak/git-autofixup"))
    (unless (magit-anything-modified-p)
      (user-error "There are no changes that could be absorbed"))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn (magit-run-git-async "autofixup" "-vv" args commit) t)
      (magit-log-select
        (lambda (commit)
          (with-no-warnings ; about non-interactive use
            (magit-commit-autofixup 'run commit args)))
        nil nil nil nil commit))))

(transient-define-argument magit-autofixup:--context ()
  :description "Diff context lines"
  :class 'transient-option
  :shortarg "-c"
  :argument "--context="
  :reader #'transient-read-number-N0)

(transient-define-argument magit-autofixup:--strict ()
  :description "Strictness"
  :class 'transient-option
  :shortarg "-s"
  :argument "--strict="
  :reader #'transient-read-number-N0)

(defvar magit-post-commit-hook-commands
  '(magit-commit-extend
    magit-commit-fixup
    magit-commit-augment
    magit-commit-instant-fixup
    magit-commit-instant-squash))

(defun magit-run-post-commit-hook ()
  (when (and (not this-command)
             (memq last-command magit-post-commit-hook-commands))
    (run-hooks 'magit-post-commit-hook)))

;;; Pending Diff

(defun magit-commit-diff ()
  (magit-repository-local-set 'this-commit-command
                              (if (eq this-command 'with-editor-finish)
                                  'magit-commit--rebase
                                last-command))
  (when (and git-commit-mode magit-commit-show-diff)
    (when-let ((diff-buffer (magit-get-mode-buffer 'magit-diff-mode)))
      ;; This window just started displaying the commit message
      ;; buffer.  Without this that buffer would immediately be
      ;; replaced with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (message "Diffing changes to be committed (C-g to abort diffing)")
    (let ((inhibit-quit nil))
      (condition-case nil
          (magit-commit-diff-1)
        (quit)))))

(defun magit-commit-diff-1 ()
  (let ((rev nil)
        (arg "--cached")
        (command (magit-repository-local-get 'this-commit-command))
        (staged (magit-anything-staged-p))
        (unstaged
         ;; Escape $GIT_DIR because `magit-anything-unstaged-p'
         ;; requires a working tree.
         (magit-with-toplevel
           (magit-anything-unstaged-p)))
        (squash (let ((f (expand-file-name "rebase-merge/rewritten-pending"
                                           (magit-gitdir))))
                  (and (file-exists-p f) (length (magit-file-lines f)))))
        (noalt nil))
    (pcase (list staged unstaged command)
      ((and `(,_ ,_ magit-commit--rebase)
            (guard (integerp squash)))
       (setq rev (format "HEAD~%s" squash)))
      (`(,_ ,_ magit-commit-amend)
       (setq rev "HEAD^"))
      (`(nil nil magit-commit--allow-empty)
       (setq rev "HEAD")
       (setq arg nil))
      ((or `(,_ ,_ magit-commit-reword)
           `(nil nil ,_))
       (setq rev "HEAD^..HEAD")
       (setq arg nil))
      (`(,_ t magit-commit--all)
       (setq rev "HEAD")
       (setq arg nil))
      (`(nil t handle-switch-frame)
       ;; Either --all or --allow-empty. Assume it is the former.
       (setq rev "HEAD")
       (setq arg nil)))
    (cond
     ((not
       (and (eq this-command 'magit-diff-while-committing)
            (and-let* ((buf (magit-get-mode-buffer
                             'magit-diff-mode nil 'selected)))
              (and (equal rev (buffer-local-value 'magit-buffer-range buf))
                   (equal arg (buffer-local-value 'magit-buffer-typearg buf)))))))
     ((eq command 'magit-commit-amend)
      (setq rev nil))
     ((or squash
          (file-exists-p (expand-file-name "rebase-merge/amend" (magit-gitdir))))
      (setq rev "HEAD^"))
     (t
      (message "No alternative diff while committing")
      (setq noalt t)))
    (unless noalt
      (let ((magit-inhibit-save-previous-winconf 'unset)
            (magit-display-buffer-noselect t)
            (display-buffer-overriding-action
             display-buffer-overriding-action))
        (when magit-commit-diff-inhibit-same-window
          (setq display-buffer-overriding-action
                '(nil (inhibit-same-window . t))))
        (magit-diff-setup-buffer rev arg (car (magit-diff-arguments)) nil
                                 (cond ((equal rev "HEAD") 'staged)
                                       ((equal rev "HEAD^..HEAD") 'committed)
                                       ('undefined)))))))

(add-hook 'server-switch-hook #'magit-commit-diff)
(add-hook 'with-editor-filter-visit-hook #'magit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp #'switch-to-buffer))

(defun magit-commit--reset-command ()
  (magit-repository-local-delete 'this-commit-command))

;;; Message Utilities

(defun magit-commit-message-buffer ()
  (let* ((find-file-visit-truename t) ; git uses truename of COMMIT_EDITMSG
         (topdir (magit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

(defvar magit-commit-add-log-insert-function #'magit-commit-add-log-insert
  "Used by `magit-commit-add-log' to insert a single entry.")

(defun magit-commit-add-log ()
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (pcase-let* ((hunk (and (magit-section-match 'hunk)
                          (magit-current-section)))
               (log  (magit-commit-message-buffer))
               (`(,buf ,pos) (magit-diff-visit-file--noselect)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit-create)
      (while (not (setq log (magit-commit-message-buffer)))
        (sit-for 0.01)))
    (magit--with-temp-position buf pos
      (funcall magit-commit-add-log-insert-function log
               (magit-file-relative-name)
               (and hunk (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                                 nil t)
             (when (equal (match-string 1) defun)
               (setq defun nil))
             (re-search-forward ": "))
            (t
             (when (re-search-backward "^[\\*(].+\n" nil t)
               (goto-char (match-end 0)))
             (while (re-search-forward "^[^\\*\n].*\n" nil t))
             (if defun
                 (progn (insert (format "* %s (%s): \n" file defun))
                        (setq defun nil))
               (insert (format "* %s: \n" file)))
             (backward-char)
             (unless (looking-at "\n[\n\\']")
               (insert ?\n)
               (backward-char))))
      (when defun
        (forward-line)
        (let ((limit (save-excursion
                       (and (re-search-forward "^\\*" nil t)
                            (point)))))
          (unless (or (looking-back (format "(%s): " defun)
                                    (line-beginning-position))
                      (re-search-forward (format "^(%s): " defun) limit t))
            (while (re-search-forward "^[^\\*\n].*\n" limit t))
            (insert (format "(%s): \n" defun))
            (backward-char)))))))

;;; _
(provide 'magit-commit)
;;; magit-commit.el ends here
