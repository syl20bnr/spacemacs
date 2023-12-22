;;; magit-extras.el --- Additional functionality for Magit  -*- lexical-binding:t -*-

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

;;; Commentary:

;; Additional functionality for Magit.

;;; Code:

(require 'magit)

;; For `magit-do-async-shell-command'.
(declare-function dired-read-shell-command "dired-aux" (prompt arg files))
;; For `magit-project-status'.
(declare-function vc-git-command "vc-git"
                  (buffer okstatus file-or-list &rest flags))

(defvar ido-exit)
(defvar ido-fallback)
(defvar project-prefix-map)
(defvar project-switch-commands)

(defgroup magit-extras nil
  "Additional functionality for Magit."
  :group 'magit-extensions)

;;; Git Tools
;;;; Git-Mergetool

;;;###autoload (autoload 'magit-git-mergetool "magit-extras" nil t)
(transient-define-prefix magit-git-mergetool (file args &optional transient)
  "Resolve conflicts in FILE using \"git mergetool --gui\".
With a prefix argument allow changing ARGS using a transient
popup.  See info node `(magit) Ediffing' for information about
alternative commands."
  :man-page "git-mergetool"
  ["Settings"
   ("-t" magit-git-mergetool:--tool)
   ("=t" magit-merge.guitool)
   ("=T" magit-merge.tool)
   ("-r" magit-mergetool.hideResolved)
   ("-b" magit-mergetool.keepBackup)
   ("-k" magit-mergetool.keepTemporaries)
   ("-w" magit-mergetool.writeToTemp)]
  ["Actions"
   (" m" "Invoke mergetool" magit-git-mergetool)]
  (interactive
   (if (and (not (eq transient-current-prefix 'magit-git-mergetool))
            current-prefix-arg)
       (list nil nil t)
     (list (magit-read-unmerged-file "Resolve")
           (transient-args 'magit-git-mergetool))))
  (if transient
      (transient-setup 'magit-git-mergetool)
    (magit-run-git-async "mergetool" "--gui" args "--" file)))

(transient-define-infix magit-git-mergetool:--tool ()
  :description "Override mergetool"
  :class 'transient-option
  :shortarg "-t"
  :argument "--tool="
  :reader #'magit--read-mergetool)

(transient-define-infix magit-merge.guitool ()
  :class 'magit--git-variable
  :variable "merge.guitool"
  :global t
  :reader #'magit--read-mergetool)

(transient-define-infix magit-merge.tool ()
  :class 'magit--git-variable
  :variable "merge.tool"
  :global t
  :reader #'magit--read-mergetool)

(defun magit--read-mergetool (prompt _initial-input history)
  (let ((choices nil)
        (lines (cdr (magit-git-lines "mergetool" "--tool-help"))))
    (while (string-prefix-p "\t\t" (car lines))
      (push (substring (pop lines) 2) choices))
    (setq choices (nreverse choices))
    (magit-completing-read (or prompt "Select mergetool")
                           choices nil t nil history)))

(transient-define-infix magit-mergetool.hideResolved ()
  :class 'magit--git-variable:boolean
  :variable "mergetool.hideResolved"
  :default "false"
  :global t)

(transient-define-infix magit-mergetool.keepBackup ()
  :class 'magit--git-variable:boolean
  :variable "mergetool.keepBackup"
  :default "true"
  :global t)

(transient-define-infix magit-mergetool.keepTemporaries ()
  :class 'magit--git-variable:boolean
  :variable "mergetool.keepTemporaries"
  :default "false"
  :global t)

(transient-define-infix magit-mergetool.writeToTemp ()
  :class 'magit--git-variable:boolean
  :variable "mergetool.writeToTemp"
  :default "false"
  :global t)

;;;; Git-Gui

;;;###autoload
(defun magit-run-git-gui-blame (commit filename &optional linenum)
  "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on."
  (interactive
   (let (revision filename)
     (when (or current-prefix-arg
               (progn
                 (setq revision "HEAD")
                 (not (setq filename (magit-file-relative-name nil 'tracked)))))
       (setq revision (magit-read-branch-or-commit "Blame from revision"))
       (setq filename (magit-read-file-from-rev revision "Blame file")))
     (list revision filename
           (and (equal filename
                       (ignore-errors
                         (magit-file-relative-name buffer-file-name)))
                (line-number-at-pos)))))
  (magit-with-toplevel
    (magit-process-git 0 "gui" "blame"
                       (and linenum (list (format "--line=%d" linenum)))
                       commit
                       filename)))

;;;; Gitk

(defcustom magit-gitk-executable
  (or (and (eq system-type 'windows-nt)
           (let ((exe (magit-git-string
                       "-c" "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                       "X" "gitk.exe")))
             (and exe (file-executable-p exe) exe)))
      (executable-find "gitk") "gitk")
  "The Gitk executable."
  :group 'magit-extras
  :set-after '(magit-git-executable)
  :type 'string)

;;;###autoload
(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (magit-with-toplevel (magit-process-git 0 "gui")))

;;;###autoload
(defun magit-run-gitk ()
  "Run `gitk' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0))

;;;###autoload
(defun magit-run-gitk-branches ()
  "Run `gitk --branches' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0 nil "--branches"))

;;;###autoload
(defun magit-run-gitk-all ()
  "Run `gitk --all' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0 nil "--all"))

;;; Emacs Tools

;;;###autoload
(defun ido-enter-magit-status ()
  "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (keymap-set ido-completion-map
                          \"C-x g\" \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (keymap-set ido-common-completion-map
              \"C-x g\" \\='ido-enter-magit-status)"
  (interactive)
  (setq ido-exit 'fallback)
  (setq ido-fallback #'magit-status)                ; for Emacs >= 26.2
  (with-no-warnings (setq fallback #'magit-status)) ; for Emacs 25
  (exit-minibuffer))

;;;###autoload
(defun magit-project-status ()
  "Run `magit-status' in the current project's root."
  (interactive)
  (if (fboundp 'project-root)
      (magit-status-setup-buffer (project-root (project-current t)))
    (user-error "`magit-project-status' requires `project' 0.3.0 or greater")))

(defvar magit-bind-magit-project-status t
  "Whether to bind \"m\" to `magit-project-status' in `project-prefix-map'.
If so, then an entry is added to `project-switch-commands' as
well.  If you want to use another key, then you must set this
to nil before loading Magit to prevent \"m\" from being bound.")

(with-eval-after-load 'project
  ;; Only more recent versions of project.el have `project-prefix-map' and
  ;; `project-switch-commands', though project.el is available in Emacs 25.
  (when (and magit-bind-magit-project-status
             (boundp 'project-prefix-map)
             ;; Only modify if it hasn't already been modified.
             (equal project-switch-commands
                    (eval (car (get 'project-switch-commands 'standard-value))
                          t)))
    (keymap-set project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window
              (and-let* ((file (magit-file-at-point)))
                (expand-file-name (if (file-directory-p file)
                                      (file-name-as-directory file)
                                    file)))))

;;;###autoload
(defun magit-dired-log (&optional follow)
  "Show log for all marked files, or the current file."
  (interactive "P")
  (if-let ((topdir (magit-toplevel default-directory)))
      (let ((args (car (magit-log-arguments)))
            (files (dired-get-marked-files nil nil #'magit-file-tracked-p)))
        (unless files
          (user-error "No marked file is being tracked by Git"))
        (when (and follow
                   (not (member "--follow" args))
                   (not (cdr files)))
          (push "--follow" args))
        (magit-log-setup-buffer
         (list (or (magit-get-current-branch) "HEAD"))
         args
         (let ((default-directory topdir))
           (mapcar #'file-relative-name files))
         magit-log-buffer-file-locked))
    (magit--not-inside-repository-error)))

;;;###autoload
(defun magit-dired-am-apply-patches (repo &optional arg)
  "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository."
  (interactive (list (or (magit-toplevel)
                         (magit-read-repository t))
                     current-prefix-arg))
  ;; Note: The ERROR argument of `dired-get-marked-files' isn't
  ;; available until Emacs 27.
  (let ((files (or (dired-get-marked-files nil arg)
                   (user-error "No files specified"))))
    (magit-status-setup-buffer repo)
    (magit-am-apply-patches files)))

;;;###autoload
(defun magit-do-async-shell-command (file)
  "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point."
  (interactive (list (or (magit-file-at-point)
                         (magit-read-file "Act on file"))))
  (require 'dired-aux)
  (dired-do-async-shell-command
   (dired-read-shell-command "& on %s: " current-prefix-arg (list file))
   nil (list file)))

;;; Shift Selection

(defun magit--turn-on-shift-select-mode-p ()
  (and shift-select-mode
       this-command-keys-shift-translated
       (not mark-active)
       (not (eq (car-safe transient-mark-mode) 'only))))

;;;###autoload
(defun magit-previous-line (&optional arg try-vscroll)
  "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference."
  (declare (interactive-only
            "use `forward-line' with negative argument instead."))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (magit-diff-inside-hunk-body-p)
                  (magit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (magit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (previous-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;;###autoload
(defun magit-next-line (&optional arg try-vscroll)
  "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference."
  (declare (interactive-only forward-line))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (magit-diff-inside-hunk-body-p)
                  (magit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (magit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (next-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;; Clean

;;;###autoload
(defun magit-clean (&optional arg)
  "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.
\n(git clean -f -d [-x|-X])"
  (interactive "p")
  (when (yes-or-no-p (format "Remove %s files? "
                             (pcase arg
                               (1 "untracked")
                               (4 "untracked and ignored")
                               (_ "ignored"))))
    (magit-wip-commit-before-change)
    (magit-run-git "clean" "-f" "-d" (pcase arg (4 "-x") (16 "-X")))))

(put 'magit-clean 'disabled t)

;;; ChangeLog

;;;###autoload
(defun magit-generate-changelog (&optional amending)
  "Insert ChangeLog entries into the current buffer.

The entries are generated from the diff being committed.
If prefix argument, AMENDING, is non-nil, include changes
in HEAD as well as staged changes in the diff to check."
  (interactive "P")
  (unless (magit-commit-message-buffer)
    (user-error "No commit in progress"))
  (require 'diff-mode) ; `diff-add-log-current-defuns'.
  (require 'vc-git)    ; `vc-git-diff'.
  (require 'add-log)   ; `change-log-insert-entries'.
  (cond
   ((and (fboundp 'change-log-insert-entries)
         (fboundp 'diff-add-log-current-defuns))
    (setq default-directory
          (if (and (file-regular-p "gitdir")
                   (not (magit-git-true "rev-parse" "--is-inside-work-tree"))
                   (magit-git-true "rev-parse" "--is-inside-git-dir"))
              (file-name-directory (magit-file-line "gitdir"))
            (magit-toplevel)))
    (let ((rev1 (if amending "HEAD^1" "HEAD"))
          (rev2 nil))
      ;; Magit may have updated the files without notifying vc, but
      ;; `diff-add-log-current-defuns' relies on vc being up-to-date.
      (mapc #'vc-file-clearprops (magit-staged-files))
      (change-log-insert-entries
       (with-temp-buffer
         (vc-git-command (current-buffer) 1 nil
                         "diff-index" "--exit-code" "--patch"
                         (and (magit-anything-staged-p) "--cached")
                         rev1 "--")
         ;; `diff-find-source-location' consults these vars.
         (defvar diff-vc-revisions)
         (setq-local diff-vc-revisions (list rev1 rev2))
         (setq-local diff-vc-backend 'Git)
         (diff-add-log-current-defuns)))))
   (t (user-error "`magit-generate-changelog' requires Emacs 27 or greater"))))

;;;###autoload
(defun magit-add-change-log-entry (&optional whoami file-name other-window)
  "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer."
  (interactive (list current-prefix-arg
                     (prompt-for-change-log-name)))
  (pcase-let ((`(,buf ,pos) (magit-diff-visit-file--noselect)))
    (magit--with-temp-position buf pos
      (let ((add-log-buffer-file-name-function
             (lambda ()
               (or magit-buffer-file-name
                   (buffer-file-name)))))
        (add-change-log-entry whoami file-name other-window)))))

;;;###autoload
(defun magit-add-change-log-entry-other-window (&optional whoami file-name)
  "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer."
  (interactive (and current-prefix-arg
                    (list current-prefix-arg
                          (prompt-for-change-log-name))))
  (magit-add-change-log-entry whoami file-name t))

;;; Edit Line Commit

;;;###autoload
(defun magit-edit-line-commit (&optional type)
  "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise."
  (interactive (list (and current-prefix-arg 'removal)))
  (let* ((chunk (magit-current-blame-chunk (or type 'addition)))
         (rev   (oref chunk orig-rev)))
    (if (string-match-p "\\`0\\{40,\\}\\'" rev)
        (message "This line has not been committed yet")
      (let ((rebase (magit-rev-ancestor-p rev "HEAD"))
            (file   (expand-file-name (oref chunk orig-file)
                                      (magit-toplevel))))
        (if rebase
            (let ((magit--rebase-published-symbol 'edit-published))
              (magit-rebase-edit-commit rev (magit-rebase-arguments)))
          (magit--checkout (or (magit-rev-branch rev) rev)))
        (unless (and buffer-file-name
                     (file-equal-p file buffer-file-name))
          (let ((blame-type (and magit-blame-mode magit-blame-type)))
            (if rebase
                (set-process-sentinel
                 magit-this-process
                 (lambda (process event)
                   (magit-sequencer-process-sentinel process event)
                   (when (eq (process-status process) 'exit)
                     (find-file file)
                     (when blame-type
                       (magit-blame--pre-blame-setup blame-type)
                       (magit-blame--run (magit-blame-arguments))))))
              (find-file file)
              (when blame-type
                (magit-blame--pre-blame-setup blame-type)
                (magit-blame--run (magit-blame-arguments))))))))))

(put 'magit-edit-line-commit 'disabled t)

;;;###autoload
(defun magit-diff-edit-hunk-commit (file)
  "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-commit' instead of this command."
  (interactive (list (magit-file-at-point t t)))
  (let ((magit-diff-visit-previous-blob nil))
    (with-current-buffer
        (magit-diff-visit-file--internal file nil #'pop-to-buffer-same-window)
      (magit-edit-line-commit))))

(put 'magit-diff-edit-hunk-commit 'disabled t)

;;; Reshelve

(defcustom magit-reshelve-since-committer-only nil
  "Whether `magit-reshelve-since' changes only the committer dates.
Otherwise the author dates are also changed."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'boolean)

;;;###autoload
(defun magit-reshelve-since (rev keyid)
  "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'."
  (interactive (list nil
                     (transient-arg-value "--gpg-sign="
                                          (magit-rebase-arguments))))
  (let* ((current (or (magit-get-current-branch)
                      (user-error "Refusing to reshelve detached head")))
         (backup (concat "refs/original/refs/heads/" current)))
    (cond
     ((not rev)
      (when (and (magit-ref-p backup)
                 (not (magit-y-or-n-p
                       (format "Backup ref %s already exists.  Override? " backup))))
        (user-error "Abort"))
      (magit-log-select
        (lambda (rev)
          (magit-reshelve-since rev keyid))
        "Type %p on a commit to reshelve it and the commits above it,"))
     (t
      (cl-flet ((adjust (time offset)
                  (format-time-string
                   "%F %T %z"
                   (+ (floor time)
                      (* offset 60)
                      (- (car (decode-time time)))))))
        (let* ((start (concat rev "^"))
               (range (concat start ".." current))
               (time-rev (adjust (float-time (string-to-number
                                              (magit-rev-format "%at" start)))
                                 1))
               (time-now (adjust (float-time)
                                 (- (string-to-number
                                     (magit-git-string "rev-list" "--count"
                                                       range))))))
          (push time-rev magit--reshelve-history)
          (let ((date (floor
                       (float-time
                        (date-to-time
                         (read-string "Date for first commit: "
                                      time-now 'magit--reshelve-history))))))
            (with-environment-variables (("FILTER_BRANCH_SQUELCH_WARNING" "1"))
              (magit-with-toplevel
                (magit-run-git-async
                 "filter-branch" "--force" "--env-filter"
                 (format
                  "case $GIT_COMMIT in %s\nesac"
                  (mapconcat
                   (lambda (rev)
                     (prog1
                         (concat
                          (format "%s) " rev)
                          (and (not magit-reshelve-since-committer-only)
                               (format "export GIT_AUTHOR_DATE=\"%s\"; " date))
                          (format "export GIT_COMMITTER_DATE=\"%s\";;" date))
                       (cl-incf date 60)))
                   (magit-git-lines "rev-list" "--reverse" range)
                   " "))
                 (and keyid
                      (list "--commit-filter"
                            (format "git commit-tree --gpg-sign=%s \"$@\";"
                                    keyid)))
                 range "--"))
              (set-process-sentinel
               magit-this-process
               (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   (if (> (process-exit-status process) 0)
                       (magit-process-sentinel process event)
                     (process-put process 'inhibit-refresh t)
                     (magit-process-sentinel process event)
                     (magit-run-git "update-ref" "-d" backup)))))))))))))

;;; Revision Stack

(defvar magit-revision-stack nil)

(defcustom magit-pop-revision-stack-format
  '("[%N: %h] "
    "%N: %cs %H\n   %s\n"
    "\\[\\([0-9]+\\)[]:]")
  "Control how `magit-pop-revision-stack' inserts a revision.

The command `magit-pop-revision-stack' inserts a representation
of the revision last pushed to the `magit-revision-stack' into
the current buffer.  It inserts text at point and/or near the end
of the buffer, and removes the consumed revision from the stack.

The entries on the stack have the format (HASH TOPLEVEL) and this
option has the format (POINT-FORMAT EOB-FORMAT INDEX-REGEXP), all
of which may be nil or a string (though either one of EOB-FORMAT
or POINT-FORMAT should be a string, and if INDEX-REGEXP is
non-nil, then the two formats should be too).

First INDEX-REGEXP is used to find the previously inserted entry,
by searching backward from point.  The first submatch must match
the index number.  That number is incremented by one, and becomes
the index number of the entry to be inserted.  If you don't want
to number the inserted revisions, then use nil for INDEX-REGEXP.

If INDEX-REGEXP is non-nil, then both POINT-FORMAT and EOB-FORMAT
should contain \"%N\", which is replaced with the number that was
determined in the previous step.

Both formats, if non-nil and after removing %N, are then expanded
using `git show --format=FORMAT ...' inside TOPLEVEL.

The expansion of POINT-FORMAT is inserted at point, and the
expansion of EOB-FORMAT is inserted at the end of the buffer (if
the buffer ends with a comment, then it is inserted right before
that)."
  :package-version '(magit . "3.2.0")
  :group 'magit-commands
  :type '(list (choice (string :tag "Insert at point format")
                       (cons (string :tag "Insert at point format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at point" nil))
               (choice (string :tag "Insert at eob format")
                       (cons (string :tag "Insert at eob format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at eob" nil))
               (choice (regexp :tag "Find index regexp")
                       (const :tag "Don't number entries" nil))))

(defcustom magit-copy-revision-abbreviated nil
  "Whether to save abbreviated revision to `kill-ring' and `magit-revision-stack'."
  :package-version '(magit . "3.0.0")
  :group 'magit-miscellaneous
  :type 'boolean)

;;;###autoload
(defun magit-pop-revision-stack (rev toplevel)
  "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g., while composing a commit
message), then that repository is used.  Otherwise (e.g., while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too."
  (interactive
   (if (or current-prefix-arg (not magit-revision-stack))
       (let ((default-directory
              (or (and (not (= (prefix-numeric-value current-prefix-arg) 16))
                       (or (magit-toplevel)
                           (cadr (car magit-revision-stack))))
                  (magit-read-repository))))
         (list (magit-read-branch-or-commit "Insert revision")
               default-directory))
     (push (caar magit-revision-stack) magit-revision-history)
     (pop magit-revision-stack)))
  (if rev
      (pcase-let ((`(,pnt-format ,eob-format ,idx-format)
                   magit-pop-revision-stack-format))
        (let ((default-directory toplevel)
              (idx (and idx-format
                        (save-excursion
                          (if (re-search-backward idx-format nil t)
                              (number-to-string
                               (1+ (string-to-number (match-string 1))))
                            "1"))))
              pnt-args eob-args)
          (when (listp pnt-format)
            (setq pnt-args (cdr pnt-format))
            (setq pnt-format (car pnt-format)))
          (when (listp eob-format)
            (setq eob-args (cdr eob-format))
            (setq eob-format (car eob-format)))
          (when pnt-format
            (when idx-format
              (setq pnt-format
                    (string-replace "%N" idx pnt-format)))
            (magit-rev-insert-format pnt-format rev pnt-args)
            (delete-char -1))
          (when eob-format
            (when idx-format
              (setq eob-format
                    (string-replace "%N" idx eob-format)))
            (save-excursion
              (goto-char (point-max))
              (skip-syntax-backward ">-")
              (beginning-of-line)
              (if (and comment-start (looking-at comment-start))
                  (while (looking-at comment-start)
                    (forward-line -1))
                (forward-line)
                (unless (= (current-column) 0)
                  (insert ?\n)))
              (insert ?\n)
              (magit-rev-insert-format eob-format rev eob-args)
              (delete-char -1)))))
    (user-error "Revision stack is empty")))

(keymap-set git-commit-mode-map "C-c C-w" #'magit-pop-revision-stack)

;;;###autoload
(defun magit-copy-section-value (arg)
  "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument."
  (interactive "P")
  (cond
   ((and arg
         (magit-section-internal-region-p)
         (magit-section-match 'hunk))
    (kill-new
     (thread-last (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))
       (replace-regexp-in-string
        (format "^\\%c.*\n?" (if (< (prefix-numeric-value arg) 0) ?+ ?-))
        "")
       (replace-regexp-in-string "^[ +-]" "")))
    (deactivate-mark))
   ((use-region-p)
    (call-interactively #'copy-region-as-kill))
   (t
    (when-let* ((section (magit-current-section))
                (value (oref section value)))
      (magit-section-case
        ((branch commit module-commit tag)
         (let ((default-directory default-directory) ref)
           (magit-section-case
             ((branch tag)
              (setq ref value))
             (module-commit
              (setq default-directory
                    (file-name-as-directory
                     (expand-file-name (magit-section-parent-value section)
                                       (magit-toplevel))))))
           (setq value (magit-rev-parse
                        (and magit-copy-revision-abbreviated "--short")
                        value))
           (push (list value default-directory) magit-revision-stack)
           (kill-new (message "%s" (or (and current-prefix-arg ref)
                                       value)))))
        (t (kill-new (message "%s" value))))))))

;;;###autoload
(defun magit-copy-buffer-revision ()
  "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (when-let ((rev (or magit-buffer-revision
                        (cl-case major-mode
                          (magit-diff-mode
                           (if (string-match "\\.\\.\\.?\\(.+\\)"
                                             magit-buffer-range)
                               (match-string 1 magit-buffer-range)
                             magit-buffer-range))
                          (magit-status-mode "HEAD")))))
      (when (magit-commit-p rev)
        (setq rev (magit-rev-parse
                   (and magit-copy-revision-abbreviated "--short")
                   rev))
        (push (list rev default-directory) magit-revision-stack)
        (kill-new (message "%s" rev))))))

;;; Buffer Switching

;;;###autoload
(defun magit-display-repository-buffer (buffer)
  "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see."
  (interactive (list (magit--read-repository-buffer
                      "Display magit buffer: ")))
  (magit-display-buffer (get-buffer buffer)))

;;;###autoload
(defun magit-switch-to-repository-buffer (buffer)
  "Switch to a Magit buffer belonging to the current Git repository."
  (interactive (list (magit--read-repository-buffer
                      "Switch to magit buffer: ")))
  (switch-to-buffer buffer))

;;;###autoload
(defun magit-switch-to-repository-buffer-other-window (buffer)
  "Switch to a Magit buffer belonging to the current Git repository."
  (interactive (list (magit--read-repository-buffer
                      "Switch to magit buffer in another window: ")))
  (switch-to-buffer-other-window buffer))

;;;###autoload
(defun magit-switch-to-repository-buffer-other-frame (buffer)
  "Switch to a Magit buffer belonging to the current Git repository."
  (interactive (list (magit--read-repository-buffer
                      "Switch to magit buffer in another frame: ")))
  (switch-to-buffer-other-frame buffer))

(defun magit--read-repository-buffer (prompt)
  (if-let ((topdir (magit-rev-parse-safe "--show-toplevel")))
      (read-buffer
       prompt (magit-get-mode-buffer 'magit-status-mode) t
       (pcase-lambda (`(,_ . ,buf))
         (and buf
              (with-current-buffer buf
                (and (or (derived-mode-p 'magit-mode
                                         'magit-repolist-mode
                                         'magit-submodule-list-mode
                                         'git-rebase-mode)
                         (and buffer-file-name
                              (string-match-p git-commit-filename-regexp
                                              buffer-file-name)))
                     (equal (magit-rev-parse-safe "--show-toplevel")
                            topdir))))))
    (user-error "Not inside a Git repository")))

;;; Miscellaneous

;;;###autoload
(defun magit-abort-dwim ()
  "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect."
  (interactive)
  (cond ((magit-merge-in-progress-p)     (magit-merge-abort))
        ((magit-rebase-in-progress-p)    (magit-rebase-abort))
        ((magit-am-in-progress-p)        (magit-am-abort))
        ((magit-sequencer-in-progress-p) (magit-sequencer-abort))
        ((magit-bisect-in-progress-p)    (magit-bisect-reset))))

;;; _
(provide 'magit-extras)
;;; magit-extras.el ends here
