;;; magit-base.el --- Early birds  -*- lexical-binding:t; coding:utf-8 -*-

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

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2023 Free Software Foundation, Inc.

;;; Commentary:

;; This library defines utility functions, options and other things that
;; have to be available early on because they are used by several other
;; libraries, which cannot depend on one another, because that would lead
;; to circular dependencies.

;;; Code:

(defconst magit--minimal-git "2.2.0")
(defconst magit--minimal-emacs "25.1")

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'subr-x)

;; For older Emacs releases we depend on an updated `seq' release from
;; GNU ELPA, for `seq-keep'.  Unfortunately something else may already
;; have required `seq', before `package' had a chance to put the more
;; recent version earlier on the `load-path'.
(when (and (featurep' seq)
           (not (fboundp 'seq-keep)))
  (unload-feature 'seq 'force))
(require 'seq)

(require 'crm)

(require 'magit-section)

(eval-when-compile (require 'ido))
(declare-function Info-get-token "info" (pos start all &optional errorstring))

(eval-when-compile (require 'vc-git))
(declare-function vc-git--run-command-string "vc-git" (file &rest args))

(eval-when-compile (require 'which-func))
(declare-function which-function "which-func" ())

;;; Options

(defcustom magit-completing-read-function #'magit-builtin-completing-read
  "Function to be called when requesting input from the user.

If you have enabled `ivy-mode' or `helm-mode', then you don't
have to customize this option; `magit-builtin-completing-read'
will work just fine.  However, if you use Ido completion, then
you do have to use `magit-ido-completing-read', because Ido is
less well behaved than the former, more modern alternatives.

If you would like to use Ivy or Helm completion with Magit but
not enable the respective modes globally, then customize this
option to use `ivy-completing-read' or
`helm--completing-read-default'.  If you choose to use
`ivy-completing-read', note that the items may always be shown in
alphabetical order, depending on your version of Ivy."
  :group 'magit-essentials
  :type '(radio (function-item magit-builtin-completing-read)
                (function-item magit-ido-completing-read)
                (function-item ivy-completing-read)
                (function-item helm--completing-read-default)
                (function :tag "Other function")))

(defcustom magit-dwim-selection
  '((magit-stash-apply        nil t)
    (magit-ediff-resolve-all  nil t)
    (magit-ediff-resolve-rest nil t)
    (magit-stash-branch       nil t)
    (magit-stash-branch-here  nil t)
    (magit-stash-format-patch nil t)
    (magit-stash-drop         nil ask)
    (magit-stash-pop          nil ask))
  "When not to offer alternatives and ask for confirmation.

Many commands by default ask the user to select from a list of
possible candidates.  They do so even when there is a thing at
point that they can act on, which is then offered as the default.

This option can be used to tell certain commands to use the thing
at point instead of asking the user to select a candidate to act
on, with or without confirmation.

The value has the form ((COMMAND nil|PROMPT DEFAULT)...).

- COMMAND is the command that should not prompt for a choice.
  To have an effect, the command has to use the function
  `magit-completing-read' or a utility function which in turn uses
  that function.

- If the command uses `magit-completing-read' multiple times, then
  PROMPT can be used to only affect one of these uses.  PROMPT, if
  non-nil, is a regular expression that is used to match against
  the PROMPT argument passed to `magit-completing-read'.

- DEFAULT specifies how to use the default.  If it is t, then
  the DEFAULT argument passed to `magit-completing-read' is used
  without confirmation.  If it is `ask', then the user is given
  a chance to abort.  DEFAULT can also be nil, in which case the
  entry has no effect."
  :package-version '(magit . "2.12.0")
  :group 'magit-commands
  :type '(repeat
          (list (symbol :tag "Command") ; It might not be fboundp yet.
                (choice (const  :tag "for all prompts" nil)
                        (regexp :tag "for prompts matching regexp"))
                (choice (const  :tag "offer other choices" nil)
                        (const  :tag "require confirmation" ask)
                        (const  :tag "use default without confirmation" t)))))

(defconst magit--confirm-actions
  '((const discard)
    (const reverse)
    (const stage-all-changes)
    (const unstage-all-changes)
    (const delete)
    (const trash)
    (const resurrect)
    (const untrack)
    (const rename)
    (const reset-bisect)
    (const abort-cherry-pick)
    (const abort-revert)
    (const abort-rebase)
    (const abort-merge)
    (const merge-dirty)
    (const delete-unmerged-branch)
    (const delete-branch-on-remote)
    (const delete-pr-remote)
    (const drop-stashes)
    (const set-and-push)
    (const amend-published)
    (const rebase-published)
    (const edit-published)
    (const remove-modules)
    (const remove-dirty-modules)
    (const trash-module-gitdirs)
    (const stash-apply-3way)
    (const kill-process)
    (const safe-with-wip)))

(defcustom magit-no-confirm '(set-and-push)
  "A list of symbols for actions Magit should not confirm, or t.

Many potentially dangerous commands by default ask the user for
confirmation.  Each of the below symbols stands for an action
which, when invoked unintentionally or without being fully aware
of the consequences, could lead to tears.  In many cases there
are several commands that perform variations of a certain action,
so we don't use the command names but more generic symbols.

Applying changes:

  `discard' Discarding one or more changes (i.e., hunks or the
  complete diff for a file) loses that change, obviously.

  `reverse' Reverting one or more changes can usually be undone
  by reverting the reversion.

  `stage-all-changes', `unstage-all-changes' When there are both
  staged and unstaged changes, then un-/staging everything would
  destroy that distinction.  Of course that also applies when
  un-/staging a single change, but then less is lost and one does
  that so often that having to confirm every time would be
  unacceptable.

Files:

  `delete' When a file that isn't yet tracked by Git is deleted
  then it is completely lost, not just the last changes.  Very
  dangerous.

  `trash' Instead of deleting a file it can also be move to the
  system trash.  Obviously much less dangerous than deleting it.

  Also see option `magit-delete-by-moving-to-trash'.

  `resurrect' A deleted file can easily be resurrected by
  \"deleting\" the deletion, which is done using the same command
  that was used to delete the same file in the first place.

  `untrack' Untracking a file can be undone by tracking it again.

  `rename' Renaming a file can easily be undone.

Sequences:

  `reset-bisect' Aborting (known to Git as \"resetting\") a
  bisect operation loses all information collected so far.

  `abort-cherry-pick' Aborting a cherry-pick throws away all
  conflict resolutions which has already been carried out by the
  user.

  `abort-revert' Aborting a revert throws away all conflict
  resolutions which has already been carried out by the user.

  `abort-rebase' Aborting a rebase throws away all already
  modified commits, but it's possible to restore those from the
  reflog.

  `abort-merge' Aborting a merge throws away all conflict
  resolutions which has already been carried out by the user.

  `merge-dirty' Merging with a dirty worktree can make it hard to
  go back to the state before the merge was initiated.

References:

  `delete-unmerged-branch' Once a branch has been deleted it can
  only be restored using low-level recovery tools provided by
  Git.  And even then the reflog is gone.  The user always has
  to confirm the deletion of a branch by accepting the default
  choice (or selecting another branch), but when a branch has
  not been merged yet, also make sure the user is aware of that.

  `delete-branch-on-remote' Deleting a \"remote branch\" may mean
  deleting the (local) \"remote-tracking\" branch only, or also
  removing it from the remote itself.  The latter often makes more
  sense because otherwise simply fetching from the remote would
  restore the remote-tracking branch, but doing that can be
  surprising and hard to recover from, so we ask.

  `delete-pr-remote' When deleting a branch that was created from
  a pull-request and if no other branches still exist on that
  remote, then `magit-branch-delete' offers to delete the remote
  as well.  This should be safe because it only happens if no
  other refs exist in the remotes namespace, and you can recreate
  the remote if necessary.

  `drop-stashes' Dropping a stash is dangerous because Git stores
  stashes in the reflog.  Once a stash is removed, there is no
  going back without using low-level recovery tools provided by
  Git.  When a single stash is dropped, then the user always has
  to confirm by accepting the default (or selecting another).
  This action only concerns the deletion of multiple stashes at
  once.

Publishing:

  `set-and-push' When pushing to the upstream or the push-remote
  and that isn't actually configured yet, then the user can first
  set the target.  If s/he confirms the default too quickly, then
  s/he might end up pushing to the wrong branch and if the remote
  repository is configured to disallow fixing such mistakes, then
  that can be quite embarrassing and annoying.

Edit published history:

  Without adding these symbols here, you will be warned before
  editing commits that have already been pushed to one of the
  branches listed in `magit-published-branches'.

  `amend-published' Affects most commands that amend to `HEAD'.

  `rebase-published' Affects commands that perform interactive
  rebases.  This includes commands from the commit popup that
  modify a commit other than `HEAD', namely the various fixup
  and squash variants.

  `edit-published' Affects the commands `magit-edit-line-commit'
  and `magit-diff-edit-hunk-commit'.  These two commands make
  it quite easy to accidentally edit a published commit, so you
  should think twice before configuring them not to ask for
  confirmation.

  To disable confirmation completely, add all three symbols here
  or set `magit-published-branches' to nil.

Removing modules:

  `remove-modules' When you remove the working directory of a
  module that does not contain uncommitted changes, then that is
  safer than doing so when there are uncommitted changes and/or
  when you also remove the gitdir.  Still, you don't want to do
  that by accident.

  `remove-dirty-modules' When you remove the working directory of
  a module that contains uncommitted changes, then those changes
  are gone for good.  It is better to go to the module, inspect
  these changes and only if appropriate discard them manually.

  `trash-module-gitdirs' When you remove the gitdir of a module,
  then all unpushed changes are gone for good.  It is very easy
  to forget that you have some unfinished work on an unpublished
  feature branch or even in a stash.

  Actually there are some safety precautions in place, that might
  help you out if you make an unwise choice here, but don't count
  on it.  In case of emergency, stay calm and check the stash and
  the `trash-directory' for traces of lost work.

Various:

  `stash-apply-3way' When a stash cannot be applied using \"git
  stash apply\", then Magit uses \"git apply\" instead, possibly
  using the \"--3way\" argument, which isn't always perfectly
  safe.  See also `magit-stash-apply'.

  `kill-process' There seldom is a reason to kill a process.

Global settings:

  Instead of adding all of the above symbols to the value of this
  option you can also set it to the atom `t', which has the same
  effect as adding all of the above symbols.  Doing that most
  certainly is a bad idea, especially because other symbols might
  be added in the future.  So even if you don't want to be asked
  for confirmation for any of these actions, you are still better
  of adding all of the respective symbols individually.

  When `magit-wip-before-change-mode' is enabled then these actions
  can fairly easily be undone: `discard', `reverse',
  `stage-all-changes', and `unstage-all-changes'.  If and only if
  this mode is enabled, then `safe-with-wip' has the same effect
  as adding all of these symbols individually."
  :package-version '(magit . "2.1.0")
  :group 'magit-essentials
  :group 'magit-commands
  :type `(choice (const :tag "Always require confirmation" nil)
                 (const :tag "Never require confirmation" t)
                 (set   :tag "Require confirmation except for"
                        ;; `remove-dirty-modules' and
                        ;; `trash-module-gitdirs' intentionally
                        ;; omitted.
                        ,@magit--confirm-actions)))

(defcustom magit-slow-confirm '(drop-stashes)
  "Whether to ask user \"y or n\" or \"yes or no\" questions.

When this is nil, then `y-or-n-p' is used when the user has to
confirm a potentially destructive action.  When this is t, then
`yes-or-no-p' is used instead.  If this is a list of symbols
identifying actions, then `yes-or-no-p' is used for those,
`y-or-no-p' for all others.  The list of actions is the same as
for `magit-no-confirm' (which see)."
  :package-version '(magit . "2.9.0")
  :group 'magit-miscellaneous
  :type `(choice (const :tag "Always ask \"yes or no\" questions" t)
                 (const :tag "Always ask \"y or n\" questions" nil)
                 (set   :tag "Ask \"yes or no\" questions only for"
                        ,@magit--confirm-actions)))

(defcustom magit-no-message nil
  "A list of messages Magit should not display.

Magit displays most echo area messages using `message', but a few
are displayed using `magit-message' instead, which takes the same
arguments as the former, FORMAT-STRING and ARGS.  `magit-message'
forgoes printing a message if any member of this list is a prefix
of the respective FORMAT-STRING.

If Magit prints a message which causes you grief, then please
first investigate whether there is another option which can be
used to suppress it.  If that is not the case, then ask the Magit
maintainers to start using `magit-message' instead of `message'
in that case.  We are not proactively replacing all uses of
`message' with `magit-message', just in case someone *might* find
some of these messages useless.

Messages which can currently be suppressed using this option are:
* \"Turning on magit-auto-revert-mode...\""
  :package-version '(magit . "2.8.0")
  :group 'magit-miscellaneous
  :type '(repeat string))

(defcustom magit-verbose-messages nil
  "Whether to make certain prompts and messages more verbose.

Occasionally a user suggests that a certain prompt or message
should be more verbose, but I would prefer to keep it as-is
because I don't think that the fact that that one user did not
understand the existing prompt/message means that a large number
of users would have the same difficulty, and that making it more
verbose would actually do a disservice to users who understand
the shorter prompt well enough.

Going forward I will start offering both messages when I feel the
suggested longer message is reasonable enough, and the value of
this option decides which will be used.  Note that changing the
value of this option affects all such messages and that I do not
intend to add an option per prompt."
  :package-version '(magit . "4.0.0")
  :group 'magit-miscellaneous
  :type 'boolean)

(defcustom magit-ellipsis
  '((margin (?… . ">"))
    (t      (?… . "...")))
  "Characters or strings used to abbreviate text in some buffers.

Each element has the form (WHERE (FANCY . UNIVERSAL)).

FANCY is a single character or nil whereas UNIVERSAL is a string
of any length.  The ellipsis produced by `magit--ellipsis' will
be FANCY if it's a non-nil character that can be displayed with
the available fonts, otherwise UNIVERSAL will be used.  FANCY is
meant to be a rich character like a horizontal ellipsis symbol or
an emoji whereas UNIVERSAL something simpler available in a less
rich environment like the CLI.  WHERE determines the use-case for
the ellipsis definition.  Currently the only acceptable values
for WHERE are `margin' or t (representing the default).

Whether collapsed sections are indicated using ellipsis is
controlled by `magit-section-visibility-indicator'."
  :package-version '(magit . "4.0.0")
  :group 'magit-miscellaneous
  :type '(repeat (list (symbol :tag "Where")
                       (cons (choice :tag "Fancy" character (const nil))
                             (string :tag "Universal")))))

(defcustom magit-update-other-window-delay 0.2
  "Delay before automatically updating the other window.

When moving around in certain buffers, then certain other
buffers, which are being displayed in another window, may
optionally be updated to display information about the
section at point.

When holding down a key to move by more than just one section,
then that would update that buffer for each section on the way.
To prevent that, updating the revision buffer is delayed, and
this option controls for how long.  For optimal experience you
might have to adjust this delay and/or the keyboard repeat rate
and delay of your graphical environment or operating system."
  :package-version '(magit . "2.3.0")
  :group 'magit-miscellaneous
  :type 'number)

(defcustom magit-view-git-manual-method 'info
  "How links to Git documentation are followed from Magit's Info manuals.

`info'  Follow the link to the node in the `gitman' Info manual
        as usual.  Unfortunately that manual is not installed by
        default on some platforms, and when it is then the nodes
        look worse than the actual manpages.

`man'   View the respective man-page using the `man' package.

`woman' View the respective man-page using the `woman' package."
  :package-version '(magit . "2.9.0")
  :group 'magit-miscellaneous
  :type '(choice (const :tag "view info manual" info)
                 (const :tag "view manpage using `man'" man)
                 (const :tag "view manpage using `woman'" woman)))

;;; Section Classes

(defclass magit-commit-section (magit-section) ())

(setf (alist-get 'commit magit--section-type-alist) 'magit-commit-section)

(defclass magit-diff-section (magit-section) () :abstract t)

(defclass magit-file-section (magit-diff-section)
  ((keymap :initform 'magit-file-section-map)
   (source :initform nil)
   (header :initform nil)
   (binary :initform nil)))

(defclass magit-module-section (magit-file-section)
  ((keymap :initform 'magit-module-section-map)
   (range  :initform nil)))

(defclass magit-hunk-section (magit-diff-section)
  ((keymap      :initform 'magit-hunk-section-map)
   (refined     :initform nil)
   (combined    :initform nil)
   (from-range  :initform nil)
   (from-ranges :initform nil)
   (to-range    :initform nil)
   (about       :initform nil)))

(setf (alist-get 'file   magit--section-type-alist) 'magit-file-section)
(setf (alist-get 'module magit--section-type-alist) 'magit-module-section)
(setf (alist-get 'hunk   magit--section-type-alist) 'magit-hunk-section)

(defclass magit-log-section (magit-section) () :abstract t)
(defclass magit-unpulled-section (magit-log-section) ())
(defclass magit-unpushed-section (magit-log-section) ())
(defclass magit-unmerged-section (magit-log-section) ())

(setf (alist-get 'unpulled magit--section-type-alist) 'magit-unpulled-section)
(setf (alist-get 'unpushed magit--section-type-alist) 'magit-unpushed-section)
(setf (alist-get 'unmerged magit--section-type-alist) 'magit-unmerged-section)

;;; User Input

(defvar helm-completion-in-region-default-sort-fn)
(defvar helm-crm-default-separator)
(defvar ivy-sort-functions-alist)
(defvar ivy-sort-matches-functions-alist)

(defvar magit-completing-read--silent-default nil)

(defun magit-completing-read ( prompt collection &optional
                               predicate require-match initial-input
                               hist def fallback)
  "Read a choice in the minibuffer, or use the default choice.

This is the function that Magit commands use when they need the
user to select a single thing to act on.  The arguments have the
same meaning as for `completing-read', except for FALLBACK, which
is unique to this function and is described below.

Instead of asking the user to choose from a list of possible
candidates, this function may instead just return the default
specified by DEF, with or without requiring user confirmation.
Whether that is the case depends on PROMPT, `this-command' and
`magit-dwim-selection'.  See the documentation of the latter for
more information.

If it does use the default without the user even having to
confirm that, then `magit-completing-read--silent-default' is set
to t, otherwise nil.

If it does read a value in the minibuffer, then this function
acts similarly to `completing-read', except for the following:

- COLLECTION must be a list of choices.  A function is not
  supported.

- If REQUIRE-MATCH is nil and the user exits without a choice,
  then nil is returned instead of an empty string.

- If REQUIRE-MATCH is non-nil and the user exits without a
  choice, `user-error' is raised.

- FALLBACK specifies a secondary default that is only used if
  the primary default DEF is nil.  The secondary default is not
  subject to `magit-dwim-selection' — if DEF is nil but FALLBACK
  is not, then this function always asks the user to choose a
  candidate, just as if both defaults were nil.

- \": \" is appended to PROMPT.

- PROMPT is modified to end with \" (default DEF|FALLBACK): \"
  provided that DEF or FALLBACK is non-nil, that neither
  `ivy-mode' nor `helm-mode' is enabled, and that
  `magit-completing-read-function' is set to its default value of
  `magit-builtin-completing-read'."
  (setq magit-completing-read--silent-default nil)
  (if-let ((dwim (and def
                      (nth 2 (seq-find (pcase-lambda (`(,cmd ,re ,_))
                                         (and (eq this-command cmd)
                                              (or (not re)
                                                  (string-match-p re prompt))))
                                       magit-dwim-selection)))))
      (if (eq dwim 'ask)
          (if (y-or-n-p (format "%s %s? " prompt def))
              def
            (user-error "Abort"))
        (setq magit-completing-read--silent-default t)
        def)
    (unless def
      (setq def fallback))
    (let ((command this-command)
          (reply (funcall magit-completing-read-function
                          (concat prompt ": ")
                          (if (and def (not (member def collection)))
                              (cons def collection)
                            collection)
                          predicate
                          require-match initial-input hist def)))
      (setq this-command command)
      ;; Note: Avoid `string=' to support `helm-comp-read-use-marked'.
      (if (equal reply "")
          (if require-match
              (user-error "Nothing selected")
            nil)
        reply))))

(defun magit--completion-table (collection)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity))
      (complete-with-action action collection string pred))))

(defun magit-builtin-completing-read
    (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (unless (or (bound-and-true-p helm-mode)
              (bound-and-true-p ivy-mode)
              (bound-and-true-p vertico-mode)
              (bound-and-true-p selectrum-mode))
    (setq prompt (magit-prompt-with-default prompt def)))
  (unless (or (bound-and-true-p helm-mode)
              (bound-and-true-p ivy-mode))
    (setq choices (magit--completion-table choices)))
  (cl-letf (((symbol-function #'completion-pcm--all-completions)))
    (when (< emacs-major-version 26)
      (fset 'completion-pcm--all-completions
            'magit-completion-pcm--all-completions))
    (let ((ivy-sort-functions-alist nil))
      (completing-read prompt choices
                       predicate require-match
                       initial-input hist def))))

(define-obsolete-function-alias 'magit-completing-read-multiple*
  'magit-completing-read-multiple "Magit-Section 4.0.0")

(defun magit-completing-read-multiple
    ( prompt table &optional predicate require-match initial-input
      hist def inherit-input-method
      no-split)
  "Read multiple strings in the minibuffer, with completion.
Like `completing-read-multiple' but don't mess with order of
TABLE and take an additional argument NO-SPLIT, which causes
the user input to be returned as a single unmodified string.
Also work around various incompatible features of various
third-party completion frameworks."
  (cl-letf*
      (;; To implement NO-SPLIT we have to manipulate the respective
       ;; `split-string' invocation.  We cannot simply advice it to
       ;; return the input string because `SELECTRUM' would choke on
       ;; that string.  Use a variable to pass along the raw user
       ;; input string. aa5f098ab
       (input nil)
       (split-string (symbol-function #'split-string))
       ((symbol-function #'split-string)
        (lambda (string &optional separators omit-nulls trim)
          (when (and no-split
                     (equal separators crm-separator)
                     (equal omit-nulls t))
            (setq input string))
          (funcall split-string string separators omit-nulls trim)))
       ;; In Emacs 25 this function has a bug, so we use a copy of the
       ;; version from Emacs 26. bef9c7aa3
       ((symbol-function #'completion-pcm--all-completions)
        (if (< emacs-major-version 26)
            'magit-completion-pcm--all-completions
          (symbol-function #'completion-pcm--all-completions)))
       ;; Prevent `BUILT-IN' completion from messing up our existing
       ;; order of the completion candidates. aa5f098ab
       (table (magit--completion-table table))
       ;; Prevent `IVY' from messing up our existing order. c7af78726
       (ivy-sort-matches-functions-alist nil)
       ;; Prevent `HELM' from messing up our existing order.  6fcf994bd
       (helm-completion-in-region-default-sort-fn nil)
       ;; Prevent `HELM' from automatically appending the separator,
       ;; which is counterproductive when NO-SPLIT is non-nil and/or
       ;; when reading commit ranges. 798aff564
       (helm-crm-default-separator
        (if no-split nil (bound-and-true-p helm-crm-default-separator)))
       ;; And now, the moment we have all been waiting for...
       (values (completing-read-multiple
                prompt table predicate require-match initial-input
                hist def inherit-input-method)))
    (if no-split input values)))

(defun magit-ido-completing-read
    (prompt choices &optional predicate require-match initial-input hist def)
  "Ido-based `completing-read' almost-replacement.

Unfortunately `ido-completing-read' is not suitable as a
drop-in replacement for `completing-read', instead we use
`ido-completing-read+' from the third-party package by the
same name."
  (if (and (require 'ido-completing-read+ nil t)
           (fboundp 'ido-completing-read+))
      (ido-completing-read+ prompt choices predicate require-match
                            initial-input hist
                            (or def (and require-match (car choices))))
    (display-warning 'magit "ido-completing-read+ is not installed

To use Ido completion with Magit you need to install the
third-party `ido-completing-read+' packages.  Falling
back to built-in `completing-read' for now." :error)
    (magit-builtin-completing-read prompt choices predicate require-match
                                   initial-input hist def)))

(defun magit-prompt-with-default (prompt def)
  (if (and def (length> prompt 2)
           (string-equal ": " (substring prompt -2)))
      (format "%s (default %s): " (substring prompt 0 -2) def)
    prompt))

(defvar-keymap magit-minibuffer-local-ns-map
  :parent minibuffer-local-map
  "SPC" #'magit-whitespace-disallowed
  "TAB" #'magit-whitespace-disallowed)

(defun magit-whitespace-disallowed ()
  "Beep to tell the user that whitespace is not allowed."
  (interactive)
  (ding)
  (message "Whitespace isn't allowed here")
  (setq defining-kbd-macro nil)
  (force-mode-line-update))

(defun magit-read-string ( prompt &optional initial-input history default-value
                           inherit-input-method no-whitespace)
  "Read a string from the minibuffer, prompting with string PROMPT.

This is similar to `read-string', but
* empty input is only allowed if DEFAULT-VALUE is non-nil in
  which case that is returned,
* whitespace is not allowed and leading and trailing whitespace is
  removed automatically if NO-WHITESPACE is non-nil,
* \": \" is appended to PROMPT, and
* an invalid DEFAULT-VALUE is silently ignored."
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (stringp default-value)
      (setq default-value nil)))
  (let* ((minibuffer-completion-table nil)
         (val (read-from-minibuffer
               (magit-prompt-with-default (concat prompt ": ") default-value)
               initial-input (and no-whitespace magit-minibuffer-local-ns-map)
               nil history default-value inherit-input-method))
         (trim (lambda (regexp string)
                 (save-match-data
                   (if (string-match regexp string)
                       (replace-match "" t t string)
                     string)))))
    (when (and (string= val "") default-value)
      (setq val default-value))
    (when no-whitespace
      (setq val (funcall trim "\\`\\(?:[ \t\n\r]+\\)"
                         (funcall trim "\\(?:[ \t\n\r]+\\)\\'" val))))
    (cond ((string= val "")
           (user-error "Need non-empty input"))
          ((and no-whitespace (string-match-p "[\s\t\n]" val))
           (user-error "Input contains whitespace"))
          (t val))))

(defun magit-read-string-ns ( prompt &optional initial-input history
                              default-value inherit-input-method)
  "Call `magit-read-string' with non-nil NO-WHITESPACE."
  (magit-read-string prompt initial-input history default-value
                     inherit-input-method t))

(defmacro magit-read-char-case (prompt verbose &rest clauses)
  (declare (indent 2)
           (debug (form form &rest (characterp form body))))
  `(prog1 (pcase (read-char-choice
                  (let ((parts (nconc (list ,@(mapcar #'cadr clauses))
                                      ,(and verbose '(list "[C-g] to abort")))))
                    (concat ,prompt
                            (mapconcat #'identity (butlast parts) ", ")
                            ", or "  (car (last parts)) " "))
                  ',(mapcar #'car clauses))
            ,@(--map `(,(car it) ,@(cddr it)) clauses))
     (message "")))

(defun magit-y-or-n-p (prompt &optional action)
  "Ask user a \"y or n\" or a \"yes or no\" question using PROMPT.
Which kind of question is used depends on whether
ACTION is a member of option `magit-slow-confirm'."
  (if (or (eq magit-slow-confirm t)
          (and action (member action magit-slow-confirm)))
      (yes-or-no-p prompt)
    (y-or-n-p prompt)))

(defvar magit--no-confirm-alist
  '((safe-with-wip magit-wip-before-change-mode
                   discard reverse stage-all-changes unstage-all-changes)))

(cl-defun magit-confirm ( action &optional prompt prompt-n noabort
                          (items nil sitems) prompt-suffix)
  (declare (indent defun))
  (setq prompt-n (format (concat (or prompt-n prompt) "? ") (length items)))
  (setq prompt   (format (concat (or prompt (magit-confirm-make-prompt action))
                                 "? ")
                         (car items)))
  (when prompt-suffix
    (setq prompt (concat prompt prompt-suffix)))
  (or (cond ((and (not (eq action t))
                  (or (eq magit-no-confirm t)
                      (memq action magit-no-confirm)
                      (cl-member-if (pcase-lambda (`(,key ,var . ,sub))
                                      (and (memq key magit-no-confirm)
                                           (memq action sub)
                                           (or (not var)
                                               (and (boundp var)
                                                    (symbol-value var)))))
                                    magit--no-confirm-alist)))
             (or (not sitems) items))
            ((not sitems)
             (magit-y-or-n-p prompt action))
            ((length= items 1)
             (and (magit-y-or-n-p prompt action) items))
            ((length> items 1)
             (and (magit-y-or-n-p (concat (mapconcat #'identity items "\n")
                                          "\n\n" prompt-n)
                                  action)
                  items)))
      (if noabort nil (user-error "Abort"))))

(defun magit-confirm-files (action files &optional prompt prompt-suffix noabort)
  (when files
    (unless prompt
      (setq prompt (magit-confirm-make-prompt action)))
    (magit-confirm action
      (concat prompt " \"%s\"")
      (concat prompt " %d files")
      noabort files prompt-suffix)))

(defun magit-confirm-make-prompt (action)
  (let ((prompt (symbol-name action)))
    (string-replace "-" " "
                    (concat (upcase (substring prompt 0 1))
                            (substring prompt 1)))))

(defun magit-read-number-string (prompt &optional default _history)
  "Like `read-number' but return value is a string.
DEFAULT may be a number or a numeric string."
  (number-to-string
   (read-number prompt (if (stringp default)
                           (string-to-number default)
                         default))))

;;; Debug Utilities

;;;###autoload
(defun magit-emacs-Q-command ()
  "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information."
  (interactive)
  (let ((cmd (mapconcat
              #'shell-quote-argument
              `(,(concat invocation-directory invocation-name)
                "-Q" "--eval" "(setq debug-on-error t)"
                ,@(cl-mapcan
                   (lambda (dir) (list "-L" dir))
                   (delete-dups
                    (cl-mapcan
                     (lambda (lib)
                       (let ((path (locate-library lib)))
                         (cond
                          (path
                           (list (file-name-directory path)))
                          ((not (equal lib "libgit"))
                           (error "Cannot find mandatory dependency %s" lib)))))
                     '(;; Like `LOAD_PATH' in `default.mk'.
                       "compat"
                       "dash"
                       "libgit"
                       "transient"
                       "with-editor"
                       ;; Obviously `magit' itself is needed too.
                       "magit"
                       ;; While these are part of the Magit repository,
                       ;; they are distributed as separate packages.
                       "magit-section"
                       "git-commit"
                       ))))
                ;; Avoid Emacs bug#16406 by using full path.
                "-l" ,(file-name-sans-extension (locate-library "magit")))
              " ")))
    (message "Uncustomized Magit command saved to kill-ring, %s"
             "please run it in a terminal.")
    (kill-new cmd)))

;;; Text Utilities

(defmacro magit-bind-match-strings (varlist string &rest body)
  "Bind variables to submatches according to VARLIST then evaluate BODY.
Bind the symbols in VARLIST to submatches of the current match
data, starting with 1 and incrementing by 1 for each symbol.  If
the last match was against a string, then that has to be provided
as STRING."
  (declare (indent 2) (debug (listp form body)))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (cl-mapcan (lambda (sym)
                            (cl-incf i)
                            (and (not (eq (aref (symbol-name sym) 0) ?_))
                                 (list (list sym (list 'match-string i s)))))
                          varlist))
         ,@body))))

(defun magit-delete-line ()
  "Delete the rest of the current line."
  (delete-region (point) (1+ (line-end-position))))

(defun magit-delete-match (&optional num)
  "Delete text matched by last search.
If optional NUM is specified, only delete that subexpression."
  (delete-region (match-beginning (or num 0))
                 (match-end (or num 0))))

(defun magit-file-line (file)
  "Return the first line of FILE as a string."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min)
                                      (line-end-position)))))

(defun magit-file-lines (file &optional keep-empty-lines)
  "Return a list of strings containing one element per line in FILE.
Unless optional argument KEEP-EMPTY-LINES is t, trim all empty lines."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" (not keep-empty-lines)))))

(defun magit-set-header-line-format (string)
  "Set the header-line using STRING.
Propertize STRING with the `magit-header-line'.  If the `face'
property of any part of STRING is already set, then that takes
precedence.  Also pad the left side of STRING so that it aligns
with the text area."
  (setq header-line-format
        (concat (propertize " " 'display '(space :align-to 0))
                string)))

(defun magit--format-spec (format specification)
  "Like `format-spec' but preserve text properties in SPECIFICATION."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
        (delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
        (let* ((num (match-string 1))
               (spec (string-to-char (match-string 2)))
               (val (assq spec specification)))
          (unless val
            (error "Invalid format character: `%%%c'" spec))
          (setq val (cdr val))
          ;; Pad result to desired length.
          (let ((text (format (concat "%" num "s") val)))
            ;; Insert first, to preserve text properties.
            (if (next-property-change 0 (concat " " text))
                ;; If the inserted text has properties, then preserve those.
                (insert text)
              ;; Otherwise preserve FORMAT's properties, like `format-spec'.
              (insert-and-inherit text))
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
        (error "Invalid format string"))))
    (buffer-string)))

;;; Missing from Emacs

(defun magit-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun magit--buffer-string (&optional min max trim)
  "Like `buffer-substring-no-properties' but the arguments are optional.

This combines the benefits of `buffer-string', `buffer-substring'
and `buffer-substring-no-properties' into one function that is
not as painful to use as the latter.  I.e., you can write
  (magit--buffer-string)
instead of
  (buffer-substring-no-properties (point-min)
                                  (point-max))

Optional MIN defaults to the value of `point-min'.
Optional MAX defaults to the value of `point-max'.

If optional TRIM is non-nil, then all leading and trailing
whitespace is remove.  If it is the newline character, then
one trailing newline is added."
  ;; Lets write that one last time and be done with it:
  (let ((str (buffer-substring-no-properties (or min (point-min))
                                             (or max (point-max)))))
    (if trim
        (concat (string-trim str)
                (and (eq trim ?\n) "\n"))
      str)))

(defun magit--version> (v1 v2)
  "Return t if version V1 is higher (younger) than V2.
This function should be named `version>' and be part of Emacs."
  (version-list-< (version-to-list v2) (version-to-list v1)))

(defun magit--version>= (v1 v2)
  "Return t if version V1 is higher (younger) than or equal to V2.
This function should be named `version>=' and be part of Emacs."
  (version-list-<= (version-to-list v2) (version-to-list v1)))

;;; Kludges for Emacs Bugs

(defun magit-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' but work around an Apple bug.
See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21573#17
and https://github.com/magit/magit/issues/2295."
  (and (file-directory-p filename)
       (file-accessible-directory-p filename)))

(when (< emacs-major-version 27)
  ;; Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559.
  ;; Fixed by cb55ccae8be946f1562d74718086a4c8c8308ee5 in Emacs 27.1.
  (with-eval-after-load 'vc-git
    (defun vc-git-conflicted-files (directory)
      "Return the list of files with conflicts in DIRECTORY."
      (let* ((status
              (vc-git--run-command-string directory "diff-files"
                                          "--name-status"))
             (lines (when status (split-string status "\n" 'omit-nulls)))
             files)
        (dolist (line lines files)
          (when (string-match "\\([ MADRCU?!]\\)[ \t]+\\(.+\\)" line)
            (let ((state (match-string 1 line))
                  (file (match-string 2 line)))
              (when (equal state "U")
                (push (expand-file-name file directory) files)))))))))

(when (< emacs-major-version 27)
  (defun vc-git--call@bug21559 (fn buffer command &rest args)
    "Backport https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559."
    (let ((process-environment process-environment))
      (when revert-buffer-in-progress-p
        (push "GIT_OPTIONAL_LOCKS=0" process-environment))
      (apply fn buffer command args)))
  (advice-add 'vc-git--call :around 'vc-git--call@bug21559)

  (defun vc-git-command@bug21559
      (fn buffer okstatus file-or-list &rest flags)
    "Backport https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559."
    (let ((process-environment process-environment))
      (when revert-buffer-in-progress-p
        (push "GIT_OPTIONAL_LOCKS=0" process-environment))
      (apply fn buffer okstatus file-or-list flags)))
  (advice-add 'vc-git-command :around 'vc-git-command@bug21559)

  (defun auto-revert-handler@bug21559 (fn)
    "Backport https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559."
    (let ((revert-buffer-in-progress-p t))
      (funcall fn)))
  (advice-add 'auto-revert-handler :around 'auto-revert-handler@bug21559)
  )

(when (< emacs-major-version 26)
  ;; In Emacs 25 `completion-pcm--all-completions' reverses the
  ;; completion list.  This is the version from Emacs 26, which
  ;; fixes that issue.  bug#24676
  (defun magit-completion-pcm--all-completions (prefix pattern table pred)
    (if (completion-pcm--pattern-trivial-p pattern)
        (all-completions (concat prefix (car pattern)) table pred)
      (let* ((regex (completion-pcm--pattern->regex pattern))
             (case-fold-search completion-ignore-case)
             (completion-regexp-list (cons regex completion-regexp-list))
             (compl (all-completions
                     (concat prefix
                             (if (stringp (car pattern)) (car pattern) ""))
                     table pred)))
        (if (not (functionp table))
            compl
          (let ((poss ()))
            (dolist (c compl)
              (when (string-match-p regex c) (push c poss)))
            (nreverse poss)))))))

(defun magit-which-function ()
  "Return current function name based on point.

This is a simple wrapper around `which-function', that resets
Imenu's potentially outdated and therefore unreliable cache by
setting `imenu--index-alist' to nil before calling that function."
  (setq imenu--index-alist nil)
  (which-function))

;;; Kludges for Custom

(defun magit-custom-initialize-reset (symbol exp)
  "Initialize SYMBOL based on EXP.
Set the value of the variable SYMBOL, using `set-default'
\(unlike `custom-initialize-reset', which uses the `:set'
function if any).  The value is either the symbol's current
value (as obtained using the `:get' function), if any, or
the value in the symbol's `saved-value' property if any, or
\(last of all) the value of EXP."
  (set-default-toplevel-value
   symbol
   (condition-case nil
       (let ((def (default-toplevel-value symbol))
             (getter (get symbol 'custom-get)))
         (if getter (funcall getter symbol) def))
     (error
      (eval (let ((sv (get symbol 'saved-value)))
              (if sv (car sv) exp)))))))

(defun magit-hook-custom-get (symbol)
  (if (symbol-file symbol 'defvar)
      (default-toplevel-value symbol)
    ;;
    ;; Called by `custom-initialize-reset' on behalf of `symbol's
    ;; `defcustom', which is being evaluated for the first time to
    ;; set the initial value, but there's already a default value,
    ;; which most likely was established by one or more `add-hook'
    ;; calls.
    ;;
    ;; We combine the `standard-value' and the current value, while
    ;; preserving the order established by `:options', and return
    ;; the result of that to be used as the "initial" default value.
    ;;
    (let ((standard (eval (car (get symbol 'standard-value))))
          (current (default-toplevel-value symbol))
          (value nil))
      (dolist (fn (get symbol 'custom-options))
        (when (or (memq fn standard)
                  (memq fn current))
          (push fn value)))
      (dolist (fn current)
        (unless (memq fn value)
          (push fn value)))
      (nreverse value))))

;;; Kludges for Info Manuals

;;;###autoload
(defun Info-follow-nearest-node--magit-gitman (fn &optional fork)
  (let ((node (Info-get-token
               (point) "\\*note[ \n\t]+"
               "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
    (if (and node (string-match "^(gitman)\\(.+\\)" node))
        (pcase magit-view-git-manual-method
          ('info  (funcall fn fork))
          ('man   (require 'man)
                  (man (match-string 1 node)))
          ('woman (require 'woman)
                  (woman (match-string 1 node)))
          (_
           (user-error "Invalid value for `magit-view-git-manual-method'")))
      (funcall fn fork))))

;;;###autoload
(advice-add 'Info-follow-nearest-node :around
            #'Info-follow-nearest-node--magit-gitman)

;; When making changes here, then also adjust the copy in docs/Makefile.
;;;###autoload
(advice-add 'org-man-export :around #'org-man-export--magit-gitman)
;;;###autoload
(defun org-man-export--magit-gitman (fn link description format)
  (if (and (eq format 'texinfo)
           (string-prefix-p "git" link))
      (string-replace "%s" link "
@ifinfo
@ref{%s,,,gitman,}.
@end ifinfo
@ifhtml
@html
the <a href=\"http://git-scm.com/docs/%s\">%s(1)</a> manpage.
@end html
@end ifhtml
@iftex
the %s(1) manpage.
@end iftex
")
    (funcall fn link description format)))

;;; Kludges for Package Managers

(defun magit--straight-chase-links (filename)
  "Chase links in FILENAME until a name that is not a link.

This is the same as `file-chase-links', except that it also
handles fake symlinks that are created by the package manager
straight.el on Windows.

See <https://github.com/raxod502/straight.el/issues/520>."
  (when (and (bound-and-true-p straight-symlink-emulation-mode)
             (fboundp 'straight-chase-emulated-symlink))
    (when-let ((target (straight-chase-emulated-symlink filename)))
      (unless (eq target 'broken)
        (setq filename target))))
  (file-chase-links filename))

;;; Kludges for older Emacs versions

(if (fboundp 'with-connection-local-variables)
    (defalias 'magit--with-connection-local-variables
      #'with-connection-local-variables)
  (defmacro magit--with-connection-local-variables (&rest body)
    "Abridged `with-connection-local-variables' for pre Emacs 27 compatibility.
Bind shell file name and switch for remote execution.
`with-connection-local-variables' isn't available until Emacs 27.
This kludge provides the minimal functionality required by
Magit."
    `(if (file-remote-p default-directory)
         (pcase-let ((`(,shell-file-name ,shell-command-switch)
                      (with-no-warnings ; about unknown tramp functions
                        (require 'tramp)
                        (let ((vec (tramp-dissect-file-name
                                    default-directory)))
                          (list (tramp-get-method-parameter
                                 vec 'tramp-remote-shell)
                                (mapconcat #'identity
                                           (tramp-get-method-parameter
                                            vec 'tramp-remote-shell-args)
                                           " "))))))
           ,@body)
       ,@body)))

;;; Miscellaneous

(defun magit-message (format-string &rest args)
  "Display a message at the bottom of the screen, or not.
Like `message', except that if the users configured option
`magit-no-message' to prevent the message corresponding to
FORMAT-STRING to be displayed, then don't."
  (unless (--first (string-prefix-p it format-string) magit-no-message)
    (apply #'message format-string args)))

(defun magit-msg (format-string &rest args)
  "Display a message at the bottom of the screen, but don't log it.
Like `message', except that `message-log-max' is bound to nil."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

(defmacro magit--with-temp-position (buf pos &rest body)
  (declare (indent 2))
  `(with-current-buffer ,buf
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (or ,pos 1))
         ,@body))))

(defun magit--ellipsis (&optional where)
  "Build an ellipsis always as string, depending on WHERE."
  (if (stringp magit-ellipsis)
      magit-ellipsis
    (if-let ((pair (car (or
                         (alist-get (or where t) magit-ellipsis)
                         (alist-get t magit-ellipsis)))))
        (pcase-let ((`(,fancy . ,universal) pair))
          (let ((ellipsis (if (and fancy (char-displayable-p fancy))
                              fancy
                            universal)))
            (if (characterp ellipsis)
                (char-to-string ellipsis)
              ellipsis)))
      (user-error "Variable magit-ellipsis is invalid"))))

;;; _
(provide 'magit-base)
;;; magit-base.el ends here
