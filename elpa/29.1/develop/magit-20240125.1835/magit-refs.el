;;; magit-refs.el --- Listing references  -*- lexical-binding:t -*-

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

;; This library implements support for listing references in a buffer.

;;; Code:

(require 'magit)

;;; Options

(defgroup magit-refs nil
  "Inspect and manipulate Git branches and tags."
  :link '(info-link "(magit)References Buffer")
  :group 'magit-modes)

(defcustom magit-refs-mode-hook nil
  "Hook run after entering Magit-Refs mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-sections-hook
  '(magit-insert-error-header
    magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into a references buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-show-commit-count nil
  "Whether to show commit counts in Magit-Refs mode buffers.

all    Show counts for branches and tags.
branch Show counts for branches only.
nil    Never show counts.

To change the value in an existing buffer use the command
`magit-refs-set-show-commit-count'."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const :tag "For branches and tags" all)
                 (const :tag "For branches only"     branch)
                 (const :tag "Never"                 nil)))
(put 'magit-refs-show-commit-count 'safe-local-variable 'symbolp)
(put 'magit-refs-show-commit-count 'permanent-local t)

(defcustom magit-refs-pad-commit-counts nil
  "Whether to pad all counts on all sides in `magit-refs-mode' buffers.

If this is nil, then some commit counts are displayed right next
to one of the branches that appear next to the count, without any
space in between.  This might look bad if the branch name faces
look too similar to `magit-dimmed'.

If this is non-nil, then spaces are placed on both sides of all
commit counts."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type 'boolean)

(defvar magit-refs-show-push-remote nil
  "Whether to show the push-remotes of local branches.
Also show the commits that the local branch is ahead and behind
the push-target.  Unfortunately there is a bug in Git that makes
this useless (the commits ahead and behind the upstream are
shown), so this isn't enabled yet.")

(defcustom magit-refs-show-remote-prefix nil
  "Whether to show the remote prefix in lists of remote branches.

This is redundant because the name of the remote is already shown
in the heading preceding the list of its branches."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type 'boolean)

(defcustom magit-refs-margin
  (list nil
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-refs-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `magit-log-margin-show-committer-date'
  controls which date is being displayed.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-refs
  :group 'magit-margin
  :safe (lambda (val) (memq val '(all branch nil)))
  :type magit-log-margin--custom-type
  :initialize #'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-refs-mode))

(defcustom magit-refs-margin-for-tags nil
  "Whether to show information about tags in the margin.

This is disabled by default because it is slow if there are many
tags."
  :package-version '(magit . "2.9.0")
  :group 'magit-refs
  :group 'magit-margin
  :type 'boolean)

(defcustom magit-refs-primary-column-width (cons 16 32)
  "Width of the focus column in `magit-refs-mode' buffers.

The primary column is the column that contains the name of the
branch that the current row is about.

If this is an integer, then the column is that many columns wide.
Otherwise it has to be a cons-cell of two integers.  The first
specifies the minimal width, the second the maximal width.  In that
case the actual width is determined using the length of the names
of the shown local branches.  (Remote branches and tags are not
taken into account when calculating to optimal width.)"
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type '(choice (integer :tag "Constant wide")
                 (cons    :tag "Wide constrains"
                          (integer :tag "Minimum")
                          (integer :tag "Maximum"))))

(defcustom magit-refs-focus-column-width 5
  "Width of the focus column in `magit-refs-mode' buffers.

The focus column is the first column, which marks one
branch (usually the current branch) as the focused branch using
\"*\" or \"@\".  For each other reference, this column optionally
shows how many commits it is ahead of the focused branch and \"<\", or
if it isn't ahead then the commits it is behind and \">\", or if it
isn't behind either, then a \"=\".

This column may also display only \"*\" or \"@\" for the focused
branch, in which case this option is ignored.  Use \"L v\" to
change the verbosity of this column."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type 'integer)

(defcustom magit-refs-filter-alist nil
  "Alist controlling which refs are omitted from `magit-refs-mode' buffers.

The purpose of this option is to forgo displaying certain refs
based on their name.  If you want to not display any refs of a
certain type, then you should remove the appropriate function
from `magit-refs-sections-hook' instead.

All keys are tried in order until one matches.  Then its value
is used and subsequent elements are ignored.  If the value is
non-nil, then the reference is displayed, otherwise it is not.
If no element matches, then the reference is displayed.

A key can either be a regular expression that the refname has to
match, or a function that takes the refname as only argument and
returns a boolean.  A remote branch such as \"origin/master\" is
displayed as just \"master\", however for this comparison the
former is used."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type '(alist :key-type   (choice  :tag "Key" regexp function)
                :value-type (boolean :tag "Value"
                                     :on  "show (non-nil)"
                                     :off "omit (nil)")))

(defcustom magit-visit-ref-behavior nil
  "Control how `magit-visit-ref' behaves in `magit-refs-mode' buffers.

By default `magit-visit-ref' behaves like `magit-show-commit',
in all buffers, including `magit-refs-mode' buffers.  When the
type of the section at point is `commit' then \"RET\" is bound to
`magit-show-commit', and when the type is either `branch' or
`tag' then it is bound to `magit-visit-ref'.

\"RET\" is one of Magit's most essential keys and at least by
default it should behave consistently across all of Magit,
especially because users quickly learn that it does something
very harmless; it shows more information about the thing at point
in another buffer.

However \"RET\" used to behave differently in `magit-refs-mode'
buffers, doing surprising things, some of which cannot really be
described as \"visit this thing\".  If you have grown accustomed
to such inconsistent, but to you useful, behavior, then you can
restore that by adding one or more of the below symbols to the
value of this option.  But keep in mind that by doing so you
don't only introduce inconsistencies, you also lose some
functionality and might have to resort to `M-x magit-show-commit'
to get it back.

`magit-visit-ref' looks for these symbols in the order in which
they are described here.  If the presence of a symbol applies to
the current situation, then the symbols that follow do not affect
the outcome.

`focus-on-ref'

  With a prefix argument update the buffer to show commit counts
  and lists of cherry commits relative to the reference at point
  instead of relative to the current buffer or `HEAD'.

  Instead of adding this symbol, consider pressing \"C-u y o RET\".

`create-branch'

  If point is on a remote branch, then create a new local branch
  with the same name, use the remote branch as its upstream, and
  then check out the local branch.

  Instead of adding this symbol, consider pressing \"b c RET RET\",
  like you would do in other buffers.

`checkout-any'

  Check out the reference at point.  If that reference is a tag
  or a remote branch, then this results in a detached `HEAD'.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers.

`checkout-branch'

  Check out the local branch at point.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers."
  :package-version '(magit . "2.9.0")
  :group 'magit-refs
  :group 'magit-commands
  :options '(focus-on-ref create-branch checkout-any checkout-branch)
  :type '(list :convert-widget custom-hook-convert-widget))

;;; Mode

(defvar-keymap magit-refs-mode-map
  :doc "Keymap for `magit-refs-mode'."
  :parent magit-mode-map
  "C-y" #'magit-refs-set-show-commit-count
  "L"   #'magit-margin-settings)

(define-derived-mode magit-refs-mode magit-mode "Magit Refs"
  "Mode which lists and compares references.

This mode is documented in info node `(magit)References Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit or branch at point.

Type \\[magit-branch] to see available branch commands.
Type \\[magit-merge] to merge the branch or commit at point.
Type \\[magit-cherry-pick] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-refs-mode-map}"
  :group 'magit-refs
  (hack-dir-local-variables-non-file-buffer)
  (setq magit--imenu-group-types '(local remote tags)))

(defun magit-refs-setup-buffer (ref args)
  (magit-setup-buffer #'magit-refs-mode nil
    (magit-buffer-upstream ref)
    (magit-buffer-arguments args)))

(defun magit-refs-refresh-buffer ()
  (setq magit-set-buffer-margin-refresh (not (magit-buffer-margin-p)))
  (unless (magit-rev-verify magit-buffer-upstream)
    (setq magit-refs-show-commit-count nil))
  (magit-set-header-line-format
   (format "%s %s" magit-buffer-upstream
           (mapconcat #'identity magit-buffer-arguments " ")))
  (magit-insert-section (branchbuf)
    (magit-run-section-hook 'magit-refs-sections-hook))
  (add-hook 'kill-buffer-hook #'magit-preserve-section-visibility-cache))

(cl-defmethod magit-buffer-value (&context (major-mode magit-refs-mode))
  (cons magit-buffer-upstream magit-buffer-arguments))

;;; Commands

;;;###autoload (autoload 'magit-show-refs "magit-refs" nil t)
(transient-define-prefix magit-show-refs (&optional transient)
  "List and compare references in a dedicated buffer."
  :man-page "git-branch"
  :value (lambda ()
           (magit-show-refs-arguments magit-prefix-use-buffer-arguments))
  ["Arguments"
   (magit-for-each-ref:--contains)
   ("-M" "Merged"               "--merged=" magit-transient-read-revision)
   ("-m" "Merged to HEAD"       "--merged")
   ("-N" "Not merged"           "--no-merged=" magit-transient-read-revision)
   ("-n" "Not merged to HEAD"   "--no-merged")
   (magit-for-each-ref:--sort)]
  ["Actions"
   ("y" "Show refs, comparing them with HEAD"           magit-show-refs-head)
   ("c" "Show refs, comparing them with current branch" magit-show-refs-current)
   ("o" "Show refs, comparing them with other branch"   magit-show-refs-other)
   ("r" "Show refs, changing commit count display"
    magit-refs-set-show-commit-count)]
  (interactive (list (or (derived-mode-p 'magit-refs-mode)
                         current-prefix-arg)))
  (if transient
      (transient-setup 'magit-show-refs)
    (magit-refs-setup-buffer "HEAD" (magit-show-refs-arguments))))

(defun magit-show-refs-arguments (&optional use-buffer-args)
  (unless use-buffer-args
    (setq use-buffer-args magit-direct-use-buffer-arguments))
  (let (args)
    (cond
     ((eq transient-current-command 'magit-show-refs)
      (setq args (transient-args 'magit-show-refs)))
     ((eq major-mode 'magit-refs-mode)
      (setq args magit-buffer-arguments))
     ((and (memq use-buffer-args '(always selected))
           (and-let* ((buffer (magit-get-mode-buffer
                               'magit-refs-mode nil
                               (eq use-buffer-args 'selected))))
             (progn ; work around debbugs#31840
               (setq args (buffer-local-value 'magit-buffer-arguments buffer))
               t))))
     (t
      (setq args (alist-get 'magit-show-refs transient-values))))
    args))

(transient-define-argument magit-for-each-ref:--contains ()
  :description "Contains"
  :class 'transient-option
  :key "-c"
  :argument "--contains="
  :reader #'magit-transient-read-revision)

(transient-define-argument magit-for-each-ref:--sort ()
  :description "Sort"
  :class 'transient-option
  :key "-s"
  :argument "--sort="
  :reader #'magit-read-ref-sort)

(defun magit-read-ref-sort (prompt initial-input _history)
  (magit-completing-read prompt
                         '("-committerdate" "-authordate"
                           "committerdate" "authordate")
                         nil nil initial-input))

;;;###autoload
(defun magit-show-refs-head (&optional args)
  "List and compare references in a dedicated buffer.
Compared with `HEAD'."
  (interactive (list (magit-show-refs-arguments)))
  (magit-refs-setup-buffer "HEAD" args))

;;;###autoload
(defun magit-show-refs-current (&optional args)
  "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached."
  (interactive (list (magit-show-refs-arguments)))
  (magit-refs-setup-buffer (magit-get-current-branch) args))

;;;###autoload
(defun magit-show-refs-other (&optional ref args)
  "List and compare references in a dedicated buffer.
Compared with a branch read from the user."
  (interactive (list (magit-read-other-branch "Compare with")
                     (magit-show-refs-arguments)))
  (magit-refs-setup-buffer ref args))

(transient-define-suffix magit-refs-set-show-commit-count ()
  "Change for which refs the commit count is shown."
  :description "Change verbosity"
  :key "v"
  :transient nil
  :if-derived 'magit-refs-mode
  (interactive)
  (setq-local magit-refs-show-commit-count
              (magit-read-char-case "Show commit counts for " nil
                (?a "[a]ll refs" 'all)
                (?b "[b]ranches only" t)
                (?n "[n]othing" nil)))
  (magit-refresh))

(defun magit-visit-ref ()
  "Visit the reference or revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

This command behaves just like `magit-show-commit', except if
point is on a reference in a `magit-refs-mode' buffer (a buffer
listing branches and tags), in which case the behavior may be
different, but only if you have customized the option
`magit-visit-ref-behavior' (which see).  When invoked from a
menu this command always behaves like `magit-show-commit'."
  (interactive)
  (if (and (derived-mode-p 'magit-refs-mode)
           (magit-section-match '(branch tag))
           (not (magit-menu-position)))
      (let ((ref (oref (magit-current-section) value)))
        (cond (current-prefix-arg
               (cond ((memq 'focus-on-ref magit-visit-ref-behavior)
                      (magit-refs-setup-buffer ref (magit-show-refs-arguments)))
                     (magit-visit-ref-behavior
                      ;; Don't prompt for commit to visit.
                      (let ((current-prefix-arg nil))
                        (call-interactively #'magit-show-commit)))))
              ((and (memq 'create-branch magit-visit-ref-behavior)
                    (magit-section-match [branch remote]))
               (let ((branch (cdr (magit-split-branch-name ref))))
                 (if (magit-branch-p branch)
                     (if (magit-rev-eq branch ref)
                         (magit-call-git "checkout" branch)
                       (setq branch (propertize branch 'face 'magit-branch-local))
                       (setq ref (propertize ref 'face 'magit-branch-remote))
                       (pcase (prog1 (read-char-choice (format (propertize "\
Branch %s already exists.
  [c]heckout %s as-is
  [r]reset %s to %s and checkout %s
  [a]bort " 'face 'minibuffer-prompt) branch branch branch ref branch)
                                                       '(?c ?r ?a))
                                (message "")) ; otherwise prompt sticks
                         (?c (magit-call-git "checkout" branch))
                         (?r (magit-call-git "checkout" "-B" branch ref))
                         (?a (user-error "Abort"))))
                   (magit-call-git "checkout" "-b" branch ref))
                 (setq magit-buffer-upstream branch)
                 (magit-refresh)))
              ((or (memq 'checkout-any magit-visit-ref-behavior)
                   (and (memq 'checkout-branch magit-visit-ref-behavior)
                        (magit-section-match [branch local])))
               (magit-call-git "checkout" ref)
               (setq magit-buffer-upstream ref)
               (magit-refresh))
              (t
               (call-interactively #'magit-show-commit))))
    (call-interactively #'magit-show-commit)))

;;; Sections

(defvar-keymap magit-remote-section-map
  :doc "Keymap for `remote' sections."
  "<remap> <magit-file-rename>"  #'magit-remote-rename
  "<remap> <magit-delete-thing>" #'magit-remote-remove
  "<2>" (magit-menu-item "Rename %s" #'magit-remote-rename)
  "<1>" (magit-menu-item "Remove %m" #'magit-remote-remove))

(defvar-keymap magit-branch-section-map
  :doc "Keymap for `branch' sections."
  "<remap> <magit-file-rename>"  #'magit-branch-rename
  "<remap> <magit-delete-thing>" #'magit-branch-delete
  "<remap> <magit-visit-thing>"  #'magit-visit-ref
  "<3>" (magit-menu-item "Rename %s"    #'magit-branch-rename)
  "<2>" (magit-menu-item "Delete %m"    #'magit-branch-delete)
  "<1>" (magit-menu-item "Visit commit" #'magit-visit-ref))

(defvar-keymap magit-tag-section-map
  :doc "Keymap for `tag' sections."
  "<remap> <magit-delete-thing>" #'magit-tag-delete
  "<remap> <magit-visit-thing>"  #'magit-visit-ref
  "<2>" (magit-menu-item "Delete %m" #'magit-tag-delete)
  "<1>" (magit-menu-item "Visit %s"  #'magit-visit-ref))

(defun magit--painted-branch-as-menu-section (section)
  (and-let* ((branch (and (magit-section-match 'commit)
                          (magit--painted-branch-at-point))))
    (let ((dummy (magit-section :type 'branch :value branch)))
      (oset dummy keymap magit-branch-section-map)
      (dolist (slot '(start content hidden parent children))
        (when (slot-boundp section slot)
          (setf (eieio-oref dummy slot)
                (eieio-oref section slot))))
      dummy)))

(add-hook 'magit-menu-alternative-section-hook
          #'magit--painted-branch-as-menu-section)

(defun magit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (when-let* ((branch (magit-get-current-branch))
              (desc (magit-get "branch" branch "description"))
              (desc (split-string desc "\n")))
    (when (equal (car (last desc)) "")
      (setq desc (butlast desc)))
    (magit-insert-section (branchdesc branch t)
      (magit-insert-heading branch ": " (car desc))
      (when (cdr desc)
        (insert (mapconcat #'identity (cdr desc) "\n"))
        (insert "\n\n")))))

(defun magit-insert-tags ()
  "Insert sections showing all tags."
  (when-let ((tags (magit-git-lines "tag" "--list" "-n" magit-buffer-arguments)))
    (let ((_head (magit-rev-parse "HEAD")))
      (magit-insert-section (tags)
        (magit-insert-heading "Tags:")
        (dolist (tag tags)
          (string-match "^\\([^ \t]+\\)[ \t]+\\([^ \t\n].*\\)?" tag)
          (let ((tag (match-string 1 tag))
                (msg (match-string 2 tag)))
            (when (magit-refs--insert-refname-p tag)
              (magit-insert-section (tag tag t)
                (magit-insert-heading
                  (magit-refs--format-focus-column tag 'tag)
                  (propertize tag 'font-lock-face 'magit-tag)
                  (make-string
                   (max 1 (- (if (consp magit-refs-primary-column-width)
                                 (car magit-refs-primary-column-width)
                               magit-refs-primary-column-width)
                             (length tag)))
                   ?\s)
                  (and msg (magit-log-propertize-keywords nil msg)))
                (when (and magit-refs-margin-for-tags (magit-buffer-margin-p))
                  (magit-refs--format-margin tag))
                (magit-refs--insert-cherry-commits tag)))))
        (insert ?\n)
        (magit-make-margin-overlay nil t)))))

(defun magit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
  (dolist (remote (magit-list-remotes))
    (magit-insert-section (remote remote)
      (magit-insert-heading
        (let ((pull (magit-get "remote" remote "url"))
              (push (magit-get "remote" remote "pushurl")))
          (format (propertize "Remote %s (%s):"
                              'font-lock-face 'magit-section-heading)
                  (propertize remote 'font-lock-face 'magit-branch-remote)
                  (concat pull (and pull push ", ") push))))
      (let (head)
        (dolist (line (magit-git-lines "for-each-ref" "--format=\
%(symref:short)%00%(refname:short)%00%(refname)%00%(subject)"
                                       (concat "refs/remotes/" remote)
                                       magit-buffer-arguments))
          (pcase-let ((`(,head-branch ,branch ,ref ,msg)
                       (cl-substitute nil ""
                                      (split-string line "\0")
                                      :test #'equal)))
            (if head-branch
                ;; Note: Use `ref' instead of `branch' for the check
                ;; below because 'refname:short' shortens the remote
                ;; HEAD to '<remote>' instead of '<remote>/HEAD' as of
                ;; Git v2.40.0.
                (progn (cl-assert
                        (equal ref (concat "refs/remotes/" remote "/HEAD")))
                       (setq head head-branch))
              (when (magit-refs--insert-refname-p branch)
                (magit-insert-section (branch branch t)
                  (let ((headp (equal branch head))
                        (abbrev (if magit-refs-show-remote-prefix
                                    branch
                                  (substring branch (1+ (length remote))))))
                    (magit-insert-heading
                      (magit-refs--format-focus-column branch)
                      (magit-refs--propertize-branch
                       abbrev ref (and headp 'magit-branch-remote-head))
                      (make-string
                       (max 1 (- (if (consp magit-refs-primary-column-width)
                                     (car magit-refs-primary-column-width)
                                   magit-refs-primary-column-width)
                                 (length abbrev)))
                       ?\s)
                      (and msg (magit-log-propertize-keywords nil msg))))
                  (when (magit-buffer-margin-p)
                    (magit-refs--format-margin branch))
                  (magit-refs--insert-cherry-commits branch)))))))
      (insert ?\n)
      (magit-make-margin-overlay nil t))))

(defun magit-insert-local-branches ()
  "Insert sections showing all local branches."
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (dolist (line (magit-refs--format-local-branches))
      (pcase-let ((`(,branch . ,strings) line))
        (magit-insert-section
          ((eval (if branch 'branch 'commit))
           (or branch (magit-rev-parse "HEAD"))
           t)
          (apply #'magit-insert-heading strings)
          (when (magit-buffer-margin-p)
            (magit-refs--format-margin branch))
          (magit-refs--insert-cherry-commits branch))))
    (insert ?\n)
    (magit-make-margin-overlay nil t)))

(defun magit-refs--format-local-branches ()
  (let ((lines (seq-keep #'magit-refs--format-local-branch
                         (magit-git-lines
                          "for-each-ref"
                          (concat "--format=\
%(HEAD)%00%(refname:short)%00%(refname)%00\
%(upstream:short)%00%(upstream)%00%(upstream:track)%00"
                                  (if magit-refs-show-push-remote "\
%(push:remotename)%00%(push)%00%(push:track)%00%(subject)"
                                 "%00%00%00%(subject)"))
                          "refs/heads"
                          magit-buffer-arguments))))
    (unless (magit-get-current-branch)
      (push (magit-refs--format-local-branch
             (concat "*\0\0\0\0\0\0\0\0" (magit-rev-format "%s")))
            lines))
    (setq-local magit-refs-primary-column-width
                (let ((def (default-value 'magit-refs-primary-column-width)))
                  (if (atom def)
                      def
                    (pcase-let ((`(,min . ,max) def))
                      (min max (apply #'max min (mapcar #'car lines)))))))
    (mapcar (pcase-lambda (`(,_ ,branch ,focus ,branch-desc ,u:ahead ,p:ahead
                                ,u:behind ,upstream ,p:behind ,push ,msg))
              (list branch focus branch-desc u:ahead p:ahead
                    (make-string (max 1 (- magit-refs-primary-column-width
                                           (length (concat branch-desc
                                                           u:ahead
                                                           p:ahead
                                                           u:behind))))
                                 ?\s)
                    u:behind upstream p:behind push
                    msg))
            lines)))

(defun magit-refs--format-local-branch (line)
  (pcase-let ((`(,head ,branch ,ref ,upstream ,u:ref ,u:track
                       ,push ,p:ref ,p:track ,msg)
               (cl-substitute nil "" (split-string line "\0") :test #'equal)))
    (when (or (not branch)
              (magit-refs--insert-refname-p branch))
      (let* ((headp (equal head "*"))
             (pushp (and push
                         magit-refs-show-push-remote
                         (magit-rev-verify p:ref)
                         (not (equal p:ref u:ref))))
             (branch-desc
              (if branch
                  (magit-refs--propertize-branch
                   branch ref (and headp 'magit-branch-current))
                (magit--propertize-face "(detached)" 'magit-branch-warning)))
             (u:ahead  (and u:track
                            (string-match "ahead \\([0-9]+\\)" u:track)
                            (magit--propertize-face
                             (concat (and magit-refs-pad-commit-counts " ")
                                     (match-string 1 u:track)
                                     ">")
                             'magit-dimmed)))
             (u:behind (and u:track
                            (string-match "behind \\([0-9]+\\)" u:track)
                            (magit--propertize-face
                             (concat "<"
                                     (match-string 1 u:track)
                                     (and magit-refs-pad-commit-counts " "))
                             'magit-dimmed)))
             (p:ahead  (and pushp p:track
                            (string-match "ahead \\([0-9]+\\)" p:track)
                            (magit--propertize-face
                             (concat (match-string 1 p:track)
                                     ">"
                                     (and magit-refs-pad-commit-counts " "))
                             'magit-branch-remote)))
             (p:behind (and pushp p:track
                            (string-match "behind \\([0-9]+\\)" p:track)
                            (magit--propertize-face
                             (concat "<"
                                     (match-string 1 p:track)
                                     (and magit-refs-pad-commit-counts " "))
                             'magit-dimmed))))
        (list (1+ (length (concat branch-desc u:ahead p:ahead u:behind)))
              branch
              (magit-refs--format-focus-column branch headp)
              branch-desc u:ahead p:ahead u:behind
              (and upstream
                   (concat (if (equal u:track "[gone]")
                               (magit--propertize-face upstream 'error)
                             (magit-refs--propertize-branch upstream u:ref))
                           " "))
              (and pushp
                   (concat p:behind
                           (magit--propertize-face
                            push 'magit-branch-remote)
                           " "))
              (and msg (magit-log-propertize-keywords nil msg)))))))

(defun magit-refs--format-focus-column (ref &optional type)
  (let ((focus magit-buffer-upstream)
        (width (if magit-refs-show-commit-count
                   magit-refs-focus-column-width
                 1)))
    (format
     (format "%%%ss " width)
     (cond ((or (equal ref focus)
                (and (eq type t)
                     (equal focus "HEAD")))
            (magit--propertize-face (concat (if (equal focus "HEAD") "@" "*")
                                            (make-string (1- width) ?\s))
                                    'magit-section-heading))
           ((if (eq type 'tag)
                (eq magit-refs-show-commit-count 'all)
              magit-refs-show-commit-count)
            (pcase-let ((`(,behind ,ahead)
                         (magit-rev-diff-count magit-buffer-upstream ref)))
              (magit--propertize-face
               (cond ((> ahead  0) (concat "<" (number-to-string ahead)))
                     ((> behind 0) (concat (number-to-string behind) ">"))
                     (t "="))
               'magit-dimmed)))
           (t "")))))

(defun magit-refs--propertize-branch (branch ref &optional head-face)
  (let ((face (cdr (cl-find-if (pcase-lambda (`(,re . ,_))
                                 (string-match-p re ref))
                               magit-ref-namespaces))))
    (magit--propertize-face
     branch (if head-face (list face head-face) face))))

(defun magit-refs--insert-refname-p (refname)
  (if-let ((entry (seq-find (pcase-lambda (`(,key . ,_))
                              (if (functionp key)
                                  (funcall key refname)
                                (string-match-p key refname)))
                            magit-refs-filter-alist)))
      (cdr entry)
    t))

(defun magit-refs--insert-cherry-commits (ref)
  (magit-insert-section-body
    (let ((start (point))
          (magit-insert-section--current nil))
      (magit-git-wash (apply-partially #'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) magit-buffer-upstream ref)
      (if (= (point) start)
          (message "No cherries for %s" ref)
        (magit-make-margin-overlay nil t)))))

(defun magit-refs--format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (magit-rev-format "%ct%cN" commit)))
      (magit-log-format-margin commit
                               (substring line 10)
                               (substring line 0 10)))))

;;; _
(provide 'magit-refs)
;;; magit-refs.el ends here
