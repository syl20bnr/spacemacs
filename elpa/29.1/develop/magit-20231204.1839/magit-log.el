;;; magit-log.el --- Inspect Git history  -*- lexical-binding:t; coding:utf-8 -*-

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

;; This library implements support for looking at Git logs, including
;; special logs like cherry-logs, as well as for selecting a commit
;; from a log.

;;; Code:

(require 'magit-core)
(require 'magit-diff)

(declare-function magit-blob-visit "magit-files" (blob-or-file))
(declare-function magit-cherry-apply "magit-sequence" (commit &optional args))
(declare-function magit-insert-head-branch-header "magit-status"
                  (&optional branch))
(declare-function magit-insert-upstream-branch-header "magit-status"
                  (&optional branch pull keyword))
(declare-function magit-read-file-from-rev "magit-files"
                  (rev prompt &optional default))
(declare-function magit-rebase--get-state-lines "magit-sequence"
                  (file))
(declare-function magit-show-commit "magit-diff"
                  (arg1 &optional arg2 arg3 arg4))
(declare-function magit-reflog-format-subject "magit-reflog" (subject))
(defvar magit-refs-focus-column-width)
(defvar magit-refs-margin)
(defvar magit-refs-show-commit-count)
(defvar magit-buffer-margin)
(defvar magit-status-margin)
(defvar magit-status-sections-hook)

(require 'ansi-color)
(require 'crm)
(require 'which-func)

;;; Options
;;;; Log Mode

(defgroup magit-log nil
  "Inspect and manipulate Git history."
  :link '(info-link "(magit)Logging")
  :group 'magit-commands
  :group 'magit-modes)

(defcustom magit-log-mode-hook nil
  "Hook run after entering Magit-Log mode."
  :group 'magit-log
  :type 'hook)

(defcustom magit-log-remove-graph-args '("--follow" "--grep" "-G" "-S" "-L")
  "The log arguments that cause the `--graph' argument to be dropped.

The default value lists the arguments that are incompatible with
`--graph' and therefore must be dropped when that is used.  You
can add additional arguments that are available in `magit-log',
but I recommend that you don't do that.  Nowadays I would define
this as a constant, but I am preserving it as an option, in case
someone actually customized it."
  :package-version '(magit . "2.3.0")
  :group 'magit-log
  :type '(repeat (string :tag "Argument"))
  :options '("--follow" "--grep" "-G" "-S" "-L"))

(defcustom magit-log-revision-headers-format "\
%+b%+N
Author:    %aN <%aE>
Committer: %cN <%cE>"
  "Additional format string used with the `++header' argument."
  :package-version '(magit . "3.2.0")
  :group 'magit-log
  :type 'string)

(defcustom magit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.
Only considered when moving past the last entry with
`magit-goto-*-section' commands."
  :group 'magit-log
  :type 'boolean)

(defcustom magit-log-margin '(t age magit-log-margin-width t 18)
  "Format of the margin in `magit-log-mode' buffers.

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
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize #'magit-custom-initialize-reset
  :set (apply-partially #'magit-margin-set-variable 'magit-log-mode))

(defcustom magit-log-margin-show-committer-date nil
  "Whether to show the committer date in the margin.

This option only controls whether the committer date is displayed
instead of the author date.  Whether some date is displayed in
the margin and whether the margin is displayed at all is
controlled by other options."
  :package-version '(magit . "3.0.0")
  :group 'magit-log
  :group 'magit-margin
  :type 'boolean)

(defcustom magit-log-show-refname-after-summary nil
  "Whether to show refnames after commit summaries.
This is useful if you use really long branch names."
  :package-version '(magit . "2.2.0")
  :group 'magit-log
  :type 'boolean)

(defcustom magit-log-highlight-keywords t
  "Whether to highlight bracketed keywords in commit summaries."
  :package-version '(magit . "2.12.0")
  :group 'magit-log
  :type 'boolean)

(defcustom magit-log-header-line-function #'magit-log-header-line-sentence
  "Function used to generate text shown in header line of log buffers."
  :package-version '(magit . "2.12.0")
  :group 'magit-log
  :type '(choice (function-item magit-log-header-line-arguments)
                 (function-item magit-log-header-line-sentence)
                 function))

(defcustom magit-log-trace-definition-function #'magit-which-function
  "Function used to determine the function at point.
This is used by the command `magit-log-trace-definition'.
You should prefer `magit-which-function' over `which-function'
because the latter may make use of Imenu's outdated cache."
  :package-version '(magit . "3.0.0")
  :group 'magit-log
  :type '(choice (function-item magit-which-function)
                 (function-item which-function)
                 (function-item add-log-current-defun)
                 function))

(defcustom magit-log-color-graph-limit 256
  "Number of commits over which log graphs are not colored.
When showing more commits than specified, then the `--color'
argument is silently dropped.  This is necessary because the
`ansi-color' library, which is used to turn control sequences
into faces, is just too slow."
  :package-version '(magit . "4.0.0")
  :group 'magit-log
  :type 'number)

(defcustom magit-log-show-signatures-limit 256
  "Number of commits over which signatures are not verified.
When showing more commits than specified by this option, then the
`--show-signature' argument, if specified, is silently dropped.
This is necessary because checking the signature of a large
number of commits is just too slow."
  :package-version '(magit . "4.0.0")
  :group 'magit-log
  :type 'number)

(defface magit-log-graph
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the graph part of the log output."
  :group 'magit-faces)

(defface magit-log-author
  '((((class color) (background light))
     :foreground "firebrick"
     :slant normal
     :weight normal)
    (((class color) (background  dark))
     :foreground "tomato"
     :slant normal
     :weight normal))
  "Face for the author part of the log output."
  :group 'magit-faces)

(defface magit-log-date
  '((((class color) (background light))
     :foreground "grey30"
     :slant normal
     :weight normal)
    (((class color) (background  dark))
     :foreground "grey80"
     :slant normal
     :weight normal))
  "Face for the date part of the log output."
  :group 'magit-faces)

(defface magit-header-line-log-select
  '((t :inherit bold))
  "Face for the `header-line' in `magit-log-select-mode'."
  :group 'magit-faces)

;;;; File Log

(defcustom magit-log-buffer-file-locked t
  "Whether `magit-log-buffer-file-quick' uses a dedicated buffer."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :group 'magit-log
  :type 'boolean)

;;;; Select Mode

(defcustom magit-log-select-show-usage 'both
  "Whether to show usage information when selecting a commit from a log.
The message can be shown in the `echo-area' or the `header-line', or in
`both' places.  If the value isn't one of these symbols, then it should
be nil, in which case no usage information is shown."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(choice (const :tag "in echo-area" echo-area)
                 (const :tag "in header-line" header-line)
                 (const :tag "in both places" both)
                 (const :tag "nowhere")))

(defcustom magit-log-select-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width t
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-log-select-mode' buffers.

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
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize #'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-log-select-mode))

;;;; Cherry Mode

(defcustom magit-cherry-sections-hook
  '(magit-insert-cherry-headers
    magit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'hook)

(defcustom magit-cherry-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width t
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-cherry-mode' buffers.

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
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize #'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-cherry-mode))

;;;; Log Sections

(defcustom magit-log-section-commit-count 10
  "How many recent commits to show in certain log sections.
How many recent commits `magit-insert-recent-commits' and
`magit-insert-unpulled-from-upstream-or-recent' (provided
the upstream isn't ahead of the current branch) show."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'number)

(defcustom magit-log-merged-commit-count 20
  "How many surrounding commits to show for `magit-log-merged'.
`magit-log-merged' will shows approximately half of this number
commits before and half after."
  :package-version '(magit . "3.3.0")
  :group 'magit-log
  :type 'integer)

;;; Arguments
;;;; Prefix Classes

(defclass magit-log-prefix (transient-prefix)
  ((history-key :initform 'magit-log)
   (major-mode  :initform 'magit-log-mode)))

(defclass magit-log-refresh-prefix (magit-log-prefix)
  ((history-key :initform 'magit-log)
   (major-mode  :initform nil)))

;;;; Prefix Methods

(cl-defmethod transient-init-value ((obj magit-log-prefix))
  (pcase-let ((`(,args ,files)
               (magit-log--get-value 'magit-log-mode
                                     magit-prefix-use-buffer-arguments)))
    (unless (eq transient-current-command 'magit-dispatch)
      (when-let ((file (magit-file-relative-name)))
        (setq files (list file))))
    (oset obj value (if files `(("--" ,@files) ,args) args))))

(cl-defmethod transient-init-value ((obj magit-log-refresh-prefix))
  (oset obj value (if magit-buffer-log-files
                      `(("--" ,@magit-buffer-log-files)
                        ,magit-buffer-log-args)
                    magit-buffer-log-args)))

(cl-defmethod transient-set-value ((obj magit-log-prefix))
  (magit-log--set-value obj))

(cl-defmethod transient-save-value ((obj magit-log-prefix))
  (magit-log--set-value obj 'save))

;;;; Argument Access

(defun magit-log-arguments (&optional mode)
  "Return the current log arguments."
  (if (memq transient-current-command '(magit-log magit-log-refresh))
      (magit--transient-args-and-files)
    (magit-log--get-value (or mode 'magit-log-mode))))

(defun magit-log--get-value (mode &optional use-buffer-args)
  (unless use-buffer-args
    (setq use-buffer-args magit-direct-use-buffer-arguments))
  (let (args files)
    (cond
     ((and (memq use-buffer-args '(always selected current))
           (eq major-mode mode))
      (setq args  magit-buffer-log-args)
      (setq files magit-buffer-log-files))
     ((and (memq use-buffer-args '(always selected))
           (when-let ((buffer (magit-get-mode-buffer
                               mode nil
                               (eq use-buffer-args 'selected))))
             (setq args  (buffer-local-value 'magit-buffer-log-args buffer))
             (setq files (buffer-local-value 'magit-buffer-log-files buffer))
             t)))
     ((plist-member (symbol-plist mode) 'magit-log-current-arguments)
      (setq args (get mode 'magit-log-current-arguments)))
     ((when-let ((elt (assq (intern (format "magit-log:%s" mode))
                            transient-values)))
        (setq args (cdr elt))
        t))
     (t
      (setq args (get mode 'magit-log-default-arguments))))
    (list args files)))

(defun magit-log--set-value (obj &optional save)
  (pcase-let* ((obj  (oref obj prototype))
               (mode (or (oref obj major-mode) major-mode))
               (key  (intern (format "magit-log:%s" mode)))
               (`(,args ,files) (magit--transient-args-and-files)))
    (put mode 'magit-log-current-arguments args)
    (when save
      (setf (alist-get key transient-values) args)
      (transient-save-values))
    (transient--history-push obj)
    (setq magit-buffer-log-args args)
    (unless (derived-mode-p 'magit-log-select-mode)
      (setq magit-buffer-log-files files))
    (magit-refresh)))

;;; Commands
;;;; Prefix Commands

;;;###autoload (autoload 'magit-log "magit-log" nil t)
(transient-define-prefix magit-log ()
  "Show a commit or reference log."
  :man-page "git-log"
  :class 'magit-log-prefix
  ;; The grouping in git-log(1) appears to be guided by implementation
  ;; details, so our logical grouping only follows it to an extend.
  ;; Arguments that are "misplaced" here:
  ;;   1. From "Commit Formatting".
  ;;   2. From "Common Diff Options".
  ;;   3. From unnamed first group.
  ;;   4. Implemented by Magit.
  ["Commit limiting"
   (magit-log:-n)
   (magit:--author)
   (7 magit-log:--since)
   (7 magit-log:--until)
   (magit-log:--grep)
   (7 "-i" "Search case-insensitive" ("-i" "--regexp-ignore-case"))
   (7 "-I" "Invert search pattern"   "--invert-grep")
   (magit-log:-G)     ;2
   (magit-log:-S)     ;2
   (magit-log:-L)     ;2
   (7 "=m" "Omit merges"            "--no-merges")
   (7 "=p" "First parent"           "--first-parent")]
  ["History simplification"
   (  "-D" "Simplify by decoration"                  "--simplify-by-decoration")
   (magit:--)
   (  "-f" "Follow renames when showing single-file log"     "--follow") ;3
   (6 "/s" "Only commits changing given paths"               "--sparse")
   (7 "/d" "Only selected commits plus meaningful history"   "--dense")
   (7 "/a" "Only commits existing directly on ancestry path" "--ancestry-path")
   (6 "/f" "Do not prune history"                            "--full-history")
   (7 "/m" "Prune some history"                              "--simplify-merges")]
  ["Commit ordering"
   (magit-log:--*-order)
   ("-r" "Reverse order" "--reverse")]
  ["Formatting"
   ("-g" "Show graph"          "--graph")          ;1
   ("-c" "Show graph in color" "--color")          ;2
   ("-d" "Show refnames"       "--decorate")       ;3
   ("=S" "Show signatures"     "--show-signature") ;1
   ("-h" "Show header"         "++header")         ;4
   ("-p" "Show diffs"          ("-p" "--patch"))   ;2
   ("-s" "Show diffstats"      "--stat")]          ;2
  [["Log"
    ("l" "current"             magit-log-current)
    ("h" "HEAD"                magit-log-head)
    ("u" "related"             magit-log-related)
    ("o" "other"               magit-log-other)]
   [""
    ("L" "local branches"      magit-log-branches)
    ("b" "all branches"        magit-log-all-branches)
    ("a" "all references"      magit-log-all)
    (7 "B" "matching branches" magit-log-matching-branches)
    (7 "T" "matching tags"     magit-log-matching-tags)
    (7 "m" "merged"            magit-log-merged)]
   ["Reflog"
    ("r" "current"             magit-reflog-current)
    ("H" "HEAD"                magit-reflog-head)
    ("O" "other"               magit-reflog-other)]
   [:if (lambda ()
          (and (fboundp 'magit--any-wip-mode-enabled-p)
               (magit--any-wip-mode-enabled-p)))
    :description "Wiplog"
    ("i" "index"          magit-wip-log-index)
    ("w" "worktree"       magit-wip-log-worktree)]
   ["Other"
    (5 "s" "shortlog"    magit-shortlog)]])

;;;###autoload (autoload 'magit-log-refresh "magit-log" nil t)
(transient-define-prefix magit-log-refresh ()
  "Change the arguments used for the log(s) in the current buffer."
  :man-page "git-log"
  :class 'magit-log-refresh-prefix
  [:if-mode magit-log-mode
   :class transient-subgroups
   ["Commit limiting"
    (magit-log:-n)
    (magit:--author)
    (magit-log:--grep)
    (7 "-i" "Search case-insensitive" ("-i" "--regexp-ignore-case"))
    (7 "-I" "Invert search pattern"   "--invert-grep")
    (magit-log:-G)
    (magit-log:-S)
    (magit-log:-L)]
   ["History simplification"
    (  "-D" "Simplify by decoration"                  "--simplify-by-decoration")
    (magit:--)
    (  "-f" "Follow renames when showing single-file log"     "--follow") ;3
    (6 "/s" "Only commits changing given paths"               "--sparse")
    (7 "/d" "Only selected commits plus meaningful history"   "--dense")
    (7 "/a" "Only commits existing directly on ancestry path" "--ancestry-path")
    (6 "/f" "Do not prune history"                            "--full-history")
    (7 "/m" "Prune some history"                              "--simplify-merges")]
   ["Commit ordering"
    (magit-log:--*-order)
    ("-r" "Reverse order" "--reverse")]
   ["Formatting"
    ("-g" "Show graph"              "--graph")
    ("-c" "Show graph in color"     "--color")
    ("-d" "Show refnames"           "--decorate")
    ("=S" "Show signatures"         "--show-signature")
    ("-h" "Show header"             "++header")
    ("-p" "Show diffs"              ("-p" "--patch"))
    ("-s" "Show diffstats"          "--stat")]]
  [:if-not-mode magit-log-mode
   :description "Arguments"
   (magit-log:-n)
   (magit-log:--*-order)
   ("-g" "Show graph"               "--graph")
   ("-c" "Show graph in color"      "--color")
   ("-d" "Show refnames"            "--decorate")]
  [["Refresh"
    ("g" "buffer"                   magit-log-refresh)
    ("s" "buffer and set defaults"  transient-set-and-exit)
    ("w" "buffer and save defaults" transient-save-and-exit)]
   ["Margin"
    (magit-toggle-margin)
    (magit-cycle-margin-style)
    (magit-toggle-margin-details)
    (magit-toggle-log-margin-style)]
   [:if-mode magit-log-mode
    :description "Toggle"
    ("b" "buffer lock"              magit-toggle-buffer-lock)]]
  (interactive)
  (cond
   ((not (eq transient-current-command 'magit-log-refresh))
    (pcase major-mode
      ('magit-reflog-mode
       (user-error "Cannot change log arguments in reflog buffers"))
      ('magit-cherry-mode
       (user-error "Cannot change log arguments in cherry buffers")))
    (transient-setup 'magit-log-refresh))
   (t
    (pcase-let ((`(,args ,files) (magit-log-arguments)))
      (setq magit-buffer-log-args args)
      (unless (derived-mode-p 'magit-log-select-mode)
        (setq magit-buffer-log-files files)))
    (magit-refresh))))

;;;; Infix Commands

(transient-define-argument magit-log:-n ()
  :description "Limit number of commits"
  :class 'transient-option
  ;; For historic reasons (and because it easy to guess what "-n"
  ;; stands for) this is the only argument where we do not use the
  ;; long argument ("--max-count").
  :shortarg "-n"
  :argument "-n"
  :reader #'transient-read-number-N+)

(transient-define-argument magit:--author ()
  :description "Limit to author"
  :class 'transient-option
  :key "-A"
  :argument "--author="
  :reader #'magit-transient-read-person)

(transient-define-argument magit-log:--since ()
  :description "Limit to commits since"
  :class 'transient-option
  :key "=s"
  :argument "--since="
  :reader #'transient-read-date)

(transient-define-argument magit-log:--until ()
  :description "Limit to commits until"
  :class 'transient-option
  :key "=u"
  :argument "--until="
  :reader #'transient-read-date)

(transient-define-argument magit-log:--*-order ()
  :description "Order commits by"
  :class 'transient-switches
  :key "-o"
  :argument-format "--%s-order"
  :argument-regexp "\\(--\\(topo\\|author-date\\|date\\)-order\\)"
  :choices '("topo" "author-date" "date"))

(transient-define-argument magit-log:--grep ()
  :description "Search messages"
  :class 'transient-option
  :key "-F"
  :argument "--grep=")

(transient-define-argument magit-log:-G ()
  :description "Search changes"
  :class 'transient-option
  :argument "-G")

(transient-define-argument magit-log:-S ()
  :description "Search occurrences"
  :class 'transient-option
  :argument "-S")

(transient-define-argument magit-log:-L ()
  :description "Trace line evolution"
  :class 'transient-option
  :argument "-L"
  :reader #'magit-read-file-trace)

(defun magit-read-file-trace (&rest _ignored)
  (let ((file  (magit-read-file-from-rev "HEAD" "File"))
        (trace (magit-read-string "Trace")))
    (concat trace ":" file)))

;;;; Setup Commands

(defvar-keymap magit-log-read-revs-map
  :parent crm-local-completion-map
  "SPC" #'self-insert-command)

(defun magit-log-read-revs (&optional use-current)
  (or (and use-current (and-let* ((buf (magit-get-current-branch))) (list buf)))
      (let ((crm-separator "\\(\\.\\.\\.?\\|[, ]\\)")
            (crm-local-completion-map magit-log-read-revs-map))
        (split-string (magit-completing-read-multiple
                       "Log rev,s: "
                       (magit-list-refnames nil t)
                       nil nil nil 'magit-revision-history
                       (or (magit-branch-or-commit-at-point)
                           (and (not use-current)
                                (magit-get-previous-branch)))
                       nil t)
                      "[, ]" t))))

(defun magit-log-read-pattern (option)
  "Read a string from the user to pass as parameter to OPTION."
  (magit-read-string (format "Type a pattern to pass to %s" option)))

;;;###autoload
(defun magit-log-current (revs &optional args files)
  "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer."
  (interactive (cons (magit-log-read-revs t)
                     (magit-log-arguments)))
  (magit-log-setup-buffer revs args files))

;;;###autoload
(defun magit-log-head (&optional args files)
  "Show log for `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list "HEAD") args files))

;;;###autoload
(defun magit-log-related (revs &optional args files)
  "Show log for the current branch, its upstream and its push target.
When the upstream is a local branch, then also show its own
upstream.  When `HEAD' is detached, then show log for that, the
previously checked out branch and its upstream and push-target."
  (interactive
   (cons (let ((current (magit-get-current-branch))
               head rebase target upstream upup)
           (unless current
             (setq rebase (magit-rebase--get-state-lines "head-name"))
             (cond (rebase
                    (setq rebase (magit-ref-abbrev rebase))
                    (setq current rebase)
                    (setq head "HEAD"))
                   (t (setq current (magit-get-previous-branch)))))
           (cond (current
                  (setq current
                        (magit--propertize-face current'magit-branch-local))
                  (setq target (magit-get-push-branch current t))
                  (setq upstream (magit-get-upstream-branch current))
                  (when upstream
                    (setq upup (and (magit-local-branch-p upstream)
                                    (magit-get-upstream-branch upstream)))))
                 (t (setq head "HEAD")))
           (delq nil (list current head target upstream upup)))
         (magit-log-arguments)))
  (magit-log-setup-buffer revs args files))

;;;###autoload
(defun magit-log-other (revs &optional args files)
  "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates."
  (interactive (cons (magit-log-read-revs)
                     (magit-log-arguments)))
  (magit-log-setup-buffer revs args files))

;;;###autoload
(defun magit-log-branches (&optional args files)
  "Show log for all local branches and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (if (magit-get-current-branch)
                              (list "--branches")
                            (list "HEAD" "--branches"))
                          args files))

;;;###autoload
(defun magit-log-matching-branches (pattern &optional args files)
  "Show log for all branches matching PATTERN and `HEAD'."
  (interactive (cons (magit-log-read-pattern "--branches") (magit-log-arguments)))
  (magit-log-setup-buffer
   (list "HEAD" (format "--branches=%s" pattern))
   args files))

;;;###autoload
(defun magit-log-matching-tags (pattern &optional args files)
  "Show log for all tags matching PATTERN and `HEAD'."
  (interactive (cons (magit-log-read-pattern "--tags") (magit-log-arguments)))
  (magit-log-setup-buffer
   (list "HEAD" (format "--tags=%s" pattern))
   args files))

;;;###autoload
(defun magit-log-all-branches (&optional args files)
  "Show log for all local and remote branches and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (if (magit-get-current-branch)
                              (list "--branches" "--remotes")
                            (list "HEAD" "--branches" "--remotes"))
                          args files))

;;;###autoload
(defun magit-log-all (&optional args files)
  "Show log for all references and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (if (magit-get-current-branch)
                              (list "--all")
                            (list "HEAD" "--all"))
                          args files))

;;;###autoload
(defun magit-log-buffer-file (&optional follow beg end)
  "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches."
  (interactive
   (cons current-prefix-arg
         (and (region-active-p)
              (magit-file-relative-name)
              (save-restriction
                (widen)
                (list (line-number-at-pos (region-beginning))
                      (line-number-at-pos
                       (let ((end (region-end)))
                         (if (char-after end)
                             end
                           ;; Ensure that we don't get the line number
                           ;; of a trailing newline.
                           (1- end)))))))))
  (require 'magit)
  (if-let ((file (magit-file-relative-name)))
      (magit-log-setup-buffer
       (list (or magit-buffer-refname
                 (magit-get-current-branch)
                 "HEAD"))
       (let ((args (car (magit-log-arguments))))
         (when (and follow (not (member "--follow" args)))
           (push "--follow" args))
         (when (and (file-regular-p
                     (expand-file-name file (magit-toplevel)))
                    beg end)
           (setq args (cons (format "-L%s,%s:%s" beg end file)
                            (cl-delete "-L" args :test
                                       #'string-prefix-p)))
           (setq file nil))
         args)
       (and file (list file))
       magit-log-buffer-file-locked)
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun magit-log-trace-definition (file fn rev)
  "Show log for the definition at point."
  (interactive (list (or (magit-file-relative-name)
                         (user-error "Buffer isn't visiting a file"))
                     (or (funcall magit-log-trace-definition-function)
                         (user-error "No function at point found"))
                     (or magit-buffer-refname
                         (magit-get-current-branch)
                         "HEAD")))
  (require 'magit)
  (magit-log-setup-buffer
   (list rev)
   (cons (format "-L:%s%s:%s"
                 (string-replace ":" "\\:" (regexp-quote fn))
                 (if (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
                     ;; Git doesn't treat "-" the same way as
                     ;; "_", leading to false-positives such as
                     ;; "foo-suffix" being considered a match
                     ;; for "foo".  Wing it.
                     "\\( \\|$\\)"
                   ;; We could use "\\b" here, but since Git
                   ;; already does something equivalent, that
                   ;; isn't necessary.
                   "")
                 file)
         (cl-delete "-L" (car (magit-log-arguments))
                    :test #'string-prefix-p))
   nil magit-log-buffer-file-locked))

(defun magit-diff-trace-definition ()
  "Show log for the definition at point in a diff."
  (interactive)
  (pcase-let ((`(,buf ,pos) (magit-diff-visit-file--noselect)))
    (magit--with-temp-position buf pos
      (call-interactively #'magit-log-trace-definition))))

;;;###autoload
(defun magit-log-merged (commit branch &optional args files)
  "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\". If COMMIT is
directly on BRANCH, then show approximately
`magit-log-merged-commit-count' surrounding commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged."
  (interactive
   (append (let ((commit (magit-read-branch-or-commit "Log merge of commit")))
             (list commit
                   (magit-read-other-branch "Merged into" commit)))
           (magit-log-arguments)))
  (unless (magit-git-executable-find "git-when-merged")
    (user-error "This command requires git-when-merged (%s)"
                "https://github.com/mhagger/git-when-merged"))
  (let (exit m)
    (with-temp-buffer
      (save-excursion
        (setq exit (magit-process-git t "when-merged" "-c"
                                      (magit-abbrev-arg)
                                      commit branch)))
      (setq m (buffer-substring-no-properties (point) (line-end-position))))
    (if (zerop exit)
        (magit-log-setup-buffer (list (format "%s^1..%s" m m))
                                args files nil commit)
      ;; Output: "<ref><lots of spaces><message>".
      ;; This is not the same as `string-trim'.
      (setq m (string-trim-left (substring m (string-match " " m))))
      (if (equal m "Commit is directly on this branch.")
          (let* ((from (format "%s~%d" commit
                               (/ magit-log-merged-commit-count 2)))
                 (to (- (car (magit-rev-diff-count branch commit t))
                        (/ magit-log-merged-commit-count 2)))
                 (to (if (<= to 0)
                         branch
                       (format "%s~%s" branch to))))
            (unless (magit-rev-verify-commit from)
              (setq from (magit-git-string "rev-list" "--max-parents=0"
                                           commit)))
            (magit-log-setup-buffer (list (concat from ".." to))
                                    (cons "--first-parent" args)
                                    files nil commit))
        (user-error "Could not find when %s was merged into %s: %s"
                    commit branch m)))))

;;;; Limit Commands

(defun magit-log-toggle-commit-limit ()
  "Toggle the number of commits the current log buffer is limited to.
If the number of commits is currently limited, then remove that
limit.  Otherwise set it to 256."
  (interactive)
  (magit-log-set-commit-limit (lambda (&rest _) nil)))

(defun magit-log-double-commit-limit ()
  "Double the number of commits the current log buffer is limited to."
  (interactive)
  (magit-log-set-commit-limit '*))

(defun magit-log-half-commit-limit ()
  "Half the number of commits the current log buffer is limited to."
  (interactive)
  (magit-log-set-commit-limit '/))

(defun magit-log-set-commit-limit (fn)
  (let* ((val magit-buffer-log-args)
         (arg (--first (string-match "^-n\\([0-9]+\\)?$" it) val))
         (num (and arg (string-to-number (match-string 1 arg))))
         (num (if num (funcall fn num 2) 256)))
    (setq val (remove arg val))
    (setq magit-buffer-log-args
          (if (and num (> num 0))
              (cons (format "-n%d" num) val)
            val)))
  (magit-refresh))

(defun magit-log-get-commit-limit (&optional args)
  (and-let* ((str (--first (string-match "^-n\\([0-9]+\\)?$" it)
                           (or args magit-buffer-log-args))))
    (string-to-number (match-string 1 str))))

;;;; Mode Commands

(defun magit-log-bury-buffer (&optional arg)
  "Bury the current buffer or the revision buffer in the same frame.
Like `magit-mode-bury-buffer' (which see) but with a negative
prefix argument instead bury the revision buffer, provided it
is displayed in the current frame."
  (interactive "p")
  (if (< arg 0)
      (let* ((buf (magit-get-mode-buffer 'magit-revision-mode))
             (win (and buf (get-buffer-window buf (selected-frame)))))
        (if win
            (with-selected-window win
              (with-current-buffer buf
                (magit-mode-bury-buffer (> (abs arg) 1))))
          (user-error "No revision buffer in this frame")))
    (magit-mode-bury-buffer (> arg 1))))

;;;###autoload
(defun magit-log-move-to-parent (&optional n)
  "Move to the Nth parent of the current commit."
  (interactive "p")
  (when (derived-mode-p 'magit-log-mode)
    (when (magit-section-match 'commit)
      (let* ((section (magit-current-section))
             (parent-rev (format "%s^%s" (oref section value) (or n 1))))
        (if-let ((parent-hash (magit-rev-parse "--short" parent-rev)))
            (if-let ((parent (--first (equal (oref it value)
                                             parent-hash)
                                      (magit-section-siblings section 'next))))
                (magit-section-goto parent)
              (user-error
               (substitute-command-keys
                (concat "Parent " parent-hash " not found.  Try typing "
                        "\\[magit-log-double-commit-limit] first"))))
          (user-error "Parent %s does not exist" parent-rev))))))

(defun magit-log-move-to-revision (rev)
  "Read a revision and move to it in current log buffer.

If the chosen reference or revision isn't being displayed in
the current log buffer, then inform the user about that and do
nothing else.

If invoked outside any log buffer, then display the log buffer
of the current repository first; creating it if necessary."
  (interactive (list (magit-read-branch-or-commit "In log, jump to")))
  (with-current-buffer
      (cond ((derived-mode-p 'magit-log-mode)
             (current-buffer))
            ((and-let* ((buf (magit-get-mode-buffer 'magit-log-mode)))
               (pop-to-buffer-same-window buf)))
            (t
             (apply #'magit-log-all-branches (magit-log-arguments))))
    (unless (magit-log-goto-commit-section (magit-rev-abbrev rev))
      (user-error "%s isn't visible in the current log buffer" rev))))

;;;; Shortlog Commands

;;;###autoload (autoload 'magit-shortlog "magit-log" nil t)
(transient-define-prefix magit-shortlog ()
  "Show a history summary."
  :man-page "git-shortlog"
  :value '("--numbered" "--summary")
  ["Arguments"
   ("-n" "Sort by number of commits"      ("-n" "--numbered"))
   ("-s" "Show commit count summary only" ("-s" "--summary"))
   ("-e" "Show email addresses"           ("-e" "--email"))
   ("-g" "Group commits by" "--group="
    :choices ("author" "committer" "trailer:"))
   (7 "-f" "Format string" "--format=")
   (7 "-w" "Linewrap" "-w" :class transient-option)]
  ["Shortlog"
   ("s" "since" magit-shortlog-since)
   ("r" "range" magit-shortlog-range)])

(defun magit-git-shortlog (rev args)
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create "*magit-shortlog*")
      (setq default-directory dir)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (magit-git-insert "shortlog" args rev))
        (switch-to-buffer-other-window (current-buffer))))))

;;;###autoload
(defun magit-shortlog-since (rev args)
  "Show a history summary for commits since REV."
  (interactive
   (list (magit-read-branch-or-commit "Shortlog since" (magit-get-current-tag))
         (transient-args 'magit-shortlog)))
  (magit-git-shortlog (concat rev "..") args))

;;;###autoload
(defun magit-shortlog-range (rev-or-range args)
  "Show a history summary for commit or range REV-OR-RANGE."
  (interactive
   (list (magit-read-range-or-commit "Shortlog for revision or range")
         (transient-args 'magit-shortlog)))
  (magit-git-shortlog rev-or-range args))

;;; Log Mode

(defvar magit-log-disable-graph-hack-args
  '("-G" "--grep" "--author")
  "Arguments which disable the graph speedup hack.")

(defvar-keymap magit-log-mode-map
  :doc "Keymap for `magit-log-mode'."
  :parent magit-mode-map
  "C-c C-b" #'magit-go-backward
  "C-c C-f" #'magit-go-forward
  "C-c C-n" #'magit-log-move-to-parent
  "j" #'magit-log-move-to-revision
  "=" #'magit-log-toggle-commit-limit
  "+" #'magit-log-double-commit-limit
  "-" #'magit-log-half-commit-limit
  "q" #'magit-log-bury-buffer)

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at Git log.

This mode is documented in info node `(magit)Log Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-branch] to see available branch commands.
Type \\[magit-merge] to merge the branch or commit at point.
Type \\[magit-cherry-pick] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-log-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq magit--imenu-item-types 'commit))

(put 'magit-log-mode 'magit-log-default-arguments
     '("--graph" "-n256" "--decorate"))

(defun magit-log-setup-buffer (revs args files &optional locked focus)
  (require 'magit)
  (with-current-buffer
      (magit-setup-buffer #'magit-log-mode locked
        (magit-buffer-revisions revs)
        (magit-buffer-log-args args)
        (magit-buffer-log-files files))
    (when (if focus
              (magit-log-goto-commit-section focus)
            (magit-log-goto-same-commit))
      (magit-section-update-highlight))
    (current-buffer)))

(defun magit-log-refresh-buffer ()
  (let ((revs  magit-buffer-revisions)
        (args  magit-buffer-log-args)
        (files magit-buffer-log-files)
        (limit (magit-log-get-commit-limit)))
    (magit-set-header-line-format
     (funcall magit-log-header-line-function revs args files))
    (unless (length= files 1)
      (setq args (remove "--follow" args)))
    (when (and (car magit-log-remove-graph-args)
               (--any-p (string-match-p
                         (concat "^" (regexp-opt magit-log-remove-graph-args)) it)
                        args))
      (setq args (remove "--graph" args)))
    (setq args (magit-log--maybe-drop-color-graph args limit))
    (when-let* ((limit limit)
                (limit (* 2 limit)) ; increase odds for complete graph
                (count (and (length= revs 1)
                            (> limit 1024) ; otherwise it's fast enough
                            (setq revs (car revs))
                            (not (string-search ".." revs))
                            (not (member revs '("--all" "--branches")))
                            (not (seq-some
                                  (lambda (arg)
                                    (--any-p (string-prefix-p it arg)
                                             magit-log-disable-graph-hack-args))
                                  args))
                            (magit-git-string "rev-list" "--count"
                                              "--first-parent" args revs))))
      (setq revs (if (< (string-to-number count) limit)
                     revs
                   (format "%s~%s..%s" revs limit revs))))
    (let ((delay (cl-find-if (lambda (arg)
                               (member arg '("++header" "--patch" "--stat")))
                             args)))
      (setq magit-section-inhibit-markers (if delay 'delay t))
      (setq magit-section-insert-in-reverse (not delay)))
    (magit-insert-section (logbuf)
      (magit--insert-log t revs args files))))

(defvar-local magit-log--color-graph nil)

(defun magit-log--maybe-drop-color-graph (args limit)
  (if (member "--color" args)
      (if (cond ((not (member "--graph" args)))
                ((not magit-log-color-graph-limit) nil)
                ((not limit)
                 (message "Dropping --color because -n isn't set (see %s)"
                          'magit-log-color-graph-limit))
                ((> limit magit-log-color-graph-limit)
                 (message "Dropping --color because -n is larger than %s"
                          'magit-log-color-graph-limit)))
          (progn (setq args (remove "--color" args))
                 (setq magit-log--color-graph nil))
        (setq magit-log--color-graph t))
    (setq magit-log--color-graph nil))
  args)

(cl-defmethod magit-buffer-value (&context (major-mode magit-log-mode))
  (append magit-buffer-revisions
          (if (and magit-buffer-revisions magit-buffer-log-files)
              (cons "--" magit-buffer-log-files)
            magit-buffer-log-files)))

(defun magit-log-header-line-arguments (revs args files)
  "Return string describing some of the used arguments."
  (mapconcat (lambda (arg)
               (if (string-search " " arg)
                   (prin1 arg)
                 arg))
             `("git" "log" ,@args ,@revs "--" ,@files)
             " "))

(defun magit-log-header-line-sentence (revs args files)
  "Return string containing all arguments."
  (concat "Commits in "
          (mapconcat #'identity revs " ")
          (and (member "--reverse" args)
               " in reverse")
          (and files (concat " touching "
                             (mapconcat #'identity files " ")))
          (--some (and (string-prefix-p "-L" it)
                       (concat " " it))
                  args)))

(defun magit-insert-log (revs &optional args files)
  (declare (obsolete magit--insert-log "Magit 4.0.0"))
  (magit--insert-log nil revs args files))

(defun magit--insert-log (keep-error revs &optional args files)
  "Insert a log section.
Do not add this to a hook variable."
  (declare (indent defun))
  (setq magit-section-preserve-visibility t) ; TODO do it here?
  (let ((magit-git-global-arguments
         (remove "--literal-pathspecs" magit-git-global-arguments)))
    (magit--git-wash (apply-partially #'magit-log-wash-log 'log) keep-error
      "log"
      (format "--format=%s%%h%%x0c%s%%x0c%s%%x0c%%aN%%x0c%s%%x0c%%s%s"
              (if (and (member "--left-right" args)
                       (not (member "--graph" args)))
                  "%m "
                "")
              (if (member "--decorate" args) "%D" "")
              (if (not (member "--show-signature" args))
                  ""
                (setq args (remove "--show-signature" args))
                (let ((limit (magit-log-get-commit-limit args)))
                  (cond
                   ((not limit)
                    (message
                     "Dropping --show-signature because -n isn't set (see %s)"
                     'magit-log-show-signatures-limit)
                    "")
                   ((> limit magit-log-show-signatures-limit)
                    (message
                     "Dropping --show-signature because -n is larger than %s"
                     'magit-log-show-signatures-limit)
                    "")
                   ("%G?"))))
              (if magit-log-margin-show-committer-date "%ct" "%at")
              (if (member "++header" args)
                  (if (member "--graph" (setq args (remove "++header" args)))
                      (concat "\n" magit-log-revision-headers-format "\n")
                    (concat "\n" magit-log-revision-headers-format "\n"))
                ""))
      (progn
        (when-let ((order (--first (string-match "^\\+\\+order=\\(.+\\)$" it)
                                   args)))
          (setq args (cons (format "--%s-order" (match-string 1 order))
                           (remove order args))))
        (when (member "--decorate" args)
          (setq args (cons "--decorate=full" (remove "--decorate" args))))
        (when (member "--reverse" args)
          (setq args (remove "--graph" args)))
        (setq args (magit-diff--maybe-add-stat-arguments args))
        args)
      "--use-mailmap" "--no-prefix" revs "--" files)))

(cl-defmethod magit-menu-common-value ((_section magit-commit-section))
  (or (magit-diff--region-range)
      (oref (magit-current-section) value)))

(defvar-keymap magit-commit-section-map
  :doc "Keymap for `commit' sections."
  "<remap> <magit-visit-thing>" #'magit-show-commit
  "<3>" (magit-menu-item "Apply %x" #'magit-cherry-apply)
  "<2>" (magit-menu-item "Show commit %x" #'magit-show-commit
                         '(:visible (not (region-active-p))))
  "<1>" (magit-menu-item "Diff %x" #'magit-diff-range
                         '(:visible (region-active-p))))

(defvar-keymap magit-module-commit-section-map
  :doc "Keymap for `module-commit' sections."
  :parent magit-commit-section-map)

(defconst magit-log-heading-re
  ;; Note: A form feed instead of a null byte is used as the delimiter
  ;; because using the latter interferes with the graph prefix when
  ;; ++header is used.
  (concat "^"
          "\\(?4:[-_/|\\*o<>. ]*\\)"               ; graph
          "\\(?1:[0-9a-fA-F]+\\)?"               ; hash
          "\\(?3:[^\n]+\\)?"                   ; refs
          "\\(?7:[BGUXYREN]\\)?"                 ; gpg
          "\\(?5:[^\n]*\\)"                    ; author
          ;; Note: Date is optional because, prior to Git v2.19.0,
          ;; `git rebase -i --root` corrupts the root's author date.
          "\\(?6:[^\n]*\\)"                    ; date
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-cherry-re
  (concat "^"
          "\\(?8:[-+]\\) "                         ; cherry
          "\\(?1:[0-9a-fA-F]+\\) "                 ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-module-re
  (concat "^"
          "\\(?:\\(?11:[<>]\\) \\)?"               ; side
          "\\(?1:[0-9a-fA-F]+\\) "                 ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-vis-re
  (concat "^"
          "\\(?4:[-_/|\\*o<>. ]*\\)"               ; graph
          "\\(?1:[0-9a-fA-F]+\\)?\0"               ; hash
          "\\(?3:[^\0\n]+\\)?\0"                   ; refs
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-log-re
  (concat "^# "
          "\\(?3:[^: \n]+:\\) "                    ; "refs"
          "\\[\\(?1:[^]\n]+\\)\\] "                ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-reflog-re
  (concat "^"
          "\\(?1:[^\0\n]+\\)\0"                    ; hash
          "\\(?5:[^\0\n]*\\)\0"                    ; author
          "\\(?:\\(?:[^@\n]+@{\\(?6:[^}\n]+\\)}\0" ; date
                                                 ;;; refsub
          "\\(?10:merge \\|autosave \\|restart \\|rewritten \\|[^:\n]+: \\)?"
          "\\(?2:.*\\)\\)\\|\0\\)$"))              ; msg

(defconst magit-reflog-subject-re
  (concat "\\(?1:[^ ]+\\) ?"                       ; command
          "\\(?2:\\(?: ?-[^ ]+\\)+\\)?"            ; option
          "\\(?: ?(\\(?3:[^)]+\\))\\)?"))          ; type

(defconst magit-log-stash-re
  (concat "^"
          "\\(?1:[^\0\n]+\\)\0"                    ; "hash"
          "\\(?5:[^\0\n]*\\)\0"                    ; author
          "\\(?6:[^\0\n]+\\)\0"                    ; date
          "\\(?2:.*\\)$"))                         ; msg

(defvar magit-log-count nil)

(defvar magit-log-format-message-function #'magit-log-propertize-keywords)

(defun magit-log-wash-log (style args)
  (setq args (flatten-tree args))
  (when (if (derived-mode-p 'magit-log-mode)
            magit-log--color-graph
          (and (member "--graph" args)
               (member "--color" args)))
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (put-text-property beg end 'font-lock-face
                                (or face 'magit-log-graph)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((magit-log-count 0))
    (when (looking-at "^\\.\\.\\.")
      (magit-delete-line))
    (magit-wash-sequence (apply-partially #'magit-log-wash-rev style
                                          (magit-abbrev-length)))
    (if (derived-mode-p 'magit-log-mode 'magit-reflog-mode)
        (when (eq magit-log-count (magit-log-get-commit-limit))
          (magit-insert-section (longer)
            (insert-text-button
             (substitute-command-keys
              (format "Type \\<%s>\\[%s] to show more history"
                      'magit-log-mode-map
                      'magit-log-double-commit-limit))
             'action (lambda (_button)
                       (magit-log-double-commit-limit))
             'follow-link t
             'mouse-face 'magit-section-highlight)))
      (insert ?\n))))

(cl-defun magit-log-wash-rev (style abbrev)
  (when (derived-mode-p 'magit-log-mode 'magit-reflog-mode)
    (cl-incf magit-log-count))
  (looking-at (pcase style
                ('log        magit-log-heading-re)
                ('cherry     magit-log-cherry-re)
                ('module     magit-log-module-re)
                ('reflog     magit-log-reflog-re)
                ('stash      magit-log-stash-re)
                ('bisect-vis magit-log-bisect-vis-re)
                ('bisect-log magit-log-bisect-log-re)))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry _ refsub side) nil
    (setq msg (substring-no-properties msg))
    (when refs
      (setq refs (substring-no-properties refs)))
    (let ((align (or (eq style 'cherry)
                     (not (member "--stat" magit-buffer-log-args))))
          (non-graph-re (if (eq style 'bisect-vis)
                            magit-log-bisect-vis-re
                          magit-log-heading-re)))
      (magit-delete-line)
      ;; If the reflog entries have been pruned, the output of `git
      ;; reflog show' includes a partial line that refers to the hash
      ;; of the youngest expired reflog entry.
      (when (and (eq style 'reflog) (not date))
        (cl-return-from magit-log-wash-rev t))
      (magit-insert-section section (commit hash)
        (pcase style
          ('stash      (oset section type 'stash))
          ('module     (oset section type 'module-commit))
          ('bisect-log (setq hash (magit-rev-parse "--short" hash))))
        (setq hash (propertize hash 'font-lock-face
                               (pcase (and gpg (aref gpg 0))
                                 (?G 'magit-signature-good)
                                 (?B 'magit-signature-bad)
                                 (?U 'magit-signature-untrusted)
                                 (?X 'magit-signature-expired)
                                 (?Y 'magit-signature-expired-key)
                                 (?R 'magit-signature-revoked)
                                 (?E 'magit-signature-error)
                                 (?N 'magit-hash)
                                 (_  'magit-hash))))
        (when cherry
          (when (and (derived-mode-p 'magit-refs-mode)
                     magit-refs-show-commit-count)
            (insert (make-string (1- magit-refs-focus-column-width) ?\s)))
          (insert (propertize cherry 'font-lock-face
                              (if (string= cherry "-")
                                  'magit-cherry-equivalent
                                'magit-cherry-unmatched)))
          (insert ?\s))
        (when side
          (insert (propertize side 'font-lock-face
                              (if (string= side "<")
                                  'magit-cherry-equivalent
                                'magit-cherry-unmatched)))
          (insert ?\s))
        (when align
          (insert hash ?\s))
        (when graph
          (insert graph))
        (unless align
          (insert hash ?\s))
        (when (and refs (not magit-log-show-refname-after-summary))
          (insert (magit-format-ref-labels refs) ?\s))
        (when (eq style 'reflog)
          (insert (format "%-2s " (1- magit-log-count)))
          (when refsub
            (insert (magit-reflog-format-subject
                     (substring refsub 0
                                (if (string-search ":" refsub) -2 -1))))))
        (insert (funcall magit-log-format-message-function hash msg))
        (when (and refs magit-log-show-refname-after-summary)
          (insert ?\s)
          (insert (magit-format-ref-labels refs)))
        (insert ?\n)
        (when (memq style '(log reflog stash))
          (goto-char (line-beginning-position))
          (when (and refsub
                     (string-match "\\`\\([^ ]\\) \\+\\(..\\)\\(..\\)" date))
            (setq date (+ (string-to-number (match-string 1 date))
                          (* (string-to-number (match-string 2 date)) 60 60)
                          (* (string-to-number (match-string 3 date)) 60))))
          (save-excursion
            (backward-char)
            (magit-log-format-margin hash author date)))
        (when (and (eq style 'cherry)
                   (magit-buffer-margin-p))
          (save-excursion
            (backward-char)
            (apply #'magit-log-format-margin hash
                   (split-string (magit-rev-format "%aN%x00%ct" hash) "\0"))))
        (when (and graph
                   (not (eobp))
                   (not (looking-at non-graph-re)))
          (when (looking-at "")
            (magit-insert-heading)
            (delete-char 1)
            (magit-insert-section (commit-header)
              (forward-line)
              (magit-insert-heading)
              (re-search-forward "")
              (delete-char -1)
              (forward-char)
              (insert ?\n))
            (delete-char 1))
          (if (looking-at "^\\(---\\|\n\s\\|\ndiff\\)")
              (let ((limit (save-excursion
                             (and (re-search-forward non-graph-re nil t)
                                  (match-beginning 0)))))
                (unless (oref magit-insert-section--current content)
                  (magit-insert-heading))
                (delete-char (if (looking-at "\n") 1 4))
                (magit-diff-wash-diffs (list "--stat") limit))
            (when align
              (setq align (make-string (1+ abbrev) ? )))
            (when (and (not (eobp)) (not (looking-at non-graph-re)))
              (when align
                (setq align (make-string (1+ abbrev) ? )))
              (while (and (not (eobp)) (not (looking-at non-graph-re)))
                (when align
                  (save-excursion (insert align)))
                (magit-make-margin-overlay)
                (forward-line))
              ;; When `--format' is used and its value isn't one of the
              ;; predefined formats, then `git-log' does not insert a
              ;; separator line.
              (save-excursion
                (forward-line -1)
                (looking-at "[-_/|\\*o<>. ]*"))
              (setq graph (match-string 0))
              (unless (string-match-p "[/\\.]" graph)
                (insert graph ?\n))))))))
  t)

(defun magit-log-propertize-keywords (_rev msg)
  (let ((boundary 0))
    (when (string-match "^\\(?:squash\\|fixup\\)! " msg boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) (1- boundary)
                       'magit-keyword-squash msg))
    (when magit-log-highlight-keywords
      (while (string-match "\\[[^][]*]" msg boundary)
        (setq boundary (match-end 0))
        (magit--put-face (match-beginning 0) boundary
                         'magit-keyword msg))))
  msg)

(defun magit-log-maybe-show-more-commits (section)
  "When point is at the end of a log buffer, insert more commits.

Log buffers end with a button \"Type + to show more history\".
When the use of a section movement command puts point on that
button, then automatically show more commits, without the user
having to press \"+\".

This function is called by `magit-section-movement-hook' and
exists mostly for backward compatibility reasons."
  (when (and (eq (oref section type) 'longer)
             magit-log-auto-more)
    (magit-log-double-commit-limit)
    (forward-line -1)
    (magit-section-forward)))

(add-hook 'magit-section-movement-hook #'magit-log-maybe-show-more-commits)

(defvar magit--update-revision-buffer nil)

(defun magit-log-maybe-update-revision-buffer (&optional _)
  "When moving in a log or cherry buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-log-mode 'magit-cherry-mode 'magit-reflog-mode)
    (magit--maybe-update-revision-buffer)))

(add-hook 'magit-section-movement-hook #'magit-log-maybe-update-revision-buffer)

(defun magit--maybe-update-revision-buffer ()
  (when-let* ((commit (magit-section-value-if 'commit))
              (buffer (magit-get-mode-buffer 'magit-revision-mode nil t)))
    (if magit--update-revision-buffer
        (setq magit--update-revision-buffer (list commit buffer))
      (setq magit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (let ((args (let ((magit-direct-use-buffer-arguments 'selected))
                     (magit-show-commit--arguments))))
         (lambda ()
           (pcase-let ((`(,rev ,buf) magit--update-revision-buffer))
             (setq magit--update-revision-buffer nil)
             (when (buffer-live-p buf)
               (let ((magit-display-buffer-noselect t))
                 (apply #'magit-show-commit rev args))))
           (setq magit--update-revision-buffer nil)))))))

(defvar magit--update-blob-buffer nil)

(defun magit-log-maybe-update-blob-buffer (&optional _)
  "When moving in a log or cherry buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-log-mode 'magit-cherry-mode 'magit-reflog-mode)
    (magit--maybe-update-blob-buffer)))

(defun magit--maybe-update-blob-buffer ()
  (when-let* ((commit (magit-section-value-if 'commit))
              (buffer (--first (with-current-buffer it
                                 (eq revert-buffer-function
                                     'magit-revert-rev-file-buffer))
                               (mapcar #'window-buffer (window-list)))))
    (if magit--update-blob-buffer
        (setq magit--update-blob-buffer (list commit buffer))
      (setq magit--update-blob-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (lambda ()
         (pcase-let ((`(,rev ,buf) magit--update-blob-buffer))
           (setq magit--update-blob-buffer nil)
           (when (buffer-live-p buf)
             (with-selected-window (get-buffer-window buf)
               (with-current-buffer buf
                 (save-excursion
                   (magit-blob-visit (list (magit-rev-parse rev)
                                           (magit-file-relative-name
                                            magit-buffer-file-name)))))))))))))

(defun magit-log-goto-commit-section (rev)
  (let ((abbrev (magit-rev-format "%h" rev)))
    (when-let ((section (--first (equal (oref it value) abbrev)
                                 (oref magit-root-section children))))
      (goto-char (oref section start)))))

(defun magit-log-goto-same-commit ()
  (when (and magit-previous-section
             (magit-section-match '(commit branch)
                                  magit-previous-section))
    (magit-log-goto-commit-section (oref magit-previous-section value))))

;;; Log Margin

(defvar-local magit-log-margin-show-shortstat nil)

(transient-define-suffix magit-toggle-log-margin-style ()
  "Toggle between the regular and the shortstat margin style.
The shortstat style is experimental and rather slow."
  :description "Toggle shortstat"
  :key "x"
  :transient t
  (interactive)
  (setq magit-log-margin-show-shortstat
        (not magit-log-margin-show-shortstat))
  (magit-set-buffer-margin nil t))

(defun magit-log-format-margin (rev author date)
  (when (magit-margin-option)
    (if magit-log-margin-show-shortstat
        (magit-log-format-shortstat-margin rev)
      (magit-log-format-author-margin author date))))

(defun magit-log-format-author-margin (author date &optional previous-line)
  (pcase-let ((`(,_ ,style ,width ,details ,details-width)
               (or magit-buffer-margin
                   (symbol-value (magit-margin-option)))))
    (magit-make-margin-overlay
     (concat (and details
                  (concat (magit--propertize-face
                           (truncate-string-to-width
                            (or author "")
                            details-width
                            nil ?\s
                            (magit--ellipsis 'margin))
                           'magit-log-author)
                          " "))
             (magit--propertize-face
              (if (stringp style)
                  (format-time-string
                   style
                   (seconds-to-time (string-to-number date)))
                (pcase-let* ((abbr (eq style 'age-abbreviated))
                             (`(,cnt ,unit) (magit--age date abbr)))
                  (format (format (if abbr "%%2d%%-%dc" "%%2d %%-%ds")
                                  (- width (if details (1+ details-width) 0)))
                          cnt unit)))
              'magit-log-date))
     previous-line)))

(defun magit-log-format-shortstat-margin (rev)
  (magit-make-margin-overlay
   (if-let ((line (and rev (magit-git-string
                            "show" "--format=" "--shortstat" rev))))
       (if (string-match "\
\\([0-9]+\\) files? changed, \
\\(?:\\([0-9]+\\) insertions?(\\+)\\)?\
\\(?:\\(?:, \\)?\\([0-9]+\\) deletions?(-)\\)?\\'" line)
           (magit-bind-match-strings (files add del) line
             (format
              "%5s %5s%4s"
              (if add
                  (magit--propertize-face (format "%s+" add)
                                          'magit-diffstat-added)
                "")
              (if del
                  (magit--propertize-face (format "%s-" del)
                                          'magit-diffstat-removed)
                "")
              files))
         "")
     "")))

(defun magit-log-margin-width (style details details-width)
  (if magit-log-margin-show-shortstat
      16
    (+ (if details (1+ details-width) 0)
       (if (stringp style)
           (length (format-time-string style))
         (+ 2 ; two digits
            1 ; trailing space
            (if (eq style 'age-abbreviated)
                1  ; single character
              (+ 1 ; gap after digits
                 (apply #'max (--map (max (length (nth 1 it))
                                          (length (nth 2 it)))
                                     magit--age-spec)))))))))

;;; Select Mode

(defvar-keymap magit-log-select-mode-map
  :doc "Keymap for `magit-log-select-mode'."
  :parent magit-log-mode-map
  "C-c C-b" #'undefined
  "C-c C-f" #'undefined
  "."       #'magit-log-select-pick
  "e"       #'magit-log-select-pick
  "C-c C-c" #'magit-log-select-pick
  "q"       #'magit-log-select-quit
  "C-c C-k" #'magit-log-select-quit)
(put 'magit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'magit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(define-derived-mode magit-log-select-mode magit-log-mode "Magit Select"
  "Mode for selecting a commit from history.

This mode is documented in info node `(magit)Select from Log'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

\\<magit-log-select-mode-map>\
Type \\[magit-log-select-pick] to select the commit at point.
Type \\[magit-log-select-quit] to abort without selecting a commit."
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(put 'magit-log-select-mode 'magit-log-default-arguments
     '("--graph" "-n256" "--decorate"))

(defun magit-log-select-setup-buffer (revs args)
  (magit-setup-buffer #'magit-log-select-mode nil
    (magit-buffer-revisions revs)
    (magit-buffer-log-args args)))

(defun magit-log-select-refresh-buffer ()
  (setq magit-section-inhibit-markers t)
  (setq magit-section-insert-in-reverse t)
  (magit-insert-section (logbuf)
    (magit--insert-log t magit-buffer-revisions
      (magit-log--maybe-drop-color-graph
       magit-buffer-log-args
       (magit-log-get-commit-limit)))))

(cl-defmethod magit-buffer-value (&context (major-mode magit-log-select-mode))
  magit-buffer-revisions)

(defvar-local magit-log-select-pick-function nil)
(defvar-local magit-log-select-quit-function nil)

(defun magit-log-select (pick &optional msg quit branch args initial)
  (declare (indent defun))
  (unless initial
    (setq initial (magit-commit-at-point)))
  (magit-log-select-setup-buffer
   (or branch (magit-get-current-branch) "HEAD")
   (append args
           (car (magit-log--get-value 'magit-log-select-mode
                                      magit-direct-use-buffer-arguments))))
  (when initial
    (magit-log-goto-commit-section initial))
  (setq magit-log-select-pick-function pick)
  (setq magit-log-select-quit-function quit)
  (when magit-log-select-show-usage
    (let ((pick (propertize (substitute-command-keys
                             "\\[magit-log-select-pick]")
                            'font-lock-face
                            'magit-header-line-key))
          (quit (propertize (substitute-command-keys
                             "\\[magit-log-select-quit]")
                            'font-lock-face
                            'magit-header-line-key)))
      (setq msg (format-spec
                 (if msg
                     (if (string-suffix-p "," msg)
                         (concat msg " or %q to abort")
                       msg)
                   "Type %p to select commit at point, or %q to abort")
                 `((?p . ,pick)
                   (?q . ,quit)))))
    (magit--add-face-text-property
     0 (length msg) 'magit-header-line-log-select t msg)
    (when (memq magit-log-select-show-usage '(both header-line))
      (magit-set-header-line-format msg))
    (when (memq magit-log-select-show-usage '(both echo-area))
      (message "%s" (substring-no-properties msg)))))

(defun magit-log-select-pick ()
  "Select the commit at point and act on it.
Call `magit-log-select-pick-function' with the selected
commit as argument."
  (interactive)
  (let ((fun magit-log-select-pick-function)
        (rev (magit-commit-at-point)))
    (magit-mode-bury-buffer 'kill)
    (funcall fun rev)))

(defun magit-log-select-quit ()
  "Abort selecting a commit, don't act on any commit.
Call `magit-log-select-quit-function' if set."
  (interactive)
  (let ((fun magit-log-select-quit-function))
    (magit-mode-bury-buffer 'kill)
    (when fun (funcall fun))))

;;; Cherry Mode

(defvar-keymap magit-cherry-mode-map
  :doc "Keymap for `magit-cherry-mode'."
  :parent magit-mode-map
  "q" #'magit-log-bury-buffer
  "L" #'magit-margin-settings)

(define-derived-mode magit-cherry-mode magit-mode "Magit Cherry"
  "Mode for looking at commits not merged upstream.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-cherry-pick] to apply the commit at point.

\\{magit-cherry-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq magit--imenu-group-types 'cherries))

(defun magit-cherry-setup-buffer (head upstream)
  (magit-setup-buffer #'magit-cherry-mode nil
    (magit-buffer-refname head)
    (magit-buffer-upstream upstream)
    (magit-buffer-range (concat upstream ".." head))))

(defun magit-cherry-refresh-buffer ()
  (setq magit-section-inhibit-markers t)
  (setq magit-section-insert-in-reverse t)
  (magit-insert-section (cherry)
    (magit-run-section-hook 'magit-cherry-sections-hook)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-cherry-mode))
  magit-buffer-range)

;;;###autoload
(defun magit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (magit-read-branch "Cherry head")))
     (list head (magit-read-other-branch "Cherry upstream" head
                                         (magit-get-upstream-branch head)))))
  (require 'magit)
  (magit-cherry-setup-buffer head upstream))

(defun magit-insert-cherry-headers ()
  "Insert headers appropriate for `magit-cherry-mode' buffers."
  (let ((branch (propertize magit-buffer-refname
                            'font-lock-face 'magit-branch-local))
        (upstream (propertize magit-buffer-upstream 'font-lock-face
                              (if (magit-local-branch-p magit-buffer-upstream)
                                  'magit-branch-local
                                'magit-branch-remote))))
    (magit-insert-head-branch-header branch)
    (magit-insert-upstream-branch-header branch upstream "Upstream: ")
    (insert ?\n)))

(defun magit-insert-cherry-commits ()
  "Insert commit sections into a `magit-cherry-mode' buffer."
  (magit-insert-section (cherries)
    (magit-insert-heading "Cherry commits:")
    (magit-git-wash (apply-partially #'magit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev"
      magit-buffer-upstream
      magit-buffer-refname)))

;;; Log Sections
;;;; Standard Log Sections

(defvar-keymap magit-log-section-map
  :doc "Keymap for log sections.
The classes `magit-{unpulled,unpushed,unmerged}-section' derive
from the abstract `magit-log-section' class.  Accordingly this
keymap is the parent of their keymaps."
  "<remap> <magit-visit-thing>" #'magit-diff-dwim
  "<1>" (magit-menu-item "Visit diff" #'magit-diff-dwim))

(defvar-keymap magit-unpulled-section-map
  :doc "Keymap for `unpulled' sections."
  :parent magit-log-section-map)

(cl-defmethod magit-section-ident-value ((section magit-unpulled-section))
  "\"..@{push}\" cannot be used as the value because that is
ambiguous if `push.default' does not allow a 1:1 mapping, and
many commands would fail because of that.  But here that does
not matter and we need an unique value so we use that string
in the pushremote case."
  (let ((value (oref section value)))
    (if (equal value "..@{upstream}") value "..@{push}")))

(magit-define-section-jumper magit-jump-to-unpulled-from-upstream
  "Unpulled from @{upstream}" unpulled "..@{upstream}")

(defun magit-insert-unpulled-from-upstream ()
  "Insert commits that haven't been pulled from the upstream yet."
  (when-let ((upstream (magit-get-upstream-branch)))
    (magit-insert-section (unpulled "..@{upstream}" t)
      (magit-insert-heading
        (format (propertize "Unpulled from %s."
                            'font-lock-face 'magit-section-heading)
                upstream))
      (magit--insert-log nil "..@{upstream}" magit-buffer-log-args)
      (magit-log-insert-child-count))))

(magit-define-section-jumper magit-jump-to-unpulled-from-pushremote
  "Unpulled from <push-remote>" unpulled "..@{push}")

(defun magit-insert-unpulled-from-pushremote ()
  "Insert commits that haven't been pulled from the push-remote yet."
  (when-let* ((target (magit-get-push-branch))
              (range  (concat ".." target)))
    (when (magit--insert-pushremote-log-p)
      (magit-insert-section (unpulled range t)
        (magit-insert-heading
          (format (propertize "Unpulled from %s."
                              'font-lock-face 'magit-section-heading)
                  (propertize target 'font-lock-face 'magit-branch-remote)))
        (magit--insert-log nil range magit-buffer-log-args)
        (magit-log-insert-child-count)))))

(defvar-keymap magit-unpushed-section-map
  :doc "Keymap for `unpushed' sections."
  :parent magit-log-section-map)

(cl-defmethod magit-section-ident-value ((section magit-unpushed-section))
  "\"..@{push}\" cannot be used as the value because that is
ambiguous if `push.default' does not allow a 1:1 mapping, and
many commands would fail because of that.  But here that does
not matter and we need an unique value so we use that string
in the pushremote case."
  (let ((value (oref section value)))
    (if (equal value "@{upstream}..") value "@{push}..")))

(magit-define-section-jumper magit-jump-to-unpushed-to-upstream
  "Unpushed to @{upstream}" unpushed "@{upstream}..")

(defun magit-insert-unpushed-to-upstream-or-recent ()
  "Insert section showing unpushed or other recent commits.
If an upstream is configured for the current branch and it is
behind of the current branch, then show the commits that have
not yet been pushed into the upstream branch.  If no upstream is
configured or if the upstream is not behind of the current branch,
then show the last `magit-log-section-commit-count' commits."
  (let ((upstream (magit-get-upstream-branch)))
    (if (or (not upstream)
            (magit-rev-ancestor-p "HEAD" upstream))
        (magit-insert-recent-commits 'unpushed "@{upstream}..")
      (magit-insert-unpushed-to-upstream))))

(defun magit-insert-unpushed-to-upstream ()
  "Insert commits that haven't been pushed to the upstream yet."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpushed "@{upstream}..")
      (magit-insert-heading
        (format (propertize "Unmerged into %s."
                            'font-lock-face 'magit-section-heading)
                (magit-get-upstream-branch)))
      (magit--insert-log nil "@{upstream}.." magit-buffer-log-args)
      (magit-log-insert-child-count))))

(defun magit-insert-recent-commits (&optional type value)
  "Insert section showing recent commits.
Show the last `magit-log-section-commit-count' commits."
  (let* ((start (format "HEAD~%s" magit-log-section-commit-count))
         (range (and (magit-rev-verify start)
                     (concat start "..HEAD"))))
    (magit-insert-section ((eval (or type 'recent))
                           (or value range)
                           t)
      (magit-insert-heading "Recent commits")
      (magit--insert-log nil range
        (cons (format "-n%d" magit-log-section-commit-count)
              (--remove (string-prefix-p "-n" it)
                        magit-buffer-log-args))))))

(magit-define-section-jumper magit-jump-to-unpushed-to-pushremote
  "Unpushed to <push-remote>" unpushed "@{push}..")

(defun magit-insert-unpushed-to-pushremote ()
  "Insert commits that haven't been pushed to the push-remote yet."
  (when-let* ((target (magit-get-push-branch))
              (range  (concat target "..")))
    (when (magit--insert-pushremote-log-p)
      (magit-insert-section (unpushed range t)
        (magit-insert-heading
          (format (propertize "Unpushed to %s."
                              'font-lock-face 'magit-section-heading)
                  (propertize target 'font-lock-face 'magit-branch-remote)))
        (magit--insert-log nil range magit-buffer-log-args)
        (magit-log-insert-child-count)))))

(defun magit--insert-pushremote-log-p ()
  (magit--with-refresh-cache
      (cons default-directory 'magit--insert-pushremote-log-p)
    (not (and (equal (magit-get-push-branch)
                     (magit-get-upstream-branch))
              (or (memq 'magit-insert-unpulled-from-upstream
                        magit-status-sections-hook)
                  (memq 'magit-insert-unpulled-from-upstream-or-recent
                        magit-status-sections-hook))))))

(defun magit-log-insert-child-count ()
  (when magit-section-show-child-count
    (let ((count (length (oref magit-insert-section--current children))))
      (when (> count 0)
        (when (eq count (magit-log-get-commit-limit))
          (setq count (format "%s+" count)))
        (save-excursion
          (goto-char (- (oref magit-insert-section--current content) 2))
          (insert (format " (%s)" count))
          (delete-char 1))))))

;;;; Auxiliary Log Sections

(defun magit-insert-unpulled-cherries ()
  "Insert section showing unpulled commits.
Like `magit-insert-unpulled-from-upstream' but prefix each commit
which has not been applied yet (i.e., a commit with a patch-id
not shared with any local commit) with \"+\", and all others with
\"-\"."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpulled "..@{upstream}")
      (magit-insert-heading "Unpulled commits:")
      (magit-git-wash (apply-partially #'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg)
        (magit-get-current-branch) "@{upstream}"))))

(defun magit-insert-unpushed-cherries ()
  "Insert section showing unpushed commits.
Like `magit-insert-unpushed-to-upstream' but prefix each commit
which has not been applied to upstream yet (i.e., a commit with
a patch-id not shared with any upstream commit) with \"+\", and
all others with \"-\"."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpushed "@{upstream}..")
      (magit-insert-heading "Unpushed commits:")
      (magit-git-wash (apply-partially #'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) "@{upstream}"))))

;;; _
(provide 'magit-log)
;;; magit-log.el ends here
