;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2023 Bozhidar Batsov <bozhidar@batsov.dev>

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 2.8.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'compile)
(require 'grep)
(require 'lisp-mnt)
(eval-when-compile
  (require 'find-dired)
  (require 'subr-x))

;;; Declarations
;;
;; A bunch of variable and function declarations
;; needed to appease the byte-compiler.
(defvar ido-mode)
(defvar ivy-mode)
(defvar helm-mode)
(defvar ag-ignore-list)
(defvar ggtags-completion-table)
(defvar tags-completion-table)
(defvar tags-loop-scan)
(defvar tags-loop-operate)
(defvar eshell-buffer-name)
(defvar explicit-shell-file-name)
(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)

(declare-function tags-completion-table "etags")
(declare-function make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
(declare-function term-ansi-make-term "term")
(declare-function eshell-search-path "esh-ext")
(declare-function vc-dir "vc-dir")
(declare-function vc-dir-busy "vc-dir")
(declare-function string-trim "subr-x")
(declare-function fileloop-continue "fileloop")
(declare-function fileloop-initialize-replace "fileloop")
(declare-function tramp-archive-file-name-p "tramp-archive")
(declare-function helm-grep-get-file-extensions "helm-grep")

(declare-function ggtags-ensure-project "ext:ggtags")
(declare-function ggtags-update-tags "ext:ggtags")
(declare-function ripgrep-regexp "ext:ripgrep")
(declare-function rg-run "ext:rg")
(declare-function vterm "ext:vterm")
(declare-function vterm-other-window "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-send-string "ext:vterm")


;;; Customization
(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/projectile")
  :link '(url-link :tag "Online Manual" "https://docs.projectile.mx/")
  :link '(emacs-commentary-link :tag "Commentary" "projectile"))

(defcustom projectile-indexing-method
  (if (eq system-type 'windows-nt) 'native 'alien)
  "Specifies the indexing method used by Projectile.

There are three indexing methods - native, hybrid and alien.

The native method is implemented in Emacs Lisp (therefore it is
native to Emacs).  Its advantage is that it is portable and will
work everywhere that Emacs does.  Its disadvantage is that it is a
bit slow (especially for large projects).  Generally it's a good
idea to pair the native indexing method with caching.

The hybrid indexing method uses external tools (e.g. git, find,
etc) to speed up the indexing process.  Still, the files will be
post-processed by Projectile for sorting/filtering purposes.
In this sense that approach is a hybrid between native indexing
and alien indexing.

The alien indexing method optimizes to the limit the speed
of the hybrid indexing method.  This means that Projectile will
not do any processing of the files returned by the external
commands and you're going to get the maximum performance
possible.  This behaviour makes a lot of sense for most people,
as they'd typically be putting ignores in their VCS config and
won't care about any additional ignores/unignores/sorting that
Projectile might also provide.

The disadvantage of the hybrid and alien methods is that they are not well
supported on Windows systems.  That's why by default alien indexing is the
default on all operating systems, except Windows."
  :group 'projectile
  :type '(radio
          (const :tag "Native" native)
          (const :tag "Hybrid" hybrid)
          (const :tag "Alien" alien)))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Project caching is automatically enabled by default if you're
using the native indexing method."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-kill-buffers-filter 'kill-all
  "Determine which buffers are killed by `projectile-kill-buffers'.

When the kill-all option is selected, kills each buffer.

When the kill-only-files option is selected, kill only the buffer
associated to a file.

Otherwise, it should be a predicate that takes one argument: the buffer to
be killed."
  :group 'projectile
  :type '(radio
          (const :tag "All project buffers" kill-all)
          (const :tag "Project file buffers" kill-only-files)
          (function :tag "Predicate")))

(defcustom projectile-file-exists-local-cache-expire nil
  "Number of seconds before the local file existence cache expires.
Local refers to a file on a local file system.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-file-exists-remote-cache-expire (* 5 60)
  "Number of seconds before the remote file existence cache expires.
Remote refers to a file on a remote file system such as tramp.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-files-cache-expire nil
  "Number of seconds before project files list cache expires.

A value of nil means the cache never expires."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-auto-discover t
  "Whether to discover projects when `projectile-mode' is activated."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.3.0"))

(defcustom projectile-auto-update-cache t
  "Whether cache is automatically updated when files are opened or deleted."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-require-project-root 'prompt
  "Require the presence of a project root to operate when true.
When set to `prompt' Projectile will ask you to select a project
directory if you're not in a project.

When nil Projectile will consider the current directory the project root."
  :group 'projectile
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Prompt for project" prompt)))

(defcustom projectile-completion-system 'auto
  "The completion system to be used by Projectile."
  :group 'projectile
  :type '(radio
          (const :tag "Auto-detect" auto)
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom projectile-keymap-prefix nil
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(make-obsolete-variable 'projectile-keymap-prefix "Use (define-key projectile-mode-map (kbd ...) 'projectile-command-map) instead." "2.0.0")

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-file-name "TAGS"
  "The tags filename Projectile's going to use."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re -f \"%s\" %s \"%s\""
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-backend 'auto
  "The tag backend that Projectile should use.

If set to `auto', `projectile-find-tag' will automatically choose
which backend to use.  Preference order is ggtags -> xref
-> etags-select -> `find-tag'.  Variable can also be set to specify which
backend to use.  If selected backend is unavailable, fall back to
`find-tag'.

If this variable is set to `auto' and ggtags is available, or if
set to `ggtags', then ggtags will be used for
`projectile-regenerate-tags'.  For all other settings
`projectile-tags-command' will be used."
  :group 'projectile
  :type '(radio
          (const :tag "auto" auto)
          (const :tag "xref" xref)
          (const :tag "ggtags" ggtags)
          (const :tag "etags" etags-select)
          (const :tag "standard" find-tag))
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files.

Note that files aren't sorted if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(radio
          (const :tag "Default (no sorting)" default)
          (const :tag "Recently opened files" recentf)
          (const :tag "Recently active buffers, then recently opened files" recently-active)
          (const :tag "Access time (atime)" access-time)
          (const :tag "Modification time (mtime)" modification-time)))

(defcustom projectile-verbose t
  "Echo messages that are not errors."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-buffers-filter-function nil
  "A function used to filter the buffers in `projectile-project-buffers'.

The function should accept and return a list of Emacs buffers.
Two example filter functions are shipped by default -
`projectile-buffers-with-file' and
`projectile-buffers-with-file-or-process'."
  :group 'projectile
  :type 'function)

(defcustom projectile-project-name nil
  "If this value is non-nil, it will be used as project name.

It has precedence over function `projectile-project-name-function'."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-name-function 'projectile-default-project-name
  "A function that receives the project-root and returns the project name.

If variable `projectile-project-name' is non-nil, this function will not be
used."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-root-files
  '(
    "GTAGS"              ; GNU Global tags
    "TAGS"               ; etags/ctags are usually in the root of project
    "configure.ac"       ; autoconf new style
    "configure.in"       ; autoconf old style
    "cscope.out"         ; cscope
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence.
See `projectile-register-project-type'."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-bottom-up
  '(".git"        ; Git VCS root dir
    ".hg"         ; Mercurial VCS root dir
    ".fslckout"   ; Fossil VCS root dir
    "_FOSSIL_"    ; Fossil VCS root DB on Windows
    ".bzr"        ; Bazaar VCS root dir
    "_darcs"      ; Darcs VCS root dir
    ".pijul"      ; Pijul VCS root dir
    )
  "A list of files considered to mark the root of a project.
The bottommost (parentmost) match has precedence."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; Csv VCS root dir
    "Makefile")
  "A list of files considered to mark the root of a project.
The search starts at the top and descends down till a directory
that contains a match file but its parent does not.  Thus, it's a
bottommost match in the topmost sequence of directories
containing a root file."
  :group 'projectile
  :type '(repeat string))

(define-obsolete-variable-alias 'projectile-project-root-files-functions 'projectile-project-root-functions "2.4")

(defcustom projectile-project-root-functions
  '(projectile-root-local
    projectile-root-marked
    projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring)
  "A list of functions for finding project root folders.
The functions will be run until one of them returns a project folder.
Reordering the default functions will alter the project discovery
algorithm."
  :group 'projectile
  :type '(repeat function))

(defcustom projectile-dirconfig-file
  ".projectile"
  "The file which serves both as a project marker and configuration file.
This should _not_ be set via .dir-locals.el."
  :group 'projectile
  :type 'file
  :package-version '(projectile . "2.7.0"))

(defcustom projectile-dirconfig-comment-prefix
  nil
  "`projectile-dirconfig-file` comment start marker.
If specified, starting a line in a project's .projectile file with this
character marks that line as a comment instead of a pattern.
Similar to '#' in .gitignore files."
  :group 'projectile
  :type 'character
  :package-version '(projectile . "2.2.0"))

(defcustom projectile-globally-ignored-files
  (list projectile-tags-file-name)
  "A list of files globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-files nil
  "A list of files globally unignored by projectile.
Regular expressions can be used.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-file-suffixes
  nil
  "A list of file suffixes globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-directories
  '("^\\.idea$"
    "^\\.vscode$"
    "^\\.ensime_cache$"
    "^\\.eunit$"
    "^\\.git$"
    "^\\.hg$"
    "^\\.fslckout$"
    "^_FOSSIL_$"
    "^\\.bzr$"
    "^_darcs$"
    "^\\.pijul$"
    "^\\.tox$"
    "^\\.svn$"
    "^\\.stack-work$"
    "^\\.ccls-cache$"
    "^\\.cache$"
    "^\\.clangd$")
  "A list of directories globally ignored by projectile.
Regular expressions can be used.

Strings that don't start with * are only ignored at the top level
of the project.  Strings that start with * are ignored everywhere
in the project, as if there was no *.  So note that * when used as
a prefix is not a wildcard; it is an indicator that the directory
should be ignored at all levels, not just root.

Examples: \"tmp\" ignores only ./tmp at the top level of the
project, but not ./src/tmp.  \"*tmp\" will ignore both ./tmp and
./src/tmp, but not ./not-a-tmp or ./src/not-a-tmp.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-directories nil
  "A list of directories globally unignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-modes
  '("erc-mode"
    "help-mode"
    "completion-list-mode"
    "Buffer-menu-mode"
    "gnus-.*-mode"
    "occur-mode")
  "A list of regular expressions for major modes ignored by projectile.

If a buffer is using a given major mode, projectile will ignore
it for functions working with buffers."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-buffers
  '("*scratch*"
    "*lsp-log*")
  "A list of buffer-names ignored by projectile.

You can use either exact buffer names or regular expressions.
If a buffer is in the list projectile will ignore it for
functions working with buffers."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-find-file-hook nil
  "Hooks run when a file is opened with `projectile-find-file'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-find-dir-hook nil
  "Hooks run when a directory is opened with `projectile-find-dir'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-switch-project-action 'projectile-find-file
  "Action invoked after switching projects with `projectile-switch-project'.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function)

(defcustom projectile-find-dir-includes-top-level nil
  "If true, add top-level dir to options offered by `projectile-find-dir'."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-use-git-grep nil
  "If true, use `vc-git-grep' in git projects."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-grep-finished-hook nil
  "Hooks run when `projectile-grep' finishes."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-test-prefix-function 'projectile-test-prefix
  "Function to find test files prefix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-test-suffix-function 'projectile-test-suffix
  "Function to find test files suffix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-related-files-fn-function 'projectile-related-files-fn
  "Function to find related files based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-dynamic-mode-line t
  "If true, update the mode-line dynamically.
Only file buffers are affected by this, as the update happens via
`find-file-hook'.

See also `projectile-mode-line-function' and `projectile-update-mode-line'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-mode-line-function 'projectile-default-mode-line
  "The function to use to generate project-specific mode-line.
The default function adds the project name and type to the mode-line.
See also `projectile-update-mode-line'."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-default-src-directory "src/"
  "The default value of a project's src-dir property.

It's used as a fallback in the case the property is not set for a project
type when `projectile-toggle-between-implementation-and-test' is used."
  :group 'projectile
  :type 'string)

(defcustom projectile-default-test-directory "test/"
  "The default value of a project's test-dir property.

It's used as a fallback in the case the property is not set for a project
type when `projectile-toggle-between-implementation-and-test' is used."
  :group 'projectile
  :type 'string)


;;; Idle Timer
(defvar projectile-idle-timer nil
  "The timer object created when `projectile-enable-idle-timer' is non-nil.")

(defcustom projectile-idle-timer-seconds 30
  "The idle period to use when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type 'number)

(defcustom projectile-idle-timer-hook '(projectile-regenerate-tags)
  "The hook run when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type '(repeat symbol))

(defcustom projectile-enable-idle-timer nil
  "Enables idle timer hook `projectile-idle-timer-functions'.

When `projectile-enable-idle-timer' is non-nil, the hook
`projectile-idle-timer-hook' is run each time Emacs has been idle
for `projectile-idle-timer-seconds' seconds and we're in a
project."
  :group 'projectile
  :set (lambda (symbol value)
         (set symbol value)
         (when projectile-idle-timer
           (cancel-timer projectile-idle-timer))
         (setq projectile-idle-timer nil)
         (when projectile-enable-idle-timer
           (setq projectile-idle-timer (run-with-idle-timer
                                        projectile-idle-timer-seconds t
                                        (lambda ()
                                          (when (projectile-project-p)
                                            (run-hooks 'projectile-idle-timer-hook)))))))
  :type 'boolean)

(defvar projectile-projects-cache nil
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-projects-cache-time nil
  "A hashmap used to record when we populated `projectile-projects-cache'.")

(defvar projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

(defvar projectile-project-type-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project type to speed up related operations.")

(defvar projectile-known-projects nil
  "List of locations where we have previously seen projects.
The list of projects is ordered by the time they have been accessed.

See also `projectile-remove-known-project',
`projectile-cleanup-known-projects' and `projectile-clear-known-projects'.")

(defvar projectile-known-projects-on-file nil
  "List of known projects reference point.

Contains a copy of `projectile-known-projects' when it was last
synchronized with `projectile-known-projects-file'.")

(defcustom projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld"
                    user-emacs-directory)
  "Name and location of the Projectile's known projects file."
  :group 'projectile
  :type 'string)

(defcustom projectile-ignored-projects nil
  "A list of projects not to be added to `projectile-known-projects'."
  :group 'projectile
  :type '(repeat :tag "Project list" directory)
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-ignored-project-function nil
  "Function to decide if a project is added to `projectile-known-projects'.

Can be either nil, or a function that takes the truename of the
project root as argument and returns non-nil if the project is to
be ignored or nil otherwise.

This function is only called if the project is not listed in
the variable `projectile-ignored-projects'.

A suitable candidate would be `file-remote-p' to ignore remote
projects."
  :group 'projectile
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Remote files" file-remote-p)
          function)
  :package-version '(projectile . "0.13.0"))

(defcustom projectile-track-known-projects-automatically t
  "Controls whether Projectile will automatically register known projects.

When set to nil you'll have always add projects explicitly with
`projectile-add-known-project'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-project-search-path nil
  "List of folders where projectile is automatically going to look for projects.
You can think of something like $PATH, but for projects instead of executables.
Examples of such paths might be ~/projects, ~/work, (~/github . 1) etc.

For elements of form (DIRECTORY . DEPTH), DIRECTORY has to be a
directory and DEPTH an integer that specifies the depth at which to
look for projects.  A DEPTH of 0 means check DIRECTORY.  A depth of 1
means check all the subdirectories of DIRECTORY.  Etc."
  :group 'projectile
  :type '(repeat (choice directory (cons directory (integer :tag "Depth"))))
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-fd-executable
  (cond
   ((executable-find "fdfind") "fdfind")
   ((executable-find "fd") "fd"))
  "Path or name of fd executable used by Projectile if enabled.
Nil means fd is not installed or should not be used."
  :type 'string
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-use-fd (when projectile-fd-executable t)
  "Non-nil means use fd to implement git ls-files.
This may change Projectile's performance in large Git repositories
depending on your system, but it will also work around the Git behavior
that causes deleted files to still be shown in Projectile listings until
their deletions are staged."
  :type 'boolean
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
  "Arguments to fd used to re-implement `git ls-files'.
This is used with `projectile-fd-executable' when `projectile-git-use-fd'
is non-nil."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-submodule-command "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'"
  "Command used by projectile to list submodules of a given git repository.
Set to nil to disable listing submodules contents."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-ignored-command "git ls-files -zcoi --exclude-standard"
  "Command used by projectile to get the ignored files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-hg-command "hg locate -f -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-fossil-command (concat "fossil ls | "
                                             (when (string-equal system-type
                                                                 "windows-nt")
                                               "dos2unix | ")
                                             "tr '\\n' '\\0'")
  "Command used by projectile to get the files in a fossil project."
  :group 'projectile
  :type 'string)

(defcustom projectile-bzr-command "bzr ls -R --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0 . "
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string)

(defcustom projectile-pijul-command "pijul list | tr '\\n' '\\0'"
   "Command used by projectile to get the files in a pijul project."
   :group 'projectile
   :type 'string)

(defcustom projectile-svn-command "svn list -R . | grep -v '$/' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in a svn project."
  :group 'projectile
  :type 'string)

(defcustom projectile-generic-command
  (cond
   ;; we prefer fd over find
   ;; note that --strip-cwd-prefix is only available in version 8.3.0+
   (projectile-fd-executable
    (format "%s . -0 --type f --color=never --strip-cwd-prefix" projectile-fd-executable))
   ;; with find we have to be careful to strip the ./ from the paths
   ;; see https://stackoverflow.com/questions/2596462/how-to-strip-leading-in-unix-find
   (t "find . -type f | cut -c3- | tr '\\n' '\\0'"))
  "Command used by projectile to get the files in a generic project."
  :group 'projectile
  :type 'string)

(defcustom projectile-vcs-dirty-state '("edited" "unregistered" "needs-update" "needs-merge" "unlocked-changes" "conflict")
  "List of states checked by `projectile-browse-dirty-projects'.
Possible checked states are:
\"edited\", \"unregistered\", \"needs-update\", \"needs-merge\",
\"unlocked-changes\" and \"conflict\",
as defined in `vc.el'."
  :group 'projectile
  :type '(repeat (string))
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-other-file-alist
  '( ;; handle C/C++ extensions
    ("cpp" . ("h" "hpp" "ipp"))
    ("ipp" . ("h" "hpp" "cpp"))
    ("hpp" . ("h" "ipp" "cpp" "cc"))
    ("cxx" . ("h" "hxx" "ixx"))
    ("ixx" . ("h" "hxx" "cxx"))
    ("hxx" . ("h" "ixx" "cxx"))
    ("c"   . ("h"))
    ("m"   . ("h"))
    ("mm"  . ("h"))
    ("h"   . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
    ("cc"  . ("h" "hh" "hpp"))
    ("hh"  . ("cc"))

    ;; OCaml extensions
    ("ml" . ("mli"))
    ("mli" . ("ml" "mll" "mly"))
    ("mll" . ("mli"))
    ("mly" . ("mli"))
    ("eliomi" . ("eliom"))
    ("eliom" . ("eliomi"))

    ;; vertex shader and fragment shader extensions in glsl
    ("vert" . ("frag"))
    ("frag" . ("vert"))

    ;; handle files with no extension
    (nil    . ("lock" "gpg"))
    ("lock" . (""))
    ("gpg"  . (""))
    )
  "Alist of extensions for switching to file with the same name,
  using other extensions based on the extension of current
  file."
  :type 'alist)

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system.  Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-per-project-compilation-buffer nil
  "When non-nil, the compilation command makes the per-project compilation buffer."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.6.0"))

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-before-switch-project-hook nil
  "Hooks run when right before project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-current-project-on-switch 'remove
  "Determines whether to display current project when switching projects.

When set to `remove' current project is not included, `move-to-end'
will display current project and the end of the list of known
projects, `keep' will leave the current project at the default
position."
  :group 'projectile
  :type '(radio
          (const :tag "Remove" remove)
          (const :tag "Move to end" move-to-end)
          (const :tag "Keep" keep)))

(defcustom projectile-max-file-buffer-count nil
  "Maximum number of file buffers per project that are kept open.

If the value is nil, there is no limit to the opend buffers count."
  :group 'projectile
  :type 'integer
  :package-version '(projectile . "2.2.0"))

(defvar projectile-project-test-suffix nil
  "Use this variable to override the current project's test-suffix property.
It takes precedence over the test-suffix for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-test-prefix nil
  "Use this variable to override the current project's test-prefix property.
It takes precedence over the test-prefix for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-related-files-fn nil
  "Use this variable to override the current project's related-files-fn property.
It takes precedence over the related-files-fn attribute for the project type
when set.  Should be set via .dir-locals.el.")

(defvar projectile-project-src-dir nil
  "Use this variable to override the current project's src-dir property.
It takes precedence over the src-dir for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-test-dir nil
  "Use this variable to override the current project's test-dir property.
It takes precedence over the test-dir for the project type when set.
Should be set via .dir-locals.el.")


;;; Version information

(defconst projectile-version "2.8.0"
  "The current version of Projectile.")

(defun projectile--pkg-version ()
  "Extract Projectile's package version from its package metadata."
  ;; Use `cond' below to avoid a compiler unused return value warning
  ;; when `package-get-version' returns nil. See #3181.
  ;; FIXME: Inline the logic from package-get-version and adapt it
  (cond ((fboundp 'package-get-version)
         (package-get-version))))

;;;###autoload
(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (or (projectile--pkg-version) projectile-version)))
   (if show-version
       (message "Projectile %s" version)
     version)))

;;; Misc utility functions
(defun projectile-difference (list1 list2)
  (cl-remove-if
   (lambda (x) (member x list2))
   list1))

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (cl-every
   (lambda (x) (executable-find x))
   '("grep" "cut" "uniq")))

(defun projectile-symbol-or-selection-at-point ()
  "Get the symbol or selected text at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (projectile-symbol-at-point)))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))

(defun projectile-generate-process-name (process make-new &optional project)
  "Infer the buffer name for PROCESS or generate a new one if MAKE-NEW is true.
The function operates on the current project by default, but you can also
specify a project explicitly via the optional PROJECT param."
  (let* ((project (or project (projectile-acquire-root)))
         (base-name (format "*%s %s*" process (projectile-project-name project))))
    (if make-new
        (generate-new-buffer-name base-name)
      base-name)))


;;; Serialization
(defun projectile-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projectile-unserialize'."
  (if (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))
    (message "Projectile cache '%s' not writeable" filename)))

(defun projectile-unserialize (filename)
  "Read data serialized by `projectile-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))


;;; Caching
(defvar projectile-file-exists-cache
  (make-hash-table :test 'equal)
  "Cached `projectile-file-exists-p' results.")

(defvar projectile-file-exists-cache-timer nil
  "Timer for scheduling`projectile-file-exists-cache-cleanup'.")

(defun projectile-file-exists-cache-cleanup ()
  "Removed timed out cache entries and reschedules or remove the
timer if no more items are in the cache."
  (let ((now (current-time)))
    (maphash (lambda (key value)
               (if (time-less-p (cdr value) now)
                   (remhash key projectile-file-exists-cache)))
             projectile-file-exists-cache)
    (setq projectile-file-exists-cache-timer
          (if (> (hash-table-count projectile-file-exists-cache) 0)
              (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))))

(defun projectile-file-exists-p (filename)
  "Return t if file FILENAME exist.
A wrapper around `file-exists-p' with additional caching support."
  (let* ((file-remote (file-remote-p filename))
         (expire-seconds
          (if file-remote
              (and projectile-file-exists-remote-cache-expire
                   (> projectile-file-exists-remote-cache-expire 0)
                   projectile-file-exists-remote-cache-expire)
            (and projectile-file-exists-local-cache-expire
                 (> projectile-file-exists-local-cache-expire 0)
                 projectile-file-exists-local-cache-expire)))
         (remote-file-name-inhibit-cache (if expire-seconds
                                             expire-seconds
                                           remote-file-name-inhibit-cache)))
    (if (not expire-seconds)
        (file-exists-p filename)
      (let* ((current-time (current-time))
             (cached (gethash filename projectile-file-exists-cache))
             (cached-value (if cached (car cached)))
             (cached-expire (if cached (cdr cached)))
             (cached-expired (if cached (time-less-p cached-expire current-time) t))
             (value (or (and (not cached-expired) cached-value)
                        (if (file-exists-p filename) 'found 'notfound))))
        (when (or (not cached) cached-expired)
          (puthash filename
                   (cons value (time-add current-time (seconds-to-time expire-seconds)))
                   projectile-file-exists-cache))
        (unless projectile-file-exists-cache-timer
          (setq projectile-file-exists-cache-timer
                (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))
        (equal value 'found)))))

;;;###autoload
(defun projectile-invalidate-cache (prompt)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if prompt
             (completing-read "Remove cache for: "
                              (hash-table-keys projectile-projects-cache))
           (projectile-acquire-root))))
    (setq projectile-project-root-cache (make-hash-table :test 'equal))
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-projects-cache)
    (remhash project-root projectile-projects-cache-time)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

(defun projectile-time-seconds ()
  "Return the number of seconds since the unix epoch."
  (if (fboundp 'time-convert)
      (time-convert nil 'integer)
    (cl-destructuring-bind (high low _usec _psec) (current-time)
      (+ (ash high 16) low))))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (puthash project (projectile-time-seconds) projectile-projects-cache-time)
    (projectile-serialize-cache)))

;;;###autoload
(defun projectile-purge-file-from-cache (file)
  "Purge FILE from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove file from cache: "
          (projectile-current-project-files))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (if (projectile-file-cached-p file project-root)
        (progn
          (puthash project-root (remove file project-cache) projectile-projects-cache)
          (projectile-serialize-cache)
          (when projectile-verbose
            (message "%s removed from cache" file)))
      (error "%s is not in the cache" file))))

;;;###autoload
(defun projectile-purge-dir-from-cache (dir)
  "Purge DIR from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (puthash project-root
             (cl-remove-if (lambda (str) (string-prefix-p dir str)) project-cache)
             projectile-projects-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

;;;###autoload
(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let ((current-project (projectile-project-root)))
    (when (and (buffer-file-name) (gethash (projectile-project-root) projectile-projects-cache))
      (let* ((abs-current-file (file-truename (buffer-file-name)))
             (current-file (file-relative-name abs-current-file current-project)))
        (unless (or (projectile-file-cached-p current-file current-project)
                    (projectile-ignored-directory-p (file-name-directory abs-current-file))
                    (projectile-ignored-file-p abs-current-file))
          (puthash current-project
                   (cons current-file (gethash current-project projectile-projects-cache))
                   projectile-projects-cache)
          (projectile-serialize-cache)
          (message "File %s added to project %s cache."
                   (propertize current-file 'face 'font-lock-keyword-face)
                   (propertize current-project 'face 'font-lock-keyword-face)))))))

;; cache opened files automatically to reduce the need for cache invalidation
(defun projectile-cache-files-find-file-hook ()
  "Function for caching files with `find-file-hook'."
  (let ((project-root (projectile-project-p)))
    (when (and projectile-enable-caching
               project-root
               (not (projectile-ignored-project-p project-root)))
      (projectile-cache-current-file))))

(defun projectile-track-known-projects-find-file-hook ()
  "Function for caching projects with `find-file-hook'."
  (when (and projectile-track-known-projects-automatically (projectile-project-p))
    (projectile-add-known-project (projectile-project-root))))

(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          projectile-cache-file))
    (projectile-invalidate-cache nil)))

;;;###autoload
(defun projectile-discover-projects-in-directory (directory &optional depth)
  "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there."
  (interactive
   (list (read-directory-name "Starting directory: ")))

  (if (file-directory-p directory)
      (if (and (numberp depth) (> depth 0))
          ;; Ignore errors when listing files in the directory, because
          ;; sometimes that directory is an unreadable one at the root of a
          ;; volume. This is the case, for example, on macOS with the
          ;; .Spotlight-V100 directory.
          (let ((progress-reporter
                 (make-progress-reporter
                  (format "Projectile is discovering projects in %s..."
                          (propertize directory 'face 'font-lock-keyword-face)))))
            (progress-reporter-update progress-reporter)
            (dolist (dir (ignore-errors (directory-files directory t)))
              (when (and (file-directory-p dir)
                         (not (member (file-name-nondirectory dir) '(".." "."))))
                (projectile-discover-projects-in-directory dir (1- depth))))
            (progress-reporter-done progress-reporter))
        (when (projectile-project-p directory)
          (let ((dir (abbreviate-file-name (projectile-project-root directory))))
            (unless (member dir projectile-known-projects)
              (projectile-add-known-project dir)))))
    (message "Project search path directory %s doesn't exist" directory)))

;;;###autoload
(defun projectile-discover-projects-in-search-path ()
  "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled."
  (interactive)
  (dolist (path projectile-project-search-path)
    (if (consp path)
        (projectile-discover-projects-in-directory (car path) (cdr path))
      (projectile-discover-projects-in-directory path 1))))


(defun delete-file-projectile-remove-from-cache (filename &optional _trash)
  (if (and projectile-enable-caching projectile-auto-update-cache (projectile-project-p))
      (let* ((project-root (projectile-project-root))
             (true-filename (file-truename filename))
             (relative-filename (file-relative-name true-filename project-root)))
        (if (projectile-file-cached-p relative-filename project-root)
            (projectile-purge-file-from-cache relative-filename)))))


;;; Project root related utilities
(defun projectile-parent (path)
  "Return the parent directory of PATH.
PATH may be a file or directory and directory paths may end with a slash."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

(defun projectile-locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  ;; copied from files.el (stripped comments) emacs-24 bzr branch 2014-03-28 10:20
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (projectile-file-exists-p (projectile-expand-file-name-wildcard name file))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defvar-local projectile-project-root nil
  "Defines a custom Projectile project root.
This is intended to be used as a file local variable.")

(defun projectile-root-local (_dir)
  "A simple wrapper around the variable `projectile-project-root'."
  projectile-project-root)

(defun projectile-root-top-down (dir &optional list)
  "Identify a project root in DIR by top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files' instead.
Return the first (topmost) matched directory or nil if not found."
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (cl-find-if (lambda (f) (projectile-file-exists-p (projectile-expand-file-name-wildcard f dir)))
                 (or list projectile-project-root-files)))))

(defun projectile-root-marked (dir)
  "Identify a project root in DIR by search for `projectile-dirconfig-file`."
  (projectile-root-bottom-up dir (list projectile-dirconfig-file)))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (projectile-locate-dominating-file
   dir
   (lambda (directory)
     (let ((files (mapcar (lambda (file) (expand-file-name file directory))
                          (or list projectile-project-root-files-bottom-up))))
       (cl-some (lambda (file) (and file (file-exists-p file))) files)))))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files-top-down-recurring'
instead.  Return the last (bottommost) matched directory in the
topmost sequence of matched directories.  Nil otherwise."
  (cl-some
   (lambda (f)
     (projectile-locate-dominating-file
      dir
      (lambda (dir)
        (and (projectile-file-exists-p (projectile-expand-file-name-wildcard f dir))
             (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                 (not (projectile-file-exists-p (projectile-expand-file-name-wildcard f (projectile-parent dir)))))))))
   (or list projectile-project-root-files-top-down-recurring)))

(defun projectile-project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  (let ((dir (or dir default-directory)))
    ;; Back out of any archives, the project will live on the outside and
    ;; searching them is slow.
    (when (and (fboundp 'tramp-archive-file-name-archive)
               (tramp-archive-file-name-p dir))
      (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
    ;; the cached value will be 'none in the case of no project root (this is to
    ;; ensure it is not reevaluated each time when not inside a project) so use
    ;; cl-subst to replace this 'none value with nil so a nil value is used
    ;; instead
    (cl-subst nil 'none
      (or
       ;; if we've already failed to find a project dir for this
       ;; dir, and cached that failure, don't recompute
       (let* ((cache-key (format "projectilerootless-%s" dir))
              (cache-value (gethash cache-key projectile-project-root-cache)))
         cache-value)
       ;; if the file isn't local, and we're not connected, don't try to
       ;; find a root now now, but don't cache failure, as we might
       ;; re-connect.  The `is-local' and `is-connected' variables are
       ;; used to fix the behavior where Emacs hangs because of
       ;; Projectile when you open a file over TRAMP. It basically
       ;; prevents Projectile from trying to find information about
       ;; files for which it's not possible to get that information
       ;; right now.
       (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
             (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
         (unless (or is-local is-connected)
           'none))
       ;; if the file is local or we're connected to it via TRAMP, run
       ;; through the project root functions until we find a project dir
       (cl-some
        (lambda (func)
          (let* ((cache-key (format "%s-%s" func dir))
                 (cache-value (gethash cache-key projectile-project-root-cache)))
            (if (and cache-value (file-exists-p cache-value))
                cache-value
              (let ((value (funcall func (file-truename dir))))
                (puthash cache-key value projectile-project-root-cache)
                value))))
        projectile-project-root-functions)
       ;; if we get here, we have failed to find a root by all
       ;; conventional means, and we assume the failure isn't transient
       ;; / network related, so cache the failure
       (let ((cache-key (format "projectilerootless-%s" dir)))
         (puthash cache-key 'none projectile-project-root-cache)
         'none)))))

(defun projectile-ensure-project (dir)
  "Ensure that DIR is non-nil.
Useful for commands that expect the presence of a project.
Controlled by `projectile-require-project-root'.

See also `projectile-acquire-root'."
  (if dir
      dir
    (cond
     ((eq projectile-require-project-root 'prompt) (projectile-completing-read
                                                    "Switch to project: " projectile-known-projects))
     (projectile-require-project-root (error "Projectile cannot find a project definition in %s" default-directory))
     (t default-directory))))

(defun projectile-acquire-root (&optional dir)
  "Find the current project root, and prompts the user for it if that fails.
Provides the common idiom (projectile-ensure-project (projectile-project-root)).
Starts the search for the project with DIR."
  (projectile-ensure-project (projectile-project-root dir)))

(defun projectile-project-p (&optional dir)
  "Check if DIR is a project.
Defaults to the current directory if not provided
explicitly."
  (projectile-project-root (or dir default-directory)))

(defun projectile-default-project-name (project-root)
  "Default function used to create the project name.
The project name is based on the value of PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun projectile-project-name (&optional project)
  "Return project name.
If PROJECT is not specified acts on the current project."
  (or projectile-project-name
      (let ((project-root (or project (projectile-project-root))))
        (if project-root
            (funcall projectile-project-name-function project-root)
          "-"))))


;;; Project indexing
(defun projectile-get-project-directories (project-dir)
  "Get the list of PROJECT-DIR directories that are of interest to the user."
  (mapcar (lambda (subdir) (concat project-dir subdir))
          (or (nth 0 (projectile-parse-dirconfig-file)) '(""))))

(defun projectile--directory-p (directory)
  "Checks if DIRECTORY is a string designating a valid directory."
  (and (stringp directory) (file-directory-p directory)))

(defun projectile-dir-files (directory)
  "List the files in DIRECTORY and in its sub-directories.
Files are returned as relative paths to DIRECTORY."
  (unless (projectile--directory-p directory)
    (error "Directory %S does not exist" directory))
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache))))
    ;; cache disabled or cache miss
    (or files-list
        (let ((vcs (projectile-project-vcs directory)))
          (pcase projectile-indexing-method
            ('native (projectile-dir-files-native directory))
            ;; use external tools to get the project files
            ('hybrid (projectile-adjust-files directory vcs (projectile-dir-files-alien directory)))
            ('alien (projectile-dir-files-alien directory))
            (_ (user-error "Unsupported indexing method `%S'" projectile-indexing-method)))))))

;;; Native Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to native.
(defun projectile-dir-files-native (directory)
  "Get the files for ROOT under DIRECTORY using just Emacs Lisp."
  (let ((progress-reporter
         (make-progress-reporter
          (format "Projectile is indexing %s"
                  (propertize directory 'face 'font-lock-keyword-face)))))
    ;; we need the files with paths relative to the project root
    (mapcar (lambda (file) (file-relative-name file directory))
            (projectile-index-directory directory (projectile-filtering-patterns)
                                        progress-reporter))))

(defun projectile-index-directory (directory patterns progress-reporter &optional ignored-files ignored-directories globally-ignored-directories)
  "Index DIRECTORY taking into account PATTERNS.

The function calls itself recursively until all sub-directories
have been indexed.  The PROGRESS-REPORTER is updated while the
function is executing.  The list of IGNORED-FILES and
IGNORED-DIRECTORIES may optionally be provided."
  ;; we compute the ignored files and directories only once and then we reuse the
  ;; pre-computed values in the subsequent recursive invocations of the function
  (let ((ignored-files (or ignored-files (projectile-ignored-files)))
        (ignored-directories (or ignored-directories (projectile-ignored-directories)))
        (globally-ignored-directories (or globally-ignored-directories (projectile-globally-ignored-directory-names))))
    (apply #'append
           (mapcar
            (lambda (f)
              (let ((local-f (file-name-nondirectory (directory-file-name f))))
                (unless (or (and patterns (projectile-ignored-rel-p f directory patterns))
                            (member local-f '("." "..")))
                  (progress-reporter-update progress-reporter)
                  (if (file-directory-p f)
                      (unless (projectile-ignored-directory-p
                               (file-name-as-directory f)
                               ignored-directories
                               local-f
                               globally-ignored-directories)
                        (projectile-index-directory f patterns progress-reporter ignored-files ignored-directories globally-ignored-directories))
                    (unless (projectile-ignored-file-p f ignored-files)
                      (list f))))))
            (directory-files directory t)))))

;;; Alien Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to hybrid or alien.
;; The only difference between the two methods is that alien doesn't do
;; any post-processing of the files obtained via the external command.
(defun projectile-dir-files-alien (directory)
  "Get the files for DIRECTORY using external tools."
  (let ((vcs (projectile-project-vcs directory)))
    (cond
     ((eq vcs 'git)
      (nconc (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))
             (projectile-get-sub-projects-files directory vcs)))
     (t (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))))))

(define-obsolete-function-alias 'projectile-dir-files-external 'projectile-dir-files-alien "2.0.0")
(define-obsolete-function-alias 'projectile-get-repo-files 'projectile-dir-files-alien "2.0.0")

(defun projectile-get-ext-command (vcs)
  "Determine which external command to invoke based on the project's VCS.
Fallback to a generic command when not in a VCS-controlled project."
  (pcase vcs
    ('git (if (and projectile-git-use-fd projectile-fd-executable)
              (concat
               projectile-fd-executable
               " "
               projectile-git-fd-args)
            projectile-git-command))
    ('hg projectile-hg-command)
    ('fossil projectile-fossil-command)
    ('bzr projectile-bzr-command)
    ('darcs projectile-darcs-command)
    ('pijul projectile-pijul-command)
    ('svn projectile-svn-command)
    (_ projectile-generic-command)))

(defun projectile-get-sub-projects-command (vcs)
  "Get the sub-projects command for VCS.
Currently that's supported just for Git (sub-projects being Git
sub-modules there)."
  (pcase vcs
    ('git projectile-git-submodule-command)
    (_ "")))

(defun projectile-get-ext-ignored-command (vcs)
  "Determine which external command to invoke based on the project's VCS."
  (pcase vcs
    ('git projectile-git-ignored-command)
    ;; TODO: Add support for other VCS
    (_ nil)))

(defun projectile-flatten (lst)
  "Take a nested list LST and return its contents as a single, flat list."
  (if (and (listp lst) (listp (cdr lst)))
      (cl-mapcan 'projectile-flatten lst)
    (list lst)))

(defun projectile-get-all-sub-projects (project)
  "Get all sub-projects for a given project.

PROJECT is base directory to start search recursively."
  (let ((submodules (projectile-get-immediate-sub-projects project)))
    (cond
     ((null submodules)
      nil)
     (t
      (nconc submodules (projectile-flatten
                         ;; recursively get sub-projects of each sub-project
                         (mapcar (lambda (s)
                                   (projectile-get-all-sub-projects s)) submodules)))))))

(defun projectile-get-immediate-sub-projects (path)
  "Get immediate sub-projects for a given project without recursing.

PATH is the vcs root or project root from which to start
searching, and should end with an appropriate path delimiter, such as
'/' or a '\\'.

If the vcs get-sub-projects query returns results outside of path,
they are excluded from the results of this function."
  (let* ((vcs (projectile-project-vcs path))
         ;; search for sub-projects under current project `project'
         (submodules (mapcar
                      (lambda (s)
                        (file-name-as-directory (expand-file-name s path)))
                      (projectile-files-via-ext-command path (projectile-get-sub-projects-command vcs))))
         (project-child-folder-regex
          (concat "\\`"
                  (regexp-quote path))))

    ;; If project root is inside of an VCS folder, but not actually an
    ;; VCS root itself, submodules external to the project will be
    ;; included in the VCS get sub-projects result. Let's remove them.
    (cl-remove-if-not
     (lambda (submodule)
       (string-match-p project-child-folder-regex
                       submodule))
     submodules)))

(defun projectile-get-sub-projects-files (project-root _vcs)
  "Get files from sub-projects for PROJECT-ROOT recursively."
  (projectile-flatten
   (mapcar (lambda (sub-project)
             (let ((project-relative-path
                    (file-name-as-directory (file-relative-name
                                             sub-project project-root))))
               (mapcar (lambda (file)
                         (concat project-relative-path file))
                       ;; TODO: Seems we forgot git hardcoded here
                       (projectile-files-via-ext-command sub-project projectile-git-command))))
           (projectile-get-all-sub-projects project-root))))

(defun projectile-get-repo-ignored-files (project vcs)
  "Get a list of the files ignored in the PROJECT using VCS."
  (let ((cmd (projectile-get-ext-ignored-command vcs)))
    (when cmd
      (projectile-files-via-ext-command project cmd))))

(defun projectile-get-repo-ignored-directory (project dir vcs)
  "Get a list of the files ignored in the PROJECT in the directory DIR.
VCS is the VCS of the project."
  (let ((cmd (projectile-get-ext-ignored-command vcs)))
    (when cmd
      (projectile-files-via-ext-command project (concat cmd " " dir)))))

(defun projectile-files-via-ext-command (root command)
  "Get a list of relative file names in the project ROOT by executing COMMAND.

If `command' is nil or an empty string, return nil.
This allows commands to be disabled.

Only text sent to standard output is taken into account."
  (when (stringp command)
    (let ((default-directory root))
      (with-temp-buffer
        (shell-command command t "*projectile-files-errors*")
        (let ((shell-output (buffer-substring (point-min) (point-max))))
          (split-string (string-trim shell-output) "\0" t))))))

(defun projectile-adjust-files (project vcs files)
  "First remove ignored files from FILES, then add back unignored files."
  (projectile-add-unignored project vcs (projectile-remove-ignored files)))

(defun projectile-remove-ignored (files)
  "Remove ignored files and folders from FILES.

If ignored directory prefixed with '*', then ignore all
directories/subdirectories with matching filename,
otherwise operates relative to project root."
  (let ((ignored-files (projectile-ignored-files-rel))
        (ignored-dirs (projectile-ignored-directories-rel)))
    (cl-remove-if
     (lambda (file)
       (or (cl-some
            (lambda (f)
              (string= f (file-name-nondirectory file)))
            ignored-files)
           (cl-some
            (lambda (dir)
              ;; if the directory is prefixed with '*' then ignore all directories matching that name
              (if (string-prefix-p "*" dir)
                  ;; remove '*' and trailing slash from ignored directory name
                  (let ((d (substring dir 1 (if (equal (substring dir -1) "/") -1 nil))))
                    (cl-some
                     (lambda (p)
                       (string= d p))
                     ;; split path by '/', remove empty strings, and check if any subdirs match name 'd'
                     (delete "" (split-string (or (file-name-directory file) "") "/"))))
                (string-prefix-p dir file)))
            ignored-dirs)
           (cl-some
            (lambda (suf)
              (string-suffix-p suf file t))
            projectile-globally-ignored-file-suffixes)))
     files)))

(defun projectile-keep-ignored-files (project vcs files)
  "Filter FILES to retain only those that are ignored."
  (when files
    (cl-remove-if-not
     (lambda (file)
       (cl-some (lambda (f) (string-prefix-p f file)) files))
     (projectile-get-repo-ignored-files project vcs))))

(defun projectile-keep-ignored-directories (project vcs directories)
  "Get ignored files within each of DIRECTORIES."
  (when directories
    (let (result)
      (dolist (dir directories result)
        (setq result (append result
                             (projectile-get-repo-ignored-directory project dir vcs))))
      result)))

(defun projectile-add-unignored (project vcs files)
  "This adds unignored files to FILES.

Useful because the VCS may not return ignored files at all.  In
this case unignored files will be absent from FILES."
  (let ((unignored-files (projectile-keep-ignored-files
                          project
                          vcs
                          (projectile-unignored-files-rel)))
        (unignored-paths (projectile-remove-ignored
                          (projectile-keep-ignored-directories
                           project
                           vcs
                           (projectile-unignored-directories-rel)))))
    (append files unignored-files unignored-paths)))

(defun projectile-buffers-with-file (buffers)
  "Return only those BUFFERS backed by files."
  (cl-remove-if-not (lambda (b) (buffer-file-name b)) buffers))

(defun projectile-buffers-with-file-or-process (buffers)
  "Return only those BUFFERS backed by files or processes."
  (cl-remove-if-not (lambda (b) (or (buffer-file-name b)
                                    (get-buffer-process b))) buffers))

(defun projectile-project-buffers (&optional project)
  "Get a list of a project's buffers.
If PROJECT is not specified the command acts on the current project."
  (let* ((project-root (or project (projectile-acquire-root)))
         (all-buffers (cl-remove-if-not
                       (lambda (buffer)
                         (projectile-project-buffer-p buffer project-root))
                       (buffer-list))))
    (if projectile-buffers-filter-function
        (funcall projectile-buffers-filter-function all-buffers)
      all-buffers)))

(defun projectile-process-current-project-buffers (action)
  "Process the current project's buffers using ACTION."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (funcall action buffer))))

(defun projectile-process-current-project-buffers-current (action)
  "Invoke ACTION on every project buffer with that buffer current.
ACTION is called without arguments."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (with-current-buffer buffer
        (funcall action)))))

(defun projectile-project-buffer-files (&optional project)
  "Get a list of a project's buffer files.
If PROJECT is not specified the command acts on the current project."
  (let ((project-root (or project (projectile-project-root))))
    (mapcar
     (lambda (buffer)
       (file-relative-name
        (buffer-file-name buffer)
        project-root))
     (projectile-buffers-with-file
      (projectile-project-buffers project)))))

(defun projectile-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (let ((directory (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
      (and (not (string-prefix-p " " (buffer-name buffer)))
           (not (projectile-ignored-buffer-p buffer))
           directory
           (string-equal (file-remote-p directory)
                         (file-remote-p project-root))
           (not (string-match-p "^http\\(s\\)?://" directory))
           (string-prefix-p project-root (file-truename directory) (eq system-type 'windows-nt))))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored.

Regular expressions can be use."
  (or
   (with-current-buffer buffer
     (cl-some
      (lambda (name)
        (string-match-p name (buffer-name)))
      projectile-globally-ignored-buffers))
   (with-current-buffer buffer
     (cl-some
      (lambda (mode)
        (string-match-p (concat "^" mode "$")
                        (symbol-name major-mode)))
      projectile-globally-ignored-modes))))

(defun projectile-recently-active-files ()
  "Get list of recently active files.

Files are ordered by recently active buffers, and then recently
opened through use of recentf."
  (let ((project-buffer-files (projectile-project-buffer-files)))
    (append project-buffer-files
            (projectile-difference
             (projectile-recentf-files)
             project-buffer-files))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar #'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  "Prepend the current project's name to STRING."
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.

This function excludes the current buffer from the offered
choices."
  (projectile-completing-read
   prompt
   (delete (buffer-name (current-buffer))
           (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer-other-window ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-window
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer-other-frame ()
  "Switch to a project buffer and show it in another frame."
  (interactive)
  (switch-to-buffer-other-frame
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-project-buffers-other-buffer ()
  "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned."
  (interactive)
  (switch-to-buffer (car (projectile-project-buffers-non-visible))) nil t)

(defun projectile-project-buffers-non-visible ()
  "Get a list of non visible project buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (not (get-buffer-window buffer 'visible)))
   (projectile-project-buffers)))

;;;###autoload
(defun projectile-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (multi-occur (projectile-project-buffers project)
                 (car (occur-read-primary-args))
                 nlines)))

(defun projectile-normalise-paths (patterns)
  "Remove leading `/' from the elements of PATTERNS."
  (delq nil (mapcar (lambda (pat) (and (string-prefix-p "/" pat)
                                       ;; remove the leading /
                                       (substring pat 1)))
                    patterns)))

(defun projectile-expand-paths (paths)
  "Expand the elements of PATHS.

Elements containing wildcards are expanded and spliced into the
resulting paths.  The returned PATHS are absolute, based on the
projectile project root."
  (let ((default-directory (projectile-project-root)))
    (projectile-flatten (mapcar
                         (lambda (pattern)
                           (or (file-expand-wildcards pattern t)
                               (projectile-expand-root pattern)))
                         paths))))

(defun projectile-normalise-patterns (patterns)
  "Remove paths from PATTERNS."
  (cl-remove-if (lambda (pat) (string-prefix-p "/" pat)) patterns))

(defun projectile-make-relative-to-root (files)
  "Make FILES relative to the project root."
  (let ((project-root (projectile-project-root)))
    (mapcar (lambda (f) (file-relative-name f project-root)) files)))

(defun projectile-ignored-directory-p
    (directory &optional ignored-directories local-directory globally-ignored-directories)
  "Check if DIRECTORY should be ignored.

Regular expressions can be used.  Pre-computed lists of
IGNORED-DIRECTORIES and GLOBALLY-IGNORED-DIRECTORIES
and the LOCAL-DIRECTORY name may optionally be provided."
  (let ((ignored-directories (or ignored-directories (projectile-ignored-directories)))
        (globally-ignored-directories (or globally-ignored-directories (projectile-globally-ignored-directory-names)))
        (local-directory (or local-directory (file-name-nondirectory (directory-file-name directory)))))
    (or (cl-some
         (lambda (name)
           (string-match-p name directory))
         ignored-directories)
        (cl-some
         (lambda (name)
           (string-match-p name local-directory))
         globally-ignored-directories))))

(defun projectile-ignored-file-p (file &optional ignored-files)
  "Check if FILE should be ignored.

Regular expressions can be used.  A pre-computed list of
IGNORED-FILES may optionally be provided."
  (cl-some
   (lambda (name)
     (string-match-p name file))
   (or ignored-files (projectile-ignored-files))))

(defun projectile-check-pattern-p (file pattern)
  "Check if FILE meets PATTERN."
  (or (string-suffix-p (directory-file-name pattern)
                       (directory-file-name file))
      (member file (file-expand-wildcards pattern t))))

(defun projectile-ignored-rel-p (file directory patterns)
  "Check if FILE should be ignored relative to DIRECTORY.
PATTERNS should have the form: (ignored . unignored)"
  (let ((default-directory directory))
    (and (cl-some
          (lambda (pat) (projectile-check-pattern-p file pat))
          (car patterns))
         (cl-notany
          (lambda (pat) (projectile-check-pattern-p file pat))
          (cdr patterns)))))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (projectile-difference
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-ignored-files
     (projectile-project-ignored-files)))
   (projectile-unignored-files)))

(defun projectile-globally-ignored-directory-names ()
  "Return list of ignored directory names."
  (projectile-difference
   projectile-globally-ignored-directories
   projectile-globally-unignored-directories))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (projectile-difference
   (mapcar
    #'file-name-as-directory
    (mapcar
     #'projectile-expand-root
     (append
      projectile-globally-ignored-directories
      (projectile-project-ignored-directories))))
   (projectile-unignored-directories)))

(defun projectile-ignored-directories-rel ()
  "Return list of ignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-directories)))

(defun projectile-ignored-files-rel ()
  "Return list of ignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-files)))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files.
Unignored files are not included."
  (cl-remove-if 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories.
Unignored directories are not included."
  (cl-remove-if-not 'file-directory-p (projectile-project-ignored)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (projectile-normalise-paths (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories.
Unignored files/directories are not included."
  (let ((paths (projectile-paths-to-ignore)))
    (projectile-expand-paths paths)))

(defun projectile-unignored-files ()
  "Return list of unignored files."
  (mapcar
   #'projectile-expand-root
   (append
    projectile-globally-unignored-files
    (projectile-project-unignored-files))))

(defun projectile-unignored-directories ()
  "Return list of unignored directories."
  (mapcar
   #'file-name-as-directory
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-unignored-directories
     (projectile-project-unignored-directories)))))

(defun projectile-unignored-directories-rel ()
  "Return list of unignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-directories)))

(defun projectile-unignored-files-rel ()
  "Return list of unignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-files)))

(defun projectile-project-unignored-files ()
  "Return list of project unignored files."
  (cl-remove-if 'file-directory-p (projectile-project-unignored)))

(defun projectile-project-unignored-directories ()
  "Return list of project unignored directories."
  (cl-remove-if-not 'file-directory-p (projectile-project-unignored)))

(defun projectile-paths-to-ensure ()
  "Return a list of unignored project paths."
  (projectile-normalise-paths (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-files-to-ensure ()
  (projectile-flatten (mapcar (lambda (pat) (file-expand-wildcards pat t))
                              (projectile-patterns-to-ensure))))

(defun projectile-patterns-to-ensure ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-filtering-patterns ()
  (cons (projectile-patterns-to-ignore)
        (projectile-patterns-to-ensure)))

(defun projectile-project-unignored ()
  "Return list of project ignored files/directories."
  (delete-dups (append (projectile-expand-paths (projectile-paths-to-ensure))
                       (projectile-expand-paths (projectile-files-to-ensure)))))


(defun projectile-dirconfig-file ()
  "Return the absolute path to the project's dirconfig file."
  (expand-file-name projectile-dirconfig-file (projectile-project-root)))

(defun projectile-parse-dirconfig-file ()
  "Parse project ignore file and return directories to ignore and keep.

The return value will be a list of three elements, the car being
the list of directories to keep, the cadr being the list of files
or directories to ignore, and the caddr being the list of files
or directories to ensure.

Strings starting with + will be added to the list of directories
to keep, and strings starting with - will be added to the list of
directories to ignore.  For backward compatibility, without a
prefix the string will be assumed to be an ignore string."
  (let (keep ignore ensure (dirconfig (projectile-dirconfig-file)))
    (when (projectile-file-exists-p dirconfig)
      (with-temp-buffer
        (insert-file-contents dirconfig)
        (while (not (eobp))
          (pcase (char-after)
            ;; ignore comment lines if prefix char has been set
            ((pred (lambda (leading-char)
                     (and projectile-dirconfig-comment-prefix
                          (eql leading-char
                               projectile-dirconfig-comment-prefix))))
             nil)
            (?+ (push (buffer-substring (1+ (point)) (line-end-position)) keep))
            (?- (push (buffer-substring (1+ (point)) (line-end-position)) ignore))
            (?! (push (buffer-substring (1+ (point)) (line-end-position)) ensure))
            (_ (push (buffer-substring (point) (line-end-position)) ignore)))
          (forward-line)))
      (list (mapcar (lambda (f) (file-name-as-directory (string-trim f)))
                    (delete "" (reverse keep)))
            (mapcar #'string-trim
                    (delete "" (reverse ignore)))
            (mapcar #'string-trim
                    (delete "" (reverse ensure)))))))

(defun projectile-expand-root (name &optional dir)
  "Expand NAME to project root.
When DIR is specified it uses DIR's project, otherwise it acts
on the current project.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root dir)))

(cl-defun projectile-completing-read (prompt choices &key initial-input action)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt))
        res)
    (setq res
          (pcase (if (eq projectile-completion-system 'auto)
                     (cond
                      ((bound-and-true-p ido-mode)  'ido)
                      ((bound-and-true-p helm-mode) 'helm)
                      ((bound-and-true-p ivy-mode)  'ivy)
                      (t 'default))
                   projectile-completion-system)
            ('default (completing-read prompt choices nil nil initial-input))
            ('ido (ido-completing-read prompt choices nil nil initial-input))
            ('helm
             (if (and (fboundp 'helm)
                      (fboundp 'helm-make-source))
                 (helm :sources
                       (helm-make-source "Projectile" 'helm-source-sync
                                         :candidates choices
                                         :action (if action
                                                     (prog1 action
                                                       (setq action nil))
                                                   #'identity))
                       :prompt prompt
                       :input initial-input
                       :buffer "*helm-projectile*")
               (user-error "Please install helm")))
            ('ivy
             (if (fboundp 'ivy-read)
                 (ivy-read prompt choices
                           :initial-input initial-input
                           :action (prog1 action
                                     (setq action nil))
                           :caller 'projectile-completing-read)
               (user-error "Please install ivy")))
            (_ (funcall projectile-completion-system prompt choices))))
    (if action
        (funcall action res)
      res)))

(defun projectile-project-files (project-root)
  "Return a list of files for the PROJECT-ROOT."
  (let (files)
    ;; If the cache is too stale, don't use it.
    (when projectile-files-cache-expire
      (let ((cache-time
             (gethash project-root projectile-projects-cache-time)))
        (when (or (null cache-time)
                  (< (+ cache-time projectile-files-cache-expire)
                     (projectile-time-seconds)))
          (remhash project-root projectile-projects-cache)
          (remhash project-root projectile-projects-cache-time))))

    ;; Use the cache, if requested and available.
    (when projectile-enable-caching
      (setq files (gethash project-root projectile-projects-cache)))

    ;; Calculate the list of files.
    (when (null files)
      (when projectile-enable-caching
        (message "Projectile is initializing cache for %s ..." project-root))
      (setq files
            (if (eq projectile-indexing-method 'alien)
                ;; In alien mode we can just skip reading
                ;; .projectile and find all files in the root dir.
                (projectile-dir-files-alien project-root)
              ;; If a project is defined as a list of subfolders
              ;; then we'll have the files returned for each subfolder,
              ;; so they are relative to the project root.
              ;;
              ;; TODO: That's pretty slow and we need to improve it.
              ;; One options would be to pass explicitly the subdirs
              ;; to commands like `git ls-files` which would return
              ;; files paths relative to the project root.
              (cl-mapcan
               (lambda (dir)
                 (mapcar (lambda (f)
                           (file-relative-name (concat dir f)
                                               project-root))
                         (projectile-dir-files dir)))
               (projectile-get-project-directories project-root))))

      ;; Save the cached list.
      (when projectile-enable-caching
        (projectile-cache-project project-root files)))

    ;;; Sorting
    ;;
    ;; Files can't be cached in sorted order as some sorting schemes
    ;; require dynamic data.  Sorting is ignored completely when in
    ;; alien mode.
    (if (eq projectile-indexing-method 'alien)
        files
      (projectile-sort-files files))))

(defun projectile-current-project-files ()
  "Return a list of the files in the current project."
  (projectile-project-files (projectile-acquire-root)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (dolist (filename project-files)
      (funcall action filename))))

(defun projectile-project-dirs (project)
  "Return a list of dirs for PROJECT."
  (delete-dups
   (delq nil
         (mapcar #'file-name-directory
                 (projectile-project-files project)))))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (projectile-project-dirs (projectile-acquire-root)))

(defun projectile-get-other-files (file-name &optional flex-matching)
  "Return a list of other files for FILE-NAME.
The list depends on `:related-files-fn' project option and
`projectile-other-file-alist'.  For the latter, FLEX-MATCHING can be used
to match any basename."
  (if-let ((plist (projectile--related-files-plist-by-kind  file-name :other)))
      (projectile--related-files-from-plist plist)
    (projectile--other-extension-files file-name
                                       (projectile-current-project-files)
                                       flex-matching)))

(defun projectile--find-other-file (&optional flex-matching ff-variant)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.  With FF-VARIANT set to a defun, use that
instead of `find-file'.   A typical example of such a defun would be
`find-file-other-window' or `find-file-other-frame'"
  (let ((ff (or ff-variant #'find-file))
        (other-files (projectile-get-other-files (buffer-file-name) flex-matching)))
    (if other-files
        (let ((file-name (projectile--choose-from-candidates other-files)))
          (funcall ff (expand-file-name file-name
                                        (projectile-project-root))))
      (error "No other file found"))))


;;; Interactive commands
;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching))

;;;###autoload
(defun projectile-find-other-file-other-window (&optional flex-matching)
  "Switch between files with different extensions in other window.
Switch between files with the same name but different extensions in other
window.  With FLEX-MATCHING, match any file that contains the base name of
current file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-window))

;;;###autoload
(defun projectile-find-other-file-other-frame (&optional flex-matching)
  "Switch between files with different extensions in other frame.
Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current
file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-frame))

(defun projectile--file-name-sans-extensions (file-name)
  "Return FILE-NAME sans any extensions.
The extensions, in a filename, are what follows the first '.', with the
exception of a leading '.'"
  (setq file-name (file-name-nondirectory file-name))
  (substring file-name 0 (string-match "\\..*" file-name 1)))

(defun projectile--file-name-extensions (file-name)
  "Return FILE-NAME's extensions.
The extensions, in a filename, are what follows the first '.', with the
exception of a leading '.'"
  ;;would it make sense to return nil instead of an empty string if no extensions are found?
  (setq file-name (file-name-nondirectory file-name))
  (let (extensions-start)
    (substring file-name
               (if (setq extensions-start (string-match "\\..*" file-name 1))
                   (1+ extensions-start)
                 (length file-name)))))

(defun projectile-associated-file-name-extensions (file-name)
  "Return projectile-other-file-extensions associated to FILE-NAME's extensions.
If no associated other-file-extensions for the complete (nested) extension
are found, remove subextensions from FILENAME's extensions until a match is
found."
  (let ((current-extensions (projectile--file-name-extensions (file-name-nondirectory file-name)))
        associated-extensions)
    (catch 'break
      (while (not (string= "" current-extensions))
        (if (setq associated-extensions (cdr (assoc current-extensions projectile-other-file-alist)))
            (throw 'break associated-extensions))
        (setq current-extensions (projectile--file-name-extensions current-extensions))))))

(defun projectile--other-extension-files (current-file project-file-list &optional flex-matching)
  "Narrow to files with the same names but different extensions.
Returns a list of possible files for users to choose.

With FLEX-MATCHING, match any file that contains the base name of current file"
  (let* ((file-ext-list (projectile-associated-file-name-extensions current-file))
         (fulldirname (if (file-name-directory current-file)
                          (file-name-directory current-file) "./"))
         (dirname (file-name-nondirectory (directory-file-name fulldirname)))
         (filename (regexp-quote (projectile--file-name-sans-extensions current-file)))
         (file-list (mapcar (lambda (ext)
                              (if flex-matching
                                  (concat ".*" filename ".*" "\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat "\." ext))
                                        "\\'")))
                            file-ext-list))
         (candidates (cl-remove-if-not
                      (lambda (project-file)
                        (string-match filename project-file))
                      project-file-list))
         (candidates
          (projectile-flatten (mapcar
                               (lambda (file)
                                 (cl-remove-if-not
                                  (lambda (project-file)
                                    (string-match file
                                                  (concat (file-name-base project-file)
                                                          (unless (equal (file-name-extension project-file) nil)
                                                            (concat "\." (file-name-extension project-file))))))
                                  candidates))
                               file-list)))
         (candidates
          (cl-remove-if-not (lambda (file) (not (backup-file-name-p file))) candidates))
         (candidates
          (cl-sort (copy-sequence candidates)
                   (lambda (file _)
                     (let ((candidate-dirname (file-name-nondirectory (directory-file-name (file-name-directory file)))))
                       (unless (equal fulldirname (file-name-directory file))
                         (equal dirname candidate-dirname)))))))
    candidates))

(defun projectile-select-files (project-files &optional invalidate-cache)
  "Select a list of files based on filename at point.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         (files (if file
                    (cl-remove-if-not
                     (lambda (project-file)
                       (string-match file project-file))
                     project-files)
                  nil)))
    files))

(defun projectile--find-file-dwim (invalidate-cache &optional ff-variant)
  "Jump to a project's files using completion based on context.

With a INVALIDATE-CACHE invalidates the cache first.

With FF-VARIANT set to a defun, use that instead of `find-file'.
A typical example of such a defun would be `find-file-other-window' or
`find-file-other-frame'

Subroutine for `projectile-find-file-dwim' and
`projectile-find-file-dwim-other-window'"
  (let* ((project-root (projectile-acquire-root))
         (project-files (projectile-project-files project-root))
         (files (projectile-select-files project-files invalidate-cache))
         (file (cond ((= (length files) 1)
                      (car files))
                     ((> (length files) 1)
                      (projectile-completing-read "Switch to: " files))
                     (t
                      (projectile-completing-read "Switch to: " project-files))))
         (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file project-root))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-dwim (&optional invalidate-cache)
  "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim' is executed on a filepath like
\"projectile/\", it lists the content of that directory.  If it is executed
on a partial filename like \"projectile/a\", a list of files with character
\"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache))

;;;###autoload
(defun projectile-find-file-dwim-other-window (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-window' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-dwim-other-frame (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-frame' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache #'find-file-other-frame))

(defun projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (projectile-acquire-root))
         (file (projectile-completing-read "Find file: "
                                           (projectile-project-files project-root)))
         (ff (or ff-variant #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

;;;###autoload
(defun projectile-find-file (&optional invalidate-cache)
  "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache))

;;;###autoload
(defun projectile-find-file-other-window (&optional invalidate-cache)
  "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-other-frame (&optional invalidate-cache)
  "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache #'find-file-other-frame))

;;;###autoload
(defun projectile-toggle-project-read-only ()
  "Toggle project read only."
  (interactive)
  (let ((inhibit-read-only t)
        (val (not buffer-read-only))
        (default-directory (projectile-acquire-root)))
    (add-dir-local-variable nil 'buffer-read-only val)
    (save-buffer)
    (kill-buffer)
    (when buffer-file-name
      (read-only-mode (if val +1 -1))
      (message "[%s] read-only-mode is %s" (projectile-project-name) (if val "on" "off")))))

;;;###autoload
(defun projectile-add-dir-local-variable (mode variable value)
  "Run `add-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to `add-dir-local-variable'."
  (let ((inhibit-read-only t)
        (default-directory (projectile-acquire-root)))
    (add-dir-local-variable mode variable value)
    (save-buffer)
    (kill-buffer)))

;;;###autoload
(defun projectile-delete-dir-local-variable (mode variable)
  "Run `delete-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to
`delete-dir-local-variable'."
  (let ((inhibit-read-only t)
        (default-directory (projectile-acquire-root)))
    (delete-dir-local-variable mode variable)
    (save-buffer)
    (kill-buffer)))


;;;; Sorting project files
(defun projectile-sort-files (files)
  "Sort FILES according to `projectile-sort-order'."
  (cl-case projectile-sort-order
    (default files)
    (recentf (projectile-sort-by-recentf-first files))
    (recently-active (projectile-sort-by-recently-active-first files))
    (modification-time (projectile-sort-by-modification-time files))
    (access-time (projectile-sort-by-access-time files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (let ((project-recentf-files (projectile-recentf-files)))
    (append project-recentf-files
            (projectile-difference files project-recentf-files))))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (let ((project-recently-active-files (projectile-recently-active-files)))
    (append project-recently-active-files
            (projectile-difference files project-recently-active-files))))

(defun projectile-sort-by-modification-time (files)
  "Sort FILES by modification time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-mtime (nth 5 (file-attributes file1)))
             (file2-mtime (nth 5 (file-attributes file2))))
         (not (time-less-p file1-mtime file2-mtime)))))))

(defun projectile-sort-by-access-time (files)
  "Sort FILES by access time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-atime (nth 4 (file-attributes file1)))
             (file2-atime (nth 4 (file-attributes file2))))
         (not (time-less-p file1-atime file2-atime)))))))


;;;; Find directory in project functionality
(defun projectile--find-dir (invalidate-cache &optional dired-variant)
  "Jump to a project's directory using completion.

With INVALIDATE-CACHE invalidates the cache first.  With DIRED-VARIANT set to a
defun, use that instead of `dired'.  A typical example of such a defun would be
`dired-other-window' or `dired-other-frame'"
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project (projectile-acquire-root))
         (dir (projectile-complete-dir project))
         (dired-v (or dired-variant #'dired)))
    (funcall dired-v (expand-file-name dir project))
    (run-hooks 'projectile-find-dir-hook)))

;;;###autoload
(defun projectile-find-dir (&optional invalidate-cache)
  "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache))

;;;###autoload
(defun projectile-find-dir-other-window (&optional invalidate-cache)
  "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-window))

;;;###autoload
(defun projectile-find-dir-other-frame (&optional invalidate-cache)
  "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-frame))

(defun projectile-complete-dir (project)
  (let ((project-dirs (projectile-project-dirs project)))
    (projectile-completing-read
     "Find dir: "
     (if projectile-find-dir-includes-top-level
         (append '("./") project-dirs)
       project-dirs))))

;;;###autoload
(defun projectile-find-test-file (&optional invalidate-cache)
  "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (cl-remove-if-not 'projectile-test-file-p files))

(defun projectile--merge-related-files-fns (related-files-fns)
  "Merge multiple RELATED-FILES-FNS into one function."
  (lambda (path)
    (let (merged-plist)
      (dolist (fn related-files-fns merged-plist)
        (let ((plist (funcall fn path)))
          (cl-loop for (key value) on plist by #'cddr
                   do (let ((values (if (consp value) value (list value))))
                        (if (plist-member merged-plist key)
                            (nconc (plist-get merged-plist key) values)
                          (setq merged-plist (plist-put merged-plist key values))))))))))

(defun projectile--related-files-plist (project-root file)
  "Return a plist containing all related files information for FILE.
PROJECT-ROOT is the project root."
  (if-let ((rel-path (if (file-name-absolute-p file)
                         (file-relative-name file project-root)
                       file))
           (custom-function (funcall projectile-related-files-fn-function (projectile-project-type))))
      (funcall (cond ((functionp custom-function)
                      custom-function)
                     ((consp custom-function)
                      (projectile--merge-related-files-fns custom-function))
                     (t
                      (error "Unsupported value type of :related-files-fn")))
               rel-path)))

(defun projectile--related-files-plist-by-kind (file kind)
  "Return a plist containing :paths and/or :predicate of KIND for FILE."
  (if-let ((project-root (projectile-project-root))
           (plist (projectile--related-files-plist project-root file))
           (has-kind? (plist-member plist kind)))
      (let* ((kind-value (plist-get plist kind))
             (values (if (cl-typep kind-value '(or string function))
                         (list kind-value)
                       kind-value))
             (paths (delete-dups (cl-remove-if-not 'stringp values)))
             (predicates (delete-dups (cl-remove-if-not 'functionp values))))
        (append
         ;; Make sure that :paths exists even with nil if there is no predicates
         (when (or paths (null predicates))
           (list :paths (cl-remove-if-not
                         (lambda (f)
                           (projectile-file-exists-p (projectile-expand-file-name-wildcard f project-root)))
                         paths)))
         (when predicates
           (list :predicate (if (= 1 (length predicates))
                                (car predicates)
                              (lambda (other-file)
                                (cl-some (lambda (predicate)
                                           (funcall predicate other-file))
                                         predicates)))))))))

(defun projectile--related-files-from-plist (plist)
  "Return a list of files matching to PLIST from current project files."
  (let* ((predicate (plist-get plist :predicate))
         (paths (plist-get plist :paths)))
    (delete-dups (append
                  paths
                  (when predicate
                    (cl-remove-if-not predicate (projectile-current-project-files)))))))

(defun projectile--related-files-kinds(file)
  "Return a list o keywords meaning available related kinds for FILE."
  (if-let ((project-root (projectile-project-root))
           (plist (projectile--related-files-plist project-root file)))
      (cl-loop for key in plist by #'cddr
               collect key)))

(defun projectile--related-files (file kind)
  "Return a list of related files of KIND for FILE."
  (projectile--related-files-from-plist (projectile--related-files-plist-by-kind file kind)))

(defun projectile--find-related-file (file &optional kind)
  "Choose a file from files related to FILE as KIND.
If KIND is not provided, a list of possible kinds can be chosen."
  (unless kind
    (if-let ((available-kinds (projectile--related-files-kinds file)))
        (setq kind (if (= (length available-kinds) 1)
                       (car available-kinds)
                     (intern (projectile-completing-read "Kind :" available-kinds))))
      (error "No related files found")))

  (if-let ((candidates (projectile--related-files file kind)))
      (projectile-expand-root (projectile--choose-from-candidates candidates))
    (error
     "No matching related file as `%s' found for project type `%s'"
     kind (projectile-project-type))))

;;;###autoload
(defun projectile-find-related-file-other-window ()
  "Open related file in other window."
  (interactive)
  (find-file-other-window
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-find-related-file-other-frame ()
  "Open related file in other frame."
  (interactive)
  (find-file-other-frame
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-find-related-file()
  "Open related file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-related-files-fn-groups(kind groups)
  "Generate a related-files-fn which relates as KIND for files in each of GROUPS."
  (lambda (path)
    (if-let ((group-found (cl-find-if (lambda (group)
                                        (member path group))
                                      groups)))
        (list kind (cl-remove path group-found :test 'equal)))))

;;;###autoload
(defun projectile-related-files-fn-extensions(kind extensions)
  "Generate a related-files-fn which relates as KIND for files having EXTENSIONS."
  (lambda (path)
    (let* ((ext (file-name-extension path))
           (basename (file-name-base path))
           (basename-regexp (regexp-quote basename)))
      (when (member ext extensions)
        (list kind (lambda (other-path)
                     (and (string-match-p basename-regexp other-path)
                          (equal basename (file-name-base other-path))
                          (let ((other-ext (file-name-extension other-path)))
                            (and (member other-ext extensions)
                                 (not (equal other-ext ext)))))))))))

;;;###autoload
(defun projectile-related-files-fn-test-with-prefix(extension test-prefix)
  "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-PREFIX."
  (lambda (path)
    (when (equal (file-name-extension path) extension)
      (let* ((file-name (file-name-nondirectory path))
             (find-impl? (string-prefix-p test-prefix file-name))
             (file-name-to-find (if find-impl?
                                    (substring file-name (length test-prefix))
                                  (concat test-prefix file-name))))
        (list (if find-impl? :impl :test)
              (lambda (other-path)
                (and (string-suffix-p file-name-to-find other-path)
                     (equal (file-name-nondirectory other-path) file-name-to-find))))))))

;;;###autoload
(defun projectile-related-files-fn-test-with-suffix(extension test-suffix)
  "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-SUFFIX."
  (lambda (path)
    (when (equal (file-name-extension path) extension)
      (let* ((file-name (file-name-nondirectory path))
             (dot-ext (concat "." extension))
             (suffix-ext (concat test-suffix dot-ext))
             (find-impl? (string-suffix-p suffix-ext file-name))
             (file-name-to-find (if find-impl?
                                    (concat (substring file-name 0 (- (length suffix-ext)))
                                            dot-ext)
                                  (concat (substring file-name 0 (- (length dot-ext)))
                                          suffix-ext))))
        (list (if find-impl? :impl :test)
              (lambda (other-path)
                (and (string-suffix-p file-name-to-find other-path)
                     (equal (file-name-nondirectory other-path) file-name-to-find))))))))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (let ((kinds (projectile--related-files-kinds file)))
    (cond ((member :impl kinds) t)
          ((member :test kinds) nil)
          (t (or (cl-some (lambda (pat) (string-prefix-p pat (file-name-nondirectory file)))
                          (delq nil (list (funcall projectile-test-prefix-function (projectile-project-type)))))
                 (cl-some (lambda (pat) (string-suffix-p pat (file-name-sans-extension (file-name-nondirectory file))))
                          (delq nil (list (funcall projectile-test-suffix-function (projectile-project-type))))))))))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-project-types nil
  "An alist holding all project types that are known to Projectile.
The project types are symbols and they are linked to plists holding
the properties of the various project types.")

(defun projectile--combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(cl-defun projectile--build-project-plist
    (marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn)
  "Return a project type plist with the provided arguments.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments:
PROJECT-FILE the main project file in the root project directory.
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
INSTALL which specifies a command to install the project.
PACKAGE which specifies a command to package the project.
TEST which specified a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root.
RELATED-FILES-FN which specifies a custom function to find the related
files such as test/impl/other files as below:
    CUSTOM-FUNCTION accepts FILE as relative path from the project root and
    returns a plist containing :test, :impl or :other as key and the
    relative path/paths or predicate as value.  PREDICATE accepts a
    relative path as the input."
  (let ((project-plist (list 'marker-files marker-files
                             'project-file project-file
                             'compilation-dir compilation-dir
                             'configure-command configure
                             'compile-command compile
                             'test-command test
                             'install-command install
                             'package-command package
                             'run-command run))
        (project-files (if (listp project-file)
                           project-file
                         (list project-file))))
    (dolist (project-file project-files)
      (when (and project-file (not (member project-file projectile-project-root-files)))
        (add-to-list 'projectile-project-root-files project-file)))
    (when test-suffix
      (plist-put project-plist 'test-suffix test-suffix))
    (when test-prefix
      (plist-put project-plist 'test-prefix test-prefix))
    (when src-dir
      (plist-put project-plist 'src-dir src-dir))
    (when test-dir
      (plist-put project-plist 'test-dir test-dir))
    (when related-files-fn
      (plist-put project-plist 'related-files-fn related-files-fn))
    project-plist))

(cl-defun projectile-register-project-type
    (project-type marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn)
  "Register a project type with projectile.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments:
PROJECT-FILE the main project file in the root project directory.
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
INSTALL which specifies a command to install the project.
PACKAGE which specifies a command to package the project.
TEST which specified a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root.
RELATED-FILES-FN which specifies a custom function to find the related
files such as test/impl/other files as below:
    CUSTOM-FUNCTION accepts FILE as relative path from the project root and
    returns a plist containing :test, :impl or :other as key and the
    relative path/paths or predicate as value.  PREDICATE accepts a
    relative path as the input."
  (setq projectile-project-types
        (cons `(,project-type .
                              ,(projectile--build-project-plist
                                marker-files
                                :project-file project-file
                                :compilation-dir compilation-dir
                                :configure configure
                                :compile compile
                                :install install
                                :package package
                                :test test
                                :run run
                                :test-suffix test-suffix
                                :test-prefix test-prefix
                                :src-dir src-dir
                                :test-dir test-dir
                                :related-files-fn related-files-fn))
              projectile-project-types)))

(cl-defun projectile-update-project-type
    (project-type
     &key precedence
     (marker-files nil marker-files-specified)
     (project-file nil project-file-specified)
     (compilation-dir nil compilation-dir-specified)
     (configure nil configure-specified)
     (compile nil compile-specified)
     (install nil install-specified)
     (package nil package-specified)
     (test nil test-specified)
     (run nil run-specified)
     (test-suffix nil test-suffix-specified)
     (test-prefix nil test-prefix-specified)
     (src-dir nil src-dir-specified)
     (test-dir nil test-dir-specified)
     (related-files-fn nil related-files-fn-specified))
    "Update an existing projectile project type.

Passed items will override existing values for the project type given
by PROJECT-TYPE.  nil can be used to remove a project type attribute.  Raise
an error if PROJECT-TYPE is not already registered with projectile.  This
function may also take the keyword argument PRECEDENCE which when set to ‘high’
will make projectile prioritise this project type over other clashing project
types, and a value of ‘low’ will make projectile prefer (all) other project
types by default.  Otherwise, the arguments to this function are as for
`projectile-register-project-type':

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments:
MARKER-FILES a set of indicator files for PROJECT-TYPE.
PROJECT-FILE the main project file in the root project directory.
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
INSTALL which specifies a command to install the project.
PACKAGE which specifies a command to package the project.
TEST which specified a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root.
RELATED-FILES-FN which specifies a custom function to find the related
files such as test/impl/other files as below:
    CUSTOM-FUNCTION accepts FILE as relative path from the project root and
    returns a plist containing :test, :impl or :other as key and the
    relative path/paths or predicate as value.  PREDICATE accepts a
    relative path as the input."
    (let* ((existing-project-plist
            (or (cl-find-if
                 (lambda (p) (eq project-type (car p))) projectile-project-types)
                (error "No existing project found for: %s" project-type)))
           (new-plist
            (append
             (when marker-files-specified `(marker-files ,marker-files))
             (when project-file-specified `(project-file ,project-file))
             (when compilation-dir-specified `(compilation-dir ,compilation-dir))
             (when configure-specified `(configure-command ,configure))
             (when compile-specified `(compile-command ,compile))
             (when test-specified `(test-command ,test))
             (when install-specified `(install-command ,install))
             (when package-specified `(package-command ,package))
             (when run-specified `(run-command ,run))
             (when test-suffix-specified `(test-suffix ,test-suffix))
             (when test-prefix-specified `(test-prefix ,test-prefix))
             (when src-dir-specified `(src-dir ,src-dir))
             (when test-dir-specified `(test-dir ,test-dir))
             (when related-files-fn-specified
               `(related-files-fn ,related-files-fn))))
           (merged-plist
            (projectile--combine-plists
             (cdr existing-project-plist) new-plist))
           (project-type-elt (cons project-type merged-plist)))
      (cl-flet* ((project-filter (p) (eq project-type (car p)))
                 (project-map (p) (if (project-filter p) project-type-elt p)))
        (setq projectile-project-types
              (if precedence
                  (let ((filtered-types
                       (cl-remove-if #'project-filter projectile-project-types)))
                    (setq projectile-project-type-cache (make-hash-table))
                    (cond ((eq precedence 'high)
                           (cons project-type-elt filtered-types))
                          ((eq precedence 'low)
                           (append filtered-types (list project-type-elt)))
                          (t (error "Precedence must be one of '(high low)"))))
                (mapcar #'project-map projectile-project-types))))))

(defun projectile-eldev-project-p (&optional dir)
  "Check if a project contains eldev files.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "Eldev" dir)
      (projectile-verify-file "Eldev-local" dir)))

(defun projectile-expand-file-name-wildcard (name-pattern dir)
  "Expand the maybe-wildcard-containing NAME-PATTERN in DIR.
If there are results expanding a wildcard, get the first result,
otherwise expand NAME-PATTERN in DIR ignoring wildcards."
  (let ((expanded (expand-file-name name-pattern dir)))
    (or (if (string-match-p "[[*?]" name-pattern)
            (car
             (file-expand-wildcards expanded)))
        expanded)))

(defun projectile-cabal-project-p (&optional dir)
  "Check if a project contains *.cabal files but no stack.yaml file.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (and (projectile-verify-file-wildcard "?*.cabal" dir)
       (not (projectile-verify-file "stack.yaml" dir))))

(defun projectile-dotnet-project-p (&optional dir)
  "Check if a project contains a .NET project marker.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file-wildcard "?*.csproj" dir)
      (projectile-verify-file-wildcard "?*.fsproj" dir)))

(defun projectile-go-project-p (&optional dir)
  "Check if a project contains Go source files.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "go.mod" dir)
      (projectile-verify-file-wildcard "*.go" dir)))

(defcustom projectile-go-project-test-function #'projectile-go-project-p
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "1.0.0"))

(defun projectile-nimble-project-p (&optional dir)
  "Check if a project contains a Nimble project marker.
Nim projects that use Nimble contain a <projectname>.nimble file.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (projectile-verify-file-wildcard "?*.nimble" dir))

;;;; Constant signifying opting out of CMake preset commands.
(defconst projectile--cmake-no-preset "*no preset*")

(defun projectile--cmake-version ()
  "Compute CMake version."
  (let* ((string (shell-command-to-string "cmake --version"))
         (match (string-match "^cmake version \\(.*\\)$" string)))
    (when match
      (version-to-list (match-string 1 string)))))

(defun projectile--cmake-check-version (version)
  "Check if CMake version is at least VERSION."
  (and
   (version-list-<= version (projectile--cmake-version))))

(defconst projectile--cmake-command-presets-minimum-version-alist
  '((:configure-command . (3 19))
    (:compile-command . (3 20))
    (:test-command . (3 20))
    (:package-command . (3 19))
    (:install-command . (3 20))))

(defun projectile--cmake-command-presets-supported (command-type)
  "Check if CMake supports presets for COMMAND-TYPE."
  (let ((minimum-version
         (cdr (assoc command-type projectile--cmake-command-presets-minimum-version-alist))))
    (projectile--cmake-check-version minimum-version)))

(defun projectile--cmake-read-preset (filename)
  "Read CMake preset from FILENAME."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (when (functionp 'json-parse-buffer)
        (json-parse-buffer :array-type 'list)))))

(defconst projectile--cmake-command-preset-array-id-alist
  '((:configure-command . "configurePresets")
    (:compile-command . "buildPresets")
    (:test-command . "testPresets")
    (:package-command . "packagePresets")
    (:install-command . "buildPresets")))

(defun projectile--cmake-command-preset-array-id (command-type)
  "Map from COMMAND-TYPE to id of command preset array in CMake preset."
  (cdr (assoc command-type projectile--cmake-command-preset-array-id-alist)))

(defun projectile--cmake-command-presets-shallow (filename command-type)
  "Get CMake COMMAND-TYPE presets from FILENAME."
  (when-let ((preset (projectile--cmake-read-preset (projectile-expand-root filename))))
    (cl-remove-if
     (lambda (preset) (equal (gethash "hidden" preset) t))
     (gethash (projectile--cmake-command-preset-array-id command-type) preset))))

(defun projectile--cmake-command-presets (filename command-type)
  "Get CMake COMMAND-TYPE presets from FILENAME.  Follows included files."
  (when-let ((preset (projectile--cmake-read-preset (projectile-expand-root filename))))
    (append
     (projectile--cmake-command-presets-shallow filename command-type)
     (mapcar
      (lambda (included-file) (projectile--cmake-command-presets
                               (expand-file-name included-file (file-name-directory filename))
                               command-type))
      (gethash "include" preset)))))

(defun projectile--cmake-all-command-presets (command-type)
  "Get CMake user and system COMMAND-TYPE presets."
  (projectile-flatten
   (mapcar (lambda (filename) (projectile--cmake-command-presets filename command-type))
           '("CMakeUserPresets.json" "CMakePresets.json"))))

(defun projectile--cmake-command-preset-names (command-type)
  "Get names of CMake user and system COMMAND-TYPE presets."
  (mapcar (lambda (preset)
            (gethash "name" preset))
          (projectile--cmake-all-command-presets command-type)))

(defcustom projectile-enable-cmake-presets nil
  "Enables configuration with CMake presets.

When `projectile-enable-cmake-presets' is non-nil, CMake projects can
be configured, built and tested using presets."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.4.0"))

(defun projectile--cmake-use-command-presets (command-type)
  "Test whether or not to use command presets for COMMAND-TYPE.

Presets are used if `projectile-enable-cmake-presets' is non-nil, and CMake
supports presets for COMMAND-TYPE, and `json-parse-buffer' is available."
  (and projectile-enable-cmake-presets
       (projectile--cmake-command-presets-supported command-type)
       (functionp 'json-parse-buffer)))

(defun projectile--cmake-select-command (command-type)
  "Select a CMake command preset or a manual CMake command.

The selection is done like this:

- If `projectile--cmake-use-commands-presets' for COMMAND-TYPE returns true, and
there is at least one preset available for COMMAND-TYPE, the user is prompted to
select a name of a command preset, or opt a manual command by selecting
`projectile--cmake-no-preset'.

- Else `projectile--cmake-no-preset' is used."
  (if-let ((use-presets (projectile--cmake-use-command-presets command-type))
           (preset-names (projectile--cmake-command-preset-names command-type)))
      (projectile-completing-read
       "Use preset: "
       (append preset-names `(,projectile--cmake-no-preset)))
    projectile--cmake-no-preset))

(defconst projectile--cmake-manual-command-alist
  '((:configure-command . "cmake -S . -B build")
    (:compile-command . "cmake --build build")
    (:test-command . "cmake --build build --target test")
    (:package-command . "cmake --build build --target package")
    (:install-command . "cmake --build build --target install")))

(defun projectile--cmake-manual-command (command-type)
  "Create maunual CMake COMMAND-TYPE command."
  (cdr (assoc command-type projectile--cmake-manual-command-alist)))

(defconst projectile--cmake-preset-command-alist
  '((:configure-command . "cmake . --preset %s")
    (:compile-command . "cmake --build --preset %s")
    (:test-command . "ctest --preset %s")
    (:package-command . "cpack --preset %s")
    (:install-command . "cmake --build --preset %s --target install")))

(defun projectile--cmake-preset-command (command-type preset)
  "Create CMake COMMAND-TYPE command using PRESET."
  (format (cdr (assoc command-type projectile--cmake-preset-command-alist)) preset))

(defun projectile--cmake-command (command-type)
  "Create a CMake COMMAND-TYPE command.

The command is created like this:

- If `projectile--cmake-select-command' returns `projectile--cmake-no-preset'
a manual COMMAND-TYPE command is created with
`projectile--cmake-manual-command'.

- Else a preset COMMAND-TYPE command using the selected preset is created with
`projectile--cmake-preset-command'."
  (let ((maybe-preset (projectile--cmake-select-command command-type)))
    (if (equal maybe-preset projectile--cmake-no-preset)
        (projectile--cmake-manual-command command-type)
      (projectile--cmake-preset-command command-type maybe-preset))))

(defun projectile--cmake-configure-command ()
  "CMake configure command."
  (projectile--cmake-command :configure-command))

(defun projectile--cmake-compile-command ()
  "CMake compile command."
  (projectile--cmake-command :compile-command))

(defun projectile--cmake-test-command ()
  "CMake test command."
  (projectile--cmake-command :test-command))

(defun projectile--cmake-install-command ()
  "CMake install command."
  (projectile--cmake-command :install-command))

(defun projectile--cmake-package-command ()
  "CMake package command."
  (projectile--cmake-command :package-command))

;;; Project type registration
;;
;; Project type detection happens in a reverse order with respect to
;; project type registration (invocations of `projectile-register-project-type').
;;
;; As function-based project type detection is pretty slow, so it
;; should be tried at the end if everything else failed (meaning here
;; it should be listed first).
;;
;; Ideally common project types should be checked earlier than exotic ones.

;; Function-based detection project type
(projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                  :compile "cabal build"
                                  :test "cabal test"
                                  :run "cabal run"
                                  :test-suffix "Spec")
(projectile-register-project-type 'dotnet #'projectile-dotnet-project-p
                                  :project-file '("?*.csproj" "?*.fsproj")
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test")
(projectile-register-project-type 'dotnet-sln '("src")
                                  :project-file "?*.sln"
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test")
(projectile-register-project-type 'nim-nimble #'projectile-nimble-project-p
                                  :project-file "?*.nimble"
                                  :compile "nimble --noColor build --colors:off"
                                  :install "nimble --noColor install --colors:off"
                                  :test "nimble --noColor test -d:nimUnittestColor:off --colors:off"
                                  :run "nimble --noColor run --colors:off"
                                  :src-dir "src"
                                  :test-dir "tests")
;; File-based detection project types

;; Universal
(projectile-register-project-type 'scons '("SConstruct")
                                  :project-file "SConstruct"
                                  :compile "scons"
                                  :test "scons test"
                                  :test-suffix "test")
(projectile-register-project-type 'meson '("meson.build")
                                  :project-file "meson.build"
                                  :compilation-dir "build"
                                  :configure "meson %s"
                                  :compile "ninja"
                                  :test "ninja test")
(projectile-register-project-type 'nix '("default.nix")
                                  :project-file "default.nix"
                                  :compile "nix-build"
                                  :test "nix-build")
(projectile-register-project-type 'nix-flake '("flake.nix")
                                  :project-file "flake.nix"
                                  :compile "nix build"
                                  :test "nix flake check"
                                  :run "nix run")
(projectile-register-project-type 'bazel '("WORKSPACE")
                                  :project-file "WORKSPACE"
                                  :compile "bazel build"
                                  :test "bazel test"
                                  :run "bazel run")
(projectile-register-project-type 'debian '("debian/control")
                                  :project-file "debian/control"
                                  :compile "debuild -uc -us")

;; Make & CMake
(projectile-register-project-type 'make '("Makefile")
                                  :project-file "Makefile"
                                  :compile "make"
                                  :test "make test"
                                  :install "make install")
(projectile-register-project-type 'gnumake '("GNUMakefile")
                                  :project-file "GNUMakefile"
                                  :compile "make"
                                  :test "make test"
                                  :install "make install")
(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :project-file "CMakeLists.txt"
                                  :configure #'projectile--cmake-configure-command
                                  :compile #'projectile--cmake-compile-command
                                  :test #'projectile--cmake-test-command
                                  :install #'projectile--cmake-install-command
                                  :package #'projectile--cmake-package-command)
;; go-task/task
(projectile-register-project-type 'go-task '("Taskfile.yml")
                                  :project-file "Taskfile.yml"
                                  :compile "task build"
                                  :test "task test"
                                  :install "task install")
;; Go should take higher precedence than Make because Go projects often have a Makefile.
(projectile-register-project-type 'go projectile-go-project-test-function
                                  :compile "go build"
                                  :test "go test ./..."
                                  :test-suffix "_test")
;; PHP
(projectile-register-project-type 'php-symfony '("composer.json" "app" "src" "vendor")
                                  :project-file "composer.json"
                                  :compile "app/console server:run"
                                  :test "phpunit -c app "
                                  :test-suffix "Test")
;; Erlang & Elixir
(projectile-register-project-type 'rebar '("rebar.config")
                                  :project-file "rebar.config"
                                  :compile "rebar3 compile"
                                  :test "rebar3 do eunit,ct"
                                  :test-suffix "_SUITE")
(projectile-register-project-type 'elixir '("mix.exs")
                                  :project-file "mix.exs"
                                  :compile "mix compile"
                                  :src-dir "lib/"
                                  :test "mix test"
                                  :test-suffix "_test")
;; JavaScript
(projectile-register-project-type 'grunt '("Gruntfile.js")
                                  :project-file "Gruntfile.js"
                                  :compile "grunt"
                                  :test "grunt test")
(projectile-register-project-type 'gulp '("gulpfile.js")
                                  :project-file "gulpfile.js"
                                  :compile "gulp"
                                  :test "gulp test")
(projectile-register-project-type 'npm '("package.json" "package-lock.json")
                                  :project-file "package.json"
                                  :compile "npm install && npm run build"
                                  :test "npm test"
                                  :test-suffix ".test")
(projectile-register-project-type 'yarn '("package.json" "yarn.lock")
                                  :project-file "package.json"
                                  :compile "yarn && yarn build"
                                  :test "yarn test"
                                  :test-suffix ".test")
(projectile-register-project-type 'pnpm '("package.json" "pnpm-lock.yaml")
                                  :project-file "package.json"
                                  :compile "pnpm install && pnpm build"
                                  :test "pnpm test"
                                  :test-suffix ".test")
;; Angular
(projectile-register-project-type 'angular '("angular.json" ".angular-cli.json")
                                  :project-file "angular.json"
                                  :compile "ng build"
                                  :run "ng serve"
                                  :test "ng test"
                                  :test-suffix ".spec")
;; Python
(projectile-register-project-type 'django '("manage.py")
                                  :project-file "manage.py"
                                  :compile "python manage.py runserver"
                                  :test "python manage.py test"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pip '("requirements.txt")
                                  :project-file "requirements.txt"
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pkg '("setup.py")
                                  :project-file "setup.py"
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-tox '("tox.ini")
                                  :project-file "tox.ini"
                                  :compile "tox -r --notest"
                                  :test "tox"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pipenv '("Pipfile")
                                  :project-file "Pipfile"
                                  :compile "pipenv run build"
                                  :test "pipenv run test"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-poetry '("poetry.lock")
                                  :project-file "poetry.lock"
                                  :compile "poetry build"
                                  :test "poetry run python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-toml '("pyproject.toml")
                                  :project-file "pyproject.toml"
                                  :compile "python -m build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
;; Java & friends
(projectile-register-project-type 'maven '("pom.xml")
                                  :project-file "pom.xml"
                                  :compile "mvn -B clean install"
                                  :test "mvn -B test"
                                  :test-suffix "Test"
                                  :src-dir "src/main/"
                                  :test-dir "src/test/")
(projectile-register-project-type 'gradle '("build.gradle")
                                  :project-file "build.gradle"
                                  :compile "gradle build"
                                  :test "gradle test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'gradlew '("gradlew")
                                  :project-file "gradlew"
                                  :compile "./gradlew build"
                                  :test "./gradlew test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'grails '("application.yml" "grails-app")
                                  :project-file "application.yml"
                                  :compile "grails package"
                                  :test "grails test-app"
                                  :test-suffix "Spec")
;; Scala
(projectile-register-project-type 'sbt '("build.sbt")
                                  :project-file "build.sbt"
                                  :src-dir "main"
                                  :test-dir "test"
                                  :compile "sbt compile"
                                  :test "sbt test"
                                  :test-suffix "Spec")

(projectile-register-project-type 'mill '("build.sc")
                                  :project-file "build.sc"
                                  :src-dir "src/"
                                  :test-dir "test/src/"
                                  :compile "mill __.compile"
                                  :test "mill __.test"
                                  :test-suffix "Test")

;; Clojure
(projectile-register-project-type 'lein-test '("project.clj")
                                  :project-file "project.clj"
                                  :compile "lein compile"
                                  :test "lein test"
                                  :test-suffix "_test")
(projectile-register-project-type 'lein-midje '("project.clj" ".midje.clj")
                                  :project-file "project.clj"
                                  :compile "lein compile"
                                  :test "lein midje"
                                  :test-prefix "t_")
(projectile-register-project-type 'boot-clj '("build.boot")
                                  :project-file "build.boot"
                                  :compile "boot aot"
                                  :test "boot test"
                                  :test-suffix "_test")
(projectile-register-project-type 'clojure-cli '("deps.edn")
                                  :project-file "deps.edn"
                                  :test-suffix "_test")
(projectile-register-project-type 'bloop '(".bloop")
                                  :project-file ".bloop"
                                  :compile "bloop compile root"
                                  :test "bloop test --propagate --reporter scalac root"
                                  :src-dir "src/main/"
                                  :test-dir "src/test/"
                                  :test-suffix "Spec")
;; Ruby
(projectile-register-project-type 'ruby-rspec '("Gemfile" "lib" "spec")
                                  :project-file "Gemfile"
                                  :compile "bundle exec rake"
                                  :src-dir "lib/"
                                  :test "bundle exec rspec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")
(projectile-register-project-type 'ruby-test '("Gemfile" "lib" "test")
                                  :project-file "Gemfile"
                                  :compile"bundle exec rake"
                                  :src-dir "lib/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test")
;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(projectile-register-project-type 'rails-test '("Gemfile" "app" "lib" "db" "config" "test")
                                  :project-file "Gemfile"
                                  :compile "bundle exec rails server"
                                  :src-dir "app/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test")
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                  :project-file "Gemfile"
                                  :compile "bundle exec rails server"
                                  :src-dir "app/"
                                  :test "bundle exec rspec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")
;; Crystal
(projectile-register-project-type 'crystal-spec '("shard.yml")
                                  :project-file "shard.yml"
                                  :src-dir "src/"
                                  :test "crystal spec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")

;; Emacs
(projectile-register-project-type 'emacs-cask '("Cask")
                                  :project-file "Cask"
                                  :compile "cask install"
                                  :test-prefix "test-"
                                  :test-suffix "-test")
(projectile-register-project-type 'emacs-eldev #'projectile-eldev-project-p
                                  :project-file "Eldev"
                                  :compile "eldev compile"
                                  :test "eldev test"
                                  :run "eldev emacs"
                                  :package "eldev package")

;; R
(projectile-register-project-type 'r '("DESCRIPTION")
                                  :project-file "DESCRIPTION"
                                  :compile "R CMD INSTALL --with-keep.source ."
                                  :test (concat "R CMD check -o " temporary-file-directory " ."))

;; Haskell
(projectile-register-project-type 'haskell-stack '("stack.yaml")
                                  :project-file "stack.yaml"
                                  :compile "stack build"
                                  :test "stack build --test"
                                  :test-suffix "Spec")

;; Rust
(projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                  :project-file "Cargo.toml"
                                  :compile "cargo build"
                                  :test "cargo test"
                                  :run "cargo run")

;; Racket
(projectile-register-project-type 'racket '("info.rkt")
                                  :project-file "info.rkt"
                                  :test "raco test ."
                                  :install "raco pkg install"
                                  :package "raco pkg create --source $(pwd)")

;; Dart
(projectile-register-project-type 'dart '("pubspec.yaml")
                                  :project-file "pubspec.yaml"
                                  :compile "pub get"
                                  :test "pub run test"
                                  :run "dart"
                                  :test-suffix "_test.dart")

;; Elm
(projectile-register-project-type 'elm '("elm.json")
                                  :project-file "elm.json"
                                  :compile "elm make")

;; Julia
(projectile-register-project-type 'julia '("Project.toml")
                                  :project-file "Project.toml"
                                  :compile "julia --project=@. -e 'import Pkg; Pkg.precompile(); Pkg.build()'"
                                  :test "julia --project=@. -e 'import Pkg; Pkg.test()' --check-bounds=yes"
                                  :src-dir "src"
                                  :test-dir "test")

;; OCaml
(projectile-register-project-type 'ocaml-dune '("dune-project")
                                  :project-file "dune-project"
                                  :compile "dune build"
                                  :test "dune runtest")

(defvar-local projectile-project-type nil
  "Buffer local var for overriding the auto-detected project type.
Normally you'd set this from .dir-locals.el.")
(put 'projectile-project-type 'safe-local-variable #'symbolp)

(defun projectile-detect-project-type (&optional dir)
  "Detect the type of the project.
When DIR is specified it detects its project type, otherwise it acts
on the current project.

Fallsback to a generic project type when the type can't be determined."
  (let ((project-type
         (or (car (cl-find-if
                   (lambda (project-type-record)
                     (let ((project-type (car project-type-record))
                           (marker (plist-get (cdr project-type-record) 'marker-files)))
                       (if (functionp marker)
                           (and (funcall marker dir) project-type)
                         (and (projectile-verify-files marker dir) project-type))))
                   projectile-project-types))
             'generic)))
    (puthash (projectile-project-root dir) project-type projectile-project-type-cache)
    project-type))

(defun projectile-project-type (&optional dir)
  "Determine a project's type based on its structure.
When DIR is specified it checks it, otherwise it acts
on the current project.

The project type is cached for improved performance."
  (or (and (not dir) projectile-project-type)
      (if-let ((project-root (projectile-project-root dir)))
          (or (gethash project-root projectile-project-type-cache)
              (projectile-detect-project-type dir)))))

;;;###autoload
(defun projectile-project-info ()
  "Display info for current project."
  (interactive)
  (message "Project dir: %s ## Project VCS: %s ## Project type: %s"
           (projectile-acquire-root)
           (projectile-project-vcs)
           (projectile-project-type)))

(defun projectile-verify-files (files &optional dir)
  "Check whether all FILES exist in the project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (cl-every #'(lambda (file) (projectile-verify-file file dir)) files))

(defun projectile-verify-file (file &optional dir)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (file-exists-p (projectile-expand-root file dir)))

(defun projectile-verify-file-wildcard (file &optional dir)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project.
Expands wildcards using `file-expand-wildcards' before checking."
  (file-expand-wildcards (projectile-expand-root file dir)))

(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
the variable `projectile-project-root'."
  (or project-root (setq project-root (projectile-acquire-root)))
  (cond
   ;; first we check for a VCS marker in the project root itself
   ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
   ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
   ((projectile-file-exists-p (expand-file-name ".fslckout" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name "_FOSSIL_" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ((projectile-file-exists-p (expand-file-name ".pijul" project-root)) 'pijul)
   ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ;; then we check if there's a VCS marker up the directory tree
   ;; that covers the case when a project is part of a multi-project repository
   ;; in those cases you can still the VCS to get a list of files for
   ;; the project in question
   ((projectile-locate-dominating-file project-root ".git") 'git)
   ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ((projectile-locate-dominating-file project-root ".fslckout") 'fossil)
   ((projectile-locate-dominating-file project-root "_FOSSIL_") 'fossil)
   ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ((projectile-locate-dominating-file project-root ".pijul") 'pijul)
   ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))

(defun projectile--test-name-for-impl-name (impl-file-path)
  "Determine the name of the test file for IMPL-FILE-PATH.

IMPL-FILE-PATH may be a absolute path, relative path or a file name."
  (let* ((project-type (projectile-project-type))
         (impl-file-name (file-name-sans-extension (file-name-nondirectory impl-file-path)))
         (impl-file-ext (file-name-extension impl-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix (concat test-prefix impl-file-name "." impl-file-ext))
     (test-suffix (concat impl-file-name test-suffix "." impl-file-ext))
     (t (error "Cannot determine a test file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

(defun projectile--impl-name-for-test-name (test-file-path)
  "Determine the name of the implementation file for TEST-FILE-PATH.

TEST-FILE-PATH may be a absolute path, relative path or a file name."
  (let* ((project-type (projectile-project-type))
         (test-file-name (file-name-sans-extension (file-name-nondirectory test-file-path)))
         (test-file-ext (file-name-extension test-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix
      (concat (string-remove-prefix test-prefix test-file-name) "." test-file-ext))
     (test-suffix
      (concat (string-remove-suffix test-suffix test-file-name) "." test-file-ext))
     (t (error "Cannot determine an implementation file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

(defun projectile--test-to-impl-dir (test-dir-path)
  "Return the directory path of an impl file with test file in TEST-DIR-PATH.

Occurrences of the current project type's test-dir property (which should be a
string) are replaced with the current project type's src-dir property
 (which should be a string) to obtain the new directory.

Nil is returned if either the src-dir or test-dir properties are not strings."
  (let* ((project-type (projectile-project-type))
         (test-dir (projectile-test-directory project-type))
         (impl-dir (projectile-src-directory project-type)))
    (when (and (stringp test-dir) (stringp impl-dir))
      (if (not (string-match-p test-dir (file-name-directory test-dir-path)))
          (error "Attempted to find a implementation file by switching this project type's (%s) test-dir property \"%s\" with this project type's src-dir property \"%s\", but %s does not contain \"%s\""
                 project-type test-dir impl-dir test-dir-path test-dir)
        (projectile-complementary-dir test-dir-path test-dir impl-dir)))))

(defun projectile--impl-to-test-dir-fallback (impl-dir-path)
  "Return the test file for IMPL-DIR-PATH by guessing a test directory.

Occurrences of the `projectile-default-src-directory' in the directory of
IMPL-DIR-PATH are replaced with `projectile-default-test-directory'.  Nil is
returned if `projectile-default-src-directory' is not a substring of
IMPL-DIR-PATH."
  (when-let ((file (projectile--complementary-file
                    impl-dir-path
                    (lambda (f)
                      (when (string-match-p projectile-default-src-directory f)
                        (projectile-complementary-dir
                         impl-dir-path
                         projectile-default-src-directory
                         projectile-default-test-directory)))
                    #'projectile--test-name-for-impl-name)))
    (file-relative-name file (projectile-project-root))))

(defun projectile--test-to-impl-dir-fallback (test-dir-path)
  "Return the impl file for TEST-DIR-PATH by guessing a source directory.

Occurrences of `projectile-default-test-directory' in the directory of
TEST-DIR-PATH are replaced with `projectile-default-src-directory'.  Nil is
returned if `projectile-default-test-directory' is not a substring of
TEST-DIR-PATH."
  (when-let ((file (projectile--complementary-file
                    test-dir-path
                    (lambda (f)
                      (when (string-match-p projectile-default-test-directory f)
                        (projectile-complementary-dir
                         test-dir-path
                         projectile-default-test-directory
                         projectile-default-src-directory)))
                    #'projectile--impl-name-for-test-name)))
    (file-relative-name file (projectile-project-root))))

(defun projectile--impl-to-test-dir (impl-dir-path)
  "Return the directory path of a test whose impl file resides in IMPL-DIR-PATH.

Occurrences of the current project type's src-dir property (which should be a
string) are replaced with the current project type's test-dir property
 (which should be a string) to obtain the new directory.

If the src-dir property is set and IMPL-DIR-PATH does not contain (as a
substring) the src-dir property of the current project type, an error is
signalled.

Nil is returned if either the src-dir or test-dir properties are not strings."
  (let* ((project-type (projectile-project-type))
         (test-dir (projectile-test-directory project-type))
         (impl-dir (projectile-src-directory project-type)))
    (when (and (stringp test-dir) (stringp impl-dir))
      (if (not (string-match-p impl-dir (file-name-directory impl-dir-path)))
          (error "Attempted to find a test file by switching this project type's (%s) src-dir property \"%s\" with this project type's test-dir property \"%s\", but %s does not contain \"%s\""
                 project-type impl-dir test-dir impl-dir-path impl-dir)
        (projectile-complementary-dir impl-dir-path impl-dir test-dir)))))

(defun projectile-complementary-dir (dir-path string replacement)
  "Return the \"complementary\" directory of DIR-PATH.
Replace STRING in DIR-PATH with REPLACEMENT."
  (let* ((project-root (projectile-project-root))
         (relative-dir (file-name-directory (file-relative-name dir-path project-root))))
    (projectile-expand-root
     ;; TODO: Use string-replace once we target emacs 28
     (replace-regexp-in-string string replacement relative-dir t))))

(defun projectile--create-directories-for (path)
  "Create directories necessary for PATH."
  (unless (file-exists-p path)
    (make-directory (if (file-directory-p path)
                        path
                      (file-name-directory path))
                    :create-parents)))

(defun projectile-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename.

If `projectile-create-missing-test-files' is non-nil, create the missing
test file."
  (unless file-name (error "The current buffer is not visiting a file"))
  (unless (projectile-project-type) (projectile-ensure-project nil))
  (if (projectile-test-file-p file-name)
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file file-name)))
        (if impl-file
            (projectile-expand-root impl-file)
          (error
           "No matching source file found for project type `%s'"
           (projectile-project-type))))
    ;; find the matching test file
    (let* ((error-msg (format
                       "No matching test file found for project type `%s'"
                       (projectile-project-type)))
           (test-file (or (projectile-find-matching-test file-name)
                          (error error-msg)))
           (expanded-test-file (projectile-expand-root test-file)))
      (cond ((file-exists-p expanded-test-file) expanded-test-file)
            (projectile-create-missing-test-files
             (projectile--create-directories-for expanded-test-file)
             expanded-test-file)
            (t (error "Determined test file to be \"%s\", which does not exist.  Set `projectile-create-missing-test-files' to allow `projectile-find-implementation-or-test' to create new files" test-file))))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-frame ()
  "Open matching implementation or test file in other frame.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (interactive)
  (find-file-other-frame
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file.


See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (interactive)
  (find-file
   (projectile-find-implementation-or-test (buffer-file-name))))


(defun projectile-project-type-attribute (project-type key &optional default-value)
  "Return the value of some PROJECT-TYPE attribute identified by KEY.
Fallback to DEFAULT-VALUE for missing attributes."
  (let ((project (alist-get project-type projectile-project-types)))
    (if (and project (plist-member project key))
        (plist-get project key)
      default-value)))

(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (or projectile-project-test-prefix
      (projectile-project-type-attribute project-type 'test-prefix)))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (or projectile-project-test-suffix
      (projectile-project-type-attribute project-type 'test-suffix)))

(defun projectile-related-files-fn (project-type)
  "Find relative file based on PROJECT-TYPE."
  (or projectile-project-related-files-fn
      (projectile-project-type-attribute project-type 'related-files-fn)))

(defun projectile-src-directory (project-type)
  "Find default src directory based on PROJECT-TYPE."
  (or projectile-project-src-dir
      (projectile-project-type-attribute project-type 'src-dir)))

(defun projectile-test-directory (project-type)
  "Find default test directory based on PROJECT-TYPE."
  (or projectile-project-test-dir
      (projectile-project-type-attribute project-type 'test-dir)))

(defun projectile-dirname-matching-count (a b)
  "Count matching dirnames ascending file paths in A and B."
  (setq a (reverse (split-string (or (file-name-directory a) "") "/" t))
        b (reverse (split-string (or (file-name-directory b) "") "/" t)))
  (let ((common 0))
    (while (and a b (string-equal (pop a) (pop b)))
      (setq common (1+ common)))
    common))

(defun projectile-group-file-candidates (file candidates)
  "Group file candidates by dirname matching count."
  (cl-sort (copy-sequence
            (let (value result)
              (while (setq value (pop candidates))
                (let* ((key (projectile-dirname-matching-count file value))
                       (kv (assoc key result)))
                  (if kv
                      (setcdr kv (cons value (cdr kv)))
                    (push (list key value) result))))
              (mapcar (lambda (x)
                        (cons (car x) (nreverse (cdr x))))
                      (nreverse result))))
           (lambda (a b) (> (car a) (car b)))))

(defun projectile--best-or-all-candidates-based-on-parents-dirs (file candidates)
  "Return a list of the best one one for FILE from CANDIDATES or all CANDIDATES."
  (let ((grouped-candidates (projectile-group-file-candidates file candidates)))
    (if (= (length (car grouped-candidates)) 2)
        (list (car (last (car grouped-candidates))))
      (apply #'append (mapcar #'cdr grouped-candidates)))))

(defun projectile--impl-to-test-predicate (impl-file)
  "Return a predicate, which returns t for any test files for IMPL-FILE."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory impl-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (prefix-name (when test-prefix (concat test-prefix basename)))
         (suffix-name (when test-suffix (concat basename test-suffix))))
    (lambda (current-file)
      (let ((name (file-name-sans-extension (file-name-nondirectory current-file))))
        (or (string-equal prefix-name name)
            (string-equal suffix-name name))))))

(defun projectile--complementary-file (file-path dir-fn filename-fn)
  "Apply DIR-FN and FILENAME-FN to the directory and name of FILE-PATH.

More specifically, return DIR-FN applied to the directory of FILE-PATH
concatenated with FILENAME-FN applied to the file name of FILE-PATH.

If either function returns nil, return nil."
  (let ((filename (file-name-nondirectory file-path)))
    (when-let ((complementary-filename (funcall filename-fn filename))
               (dir (funcall dir-fn (file-name-directory file-path))))
     (concat (file-name-as-directory dir) complementary-filename))))

(defun projectile--impl-file-from-src-dir-str (file-name)
  "Get the relative path of the implementation file FILE-NAME.
Return a path relative to the project root for the impl file of FILE-NAME
using the src-dir and test-dir properties of the current project type which
should be strings, nil returned if this is not the case."
  (when-let ((complementary-file (projectile--complementary-file
                                  file-name
                                  #'projectile--test-to-impl-dir
                                  #'projectile--impl-name-for-test-name)))
    (file-relative-name complementary-file (projectile-project-root))))

(defun projectile--test-file-from-test-dir-str (file-name)
  "Get the relative path of the test file FILE-NAME.
Return a path relative to the project root for the test file of FILE-NAME
using the src-dir and test-dir properties of the current project type which
should be strings, nil returned if this is not the case."
  (when-let (complementary-file (projectile--complementary-file
                                 file-name
                                 #'projectile--impl-to-test-dir
                                 #'projectile--test-name-for-impl-name))
    (file-relative-name complementary-file (projectile-project-root))))

(defun projectile--impl-file-from-src-dir-fn (test-file)
  "Get the relative path to the implementation file corresponding to TEST-FILE.
Return the implementation file path for the absolute path TEST-FILE
relative to the project root in the case the current project type's src-dir
has been set to a custom function, return nil if this is not the case or
the path points to a file that does not exist."
  (when-let ((src-dir (projectile-src-directory (projectile-project-type))))
    (when (functionp src-dir)
      (let ((impl-file (projectile--complementary-file
                        test-file
                        src-dir
                        #'projectile--impl-name-for-test-name)))
        (when (file-exists-p impl-file)
          (file-relative-name impl-file (projectile-project-root)))))))

(defun projectile--test-file-from-test-dir-fn (impl-file)
  "Get the relative path to the test file corresponding to IMPL-FILE.
Return the test file path for the absolute path IMPL-FILE relative to the
project root, in the case the current project type's test-dir has been set
to a custom function, else return nil."
  (when-let ((test-dir (projectile-test-directory (projectile-project-type))))
    (when (functionp test-dir)
      (file-relative-name
       (projectile--complementary-file
        impl-file
        test-dir
        #'projectile--test-name-for-impl-name)
       (projectile-project-root)))))

(defmacro projectile--acond (&rest clauses)
  "Like `cond', but the result of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'.  This is copied from
anaphora.el."
  (declare (debug cond))
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (cl-gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (if (null ',(cdr cl1))
                 ,sym
               (let ((it ,sym)) ,@(cdr cl1)))
           (projectile--acond ,@(cdr clauses)))))))

(defun projectile--find-matching-test (impl-file)
  "Return a list of test files for IMPL-FILE.

The precedence for determining test files to return is:

1. Use the project type's test-dir property if it's set to a function
2. Use the project type's related-files-fn property if set
3. Use the project type's test-dir property if it's set to a string
4. Attempt to find a file by matching all project files against
   `projectile--impl-to-test-predicate'
5. Fallback to swapping \"src\" for \"test\" in IMPL-FILE if \"src\"
   is a substring of IMPL-FILE."
  (projectile--acond
   ((projectile--test-file-from-test-dir-fn impl-file) (list it))
   ((projectile--related-files-plist-by-kind impl-file :test)
    (projectile--related-files-from-plist it))
   ((projectile--test-file-from-test-dir-str impl-file) (list it))
   ((projectile--best-or-all-candidates-based-on-parents-dirs
     impl-file (cl-remove-if-not
                (projectile--impl-to-test-predicate impl-file)
                (projectile-current-project-files))) it)
   ((projectile--impl-to-test-dir-fallback impl-file)
    (list it))))

(defun projectile--test-to-impl-predicate (test-file)
  "Return a predicate, which returns t for any impl files for TEST-FILE."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory test-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type))))
    (lambda (current-file)
      (let ((name (file-name-nondirectory (file-name-sans-extension current-file))))
        (or (when test-prefix (string-equal (concat test-prefix name) basename))
            (when test-suffix (string-equal (concat name test-suffix) basename)))))))

(defun projectile--find-matching-file (test-file)
  "Return a list of impl files tested by TEST-FILE.

The precedence for determining implementation files to return is:

1. Use the project type's src-dir property if it's set to a function
2. Use the project type's related-files-fn property if set
3. Use the project type's src-dir property if it's set to a string
4. Default to a fallback which matches all project files against
   `projectile--test-to-impl-predicate'
5. Fallback to swapping \"test\" for \"src\" in TEST-FILE if \"test\"
   is a substring of TEST-FILE."
  (projectile--acond
   ((projectile--impl-file-from-src-dir-fn test-file) (list it))
   ((projectile--related-files-plist-by-kind test-file :impl)
    (projectile--related-files-from-plist it))
   ((projectile--impl-file-from-src-dir-str test-file) (list it))
   ((projectile--best-or-all-candidates-based-on-parents-dirs
     test-file (cl-remove-if-not
                (projectile--test-to-impl-predicate test-file)
                (projectile-current-project-files))) it)
   ((projectile--test-to-impl-dir-fallback test-file) (list it))))

(defun projectile--choose-from-candidates (candidates)
  "Choose one item from CANDIDATES."
  (if (= (length candidates) 1)
      (car candidates)
    (projectile-completing-read "Switch to: " candidates)))

(defun projectile-find-matching-test (impl-file)
  "Compute the name of the test matching IMPL-FILE."
  (when-let ((candidates (projectile--find-matching-test impl-file)))
    (projectile--choose-from-candidates candidates)))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (when-let ((candidates (projectile--find-matching-file test-file)))
    (projectile--choose-from-candidates candidates)))

(defun projectile-grep-default-files ()
  "Try to find a default pattern for `projectile-grep'.
This is a subset of `grep-read-files', where either a matching entry from
`grep-files-aliases' or file name extension pattern is returned."
  (when buffer-file-name
    (let* ((fn (file-name-nondirectory buffer-file-name))
           (default-alias
             (let ((aliases (remove (assoc "all" grep-files-aliases)
                                    grep-files-aliases))
                   alias)
               (while aliases
                 (setq alias (car aliases)
                       aliases (cdr aliases))
                 (if (string-match (mapconcat
                                    #'wildcard-to-regexp
                                    (split-string (cdr alias) nil t)
                                    "\\|")
                                   fn)
                     (setq aliases nil)
                   (setq alias nil)))
               (cdr alias)))
           (default-extension
             (let ((ext (file-name-extension fn)))
               (and ext (concat "*." ext)))))
      (or default-alias default-extension))))

(defun projectile--globally-ignored-file-suffixes-glob ()
  "Return ignored file suffixes as a list of glob patterns."
  (mapcar (lambda (pat) (concat "*" pat)) projectile-globally-ignored-file-suffixes))

(defun projectile--read-search-string-with-default (prefix-label)
  (let* ((prefix-label (projectile-prepend-project-name prefix-label))
         (default-value (projectile-symbol-or-selection-at-point))
         (default-label (if (or (not default-value)
                                (string= default-value ""))
                            ""
                          (format " (default %s)" default-value))))
    (read-string (format "%s%s: " prefix-label default-label) nil nil default-value)))

(defvar projectile-grep-find-ignored-paths)
(defvar projectile-grep-find-unignored-paths)
(defvar projectile-grep-find-ignored-patterns)
(defvar projectile-grep-find-unignored-patterns)

(defun projectile-rgrep-default-command (regexp files dir)
  "Compute the command for \\[rgrep] to use by default.

Extension of the Emacs 25.1 implementation of `rgrep-default-command', with
which it shares its arglist."
  (require 'find-dired)      ; for `find-name-arg'
  (grep-expand-template
   grep-find-template
   regexp
   (concat (shell-quote-argument "(")
           " " find-name-arg " "
           (mapconcat
            #'shell-quote-argument
            (split-string files)
            (concat " -o " find-name-arg " "))
           " "
           (shell-quote-argument ")"))
   dir
   (concat
    (and grep-find-ignored-directories
         (concat "-type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -path "
                 (mapconcat
                  #'identity
                  (delq nil (mapcar
                             #'(lambda (ignore)
                                 (cond ((stringp ignore)
                                        (shell-quote-argument
                                         (concat "*/" ignore)))
                                       ((consp ignore)
                                        (and (funcall (car ignore) dir)
                                             (shell-quote-argument
                                              (concat "*/"
                                                      (cdr ignore)))))))
                             grep-find-ignored-directories))
                  " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and grep-find-ignored-files
         (concat (shell-quote-argument "!") " -type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -name "
                 (mapconcat
                  #'(lambda (ignore)
                      (cond ((stringp ignore)
                             (shell-quote-argument ignore))
                            ((consp ignore)
                             (and (funcall (car ignore) dir)
                                  (shell-quote-argument
                                   (cdr ignore))))))
                  grep-find-ignored-files
                  " -o -name ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and projectile-grep-find-ignored-paths
         (concat (shell-quote-argument "(")
                 " -path "
                 (mapconcat
                  (lambda (ignore) (shell-quote-argument
                                    (concat "./" ignore)))
                  projectile-grep-find-ignored-paths
                  " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and projectile-grep-find-ignored-patterns
         (concat (shell-quote-argument "(")
                 (and (or projectile-grep-find-unignored-paths
                          projectile-grep-find-unignored-patterns)
                      (concat " "
                              (shell-quote-argument "(")))
                 " -path "
                 (mapconcat
                  (lambda (ignore)
                    (shell-quote-argument
                     (if (string-prefix-p "*" ignore) ignore
                       (concat "*/" ignore))))
                  projectile-grep-find-ignored-patterns
                  " -o -path ")
                 (and (or projectile-grep-find-unignored-paths
                          projectile-grep-find-unignored-patterns)
                      (concat " "
                              (shell-quote-argument ")")
                              " -a "
                              (shell-quote-argument "!")
                              " "
                              (shell-quote-argument "(")
                              (and projectile-grep-find-unignored-paths
                                   (concat " -path "
                                           (mapconcat
                                            (lambda (ignore) (shell-quote-argument
                                                              (concat "./" ignore)))
                                            projectile-grep-find-unignored-paths
                                            " -o -path ")))
                              (and projectile-grep-find-unignored-paths
                                   projectile-grep-find-unignored-patterns
                                   " -o")
                              (and projectile-grep-find-unignored-patterns
                                   (concat " -path "
                                           (mapconcat
                                            (lambda (ignore)
                                              (shell-quote-argument
                                               (if (string-prefix-p "*" ignore) ignore
                                                 (concat "*/" ignore))))
                                            projectile-grep-find-unignored-patterns
                                            " -o -path ")))
                              " "
                              (shell-quote-argument ")")))
                 " "
                 (shell-quote-argument ")")
                 " -prune -o ")))))

;;;###autoload
(defun projectile-grep (&optional regexp arg)
  "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  (require 'grep) ;; for `rgrep'
  (let* ((roots (projectile-get-project-directories (projectile-acquire-root)))
         (search-regexp (or regexp
                            (projectile--read-search-string-with-default "Grep for")))
         (files (and arg (or (and (equal current-prefix-arg '-)
                                  (projectile-grep-default-files))
                             (read-string (projectile-prepend-project-name "Grep in: ")
                                          (projectile-grep-default-files))))))
    (dolist (root-dir roots)
      (require 'vc-git) ;; for `vc-git-grep'
      ;; in git projects users have the option to use `vc-git-grep' instead of `rgrep'
      (if (and (eq (projectile-project-vcs) 'git)
               projectile-use-git-grep
               (fboundp 'vc-git-grep))
          (vc-git-grep search-regexp (or files "") root-dir)
        ;; paths for find-grep should relative and without trailing /
        (let ((grep-find-ignored-files
               (cl-union (projectile--globally-ignored-file-suffixes-glob)
                         grep-find-ignored-files))
              (projectile-grep-find-ignored-paths
               (append (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                               (projectile-ignored-directories))
                       (mapcar (lambda (file)
                                 (file-relative-name file root-dir))
                               (projectile-ignored-files))))
              (projectile-grep-find-unignored-paths
               (append (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                               (projectile-unignored-directories))
                       (mapcar (lambda (file)
                                 (file-relative-name file root-dir))
                               (projectile-unignored-files))))
              (projectile-grep-find-ignored-patterns (projectile-patterns-to-ignore))
              (projectile-grep-find-unignored-patterns (projectile-patterns-to-ensure)))
          (grep-compute-defaults)
          (cl-letf (((symbol-function 'rgrep-default-command) #'projectile-rgrep-default-command))
            (rgrep search-regexp (or files "* .*") root-dir)
            (when (get-buffer "*grep*")
              ;; When grep is using a global *grep* buffer rename it to be
              ;; scoped to the current root to allow multiple concurrent grep
              ;; operations, one per root
              (with-current-buffer "*grep*"
                (rename-buffer (concat "*grep <" root-dir ">*"))))))))
    (run-hooks 'projectile-grep-finished-hook)))

;;;###autoload
(defun projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ag %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (if (require 'ag nil 'noerror)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            (ag-ignore-list (delq nil
                                  (delete-dups
                                   (append
                                    ag-ignore-list
                                    (projectile-ignored-files-rel)
                                    (projectile-ignored-directories-rel)
                                    (projectile--globally-ignored-file-suffixes-glob)
                                    ;; ag supports git ignore files directly
                                    (unless (eq (projectile-project-vcs) 'git)
                                      (append grep-find-ignored-files
                                              grep-find-ignored-directories
                                              '()))))))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-acquire-root)))
    (error "Package 'ag' is not available")))

;;;###autoload
(defun projectile-ripgrep (search-term &optional arg)
  "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (let ((args (mapcar (lambda (val) (concat "--glob !" val))
                      (append projectile-globally-ignored-files
                              projectile-globally-ignored-directories))))
    ;; we rely on the external packages ripgrep and rg for the actual search
    ;;
    ;; first we check if we can load ripgrep
    (cond ((require 'ripgrep nil 'noerror)
           (ripgrep-regexp search-term
                           (projectile-acquire-root)
                           (if arg
                               args
                             (cons "--fixed-strings --hidden" args))))
          ;; and then we try rg
          ((require 'rg nil 'noerror)
           (rg-run search-term
                   "*"                       ;; all files
                   (projectile-acquire-root)
                   (not arg)                 ;; literal search?
                   nil                       ;; no need to confirm
                   args))
          (t (error "Packages `ripgrep' and `rg' are not available")))))

(defun projectile-find-references (&optional symbol)
  "Find all references to SYMBOL in the current project.

A thin wrapper around `xref-references-in-directory'."
  (interactive)
  (when (and (fboundp 'xref-references-in-directory)
             (fboundp 'xref--show-xrefs))
    (let ((project-root (projectile-acquire-root))
          (symbol (or symbol (read-from-minibuffer "Lookup in project: " (projectile-symbol-at-point)))))
      (xref--show-xrefs (xref-references-in-directory symbol project-root) nil))))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                       (directory-file-name pattern)))
             (append
              (projectile-ignored-directories-rel)
              (projectile-patterns-to-ignore)) " "))

;;;###autoload
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (if (and (boundp 'ggtags-mode)
           (memq projectile-tags-backend '(auto ggtags)))
      (progn
        (let* ((ggtags-project-root (projectile-acquire-root))
               (default-directory ggtags-project-root))
          (ggtags-ensure-project)
          (ggtags-update-tags t)))
    (let* ((project-root (projectile-acquire-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command
                            (or (file-remote-p tags-file 'localname) tags-file)
                            tags-exclude
                            "."))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (process-file-shell-command command nil (current-buffer))
              shell-output (string-trim
                            (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file)
      (message "Regenerated %s" tags-file))))

(defun projectile-visit-project-tags-table ()
  "Visit the current project's tags table."
  (when (projectile-project-p)
    (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
      (when (file-exists-p tags-file)
        (with-demoted-errors "Error loading tags-file: %s"
          (visit-tags-table tags-file t))))))

(defun projectile-determine-find-tag-fn ()
  "Determine which function to use for a call to `projectile-find-tag'."
  (or
   (cond
    ((eq projectile-tags-backend 'auto)
     (cond
      ((fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim)
      ((fboundp 'xref-find-definitions)
       'xref-find-definitions)
      ((fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
    ((eq projectile-tags-backend 'xref)
     (when (fboundp 'xref-find-definitions)
       'xref-find-definitions))
    ((eq projectile-tags-backend 'ggtags)
     (when (fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim))
    ((eq projectile-tags-backend 'etags-select)
     (when (fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
   'find-tag))

;;;###autoload
(defun projectile-find-tag ()
  "Find tag in project."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (projectile-determine-find-tag-fn)))
    (call-interactively find-tag-fn)))

(defmacro projectile-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

;;;###autoload
(defun projectile-run-command-in-root ()
  "Invoke `execute-extended-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively #'execute-extended-command)))

;;;###autoload
(defun projectile-run-shell-command-in-root (command &optional output-buffer error-buffer)
  "Invoke `shell-command' in the project's root."
  (interactive (list (read-shell-command "Shell command: ")))
  (projectile-with-default-dir (projectile-acquire-root)
    (shell-command command output-buffer error-buffer)))

;;;###autoload
(defun projectile-run-async-shell-command-in-root (command &optional output-buffer error-buffer)
  "Invoke `async-shell-command' in the project's root."
  (interactive (list (read-shell-command "Async shell command: ")))
  (projectile-with-default-dir (projectile-acquire-root)
    (async-shell-command command output-buffer error-buffer)))

;;;###autoload
(defun projectile-run-gdb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively 'gdb)))

;;;###autoload
(defun projectile-run-shell (&optional arg)
  "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (projectile-with-default-dir project
      (shell (projectile-generate-process-name "shell" arg project)))))

;;;###autoload
(defun projectile-run-eshell (&optional arg)
  "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (projectile-with-default-dir project
      (let ((eshell-buffer-name (projectile-generate-process-name "eshell" arg project)))
        (eshell)))))

;;;###autoload
(defun projectile-run-ielm (&optional arg)
  "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let* ((project (projectile-acquire-root))
         (ielm-buffer-name (projectile-generate-process-name "ielm" arg project)))
    (if (get-buffer ielm-buffer-name)
        (switch-to-buffer ielm-buffer-name)
      (projectile-with-default-dir project
        (ielm))
      ;; ielm's buffer name is hardcoded, so we have to rename it after creation
      (rename-buffer ielm-buffer-name))))

;;;###autoload
(defun projectile-run-term (&optional arg)
  "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let* ((project (projectile-acquire-root))
         (buffer-name (projectile-generate-process-name "term" arg project))
         (default-program (or explicit-shell-file-name
                              (getenv "ESHELL")
                              (getenv "SHELL")
                              "/bin/sh")))
    (unless (get-buffer buffer-name)
      (require 'term)
      (let ((program (read-from-minibuffer "Run program: " default-program)))
        (projectile-with-default-dir project
          (set-buffer (term-ansi-make-term buffer-name program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer-name)))

(defun projectile--vterm (&optional new-process other-window)
  "Invoke `vterm' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indentation whether the buffer should
be displayed in a different window.

Switch to the project specific term buffer if it already exists."
  (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "vterm" new-process project)))
    (unless (require 'vterm nil 'noerror)
      (error "Package 'vterm' is not available"))
    (if (buffer-live-p (get-buffer buffer))
        (if other-window
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer))
      (projectile-with-default-dir project
        (if other-window
            (vterm-other-window buffer)
          (vterm buffer))))))

;;;###autoload
(defun projectile-run-vterm (&optional arg)
  "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--vterm arg))

;;;###autoload
(defun projectile-run-vterm-other-window (&optional arg)
  "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--vterm arg 'other-window))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let* ((project (projectile-acquire-root))
         (dir (file-relative-name (expand-file-name directory)
                                  project)))
    (cl-remove-if-not
     (lambda (f) (string-prefix-p dir f))
     (projectile-project-files project))))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (mapcar (lambda (str)
              (concat directory
                      (if (string-prefix-p "./" str)
                          (substring str 2)
                        str)))
            (split-string
             (string-trim (shell-command-to-string cmd))
             "\n+"
             t))))

(defvar projectile-files-with-string-commands
  '((rg . "rg -lF --no-heading --color never ")
    (ag . "ag --literal --nocolor --noheading -l ")
    (ack . "ack --literal --nocolor -l ")
    (git . "git grep -HlI ")
    ;; -r: recursive
    ;; -H: show filename for each match
    ;; -l: show only file names with matches
    ;; -I: no binary files
    (grep . "grep -rHlI %s .")))

(defun projectile--rg-construct-command (search-term &optional file-ext)
  "Construct Rg option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (cdr (assoc 'rg projectile-files-with-string-commands))
              "-g '"
              file-ext
              "' "
              search-term)
    (concat (cdr (assoc 'rg projectile-files-with-string-commands))
            search-term)))

(defun projectile--ag-construct-command (search-term &optional file-ext)
  "Construct Ag option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (cdr (assoc 'ag projectile-files-with-string-commands))
              "-G "
              (replace-regexp-in-string
               "\\*" ""
               (replace-regexp-in-string "\\." "\\\\." file-ext))
              "$ "
              search-term)
    (concat (cdr (assoc 'ag projectile-files-with-string-commands))
            search-term)))

(defun projectile--ack-construct-command (search-term &optional file-ext)
  "Construct Ack option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat "ack -g '"
              (replace-regexp-in-string
               "\\*" ""
               (replace-regexp-in-string "\\." "\\\\." file-ext))
              "$' | "
              (cdr (assoc 'ack projectile-files-with-string-commands))
              "-x "
              search-term)
    (concat (cdr (assoc 'ack projectile-files-with-string-commands))
            search-term)))

(defun projectile--git-grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (cdr (assoc 'git projectile-files-with-string-commands))
              search-term
              "  -- '"
              file-ext
              "'")
    (concat (cdr (assoc 'git projectile-files-with-string-commands))
            search-term)))

(defun projectile--grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (format (cdr (assoc 'grep projectile-files-with-string-commands))
                      search-term)
              " --include '"
              file-ext
              "'")
    (format (cdr (assoc 'grep projectile-files-with-string-commands))
            search-term)))

(defun projectile-files-with-string (string directory &optional file-ext)
  "Return a list of all files containing STRING in DIRECTORY.

Tries to use rg, ag, ack, git-grep, and grep in that order.  If those
are impossible (for instance on Windows), returns a list of all
files in the project."
  (if (projectile-unixy-system-p)
      (let* ((search-term (shell-quote-argument string))
             (cmd (cond ((executable-find "rg")
                         (projectile--rg-construct-command search-term file-ext))
                        ((executable-find "ag")
                         (projectile--ag-construct-command search-term file-ext))
                        ((executable-find "ack")
                         (projectile--ack-construct-command search-term file-ext))
                        ((and (executable-find "git")
                              (eq (projectile-project-vcs) 'git))
                         (projectile--git-grep-construct-command search-term file-ext))
                        (t
                         (projectile--grep-construct-command search-term file-ext)))))
        (projectile-files-from-cmd cmd directory))
    ;; we have to reject directories as a workaround to work with git submodules
    (cl-remove-if
     #'file-directory-p
     (mapcar #'(lambda (file) (expand-file-name file directory))
             (projectile-dir-files directory)))))

;;;###autoload
(defun projectile-replace (&optional arg)
  "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory and file name patterns
on which to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-acquire-root)))
         (file-ext (if arg
                       (if (fboundp #'helm-grep-get-file-extensions)
                           (car (helm-grep-get-file-extensions (list directory)))
                         (read-string
                          (projectile-prepend-project-name
                           "With file extension (empty string means all files): ")))
                     nil))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (files (projectile-files-with-string old-text directory file-ext)))
    (if (fboundp #'fileloop-continue)
        ;; Emacs 27+
        (progn (fileloop-initialize-replace old-text new-text files 'default)
               (fileloop-continue))
      ;; Emacs 25 and 26
      ;;
      ;; Adapted from `tags-query-replace' for literal strings (not regexp)
      (with-no-warnings
        (setq tags-loop-scan
              `(let ,(unless (equal old-text (downcase old-text))
                       '((case-fold-search nil)))
                 (if (search-forward ',old-text nil t)
                     ;; When we find a match, move back to
                     ;; the beginning of it so
                     ;; perform-replace will see it.
                     (goto-char (match-beginning 0)))))
        (setq tags-loop-operate
              `(perform-replace ',old-text ',new-text t nil nil
                                nil multi-query-replace-map))
        (tags-loop-continue (or (cons 'list files) t))))))

;;;###autoload
(defun projectile-replace-regexp (&optional arg)
  "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace regexp in directory: "))
                      (projectile-acquire-root)))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (cl-remove-if
           #'file-directory-p
           (mapcar #'(lambda (file) (expand-file-name file directory))
                   (projectile-dir-files directory)))))
    ;; FIXME: Probably would fail on Emacs 27+, fourth argument is gone.
    (with-no-warnings (tags-query-replace old-text new-text nil (cons 'list files)))))

;;;###autoload
(defun projectile-kill-buffers ()
  "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'."
  (interactive)
  (let* ((project (projectile-acquire-root))
         (project-name (projectile-project-name project))
         (buffers (projectile-project-buffers project)))
    (when (yes-or-no-p
           (format "Are you sure you want to kill %s buffers for '%s'? "
                   (length buffers) project-name))
      (dolist (buffer buffers)
        (when (and
               ;; we take care not to kill indirect buffers directly
               ;; as we might encounter them after their base buffers are killed
               (not (buffer-base-buffer buffer))
               (if (functionp projectile-kill-buffers-filter)
                   (funcall projectile-kill-buffers-filter buffer)
                 (pcase projectile-kill-buffers-filter
                   ('kill-all t)
                   ('kill-only-files (buffer-file-name buffer))
                   (_ (user-error "Invalid projectile-kill-buffers-filter value: %S" projectile-kill-buffers-filter)))))
          (kill-buffer buffer))))))

;;;###autoload
(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (let* ((project (projectile-acquire-root))
         (project-name (projectile-project-name project))
         (modified-buffers (cl-remove-if-not (lambda (buf)
                                               (and (buffer-file-name buf)
                                                    (buffer-modified-p buf)))
                                             (projectile-project-buffers project))))
    (if (null modified-buffers)
        (message "[%s] No buffers need saving" project-name)
      (dolist (buf modified-buffers)
        (with-current-buffer buf
          (save-buffer)))
      (message "[%s] Saved %d buffers" project-name (length modified-buffers)))))

;;;###autoload
(defun projectile-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (dired (projectile-acquire-root)))

;;;###autoload
(defun projectile-dired-other-window ()
  "Open `dired'  at the root of the project in another window."
  (interactive)
  (dired-other-window (projectile-acquire-root)))

;;;###autoload
(defun projectile-dired-other-frame ()
  "Open `dired' at the root of the project in another frame."
  (interactive)
  (dired-other-frame (projectile-acquire-root)))

;;;###autoload
(defun projectile-vc (&optional project-root)
  "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open."
  (interactive (and current-prefix-arg
                    (list
                     (projectile-completing-read
                      "Open project VC in: "
                      projectile-known-projects))))
  (unless project-root
    (setq project-root (projectile-acquire-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (cl-case vcs
      (git
       (cond ((fboundp 'magit-status-internal)
              (magit-status-internal project-root))
             ((fboundp 'magit-status)
              (with-no-warnings (magit-status project-root)))
             (t
              (vc-dir project-root))))
      (hg
       (if (fboundp 'monky-status)
           (monky-status project-root)
         (vc-dir project-root)))
      (t (vc-dir project-root)))))

;;;###autoload
(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root
                  (projectile-completing-read
                   "Recently visited files: "
                   (projectile-recentf-files))))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let ((project-root (projectile-acquire-root)))
         (mapcar
          (lambda (f) (file-relative-name f project-root))
          (cl-remove-if-not
           (lambda (f) (string-prefix-p project-root (expand-file-name f)))
           recentf-list)))))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-configure-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last configure command used on them.")

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")

(defvar projectile-install-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last install command used on them.")

(defvar projectile-package-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last package command used on them.")

(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defvar projectile-run-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defvar projectile-project-enable-cmd-caching t
  "Enables command caching for the project.  Set to nil to disable.
Should be set via .dir-locals.el.")

(defun projectile--cache-project-commands-p ()
  "Whether to cache project commands."
  (with-temp-buffer
    (hack-dir-local-variables-non-file-buffer)
    projectile-project-enable-cmd-caching))

(defvar projectile-project-configure-cmd nil
  "The command to use with `projectile-configure-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-compilation-cmd nil
  "The command to use with `projectile-compile-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-compilation-dir nil
  "The directory to use with `projectile-compile-project'.
The directory path is relative to the project root.
Should be set via .dir-locals.el.")

(defvar projectile-project-test-cmd nil
  "The command to use with `projectile-test-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-install-cmd nil
  "The command to use with `projectile-install-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-package-cmd nil
  "The command to use with `projectile-package-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-run-cmd nil
  "The command to use with `projectile-run-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defun projectile-default-generic-command (project-type command-type)
  "Generic retrieval of COMMAND-TYPEs default cmd-value for PROJECT-TYPE.

If found, checks if value is symbol or string.  In case of symbol
resolves to function `funcall's.  Return value of function MUST
be string to be executed as command."
  (let ((command (plist-get (alist-get project-type projectile-project-types) command-type)))
    (cond
     ((not command) nil)
     ((stringp command) command)
     ((functionp command)
      (if (fboundp command)
          (funcall (symbol-function command))))
     (t
      (error "The value for: %s in project-type: %s was neither a function nor a string" command-type project-type)))))

(defun projectile-default-configure-command (project-type)
  "Retrieve default configure command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'configure-command))

(defun projectile-default-compilation-command (project-type)
  "Retrieve default compilation command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'compile-command))

(defun projectile-default-compilation-dir (project-type)
  "Retrieve default compilation directory for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'compilation-dir))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'test-command))

(defun projectile-default-install-command (project-type)
  "Retrieve default install command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'install-command))

(defun projectile-default-package-command (project-type)
  "Retrieve default package command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'package-command))

(defun projectile-default-run-command (project-type)
  "Retrieve default run command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'run-command))

(defun projectile-configure-command (compile-dir)
  "Retrieve the configure command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-configure-cmd-map' for the last
configure command that was invoked on the project

- then we check for `projectile-project-configure-cmd' supplied
via .dir-locals.el

- finally we check for the default configure command for a
project of that type"
  (or (gethash compile-dir projectile-configure-cmd-map)
      projectile-project-configure-cmd
      (let ((cmd-format-string (projectile-default-configure-command (projectile-project-type))))
        (when cmd-format-string
          (format cmd-format-string (projectile-project-root) compile-dir)))))

(defun projectile-compilation-buffer-name (compilation-mode)
  "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the
compilation buffer."
  (concat "*" (downcase compilation-mode) "*"
          (if (projectile-project-p) (concat "<" (projectile-project-name) ">") "")))

(defun projectile-current-project-buffer-p ()
  "Meant to be used for `compilation-save-buffers-predicate`.
This indicates whether the current buffer is in the same project as the current
window (including returning true if neither is in a project)."
  (let ((root (with-current-buffer (window-buffer) (projectile-project-root))))
    (or (not root)
        (projectile-project-buffer-p (current-buffer) root))))

(defun projectile-compilation-command (compile-dir)
  "Retrieve the compilation command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-compilation-cmd-map' for the last
compile command that was invoked on the project

- then we check for `projectile-project-compilation-cmd' supplied
via .dir-locals.el

- finally we check for the default compilation command for a
project of that type"
  (or (gethash compile-dir projectile-compilation-cmd-map)
      projectile-project-compilation-cmd
      (projectile-default-compilation-command (projectile-project-type))))

(defun projectile-test-command (compile-dir)
  "Retrieve the test command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-test-cmd-map' for the last
test command that was invoked on the project

- then we check for `projectile-project-test-cmd' supplied
via .dir-locals.el

- finally we check for the default test command for a
project of that type"
  (or (gethash compile-dir projectile-test-cmd-map)
      projectile-project-test-cmd
      (projectile-default-test-command (projectile-project-type))))

(defun projectile-install-command (compile-dir)
  "Retrieve the install command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-install-cmd-map' for the last
install command that was invoked on the project

- then we check for `projectile-project-install-cmd' supplied
via .dir-locals.el

- finally we check for the default install command for a
project of that type"
  (or (gethash compile-dir projectile-install-cmd-map)
      projectile-project-install-cmd
      (projectile-default-install-command (projectile-project-type))))

(defun projectile-package-command (compile-dir)
  "Retrieve the package command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-packgage-cmd-map' for the last
install command that was invoked on the project

- then we check for `projectile-project-package-cmd' supplied
via .dir-locals.el

- finally we check for the default package command for a
project of that type"
  (or (gethash compile-dir projectile-package-cmd-map)
      projectile-project-package-cmd
      (projectile-default-package-command (projectile-project-type))))

(defun projectile-run-command (compile-dir)
  "Retrieve the run command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-run-cmd-map' for the last
run command that was invoked on the project

- then we check for `projectile-project-run-cmd' supplied
via .dir-locals.el

- finally we check for the default run command for a
project of that type"
  (or (gethash compile-dir projectile-run-cmd-map)
      projectile-project-run-cmd
      (projectile-default-run-command (projectile-project-type))))

(defun projectile-read-command (prompt command)
  "Adapted from the function `compilation-read-command'."
  (let ((compile-history
         ;; fetch the command history for the current project
         (ring-elements (projectile--get-command-history (projectile-acquire-root)))))
    (read-shell-command prompt command
                        (if (equal (car compile-history) command)
                            '(compile-history . 1)
                          'compile-history))))

(defun projectile-compilation-dir ()
  "Retrieve the compilation directory for this project."
  (let* ((type (projectile-project-type))
         (directory (or projectile-project-compilation-dir
                        (projectile-default-compilation-dir type))))
    (if directory
        (file-truename
         (concat (file-name-as-directory (projectile-project-root))
                 (file-name-as-directory directory)))
      (projectile-project-root))))

(defun projectile-maybe-read-command (arg default-cmd prompt)
  "Prompt user for command unless DEFAULT-CMD is an Elisp function."
  (if (and (or (stringp default-cmd) (null default-cmd))
           (or compilation-read-command arg))
      (projectile-read-command prompt default-cmd)
    default-cmd))

(defun projectile-run-compilation (cmd &optional use-comint-mode)
  "Run external or Elisp compilation command CMD."
  (if (functionp cmd)
      (funcall cmd)
    (compile cmd use-comint-mode)))

(defvar projectile-project-command-history (make-hash-table :test 'equal)
  "The history of last executed project commands, per project.

Projects are indexed by their project-root value.")

(defun projectile--get-command-history (project-root)
  (or (gethash project-root projectile-project-command-history)
      (puthash project-root
               (make-ring 16)
               projectile-project-command-history)))

(cl-defun projectile--run-project-cmd
    (command command-map &key show-prompt prompt-prefix save-buffers use-comint-mode)
  "Run a project COMMAND, typically a test- or compile command.

Cache the COMMAND for later use inside the hash-table COMMAND-MAP.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
by setting SHOW-PROMPT.  The prompt will be prefixed with PROMPT-PREFIX.

If SAVE-BUFFERS is non-nil save all projectile buffers before
running the command.

The command actually run is returned."
  (let* ((project-root (projectile-project-root))
         (default-directory (projectile-compilation-dir))
         (command (projectile-maybe-read-command show-prompt
                                                 command
                                                 prompt-prefix))
         compilation-buffer-name-function
         compilation-save-buffers-predicate)
    (when command-map
      (puthash default-directory command command-map)
      (let ((hist (projectile--get-command-history project-root)))
        (unless (string= (car-safe (ring-elements hist)) command)
          (ring-insert hist command))))
    (when save-buffers
      (save-some-buffers (not compilation-ask-about-save)
                         (lambda ()
                           (projectile-project-buffer-p (current-buffer)
                                                        project-root))))
    (when projectile-per-project-compilation-buffer
      (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
      (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p))
    (unless (file-directory-p default-directory)
      (mkdir default-directory))
    (projectile-run-compilation command use-comint-mode)
    command))

(defcustom projectile-configure-use-comint-mode nil
  "Make the output buffer of `projectile-configure-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-compile-use-comint-mode nil
  "Make the output buffer of `projectile-compile-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-test-use-comint-mode nil
  "Make the output buffer of `projectile-test-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-install-use-comint-mode nil
  "Make the output buffer of `projectile-install-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-package-use-comint-mode nil
  "Make the output buffer of `projectile-package-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-run-use-comint-mode nil
  "Make the output buffer of `projectile-run-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

;;;###autoload
(defun projectile-configure-project (arg)
  "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-configure-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-configure-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Configure command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-configure-use-comint-mode)))

;;;###autoload
(defun projectile-compile-project (arg)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.  Per project default command can be set through
`projectile-project-compilation-cmd'."
  (interactive "P")
  (let ((command (projectile-compilation-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-compilation-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-compile-use-comint-mode)))

;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-test-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-test-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Test command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-test-use-comint-mode)))

;;;###autoload
(defun projectile-install-project (arg)
  "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-install-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-install-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Install command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-install-use-comint-mode)))

;;;###autoload
(defun projectile-package-project (arg)
  "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-package-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-package-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Package command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-package-use-comint-mode)))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-run-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-run-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Run command: "
                                 :use-comint-mode projectile-run-use-comint-mode)))

;;;###autoload
(defun projectile-repeat-last-command (show-prompt)
  "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited."
  (interactive "P")
  (let* ((project-root (projectile-acquire-root))
         (command-history (projectile--get-command-history project-root))
         (command (car-safe (ring-elements command-history)))
         (compilation-read-command show-prompt)
         executed-command)
    (unless command
      (user-error "No command has been run yet for this project"))
    (setq executed-command
          (projectile--run-project-cmd command
                                       nil
                                       :save-buffers t
                                       :prompt-prefix "Execute command: "))
    (unless (string= command executed-command)
      (ring-insert command-history executed-command))))

(defun compilation-find-file-projectile-find-compilation-buffer (orig-fun marker filename directory &rest formats)
  "Advice around compilation-find-file.
We enhance its functionality by appending the current project's directories
to its search path. This way when filenames in compilation buffers can't be
found by compilation's normal logic they are searched for in project
directories."
  (let* ((root (projectile-project-root))
         (compilation-search-path
          (if (projectile-project-p)
              (append compilation-search-path (list root)
                      (mapcar (lambda (f) (expand-file-name f root))
                              (projectile-current-project-dirs)))
            compilation-search-path)))
    (apply orig-fun `(,marker ,filename ,directory ,@formats))))

(defun projectile-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (delete-dups
   (delq nil
         (mapcar (lambda (buffer)
                   (with-current-buffer buffer
                     (when-let ((project-root (projectile-project-root)))
                       (when (projectile-project-buffer-p buffer project-root)
                         (abbreviate-file-name project-root)))))
                 (buffer-list)))))

(defun projectile--remove-current-project (projects)
  "Remove the current project (if any) from the list of PROJECTS."
  (if-let ((project (projectile-project-root)))
      (projectile-difference projects
                             (list (abbreviate-file-name project)))
    projects))

(defun projectile--move-current-project-to-end (projects)
  "Move current project (if any) to the end of list in the list of PROJECTS."
  (if-let ((project (projectile-project-root)))
      (append
       (projectile--remove-current-project projects)
       (list (abbreviate-file-name project)))
    projects))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects."
  (pcase projectile-current-project-on-switch
    ('remove (projectile--remove-current-project projectile-known-projects))
    ('move-to-end (projectile--move-current-project-to-end projectile-known-projects))
    ('keep projectile-known-projects)))

(defun projectile-relevant-open-projects ()
  "Return a list of open projects."
  (let ((open-projects (projectile-open-projects)))
    (pcase projectile-current-project-on-switch
      ('remove (projectile--remove-current-project open-projects))
      ('move-to-end (projectile--move-current-project-to-end open-projects))
      ('keep open-projects))))

;;;###autoload
(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg)))
      (user-error "There are no known projects"))))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-open-projects)))
    (if projects
        (projectile-completing-read
         "Switch to open project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg)))
      (user-error "There are no open projects"))))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  ;; let's make sure that the target directory exists and is actually a project
  ;; we ignore remote folders, as the check breaks for TRAMP unless already connected
  (unless (or (file-remote-p project-to-switch) (projectile-project-p project-to-switch))
    (projectile-remove-known-project project-to-switch)
    (error "Directory %s is not a project" project-to-switch))
  (let ((switch-project-action (if arg
                                   'projectile-commander
                                 projectile-switch-project-action)))
    (run-hooks 'projectile-before-switch-project-hook)
    (let* ((default-directory project-to-switch)
           (switched-buffer
            ;; use a temporary buffer to load PROJECT-TO-SWITCH's dir-locals
            ;; before calling SWITCH-PROJECT-ACTION
            (with-temp-buffer
              (hack-dir-local-variables-non-file-buffer)
              ;; Normally the project name is determined from the current
              ;; buffer. However, when we're switching projects, we want to
              ;; show the name of the project being switched to, rather than
              ;; the current project, in the minibuffer. This is a simple hack
              ;; to tell the `projectile-project-name' function to ignore the
              ;; current buffer and the caching mechanism, and just return the
              ;; value of the `projectile-project-name' variable.
              (let ((projectile-project-name (funcall projectile-project-name-function
                                                      project-to-switch)))
                (funcall switch-project-action)
                (current-buffer)))))
      ;; If switch-project-action switched buffers then with-temp-buffer will
      ;; have lost that change, so switch back to the correct buffer.
      (when (buffer-live-p switched-buffer)
        (switch-to-buffer switched-buffer)))
    (run-hooks 'projectile-after-switch-project-hook)))

;;;###autoload
(defun projectile-find-file-in-directory (&optional directory)
  "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in."
  (interactive "DFind file in directory: ")
  (unless (projectile--directory-p directory)
    (user-error "Directory %S does not exist" directory))
  (let ((default-directory directory))
    (if (projectile-project-p)
        ;; target directory is in a project
        (let ((file (projectile-completing-read "Find file: "
                                                (projectile-dir-files directory))))
          (find-file (expand-file-name file directory))
          (run-hooks 'projectile-find-file-hook))
      ;; target directory is not in a project
      (projectile-find-file))))

(defun projectile-all-project-files ()
  "Get a list of all files in all projects."
  (cl-mapcan
   (lambda (project)
     (when (file-exists-p project)
       (mapcar (lambda (file)
                 (expand-file-name file project))
               (projectile-project-files project))))
   projectile-known-projects))

;;;###autoload
(defun projectile-find-file-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (find-file (projectile-completing-read "Find file in projects: " (projectile-all-project-files))))

(defun projectile-keep-project-p (project)
  "Determine whether we should cleanup (remove) PROJECT or not.

It handles the case of remote projects as well.
See `projectile--cleanup-known-projects'."
  ;; Taken from from `recentf-keep-default-predicate'
  (cond
   ((file-remote-p project nil t) (file-readable-p project))
   ((file-remote-p project))
   ((file-readable-p project))))

(defun projectile--cleanup-known-projects ()
  "Remove known projects that don't exist anymore.
Return a list of projects removed."
  (projectile-merge-known-projects)
  (let ((projects-kept (cl-remove-if-not #'projectile-keep-project-p projectile-known-projects))
        (projects-removed (cl-remove-if #'projectile-keep-project-p projectile-known-projects)))
    (setq projectile-known-projects projects-kept)
    (projectile-merge-known-projects)
    projects-removed))

;;;###autoload
(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (if-let ((projects-removed (projectile--cleanup-known-projects)))
      (message "Projects removed: %s"
               (mapconcat #'identity projects-removed ", "))
    (message "No projects needed to be removed.")))

;;;###autoload
(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

;;;###autoload
(defun projectile-reset-known-projects ()
  "Clear known projects and rediscover."
  (interactive)
  (projectile-clear-known-projects)
  (projectile-discover-projects-in-search-path))

;;;###autoload
(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read
                      "Remove from known projects: " projectile-known-projects
                      :action 'projectile-remove-known-project)))
  (unless (called-interactively-p 'any)
    (setq projectile-known-projects
          (cl-remove-if
           (lambda (proj) (string= project proj))
           projectile-known-projects))
    (projectile-merge-known-projects)
    (when projectile-verbose
      (message "Project %s removed from the list of known projects." project))))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-acquire-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be save in `projectile-known-projects'."
  (mapcar #'file-truename projectile-ignored-projects))

(defun projectile-ignored-project-p (project-root)
  "Return t if PROJECT-ROOT should not be added to `projectile-known-projects'."
  (or (member project-root (projectile-ignored-projects))
      (and (functionp projectile-ignored-project-function)
           (funcall projectile-ignored-project-function project-root))))

;;;###autoload
(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: ")))
  (unless (projectile-ignored-project-p project-root)
    (push (file-name-as-directory (abbreviate-file-name project-root)) projectile-known-projects)
    (delete-dups projectile-known-projects)
    (projectile-merge-known-projects)))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (setq projectile-known-projects
        (projectile-unserialize projectile-known-projects-file))
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

(defun projectile-save-known-projects ()
  "Save PROJECTILE-KNOWN-PROJECTS to PROJECTILE-KNOWN-PROJECTS-FILE."
  (projectile-serialize projectile-known-projects
                        projectile-known-projects-file)
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

(defun projectile-merge-known-projects ()
  "Merge any change from `projectile-known-projects-file' and save to disk.

This enables multiple Emacs processes to make changes without
overwriting each other's changes."
  (let* ((known-now projectile-known-projects)
         (known-on-last-sync projectile-known-projects-on-file)
         (known-on-file
          (projectile-unserialize projectile-known-projects-file))
         (removed-after-sync (projectile-difference known-on-last-sync known-now))
         (removed-in-other-process
          (projectile-difference known-on-last-sync known-on-file))
         (result (delete-dups
                  (projectile-difference
                   (append known-now known-on-file)
                   (append removed-after-sync removed-in-other-process)))))
    (setq projectile-known-projects result)
    (projectile-save-known-projects)))


;;; IBuffer integration
(define-ibuffer-filter projectile-files
    "Show Ibuffer with all buffers in the current project."
  (:reader (read-directory-name "Project root: " (projectile-project-root))
           :description nil)
  (with-current-buffer buf
    (let ((directory (file-name-as-directory (expand-file-name qualifier))))
      (and (projectile-project-buffer-p buf directory)
           (equal directory
                  (projectile-project-root))))))

(defun projectile-ibuffer-by-project (project-root)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let ((project-name (funcall projectile-project-name-function project-root)))
    (ibuffer nil (format "*%s Buffers*" project-name)
             (list (cons 'projectile-files project-root)))))

;;;###autoload
(defun projectile-ibuffer (prompt-for-project)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied."
  (interactive "P")
  (let ((project-root (if prompt-for-project
                          (projectile-completing-read
                           "Project name: "
                           (projectile-relevant-known-projects))
                        (projectile-acquire-root))))
    (projectile-ibuffer-by-project project-root)))


;;;; projectile-commander

(defconst projectile-commander-help-buffer "*Projectile Commander Help*")

(defvar projectile-commander-methods nil
  "List of file-selection methods for the `projectile-commander' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

;;;###autoload
(defun projectile-commander ()
  "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods."
  (interactive)
  (let* ((choices (mapcar #'car projectile-commander-methods))
         (prompt (concat "Select Projectile command [" choices "]: "))
         (ch (read-char-choice prompt choices))
         (fn (nth 2 (assq ch projectile-commander-methods))))
    (funcall fn)))

(defmacro def-projectile-commander-method (key description &rest body)
  "Define a new `projectile-commander' method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method.

BODY is a series of forms which are evaluated when the find
is chosen."
  (let ((method `(lambda ()
                   ,@body)))
    `(setq projectile-commander-methods
           (cl-sort (copy-sequence
                     (cons (list ,key ,description ,method)
                           (assq-delete-all ,key projectile-commander-methods)))
                    (lambda (a b) (< (car a) (car b)))))))

(def-projectile-commander-method ?? "Commander help buffer."
  (ignore-errors (kill-buffer projectile-commander-help-buffer))
  (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
    (insert "Projectile Commander Methods:\n\n")
    (dolist (met projectile-commander-methods)
      (insert (format "%c:\t%s\n" (car met) (cadr met))))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (projectile-commander))

(defun projectile-commander-bindings ()
  "Setup the keybindings for the Projectile Commander."
  (def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))

  (def-projectile-commander-method ?T
    "Find test file in project."
    (projectile-find-test-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (projectile-find-dir))

  (def-projectile-commander-method ?D
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?v
    "Open project root in vc-dir or magit."
    (projectile-vc))

  (def-projectile-commander-method ?V
    "Browse dirty projects"
    (projectile-browse-dirty-projects))

  (def-projectile-commander-method ?r
    "Replace a string in the project."
    (projectile-replace))

  (def-projectile-commander-method ?R
    "Regenerate the project's [e|g]tags."
    (projectile-regenerate-tags))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?p
    "Run ripgrep on project."
    (call-interactively #'projectile-ripgrep))

  (def-projectile-commander-method ?a
    "Run ag on project."
    (call-interactively #'projectile-ag))

  (def-projectile-commander-method ?s
    "Switch project."
    (projectile-switch-project))

  (def-projectile-commander-method ?o
    "Run multi-occur on project buffers."
    (projectile-multi-occur))

  (def-projectile-commander-method ?j
    "Find tag in project."
    (projectile-find-tag))

  (def-projectile-commander-method ?k
    "Kill all project buffers."
    (projectile-kill-buffers))

  (def-projectile-commander-method ?e
    "Find recently visited file in project."
    (projectile-recentf)))


;;; Dirty (modified) project check related functionality
(defun projectile-check-vcs-status (&optional project-path)
  "Check the status of the current project.
If PROJECT-PATH is a project, check this one instead."
  (let ((project-path (or project-path (projectile-acquire-root)))
        (project-status nil))
    (save-excursion
      (vc-dir project-path)
      ;; wait until vc-dir is done
      (while (vc-dir-busy) (sleep-for 0 100))
      ;; check for status
      (save-excursion
        (save-match-data
          (dolist (check projectile-vcs-dirty-state)
            (goto-char (point-min))
            (when (search-forward check nil t)
              (setq project-status (cons check project-status))))))
      (kill-buffer)
      project-status)))

(defvar projectile-cached-dirty-projects-status nil
  "Cache of the last dirty projects check.")

(defun projectile-check-vcs-status-of-known-projects ()
  "Return the list of dirty projects.
The list is composed of sublists~: (project-path, project-status).
Raise an error if their is no dirty project."
  (save-window-excursion
    (message "Checking for modifications in known projects...")
    (let ((projects projectile-known-projects)
          (status ()))
      (dolist (project projects)
        (when (and (projectile-keep-project-p project) (not (string= 'none (projectile-project-vcs project))))
          (let ((tmp-status (projectile-check-vcs-status project)))
            (when tmp-status
              (setq status (cons (list project tmp-status) status))))))
      (when (= (length status) 0)
        (message "No dirty projects have been found"))
      (setq projectile-cached-dirty-projects-status status)
      status)))

;;;###autoload
(defun projectile-browse-dirty-projects (&optional cached)
  "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list."
  (interactive "P")
  (let ((status (if (and cached projectile-cached-dirty-projects-status)
                    projectile-cached-dirty-projects-status
                  (projectile-check-vcs-status-of-known-projects)))
        (mod-proj nil))
    (while (not (= (length status) 0))
      (setq mod-proj (cons (car (pop status)) mod-proj)))
    (projectile-completing-read "Select project: " mod-proj
                                :action 'projectile-vc)))


;;; Find next/previous project buffer
(defun projectile--repeat-until-project-buffer (orig-fun &rest args)
  "Repeat ORIG-FUN with ARGS until the current buffer is a project buffer."
  (if (projectile-project-root)
      (let* ((other-project-buffers (make-hash-table :test 'eq))
             (projectile-project-buffers (projectile-project-buffers))
             (max-iterations (length (buffer-list)))
             (counter 0))
        (dolist (buffer projectile-project-buffers)
          (unless (eq buffer (current-buffer))
            (puthash buffer t other-project-buffers)))
        (when (cdr-safe projectile-project-buffers)
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-project-buffers)))
            (apply orig-fun args)
            (cl-incf counter))))
    (apply orig-fun args)))

(defun projectile-next-project-buffer ()
  "In selected window switch to the next project buffer.

If the current buffer does not belong to a project, call `next-buffer'."
  (interactive)
  (projectile--repeat-until-project-buffer #'next-buffer))

(defun projectile-previous-project-buffer ()
  "In selected window switch to the previous project buffer.

If the current buffer does not belong to a project, call `previous-buffer'."
  (interactive)
  (projectile--repeat-until-project-buffer #'previous-buffer))


;;; Editing a project's .dir-locals
(defun projectile-read-variable ()
  "Prompt for a variable and return its name."
  (completing-read "Variable: "
                   obarray
                   (lambda (v)
                     (and (boundp v) (not (keywordp v))))
                   t))

(define-skeleton projectile-skel-variable-cons
  "Insert a variable-name and a value in a cons-cell."
  "Value: "
  "("
  (projectile-read-variable)
  " . "
  str
  ")")

(define-skeleton projectile-skel-dir-locals
  "Insert a .dir-locals.el template."
  nil
  "((nil . ("
  ("" '(projectile-skel-variable-cons) \n)
  resume:
  ")))")

;;;###autoload
(defun projectile-edit-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (let ((file (expand-file-name ".dir-locals.el" (projectile-acquire-root))))
    (find-file file)
    (when (not (file-exists-p file))
      (unwind-protect
          (projectile-skel-dir-locals)
        (save-buffer)))))


;;; Projectile Minor mode
(define-obsolete-variable-alias 'projectile-mode-line-lighter 'projectile-mode-line-prefix "0.12.0")
(defcustom projectile-mode-line-prefix
  " Projectile"
  "Mode line lighter prefix for Projectile.
It's used by `projectile-default-mode-line'
when using dynamic mode line lighter and is the only
thing shown in the mode line otherwise."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-show-menu t
  "Controls whether to display Projectile's menu."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.6.0"))

(defvar-local projectile--mode-line projectile-mode-line-prefix)

(defun projectile-default-mode-line ()
  "Report project name and type in the modeline."
  (let ((project-name (projectile-project-name))
        (project-type (projectile-project-type)))
    (format "%s[%s%s]"
            projectile-mode-line-prefix
            (or project-name "-")
            (if project-type
                (format ":%s" project-type)
              ""))))

(defun projectile-update-mode-line ()
  "Update the Projectile mode-line."
  (let ((mode-line (funcall projectile-mode-line-function)))
    (setq projectile--mode-line mode-line))
  (force-mode-line-update))

(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D") #'projectile-dired-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D") #'projectile-dired-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
    (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "?") #'projectile-find-references)
    (define-key map (kbd "a") #'projectile-find-other-file)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "D") #'projectile-dired)
    (define-key map (kbd "e") #'projectile-recentf)
    (define-key map (kbd "E") #'projectile-edit-dir-locals)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "g") #'projectile-find-file-dwim)
    (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
    (define-key map (kbd "i") #'projectile-invalidate-cache)
    (define-key map (kbd "I") #'projectile-ibuffer)
    (define-key map (kbd "j") #'projectile-find-tag)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "l") #'projectile-find-file-in-directory)
    (define-key map (kbd "m") #'projectile-commander)
    (define-key map (kbd "o") #'projectile-multi-occur)
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "q") #'projectile-switch-open-project)
    (define-key map (kbd "r") #'projectile-replace)
    (define-key map (kbd "R") #'projectile-regenerate-tags)
    (define-key map (kbd "s g") #'projectile-grep)
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd "s s") #'projectile-ag)
    (define-key map (kbd "s x") #'projectile-find-references)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "v") #'projectile-vc)
    (define-key map (kbd "V") #'projectile-browse-dirty-projects)
    ;; project lifecycle external commands
    ;; TODO: Bundle those under some prefix key
    (define-key map (kbd "C") #'projectile-configure-project)
    (define-key map (kbd "c") #'projectile-compile-project)
    (define-key map (kbd "K") #'projectile-package-project)
    (define-key map (kbd "L") #'projectile-install-project)
    (define-key map (kbd "P") #'projectile-test-project)
    (define-key map (kbd "u") #'projectile-run-project)
    ;; integration with utilities
    (define-key map (kbd "x e") #'projectile-run-eshell)
    (define-key map (kbd "x i") #'projectile-run-ielm)
    (define-key map (kbd "x t") #'projectile-run-term)
    (define-key map (kbd "x s") #'projectile-run-shell)
    (define-key map (kbd "x g") #'projectile-run-gdb)
    (define-key map (kbd "x v") #'projectile-run-vterm)
    (define-key map (kbd "x 4 v") #'projectile-run-vterm-other-window)
    ;; misc
    (define-key map (kbd "z") #'projectile-cache-current-file)
    (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
    (define-key map (kbd "<right>") #'projectile-next-project-buffer)
    (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'.")
(fset 'projectile-command-map projectile-command-map)

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (when projectile-keymap-prefix
      (define-key map projectile-keymap-prefix 'projectile-command-map))
    (easy-menu-define projectile-mode-menu map
      "Menu for Projectile"
      '("Projectile" :visible projectile-show-menu
        ("Find..."
         ["Find file" projectile-find-file]
         ["Find file in known projects" projectile-find-file-in-known-projects]
         ["Find test file" projectile-find-test-file]
         ["Find directory" projectile-find-dir]
         ["Find file in directory" projectile-find-file-in-directory]
         ["Find other file" projectile-find-other-file]
         ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test])
        ("Buffers"
         ["Switch to buffer" projectile-switch-to-buffer]
         ["Kill project buffers" projectile-kill-buffers]
         ["Save project buffers" projectile-save-project-buffers]
         ["Recent files" projectile-recentf]
         ["Previous buffer" projectile-previous-project-buffer]
         ["Next buffer" projectile-next-project-buffer])
        ("Projects"
         ["Switch to project" projectile-switch-project]
         ["Switch to open project" projectile-switch-open-project]
         "--"
         ["Discover projects in directory" projectile-discover-projects-in-directory]
         ["Discover projects in search path" projectile-discover-projects-in-search-path]
         ["Clear known projects" projectile-clear-known-projects]
         ["Reset known projects" projectile-reset-known-projects]
         "--"
         ["Open project in dired" projectile-dired]
         "--"
         ["Browse dirty projects" projectile-browse-dirty-projects]
         "--"
         ["Cache current file" projectile-cache-current-file]
         ["Invalidate cache" projectile-invalidate-cache]
         ["Regenerate [e|g]tags" projectile-regenerate-tags]
         "--"
         ["Toggle project wide read-only" projectile-toggle-project-read-only]
         ["Edit .dir-locals.el" projectile-edit-dir-locals]
         ["Project info" projectile-project-info])
        ("Search"
         ["Search with grep" projectile-grep]
         ["Search with ag" projectile-ag]
         ["Search with ripgrep" projectile-ripgrep]
         ["Replace in project" projectile-replace]
         ["Multi-occur in project" projectile-multi-occur]
         ["Find references in project" projectile-find-references])
        ("Run..."
         ["Run shell" projectile-run-shell]
         ["Run eshell" projectile-run-eshell]
         ["Run ielm" projectile-run-ielm]
         ["Run term" projectile-run-term]
         ["Run vterm" projectile-run-vterm]
         "--"
         ["Run GDB" projectile-run-gdb])
        ("Build"
         ["Configure project" projectile-configure-project]
         ["Compile project" projectile-compile-project]
         ["Test project" projectile-test-project]
         ["Install project" projectile-install-project]
         ["Package project" projectile-package-project]
         ["Run project" projectile-run-project]
         "--"
         ["Repeat last build command" projectile-repeat-last-command])
        "--"
        ["About" projectile-version]))
    map)
  "Keymap for Projectile mode.")

(defun projectile-find-file-hook-function ()
  "Called by `find-file-hook' when `projectile-mode' is on.

The function does pretty much nothing when triggered on remote files
as all the operations it normally performs are extremely slow over
tramp."
  (projectile-maybe-limit-project-file-buffers)
  (unless (file-remote-p default-directory)
    (when projectile-dynamic-mode-line
      (projectile-update-mode-line))
    (when projectile-auto-update-cache
      (projectile-cache-files-find-file-hook))
    (projectile-track-known-projects-find-file-hook)
    (projectile-visit-project-tags-table)))

(defun projectile-maybe-limit-project-file-buffers ()
  "Limit the opened file buffers for a project.

The function simply kills the last buffer, as it's normally called
when opening new files."
  (when projectile-max-file-buffer-count
    (let ((project-buffers (projectile-project-buffer-files)))
      (when (> (length project-buffers) projectile-max-file-buffer-count)
        (kill-buffer (car (last project-buffers)))))))

;;;; project.el integration
;;
;; Projectile will become the default provider for
;; project.el project and project files lookup when
;; projectile-mode is enabled.
;;
;; The integration can also be manually enabled like this:
;;
;; (add-hook 'project-find-functions #'project-projectile)
;;
;; See https://github.com/bbatsov/projectile/issues/1591 for
;; more details.

;; it's safe to require this directly, as it was added in Emacs 25.1
(require 'project)

;; Only define an override for project-root if the method exists.  For versions
;; before emacs 28, project.el provided project-roots instead of project.root.
(if (fboundp 'project-root)
    (cl-defmethod project-root ((project (head projectile)))
      (cdr project)))

(cl-defmethod project-files ((project (head projectile)) &optional _dirs)
  (let ((root (project-root project)))
    ;; Make paths absolute and ignore the optional dirs argument,
    ;; see https://github.com/bbatsov/projectile/issues/1591#issuecomment-896423965
    ;; That's needed because Projectile uses relative paths for project files
    ;; and project.el expects them to be absolute.
    ;; FIXME: That's probably going to be very slow in large projects.
    (mapcar (lambda (f)
              (concat root f))
            (projectile-project-files root))))

(defun project-projectile (dir)
  "Return Projectile project of form ('projectile . root-dir) for DIR."
  (let ((root (projectile-project-root dir)))
    (when root
      (cons 'projectile root))))

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
  :lighter projectile--mode-line
  :keymap projectile-mode-map
  :group 'projectile
  :require 'projectile
  :global t
  (cond
   (projectile-mode
    ;; setup the commander bindings
    (projectile-commander-bindings)
    ;; initialize the projects cache if needed
    (unless projectile-projects-cache
      (setq projectile-projects-cache
            (or (projectile-unserialize projectile-cache-file)
                (make-hash-table :test 'equal))))
    (unless projectile-projects-cache-time
      (setq projectile-projects-cache-time
            (make-hash-table :test 'equal)))
    ;; load the known projects
    (projectile-load-known-projects)
    ;; update the list of known projects
    (projectile--cleanup-known-projects)
    (when projectile-auto-discover
      (projectile-discover-projects-in-search-path))
    (add-hook 'project-find-functions #'project-projectile)
    (add-hook 'find-file-hook 'projectile-find-file-hook-function)
    (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
    (advice-add 'compilation-find-file :around #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))
   (t
    (remove-hook 'project-find-functions #'project-projectile)
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
    (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
    (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))

;;; savehist-mode - When `savehist-mode' is t, projectile-project-command-history will be saved.
;; See https://github.com/bbatsov/projectile/issues/1637 for more details
(if (bound-and-true-p savehist-loaded)
    (add-to-list 'savehist-additional-variables 'projectile-project-command-history)
  (defvar savehist-additional-variables nil)
  (add-hook 'savehist-mode-hook
            (lambda()
              (add-to-list 'savehist-additional-variables 'projectile-project-command-history))))

;;;###autoload
(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0")

(provide 'projectile)

;;; projectile.el ends here
