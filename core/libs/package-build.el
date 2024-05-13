;;; package-build.el --- Tools for assembling a package archive  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2011-2024 Donald Ephraim Curtis
;; Copyright (C) 2012-2024 Steve Purcell
;; Copyright (C) 2016-2024 Jonas Bernoulli
;; Copyright (C) 2009 Phil Hagelberg

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;;     Steve Purcell <steve@sanityinc.com>
;;     Jonas Bernoulli <emacs.package-build@jonas.bernoulli.dev>
;;     Phil Hagelberg <technomancy@gmail.com>
;; Homepage: https://github.com/melpa/package-build
;; Keywords: maint tools

;; Package-Version: 4.0.0.50-git
;; Package-Requires: ((emacs "26.1") (compat "27.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from a set of recipes, which describe elisp
;; projects and repositories from which to get them.  The term "package"
;; here is used to mean a specific version of a project that is prepared
;; for download and installation.

;;; Code:

(require 'cl-lib)
(require 'compat nil t)
(require 'pcase)
(require 'subr-x)

(require 'package)
(require 'lisp-mnt)
(require 'json)

(require 'package-recipe)
(require 'package-build-badges)

;;; Options

(defvar package-build--melpa-base
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name (buffer-file-name))))))

(defgroup package-build nil
  "Tools for building package.el-compliant packages from upstream source code."
  :group 'development)

(defcustom package-build-working-dir
  (expand-file-name "working/" package-build--melpa-base)
  "Directory in which to keep checkouts."
  :group 'package-build
  :type 'string)

(defcustom package-build-archive-dir
  (expand-file-name "packages/" package-build--melpa-base)
  "Directory in which to keep compiled archives."
  :group 'package-build
  :type 'string)

(defcustom package-build-recipes-dir
  (expand-file-name "recipes/" package-build--melpa-base)
  "Directory containing recipe files."
  :group 'package-build
  :type 'string)

(defcustom package-build-verbose t
  "When non-nil, then print additional progress information."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-stable nil
  "Whether to build release or snapshot packages.

If nil, snapshot packages are build, otherwise release packages
are build.  `package-build-snapshot-version-functions' and/or
`package-build-release-version-functions' are used to determine
the appropriate version for each package and how the version
string is formatted."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-all-publishable (not package-build-stable)
  "Whether even packages that lack a release can be published.

This option is used to determine whether failure to come up with
a version string should be considered an error or not.

Currently this defaults to (not package-build-stable), but the
default is likely to be changed to just `t' in the future.  See
also the commit that added this option."
  :group 'package-build
  :type 'boolean
  :set-after '(package-build-stable))

(make-obsolete-variable 'package-build-get-version-function
                        'package-build-stable
                        "Package-Build 5.0.0")
(defvar package-build-get-version-function nil
  "This variable is obsolete and its value should be nil.
If this is non-nil, then it overrides
`package-build-release-version-functions' and
`package-build-snapshot-version-functions'.")

(defcustom package-build-release-version-functions
  (list #'package-build-tag-version)
  "Functions used to determine the current release of a package.

Each function is called in order, with the recipe object as
argument, until one returns non-nil.  The returned value must
have the form (COMMIT TIME VERSION), where COMMIT is the commit
chosen by the function, TIME is its committer date, and VERSION
is the version string chosen for COMMIT.

If obsolete `package-build-get-version-function' is non-nil,
then that overrides the value set here."
  :group 'package-build
  :type 'hook
  :options (list #'package-build-tag-version
                 #'package-build-header-version
                 #'package-build-pkg-version))

(defcustom package-build-snapshot-version-functions
  (list #'package-build-timestamp-version)
  "Function used to determine the current snapshot of a package.

Each function is called in order, with the recipe object as
argument, until one returns non-nil.  The returned value must
have the form (COMMIT TIME VERSION), where COMMIT is the commit
chosen by the function, TIME is its committer date, and VERSION
is the version string chosen for COMMIT.

Some of the functions that return snapshot versions, internally
use `package-build-release-version-functions' to determine the
current release, which they use as part of the returned VERSION.

If obsolete `package-build-get-version-function' is non-nil,
then that overrides the value set here."
  :group 'package-build
  :type 'hook
  :options (list #'package-build-release+count-version
                 #'package-build-release+timestamp-version
                 #'package-build-timestamp-version))

(defcustom package-build-predicate-function nil
  "Predicate used by `package-build-all' to determine which packages to build.
If non-nil, this function is called with the recipe object as
argument, and must return non-nil if the package is to be build.
If nil (the default), then all packages are build."
  :group 'package-build
  :type '(choice (const :tag "build all") function))

(defcustom package-build-build-function
  #'package-build--build-multi-file-package
  "Low-level function used to build a package.
By default a tarball is used for all packages, including those
consisting of a single file.  It this is nil, then single-file
packages are distributed without using tarballs."
  :group 'package-build
  :type '(choice (const :tag "use tarball for all packages"
                        package-build--build-multi-file-package)
                 (const :tag "only use tarball for multi-file packages" nil)
                 function))

;; NOTE that these hooks are still experimental.  Let me know if these
;; are potentially useful for you and whether any changes are required
;; to make them more appropriate for your usecase.
(defvar package-build-worktree-function #'package-recipe--working-tree)
(defvar package-build-early-worktree-function #'package-recipe--working-tree)
(defvar package-build-fetch-function #'package-build--fetch)
(defvar package-build-checkout-function #'package-build--checkout)
(defvar package-build-cleanup-function #'package-build--cleanup)

(defcustom package-build-timeout-executable "timeout"
  "Path to a GNU coreutils \"timeout\" command if available.
This must be a version which supports the \"-k\" option.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtimeout\"."
  :group 'package-build
  :type '(file :must-match t))

(defcustom package-build-timeout-secs nil
  "Wait this many seconds for external processes to complete.

If an external process takes longer than specified here to
complete, then it is terminated.  If nil, then no time limit is
applied.  This setting requires
`package-build-timeout-executable' to be set."
  :group 'package-build
  :type 'number)

(defcustom package-build-tar-executable "tar"
  "Path to a (preferably GNU) tar command.
Certain package names (e.g., \"@\") may not work properly with a BSD tar.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtar\"."
  :group 'package-build
  :type '(file :must-match t))

(defvar package-build--tar-type nil
  "Type of `package-build-tar-executable'.
Can be `gnu' or `bsd'; nil means the type is not decided yet.")

(define-obsolete-variable-alias 'package-build-write-melpa-badge-images
  'package-build-badge-data "Package-Build 5.0.0")

(defcustom package-build-badge-data nil
  "Text and color used in badge images, if any.

If nil (the default), then no badge images are generated,
otherwise this has the form (NAME COLOR).  MELPA sets the value
in its top-level Makefile, to different values, depending on the
channel that is being build."
  :group 'package-build
  :type '(list (string :tag "Archive name") color))

(defcustom package-build-version-regexp
  "\\`[rRvV]?\\(?1:[0-9]+\\(\\.[0-9]+\\)*\\)\\'"
  "Regexp used to match valid version-strings.

The first capture is used to extract the actual version string.
Strings matched by that group must be valid according to
`version-to-list', but the used regexp can be more strict.  The
default value supports only releases but no pre-releases.  It
also intentionally ignores cedrtain unfortunate version strings
such as \"1A\" or \".5\", and only supports \".\" as separator.

The part before the first capture group should match prefixes
commonly used in version tags.

Note that this variable can be overridden in a package's recipe,
using the `:version-regexp' slot."
  :group 'package-build
  :type 'string)

(defcustom package-build-allowed-git-protocols '("https" "file" "ssh")
  "Protocols that can be used to fetch from upstream with git.
By default insecure protocols, such as \"http\" or \"git\", are
disallowed."
  :group 'package-build
  :type '(repeat string))

(defvar package-build-use-git-remote-hg nil
  "Whether to use `git-remote-hg' remote helper for mercurial repos.")

(defvar package-build--inhibit-fetch nil
  "Whether to inhibit fetching.  Useful for testing purposes.")

(defvar package-build--inhibit-checkout nil
  "Whether to inhibit checkout.  Useful for testing purposes.")

(defvar package-build--inhibit-build nil
  "Whether to inhibit building.  Useful for testing purposes.")

;;; Generic Utilities

(defun package-build--message (format-string &rest args)
  "Behave like `message' if `package-build-verbose' is non-nil.
Otherwise do nothing.  FORMAT-STRING and ARGS are as per that function."
  (when package-build-verbose
    (apply #'message format-string args)))

(defun package-build--error (package format-string &rest args)
  "Behave similar to `error' but with additional logging.
Log the error to \"errors.log\" in `package-build-archive-dir'.
Prefix the entry with the date and if possible the name of a
package.  PACKAGE identifies a package, it must be a package
name, a `package-recipe' object or nil, if the command is not
being run for a particular package."
  (declare (indent defun))
  (let ((err (apply #'format-message format-string args)))
    ;; That's a bit of an inconvenient interface...
    (with-temp-buffer
      (insert (format "%s  %-25s  %s\n"
                      (format-time-string "%FT%T%z" nil t)
                      (if (cl-typep package 'package-recipe)
                          (oref package name)
                        (or package "n/a"))
                      err))
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (goto-char (point-min))
      (append-to-file
       (point)
       (1+ (line-end-position))
       (expand-file-name "errors.log" package-build-archive-dir)))
    (error "%s" err)))

;;; Version Handling
;;;; Common

(defun package-build--select-version (rcp)
  (pcase-let*
      ((default-directory (package-build--working-tree rcp t))
       (`(,commit ,time ,version)
        (cond
         ((with-no-warnings package-build-get-version-function)
          (display-warning 'package-build "\
Variable `package-build-get-version-function' is obsolete.
Instead set `package-build-release-version-functions'
and/or `package-build-snapshot-version-functions', and
set `package-build-stable' to control whether releases
or snapshots are build.")
          (with-no-warnings (funcall package-build-get-version-function rcp)))
         (package-build-stable
          (run-hook-with-args-until-success
           'package-build-release-version-functions rcp))
         ((run-hook-with-args-until-success
           'package-build-snapshot-version-functions rcp)))))
    (if (not version)
        (funcall (if package-build-all-publishable #'error #'message)
                 "Cannot determine version for %s" (oref rcp name))
      (oset rcp commit commit)
      (oset rcp time time)
      (oset rcp version version))))

(cl-defmethod package-build--select-commit ((rcp package-git-recipe) rev exact)
  (pcase-let*
      ((`(,hash ,time)
        (split-string
         (car (apply #'process-lines
                     "git" "log" "-n1" "--first-parent" "--no-show-signature"
                     "--pretty=format:%H %cd" "--date=unix" rev
                     (and (not exact)
                          (cons "--" (package-build--spec-globs rcp)))))
         " ")))
    (list hash (string-to-number time))))

(cl-defmethod package-build--select-commit ((rcp package-hg-recipe) rev exact)
  (pcase-let*
      ((`(,hash ,time ,_timezone)
        (split-string
         (car (apply #'process-lines
                     ;; The "date" keyword uses UTC. The "hgdate" filter
                     ;; returns two integers separated by a space; the
                     ;; unix timestamp and the timezone offset.  We use
                     ;; "hgdate" because that makes it easier to discard
                     ;; the time zone offset, which doesn't interest us.
                     "hg" "log" "--limit" "1"
                     "--template" "{node} {date|hgdate}\n" "--rev" rev
                     (and (not exact)
                          (cons "--" (package-build--spec-globs rcp)))))
         " ")))
    (list hash (string-to-number time))))

;;;; Tag

(defun package-build-tag-version (rcp)
  "Determine version corresponding to largest version tag for RCP.
Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING)."
  (let ((regexp (or (oref rcp version-regexp) package-build-version-regexp))
        (tag nil)
        (version '(0)))
    (dolist (n (package-build--list-tags rcp))
      (let ((v (ignore-errors
                 (version-to-list (and (string-match regexp n)
                                       (match-string 1 n))))))
        (when (and v (version-list-<= version v))
          (if (cl-typep rcp 'package-git-recipe)
              (setq tag (concat "refs/tags/" n))
            (setq tag n))
          (setq version v))))
    (and tag
         (pcase-let ((`(,hash ,time) (package-build--select-commit rcp tag t)))
           (list hash time (package-version-join version))))))

(cl-defmethod package-build--list-tags ((_rcp package-git-recipe))
  (process-lines "git" "tag" "--list"))

(cl-defmethod package-build--list-tags ((_rcp package-hg-recipe))
  (process-lines "hg" "tags" "--quiet"))

(define-obsolete-function-alias 'package-build-get-tag-version
  'package-build-tag-version "Package-Build 5.0.0")

;;;; Header

(defun package-build-header-version (rcp)
  "Return version specified in the header of the main library.

Walk the history of the main library until a commit is found
which changes the `Package-Version' or `Version' header in the
main library to a version that qualifies as a release, ignoring
any pre-releases.

Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING)."
  (and-let* ((lib (package-build--main-library rcp)))
    (with-temp-buffer
      (let (commit date version)
        (save-excursion
          (package-build--insert-version-header-log
           rcp (file-relative-name lib)))
        (while (and (not version)
                    (re-search-forward "^commit \\([^ ]+\\) \\(.+\\)" nil t))
          (setq commit (match-string 1))
          (setq date (match-string 2))
          (let ((end (save-excursion (re-search-forward "^$" nil t))))
            (when (re-search-forward
                   "^\\+;;* *\\(Package-\\)?Version: *\\(.+\\)" end t)
              (let ((ver (match-string 2)))
                (when (and (not (equal ver "0"))
                           (string-match
                            "\\`\\([0-9]+\\)\\(\\.[0-9]+\\)*\\'" ver))
                  (setq version ver))))
            (when end
              (goto-char end))))
        (and version
             (list commit
                   (string-to-number date)
                   (package-version-join (version-to-list version))))))))

(defun package-build--main-library (rcp)
  (package-build--match-library rcp))

(defun package-build--match-library (rcp &optional filename)
  (let ((libs (package-build--list-libraries rcp))
        (filename (or filename (concat (oref rcp name) ".el"))))
    (cond
     ((car (member (concat "lisp/" filename) libs)))
     ((car (member filename libs)))
     ((cl-find filename libs :test #'equal :key #'file-name-nondirectory)))))

(cl-defmethod package-build--list-libraries ((_rcp package-git-recipe))
  (process-lines "git" "ls-files" "*.el"))

(cl-defmethod package-build--list-libraries ((_rcp package-hg-recipe))
  (process-lines "hg" "files" "--include" "**/*.el"))

(cl-defmethod package-build--insert-version-header-log
  ((_rcp package-git-recipe) lib)
  (call-process "git" nil t nil
                "log" "--first-parent" "--no-renames"
                "--pretty=format:commit %H %cd" "--date=unix"
                "-L" (format "/^;;* *\\(Package-\\)\\?Version:/,+1:%s" lib)))

(cl-defmethod package-build--insert-version-header-log
  ((_rcp package-hg-recipe) _lib)
  (call-process "hg" nil t nil
                "log" "--first-parent"
                "--template" "commit: {node} {date|hgdate}\n"
                )) ; TODO What is the equivalent of Git's "-L"?

;;;; NAME-pkg

(defun package-build-pkg-version (rcp)
  "Return version specified in the \"NAME-pkg.el\" file.
Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING)."
  (and-let* ((file (package-build--pkgfile rcp)))
    (let ((regexp (or (oref rcp version-regexp) package-build-version-regexp))
          commit date version)
      (catch 'before-latest
        (pcase-dolist (`(,c ,d) (package-build--pkgfile-commits rcp file))
          (with-temp-buffer
            (save-excursion
              (package-build--insert-pkgfile rcp c file))
            (when-let* ((n (ignore-errors (nth 2 (read (current-buffer)))))
                        (v (ignore-errors
                             (version-to-list
                              (and (string-match regexp n)
                                   ;; Use match-group 0, not 1, because in
                                   ;; this file a version string without a
                                   ;; prefix is expected.
                                   (match-string 0 n))))))
              (when (and version (not (equal v version)))
                (throw 'before-latest nil))
              (setq commit c)
              (setq date d)
              (setq version v)))))
      (and version
           (list commit
                 (string-to-number date)
                 (package-version-join version))))))

(defun package-build--pkgfile (rcp)
  (package-build--match-library rcp (concat (oref rcp name) "-pkg.el")))

(cl-defmethod package-build--pkgfile-commits
  ((_rcp package-git-recipe) file)
  (mapcar (lambda (line) (split-string line " "))
          (process-lines "git" "log" "--first-parent"
                         "--pretty=format:%H %cd" "--date=unix"
                         "--" file)))

(cl-defmethod package-build--pkgfile-commits
  ((_rcp package-hg-recipe) file)
  (mapcar (lambda (line) (seq-take (split-string line " ") 2))
          (process-lines "hg" "log"
                         "--template" "{node} {date|hgdate}\n"
                         "--" file)))

(cl-defmethod package-build--insert-pkgfile
  ((_rcp package-git-recipe) commit file)
  (call-process "git" nil t nil "show" (concat commit ":" file)))

(cl-defmethod package-build--insert-pkgfile
  ((_rcp package-hg-recipe) commit file)
  (call-process "hg" nil t nil "cat" "-r" commit file))

;;;; Timestamp

(defun package-build-timestamp-version (rcp)
  "Determine timestamp version corresponding to latest relevant commit for RCP.
Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING), where
VERSION-STRING has the format \"%Y%m%d.%H%M\"."
  (pcase-let ((`(,hash ,time) (package-build--timestamp-version rcp)))
    (list hash time
          ;; We remove zero-padding of the HH portion, as
          ;; that is lost when stored in archive-contents.
          (concat (format-time-string "%Y%m%d." time t)
                  (format "%d" (string-to-number
                                (format-time-string "%H%M" time t)))))))

(cl-defmethod package-build--timestamp-version ((rcp package-git-recipe))
  (pcase-let*
      ((commit (oref rcp commit))
       (branch (oref rcp branch))
       (branch (and branch (concat "origin/" branch)))
       (rev (or commit branch "origin/HEAD"))
       (`(,rev-hash ,rev-time) (package-build--select-commit rcp rev commit))
       (`(,tag-hash ,tag-time) (package-build-tag-version rcp)))
    ;; If the latest commit that touches a relevant file is an ancestor of
    ;; the latest tagged release and the tag is reachable from origin/HEAD
    ;; (i.e., it isn't on a separate release branch) then use the tagged
    ;; release.  Snapshots should not be older than the latest release.
    (if (and tag-hash
             (zerop (call-process "git" nil nil nil
                                  "merge-base" "--is-ancestor"
                                  rev-hash tag-hash))
             (zerop (call-process "git" nil nil nil
                                  "merge-base" "--is-ancestor"
                                  tag-hash rev)))
        (list tag-hash tag-time)
      (list rev-hash rev-time))))

(cl-defmethod package-build--timestamp-version ((rcp package-hg-recipe))
  (let* ((commit (oref rcp commit))
         (branch (or (oref rcp branch) "default"))
         (rev (format "sort(ancestors(%s), -rev)"
                      (or commit (format "max(branch(%s))" branch)))))
    (package-build--select-commit rcp rev nil)))

(define-obsolete-function-alias 'package-build-get-snapshot-version
  'package-build-snapshot-version "Package-Build 5.0.0")

;;;; Release+Timestamp

(defun package-build-release+timestamp-version (rcp)
  "Determine version string in the \"RELEASE.0.TIMESTAMP\" format for RCP.

*Experimental* This function is still subject to change.

Use `package-build-release-version-functions' to determine
RELEASE.  TIMESTAMP is the COMMITTER-DATE for the identified
last relevant commit, using the format \"%Y%m%d.%H%M\".

Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING)."
  (pcase-let*
      ((`(,scommit ,stime ,sversion) (package-build-timestamp-version rcp))
       (`(,rcommit ,rtime ,rversion)
        (run-hook-with-args-until-success
         'package-build-release-version-functions rcp))
       (ahead (package-build--commit-count rcp scommit rcommit)))
    (cond
     ((> ahead 0)
      (list scommit stime
            (package-version-join
             (nconc (if rversion (version-to-list rversion) (list 0 0))
                    (list 0)
                    (version-to-list sversion)))))
     (t
      ;; The latest commit, which touched a relevant file, is either the
      ;; latest release itself, or a commit before that.  Distribute the
      ;; same commit/release as on the stable channel; as it would not
      ;; make sense for the development channel to lag behind the latest
      ;; release.
      (list rcommit rtime (package-version-join rversion))))))

;;;; Release+Count

(defun package-build-release+count-version (rcp &optional single-count)
  "Determine version string in the \"RELEASE.0.COUNT\" format for RCP.

*Experimental* This function is still subject to change.

Use `package-build-release-version-functions' to determine
RELEASE.  COUNT is the number of commits since RELEASE until the
last relevant commit.  If RELEASE is the same as for the last
snapshot but COUNT is not larger than for that snapshot because
history was rewritten, then use \"RELEASE.0.OLDCOUNT.NEWCOUNT\".

Return (COMMIT-HASH COMMITTER-DATE VERSION-STRING).
\n(fn RCP)"
  (pcase-let*
      ;; Get the commit but ignore the associated timestamp.
      ((`(,scommit ,stime ,_) (package-build-timestamp-version rcp))
       (`(,rcommit ,rtime ,version)
        (run-hook-with-args-until-success
         'package-build-release-version-functions rcp))
       (version (and rcommit (version-to-list version)))
       (merge-base (and rcommit
                        (package-build--merge-base rcp scommit rcommit)))
       (ahead (package-build--commit-count rcp scommit rcommit)))
    (cond
     ((or (when (not rcommit)
            ;; No appropriate release detected.
            (setq version (list 0 0))
            t)
          (when (not merge-base)
            ;; As a result of butchered history rewriting, version tags
            ;; share no history at all with what is currently reachable
            ;; from the tip.  Completely ignore these unreachable tags and
            ;; behave as if no version tags existed at all.  Unfortunately
            ;; that means that users, who have installed a snapshot based
            ;; on a now abandoned tag, are stuck on that snapshot until
            ;; upstream creates a new version tag.
            (setq version (list 0 0))
            t)
          ;; Snapshot commit is newer than latest release (or there is no
          ;; release).
          (> ahead 0))
      (list scommit stime
            (package-version-join
             (append version
                     (list 0)
                     ;; (This argument *could* be used by a wrapper.)
                     (if single-count
                         ahead ; Pretend time-travel doesn't happen.
                       (package-build--ensure-count-increase
                        rcp (copy-sequence version) ahead))))))
     (t
      ;; The latest commit, which touched a relevant file, is either the
      ;; latest release itself, or a commit before that.  Distribute the
      ;; same commit/release as on the stable channel; as it would not
      ;; make sense for the development channel to lag behind the latest
      ;; release.
      (list rcommit rtime (package-version-join version))))))

(defun package-build--ensure-count-increase (rcp version ahead)
  (if-let ((previous (cdr (assq (intern (oref rcp name))
                                (package-build-archive-alist)))))
      ;; Because upstream may have rewritten history, we cannot be certain
      ;; that appending the new count of commits would result in a version
      ;; string that is greater than the version string used for the
      ;; previous snapshot.
      (let ((count (list ahead))
            (pversion (aref previous 0))
            (pcount nil))
        (when (and
               ;; If there is no zero part, then we know that the previous
               ;; snapshot exactly matched a tagged release (in which case
               ;; we do not append zero and the count).
               (memq 0 pversion)
               ;; Likewise if there is a tag that exactly matches the
               ;; previous (non-)snapshot, then there is no old count
               ;; which we would have to compare with the new count.
               (not (member (mapconcat #'number-to-string pversion ".")
                            (package-build--list-tags rcp))))
          ;; The previous snapshot does not exactly match a tagged
          ;; version.  We must split the version string into its tag
          ;; and count parts.  The last zero part is the boundary.
          (let ((split (cl-position 0 pversion :from-end t))
                (i 0)
                (tagged nil))
            (while (< i split)
              (push (pop pversion) tagged)
              (cl-incf i))
            (setq pcount (cdr pversion))
            (setq pversion (nreverse tagged)))
          ;; Determine whether we can reset the count or increase it, or
          ;; whether we have to preserve the old count due to rewritten
          ;; history in order to ensure that the new snapshot version is
          ;; greater than the previous snapshot.
          ;; If the previous and current snapshot commits do not follow
          ;; the same tag, then their respective counts of commits since
          ;; their respective tag have no relation to each other and we
          ;; can simply reset the count, determined above.
          (when (equal version pversion)
            ;; If the new count is smaller than the old, then we keep the
            ;; old count and append the new count as a separate version
            ;; part.
            ;;
            ;; We may have had to do that for previous snapshots, possibly
            ;; even for multiple consecutive snapshots.  Beginning at the
            ;; end, scrape of all counts that are smaller than the current
            ;; count, but leave the others intact.
            (setq pcount (nreverse pcount))
            (while (and pcount (> ahead (car pcount)))
              (pop pcount))
            (when pcount
              ;; This snapshot is based on the same tag as the previous
              ;; snapshot and, due to history rewriting, the count did
              ;; not increase.
              (setq count (nreverse (cons (car count) pcount))))))
        count)
    (list ahead)))

(cl-defmethod package-build--merge-base ((_rcp package-git-recipe) a b)
  (ignore-errors (car (process-lines "git" "merge-base" a b))))

(cl-defmethod package-build--merge-base ((_rcp package-hg-recipe) a b)
  (car (process-lines "hg" "log" "--template" "{node}\\n" "--rev"
                      (format "ancestor(%s, %s)" a b))))

(cl-defmethod package-build--commit-count ((_rcp package-git-recipe) rev since)
  (string-to-number
   (car (if since
            (process-lines "git" "rev-list" "--count" rev (concat "^" since))
          (process-lines "git" "rev-list" "--count" rev)))))

(cl-defmethod package-build--commit-count ((_rcp package-hg-recipe) rev since)
  (length (process-lines "hg" "log" "--template" "{rev}\\n" "--rev"
                         (if since
                             (format "only(%s, %s)" rev since)
                           (format "ancestors(%s)" rev)))))

;;;; Fallback-Count

(defun package-build-fallback-count-version (rcp)
  "Determine version string in the \"0.0.0.COUNT\" format for RCP.

*Experimental* This function is still subject to change.

This function implements a fallback that can be used on the
release channel, for packages that don't do releases.  It should
be the last element of `package-build-release-version-functions',
and at the same time `package-build-snapshot-version-functions'
should contain only `package-build-release+count-version'.

The result of such a configuration is that, for packages that
don't do releases, the release and snapshot channels provide
the same \"0.0.0.COUNT\" snapshot.  That way, all packages are
available on the release channel, which makes that channel more
attractive to users, which might encourage some maintainers to
release more often, or if they have never done a release before,
to finally get around to that initial release.  In other words,
this might help overcome the release channel's chicken and egg
problem."
  (let ((package-build-release-version-functions nil))
    (package-build-release+count-version rcp)))

;;; Call Process

(defun package-build--call-process (package command &rest args)
  "For PACKAGE, run COMMAND with ARGS in `default-directory'.
We use this to wrap commands is proper environment settings and
with a timeout so that no command can block the build process,
and so we can properly log errors.  PACKAGE must be the name of
a package, a `package-recipe' object or nil, and is only used
for logging purposes."
  (unless (file-directory-p default-directory)
    (error "Cannot run process in non-existent directory: %s"
           default-directory))
  (with-temp-buffer
    (pcase-let* ((args-orig (cons command args))
                 (`(,command . ,args)
                  (nconc (and (not (eq system-type 'windows-nt))
                              (list "env" "LC_ALL=C"))
                         (if (and package-build-timeout-secs
                                  package-build-timeout-executable)
                             (nconc (list package-build-timeout-executable
                                          "-k" "60"
                                          (number-to-string
                                           package-build-timeout-secs)
                                          command)
                                    args)
                           (cons command args))))
                 (exit-code
                  (apply #'call-process command nil (current-buffer) nil args)))
      (unless (equal exit-code 0) ; may also be a string
        (let ((summary (format-message
                        "Command `%s' exited with non-zero exit-code: %s"
                        (mapconcat #'shell-quote-argument args-orig " ")
                        exit-code)))
          ;; Duplicating the summary like this is a bit unfortunate, but
          ;; still the best option because we want to show it before the
          ;; output, but also want it to appear as an error message,
          ;; without making the, potentially multi-line, output part of
          ;; the error message.
          (message "%s" summary)
          (message "%s" (buffer-string))
          (package-build--error package "%s" summary))))))

(defun package-build--run-process (command &rest args)
  "Like `package-build--call-process', but lacks the PACKAGE argument."
  (apply #'package-build--call-process nil command args))
(make-obsolete 'package-build--run-process 'package-build--call-process "5.0.0")

;;; Worktree

(defun package-build--working-tree (rcp &optional early)
  (if early
      (funcall package-build-early-worktree-function rcp)
    (funcall package-build-worktree-function rcp)))

;;; Fetch

(cl-defmethod package-build--fetch ((rcp package-git-recipe))
  (let ((dir (package-build--working-tree rcp t))
        (url (package-recipe--upstream-url rcp))
        (protocol (package-recipe--upstream-protocol rcp)))
    (unless (member protocol package-build-allowed-git-protocols)
      (package-build--error rcp
        "Fetching using the %s protocol is not allowed" protocol))
    (cond
     ((and (file-exists-p (expand-file-name ".git" dir))
           (let ((default-directory dir))
             (string= (car (process-lines "git" "config" "remote.origin.url"))
                      url)))
      (unless package-build--inhibit-fetch
        (let ((default-directory dir))
          (package-build--message "Updating %s" dir)
          (package-build--call-process rcp "git" "fetch" "-f" "--tags" "origin")
          ;; We might later checkout "origin/HEAD". Sadly "git fetch"
          ;; cannot be told to keep it up-to-date, so we have to make
          ;; a second request.
          (package-build--call-process
           rcp "git" "remote" "set-head" "origin" "--auto"))))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (make-directory package-build-working-dir t)
      (let ((default-directory package-build-working-dir))
        (package-build--call-process rcp "git" "clone" url dir))))))

(cl-defmethod package-build--fetch ((rcp package-hg-recipe))
  (let ((dir (package-build--working-tree rcp t))
        (url (package-recipe--upstream-url rcp)))
    (cond
     ((and (file-exists-p (expand-file-name ".hg" dir))
           (let ((default-directory dir))
             (string= (car (process-lines "hg" "paths" "default")) url)))
      (unless package-build--inhibit-fetch
        (let ((default-directory dir))
          (package-build--message "Updating %s" dir)
          (package-build--call-process rcp "hg" "pull")
          (package-build--call-process rcp "hg" "update"))))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (make-directory package-build-working-dir t)
      (let ((default-directory package-build-working-dir))
        (package-build--call-process rcp "hg" "clone" url dir))))))

;;; Checkout

(cl-defmethod package-build--checkout ((rcp package-git-recipe))
  (unless package-build--inhibit-checkout
    (let ((rev (oref rcp commit)))
      (package-build--message "Checking out %s" rev)
      (package-build--call-process rcp "git" "reset" "--hard" rev))))

(cl-defmethod package-build--checkout ((rcp package-hg-recipe))
  (unless package-build--inhibit-checkout
    (let ((rev (oref rcp commit)))
      (package-build--message "Checking out %s" rev)
      (package-build--call-process rcp "hg" "update" rev))))

;;; Generate Files

(defun package-build--write-pkg-file (desc dir)
  (let ((name (package-desc-name desc)))
    (with-temp-file (expand-file-name (format "%s-pkg.el" name) dir)
      (pp `(define-package ,(symbol-name name)
             ,(package-version-join (package-desc-version desc))
             ,(package-desc-summary desc)
             ',(mapcar (pcase-lambda (`(,pkg ,ver))
                         (list pkg (package-version-join ver)))
                       (package-desc-reqs desc))
             ,@(cl-mapcan (pcase-lambda (`(,key . ,val))
                            (when (or (symbolp val) (listp val))
                              ;; We must quote lists and symbols,
                              ;; because Emacs 24.3 and earlier evaluate
                              ;; the package information, which would
                              ;; break for unquoted symbols or lists.
                              ;; While this library does not support
                              ;; such old Emacsen, the packages that
                              ;; we produce should remain compatible.
                              (setq val (list 'quote val)))
                            (list key val))
                          (package-desc-extras desc)))
          (current-buffer))
      (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n"
             (current-buffer)))))

(defun package-build--tar-type ()
  "Return `bsd' or `gnu' depending on type of Tar executable.
Tests and sets variable `package-build--tar-type' if not already set."
  (or package-build--tar-type
      (and package-build-tar-executable
           (let ((v (shell-command-to-string
                     (format "%s --version" package-build-tar-executable))))
             (setq package-build--tar-type
                   (cond ((string-match-p "bsdtar" v) 'bsd)
                         ((string-match-p "GNU tar" v) 'gnu)
                         (t 'gnu)))))))

(defun package-build--create-tar (rcp directory)
  "Create a tar file containing the package version specified by RCP.
DIRECTORY is a temporary directory that contains the directory
that is put in the tarball."
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (time (oref rcp time))
         (tar (expand-file-name (concat name "-" version ".tar")
                                package-build-archive-dir))
         (dir (concat name "-" version)))
    (when (and (eq system-type 'windows-nt)
               (eq (package-build--tar-type) 'gnu))
      (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file
       package-build-tar-executable nil
       (get-buffer-create "*package-build-checkout*") nil
       "-cf" tar dir
       ;; Arguments that are need to strip metadata that
       ;; prevent a reproducible tarball as described at
       ;; https://reproducible-builds.org/docs/archives.
       "--sort=name"
       (format "--mtime=@%d" time)
       "--owner=0" "--group=0" "--numeric-owner"
       "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

(defun package-build--write-pkg-readme (pkg files)
  (when-let* ((name (oref pkg name))
              (commentary
               (let* ((file (concat name ".el"))
                      (file (or (car (rassoc file files)) file))
                      (file (and file (expand-file-name file))))
                 (and (file-exists-p file)
                      (lm-commentary file)))))
    (with-temp-buffer
      (if (>= emacs-major-version 28)
          (insert commentary)
        ;; Taken from 28.0's `lm-commentary'.
        (insert
         (replace-regexp-in-string       ; Get rid of...
          "[[:blank:]]*$" ""             ; trailing white-space
          (replace-regexp-in-string
           (format "%s\\|%s\\|%s"
                   ;; commentary header
                   (concat "^;;;[[:blank:]]*\\("
                           lm-commentary-header
                           "\\):[[:blank:]\n]*")
                   "^;;[[:blank:]]?"     ; double semicolon prefix
                   "[[:blank:]\n]*\\'")  ; trailing new-lines
           "" commentary))))
      (unless (or (bobp) (= (char-before) ?\n))
        (insert ?\n))
      ;; We write the file even if it is empty, which is perhaps
      ;; a questionable choice, but at least it's consistent.
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region nil nil
                      (expand-file-name (concat name "-readme.txt")
                                        package-build-archive-dir))))))

(defun package-build--generate-info-files (rcp files target-dir)
  "Create an info file for each texinfo file listed in FILES.
Also create the info dir file.  Remove each original texinfo
file.  The source and destination file paths are expanded in
`default-directory' and TARGET-DIR respectively."
  (pcase-dolist (`(,src . ,tmp) files)
    (let ((extension (file-name-extension tmp)))
      (when (member extension '("info" "texi" "texinfo"))
        (let* ((src (expand-file-name src))
               (tmp (expand-file-name tmp target-dir))
               (texi src)
               (info tmp))
          (when (member extension '("texi" "texinfo"))
            (delete-file tmp)
            (setq info (concat (file-name-sans-extension tmp) ".info"))
            (unless (file-exists-p info)
              (package-build--message "Generating %s" info)
              ;; If the info file is located in a subdirectory
              ;; and contains relative includes, then it is
              ;; necessary to run makeinfo in the subdirectory.
              (with-demoted-errors "Error: %S"
                (let ((default-directory (file-name-directory texi)))
                  (package-build--call-process
                   rcp "makeinfo" "--no-split" texi "-o" info)))))
          (with-demoted-errors "Error: %S"
            (let ((default-directory target-dir))
              (package-build--call-process
               rcp "install-info" "--dir=dir" info))))))))

;;; Patch Libraries

(defun package-build--update-or-insert-header (name value)
  "Ensure current buffer has NAME header with the given VALUE.
Any existing header will be preserved and given the \"X-Original-\" prefix.
If VALUE is nil, the new header will not be inserted, but any original will
still be renamed."
  (goto-char (point-min))
  (cond
   ((let ((case-fold-search t))
      (re-search-forward (format "^;+* *%s *: *" (regexp-quote name)) nil t))
    (move-beginning-of-line nil)
    (search-forward "V" nil t)
    (backward-char)
    (insert "X-Original-")
    (move-beginning-of-line nil))
   (t
    ;; Put the new header in a sensible place if we can.
    (re-search-forward
     "^;+* *\\(Version\\|Package-Requires\\|Keywords\\|URL\\) *:" nil t)
    (forward-line)))
  (insert (format ";; %s: %s\n" name value)))

(defun package-build--ensure-ends-here-line (file)
  "Add the \"FILE ends here\" trailing line if it is missing."
  (save-excursion
    (goto-char (point-min))
    (let ((trailer (format ";;; %s ends here" (file-name-nondirectory file))))
      (unless (re-search-forward (format "^%s" (regexp-quote trailer)) nil t)
        (goto-char (point-max))
        (insert ?\n trailer ?\n)))))

;;; Package Structs

(defun package-build--desc-from-library (rcp files &optional kind)
  "Return the package description for RCP.

This function is used for all packages that consist of a single
file and those packages that consist of multiple files but lack
a file named \"NAME-pkg.el\" or \"NAME-pkg.el\".

The returned value is a `package-desc' struct (which see).
The values of the `name' and `version' slots are taken from RCP
itself.  The value of `kind' is taken from the KIND argument,
which defaults to `single'; the other valid value being `tar'.

Other information is taken from the file named \"NAME-pkg.el\",
which should appear in FILES.  As a fallback, \"NAME-pkg.el.in\"
is also tried.  If neither file exists, then return nil.  If a
value is not specified in the used file, then fall back to the
value specified in the file \"NAME.el\"."
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (commit (oref rcp commit))
         (file (concat name ".el"))
         (file (or (car (rassoc file files)) file))
         (maintainers nil))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (setq maintainers
                 (if (fboundp 'lm-maintainers)
                     (lm-maintainers)
                   (with-no-warnings
                     (and-let* ((maintainer (lm-maintainer)))
                       (list maintainer)))))
           (package-desc-from-define
            name version
            (or (save-excursion
                  (goto-char (point-min))
                  (and (re-search-forward "\
^;;; [^ ]*\\.el ---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$" nil t)
                       (match-string-no-properties 1)))
                "No description available.")
            (cond
             ((fboundp 'lm-package-requires)
              (lm-package-requires))
             ((fboundp 'package--prepare-dependencies)
              (and-let* ((require-lines
                          (lm-header-multiline "package-requires")))
                (package--prepare-dependencies
                 (package-read-from-string
                  (mapconcat #'identity require-lines " "))))))
            ;; `:kind' and `:archive' are handled separately.
            :kind       (or kind 'single)
            ;; The other keyword arguments are appended to the alist
            ;; stored in the `extras' slot.  Make sure `:commit', which
            ;; always exists and never has to be removed, comes first in
            ;; the end result, so we can post-process the returned data
            ;; by side-effect, e.g., to remove somewhat broken maintainer
            ;; information, that cannot easily be encoded as json (see
            ;; `package-build--archive-alist-for-json').
            :url        (lm-homepage)
            :keywords   (lm-keywords-list)
            ;; Newer `package.el' versions support both `:maintainers' and
            ;; `:maintainer', while older versions only support the latter.
            :maintainer  (car maintainers)
            :maintainers maintainers
            :authors     (lm-authors)
            :commit      commit)))))

(defun package-build--desc-from-package (rcp files)
  "Return the package description for RCP.

This function is used for packages that consist of multiple files.

The returned value is a `package-desc' struct (which see).
The values of the `name' and `version' slots are taken from RCP
itself.  The value of `kind' is always `tar'.

Other information is taken from the file named \"NAME.el\",
which should appear in FILES.  As a fallback, \"NAME.el.in\"
is also tried.  If neither file exists, then return nil."
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (commit (oref rcp commit))
         (file (concat name "-pkg.el"))
         (file (or (car (rassoc file files))
                   file)))
    (and (or (file-exists-p file)
             (file-exists-p (setq file (concat file ".in"))))
         (let ((form (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer)))))
           (unless (eq (car form) 'define-package)
             (package-build--error name "No define-package found in %s" file))
           (pcase-let*
               ((`(,_ ,_ ,_ ,summary ,deps . ,extra) form)
                (deps (eval deps))
                (alt-desc (package-build--desc-from-library rcp files))
                (alt (and alt-desc (package-desc-extras alt-desc))))
             (when (string-match "[\r\n]" summary)
               (package-build--error name
                 "Illegal multi-line package description in %s" file))
             (package-desc-from-define
              name version
              (if (string-empty-p summary)
                  (or (and alt-desc (package-desc-summary alt-desc))
                      "No description available.")
                summary)
              (mapcar (pcase-lambda (`(,pkg ,ver))
                        (unless (symbolp pkg)
                          (package-build--error name
                            "Invalid package name in dependency: %S" pkg))
                        (list pkg ver))
                      deps)
              :kind       'tar
              :url        (or (alist-get :url extra)
                              (alist-get :homepage extra)
                              (alist-get :url alt))
              :keywords   (or (alist-get :keywords extra)
                              (alist-get :keywords alt))
              :maintainer (or (alist-get :maintainer extra)
                              (alist-get :maintainer alt))
              :authors    (or (alist-get :authors extra)
                              (alist-get :authors alt))
              :commit     commit))))))

(defun package-build--write-archive-entry (desc)
  (with-temp-file
      (expand-file-name (concat (package-desc-full-name desc) ".entry")
                        package-build-archive-dir)
    (set-buffer-file-coding-system 'utf-8)
    (pp (cons (package-desc-name    desc)
              (vector (package-desc-version desc)
                      (package-desc-reqs    desc)
                      (package-desc-summary desc)
                      (package-desc-kind    desc)
                      (package-desc-extras  desc)))
        (current-buffer))))

;;; Files Spec

(defconst package-build-default-files-spec
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun package-build-expand-files-spec (rcp &optional assert repo spec)
  "Return an alist of files of package RCP to be included in tarball.

Each element has the form (SOURCE . DESTINATION), where SOURCE
is a file in the package's repository and DESTINATION is where
that file is placed in the package's tarball.

RCP is the package recipe as an object.  If the `files' slot of
RCP is non-nil, then that is used as the file specification.
Otherwise `package-build-default-files-spec' is used.

If optional ASSERT is non-nil, then raise an error if nil would
be returned.  If ASSERT and `files' are both non-nil and using
`files' results in the same set of files as the default spec,
then show a warning.

A files specification is a list.  Its elements are processed in
order and can have the following form:

- :defaults

  If the first element is `:defaults', then that means to prepend
  the default files spec to the SPEC specified by the remaining
  elements.

- GLOB

  A string is glob-expanded to match zero or more files.  Matched
  files are copied to the top-level directory.

- (SUBDIRECTORY GLOB...)

  A list that begins with a string causes the files matched by
  the second and subsequent elements to be copied into the sub-
  directory specified by the first element.

- (:exclude GLOB...)

  A list that begins with `:exclude' causes files that were
  matched by earlier elements that are also matched by the second
  and subsequent elements of this list to be removed from the
  returned alist.  Files matched by later elements are not
  affected."
  (let ((default-directory (or repo (package-build--working-tree rcp)))
        (spec (or spec (oref rcp files)))
        (name (oref rcp name)))
    (when (eq (car spec) :defaults)
      (setq spec (append package-build-default-files-spec (cdr spec))))
    (let ((files (package-build--expand-files-spec-1
                  (or spec package-build-default-files-spec))))
      (when assert
        (when (and rcp spec
                   (equal files (package-build--expand-files-spec-1
                                 package-build-default-files-spec)))
          (message "Warning: %s :files spec is equivalent to the default" name))
        (unless files
          (package-build--error name
            "No matching file(s) found in %s using %s"
            default-directory (or spec "default spec"))))
      files)))

(defun package-build--expand-files-spec-1 (spec)
  "Return a list of all files matching SPEC in `default-directory'.
SPEC is a full files spec as stored in a recipe object."
  (let (include exclude)
    (dolist (entry spec)
      (if (eq (car-safe entry) :exclude)
          (dolist (entry (cdr entry))
            (push entry exclude))
        (push entry include)))
    (cl-set-difference
     (package-build--expand-files-spec-2 (nreverse include))
     (package-build--expand-files-spec-2 (nreverse exclude))
     :test #'equal :key #'car)))

(defun package-build--expand-files-spec-2 (spec &optional subdir)
  "Return a list of all files matching SPEC in SUBDIR.
If SUBDIR is nil, use `default-directory'.  SPEC is expected to
be a partial files spec, consisting of either all include rules
or all exclude rules (with the `:exclude' keyword removed)."
  (mapcan (lambda (entry)
            (if (stringp entry)
                (mapcar (lambda (f)
                          (cons f
                                (concat subdir
                                        (replace-regexp-in-string
                                         "\\.el\\.in\\'"  ".el"
                                         (file-name-nondirectory f)))))
                        (file-expand-wildcards entry))
              (package-build--expand-files-spec-2
               (cdr entry)
               (concat subdir (car entry) "/"))))
          spec))

(defun package-build--copy-package-files (files target-dir)
  "Copy FILES from `default-directory' to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (package-build--message
   "Copying files (->) and directories (=>)\n  from %s\n  to %s"
   default-directory target-dir)
  (pcase-dolist (`(,src . ,dst) files)
    (let ((src* (expand-file-name src))
          (dst* (expand-file-name dst target-dir)))
      (make-directory (file-name-directory dst*) t)
      (cond ((file-regular-p src*)
             (package-build--message
              "  %s %s -> %s" (if (equal src dst) " " "!") src dst)
             (copy-file src* dst*))
            ((file-directory-p src*)
             (package-build--message
              "  %s %s => %s" (if (equal src dst) " " "!") src dst)
             (copy-directory src* dst*))))))

(defun package-build--spec-globs (rcp)
  "Return a list of vcs arguments to match the files specified in RCP."
  ;; See glob(7), gitglossary(7) and "hg help patterns".
  (cl-flet ((toargs (glob &optional exclude)
              ;; Given an element like ("dir" "dir/*"), we want to move
              ;; all children of "dir" to the top-level.  Glob handling
              ;; of git-log/hg-log only cares about regular file, so if
              ;; "dir/subdir/file" is modified, then "dir/*" does not
              ;; match that change.  Use "dir/**" instead, to make them
              ;; look for changes to files in "dir" and all subdirs.
              (when (string-suffix-p "/*" glob)
                (setq glob (concat glob "*")))
              (cl-etypecase rcp
                (package-git-recipe
                 (list (format ":(glob%s)%s" (if exclude ",exclude" "") glob)))
                (package-hg-recipe
                 (list (if exclude "--exclude" "--include")
                       (concat "glob:" glob))))))
    (mapcan (lambda (entry)
              (pcase-exhaustive entry
                ((and glob (pred stringp))
                 (toargs glob))
                ((and `(:exclude . ,globs)
                      (guard (cl-every #'stringp globs)))
                 (mapcan (lambda (g) (toargs g t)) globs))
                ((and `(,dir . ,globs)
                      (guard (stringp dir))
                      (guard (cl-every #'stringp globs)))
                 dir ; Silence byte-compiler of Emacs < 28.1.
                 (mapcan #'toargs globs))))
            (let ((spec (or (oref rcp files) package-build-default-files-spec)))
              (if (eq (car spec) :defaults)
                  (append package-build-default-files-spec (cdr spec))
                spec)))))

;;; Commands

;;;###autoload
(defun package-build-archive (name &optional dump-archive-contents)
  "Build a package archive for the package named NAME.
If DUMP-ARCHIVE-CONTENTS is non-nil, the updated archive contents
are subsequently dumped."
  (interactive (list (package-recipe-read-name) t))
  (unless (file-exists-p package-build-archive-dir)
    (package-build--message "Creating directory %s" package-build-archive-dir)
    (make-directory package-build-archive-dir))
  (let* ((start-time (current-time))
         (rcp (package-recipe-lookup name))
         (url (package-recipe--upstream-url rcp))
         (repo (oref rcp repo))
         (fetcher (package-recipe--fetcher rcp))
         (version nil))
    (cond ((not noninteractive)
           (message " • %s package %s (from %s)..."
                    (if package-build--inhibit-build "Fetching" "Building")
                    name
                    (if repo (format "%s:%s" fetcher repo) url)))
          (package-build-verbose
           (message "Package: %s" name)
           (message "Fetcher: %s" fetcher)
           (message "Source:  %s\n" url)))
    (funcall package-build-fetch-function rcp)
    (unless package-build--inhibit-build
      (package-build--select-version rcp)
      (setq version (oref rcp version))
      (when version
        (package-build--package rcp)
        (when dump-archive-contents
          (package-build-dump-archive-contents)))
      (if (not version)
          (message " ✗ Cannot determine version!")
        (message " ✓ Success:")
        (pcase-dolist (`(,file . ,attrs)
                       (directory-files-and-attributes
                        package-build-archive-dir nil
                        (format "\\`%s-[0-9]+" name)))
          (message "  %s  %s"
                   (format-time-string
                    "%FT%T%z" (file-attribute-modification-time attrs) t)
                   file))))
    (message "%s %s in %.3fs, finished at %s"
             (if version "Built" "Fetched")
             name
             (float-time (time-since start-time))
             (format-time-string "%FT%T%z" nil t))))

;;;###autoload
(defun package-build--package (rcp)
  "Build the package version specified by RCP.
Return the archive entry for the package and store the package
in `package-build-archive-dir'."
  (let ((default-directory (package-build--working-tree rcp)))
    (unwind-protect
        (progn
          (funcall package-build-checkout-function rcp)
          (let ((files (package-build-expand-files-spec rcp t)))
            (cond
             ((= (length files) 0)
              (package-build--error rcp
                "Unable to find files matching recipe patterns"))
             (package-build-build-function
              (funcall package-build-build-function rcp files))
             ((= (length files) 1)
              (package-build--build-single-file-package rcp files))
             (t
              (package-build--build-multi-file-package rcp files)))
            (when package-build-badge-data
              (package-build--write-badge-image
               (oref rcp name) (oref rcp version) package-build-archive-dir))))
      (funcall package-build-cleanup-function rcp))))

(defun package-build--build-single-file-package (rcp files)
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (commit (oref rcp commit))
         (file (caar files))
         (source (expand-file-name file))
         (target (expand-file-name (concat name "-" version ".el")
                                   package-build-archive-dir))
         (desc (package-build--desc-from-library rcp files)))
    (unless (member (downcase (file-name-nondirectory file))
                    (list (downcase (concat name ".el"))
                          (downcase (concat name ".el.in"))))
      (package-build--error name
        "Single file %s does not match package name %s" file name))
    (copy-file source target t)
    (let ((enable-local-variables nil)
          (make-backup-files nil)
          (before-save-hook nil))
      (with-current-buffer (find-file target)
        (package-build--update-or-insert-header "Package-Commit" commit)
        (package-build--update-or-insert-header "Package-Version" version)
        (package-build--ensure-ends-here-line source)
        (write-file target nil)
        (kill-buffer)))
    (package-build--write-pkg-readme rcp files)
    (package-build--write-archive-entry desc)))

(defun package-build--build-multi-file-package (rcp files)
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (tmp-dir (file-name-as-directory (make-temp-file name t))))
    (unwind-protect
        (let* ((target (expand-file-name (concat name "-" version) tmp-dir))
               (desc (or (package-build--desc-from-package rcp files)
                         (package-build--desc-from-library rcp files 'tar)
                         (package-build--error name
                           "%s[-pkg].el matching package name is missing"
                           name))))
          (package-build--copy-package-files files target)
          (package-build--write-pkg-file desc target)
          (package-build--generate-info-files rcp files target)
          (package-build--create-tar rcp tmp-dir)
          (package-build--write-pkg-readme rcp files)
          (package-build--write-archive-entry desc))
      (delete-directory tmp-dir t nil))))

(defun package-build--cleanup (rcp)
  (cond ((cl-typep rcp 'package-git-recipe)
         (package-build--call-process rcp "git" "clean" "-f" "-d" "-x"))
        ((cl-typep rcp 'package-hg-recipe)
         ;; Mercurial's interface is so much better than Git's, they said.
         (with-temp-buffer
           (process-file "hg" nil t nil "status" "--no-status" "--unknown" "-0")
           (mapc #'delete-file (split-string (buffer-string) "\0" t)))
         (with-temp-buffer
           (process-file "hg" nil t nil "status" "--no-status" "--ignored" "-0")
           (mapc #'delete-file (split-string (buffer-string) "\0" t)))
         (package-build--call-process rcp "hg" "purge"))))

;;;###autoload
(defun package-build-all ()
  "Build a package for each of the available recipes.
If `package-build-predicate-function' is non-nil, then only
packages for which that returns non-nil are build."
  (interactive)
  (let* ((start (current-time))
         (recipes (package-recipe-recipes))
         (total (length recipes))
         (success 0)
         skipped invalid failed)
    (dolist (name recipes)
      (let ((rcp (with-demoted-errors "Recipe error: %S"
                   (package-recipe-lookup name))))
        (cond ((not rcp)
               (push name invalid))
              ((and package-build-predicate-function
                    (not (funcall package-build-predicate-function rcp)))
               (push name skipped))
              ((with-demoted-errors "Build error: %S"
                 (package-build-archive name) t)
               (cl-incf success))
              ((push name failed)))))
    (let ((duration (/ (float-time (time-subtract (current-time) start)) 60)))
      (if (not (or skipped invalid failed))
          (message "Successfully built all %s packages (%.0fm)" total duration)
        (message "Successfully built %i of %s packages (%.0fm)"
                 success total duration)
        (when skipped
          (message "Skipped %i packages:\n%s"
                   (length skipped)
                   (mapconcat (lambda (n) (concat "  " n)) (nreverse skipped) "\n")))
        (when invalid
          (message "Did not built packages for %i invalid recipes:\n%s"
                   (length invalid)
                   (mapconcat (lambda (n) (concat "  " n)) (nreverse invalid) "\n")))
        (when failed
          (message "Building %i packages failed:\n%s"
                   (length failed)
                   (mapconcat (lambda (n) (concat "  " n)) (nreverse failed) "\n"))))))
  (package-build-dump-archive-contents))

(defun package-build-cleanup ()
  "Remove previously built packages that no longer have recipes."
  (interactive)
  (package-build-dump-archive-contents))

;;; Archive

(defun package-build-archive-alist ()
  "Return the archive contents, without updating it first."
  (let ((file (expand-file-name "archive-contents" package-build-archive-dir)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (cdr (read (current-buffer)))))))

(defun package-build-dump-archive-contents (&optional file pretty-print)
  "Update and return the archive contents.

Update files \"archive-contents\" and \"elpa-packages.eld\" in
`package-build-archive-dir'.  If optional FILE is non-nil,
use that to store the archive contents and place the second
file next to it.

If optional PRETTY-PRINT is non-nil, then pretty-print
\"archive-contents\" instead of using one line per entry.
\"elpa-packages.eld\" always uses one line per entry."
  (let ((default-directory package-build-archive-dir)
        (entries nil)
        (vc-pkgs nil))
    (dolist (file (sort (directory-files default-directory t ".*\\.entry\\'")
                        ;; Sort more recently build packages first.
                        #'file-newer-than-file-p))
      (let* ((entry (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))
             (symbol (car entry))
             (name (symbol-name symbol))
             (outdated (assq symbol entries)))
        (cond
         ((not (file-exists-p (expand-file-name name package-build-recipes-dir)))
          ;; Recipe corresponding to this entry no longer exists.
          (package-build--remove-archive-files entry))
         (outdated
          ;; Prefer the more recently built package, which may not
          ;; necessarily have the highest version number, e.g., if
          ;; commit histories were changed.
          (package-build--remove-archive-files entry))
         (t
          (push entry entries)
          ;; [Non]GNU ELPA recipes are not compatible with Melpa recipes.
          ;; See around occurrences of "pkg-spec" in "package-vc.el";
          ;; section "Specifications (elpa-packages)" in "README" of the
          ;; "elpa-admin" branch in "emacs/elpa.git" repository; and also
          ;; `elpaa--supported-keywords' and `elpaa--publish-package-spec'.
          (and-let* ((recipe (with-demoted-errors "Recipe error: %S"
                               (package-recipe-lookup name))))
            (push `(,symbol
                    :url ,(package-recipe--upstream-url recipe)
                    ,@(and (cl-typep recipe 'package-hg-recipe)
                           (list :vc-backend 'Hg))
                    ,@(and-let* ((branch (oref recipe branch)))
                        (list :branch branch)))
                  vc-pkgs))))))
    (setq entries (cl-sort entries #'string< :key #'car))
    (with-temp-file (or file (expand-file-name "archive-contents"))
      (let ((print-level nil)
            (print-length nil))
        (if pretty-print
            (pp (cons 1 entries) (current-buffer))
          (insert "(1")
          (dolist (entry entries)
            (newline)
            (insert " ")
            (prin1 entry (current-buffer)))
          (insert ")\n"))))
    (setq vc-pkgs (cl-sort vc-pkgs #'string< :key #'car))
    (with-temp-file (expand-file-name "elpa-packages.eld"
                                      (and file (file-name-nondirectory file)))
      (let ((print-level nil)
            (print-length nil))
        (insert "((")
        (prin1 (car vc-pkgs) (current-buffer))
        (dolist (entry (cdr vc-pkgs))
          (newline)
          (insert "  ")
          (prin1 entry (current-buffer)))
        (insert ")\n :version 1 :default-vc Git)\n")))
    entries))

(defun package-build--remove-archive-files (archive-entry)
  "Remove the entry and archive file for ARCHIVE-ENTRY."
  (package-build--message "Removing archive: %s-%s"
                          (car archive-entry)
                          (package-version-join (aref (cdr archive-entry) 0)))
  (let ((file (package-build--artifact-file archive-entry)))
    (when (file-exists-p file)
      (delete-file file)))
  (let ((file (package-build--archive-entry-file archive-entry)))
    (when (file-exists-p file)
      (delete-file file))))

(defun package-build--artifact-file (archive-entry)
  "Return the artifact file for the package specified by ARCHIVE-ENTRY.
This is either a tarball or an Elisp file."
  (pcase-let* ((`(,name . ,desc) archive-entry)
               (version (package-version-join (aref desc 0)))
               (flavour (aref desc 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     package-build-archive-dir)))

(defun package-build--archive-entry-file (archive-entry)
  "Return the file in which ARCHIVE-ENTRY should be stored.
ARCHIVE-ENTRY contains information about a specific version of
a package."
  (pcase-let* ((`(,name . ,desc) archive-entry)
               (version (package-version-join (aref desc 0))))
    (expand-file-name
     (format "%s-%s.entry" name version)
     package-build-archive-dir)))

;;; Json Exports

(defun package-build-recipe-alist-as-json (file)
  "Dump the recipe list to FILE as json."
  (interactive "FDump json to file: ")
  (with-temp-file file
    (insert
     (json-encode
      (cl-mapcan
       (lambda (name)
         (with-demoted-errors "Recipe error: %S"
           (and (package-recipe-lookup name)
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name name package-build-recipes-dir))
                  (let ((exp (read (current-buffer))))
                    (when (plist-member (cdr exp) :files)
                      (plist-put (cdr exp) :files
                                 (format "%S" (plist-get (cdr exp) :files))))
                    (list exp))))))
       (package-recipe-recipes))))))

(defun package-build--pkg-info-for-json (info)
  "Convert INFO so that it can be serialize to JSON in the desired shape."
  (pcase-let ((`(,ver ,deps ,desc ,type . (,props)) (append info nil)))
    (list :ver ver
          :deps (cl-mapcan (lambda (dep)
                             (list (intern (format ":%s" (car dep)))
                                   (cadr dep)))
                           deps)
          :desc desc
          :type type
          :props props)))

(defun package-build--archive-alist-for-json ()
  "Return the archive alist in a form suitable for JSON encoding."
  (cl-flet ((format-person
             (person)
             (let ((name (car person))
                   (mail (cdr person)))
               (if (and name mail)
                   (format "%s <%s>" name mail)
                 (or name
                     (format "<%s>" mail))))))
    (cl-mapcan (lambda (entry)
                 (list (intern (format ":%s" (car entry)))
                       (let* ((info (cdr entry))
                              (extra (aref info 4))
                              (maintainer (assq :maintainer extra))
                              (maintainers (assq :maintainers extra))
                              (authors (assq :authors extra)))
                         (when maintainer
                           (setcdr maintainer
                                   (format-person (cdr maintainer))))
                         (when maintainers
                           (if (cl-every #'listp (cdr maintainers))
                               (setcdr maintainers
                                       (mapcar #'format-person
                                               (cdr maintainers)))
                             (setq maintainers ; silence >= 30 compiler
                                   (assq-delete-all :maintainers extra))))
                         (when authors
                           (if (cl-every #'listp (cdr authors))
                               (setcdr authors
                                       (mapcar #'format-person (cdr authors)))
                             (setq authors ; silence >= 30 compiler
                                   (assq-delete-all :authors extra))))
                         (package-build--pkg-info-for-json info))))
               (package-build-archive-alist))))

(defun package-build-archive-alist-as-json (file)
  "Dump the build packages list to FILE as json."
  (with-temp-file file
    (insert (json-encode (package-build--archive-alist-for-json)))))

;;; _

(provide 'package-build)
;;; package-build.el ends here
