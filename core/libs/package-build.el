;;; package-build.el --- Tools for assembling a package archive  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2011-2022 Donald Ephraim Curtis
;; Copyright (C) 2012-2022 Steve Purcell
;; Copyright (C) 2016-2022 Jonas Bernoulli
;; Copyright (C) 2009 Phil Hagelberg

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;;     Steve Purcell <steve@sanityinc.com>
;;     Jonas Bernoulli <jonas@bernoul.li>
;;     Phil Hagelberg <technomancy@gmail.com>
;; Homepage: https://github.com/melpa/package-build
;; Keywords: maint tools

;; Package-Version: 4.0.0.50-git
;; Package-Requires: ((emacs "25.1"))

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
  "When non-nil, then try to build packages from versions-tagged code."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-get-version-function
  (if package-build-stable
      'package-build-get-tag-version
    'package-build-get-timestamp-version)
  "The function used to determine the commit and version of a package.

The default depends on the value of option `package-build-stable'.

This function is called with one argument, the recipe object,
and must return (COMMIT TIME VERSION), where COMMIT is the commit
choosen by the function, TIME is its commit date, and VERSION is
the version string choosen for COMMIT."
  :group 'package-build
  :set-after '(package-build-stable)
  :type 'function)

(defcustom package-build-predicate-function nil
  "Predicate used by `package-build-all' to determine which packages to build.
If non-nil, this function is called with the recipe object as
argument, and must return non-nil if the package is to be build.
If nil (the default), then all packages are build."
  :group 'package-build
  :type '(choice (const :tag "build all") function))

(defcustom package-build-build-function nil
  "Low-level function used to build a package.
If nil (the default) then the funcion used depends on whether the
package consists of more than one file or not.  One possible value
is `package-build--build-multi-file-package', which would force
building a tarball, even for packages that consist of a single
file."
  :group 'package-build
  :type '(choice (const :tag "default, depending on number of files")
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
Certain package names (e.g. \"@\") may not work properly with a BSD tar.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtar\"."
  :group 'package-build
  :type '(file :must-match t))

(defcustom package-build-write-melpa-badge-images nil
  "When non-nil, write MELPA badge images alongside packages.
These batches can, for example, be used on GitHub pages."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
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

;;; Generic Utilities

(defun package-build--message (format-string &rest args)
  "Behave like `message' if `package-build-verbose' is non-nil.
Otherwise do nothing.  FORMAT-STRING and ARGS are as per that function."
  (when package-build-verbose
    (apply #'message format-string args)))

;;; Version Handling
;;;; Common

(defun package-build--select-version (rcp)
  (pcase-let* ((default-directory (package-build--working-tree rcp t))
               (`(,commit ,time ,version)
                (funcall package-build-get-version-function rcp)))
    (unless version
      (error "Cannot detect version for %s" (oref rcp name)))
    (oset rcp commit commit)
    (oset rcp time time)
    (oset rcp version version)))

(cl-defmethod package-build--select-commit ((rcp package-git-recipe) rev exact)
  (pcase-let*
      ((`(,hash ,time)
        (split-string
         (car (apply #'process-lines
                     "git" "log" "-n1" "--first-parent"
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

;;;; Release

(defun package-build-get-tag-version (rcp)
  (let ((regexp (or (oref rcp version-regexp) package-build-version-regexp))
        (tag nil)
        (version '(0)))
    (dolist (n (cl-etypecase rcp
                 (package-git-recipe (process-lines "git" "tag" "--list"))
                 (package-hg-recipe  (process-lines "hg" "tags" "--quiet"))))
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

;;;; Snapshot

(defun package-build-get-timestamp-version (rcp)
  (pcase-let ((`(,hash ,time) (package-build--get-timestamp-version rcp)))
    (list hash time
          ;; We remove zero-padding of the HH portion, as
          ;; that is lost when stored in archive-contents.
          (concat (format-time-string "%Y%m%d." time t)
                  (format "%d" (string-to-number
                                (format-time-string "%H%M" time t)))))))

(cl-defmethod package-build--get-timestamp-version ((rcp package-git-recipe))
  (pcase-let*
      ((commit (oref rcp commit))
       (branch (oref rcp branch))
       (branch (and branch (concat "origin/" branch)))
       (rev (or commit branch "origin/HEAD"))
       (`(,rev-hash ,rev-time) (package-build--select-commit rcp rev commit))
       (`(,tag-hash ,tag-time) (package-build-get-tag-version rcp)))
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

(cl-defmethod package-build--get-timestamp-version ((rcp package-hg-recipe))
  ;; TODO Respect commit and branch properties.
  ;; TODO Use latest release if appropriate.
  (package-build--select-commit rcp "." nil))

;;; Run Process

(defun package-build--run-process (command &rest args)
  "Run COMMAND with ARGS in `default-directory'.
We use this to wrap commands is proper environment settings and
with a timeout so that no command can block the build process."
  (unless (file-directory-p default-directory)
    (error "Cannot run process in non-existent directory: %s"
           default-directory))
  (with-temp-buffer
    (pcase-let* ((`(,command . ,args)
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
      (unless (zerop exit-code)
        (message "\nCommand '%s' exited with non-zero exit-code: %d\n"
                 (mapconcat #'shell-quote-argument argv " ")
                 exit-code)
        (message "%s" (buffer-string))
        (error "Command exited with non-zero exit-code: %d" exit-code)))))

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
      (error "Fetching using the %s protocol is not allowed" protocol))
    (cond
     ((and (file-exists-p (expand-file-name ".git" dir))
           (let ((default-directory dir))
             (string= (car (process-lines "git" "config" "remote.origin.url"))
                      url)))
      (unless package-build--inhibit-fetch
        (let ((default-directory dir))
          (package-build--message "Updating %s" dir)
          (package-build--run-process "git" "fetch" "-f" "--all" "--tags")
          ;; We might later checkout "origin/HEAD". Sadly "git fetch"
          ;; cannot be told to keep it up-to-date, so we have to make
          ;; a second request.
          (package-build--run-process "git" "remote" "set-head"
                                      "origin" "--auto"))))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (let ((default-directory package-build-working-dir))
        (apply #'package-build--run-process "git" "clone" url dir
               ;; This can dramatically reduce the size of large repos.
               ;; But we can only do this when using a version function
               ;; that is known not to require a checkout and history.
               ;; See #52.
               (and (eq package-build-get-version-function
                        #'package-build-get-tag-version)
                    (list "--filter=blob:none" "--no-checkout"))))))))

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
          (package-build--run-process "hg" "pull")
          (package-build--run-process "hg" "update"))))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (let ((default-directory package-build-working-dir))
        (package-build--run-process "hg" "clone" url dir))))))

;;; Checkout

(cl-defmethod package-build--checkout ((rcp package-git-recipe))
  (unless package-build--inhibit-checkout
    (let ((rev (oref rcp commit)))
      (package-build--message "Checking out %s" rev)
      (package-build--run-process "git" "reset" "--hard" rev))))

(cl-defmethod package-build--checkout ((rcp package-hg-recipe))
  (unless package-build--inhibit-checkout
    (let ((rev (oref rcp commit)))
      (package-build--message "Checking out %s" rev)
      (package-build--run-process "hg" "update" rev))))

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
    (when (eq system-type 'windows-nt)
      (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file
       package-build-tar-executable nil
       (get-buffer-create "*package-build-checkout*") nil
       "-cf" tar dir
       ;; Arguments that are need to strip metadata that
       ;; prevent a reproducable tarball as described at
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

(defun package-build--generate-info-files (files target-dir)
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
                  (package-build--run-process
                   "makeinfo" "--no-split" texi "-o" info)))))
          (with-demoted-errors "Error: %S"
            (let ((default-directory target-dir))
              (package-build--run-process
               "install-info" "--dir=dir" info))))))))

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
         (file (or (car (rassoc file files)) file)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (package-desc-from-define
            name version
            (or (save-excursion
                  (goto-char (point-min))
                  (and (re-search-forward
                        "^;;; [^ ]*\\.el ---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$"
                        nil t)
                       (match-string-no-properties 1)))
                "No description available.")
            (when-let ((require-lines (lm-header-multiline "package-requires")))
              (package--prepare-dependencies
               (package-read-from-string (mapconcat #'identity require-lines " "))))
            :kind       (or kind 'single)
            :url        (lm-homepage)
            :keywords   (lm-keywords-list)
            :maintainer (if (fboundp 'lm-maintainers)
                            (car (lm-maintainers))
                          (with-no-warnings
                            (lm-maintainer)))
            :authors    (lm-authors)
            :commit     commit)))))

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
             (error "No define-package found in %s" file))
           (pcase-let*
               ((`(,_ ,_ ,_ ,summary ,deps . ,extra) form)
                (deps (eval deps))
                (alt-desc (package-build--desc-from-library rcp files))
                (alt (and alt-desc (package-desc-extras alt-desc))))
             (when (string-match "[\r\n]" summary)
               (error "Illegal multi-line package description in %s" file))
             (package-desc-from-define
              name version
              (if (string-empty-p summary)
                  (or (and alt-desc (package-desc-summary alt-desc))
                      "No description available.")
                summary)
              (mapcar (pcase-lambda (`(,pkg ,ver))
                        (unless (symbolp pkg)
                          (error "Invalid package name in dependency: %S" pkg))
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
        (spec (or spec (oref rcp files))))
    (when (eq (car spec) :defaults)
      (setq spec (append package-build-default-files-spec (cdr spec))))
    (let ((files (package-build--expand-files-spec-1
                  (or spec package-build-default-files-spec))))
      (when assert
        (when (and rcp spec
                   (equal files (package-build--expand-files-spec-1
                                 package-build-default-files-spec)))
          (message "Warning: %s :files spec is equivalent to the default"
                   (oref rcp name)))
        (unless files
          (error "No matching file(s) found in %s using %s"
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
         (fetcher (package-recipe--fetcher rcp)))
    (cond ((not noninteractive)
           (message " • Building package %s (from %s)..." name
                    (if repo (format "%s:%s" fetcher repo) url)))
          (package-build-verbose
           (message "Package: %s" name)
           (message "Fetcher: %s" fetcher)
           (message "Source:  %s\n" url)))
    (funcall package-build-fetch-function rcp)
    (package-build--select-version rcp)
    (package-build--package rcp)
    (when dump-archive-contents
      (package-build-dump-archive-contents))
    (message "Built %s in %.3fs, finished at %s" name
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
              (error "Unable to find files matching recipe patterns"))
             (package-build-build-function
              (funcall package-build-build-function))
             ((= (length files) 1)
              (package-build--build-single-file-package rcp files))
             (t
              (package-build--build-multi-file-package rcp files)))
            (when package-build-write-melpa-badge-images
              (package-build--write-melpa-badge-image
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
      (error "Single file %s does not match package name %s" file name))
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
                         (error "%s[-pkg].el matching package name is missing"
                                name))))
          (package-build--copy-package-files files target)
          (package-build--write-pkg-file desc target)
          (package-build--generate-info-files files target)
          (package-build--create-tar rcp tmp-dir)
          (package-build--write-pkg-readme rcp files)
          (package-build--write-archive-entry desc))
      (delete-directory tmp-dir t nil))))

(defun package-build--cleanup (rcp)
  (cond ((cl-typep rcp 'package-git-recipe)
         (package-build--run-process "git" "clean" "-f" "-d" "-x"))
        ((cl-typep rcp 'package-hg-recipe)
         (package-build--run-process "hg" "purge"))))

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
  (package-build-cleanup))

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

If non-nil, then store the archive contents in FILE instead of in
the \"archive-contents\" file inside `package-build-archive-dir'.
If PRETTY-PRINT is non-nil, then pretty-print instead of using one
line per entry."
  (let (entries)
    (dolist (file (sort (directory-files package-build-archive-dir t ".*\.entry$")
                        ;; Sort more recently-build packages first
                        (lambda (f1 f2)
                          (let ((default-directory package-build-archive-dir))
                            (file-newer-than-file-p f1 f2)))))
      (let* ((entry (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))
             (name (car entry))
             (newer-entry (assq name entries)))
        (if (not (file-exists-p (expand-file-name (symbol-name name)
                                                  package-build-recipes-dir)))
            (package-build--remove-archive-files entry)
          ;; Prefer the more-recently-built package, which may not
          ;; necessarily have the highest version number, e.g. if
          ;; commit histories were changed.
          (if newer-entry
              (package-build--remove-archive-files entry)
            (push entry entries)))))
    (setq entries (sort entries (lambda (a b)
                                  (string< (symbol-name (car a))
                                           (symbol-name (car b))))))
    (with-temp-file
        (or file
            (expand-file-name "archive-contents" package-build-archive-dir))
      (let ((print-level nil)
            (print-length nil))
        (if pretty-print
            (pp (cons 1 entries) (current-buffer))
          (insert "(1")
          (dolist (entry entries)
            (newline)
            (insert " ")
            (prin1 entry (current-buffer)))
          (insert ")"))))
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
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (pcase-let* ((`(,name . ,desc) archive-entry)
               (version (package-version-join (aref desc 0)))
               (flavour (aref desc 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     package-build-archive-dir)))

(defun package-build--archive-entry-file (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
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
         (ignore-errors ; Silently ignore corrupted recipes.
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
                              (authors (assq :authors extra)))
                         (when maintainer
                           (setcdr maintainer
                                   (format-person (cdr maintainer))))
                         (when authors
                           (if (cl-every #'listp (cdr authors))
                               (setcdr authors
                                       (mapcar #'format-person (cdr authors)))
                             (assq-delete-all :authors extra)))
                         (package-build--pkg-info-for-json info))))
               (package-build-archive-alist))))

(defun package-build-archive-alist-as-json (file)
  "Dump the build packages list to FILE as json."
  (with-temp-file file
    (insert (json-encode (package-build--archive-alist-for-json)))))

;;; _

(provide 'package-build)
;;; package-build.el ends here
