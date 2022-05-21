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

;; Package-Version: 3.1-git
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

(defconst package-build--melpa-base
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
  "The function used to determine the revision and version of a package.

The default depends on the value of option `package-build-stable'.

This function is called with one argument, the recipe object, and
must return (REVISION . VERSION), where REVISION is the \"current\"
revision according to some definition of the authors choosing and
VERSION is the version string corresponding to that.

REVISION should be determined first.  If it is necessary for that
to be checked out to determine VERSION, then this function has to
do so by calling `package-build--checkout-1'.  If not, then this
step can be omitted.  Note that a helper functions might call the
checkout function themselves; `package-build--get-timestamp' does.

It might be appropriate to respect the `commit' and `branch' slots
of the recipe."
  :group 'package-build
  :set-after '(package-build-stable)
  :type 'function)

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

(defvar package-build-use-hg-purge
  "Whether `package-build--package' runs \"hg purge\" in mercurial repos."
  (let ((value (ignore-errors
                 (car (process-lines "hg" "config" "extensions.purge")))))
    (and value (not (string-prefix-p "!" value)))))

;;; Generic Utilities

(defun package-build--message (format-string &rest args)
  "Behave like `message' if `package-build-verbose' is non-nil.
Otherwise do nothing.  FORMAT-STRING and ARGS are as per that function."
  (when package-build-verbose
    (apply #'message format-string args)))

;;; Version Handling
;;;; Public

(defun package-build-get-tag-version (rcp)
  (pcase-let ((`(,tag . ,version)
               (package-build--find-version-newest
                (package-build--list-tags rcp)
                (oref rcp version-regexp))))
    (unless tag
      (error "No valid stable versions found for %s" (oref rcp name)))
    (when (cl-typep rcp 'package-git-recipe)
      (setq tag (concat "tags/" tag)))
    (cons (package-build--get-commit rcp tag)
          version)))

(defun package-build-get-timestamp-version (rcp)
  (let ((rev (and (cl-typep rcp 'package-git-recipe)
                  (or (oref rcp commit)
                      (when-let ((branch (oref rcp branch)))
                        (concat "origin/" branch))
                      "origin/HEAD"))))
    (cons (package-build--get-commit rcp rev)
          (package-build--parse-time
           (package-build--get-timestamp rcp rev)
           (oref rcp time-regexp)))))

;;;; Internal

(defun package-build--parse-time (str &optional regexp)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string.
Always use Coordinated Universal Time (UTC) for output string.
If REGEXP is provided, it is applied to STR and the function
parses the first match group instead of STR."
  (unless str
    (error "No valid timestamp found"))
  (setq str (substring-no-properties str))
  (when regexp
    (if (string-match regexp str)
        (setq str (match-string 1 str))
      (error "No valid timestamp found")))
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (let ((time (date-to-time
               (if (string-match "\
^\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\) \
\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)$" str)
                   (concat (match-string 1 str) "-" (match-string 2 str) "-"
                           (match-string 3 str) " " (match-string 4 str))
                 str))))
    (concat (format-time-string "%Y%m%d." time t)
            (format "%d" (string-to-number (format-time-string "%H%M" time t))))))

(defun package-build--find-version-newest (tags &optional regexp)
  "Find the newest version in TAGS matching REGEXP.
If optional REGEXP is nil, then `package-build-version-regexp'
is used instead."
  (let ((ret '(nil 0))
        (regexp (or regexp package-build-version-regexp)))
    (cl-flet ((match (regexp separator tag)
                (let* ((version-string (and (string-match regexp tag)
                                            (match-string 1 tag)))
                       (version-separator separator)
                       (version (ignore-errors (version-to-list version-string))))
                  (when (and version (version-list-<= (cdr ret) version))
                    (setq ret (cons tag version))))))
      (dolist (tag tags)
        (match regexp "." tag)
        ;; Some version tags use "_" as version separator instead of
        ;; the default ".", e.g. "1_4_5".  Check for valid versions
        ;; again, this time using "_" as a `version-separator'.
        ;; Since "_" is otherwise treated as a snapshot separator by
        ;; `version-regexp-alist', we don't have to worry about the
        ;; incorrect version list above `(1 -4 4 -4 5)' since it will
        ;; always be treated as smaller by `version-list-<'.
        (match regexp "_" tag)))
    (and (car ret)
         (cons (car ret)
               (package-version-join (cdr ret))))))

;;; Run Process

(defun package-build--run-process (directory destination command &rest args)
  (setq directory (file-name-as-directory (or directory default-directory)))
  (with-current-buffer
      (if (eq destination t)
          (current-buffer)
        (or destination (get-buffer-create "*package-build-checkout*")))
    (unless destination
      (setq default-directory directory))
    (let ((default-directory directory)
          (argv (nconc (unless (eq system-type 'windows-nt)
                         (list "env" "LC_ALL=C"))
                       (if (and package-build-timeout-secs
                                package-build-timeout-executable)
                           (nconc (list package-build-timeout-executable
                                        "-k" "60" (number-to-string
                                                   package-build-timeout-secs)
                                        command)
                                  args)
                         (cons command args)))))
      (unless (file-directory-p default-directory)
        (error "Can't run process in non-existent directory: %s" default-directory))
      (let ((exit-code (apply #'call-process
                              (car argv) nil (current-buffer) nil
                              (cdr argv))))
        (unless (zerop exit-code)
          (message "\nCommand '%s' exited with non-zero exit-code: %d\n"
                   (mapconcat #'shell-quote-argument argv " ")
                   exit-code)
          (message "%s" (buffer-string))
          (error "Command exited with non-zero exit-code: %d" exit-code))))))

;;; Checkout
;;;; Common

(cl-defmethod package-build--checkout :before ((rcp package-recipe))
  (package-build--message "Package: %s" (oref rcp name))
  (package-build--message "Fetcher: %s" (package-recipe--fetcher rcp))
  (package-build--message "Source:  %s\n" (package-recipe--upstream-url rcp)))

(cl-defmethod package-build--checkout-1 :before ((_rcp package-recipe) rev)
  (when rev
    (package-build--message "Checking out %s" rev)))

;;;; Git

(cl-defmethod package-build--checkout ((rcp package-git-recipe))
  (let ((dir (package-recipe--working-tree rcp))
        (url (package-recipe--upstream-url rcp))
        (protocol (package-recipe--upstream-protocol rcp)))
    (unless (member protocol package-build-allowed-git-protocols)
      (error "Fetching using the %s protocol is not allowed" protocol))
    (cond
     ((and (file-exists-p (expand-file-name ".git" dir))
           (string-equal (package-build--used-url rcp) url))
      (package-build--message "Updating %s" dir)
      (package-build--run-process dir nil "git" "fetch" "-f" "--all" "--tags")
      ;; We might later checkout "origin/HEAD". Sadly "git fetch"
      ;; cannot be told to keep it up-to-date, so we have to make
      ;; a second request.
      (package-build--run-process dir nil "git" "remote" "set-head"
                                  "origin" "--auto"))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (apply #'package-build--run-process nil nil "git" "clone" url dir
             ;; This can dramatically reduce the size of large repos.
             ;; But we can only do this when using a version function
             ;; that is known not to require a checkout and history.
             ;; See #52.
             (and (eq package-build-get-version-function
                      #'package-build-get-tag-version)
                  (list "--filter=blob:none" "--no-checkout")))))
    (pcase-let ((`(,rev . ,version)
                 (funcall package-build-get-version-function rcp)))
      (package-build--checkout-1 rcp rev)
      version)))

(cl-defmethod package-build--checkout-1 ((rcp package-git-recipe) rev)
  (let ((dir (package-recipe--working-tree rcp)))
    (package-build--run-process dir nil "git" "reset" "--hard" rev)
    (package-build--run-process dir nil "git" "submodule" "sync" "--recursive")
    (package-build--run-process dir nil "git" "submodule" "update"
                                "--init" "--recursive")))

(cl-defmethod package-build--list-tags ((rcp package-git-recipe))
  (let ((default-directory (package-recipe--working-tree rcp)))
    (process-lines "git" "tag")))

(cl-defmethod package-build--get-timestamp ((rcp package-git-recipe) rev)
  (let ((default-directory (package-recipe--working-tree rcp)))
    ;; package-build--expand-source-file-list expects REV to be checked out.
    (package-build--checkout-1 rcp rev)
    (car (apply #'process-lines
                "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
                rev "--" (package-build--expand-source-file-list rcp)))))

(cl-defmethod package-build--used-url ((rcp package-git-recipe))
  (let ((default-directory (package-recipe--working-tree rcp)))
    (car (process-lines "git" "config" "remote.origin.url"))))

(cl-defmethod package-build--get-commit ((rcp package-git-recipe) &optional rev)
  (let ((default-directory (package-recipe--working-tree rcp)))
    (car (process-lines "git" "rev-parse" (or rev "HEAD")))))

;;;; Hg

(cl-defmethod package-build--checkout ((rcp package-hg-recipe))
  (let ((dir (package-recipe--working-tree rcp))
        (url (package-recipe--upstream-url rcp)))
    (cond
     ((and (file-exists-p (expand-file-name ".hg" dir))
           (string-equal (package-build--used-url rcp) url))
      (package-build--message "Updating %s" dir)
      (package-build--run-process dir nil "hg" "pull")
      (package-build--run-process dir nil "hg" "update"))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (package-build--run-process nil nil "hg" "clone" url dir)))
    (pcase-let ((`(,rev . ,version)
                 (funcall package-build-get-version-function rcp)))
      (package-build--checkout-1 rcp rev)
      version)))

(cl-defmethod package-build--checkout-1 ((rcp package-hg-recipe) rev)
  (when rev
    (package-build--run-process (package-recipe--working-tree rcp)
                                nil "hg" "update" rev)))

(cl-defmethod package-build--list-tags ((rcp package-hg-recipe))
  (let ((default-directory (package-recipe--working-tree rcp)))
    (mapcar (lambda (line)
              ;; Remove space and rev that follow ref.
              (string-match "\\`[^ ]+" line)
              (match-string 0))
            (process-lines "hg" "tags"))))

(cl-defmethod package-build--get-timestamp ((rcp package-hg-recipe) rev)
  (let ((default-directory (package-recipe--working-tree rcp)))
    (car (apply #'process-lines
                "hg" "log" "--style" "compact" "-l1"
                `(,@(and rev (list "--rev" rev))
                  ,@(package-build--expand-source-file-list rcp))))))

(cl-defmethod package-build--used-url ((rcp package-hg-recipe))
  (let ((default-directory (package-recipe--working-tree rcp)))
    (car (process-lines "hg" "paths" "default"))))

(cl-defmethod package-build--get-commit ((rcp package-hg-recipe) &optional rev)
  (let ((default-directory (package-recipe--working-tree rcp)))
    ;; "--debug" is needed to get the full hash.
    (car (apply #'process-lines "hg" "--debug" "id" "-i"
                (and rev (list rev))))))


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

(defun package-build--create-tar (name version directory)
  "Create a tar file containing the contents of VERSION of package NAME."
  (let ((tar (expand-file-name (concat name "-" version ".tar")
                               package-build-archive-dir))
        (dir (concat name "-" version)))
    (when (eq system-type 'windows-nt)
      (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file package-build-tar-executable nil
                    (get-buffer-create "*package-build-checkout*") nil
                    "-cvf" tar
                    "--exclude=.git"
                    "--exclude=.hg"
                    dir))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

(defun package-build--write-pkg-readme (name files directory)
  (when-let ((commentary
              (let* ((file (concat name ".el"))
                     (file (or (car (rassoc file files)) file))
                     (file (and file (expand-file-name file directory))))
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

(defun package-build--generate-info-files (files source-dir target-dir)
  "Create an info file for each texinfo file listed in FILES.
Also create the info dir file.  Remove each original texinfo
file.  The source and destination file paths are expanded in
SOURCE-DIR and TARGET-DIR respectively."
  (pcase-dolist (`(,src . ,tmp) files)
    (let ((extension (file-name-extension tmp)))
      (when (member extension '("info" "texi" "texinfo"))
        (let* ((src (expand-file-name src source-dir))
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
                (package-build--run-process
                 (file-name-directory texi) nil
                 "makeinfo" "--no-split" texi "-o" info))))
          (with-demoted-errors "Error: %S"
            (package-build--run-process
             target-dir nil "install-info" "--dir=dir" info)))))))

;;; Patch Libraries

(defun package-build--update-or-insert-header (name value)
  "Ensure current buffer has NAME header with the given VALUE.
Any existing header will be preserved and given the \"X-Original-\" prefix.
If VALUE is nil, the new header will not be inserted, but any original will
still be renamed."
  (goto-char (point-min))
  (if (let ((case-fold-search t))
        (re-search-forward (concat "^;+* *" (regexp-quote name)  " *: *") nil t))
      (progn
        (move-beginning-of-line nil)
        (search-forward "V" nil t)
        (backward-char)
        (insert "X-Original-")
        (move-beginning-of-line nil))
    ;; Put the new header in a sensible place if we can
    (re-search-forward "^;+* *\\(Version\\|Package-Requires\\|Keywords\\|URL\\) *:"
                       nil t)
    (forward-line))
  (insert (format ";; %s: %s" name value))
  (newline))

(defun package-build--ensure-ends-here-line (file)
  "Add a 'FILE ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let ((trailer (concat ";;; "
                           (file-name-nondirectory file)
                           " ends here")))
      (unless (search-forward trailer nil t)
        (goto-char (point-max))
        (newline)
        (insert trailer)
        (newline)))))

;;; Package Structs

(defun package-build--desc-from-library (name version commit files &optional type)
  (let* ((file (concat name ".el"))
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
            :kind       (or type 'single)
            :url        (lm-homepage)
            :keywords   (lm-keywords-list)
            :maintainer (if (fboundp 'lm-maintainers)
                            (car (lm-maintainers))
                          (with-no-warnings
                            (lm-maintainer)))
            :authors    (lm-authors)
            :commit     commit)))))

(defun package-build--desc-from-package (name version commit files)
  (let* ((file (concat name "-pkg.el"))
         (file (or (car (rassoc file files))
                   file)))
    (and (or (file-exists-p file)
             (file-exists-p (setq file (concat file ".in"))))
         (let ((form (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer)))))
           (unless (eq (car form) #'define-package)
             (error "No define-package found in %s" file))
           (pcase-let*
               ((`(,_ ,_ ,_ ,summary ,deps . ,extra) form)
                (deps (eval deps))
                (alt-desc (package-build--desc-from-library
                           name version nil files))
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

;;; File Specs

(defconst package-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun package-build-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (package-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key #'car
                                        :test #'equal)
                  (nconc lst
                         (package-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun package-build--config-file-list (rcp)
  (let ((file-list (oref rcp files)))
    (cond
     ((null file-list)
      package-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append package-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun package-build--expand-source-file-list (rcp)
  (mapcar #'car
          (package-build-expand-file-specs
           (package-recipe--working-tree rcp)
           (package-build--config-file-list rcp))))

(defun package-build--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (package-build--message
   "Copying files (->) and directories (=>)\n  from %s\n  to %s"
   source-dir target-dir)
  (pcase-dolist (`(,src . ,dst) files)
    (let ((src* (expand-file-name src source-dir))
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

;;; Commands

;;;###autoload
(defun package-build-archive (name &optional dump-archive-contents)
  "Build a package archive for the package named NAME.
If DUMP-ARCHIVE-CONTENTS is non-nil, the updated archive contents
are subsequently dumped."
  (interactive (list (package-recipe-read-name) t))
  (let ((start-time (current-time))
        (rcp (package-recipe-lookup name)))
    (unless (file-exists-p package-build-archive-dir)
      (package-build--message "Creating directory %s" package-build-archive-dir)
      (make-directory package-build-archive-dir))
    (let ((default-directory package-build-working-dir)
          (version (package-build--checkout rcp)))
      (package-build--package rcp version)
      (when package-build-write-melpa-badge-images
        (package-build--write-melpa-badge-image
         name version package-build-archive-dir))
      (package-build--message "Built %s in %.3fs, finished at %s"
                              name
                              (float-time (time-since start-time))
                              (format-time-string "%FT%T%z" nil t))))
  (when dump-archive-contents
    (package-build-dump-archive-contents)))

;;;###autoload
(defun package-build--package (rcp version)
  "Create version VERSION of the package specified by RCP.
Return the archive entry for the package and store the package
in `package-build-archive-dir'."
  (let ((source-dir (package-recipe--working-tree rcp)))
    (unwind-protect
        (let* ((file-specs (package-build--config-file-list rcp))
               (files (package-build-expand-file-specs source-dir file-specs))
               (commit (package-build--get-commit rcp))
               (name (oref rcp name)))
          (unless (equal file-specs package-build-default-files-spec)
            (when (equal files (package-build-expand-file-specs
                                source-dir
                                package-build-default-files-spec
                                nil t))
              (package-build--message
               "Note: %s :files spec is equivalent to the default." name)))
          (cond
           ((not version)
            (error "Unable to check out repository for %s" name))
           ((= (length files) 1)
            (package-build--build-single-file-package
             rcp version commit files source-dir))
           ((> (length files) 1)
            (package-build--build-multi-file-package
             rcp version commit files source-dir))
           (t (error "Unable to find files matching recipe patterns"))))
      (cond ((cl-typep rcp 'package-git-recipe)
             (package-build--run-process
              source-dir nil "git" "clean" "-f" "-d" "-x"))
            ((and (cl-typep rcp 'package-hg-recipe)
                  package-build-use-hg-purge)
             (package-build--run-process source-dir nil "hg" "purge"))))))

(defun package-build--build-single-file-package (rcp version commit files source-dir)
  (let* ((name (oref rcp name))
         (file (caar files))
         (source (expand-file-name file source-dir))
         (target (expand-file-name (concat name "-" version ".el")
                                   package-build-archive-dir))
         (desc (let ((default-directory source-dir))
                 (package-build--desc-from-library
                  name version commit files))))
    (unless (string-equal (downcase (concat name ".el"))
                          (downcase (file-name-nondirectory file)))
      (error "Single file %s does not match package name %s" file name))
    (copy-file source target t)
    (let ((enable-local-variables nil)
          (make-backup-files nil))
      (with-current-buffer (find-file target)
        (package-build--update-or-insert-header "Package-Commit" commit)
        (package-build--update-or-insert-header "Package-Version" version)
        (package-build--ensure-ends-here-line source)
        (write-file target nil)
        (kill-buffer)))
    (package-build--write-pkg-readme name files source-dir)
    (package-build--write-archive-entry desc)))

(defun package-build--build-multi-file-package (rcp version commit files source-dir)
  (let* ((name (oref rcp name))
         (tmp-dir (file-name-as-directory (make-temp-file name t))))
    (unwind-protect
        (let* ((target (expand-file-name (concat name "-" version) tmp-dir))
               (desc (let ((default-directory source-dir))
                       (or (package-build--desc-from-package
                            name version commit files)
                           (package-build--desc-from-library
                            name version commit files 'tar)
                           (error "%s[-pkg].el matching package name is missing"
                                  name)))))
          (package-build--copy-package-files files source-dir target)
          (package-build--write-pkg-file desc target)
          (package-build--generate-info-files files source-dir target)
          (package-build--create-tar name version tmp-dir)
          (package-build--write-pkg-readme name files source-dir)
          (package-build--write-archive-entry desc))
      (delete-directory tmp-dir t nil))))

;;;###autoload
(defun package-build-all ()
  "Build a package for each of the available recipes."
  (interactive)
  (let* ((recipes (package-recipe-recipes))
         (total (length recipes))
         (success 0)
         invalid failed)
    (dolist (name recipes)
      (let ((rcp (with-demoted-errors "Build error: %S"
                   (package-recipe-lookup name))))
        (if rcp
            (if (with-demoted-errors "Build error: %S"
                  (package-build-archive name) t)
                (cl-incf success)
              (push name failed))
          (push name invalid))))
    (if (not (or invalid failed))
        (message "Successfully built all %s packages" total)
      (message "Successfully built %i of %s packages" success total)
      (when invalid
        (message "Did not built packages for %i invalid recipes:\n%s"
                 (length invalid)
                 (mapconcat (lambda (n) (concat "  " n)) invalid "\n")))
      (when failed
        (message "Building %i packages failed:\n%s"
                 (length failed)
                 (mapconcat (lambda (n) (concat "  " n)) failed "\n")))))
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

(define-obsolete-function-alias 'package-build--archive-entries
  #'package-build-dump-archive-contents "Package-Build 3.0")

(provide 'package-build)
;;; package-build.el ends here
