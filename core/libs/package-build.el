;;; package-build.el --- Tools for assembling a package archive

;; Copyright (C) 2011-2020 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012-2020 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2016-2020 Jonas Bernoulli <jonas@bernoul.li>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Keywords: tools
;; Package-Requires: ((cl-lib "0.5") (emacs "24.1"))

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from a set of recipes which describe elisp
;; projects and repositories from which to get them.  The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;;; Code:

(require 'cl-lib)

(require 'package)
(require 'lisp-mnt)
(require 'json)

(require 'package-recipe)

;;; Options

(defconst package-build--melpa-base
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name (buffer-file-name))))))

(defgroup package-build nil
  "Facilities for building package.el-compliant packages from upstream source code."
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

;;; Generic Utilities

(defun package-build--message (format-string &rest args)
  "Behave like `message' if `package-build-verbose' is non-nil.
Otherwise do nothing.  FORMAT-STRING and ARGS are as per that function."
  (when package-build-verbose
    (apply 'message format-string args)))

;;; Version Handling

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
  (let ((ret '(nil 0)))
    (dolist (tag tags)
      (string-match (or regexp package-build-version-regexp) tag)
      (let ((version (ignore-errors (version-to-list (match-string 1 tag)))))
        (when (and version (version-list-<= (cdr ret) version))
          (setq ret (cons tag version))))
      ;; Some version tags use "_" as version separator instead of
      ;; the default ".", e.g. "1_4_5".  Check for valid versions
      ;; again, this time using "_" as a `version-separator'.
      ;; Since "_" is otherwise treated as a snapshot separator by
      ;; `version-regexp-alist', we don't have to worry about the
      ;; incorrect version list above `(1 -4 4 -4 5)' since it will
      ;; always be treated as smaller by `version-list-<'.
      (string-match (or regexp package-build-version-regexp) tag)
      (let* ((version-separator "_")
             (version (ignore-errors (version-to-list (match-string 1 tag)))))
        (when (and version (version-list-<= (cdr ret) version))
          (setq ret (cons tag version)))))
    (and (car ret)
         (cons (car ret)
               (package-version-join (cdr ret))))))

;;; Run Process

(defun package-build--run-process (directory destination command &rest args)
  (with-current-buffer
      (if (eq destination t)
          (current-buffer)
        (or destination (get-buffer-create "*package-build-checkout*")))
    (let ((default-directory
            (file-name-as-directory (or directory default-directory)))
          (argv (nconc (unless (eq system-type 'windows-nt)
                         (list "env" "LC_ALL=C"))
                       (if (and package-build-timeout-secs package-build-timeout-executable)
                           (nconc (list package-build-timeout-executable
                                        "-k" "60" (number-to-string
                                                   package-build-timeout-secs)
                                        command)
                                  args)
                         (cons command args)))))
      (unless (file-directory-p default-directory)
        (error "Can't run process in non-existent directory: %s" default-directory))
      (let ((exit-code (apply 'process-file
                              (car argv) nil (current-buffer) t
                              (cdr argv))))
        (or (zerop exit-code)
            (error "Command '%s' exited with non-zero status %d: %s"
                   argv exit-code (buffer-string)))))))

(defun package-build--run-process-match (regexp directory command &rest args)
  (with-temp-buffer
    (apply 'package-build--run-process directory t command args)
    (goto-char (point-min))
    (re-search-forward regexp)
    (match-string-no-properties 1)))

(defun package-build--process-lines (directory command &rest args)
  (with-temp-buffer
    (apply 'package-build--run-process directory t command args)
    (split-string (buffer-string) "\n" t)))

;;; Checkout
;;;; Common

(defmethod package-build--checkout :before ((rcp package-recipe))
  (package-build--message "Package: %s" (oref rcp name))
  (package-build--message "Fetcher: %s"
                          (substring (symbol-name
                                      (with-no-warnings
                                        ;; Use eieio-object-class once we
                                        ;; no longer support Emacs 24.3.
                                        (object-class-fast rcp)))
                                     8 -7))
  (package-build--message "Source:  %s\n" (package-recipe--upstream-url rcp)))

;;;; Git

(defmethod package-build--checkout ((rcp package-git-recipe))
  (let ((dir (package-recipe--working-tree rcp))
        (url (package-recipe--upstream-url rcp)))
    (cond
     ((and (file-exists-p (expand-file-name ".git" dir))
           (string-equal (package-build--used-url rcp) url))
      (package-build--message "Updating %s" dir)
      (package-build--run-process dir nil "git" "fetch" "-f" "--all" "--tags"))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (package-build--message "Cloning %s to %s" url dir)
      (package-build--run-process nil nil "git" "clone" url dir)))
    (if package-build-stable
        (cl-destructuring-bind (tag . version)
            (or (package-build--find-version-newest
                 (let ((default-directory (package-recipe--working-tree rcp)))
                   (process-lines "git" "tag"))
                 (oref rcp version-regexp))
                (error "No valid stable versions found for %s" (oref rcp name)))
          (package-build--checkout-1 rcp (concat "tags/" tag))
          version)
      (package-build--checkout-1 rcp)
      (package-build--parse-time
       (car (apply #'package-build--process-lines dir
                   "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
                   (package-build--expand-source-file-list rcp)))
       (oref rcp tag-regexp)))))

(defmethod package-build--checkout-1 ((rcp package-git-recipe) &optional rev)
  (let ((dir (package-recipe--working-tree rcp)))
    (unless rev
      (setq rev (or (oref rcp commit)
                    (concat "origin/"
                            (or (oref rcp branch)
                                (ignore-errors
                                  (package-build--run-process-match
                                   "HEAD branch: \\(.*\\)" dir
                                   "git" "remote" "show" "origin"))
                                "master")))))
    (package-build--run-process dir nil "git" "reset" "--hard" rev)
    (package-build--run-process dir nil "git" "submodule" "sync" "--recursive")
    (package-build--run-process dir nil "git" "submodule" "update"
                                "--init" "--recursive")))

(defmethod package-build--used-url ((rcp package-git-recipe))
  (let ((default-directory (package-recipe--working-tree rcp)))
    (car (process-lines "git" "config" "remote.origin.url"))))

;;;; Hg

(defmethod package-build--checkout ((rcp package-hg-recipe))
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
    (if package-build-stable
        (cl-destructuring-bind (tag . version)
            (or (package-build--find-version-newest
                 (mapcar (lambda (line)
                           ;; Remove space and rev that follow ref.
                           (string-match "\\`[^ ]+" line)
                           (match-string 0))
                         (process-lines "hg" "tags"))
                 (oref rcp version-regexp))
                (error "No valid stable versions found for %s" (oref rcp name)))
          (package-build--run-process dir nil "hg" "update" tag)
          version)
      (package-build--parse-time
       (car (apply #'package-build--process-lines dir
                   "hg" "log" "--style" "compact" "-l1"
                   (package-build--expand-source-file-list rcp)))
       (oref rcp tag-regexp)))))

(defmethod package-build--used-url ((rcp package-hg-recipe))
  (package-build--run-process-match "default = \\(.*\\)"
                                    (package-recipe--working-tree rcp)
                                    "hg" "paths"))

;;; Various Files

(defun package-build--write-pkg-file (pkg-file pkg-info)
  "Write PKG-FILE containing PKG-INFO."
  (with-temp-file pkg-file
    (pp
     `(define-package
        ,(aref pkg-info 0)
        ,(aref pkg-info 3)
        ,(aref pkg-info 2)
        ',(mapcar
           (lambda (elt)
             (list (car elt)
                   (package-version-join (cadr elt))))
           (aref pkg-info 1))
        ;; Append our extra information
        ,@(cl-mapcan (lambda (entry)
                       (let ((value (cdr entry)))
                         (when (or (symbolp value) (listp value))
                           ;; We must quote lists and symbols,
                           ;; because Emacs 24.3 and earlier evaluate
                           ;; the package information, which would
                           ;; break for unquoted symbols or lists
                           (setq value (list 'quote value)))
                         (list (car entry) value)))
                     (when (> (length pkg-info) 4)
                       (aref pkg-info 4))))
     (current-buffer))
    (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n"
           (current-buffer))))

(defun package-build--create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
  (when (eq system-type 'windows-nt)
    (setq file (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" file)))
  (apply 'process-file
         package-build-tar-executable nil
         (get-buffer-create "*package-build-checkout*")
         nil "-cvf"
         file
         "--exclude=.git"
         "--exclude=.hg"
         (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))

(defun package-build--find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun package-build--write-pkg-readme (target-dir commentary file-name)
  "In TARGET-DIR, write COMMENTARY to a -readme.txt file prefixed with FILE-NAME."
  (when commentary
    (with-temp-buffer
      (insert commentary)
      ;; Adapted from `describe-package-1'.
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "^;;; Commentary:\n" nil t)
          (replace-match ""))
        (while (re-search-forward "^\\(;+ ?\\)" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (when (re-search-forward "\\`\\( *\n\\)+" nil t)
          (replace-match "")))
      (delete-trailing-whitespace)
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region nil nil
                      (expand-file-name (concat file-name "-readme.txt")
                                        target-dir))))))

;;; Entries

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

(defun package-build--ensure-ends-here-line (file-path)
  "Add a 'FILE-PATH ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let ((trailer (concat ";;; "
                           (file-name-nondirectory file-path)
                           " ends here")))
      (unless (search-forward trailer nil t)
        (goto-char (point-max))
        (newline)
        (insert trailer)
        (newline)))))

(defun package-build--get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (package-build--update-or-insert-header "Package-Version" "0")
        (package-build--ensure-ends-here-line file-path)
        (cl-flet ((package-strip-rcs-id (str) "0"))
          (package-build--package-buffer-info-vec))))))

(defun package-build--package-buffer-info-vec ()
  "Return a vector of package info.
`package-buffer-info' returns a vector in older Emacs versions,
and a cl struct in Emacs HEAD.  This wrapper normalises the results."
  (let ((desc (package-buffer-info))
        (keywords (lm-keywords-list)))
    (if (fboundp 'package-desc-create)
        (let ((extras (package-desc-extras desc)))
          (when (and keywords (not (assq :keywords extras)))
            (push (cons :keywords keywords) extras))
          (vector (package-desc-name desc)
                  (package-desc-reqs desc)
                  (package-desc-summary desc)
                  (package-desc-version desc)
                  extras))
      (let ((homepage (package-build--lm-homepage))
            extras)
        (when keywords (push (cons :keywords keywords) extras))
        (when homepage (push (cons :url homepage) extras))
        (vector  (aref desc 0)
                 (aref desc 1)
                 (aref desc 2)
                 (aref desc 3)
                 extras)))))

(defun package-build--get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (with-temp-buffer
                         (insert-file-contents file-path)
                         (read (current-buffer)))))
      (if (eq 'define-package (car package-def))
          (let* ((pkgfile-info (cdr package-def))
                 (descr (nth 2 pkgfile-info))
                 (rest-plist (cl-subseq pkgfile-info (min 4 (length pkgfile-info))))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (let ((value (cadr rest-plist)))
                                 (when value
                                   (push (cons (car rest-plist)
                                               (if (eq (car-safe value) 'quote)
                                                   (cadr value)
                                                 value))
                                         alist))))
                             (setq rest-plist (cddr rest-plist)))
                           alist)))
            (when (string-match "[\r\n]" descr)
              (error "Illegal multi-line package description in %s" file-path))
            (vector
             (nth 0 pkgfile-info)
             (mapcar
              (lambda (elt)
                (unless (symbolp (car elt))
                  (error "Invalid package name in dependency: %S" (car elt)))
                (list (car elt) (version-to-list (cadr elt))))
              (eval (nth 3 pkgfile-info)))
             descr
             (nth 1 pkgfile-info)
             extras))
        (error "No define-package found in %s" file-path)))))

(defun package-build--merge-package-info (pkg-info name version commit)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let ((merged (or (copy-sequence pkg-info)
                    (vector name nil "No description available." version nil))))
    (aset merged 0 name)
    (aset merged 3 version)
    (when commit
      (aset merged 4 (cons (cons :commit commit) (elt pkg-info 4))))
    merged))

(defun package-build--write-archive-entry (rcp pkg-info type)
  (let ((entry (package-build--archive-entry rcp pkg-info type)))
    (with-temp-file (package-build--archive-entry-file entry)
      (print entry (current-buffer)))))

(defmethod package-build--get-commit ((rcp package-git-recipe))
  (ignore-errors
    (package-build--run-process-match
     "\\(.*\\)"
     (package-recipe--working-tree rcp)
     "git" "rev-parse" "HEAD")))

(defmethod package-build--get-commit ((rcp package-hg-recipe))
  (ignore-errors
    (package-build--run-process-match
     "changeset:[[:space:]]+[[:digit:]]+:\\([[:xdigit:]]+\\)"
     (package-recipe--working-tree rcp)
     "hg" "log" "--debug" "--limit=1")))

(defun package-build--archive-entry (rcp pkg-info type)
  (let ((name (intern (aref pkg-info 0)))
        (requires (aref pkg-info 1))
        (desc (or (aref pkg-info 2) "No description available."))
        (version (aref pkg-info 3))
        (extras (and (> (length pkg-info) 4)
                     (aref pkg-info 4))))
    (cons name
          (vector (version-to-list version)
                  requires
                  desc
                  type
                  extras))))

(defun package-build--artifact-file (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     package-build-archive-dir)))

(defun package-build--archive-entry-file (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0))))
    (expand-file-name
     (format "%s-%s.entry" name version)
     package-build-archive-dir)))

;;; File Specs

(defconst package-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
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
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (package-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (package-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (let ((destname)))
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
  (mapcar 'car
          (package-build-expand-file-specs
           (package-recipe--working-tree rcp)
           (package-build--config-file-list rcp))))

;;; Info Manuals

(defun package-build--generate-info-files (files source-dir target-dir)
  "Create .info files from any .texi files listed in FILES.

The source and destination file paths are expanded in SOURCE-DIR
and TARGET-DIR respectively.

Any of the original .texi(nfo) files found in TARGET-DIR are
deleted."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (source-path (expand-file-name source-file source-dir))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (string-match ".texi\\(nfo\\)?$" source-file)
        (unless (file-exists-p info-path)
          (ignore-errors
            (package-build--run-process
             (file-name-directory source-path) nil
             "makeinfo" source-path "-o" info-path)
            (package-build--message "Created %s" info-path)))
        (package-build--message "Removing %s"
                                (expand-file-name dest-file target-dir))
        (delete-file (expand-file-name dest-file target-dir))))))

(defun package-build--generate-dir-file (files target-dir)
  "Create dir file from any .info files listed in FILES in TARGET-DIR."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (and (or (string-match ".info$" source-file)
                     (string-match ".texi\\(nfo\\)?$" source-file))
                 (file-exists-p info-path))
        (ignore-errors
          (package-build--run-process
           nil nil
           "install-info"
           (concat "--dir=" (expand-file-name "dir" target-dir))
           info-path))))))

;;; Building Utilities

(defun package-build--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (package-build--message
   "Copying files (->) and directories (=>)\n  from %s\n  to %s"
   source-dir target-dir)
  (dolist (elt files)
    (let* ((src  (car elt))
           (dst  (cdr elt))
           (src* (expand-file-name src source-dir))
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

(defconst package-build--this-file load-file-name)

;;; Building

;;;###autoload
(defun package-build-archive (name &optional dump-archive-contents)
  "Build a package archive for the package named NAME.
if DUMP-ARCHIVE-CONTENTS is non-nil, the updated archive contents
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
                              (current-time-string))
      (list name version)))
  (when dump-archive-contents
    (package-build-dump-archive-contents)))

;;;###autoload
(defun package-build--package (rcp version)
  "Create version VERSION of the package specified by RCP.
Return the archive entry for the package and store the package
in `package-build-archive-dir'."
  (let* ((source-dir (package-recipe--working-tree rcp))
         (file-specs (package-build--config-file-list rcp))
         (files (package-build-expand-file-specs source-dir file-specs))
         (commit (package-build--get-commit rcp))
         (name (oref rcp name)))
    (unless (equal file-specs package-build-default-files-spec)
      (when (equal files (package-build-expand-file-specs
                          source-dir package-build-default-files-spec nil t))
        (package-build--message
         "Note: %s :files spec is equivalent to the default." name)))
    (cond
     ((not version)
      (error "Unable to check out repository for %s" name))
     ((= 1 (length files))
      (package-build--build-single-file-package
       rcp version commit (caar files) source-dir))
     ((< 1 (length  files))
      (package-build--build-multi-file-package
       rcp version commit files source-dir))
     (t (error "Unable to find files matching recipe patterns")))))

(define-obsolete-function-alias 'package-build-package 'package-build--package
  "Package-Build 2.0.

The purpose of this alias is to get Cask working again.

This alias is only a temporary kludge and is going to be removed
again.  It will likely be replaced by a function with the same
name but a different signature.

Do not use this alias elsewhere.")

(defun package-build--build-single-file-package (rcp version commit file source-dir)
  (let* ((name (oref rcp name))
         (pkg-source (expand-file-name file source-dir))
         (pkg-target (expand-file-name
                      (concat name "-" version ".el")
                      package-build-archive-dir))
         (pkg-info (package-build--merge-package-info
                    (package-build--get-package-info pkg-source)
                    name version commit)))
    (unless (string-equal (downcase (concat name ".el"))
                          (downcase (file-name-nondirectory pkg-source)))
      (error "Single file %s does not match package name %s"
             (file-name-nondirectory pkg-source) name))
    (copy-file pkg-source pkg-target t)
    (let ((enable-local-variables nil)
          (make-backup-files nil))
      (with-current-buffer (find-file pkg-target)
        (package-build--update-or-insert-header "Package-Commit" commit)
        (package-build--update-or-insert-header "Package-Version" version)
        (package-build--ensure-ends-here-line pkg-source)
        (write-file pkg-target nil)
        (condition-case err
            (package-build--package-buffer-info-vec)
          (error
           (package-build--message "Warning: %S" err)))
        (kill-buffer)))
    (package-build--write-pkg-readme
     package-build-archive-dir
     (package-build--find-package-commentary pkg-source)
     name)
    (package-build--write-archive-entry rcp pkg-info 'single)))

(defun package-build--build-multi-file-package (rcp version commit files source-dir)
  (let* ((name (oref rcp name))
         (tmp-dir (file-name-as-directory (make-temp-file name t))))
    (unwind-protect
        (let* ((pkg-dir-name (concat name "-" version))
               (pkg-tmp-dir (expand-file-name pkg-dir-name tmp-dir))
               (pkg-file (concat name "-pkg.el"))
               (pkg-file-source (or (car (rassoc pkg-file files))
                                    pkg-file))
               (file-source (concat name ".el"))
               (pkg-source (or (car (rassoc file-source files))
                               file-source))
               (pkg-info (package-build--merge-package-info
                          (let ((default-directory source-dir))
                            (or (package-build--get-pkg-file-info pkg-file-source)
                                ;; Some packages provide NAME-pkg.el.in
                                (package-build--get-pkg-file-info
                                 (expand-file-name (concat pkg-file ".in")
                                                   (file-name-directory pkg-source)))
                                (package-build--get-package-info pkg-source)))
                          name version commit)))
          (package-build--copy-package-files files source-dir pkg-tmp-dir)
          (package-build--write-pkg-file (expand-file-name
                                          pkg-file
                                          (file-name-as-directory pkg-tmp-dir))
                                         pkg-info)

          (package-build--generate-info-files files source-dir pkg-tmp-dir)
          (package-build--generate-dir-file files pkg-tmp-dir)

          (let ((default-directory tmp-dir))
            (package-build--create-tar
             (expand-file-name (concat name "-" version ".tar")
                               package-build-archive-dir)
             pkg-dir-name))

          (let ((default-directory source-dir))
            (package-build--write-pkg-readme
             package-build-archive-dir
             (package-build--find-package-commentary pkg-source)
             name))
          (package-build--write-archive-entry rcp pkg-info 'tar))
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
      (let ((rcp (with-demoted-errors (package-recipe-lookup name))))
        (if rcp
            (if (with-demoted-errors (package-build-archive name) t)
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
If PRETTY-PRINT is non-nil, then pretty-print insted of using one
line per entry."
  (let (entries)
    (dolist (file (directory-files package-build-archive-dir t ".*\.entry$"))
      (let* ((entry (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))
             (name (car entry))
             (other-entry (assq name entries)))
        (if (not (file-exists-p (expand-file-name (symbol-name name)
                                                  package-build-recipes-dir)))
            (package-build--remove-archive-files entry)
          (when other-entry
            (when (version-list-< (elt (cdr entry) 0)
                                  (elt (cdr other-entry) 0))
              ;; Swap so that other-entry has the smallest version.
              (cl-rotatef other-entry entry))
            (package-build--remove-archive-files other-entry)
            (setq entries (remove other-entry entries)))
          (add-to-list 'entries entry))))
    (setq entries (nreverse entries))
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

(defalias 'package-build--archive-entries 'package-build-dump-archive-contents)

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

;;; Exporting Data as Json

(defun package-build-recipe-alist-as-json (file)
  "Dump the recipe list to FILE as json."
  (interactive)
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
  "Convert INFO into a data structure which will serialize to JSON in the desired shape."
  (let ((ver (elt info 0))
        (deps (elt info 1))
        (desc (elt info 2))
        (type (elt info 3))
        (props (and (> (length info) 4)
                    (elt info 4))))
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

;;; Backports

(defun package-build--lm-homepage (&optional file)
  "Return the homepage in file FILE, or current buffer if FILE is nil.
This is a copy of `lm-homepage', which first appeared in Emacs 24.4."
  (let ((page (lm-with-file file
                (lm-header "\\(?:x-\\)?\\(?:homepage\\|url\\)"))))
    (if (and page (string-match "^<.+>$" page))
        (substring page 1 -1)
      page)))

;;; _

(provide 'package-build)

;; For the time being just require all libraries that contain code
;; that was previously located in this library.

(require 'package-build-badges)
(require 'package-recipe-mode)
;; End:
;;; package-build.el ends here
