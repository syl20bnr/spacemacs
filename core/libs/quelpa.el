;;; quelpa.el --- Emacs Lisp packages built directly from source

;; Copyright 2014-2018, Steckerhalter
;; Copyright 2014-2015, Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools package management build source elpa

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Your personal local Emacs Lisp Package Archive (ELPA) with packages
;; built on-the-fly directly from source.

;; See the README for more info:
;; https://github.com/quelpa/quelpa/blob/master/README.org

;;; Requirements:

;; Emacs 25.1

;;; Code:

(require 'cl-lib)
(require 'help-fns)
(require 'url-parse)
(require 'package)
(require 'lisp-mnt)
(eval-when-compile (require 'subr-x))

;; --- customs / variables ---------------------------------------------------

(defgroup quelpa nil
  "Build and install packages from source code"
  :group 'package)

(defcustom quelpa-upgrade-p nil
  "When non-nil, `quelpa' will try to upgrade packages.
The global value can be overridden for each package by supplying
the `:upgrade' argument."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-stable-p nil
  "When non-nil, try to build stable packages like MELPA does."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-autoremove-p t
  "When non-nil, automatically remove old packages after upgrading.
The global value can be overridden for each package by supplying the
`:autoremove' argument."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-verbose t
  "When non-nil, `quelpa' prints log messages."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-before-hook nil
  "List of functions to be called before quelpa."
  :group 'quelpa
  :type 'hook)

(defcustom quelpa-after-hook nil
  "List of functions to be called after quelpa."
  :group 'quelpa
  :type 'hook)

(defcustom quelpa-dir (expand-file-name "quelpa" user-emacs-directory)
  "Where quelpa builds and stores packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-melpa-dir (expand-file-name "melpa" quelpa-dir)
  "Where the melpa repo cloned to."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-build-dir (expand-file-name "build" quelpa-dir)
  "Where quelpa builds packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-packages-dir (expand-file-name "packages" quelpa-dir)
  "Where quelpa puts built packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-melpa-recipe-stores (list (expand-file-name
                                             "recipes"
                                             quelpa-melpa-dir))
  "Recipe stores where quelpa finds default recipes for packages.
A store can either be a string pointing to a directory with
recipe files or a list with recipes."
  :group 'quelpa
  :type '(repeat
          (choice directory
                  (repeat
                   :tag "List of recipes"
                   (restricted-sexp :tag "Recipe"
                                    :match-alternatives (listp))))))

(defcustom quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
  "Location of the persistent cache file."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-persistent-cache-p t
  "Non-nil when quelpa's cache is saved on and read from disk."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-checkout-melpa-p t
  "If non-nil the MELPA git repo is cloned when quelpa is initialized."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-update-melpa-p t
  "If non-nil the MELPA git repo is updated when quelpa is initialized.
If nil the update is disabled and the repo is only updated on
`quelpa-upgrade' or `quelpa-self-upgrade'."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-melpa-repo-url "https://github.com/melpa/melpa.git"
  "The melpa git repository url."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-self-upgrade-p t
  "If non-nil upgrade quelpa itself when doing a
`quelpa-upgrade-all', otherwise only upgrade the packages in the
quelpa cache."
  :group 'quelpa
  :type 'boolean)

(defcustom quelpa-git-clone-depth 1
  "If non-nil shallow clone quelpa git recipes."
  :group 'quelpa
  :type '(choice (const :tag "Don't shallow clone" nil)
                 (integer :tag "Depth")))

(defcustom quelpa-upgrade-interval nil
  "Interval in days for `quelpa-upgrade-all-maybe'."
  :group 'quelpa
  :type 'integer)

(defvar quelpa-initialized-p nil
  "Non-nil when quelpa has been initialized.")

(defvar quelpa-cache nil
  "The `quelpa' command stores processed pkgs/recipes in the cache.")

(defvar quelpa-recipe '(quelpa :repo "quelpa/quelpa" :fetcher github)
  "The recipe for quelpa.")

;; --- package building ------------------------------------------------------

(defun quelpa-package-type (file)
  "Determine the package type of FILE.
Return `tar' for tarball packages, `single' for single file
packages, or nil, if FILE is not a package."
  (let ((ext (file-name-extension file)))
    (cond
     ((string= ext "tar") 'tar)
     ((string= ext "el") 'single)
     (:else nil))))

(defun quelpa-get-package-desc (file)
  "Extract and return the PACKAGE-DESC struct from FILE.
On error return nil."
  (let* ((kind (quelpa-package-type file))
         (desc (with-demoted-errors "Error getting PACKAGE-DESC: %s"
                 (with-temp-buffer
                   (pcase kind
                     (`single (insert-file-contents file)
                              (package-buffer-info))
                     (`tar (insert-file-contents-literally file)
                           (tar-mode)
                           (with-no-warnings
                             (package-tar-file-info))))))))
    (when (package-desc-p desc)
      desc)))

(defun quelpa-archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     quelpa-packages-dir)))

(defconst quelpa--min-ver '(0 -10) "Smallest possible version.")
(defun quelpa-version-cmp (name version op)
  "Return non-nil if version of pkg with NAME and VERSION satisfies OP.
OP is taking two version list and comparing."
  (let ((ver (if version (version-to-list version) quelpa--min-ver))
        (pkg-ver
         (or (when-let ((pkg-desc (cdr (assq name package-alist)))
                        (pkg-ver (package-desc-version (car pkg-desc))))
               pkg-ver)
             (alist-get name package--builtin-versions)
             quelpa--min-ver)))
    (funcall op ver pkg-ver)))

(defmacro quelpa-version>-p (name version)
  "Return non-nil if VERSION of pkg with NAME is newer than what is currently installed."
  `(quelpa-version-cmp ,name ,version (lambda (o1 o2) (not (version-list-<= o1 o2)))))

(defmacro quelpa-version<-p (name version)
  "Return non-nil if VERSION of pkg with NAME is older than what is currently installed."
  `(quelpa-version-cmp ,name ,version 'version-list-<))

(defmacro quelpa-version=-p (name version)
  "Return non-nil if VERSION of pkg with NAME is same which what is currently installed."
  `(quelpa-version-cmp ,name ,version 'version-list-=))

(defun quelpa--package-installed-p (package &optional min-version)
  "Return non-nil if PACKAGE, of MIN-VERSION or newer, is installed.
Like `package-installed-p' but properly check for built-in package even when all
packages are not initialized."
  (or (package-installed-p package (or min-version quelpa--min-ver))
      (package-built-in-p package (or min-version quelpa--min-ver))))

(defvar quelpa--override-version-check nil)
(defun quelpa-checkout (rcp dir)
  "Return the version of the new package given a RCP and DIR.
Return nil if the package is already installed and should not be upgraded."
  (pcase-let ((`(,name . ,config) rcp)
              (quelpa-build-stable quelpa-stable-p)
              (quelpa--override-version-check quelpa--override-version-check))
    (unless (or (and (quelpa--package-installed-p name) (not quelpa-upgrade-p))
                (and (not config)
                     (quelpa-message t "no recipe found for package `%s'" name)))
      (let ((version (condition-case-unless-debug err
                         (quelpa-build-checkout name config dir)
                       (error
                        (error "Failed to checkout `%s': `%s'"
                               name (error-message-string err))))))
        (cond
          ((and quelpa--override-version-check
                (quelpa-version=-p name version))
           (setq version (concat version ".1"))
           version)
          ((or quelpa--override-version-check
               (quelpa-version>-p name version))
           version))))))

(defun quelpa-build (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-build-dir' storing it in
`quelpa-packages-dir'.  Return the path to the created file or nil
if no action is necessary (like when the package is installed
already and should not be upgraded etc)."
  (let* ((name (car rcp))
         (build-dir (expand-file-name (symbol-name name) quelpa-build-dir))
         (version (quelpa-checkout rcp build-dir)))
    (prog1
        (if version
            (quelpa-archive-file-name
             (quelpa-build-package (symbol-name name)
                                   version
                                   (quelpa-build--config-file-list (cdr rcp))
                                   build-dir
                                   quelpa-packages-dir))
          (quelpa-build--message "Newer package has been installed. Not upgrading.")
          nil)
      (when (fboundp 'package--quickstart-maybe-refresh)
        (package--quickstart-maybe-refresh)))))

;; --- package-build.el integration ------------------------------------------

(defun quelpa-file-version (file-path type version time-stamp)
  "Return version of file at FILE-PATH."
  (if (eq type 'directory)
      time-stamp
    (cl-letf* ((package-strip-rcs-id-orig (symbol-function 'package-strip-rcs-id))
               ((symbol-function 'package-strip-rcs-id)
                (lambda (str)
                  (or (funcall package-strip-rcs-id-orig (lm-header "package-version"))
                      (funcall package-strip-rcs-id-orig (lm-header "version"))
                      "0"))))
      (concat (mapconcat
               #'number-to-string
               (package-desc-version (quelpa-get-package-desc file-path)) ".")
              (pcase version
                (`original "")
                (_ (concat "pre0." time-stamp)))))))

(defun quelpa-directory-files (path)
  "Return list of directory files from PATH recursively."
  (let ((result '()))
    (mapc
     (lambda (file)
       (if (file-directory-p file)
           (progn
             ;; When directory is not empty.
             (when (cddr (directory-files file))
               (dolist (subfile (quelpa-directory-files file))
                 (add-to-list 'result subfile))))
         (add-to-list 'result file)))
     (mapcar
      (lambda (file) (expand-file-name file path))
      ;; Without first two entries because they are always "." and "..".
      (remove ".." (remove "." (directory-files path)))))
    result))

(defun quelpa-expand-source-file-list (file-path config)
  "Return list of source files from FILE-PATH corresponding to
CONFIG."
  (let ((source-files
         (mapcar
          (lambda (file) (expand-file-name file file-path))
          (quelpa-build--expand-source-file-list file-path config))))
    ;; Replace any directories in the source file list with the filenames of the
    ;; files they contain (so that these files can subsequently be hashed).
    (dolist (file source-files source-files)
      (when (file-directory-p file)
        (setq source-files (remove file source-files))
        (setq source-files (append source-files
                                   (quelpa-directory-files file)))))))

(defun quelpa-slurp-file (file)
  "Return the contents of FILE as a string, or nil if no such
file exists."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq-local buffer-file-coding-system 'binary)
      (insert-file-contents-literally file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun quelpa-check-hash (name config file-path dir &optional fetcher)
  "Check if hash of FILE-PATH is different as in STAMP-FILE.
If it is different save the new hash and timestamp to STAMP-FILE
and return TIME-STAMP, otherwise return OLD-TIME-STAMP."
  (unless (file-directory-p dir)
    (make-directory dir))
  (let* (files
         hashes
         new-stamp-info
         new-content-hash
         (time-stamp
          (replace-regexp-in-string "\\.0+" "." (format-time-string "%Y%m%d.%H%M%S")))
         (stamp-file (concat (expand-file-name (symbol-name name) dir) ".stamp"))
         (old-stamp-info (quelpa-build--read-from-file stamp-file))
         (old-content-hash (cdr old-stamp-info))
         (old-time-stamp (car old-stamp-info))
         (type (if (file-directory-p file-path) 'directory 'file))
         (version (plist-get config :version)))

    (if (not (file-exists-p file-path))
        (error "`%s' does not exist" file-path)
      (if (eq type 'directory)
          (setq files (quelpa-expand-source-file-list file-path config)
                hashes (mapcar
                        (lambda (file)
                          (secure-hash
                           'sha1 (concat file (quelpa-slurp-file file)))) files)
                new-content-hash (secure-hash 'sha1 (mapconcat 'identity hashes "")))
        (setq new-content-hash (secure-hash 'sha1 (quelpa-slurp-file file-path)))))

    (setq new-stamp-info (cons time-stamp new-content-hash))
    (if (and old-content-hash
             (string= new-content-hash old-content-hash))
        (quelpa-file-version file-path type version old-time-stamp)
      (unless (eq fetcher 'url)
        (delete-directory dir t)
        (make-directory dir)
        (if (eq type 'file)
            (copy-file file-path dir t t t t)
          (copy-directory file-path dir t t t)))
      (quelpa-build--dump new-stamp-info stamp-file)
      (quelpa-file-version file-path type version time-stamp))))

;; --- package-build fork ------------------------------------------

(defcustom quelpa-build-verbose t
  "When non-nil, then print additional progress information."
  :type 'boolean)

(defcustom quelpa-build-stable nil
  "When non-nil, then try to build packages from versions-tagged code."
  :type 'boolean)

(defcustom quelpa-build-timeout-executable
  (let ((prog (or (executable-find "timeout")
                  (executable-find "gtimeout"))))
    (when (and prog
               (string-match-p "^ *-k"
                               (shell-command-to-string (concat prog " --help"))))
      prog))
  "Path to a GNU coreutils \"timeout\" command if available.
This must be a version which supports the \"-k\" option."
  :type '(file :must-match t))

(defcustom quelpa-build-timeout-secs 600
  "Wait this many seconds for external processes to complete.

If an external process takes longer than specified here to
complete, then it is terminated.  This only has an effect
if `quelpa-build-timeout-executable' is non-nil."
  :type 'number)

(defcustom quelpa-build-tar-executable
  (or (executable-find "gtar")
      (executable-find "tar"))
  "Path to a (preferably GNU) tar command.
Certain package names (e.g. \"@\") may not work properly with a BSD tar."
  :type '(file :must-match t))

(defvar quelpa--tar-type nil
  "Type of `quelpa-build-tar-executable'.  Can be `gnu' or `bsd'.
nil means the type is not decided yet.")

(defcustom quelpa-build-explicit-tar-format-p nil
  "If non-nil pass \"--format=gnu\" option to tar command.

Passing the option is necessary on the systems where the default
tar format isn't gnu."
  :type 'boolean)


(defcustom quelpa-build-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
  :type 'string)

;;; Internal Variables

(defconst quelpa-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")

;;; Utilities

(defun quelpa-build--message (format-string &rest args)
  "Behave like `message' if `quelpa-build-verbose' is non-nil.
Otherwise do nothing."
  (when quelpa-build-verbose
    (apply 'message format-string args)))

(defun quelpa-build--slurp-file (file)
  "Return the contents of FILE as a string, or nil if no such file exists."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun quelpa-build--string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n\r]+$" "" str))

(defun quelpa-build--trim (str &optional chr)
  "Return a copy of STR without any trailing CHR (or space if unspecified)."
  (if (equal (elt str (1- (length str))) (or chr ? ))
      (substring str 0 (1- (length str)))
    str))

;;; Version Handling

(defun quelpa-build--valid-version (str &optional regexp)
  "Apply to STR the REGEXP if defined, \
then pass the string to `version-to-list' and return the result, \
or nil if the version cannot be parsed."
  (when (and regexp (string-match regexp str))
    (setq str (match-string 1 str)))
  (ignore-errors (version-to-list str)))

(defun quelpa-build--parse-time (str)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string."
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (setq str (substring-no-properties str))
  (let ((time (date-to-time
               (if (string-match "\
^\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\) \
\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)$" str)
                   (concat (match-string 1 str) "-" (match-string 2 str) "-"
                           (match-string 3 str) " " (match-string 4 str))
                 str))))
    (concat (format-time-string "%Y%m%d." time)
            (format "%d" (string-to-number (format-time-string "%H%M" time))))))

(defun quelpa-build--find-parse-time (regexp &optional bound)
  "Find REGEXP in current buffer and format as a time-based version string.
An optional second argument BOUND bounds the search; it is a
buffer position. The match found must not end after that
position."
  (and (re-search-backward regexp bound t)
       (quelpa-build--parse-time (match-string-no-properties 1))))

(defun quelpa-build--find-parse-time-newest (regexp &optional bound)
  "Find REGEXP in current buffer and format as a time-based version string.
An optional second argument BOUND bounds the search; it is a
buffer position. The match found must not end after that
position."
  (save-match-data
    (let (cur matches)
      (while (setq cur (quelpa-build--find-parse-time regexp bound))
        (push cur matches))
      (car (nreverse (sort matches 'string<))))))

(defun quelpa-build--find-version-newest (regexp &optional bound)
  "Find the newest version matching REGEXP before point.
An optional second argument BOUND bounds the search; it is a
buffer position. The match found must not before after that
position."
  (let ((tags (split-string
               (buffer-substring-no-properties
                (or bound (point-min)) (point))
               "\n")))
    (setq tags (append
                (mapcar
                 ;; Because the default `version-separator' is ".",
                 ;; version-strings like "1_4_5" will be parsed
                 ;; wrongly as (1 -4 4 -4 5), so we set
                 ;; `version-separator' to "_" below and run again.
                 (lambda (tag)
                   (when (quelpa-build--valid-version tag regexp)
                     (list (quelpa-build--valid-version tag regexp) tag)))
                 tags)
                (mapcar
                 ;; Check for valid versions again, this time using
                 ;; "_" as a separator instead of "." to catch
                 ;; version-strings like "1_4_5".  Since "_" is
                 ;; otherwise treated as a snapshot separator by
                 ;; `version-regexp-alist', we don't have to worry
                 ;; about the incorrect version list above—(1 -4 4 -4
                 ;; 5)—since it will always be treated as older by
                 ;; `version-list-<'.
                 (lambda (tag)
                   (let ((version-separator "_"))
                     (when (quelpa-build--valid-version tag regexp)
                       (list (quelpa-build--valid-version tag regexp) tag))))
                 tags)))
    (setq tags (cl-remove-if nil tags))
    ;; Returns a list like ((0 1) ("v0.1")); the first element is used
    ;; for comparison and for `package-version-join', and the second
    ;; (the original tag) is used by git/hg/etc.
    (car (nreverse (sort tags (lambda (v1 v2) (version-list-< (car v1) (car v2))))))))

;;; Run Process

(defun quelpa-build--run-process (dir command &rest args)
  "In DIR run COMMAND with ARGS.
If DIR is unset, try to run from `quelpa-build-dir'
or variable `temporary-file-directory'.
Output is written to the current buffer."
  (let ((default-directory (file-name-as-directory (or dir
                                                       quelpa-build-dir
                                                       temporary-file-directory)))
        (argv (nconc (unless (eq system-type 'windows-nt)
                       (list "env" "LC_ALL=C"))
                     (if quelpa-build-timeout-executable
                         (nconc (list quelpa-build-timeout-executable
                                      "-k" "60" (number-to-string
                                                 quelpa-build-timeout-secs)
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
                 argv exit-code (buffer-string))))))

(defun quelpa-build--run-process-match (regexp dir prog &rest args)
  "Run PROG with args and return the first match for REGEXP in its output.
PROG is run in DIR, or if that is nil in `default-directory'."
  (with-temp-buffer
    (apply 'quelpa-build--run-process dir prog args)
    (goto-char (point-min))
    (re-search-forward regexp)
    (match-string-no-properties 1)))

;;; Checkout
;;;; Common

(defun quelpa-build-checkout (package-name config working-dir)
  "Check out source for PACKAGE-NAME with CONFIG under WORKING-DIR.
In turn, this function uses the :fetcher option in the CONFIG to
choose a source-specific fetcher function, which it calls with
the same arguments.

Returns the package version as a string."
  (let ((fetcher (plist-get config :fetcher)))
    (quelpa-build--message "Fetcher: %s" fetcher)
    (unless (eq fetcher 'wiki)
      (quelpa-build--message "Source: %s\n"
                             (or (plist-get config :repo)
                                 (plist-get config :url))))
    (funcall (intern (format "quelpa-build--checkout-%s" fetcher))
             package-name config (file-name-as-directory working-dir))))

(defun quelpa-build--princ-exists (dir)
  "Print a message that the contents of DIR will be updated."
  (quelpa-build--message "Updating %s" dir))

(defun quelpa-build--princ-checkout (repo dir)
  "Print a message that REPO will be checked out into DIR."
  (quelpa-build--message "Cloning %s to %s" repo dir))

;;;; Wiki

(defvar quelpa-build--last-wiki-fetch-time 0
  "The time at which an emacswiki URL was last requested.
This is used to avoid exceeding the rate limit of 1 request per 2
seconds; the server cuts off after 10 requests in 20 seconds.")

(defvar quelpa-build--wiki-min-request-interval 3
  "The shortest permissible interval between successive requests for Emacswiki URLs.")

(defmacro quelpa-build--with-wiki-rate-limit (&rest body)
  "Rate-limit BODY code passed to this macro to match EmacsWiki's rate limiting."
  (let ((elapsed (cl-gensym)))
    `(let ((,elapsed (- (float-time) quelpa-build--last-wiki-fetch-time)))
       (when (< ,elapsed quelpa-build--wiki-min-request-interval)
         (let ((wait (- quelpa-build--wiki-min-request-interval ,elapsed)))
           (quelpa-build--message
            "Waiting %.2f secs before hitting Emacswiki again" wait)
           (sleep-for wait)))
       (unwind-protect
           (progn ,@body)
         (setq quelpa-build--last-wiki-fetch-time (float-time))))))

(require 'mm-decode)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun quelpa-build--url-copy-file (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.  Both args must be strings.
Returns the http request's header as a string.
Like `url-copy-file', but it produces an error if the http response is not 200.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists."
  (if (and (file-exists-p newname)
           (not ok-if-already-exists))
      (error "Opening output file: File already exists, %s" newname))
  (let ((buffer (url-retrieve-synchronously url))
        (headers nil)
        (handle nil))
    (if (not buffer)
        (error "Opening input file: No such file or directory, %s" url))
    (with-current-buffer buffer
      (unless (= 200 url-http-response-status)
        (error "HTTP error %s fetching %s" url-http-response-status url))
      (setq handle (mm-dissect-buffer t))
      (mail-narrow-to-head)
      (setq headers (buffer-string)))
    (mm-save-part-to-file handle newname)
    (kill-buffer buffer)
    (mm-destroy-parts handle)
    headers))

(defun quelpa-build--grab-wiki-file (filename)
  "Download FILENAME from emacswiki, returning its last-modified time."
  (let ((download-url
         (format "https://www.emacswiki.org/emacs/download/%s" filename))
        headers)
    (quelpa-build--with-wiki-rate-limit
     (setq headers (quelpa-build--url-copy-file download-url filename t)))
    (when (zerop (nth 7 (file-attributes filename)))
      (error "Wiki file %s was empty - has it been removed?" filename))
    (quelpa-build--parse-time
     (with-temp-buffer
       (insert headers)
       (mail-fetch-field "last-modified")))))

(defun quelpa-build--checkout-wiki (name config dir)
  "Checkout package NAME with config CONFIG from the EmacsWiki into DIR."
  (unless quelpa-build-stable
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (unless (file-exists-p dir)
        (make-directory dir))
      (let ((files (or (plist-get config :files)
                       (list (format "%s.el" name))))
            (default-directory dir))
        (car (nreverse (sort (mapcar 'quelpa-build--grab-wiki-file files)
                             'string-lessp)))))))

;;;; Darcs

(defun quelpa-build--darcs-repo (dir)
  "Get the current darcs repo for DIR."
  (quelpa-build--run-process-match "Default Remote: \\(.*\\)"
                                   dir "darcs" "show" "repo"))

(defun quelpa-build--checkout-darcs (name config dir)
  "Check package NAME with config CONFIG out of darcs into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (cond
       ((and (file-exists-p (expand-file-name "_darcs" dir))
             (string-equal (quelpa-build--darcs-repo dir) repo))
        (quelpa-build--princ-exists dir)
        (quelpa-build--run-process dir "darcs" "pull" "--all"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (quelpa-build--princ-checkout repo dir)
        (quelpa-build--run-process nil "darcs" "get" repo dir)))
      (if quelpa-build-stable
          (let* ((min-bound (goto-char (point-max)))
                 (tag-version
                  (and (quelpa-build--run-process dir "darcs" "show" "tags")
                       (or (quelpa-build--find-version-newest
                            (or (plist-get config :version-regexp)
                                quelpa-build-version-regexp)
                            min-bound)
                           (error "No valid stable versions found for %s" name)))))
            (quelpa-build--run-process dir "darcs" "obliterate"
                                       "--all" "--from-tag"
                                       (cadr tag-version))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'quelpa-build--run-process
               dir "darcs" "changes" "--max-count" "1"
               (quelpa-build--expand-source-file-list dir config))
        (quelpa-build--find-parse-time "\
\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} \
\\( \\|[0-9]\\)[0-9] [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} \
[A-Za-z]\\{3\\} [0-9]\\{4\\}\\)")))))

;;;; Fossil

(defun quelpa-build--fossil-repo (dir)
  "Get the current fossil repo for DIR."
  (quelpa-build--run-process-match "\\(.*\\)" dir "fossil" "remote-url"))

(defun quelpa-build--checkout-fossil (name config dir)
  "Check package NAME with config CONFIG out of fossil into DIR."
  (unless quelpa-build-stable
    (let ((repo (plist-get config :url)))
      (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
        (cond
         ((and (or (file-exists-p (expand-file-name ".fslckout" dir))
                   (file-exists-p (expand-file-name "_FOSSIL_" dir)))
               (string-equal (quelpa-build--fossil-repo dir) repo))
          (quelpa-build--princ-exists dir)
          (quelpa-build--run-process dir "fossil" "update"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (quelpa-build--princ-checkout repo dir)
          (make-directory dir)
          (quelpa-build--run-process dir "fossil" "clone" repo "repo.fossil")
          (quelpa-build--run-process dir "fossil" "open" "repo.fossil")))
        (quelpa-build--run-process dir "fossil" "timeline" "-n" "1" "-t" "ci")
        (or (quelpa-build--find-parse-time "\
=== \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ===\n\
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) ")
            (error "No valid timestamps found!"))))))

;;;; Svn

(defun quelpa-build--svn-repo (dir)
  "Get the current svn repo for DIR."
  (quelpa-build--run-process-match "URL: \\(.*\\)" dir "svn" "info"))

(defun quelpa-build--checkout-svn (name config dir)
  "Check package NAME with config CONFIG out of svn into DIR."
  (unless quelpa-build-stable
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (let ((repo (quelpa-build--trim (plist-get config :url) ?/))
            (bound (goto-char (point-max))))
        (cond
         ((and (file-exists-p (expand-file-name ".svn" dir))
               (string-equal (quelpa-build--svn-repo dir) repo))
          (quelpa-build--princ-exists dir)
          (quelpa-build--run-process dir "svn" "up"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (quelpa-build--princ-checkout repo dir)
          (quelpa-build--run-process nil "svn" "checkout" repo dir)))
        (apply 'quelpa-build--run-process dir "svn" "info"
               (quelpa-build--expand-source-file-list dir config))
        (or (quelpa-build--find-parse-time-newest "\
Last Changed Date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)"
                                                  bound)
            (error "No valid timestamps found!"))))))

;;;; Cvs

(defun quelpa-build--cvs-repo (dir)
  "Get the current CVS root and repository for DIR.

Return a cons cell whose `car' is the root and whose `cdr' is the repository."
  (apply 'cons
         (mapcar (lambda (file)
                   (quelpa-build--string-rtrim
                    (quelpa-build--slurp-file (expand-file-name file dir))))
                 '("CVS/Root" "CVS/Repository"))))

(defun quelpa-build--checkout-cvs (name config dir)
  "Check package NAME with config CONFIG out of cvs into DIR."
  (unless quelpa-build-stable
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (let ((root (quelpa-build--trim (plist-get config :url) ?/))
            (repo (or (plist-get config :module) (symbol-name name)))
            (bound (goto-char (point-max)))
            latest)
        (cond
         ((and (file-exists-p (expand-file-name "CVS" dir))
               (equal (quelpa-build--cvs-repo dir) (cons root repo)))
          (quelpa-build--princ-exists dir)
          (quelpa-build--run-process dir "cvs" "update" "-dP"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (quelpa-build--princ-checkout (format "%s from %s" repo root) dir)
          ;; CVS insists on relative paths as target directory for checkout (for
          ;; whatever reason), and puts "CVS" directories into every subdirectory
          ;; of the current working directory given in the target path. To get CVS
          ;; to just write to DIR, we need to execute CVS from the parent
          ;; directory of DIR, and specific DIR as relative path.  Hence all the
          ;; following mucking around with paths.  CVS is really horrid.
          (let ((dir (directory-file-name dir)))
            (quelpa-build--run-process (file-name-directory dir)
                                       "env" "TZ=UTC" "cvs" "-z3"
                                       "-d" root "checkout"
                                       "-d" (file-name-nondirectory dir)
                                       repo))))
        (apply 'quelpa-build--run-process dir "cvs" "log"
               (quelpa-build--expand-source-file-list dir config))

        ;; `cvs log` does not provide a way to view the previous N
        ;; revisions, so instead of parsing the entire log we examine
        ;; the Entries file, which looks like this:
        ;;
        ;; /.cvsignore/1.2/Thu Sep  1 12:42:02 2005//
        ;; /CHANGES/1.1/Tue Oct  4 11:47:54 2005//
        ;; /GNUmakefile/1.8/Tue Oct  4 11:47:54 2005//
        ;; /Makefile/1.14/Tue Oct  4 11:47:54 2005//
        ;;
        (insert-file-contents (concat dir "/CVS/Entries"))
        (setq latest
              (car
               (sort
                (split-string (buffer-substring-no-properties (point) (point-max)) "\n")
                (lambda (x y)
                  (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" x)
                    (setq x (quelpa-build--parse-time (match-string 1 x))))
                  (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" y)
                    (setq y (quelpa-build--parse-time (match-string 1 y))))
                  (version-list-<= (quelpa-build--valid-version y)
                                   (quelpa-build--valid-version x))))))
        (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" latest)
          (setq latest (match-string 1 latest)))
        (or (quelpa-build--parse-time latest)
            (error "No valid timestamps found!"))))))

;;;; Git

(defun quelpa-build--git-repo (dir remote)
  "Get the current git repo for DIR from REMOTE."
  (quelpa-build--run-process-match
   "Fetch URL: \\(.*\\)" dir "git" "remote" "show" "-n" remote))

(defun quelpa-build--checkout-git (name config dir)
  "Check package NAME with config CONFIG out of git into DIR."
  (let* ((repo (plist-get config :url))
         (remote (or (plist-get config :remote) "origin"))
         (commit (or (plist-get config :commit)
                     (when-let ((branch (plist-get config :branch)))
                       (concat remote "/" branch))))
         (depth (or (plist-get config :depth) quelpa-git-clone-depth))
         (force (plist-get config :force))
         (use-current-ref (plist-get config :use-current-ref)))
    (when (string-match (rx bos "file://" (group (1+ anything))) repo)
      ;; Expand local file:// URLs
      (setq repo (expand-file-name (match-string 1 repo))))
    (setq quelpa--override-version-check use-current-ref)
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".git" dir))
             (string-equal (quelpa-build--git-repo dir remote) repo))
        (quelpa-build--princ-exists dir)
        (quelpa-build--run-process dir "git" "fetch" "--tags" remote))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (quelpa-build--princ-checkout repo dir)
        (apply #'quelpa-build--run-process
               (append
                `(nil "git" "clone" ,repo ,dir)
                `("--origin" ,remote)
                (when (and depth (not (plist-get config :commit)))
                  `("--depth" ,(int-to-string depth)
                    "--no-single-branch"))
                (when-let ((branch (plist-get config :branch)))
                  `("--branch" ,branch))))))
      (if quelpa-build-stable
          (let* ((min-bound (goto-char (point-max)))
                 (tag-version
                  (and (quelpa-build--run-process dir "git" "tag")
                       (or (quelpa-build--find-version-newest
                            (or (plist-get config :version-regexp)
                                quelpa-build-version-regexp)
                            min-bound)
                           (error "No valid stable versions found for %s" name)))))
            ;; Using reset --hard here to comply with what's used for
            ;; unstable, but maybe this should be a checkout?
            (unless use-current-ref
              (quelpa-build--update-git-to-ref
               dir (concat "tags/" (cadr tag-version))
               force))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (unless use-current-ref
          (quelpa-build--update-git-to-ref
           dir (or commit (concat remote "/" (quelpa-build--git-head-branch dir)))
           force))
        (apply 'quelpa-build--run-process
               dir "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
               (quelpa-build--expand-source-file-list dir config))
        (quelpa-build--find-parse-time "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

(defun quelpa-build--git-head-branch (dir)
  "Get the current git repo for DIR."
  (or (ignore-errors
        (quelpa-build--run-process-match
         "HEAD branch: \\(.*\\)" dir "git" "remote" "show" "origin"))
      "master"))

(defun quelpa-build--git-head-sha (dir)
  "Get the current head SHA for DIR."
  (ignore-errors
    (quelpa-build--run-process-match
     "\\(.*\\)" dir "git" "rev-parse" "HEAD")))

(defun quelpa-build--update-git-to-ref (dir ref &optional force)
  "Update the git repo in DIR so that HEAD is REF.
This will perform an checkout or a reset if FORCE."
  (condition-case nil
      (quelpa-build--run-process dir "git" "cat-file" "-e" ref)
    (error
     ;; unshallow if needed
     (quelpa-build--run-process dir "git" "fetch" "--unshallow" "--tags")))
  (if force
      (quelpa-build--run-process dir "git" "reset" "--hard" ref)
    (with-demoted-errors "Error: %s"
      (quelpa-build--run-process dir "git" "checkout" ref)))
  (quelpa-build--run-process dir "git" "submodule" "sync" "--recursive")
  (quelpa-build--run-process dir "git" "submodule" "update" "--init" "--recursive"))

(defun quelpa-build--checkout-github (name config dir)
  "Check package NAME with config CONFIG out of github into DIR."
  (let ((url (format "https://github.com/%s.git" (plist-get config :repo))))
    (quelpa-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun quelpa-build--checkout-github-ssh (name config dir)
  "Check package NAME with config CONFIG out of github into DIR."
  (let ((url (format "git@github.com:%s.git" (plist-get config :repo))))
    (quelpa-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun quelpa-build--checkout-gitlab (name config dir)
  "Check package NAME with config CONFIG out of gitlab into DIR."
  (let ((url (format "https://gitlab.com/%s.git" (plist-get config :repo))))
    (quelpa-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

;;;; Bzr

(defun quelpa-build--bzr-repo (dir)
  "Get the current bzr repo for DIR."
  (quelpa-build--run-process-match "parent branch: \\(.*\\)" dir "bzr" "info"))

(defun quelpa-build--checkout-bzr (name config dir)
  "Check package NAME with config CONFIG out of bzr into DIR."
  (let ((repo (quelpa-build--run-process-match
               "\\(?:branch root\\|repository branch\\): \\(.*\\)"
               nil "bzr" "info" (plist-get config :url))))
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".bzr" dir))
             (string-equal (quelpa-build--bzr-repo dir) repo))
        (quelpa-build--princ-exists dir)
        (quelpa-build--run-process dir "bzr" "merge" "--force"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (quelpa-build--princ-checkout repo dir)
        (quelpa-build--run-process nil "bzr" "branch" repo dir)))
      (if quelpa-build-stable
          (let ((bound (goto-char (point-max)))
                (regexp (or (plist-get config :version-regexp)
                            quelpa-build-version-regexp))
                tag-version)
            (quelpa-build--run-process dir "bzr" "tags")
            (goto-char bound)
            (ignore-errors (while (re-search-forward "\\ +.*")
                             (replace-match "")))
            (setq tag-version
                  (or (quelpa-build--find-version-newest regexp bound)
                      (error "No valid stable versions found for %s" name)))
            (quelpa-build--run-process dir
                                       "bzr" "revert" "-r"
                                       (concat "tag:" (cadr tag-version)))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'quelpa-build--run-process dir "bzr" "log" "-l1"
               (quelpa-build--expand-source-file-list dir config))
        (quelpa-build--find-parse-time "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

;;;; Hg

(defun quelpa-build--hg-repo (dir)
  "Get the current hg repo for DIR."
  (quelpa-build--run-process-match "default = \\(.*\\)" dir "hg" "paths"))

(defun quelpa-build--checkout-hg (name config dir)
  "Check package NAME with config CONFIG out of hg into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".hg" dir))
             (string-equal (quelpa-build--hg-repo dir) repo))
        (quelpa-build--princ-exists dir)
        (quelpa-build--run-process dir "hg" "pull")
        (quelpa-build--run-process dir "hg" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (quelpa-build--princ-checkout repo dir)
        (quelpa-build--run-process nil "hg" "clone" repo dir)))
      (if quelpa-build-stable
          (let ((min-bound (goto-char (point-max)))
                (regexp (or (plist-get config :version-regexp)
                            quelpa-build-version-regexp))
                tag-version)
            (quelpa-build--run-process dir "hg" "tags")
            ;; The output of `hg tags` shows the ref of the tag as well
            ;; as the tag itself, e.g.:
            ;;
            ;; tip                             1696:73ad80e8fea1
            ;; 1.2.8                           1691:464af57fd2b7
            ;;
            ;; So here we remove that second column before passing the
            ;; buffer contents to `quelpa-build--find-version-newest'.
            ;; This isn't strictly necessary for Mercurial since the
            ;; colon in "1691:464af57fd2b7" means that won't be parsed
            ;; as a valid version-string, but it's an example of how to
            ;; do it in case it's necessary elsewhere.
            (goto-char min-bound)
            (ignore-errors (while (re-search-forward "\\ +.*")
                             (replace-match "")))
            (setq tag-version
                  (or (quelpa-build--find-version-newest regexp min-bound)
                      (error "No valid stable versions found for %s" name)))
            (quelpa-build--run-process dir "hg" "update" (cadr tag-version))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'quelpa-build--run-process
               dir "hg" "log" "--style" "compact" "-l1"
               (quelpa-build--expand-source-file-list dir config))
        (quelpa-build--find-parse-time "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

(defun quelpa-build--checkout-bitbucket (name config dir)
  "Check package NAME with config CONFIG out of bitbucket into DIR."
  (let ((url (format "https://bitbucket.com/%s" (plist-get config :repo))))
    (quelpa-build--checkout-hg name (plist-put (copy-sequence config) :url url) dir)))

;;; Utilities

(defun quelpa-build--dump (data file &optional pretty-print)
  "Write DATA to FILE as a Lisp sexp.
Optionally PRETTY-PRINT the data."
  (with-temp-file file
    (quelpa-build--message "File: %s" file)
    (if pretty-print
        (pp data (current-buffer))
      (print data (current-buffer)))))

(defun quelpa-build--write-pkg-file (pkg-file pkg-info)
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

(defun quelpa-build--read-from-file (file)
  "Read and return the Lisp data stored in FILE, or nil if no such file exists."
  (when (file-exists-p file)
    (car (read-from-string (quelpa-build--slurp-file file)))))

(defun quelpa--tar-type ()
  "Return `bsd' or `gnu' depending on type of Tar executable.
Tests and sets variable `quelpa--tar-type' if not already set."
  (or quelpa--tar-type
      (when (and quelpa-build-tar-executable
                 (file-executable-p quelpa-build-tar-executable))
        (setq quelpa--tar-type
              (let ((v (shell-command-to-string
                        (format "%s --version" quelpa-build-tar-executable))))
                (cond ((string-match-p "bsdtar" v) 'bsd)
                      ((string-match-p "GNU tar" v) 'gnu)
                      (t 'gnu)))))))

(defun quelpa-build--create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
  (when (and (eq (quelpa--tar-type) 'gnu)
             (eq system-type 'windows-nt))
    (setq file (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" file)))
  (apply 'process-file
         quelpa-build-tar-executable nil
         (get-buffer-create "*quelpa-build-checkout*")
         nil "-cvf"
         file
         "--exclude=.svn"
         "--exclude=CVS"
         "--exclude=.git"
         "--exclude=_darcs"
         "--exclude=.fslckout"
         "--exclude=_FOSSIL_"
         "--exclude=.bzr"
         "--exclude=.hg"
         (append (and quelpa-build-explicit-tar-format-p '("--format=gnu"))
                 (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir)))))

(defun quelpa-build--find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun quelpa-build--write-pkg-readme (target-dir commentary file-name)
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
                      (quelpa-build--readme-file-name target-dir file-name))))))

(defun quelpa-build--readme-file-name (target-dir file-name)
  "Name of the readme file in TARGET-DIR for the package FILE-NAME."
  (expand-file-name (concat file-name "-readme.txt")
                    target-dir))

(defun quelpa-build--update-or-insert-version (version)
  "Ensure current buffer has a \"Package-Version: VERSION\" header."
  (goto-char (point-min))
  (if (let ((case-fold-search t))
        (re-search-forward "^;+* *Package-Version *: *" nil t))
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
  (insert (format ";; Package-Version: %s" version))
  (newline))

(defun quelpa-build--ensure-ends-here-line (file-path)
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

(defun quelpa-build--get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (quelpa-build--update-or-insert-version "0")
        (quelpa-build--ensure-ends-here-line file-path)
        (cl-flet ((package-strip-rcs-id (str) "0"))
          (quelpa-build--package-buffer-info-vec))))))

(defun quelpa-build--get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (quelpa-build--read-from-file file-path)))
      (if (eq 'define-package (car package-def))
          (let* ((pkgfile-info (cdr package-def))
                 (descr (nth 2 pkgfile-info))
                 (rest-plist (cl-subseq pkgfile-info (min 4 (length pkgfile-info))))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (when-let ((value (cadr rest-plist)))
                                 (push (cons (car rest-plist)
                                             (if (eq (car-safe value) 'quote)
                                                 (cadr value)
                                               value))
                                       alist)))
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

(defun quelpa-build--merge-package-info (pkg-info name version)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let ((merged (or (copy-sequence pkg-info)
                    (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 3 version)
    merged))

(defun quelpa-build--archive-entry (pkg-info type)
  "Return the archive-contents cons cell for PKG-INFO and TYPE."
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

;;; Recipes

(defun quelpa-build-expand-file-specs (dir specs &optional subdir allow-empty)
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
                                        (quelpa-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (quelpa-build-expand-file-specs
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
                                            "\\.in\\'"
                                            ""
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun quelpa-build--config-file-list (config)
  "Get the :files spec from CONFIG, or return `quelpa-build-default-files-spec'."
  (let ((file-list (plist-get config :files)))
    (cond
     ((null file-list)
      quelpa-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append quelpa-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun quelpa-build--expand-source-file-list (dir config)
  "Shorthand way to expand paths in DIR for source files listed in CONFIG."
  (mapcar 'car
          (quelpa-build-expand-file-specs
           dir (quelpa-build--config-file-list config))))

(defun quelpa-build--generate-info-files (files source-dir target-dir)
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
        (when (not (file-exists-p info-path))
          (with-current-buffer (get-buffer-create "*quelpa-build-info*")
            (ignore-errors
              (quelpa-build--run-process
               (file-name-directory source-path)
               "makeinfo"
               source-path
               "-o"
               info-path)
              (quelpa-build--message "Created %s" info-path))))
        (quelpa-build--message "Removing %s"
                               (expand-file-name dest-file target-dir))
        (delete-file (expand-file-name dest-file target-dir))))))

;;; Info Manuals

(defun quelpa-build--generate-dir-file (files target-dir)
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
        (with-current-buffer (get-buffer-create "*quelpa-build-info*")
          (ignore-errors
            (quelpa-build--run-process
             nil
             "install-info"
             (concat "--dir=" (expand-file-name "dir" target-dir))
             info-path)))))))

;;; Utilities

(defun quelpa-build--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (cl-loop for (source-file . dest-file) in files
           do (quelpa-build--copy-file
               (expand-file-name source-file source-dir)
               (expand-file-name dest-file target-dir))))

(defun quelpa-build--copy-file (file newname)
  "Copy FILE to NEWNAME and create parent directories for NEWNAME if they don't exist."
  (let ((newdir (file-name-directory newname)))
    (unless (file-exists-p newdir)
      (make-directory newdir t)))
  (cond
   ((file-regular-p file)
    (quelpa-build--message "%s -> %s" file newname)
    (copy-file file newname))
   ((file-directory-p file)
    (quelpa-build--message "%s => %s" file newname)
    (copy-directory file newname))))

(defun quelpa-build--find-source-file (target files)
  "Search for source of TARGET in FILES."
  (car (rassoc target files)))

(defun quelpa-build--package-buffer-info-vec ()
  "Return a vector of package info.
`package-buffer-info' returns a vector in older Emacs versions,
and a cl struct in Emacs HEAD.  This wrapper normalises the results."
  (let ((desc (package-buffer-info))
        (keywords (lm-keywords-list)))
    (if (fboundp 'package-desc-create)
        (let ((extras (package-desc-extras desc)))
          (when (and keywords (not (assq :keywords extras)))
            ;; Add keywords to package properties, if not already present
            (push (cons :keywords keywords) extras))
          (vector (package-desc-name desc)
                  (package-desc-reqs desc)
                  (package-desc-summary desc)
                  (package-desc-version desc)
                  extras))
      (let ((homepage (lm-homepage))
            extras)
        (when keywords (push (cons :keywords keywords) extras))
        (when homepage (push (cons :url homepage) extras))
        (vector  (aref desc 0)
                 (aref desc 1)
                 (aref desc 2)
                 (aref desc 3)
                 extras)))))

;;; Building

;;;###autoload
(defun quelpa-build-package (package-name version file-specs source-dir target-dir)
  "Create PACKAGE-NAME with VERSION.

The information in FILE-SPECS is used to gather files from
SOURCE-DIR.

The resulting package will be stored as a .el or .tar file in
TARGET-DIR, depending on whether there are multiple files.

Argument FILE-SPECS is a list of specs for source files, which
should be relative to SOURCE-DIR.  The specs can be wildcards,
and optionally specify different target paths.  They extended
syntax is currently only documented in the MELPA README.  You can
simply pass `quelpa-build-default-files-spec' in most cases.

Returns the archive entry for the package."
  (when (symbolp package-name)
    (setq package-name (symbol-name package-name)))
  (let ((files (quelpa-build-expand-file-specs source-dir file-specs)))
    (unless (equal file-specs quelpa-build-default-files-spec)
      (when (equal files (quelpa-build-expand-file-specs
                          source-dir quelpa-build-default-files-spec nil t))
        (quelpa-build--message "Note: %s :files spec is equivalent to the default."
                               package-name)))
    (cond
     ((not version)
      (error "Unable to check out repository for %s" package-name))
     ((= 1 (length files))
      (quelpa-build--build-single-file-package
       package-name version (caar files) source-dir target-dir))
     ((< 1 (length  files))
      (quelpa-build--build-multi-file-package
       package-name version files source-dir target-dir))
     (t (error "Unable to find files matching recipe patterns")))))

(defun quelpa-build--build-single-file-package
    (package-name version file source-dir target-dir)
  (let* ((pkg-source (expand-file-name file source-dir))
         (pkg-target (expand-file-name
                      (concat package-name "-" version ".el")
                      target-dir))
         (pkg-info (quelpa-build--merge-package-info
                    (quelpa-build--get-package-info pkg-source)
                    package-name
                    version)))
    (unless (string-equal (downcase (concat package-name ".el"))
                          (downcase (file-name-nondirectory pkg-source)))
      (error "Single file %s does not match package name %s"
             (file-name-nondirectory pkg-source) package-name))
    (if (file-exists-p pkg-target)
        (quelpa-build--message "Skipping rebuild of %s" pkg-target)
      (copy-file pkg-source pkg-target)
      (let ((enable-local-variables nil)
            (make-backup-files nil))
        (with-temp-buffer
          (insert-file-contents pkg-target)
          (quelpa-build--update-or-insert-version version)
          (quelpa-build--ensure-ends-here-line pkg-source)
          (write-file pkg-target nil)
          (condition-case err
              (quelpa-build--package-buffer-info-vec)
            (error
             (quelpa-build--message "Warning: %S" err)))))

      (quelpa-build--write-pkg-readme
       target-dir
       (quelpa-build--find-package-commentary pkg-source)
       package-name))
    (quelpa-build--archive-entry pkg-info 'single)))

(defun quelpa-build--build-multi-file-package
    (package-name version files source-dir target-dir)
  (let ((tmp-dir (file-name-as-directory (make-temp-file package-name t))))
    (unwind-protect
        (let* ((pkg-dir-name (concat package-name "-" version))
               (pkg-tmp-dir (expand-file-name pkg-dir-name tmp-dir))
               (pkg-file (concat package-name "-pkg.el"))
               (pkg-file-source (or (quelpa-build--find-source-file pkg-file files)
                                    pkg-file))
               (file-source (concat package-name ".el"))
               (pkg-source (or (quelpa-build--find-source-file file-source files)
                               file-source))
               (pkg-info (quelpa-build--merge-package-info
                          (let ((default-directory source-dir))
                            (or (quelpa-build--get-pkg-file-info pkg-file-source)
                                ;; some packages (like magit) provide name-pkg.el.in
                                (quelpa-build--get-pkg-file-info
                                 (expand-file-name (concat pkg-file ".in")
                                                   (file-name-directory pkg-source)))
                                (quelpa-build--get-package-info pkg-source)))
                          package-name
                          version)))
          (quelpa-build--copy-package-files files source-dir pkg-tmp-dir)
          (quelpa-build--write-pkg-file (expand-file-name
                                         pkg-file
                                         (file-name-as-directory pkg-tmp-dir))
                                        pkg-info)

          (quelpa-build--generate-info-files files source-dir pkg-tmp-dir)
          (quelpa-build--generate-dir-file files pkg-tmp-dir)

          (let ((default-directory tmp-dir))
            (quelpa-build--create-tar
             (expand-file-name (concat package-name "-" version ".tar")
                               target-dir)
             pkg-dir-name))

          (let ((default-directory source-dir))
            (quelpa-build--write-pkg-readme
             target-dir
             (quelpa-build--find-package-commentary pkg-source)
             package-name))
          (quelpa-build--archive-entry pkg-info 'tar))
      (delete-directory tmp-dir t nil))))

(defun quelpa-build--checkout-file (name config dir)
  "Build according to a PATH with config CONFIG into DIR as NAME.
Generic local file handler for package-build.el.

Handles the following cases:

local file:

Installs a single-file package from a local file.  Use the :path
attribute with a PATH like \"/path/to/file.el\".

local directory:

Installs a multi-file package from a local directory.  Use
the :path attribute with a PATH like \"/path/to/dir\"."
  (quelpa-check-hash name config (expand-file-name (plist-get config :path)) dir))

(defun quelpa-build--checkout-url (name config dir)
  "Build according to an URL with config CONFIG into DIR as NAME.
Generic URL handler for package-build.el.

Handles the following cases:

local file:

Installs a single-file package from a local file.  Use the :url
attribute with an URL like \"file:///path/to/file.el\".

remote file:

Installs a single-file package from a remote file.  Use the :url
attribute with an URL like \"http://domain.tld/path/to/file.el\"."
  (let* ((url (plist-get config :url))
         (remote-file-name (file-name-nondirectory
                            (url-filename (url-generic-parse-url url))))
         (local-path (expand-file-name remote-file-name dir))
         (mm-attachment-file-modes (default-file-modes)))
    (unless (string= (file-name-extension url) "el")
      (error "<%s> does not end in .el" url))
    (unless (file-directory-p dir)
      (make-directory dir))
    (url-copy-file url local-path t)
    (quelpa-check-hash name config local-path dir 'url)))

;; --- helpers ---------------------------------------------------------------

(defun quelpa-message (wait format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS when `quelpa-verbose' is non-nil.
If WAIT is nil don't wait after showing the message. If it is a
number, wait so many seconds. If WAIT is t wait the default time.
Return t in each case."
  (when quelpa-verbose
    (message "Quelpa: %s" (apply 'format format-string args))
    (when (or (not noninteractive) wait) ; no wait if emacs is noninteractive
      (sit-for (or (and (numberp wait) wait) 1.5) t)))
  t)

(defun quelpa-read-cache ()
  "Read from `quelpa-persistent-cache-file' in `quelpa-cache'."
  (when (and quelpa-persistent-cache-p
             (file-exists-p quelpa-persistent-cache-file))
    (with-temp-buffer
      (insert-file-contents-literally quelpa-persistent-cache-file)
      (setq quelpa-cache
            (read (buffer-substring-no-properties (point-min) (point-max)))))))

(defun quelpa-save-cache ()
  "Write `quelpa-cache' to `quelpa-persistent-cache-file'."
  (when quelpa-persistent-cache-p
    (let (print-level print-length)
      (with-temp-file quelpa-persistent-cache-file
        (insert (prin1-to-string quelpa-cache))))))

(defun quelpa-update-cache (cache-item)
  "Update `quelpa-cache' with new CACHE-ITEM."
  ;; try removing existing recipes by name
  (setq quelpa-cache (cl-remove (car cache-item)
                                quelpa-cache :key #'car))
  (push cache-item quelpa-cache)
  (setq quelpa-cache
        (cl-sort quelpa-cache #'string<
                 :key (lambda (item) (symbol-name (car item))))))

(defun quelpa-parse-stable (cache-item)
  ;; in case :stable doesn't originate from PLIST, shadow the
  ;; default value anyways
  (when (plist-member (cdr cache-item) :stable)
    (setq quelpa-stable-p (plist-get (cdr cache-item) :stable)))
  (when (and quelpa-stable-p
             (plist-member (cdr cache-item) :stable)
             (not (plist-get (cdr cache-item) :stable)))
    (setf (cdr (last cache-item)) '(:stable t))))

;;;###autoload
(defun quelpa-checkout-melpa (&optional force)
  "Fetch or update the melpa source code from Github.
If there is no error return non-nil.
If there is an error but melpa is already checked out return non-nil.
If there is an error and no existing checkout return nil.

When FORCE is non-nil we will always update MELPA regrdless of
`quelpa-update-melpa-p`."
  (interactive "p")
  (or (and (not (or force quelpa-update-melpa-p))
           (file-exists-p (expand-file-name ".git" quelpa-melpa-dir)))
      (condition-case err
          (quelpa-build--checkout-git
           'package-build
           `(:url ,quelpa-melpa-repo-url :files ("*"))
           quelpa-melpa-dir)
        (error "Failed to checkout melpa git repo: `%s'" (error-message-string err)))))

(defun quelpa-get-melpa-recipe (name)
  "Read recipe with NAME for melpa git checkout.
Return the recipe if it exists, otherwise nil."
  (cl-loop for store in quelpa-melpa-recipe-stores
           if (stringp store)
           for file = (assoc-string name (directory-files store nil "^[^.].*$"))
           when file
           return (with-temp-buffer
                    (insert-file-contents-literally
                     (expand-file-name file store))
                    (read (buffer-string)))
           else
           for rcp = (assoc-string name store)
           when rcp
           return rcp))

(defun quelpa-setup-p ()
  "Setup what we need for quelpa.
Return non-nil if quelpa has been initialized properly."
  (catch 'quit
    (dolist (dir (list quelpa-packages-dir quelpa-build-dir))
      (unless (file-exists-p dir) (make-directory dir t)))
    (unless quelpa-initialized-p
      (quelpa-read-cache)
      (when (and quelpa-checkout-melpa-p
                 (not (quelpa-checkout-melpa)))
        (throw 'quit nil))
      (unless package-alist (package-load-all-descriptors))
      (setq quelpa-initialized-p t))
    t))

(defun quelpa-shutdown ()
  "Do things that need to be done after running quelpa."
  (quelpa-save-cache)
  ;; remove the packages dir because we are done with the built pkgs
  (ignore-errors (delete-directory quelpa-packages-dir t)))

(defun quelpa-arg-rcp (arg)
  "Given recipe or package name ARG, return an alist '(NAME . RCP).
If RCP cannot be found it will be set to nil"
  (pcase arg
    (`(,name) (quelpa-get-melpa-recipe name))
    (`(,name . ,_) arg)
    (name (quelpa-get-melpa-recipe name))))

(defun quelpa-parse-plist (plist)
  "Parse the optional PLIST argument of `quelpa'.
Recognized keywords are:

:upgrade

If t, `quelpa' tries to do an upgrade.

:stable

If t, `quelpa' tries building the stable version of a package.

:autoremove

If t, `quelpa' tries to remove obsoleted packages."
  (while plist
    (let ((key (car plist))
          (value (cadr plist)))
      (pcase key
        (:upgrade (setq quelpa-upgrade-p value))
        (:stable (setq quelpa-stable-p value))
        (:autoremove (setq quelpa-autoremove-p value))))
    (setq plist (cddr plist))))

(defun quelpa-package-install-file (file)
  "Workaround problem with `package-install-file'.
`package-install-file' uses `insert-file-contents-literally'
which causes problems when the FILE inserted has crlf line endings (Windows).
So here we replace that with `insert-file-contents' for non-tar files."
  (if (eq system-type 'windows-nt)
      (cl-letf* ((insert-file-contents-literally-orig
                  (symbol-function 'insert-file-contents-literally))
                 ((symbol-function 'insert-file-contents-literally)
                  (lambda (file)
                    (if (string-match "\\.tar\\'" file)
                        (funcall insert-file-contents-literally-orig file)
                      (insert-file-contents file)))))
        (package-install-file file))
    (package-install-file file)))

(defun quelpa-package-install (arg &rest plist)
  "Build and install package from ARG (a recipe or package name).
PLIST is a plist that may modify the build and/or fetch process.
If the package has dependencies recursively call this function to install them.
Return new package version."
  (let* ((rcp (quelpa-arg-rcp arg))
         (file (when rcp (quelpa-build (append rcp plist)))))
    (when file
      (let* ((pkg-desc (quelpa-get-package-desc file))
             (requires (package-desc-reqs pkg-desc))
             (ver (package-desc-version pkg-desc)))
        (when requires
          (mapc (lambda (req)
                  (unless (or (equal 'emacs (car req))
                              (quelpa--package-installed-p (car req) (cadr req)))
                    (quelpa-package-install (car req))))
                requires))
        (quelpa-package-install-file file)
        ver))))

(defun quelpa-interactive-candidate ()
  "Query the user for a recipe and return the name or recipe."
  (when (quelpa-setup-p)
    (let*  ((recipes (cl-loop
                      for store in quelpa-melpa-recipe-stores
                      if (stringp store)
                      ;; this regexp matches all files except dotfiles
                      append (directory-files store nil "^[^.].*$")
                      else if (listp store)
                      append store))
            (recipe (completing-read "Choose MELPA recipe: " recipes nil t)))
      (pcase (assoc-string recipe recipes)
        ((and re (pred stringp)) (intern re))
        (re re)))))

(defun quelpa--delete-obsoleted-package (name &optional new-version)
  "Delete obsoleted packages with name NAME.
With NEW-VERSION, will delete obsoleted packages that are not in same
version."
  (when-let ((all-pkgs (alist-get name package-alist))
             (new-pkg-version (or new-version
                                   (package-desc-version (car all-pkgs)))))
    (with-demoted-errors "Error deleting package: %S"
      (mapc (lambda (pkg-desc)
              (unless (equal (package-desc-version pkg-desc)
                             new-pkg-version)
                (let ((inhibit-message t))
                  (package-delete pkg-desc 'force))))
            all-pkgs))
    ;; Only packages with same version remained. Just pick the first one.
    (when-let (all-pkgs (alist-get name package-alist))
      (setf (cdr all-pkgs) nil))))

;; --- public interface ------------------------------------------------------

;;;###autoload
(defun quelpa-expand-recipe (recipe)
  "Expand a given RECIPE into full recipe.
If called interactively, let the user choose a recipe name and
insert the result into the current buffer."
  (interactive (list (quelpa-interactive-candidate)))
  (when (quelpa-setup-p)
    (let* ((recipe (if (listp recipe) recipe
                     (quelpa-get-melpa-recipe recipe))))
      (when recipe
        (if (called-interactively-p 'any)
            (prin1 recipe (current-buffer)))
        recipe))))

;;;###autoload
(defun quelpa-self-upgrade (&optional args)
  "Upgrade quelpa itself.
ARGS are additional options for the quelpa recipe."
  (interactive)
  (when (quelpa-setup-p)
    (quelpa (append quelpa-recipe args) :upgrade t)))

;;;###autoload
(defun quelpa-upgrade-all (&optional force)
  "Upgrade all packages found in `quelpa-cache'.
This provides an easy way to upgrade all the packages for which
the `quelpa' command has been run in the current Emacs session.
With prefix FORCE, packages will all be upgraded discarding local changes."
  (interactive "P")
  (when (quelpa-setup-p)
    (when quelpa-self-upgrade-p
      (quelpa-self-upgrade))
    (mapc (lambda (rcp)
            (quelpa-upgrade rcp (when force 'force)))
          quelpa-cache)))

;;;###autoload
(defun quelpa-upgrade (rcp &optional action)
  "Upgrade a package found in `quelpa-cache' with recipe RCP.
Optionally, ACTION can be passed for non-interactive call with value of:
- `force' (or \\[universal-argument] \\[quelpa-upgrade]) for forced upgrade.
- `local' (or \\[universal-argument] \\[universal-argument] \\[quelpa-upgrade])
  for upgrade using current working tree."
  (interactive
   (let ((prefix (prefix-numeric-value current-prefix-arg)))
     (list nil
           (cond  ((eq prefix 4) 'force)
                  ((eq prefix 16) 'local)))))
  (when (quelpa-setup-p)
    (let* ((rcp (or rcp
                    (let ((quelpa-melpa-recipe-stores
                           (list (cl-remove-if-not #'quelpa--package-installed-p
                                                   quelpa-cache :key #'car))))
                      (quelpa-interactive-candidate))))
           (quelpa-upgrade-p t)
           (current-prefix-arg nil)
           (config (append (cond ((eq action 'force) `(:force t))
                                 ((eq action 'local) `(:use-current-ref t)))
                           `(:autoremove ,quelpa-autoremove-p))))
      (when (quelpa--package-installed-p (car (quelpa-arg-rcp rcp)))
        (apply #'quelpa rcp config)))))

;;;###autoload
(defun quelpa (arg &rest plist)
  "Build and install a package with quelpa.
ARG can be a package name (symbol) or a melpa recipe (list).
PLIST is a plist that may modify the build and/or fetch process.
If called interactively, `quelpa' will prompt for a MELPA package
to install.

When `quelpa' is called interactively with a prefix argument (e.g
\\[universal-argument] \\[quelpa]) it will try to upgrade the
given package even if the global var `quelpa-upgrade-p' is set to
nil."
  (interactive (list nil))
  (run-hooks 'quelpa-before-hook)
  (when (quelpa-setup-p) ;if init fails we do nothing
    (let* ((arg (or arg
                    (let ((quelpa-melpa-recipe-stores
                           `(,@quelpa-melpa-recipe-stores ,quelpa-cache)))
                      (quelpa-interactive-candidate))))
           (quelpa-upgrade-p (if current-prefix-arg t quelpa-upgrade-p)) ;shadow `quelpa-upgrade-p'
           (quelpa-stable-p quelpa-stable-p) ;shadow `quelpa-stable-p'
           (quelpa-autoremove-p (if current-prefix-arg quelpa-autoremove-p nil))
           (cache-item (quelpa-arg-rcp arg)))
      (quelpa-parse-plist plist)
      (quelpa-parse-stable cache-item)
      (when-let ((ver (apply #'quelpa-package-install arg plist)))
        (when quelpa-autoremove-p
          (quelpa--delete-obsoleted-package (car cache-item) ver))
        (quelpa-update-cache cache-item))))
  (quelpa-shutdown)
  (run-hooks 'quelpa-after-hook))

;;;###autoload
(defun quelpa-upgrade-all-maybe (&optional force)
  "Run `quelpa-upgrade-all' if at least `quelpa-upgrade-interval' days have passed since the last run.
With prefix FORCE, packages will all be upgraded discarding local changes."
  (interactive "P")
  (when quelpa-upgrade-interval
    (let ((timestamp (expand-file-name "last_upgrade" quelpa-dir)))
      (when (or (not (file-exists-p timestamp))
                (> (- (time-to-seconds) ; Current time - modification time.
                      (time-to-seconds (nth 5 (file-attributes timestamp))))
                   (* 60 60 24 quelpa-upgrade-interval)))
        (quelpa-upgrade-all force)
        (write-region "" nil timestamp)))))

(provide 'quelpa)

;;; quelpa.el ends here
