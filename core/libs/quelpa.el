;;; quelpa.el --- Emacs Lisp packages built directly from source

;; Copyright 2014-2017, Steckerhalter
;; Copyright 2014-2015, Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; Package-Requires: ((package-build "0") (emacs "24.3"))
;; Keywords: package management build source elpa

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
;; https://github.com/quelpa/quelpa/blob/master/README.md

;;; Requirements:

;; Emacs 24.3.1

;;; Code:

(require 'package-build)
(require 'cl-lib)
(require 'help-fns)
(require 'url-parse)

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
  `quelpa-upgrade', otherwise only upgrade the packages in the
  quelpa cache."
  :group 'quelpa
  :type 'boolean)

(defvar quelpa-initialized-p nil
  "Non-nil when quelpa has been initialized.")

(defvar quelpa-cache nil
  "The `quelpa' command stores processed pkgs/recipes in the cache.")

(defvar quelpa-recipe '(quelpa :repo "quelpa/quelpa" :fetcher github)
  "The recipe for quelpa.")

;; --- compatibility for legacy `package.el' in Emacs 24.3  -------------------

(defun quelpa-setup-package-structs ()
  "Setup the struct `package-desc' when not available.
`package-desc-from-legacy' is provided to convert the legacy
vector desc into a valid PACKAGE-DESC."
  (unless (functionp 'package-desc-p)
    (cl-defstruct
        (package-desc
         (:constructor
          ;; convert legacy package desc into PACKAGE-DESC
          package-desc-from-legacy
          (pkg-info kind
                    &aux
                    (name (intern (aref pkg-info 0)))
                    (version (version-to-list (aref pkg-info 3)))
                    (summary (if (string= (aref pkg-info 2) "")
                                 "No description available."
                               (aref pkg-info 2)))
                    (reqs  (aref pkg-info 1))
                    (kind kind))))
      name
      version
      (summary "No description available.")
      reqs
      kind
      archive
      dir
      extras
      signed)))

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
                           (if (help-function-arglist 'package-tar-file-info)
                               ;; legacy `package-tar-file-info' requires an arg
                               (package-tar-file-info file)
                             (with-no-warnings (package-tar-file-info)))))))))
    (pcase desc
      ((pred package-desc-p) desc)
      ((pred vectorp) (package-desc-from-legacy desc kind)))))

(defun quelpa-archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     quelpa-packages-dir)))

(defun quelpa-version>-p (name version)
  "Return non-nil if VERSION of pkg with NAME is newer than what is currently installed."
  (not (or (not version)
           (let ((pkg-desc (cdr (assq name package-alist))))
             (and pkg-desc
                  (version-list-<=
                   (version-to-list version)
                   (if (functionp 'package-desc-vers)
                       (package-desc-vers pkg-desc) ;old implementation
                     (package-desc-version (car pkg-desc))))))
           ;; Also check built-in packages.
           (package-built-in-p name (version-to-list version)))))

(defun quelpa-checkout (rcp dir)
  "Return the version of the new package given a RCP.
Return nil if the package is already installed and should not be upgraded."
  (pcase-let ((`(,name . ,config) rcp)
              (package-build-stable quelpa-stable-p))
    (unless (or (and (assq name package-alist) (not quelpa-upgrade-p))
                (and (not config)
                     (quelpa-message t "no recipe found for package `%s'" name)))
      (if (member (plist-get config :fetcher) '(wiki bzr cvs darcs fossil svn))
          (user-error
           "The `%s' fetcher is not supported anymore.
It has been removed from the `package-build' library: cannot install `%s'"
           (plist-get config :fetcher)
           name)
        (let ((version (condition-case err
                           (package-build-checkout name config dir)
                         (error "failed to checkout `%s': `%s'"
                                name (error-message-string err)))))
          (when (quelpa-version>-p name version)
            version))))))

(defun quelpa-build-package (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-build-dir' storing it in
`quelpa-packages-dir'. Return the path to the created file or nil
if no action is necessary (like when the package is installed
already and should not be upgraded etc)."
  (let* ((name (car rcp))
         (build-dir (expand-file-name (symbol-name name) quelpa-build-dir))
         (version (quelpa-checkout rcp build-dir)))
    (when version
      (quelpa-archive-file-name
       (package-build-package (symbol-name name)
                              version
                              (package-build--config-file-list (cdr rcp))
                              build-dir
                              quelpa-packages-dir)))))

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
      (cddr (directory-files path))))
    result))

(defun quelpa-expand-source-file-list (file-path config)
  "Return list of source files from FILE-PATH corresponding to
CONFIG."
  (let ((source-files
         (mapcar
          (lambda (file) (expand-file-name file file-path))
          (package-build--expand-source-file-list file-path config))))
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
         (old-stamp-info (package-build--read-from-file stamp-file))
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
      (package-build--dump new-stamp-info stamp-file)
      (quelpa-file-version file-path type version time-stamp))))

(defun package-build--checkout-file (name config dir)
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

(defun package-build--checkout-url (name config dir)
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
  (when (and quelpa-stable-p (not (plist-get (cdr cache-item) :stable)))
    (setf (cdr (last cache-item)) '(:stable t))))

(defun quelpa-checkout-melpa ()
  "Fetch or update the melpa source code from Github.
If there is no error return non-nil.
If there is an error but melpa is already checked out return non-nil.
If there is an error and no existing checkout return nil."
  (or (and (null quelpa-update-melpa-p)
           (file-exists-p (expand-file-name ".git" quelpa-melpa-dir)))
      (condition-case err
          (package-build--checkout-git
           'package-build
           `(:url ,quelpa-melpa-repo-url :files ("*"))
           quelpa-melpa-dir)
        (error "failed to checkout melpa git repo: `%s'" (error-message-string err)))))

(defun quelpa-get-melpa-recipe (name)
  "Read recipe with NAME for melpa git checkout.
Return the recipe if it exists, otherwise nil."
  (cl-loop for store in quelpa-melpa-recipe-stores
           if (stringp store)
           for file = (assoc-string name (directory-files store nil "^[^\.]+"))
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
      (quelpa-setup-package-structs)
      (if quelpa-checkout-melpa-p
          (unless (quelpa-checkout-melpa) (throw 'quit nil)))
      (setq quelpa-initialized-p t))
    t))

(defun quelpa-shutdown ()
  "Do things that need to be done after running quelpa."
  (quelpa-save-cache)
  ;; remove the packages dir because we are done with the built pkgs
  (ignore-errors (delete-directory quelpa-packages-dir t)))

(defun quelpa-arg-rcp (arg)
  "Given recipe or package name, return an alist '(NAME . RCP).
If RCP cannot be found it will be set to nil"
  (pcase arg
    (`(,a . nil) (quelpa-get-melpa-recipe (car arg)))
    (`(,a . ,_) arg)
    ((pred symbolp) (quelpa-get-melpa-recipe arg))))

(defun quelpa-parse-plist (plist)
  "Parse the optional PLIST argument of `quelpa'.
Recognized keywords are:

:upgrade

If t, `quelpa' tries to do an upgrade.

:stable

If t, `quelpa' tries building the stable version of a package."
  (while plist
    (let ((key (car plist))
          (value (cadr plist)))
      (pcase key
        (:upgrade (setq quelpa-upgrade-p value))
        (:stable (setq quelpa-stable-p value))))
    (setq plist (cddr plist))))

(defun quelpa-package-install-file (file)
  "Workaround problem with `package-install-file'.
`package-install-file' uses `insert-file-contents-literally'
which causes problems when the file inserted has crlf line
endings (Windows). So here we replace that with
`insert-file-contents' for non-tar files."
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

(defun quelpa-package-install (arg)
  "Build and install package from ARG (a recipe or package name).
If the package has dependencies recursively call this function to
install them."
  (let* ((rcp (quelpa-arg-rcp arg))
         (file (and rcp (quelpa-build-package rcp))))
    (when file
      (let* ((pkg-desc (quelpa-get-package-desc file))
             (requires (package-desc-reqs pkg-desc)))
        (when requires
          (mapc (lambda (req)
                  (unless (or (equal 'emacs (car req))
                              (package-installed-p (car req) (cadr req)))
                    (quelpa-package-install (car req))))
                requires))
        (quelpa-package-install-file file)))))

(defun quelpa-interactive-candidate ()
  "Query the user for a recipe and return the name."
  (when (quelpa-setup-p)
    (let  ((recipes (cl-loop
                     for store in quelpa-melpa-recipe-stores
                     if (stringp store)
                     ;; this regexp matches all files except dotfiles
                     append (directory-files store nil "^[^.].+$")
                     else if (listp store)
                     append store)))
      (intern (completing-read "Choose MELPA recipe: "
                               recipes nil t)))))

;; --- public interface ------------------------------------------------------

;;;###autoload
(defun quelpa-expand-recipe (recipe-name)
  "Expand a given recipe name into full recipe.
If called interactively, let the user choose a recipe name and
insert the result into the current buffer."
  (interactive (list (quelpa-interactive-candidate)))
  (when (quelpa-setup-p)
    (let* ((recipe (quelpa-get-melpa-recipe recipe-name)))
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
(defun quelpa-upgrade ()
  "Upgrade all packages found in `quelpa-cache'.
This provides an easy way to upgrade all the packages for which
the `quelpa' command has been run in the current Emacs session."
  (interactive)
  (when (quelpa-setup-p)
    (let ((quelpa-upgrade-p t))
      (when quelpa-self-upgrade-p
        (quelpa-self-upgrade))
      (setq quelpa-cache
            (cl-remove-if-not #'package-installed-p quelpa-cache :key #'car))
      (mapc (lambda (item)
              (when (package-installed-p (car (quelpa-arg-rcp item)))
                (quelpa item)))
            quelpa-cache))))

;;;###autoload
(defun quelpa (arg &rest plist)
  "Build and install a package with quelpa.
ARG can be a package name (symbol) or a melpa recipe (list).
PLIST is a plist that may modify the build and/or fetch process.
If called interactively, `quelpa' will prompt for a MELPA package
to install.

When `quelpa' is called interactively with a prefix argument (e.g
C-u M-x quelpa) it will try to upgrade the given package even if
the global var `quelpa-upgrade-p' is set to nil."

  (interactive (list (quelpa-interactive-candidate)))
  (run-hooks 'quelpa-before-hook)
  (when (quelpa-setup-p) ;if init fails we do nothing
    (let* ((quelpa-upgrade-p (if current-prefix-arg t quelpa-upgrade-p)) ;shadow `quelpa-upgrade-p'
           (quelpa-stable-p quelpa-stable-p) ;shadow `quelpa-stable-p'
           (cache-item (if (symbolp arg) (list arg) arg)))
      (quelpa-parse-plist plist)
      (quelpa-parse-stable cache-item)
      (quelpa-package-install arg)
      (quelpa-update-cache cache-item)))
  (quelpa-shutdown)
  (run-hooks 'quelpa-after-hook))

(provide 'quelpa)

;;; quelpa.el ends here
