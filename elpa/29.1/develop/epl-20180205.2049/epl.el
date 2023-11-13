;;; epl.el --- Emacs Package Library -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Sebastian Wiesner
;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2015 Free Software

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <swiesner@lunaryorn.com>
;; Version: 0.10-cvs
;; Package-Requires: ((cl-lib "0.3"))
;; Keywords: convenience
;; URL: http://github.com/cask/epl

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package management library for Emacs, based on package.el.

;; The purpose of this library is to wrap all the quirks and hassle of
;; package.el into a sane API.

;; The following functions comprise the public interface of this library:

;;; Package directory selection

;; `epl-package-dir' gets the directory of packages.

;; `epl-default-package-dir' gets the default package directory.

;; `epl-change-package-dir' changes the directory of packages.

;;; Package system management

;; `epl-initialize' initializes the package system and activates all
;; packages.

;; `epl-reset' resets the package system.

;; `epl-refresh' refreshes all package archives.

;; `epl-add-archive' adds a new package archive.

;;; Package objects

;; Struct `epl-requirement' describes a requirement of a package with `name' and
;; `version' slots.

;; `epl-requirement-version-string' gets a requirement version as string.

;; Struct `epl-package' describes an installed or installable package with a
;; `name' and some internal `description'.

;; `epl-package-version' gets the version of a package.

;; `epl-package-version-string' gets the version of a package as string.

;; `epl-package-summary' gets the summary of a package.

;; `epl-package-requirements' gets the requirements of a package.

;; `epl-package-directory' gets the installation directory of a package.

;; `epl-package-from-buffer' creates a package object for the package contained
;; in the current buffer.

;; `epl-package-from-file' creates a package object for a package file, either
;; plain lisp or tarball.

;; `epl-package-from-descriptor-file' creates a package object for a package
;; description (i.e. *-pkg.el) file.

;;; Package database access

;; `epl-package-installed-p' determines whether a package is installed, either
;; built-in or explicitly installed.

;; `epl-package-outdated-p' determines whether a package is outdated, that is,
;; whether a package with a higher version number is available.

;; `epl-built-in-packages', `epl-installed-packages', `epl-outdated-packages'
;; and `epl-available-packages' get all packages built-in, installed, outdated,
;; or available for installation respectively.

;; `epl-find-built-in-package', `epl-find-installed-packages' and
;; `epl-find-available-packages' find built-in, installed and available packages
;; by name.

;; `epl-find-upgrades' finds all upgradable packages.

;; `epl-built-in-p' return true if package is built-in to Emacs.

;;; Package operations

;; `epl-install-file' installs a package file.

;; `epl-package-install' installs a package.

;; `epl-package-delete' deletes a package.

;; `epl-upgrade' upgrades packages.

;;; Code:

(require 'cl-lib)
(require 'package)


(unless (fboundp #'define-error)
  ;; `define-error' for 24.3 and earlier, copied from subr.el
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'append
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(defsubst epl--package-desc-p (package)
  "Whether PACKAGE is a `package-desc' object.

Like `package-desc-p', but return nil, if `package-desc-p' is not
defined as function."
  (and (fboundp 'package-desc-p) (package-desc-p package)))


;;; EPL errors
(define-error 'epl-error "EPL error")

(define-error 'epl-invalid-package "Invalid EPL package" 'epl-error)

(define-error 'epl-invalid-package-file "Invalid EPL package file"
  'epl-invalid-package)


;;; Package directory
(defun epl-package-dir ()
  "Get the directory of packages."
  package-user-dir)

(defun epl-default-package-dir ()
  "Get the default directory of packages."
  (eval (car (get 'package-user-dir 'standard-value))))

(defun epl-change-package-dir (directory)
  "Change the directory of packages to DIRECTORY."
  (setq package-user-dir directory)
  (epl-initialize))


;;; Package system management
(defvar epl--load-path-before-initialize nil
  "Remember the load path for `epl-reset'.")

(defun epl-initialize (&optional no-activate)
  "Load Emacs Lisp packages and activate them.

With NO-ACTIVATE non-nil, do not activate packages."
  (setq epl--load-path-before-initialize load-path)
  (package-initialize no-activate))

(defalias 'epl-refresh 'package-refresh-contents)

(defun epl-add-archive (name url)
  "Add a package archive with NAME and URL."
  (add-to-list 'package-archives (cons name url)))

(defun epl-reset ()
  "Reset the package system.

Clear the list of installed and available packages, the list of
package archives and reset the package directory."
  (setq package-alist nil
        package-archives nil
        package-archive-contents nil
        load-path epl--load-path-before-initialize)
  (when (boundp 'package-obsolete-alist) ; Legacy package.el
    (setq package-obsolete-alist nil))
  (epl-change-package-dir (epl-default-package-dir)))


;;; Package structures
(cl-defstruct (epl-requirement
               (:constructor epl-requirement-create))
  "Structure describing a requirement.

Slots:

`name' The name of the required package, as symbol.

`version' The version of the required package, as version list."
  name
  version)

(defun epl-requirement-version-string (requirement)
  "The version of a REQUIREMENT, as string."
  (package-version-join (epl-requirement-version requirement)))

(cl-defstruct (epl-package (:constructor epl-package-create))
  "Structure representing a package.

Slots:

`name' The package name, as symbol.

`description' The package description.

The format package description varies between package.el
variants.  For `package-desc' variants, it is simply the
corresponding `package-desc' object.  For legacy variants, it is
a vector `[VERSION REQS DOCSTRING]'.

Do not access `description' directly, but instead use the
`epl-package' accessors."
  name
  description)

(defmacro epl-package-as-description (var &rest body)
  "Cast VAR to a package description in BODY.

VAR is a symbol, bound to an `epl-package' object.  This macro
casts this object to the `description' object, and binds the
description to VAR in BODY."
  (declare (indent 1))
  (unless (symbolp var)
    (signal 'wrong-type-argument (list #'symbolp var)))
  `(if (epl-package-p ,var)
       (let ((,var (epl-package-description ,var)))
         ,@body)
     (signal 'wrong-type-argument (list #'epl-package-p ,var))))

(defsubst epl-package--package-desc-p (package)
  "Whether the description of PACKAGE is a `package-desc'."
  (epl--package-desc-p (epl-package-description package)))

(defun epl-package-version (package)
  "Get the version of PACKAGE, as version list."
  (epl-package-as-description package
    (cond
     ((fboundp 'package-desc-version) (package-desc-version package))
     ;; Legacy
     ((fboundp 'package-desc-vers)
      (let ((version (package-desc-vers package)))
        (if (listp version) version (version-to-list version))))
     (:else (error "Cannot get version from %S" package)))))

(defun epl-package-version-string (package)
  "Get the version from a PACKAGE, as string."
  (package-version-join (epl-package-version package)))

(defun epl-package-summary (package)
  "Get the summary of PACKAGE, as string."
  (epl-package-as-description package
    (cond
     ((fboundp 'package-desc-summary) (package-desc-summary package))
     ((fboundp 'package-desc-doc) (package-desc-doc package)) ; Legacy
     (:else (error "Cannot get summary from %S" package)))))

(defsubst epl-requirement--from-req (req)
  "Create a `epl-requirement' from a `package-desc' REQ."
  (let  ((version (cadr req)))
    (epl-requirement-create :name (car req)
                            :version (if (listp version) version
                                       (version-to-list version)))))

(defun epl-package-requirements (package)
  "Get the requirements of PACKAGE.

The requirements are a list of `epl-requirement' objects."
  (epl-package-as-description package
    (mapcar #'epl-requirement--from-req (package-desc-reqs package))))

(defun epl-package-directory (package)
  "Get the directory PACKAGE is installed to.

Return the absolute path of the installation directory of
PACKAGE, or nil, if PACKAGE is not installed."
  (cond
   ((fboundp 'package-desc-dir)
    (package-desc-dir (epl-package-description package)))
   ((fboundp 'package--dir)
    (package--dir (symbol-name (epl-package-name package))
                  (epl-package-version-string package)))
   (:else (error "Cannot get package directory from %S" package))))

(defun epl-package-->= (pkg1 pkg2)
  "Determine whether PKG1 is before PKG2 by version."
  (not (version-list-< (epl-package-version pkg1)
                       (epl-package-version pkg2))))

(defun epl-package--from-package-desc (package-desc)
  "Create an `epl-package' from a PACKAGE-DESC.

PACKAGE-DESC is a `package-desc' object, from recent package.el
variants."
  (if (and (fboundp 'package-desc-name)
           (epl--package-desc-p package-desc))
      (epl-package-create :name (package-desc-name package-desc)
                          :description package-desc)
    (signal 'wrong-type-argument (list 'epl--package-desc-p package-desc))))

(defun epl-package--parse-info (info)
  "Parse a package.el INFO."
  (if (epl--package-desc-p info)
      (epl-package--from-package-desc info)
    ;; For legacy package.el, info is a vector [NAME REQUIRES DESCRIPTION
    ;; VERSION COMMENTARY].  We need to re-shape this vector into the
    ;; `package-alist' format [VERSION REQUIRES DESCRIPTION] to attach it to the
    ;; new `epl-package'.
    (let ((name (intern (aref info 0)))
          (info (vector (aref info 3) (aref info 1) (aref info 2))))
      (epl-package-create :name name :description info))))

(defun epl-package-from-buffer (&optional buffer)
  "Create an `epl-package' object from BUFFER.

BUFFER defaults to the current buffer.

Signal `epl-invalid-package' if the buffer does not contain a
valid package file."
  (let ((info (with-current-buffer (or buffer (current-buffer))
                (condition-case err
                    (package-buffer-info)
                  (error (signal 'epl-invalid-package (cdr err)))))))
    (epl-package--parse-info info)))

(defun epl-package-from-lisp-file (file-name)
  "Parse the package headers the file at FILE-NAME.

Return an `epl-package' object with the header metadata."
  (with-temp-buffer
    (insert-file-contents file-name)
    (condition-case err
        (epl-package-from-buffer (current-buffer))
      ;; Attach file names to invalid package errors
      (epl-invalid-package
       (signal 'epl-invalid-package-file (cons file-name (cdr err))))
      ;; Forward other errors
      (error (signal (car err) (cdr err))))))

(defun epl-package-from-tar-file (file-name)
  "Parse the package tarball at FILE-NAME.

Return a `epl-package' object with the meta data of the tarball
package in FILE-NAME."
  (condition-case nil
      ;; In legacy package.el, `package-tar-file-info' takes the name of the tar
      ;; file to parse as argument.  In modern package.el, it has no arguments
      ;; and works on the current buffer.  Hence, we just try to call the legacy
      ;; version, and if that fails because of a mismatch between formal and
      ;; actual arguments, we use the modern approach.  To avoid spurious
      ;; signature warnings by the byte compiler, we suppress warnings when
      ;; calling the function.
      (epl-package--parse-info (with-no-warnings
                                 (package-tar-file-info file-name)))
    (wrong-number-of-arguments
     (with-temp-buffer
       (insert-file-contents-literally file-name)
       ;; Switch to `tar-mode' to enable extraction of the file.  Modern
       ;; `package-tar-file-info' relies on `tar-mode', and signals an error if
       ;; called in a buffer with a different mode.
       (tar-mode)
       (epl-package--parse-info (with-no-warnings
                                  (package-tar-file-info)))))))

(defun epl-package-from-file (file-name)
  "Parse the package at FILE-NAME.

Return an `epl-package' object with the meta data of the package
at FILE-NAME."
  (if (string-match-p (rx ".tar" string-end) file-name)
      (epl-package-from-tar-file file-name)
    (epl-package-from-lisp-file file-name)))

(defun epl-package--parse-descriptor-requirement (requirement)
  "Parse a REQUIREMENT in a package descriptor."
  ;; This function is only called on legacy package.el.  On package-desc
  ;; package.el, we just let package.el do the work.
  (cl-destructuring-bind (name version-string) requirement
    (list name (version-to-list version-string))))

(defun epl-package-from-descriptor-file (descriptor-file)
  "Load a `epl-package' from a package DESCRIPTOR-FILE.

A package descriptor is a file defining a new package.  Its name
typically ends with -pkg.el."
  (with-temp-buffer
    (insert-file-contents descriptor-file)
    (goto-char (point-min))
    (let ((sexp (read (current-buffer))))
      (unless (eq (car sexp) 'define-package)
        (error "%S is no valid package descriptor" descriptor-file))
      (if (and (fboundp 'package-desc-from-define)
               (fboundp 'package-desc-name))
          ;; In Emacs snapshot, we can conveniently call a function to parse the
          ;; descriptor
          (let ((desc (apply #'package-desc-from-define (cdr sexp))))
            (epl-package-create :name (package-desc-name desc)
                                :description desc))
        ;; In legacy package.el, we must manually deconstruct the descriptor,
        ;; because the load function has eval's the descriptor and has a lot of
        ;; global side-effects.
        (cl-destructuring-bind
            (name version-string summary requirements) (cdr sexp)
          (epl-package-create
           :name (intern name)
           :description
           (vector (version-to-list version-string)
                   (mapcar #'epl-package--parse-descriptor-requirement
                           ;; Strip the leading `quote' from the package list
                           (cadr requirements))
                   summary)))))))


;;; Package database access
(defun epl-package-installed-p (package &optional min-version)
  "Determine whether a PACKAGE, of MIN-VERSION or newer, is installed.

PACKAGE is either a package name as symbol, or a package object.
When a explicit MIN-VERSION is provided it overwrites the version of the PACKAGE object."
  (let ((name (if (epl-package-p package)
                  (epl-package-name package)
                package))
        (min-version (or min-version (and (epl-package-p package)
                                          (epl-package-version package)))))
    (package-installed-p name min-version)))

(defun epl--parse-built-in-entry (entry)
  "Parse an ENTRY from the list of built-in packages.

Return the corresponding `epl-package' object."
  (if (fboundp 'package--from-builtin)
      ;; In package-desc package.el, convert the built-in package to a
      ;; `package-desc' and convert that to an `epl-package'
      (epl-package--from-package-desc (package--from-builtin entry))
    (epl-package-create :name (car entry) :description (cdr entry))))

(defun epl-built-in-packages ()
  "Get all built-in packages.

Return a list of `epl-package' objects."
  ;; This looks mighty strange, but it's the only way to force package.el to
  ;; build the list of built-in packages.  Without this, `package--builtins'
  ;; might be empty.
  (package-built-in-p 'foo)
  (mapcar #'epl--parse-built-in-entry package--builtins))

(defun epl-find-built-in-package (name)
  "Find a built-in package with NAME.

NAME is a package name, as symbol.

Return the built-in package as `epl-package' object, or nil if
there is no built-in package with NAME."
  (when (package-built-in-p name)
    ;; We must call `package-built-in-p' *before* inspecting
    ;; `package--builtins', because otherwise `package--builtins' might be
    ;; empty.
    (epl--parse-built-in-entry (assq name package--builtins))))

(defun epl-package-outdated-p (package)
  "Determine whether a PACKAGE is outdated.

A package is outdated, if there is an available package with a
higher version.

PACKAGE is either a package name as symbol, or a package object.
In the former case, test the installed or built-in package with
the highest version number, in the later case, test the package
object itself.

Return t, if the package is outdated, or nil otherwise."
  (let* ((package (if (epl-package-p package)
                      package
                    (or (car (epl-find-installed-packages package))
                        (epl-find-built-in-package package))))
         (available (car (epl-find-available-packages
                          (epl-package-name package)))))
    (and package available (version-list-< (epl-package-version package)
                                           (epl-package-version available)))))

(defun epl--parse-package-list-entry (entry)
  "Parse a list of packages from ENTRY.

ENTRY is a single entry in a package list, e.g. `package-alist',
`package-archive-contents', etc.  Typically it is a cons cell,
but the exact format varies between package.el versions.  This
function tries to parse all known variants.

Return a list of `epl-package' objects parsed from ENTRY."
  (let ((descriptions (cdr entry)))
    (cond
     ((listp descriptions)
      (sort (mapcar #'epl-package--from-package-desc descriptions)
            #'epl-package-->=))
     ;; Legacy package.el has just a single package in an entry, which is a
     ;; standard description vector
     ((vectorp descriptions)
      (list (epl-package-create :name (car entry)
                                :description descriptions)))
     (:else (error "Cannot parse entry %S" entry)))))

(defun epl-installed-packages ()
  "Get all installed packages.

Return a list of package objects."
  (apply #'append (mapcar #'epl--parse-package-list-entry package-alist)))

(defsubst epl--filter-outdated-packages (packages)
  "Filter outdated packages from PACKAGES."
  (let (res)
    (dolist (package packages)
      (when (epl-package-outdated-p package)
        (push package res)))
    (nreverse res)))

(defun epl-outdated-packages ()
  "Get all outdated packages, as in `epl-package-outdated-p'.

Return a list of package objects."
  (epl--filter-outdated-packages (epl-installed-packages)))

(defsubst epl--find-package-in-list (name list)
  "Find a package by NAME in a package LIST.

Return a list of corresponding `epl-package' objects."
  (let ((entry (assq name list)))
    (when entry
      (epl--parse-package-list-entry entry))))

(defun epl-find-installed-package (name)
  "Find the latest installed package by NAME.

NAME is a package name, as symbol.

Return the installed package with the highest version number as
`epl-package' object, or nil, if no package with NAME is
installed."
  (car (epl-find-installed-packages name)))
(make-obsolete 'epl-find-installed-package 'epl-find-installed-packages "0.7")

(defun epl-find-installed-packages (name)
  "Find all installed packages by NAME.

NAME is a package name, as symbol.

Return a list of all installed packages with NAME, sorted by
version number in descending order.  Return nil, if there are no
packages with NAME."
  (epl--find-package-in-list name package-alist))

(defun epl-available-packages ()
  "Get all packages available for installation.

Return a list of package objects."
  (apply #'append (mapcar #'epl--parse-package-list-entry
                          package-archive-contents)))

(defun epl-find-available-packages (name)
  "Find available packages for NAME.

NAME is a package name, as symbol.

Return a list of available packages for NAME, sorted by version
number in descending order.  Return nil, if there are no packages
for NAME."
  (epl--find-package-in-list name package-archive-contents))

(cl-defstruct (epl-upgrade
               (:constructor epl-upgrade-create))
  "Structure describing an upgradable package.
Slots:

`installed' The installed package

`available' The package available for installation."
  installed
  available)

(defun epl-find-upgrades (&optional packages)
  "Find all upgradable PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

Return a list of `epl-upgrade' objects describing all upgradable
packages."
  (let ((packages (or packages (epl-installed-packages)))
        upgrades)
    (dolist (pkg packages)
      (let* ((version (epl-package-version pkg))
             (name (epl-package-name pkg))
             ;; Find the latest available package for NAME
             (available-pkg (car (epl-find-available-packages name)))
             (available-version (when available-pkg
                                  (epl-package-version available-pkg))))
        (when (and available-version (version-list-< version available-version))
          (push (epl-upgrade-create :installed pkg
                                    :available available-pkg)
                upgrades))))
    (nreverse upgrades)))

(defalias 'epl-built-in-p 'package-built-in-p)


;;; Package operations

(defun epl-install-file (file)
    "Install a package from FILE, like `package-install-file'."
    (interactive (advice-eval-interactive-spec
                  (cadr (interactive-form #'package-install-file))))
    (apply #'package-install-file (list file))
    (let ((package (epl-package-from-file file)))
      (unless (epl-package--package-desc-p package)
        (epl--kill-autoload-buffer package))))

(defun epl--kill-autoload-buffer (package)
  "Kill the buffer associated with autoloads for PACKAGE."
  (let* ((auto-name (format "%s-autoloads.el" (epl-package-name package)))
         (generated-autoload-file (expand-file-name auto-name (epl-package-directory package)))
         (buf (find-buffer-visiting generated-autoload-file)))
    (when buf (kill-buffer buf))))

(defun epl-package-install (package &optional force)
  "Install a PACKAGE.

PACKAGE is a `epl-package' object.  If FORCE is given and
non-nil, install PACKAGE, even if it is already installed."
  (when (or force (not (epl-package-installed-p package)))
    (if (epl-package--package-desc-p package)
        (package-install (epl-package-description package))
      ;; The legacy API installs by name.  We have no control over versioning,
      ;; etc.
      (package-install (epl-package-name package))
      (epl--kill-autoload-buffer package))))

(defun epl-package-delete (package)
  "Delete a PACKAGE.

PACKAGE is a `epl-package' object to delete."
  ;; package-delete allows for packages being trashed instead of fully deleted.
  ;; Let's prevent his silly behavior
  (let ((delete-by-moving-to-trash nil))
    ;; The byte compiler will warn us that we are calling `package-delete' with
    ;; the wrong number of arguments, since it can't infer that we guarantee to
    ;; always call the correct version.  Thus we suppress all warnings when
    ;; calling `package-delete'.  I wish there was a more granular way to
    ;; disable just that specific warning, but it is what it is.
    (if (epl-package--package-desc-p package)
        (with-no-warnings
          (package-delete (epl-package-description package)))
      ;; The legacy API deletes by name (as string!) and version instead by
      ;; descriptor.  Hence `package-delete' takes two arguments.  For some
      ;; insane reason, the arguments are strings here!
      (let ((name (symbol-name (epl-package-name package)))
            (version (epl-package-version-string package)))
        (with-no-warnings
          (package-delete name version))
        ;; Legacy package.el does not remove the deleted package
        ;; from the `package-alist', so we do it manually here.
        (let ((pkg (assq (epl-package-name package) package-alist)))
          (when pkg
            (setq package-alist (delq pkg package-alist))))))))

(defun epl-upgrade (&optional packages preserve-obsolete)
  "Upgrade PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

The old versions of the updated packages are deleted, unless
PRESERVE-OBSOLETE is non-nil.

Return a list of all performed upgrades, as a list of
`epl-upgrade' objects."
  (let ((upgrades (epl-find-upgrades packages)))
    (dolist (upgrade upgrades)
      (epl-package-install (epl-upgrade-available upgrade) 'force)
      (unless preserve-obsolete
        (epl-package-delete (epl-upgrade-installed upgrade))))
    upgrades))

(provide 'epl)

;;; epl.el ends here
