;;; pkg-info.el --- Information about packages       -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/pkg-info.el
;; Keywords: convenience
;; Version: 0.7-cvs
;; Package-Requires: ((epl "0.8"))

;; This file is not part of GNU Emacs.

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

;; This library extracts information from installed packages.

;;;; Functions:

;; `pkg-info-library-version' extracts the version from the header of a library.
;;
;; `pkg-info-defining-library-version' extracts the version from the header of a
;;  library defining a function.
;;
;; `pkg-info-package-version' gets the version of an installed package.
;;
;; `pkg-info-format-version' formats a version list as human readable string.
;;
;; `pkg-info-version-info' returns complete version information for a specific
;; package.
;;
;; `pkg-info-get-melpa-recipe' gets the MELPA recipe for a package.
;;
;; `pkg-info-get-melpa-fetcher' gets the fetcher used to build a package on
;; MELPA.
;;
;; `pkg-info-wiki-package-p' determines whether a package was build from
;; EmacsWiki on MELPA.

;;; Code:

(require 'epl)

(require 'lisp-mnt)
(require 'find-func)
(require 'json)                         ; `json-read'
(require 'url-http)                     ; `url-http-parse-response'

(defvar url-http-end-of-headers)


;;; Version information
(defun pkg-info-format-version (version)
  "Format VERSION as human-readable string.

Return a human-readable string representing VERSION."
  ;; XXX: Find a better, more flexible way of formatting?
  (package-version-join version))

(defsubst pkg-info--show-version-and-return (version show)
  "Show and return VERSION.

When SHOW is non-nil, show VERSION in minibuffer.

Return VERSION."
  (when show
    (message (if (listp version) (pkg-info-format-version version) version)))
  version)

(defun pkg-info--read-library ()
  "Read a library from minibuffer."
  (completing-read "Load library: "
                   (apply-partially 'locate-file-completion-table
                                    load-path
                                    (get-load-suffixes))))

(defun pkg-info--read-function ()
  "Read a function name from minibuffer."
  (let ((input (completing-read "Function: " obarray #'boundp :require-match)))
    (if (string= input "") nil (intern input))))

(defun pkg-info--read-package ()
  "Read a package name from minibuffer."
  (let* ((installed (epl-installed-packages))
         (names (sort (mapcar (lambda (pkg)
                                (symbol-name (epl-package-name pkg)))
                              installed)
                      #'string<))
         (default (car names)))
    (completing-read "Installed package: " names nil 'require-match
                     nil nil default)))

(defun pkg-info-library-source (library)
  "Get the source file of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

Return the source file of LIBRARY as string."
  (find-library-name (if (symbolp library) (symbol-name library) library)))

(defun pkg-info-defining-library (function)
  "Get the source file of the library defining FUNCTION.

FUNCTION is a function symbol.

Return the file name of the library as string.  Signal an error
if the library does not exist, or if the definition of FUNCTION
was not found."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (let ((library (symbol-file function 'defun)))
    (unless library
      (error "Can't find definition of %s" function))
    library))

(defun pkg-info-x-original-version (file)
  "Read the X-Original-Version header from FILE.

Return the value as version list, or return nil if FILE lacks
this header.  Signal an error, if the value of the header is not
a valid version."
  (let ((version-str (with-temp-buffer
                       (insert-file-contents file)
                       (lm-header "X-Original-Version"))))
    (when version-str
      (version-to-list version-str))))

;;;###autoload
(defun pkg-info-library-original-version (library &optional show)
  "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers."
  (interactive (list (pkg-info--read-library) t))
  (let ((version (pkg-info-x-original-version
                  (pkg-info-library-source library))))
    (if version
        (pkg-info--show-version-and-return version show)
      (error "Library %s has no original version" library))))

;;;###autoload
(defun pkg-info-library-version (library &optional show)
  "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers."
  (interactive (list (pkg-info--read-library) t))
  (let* ((source (pkg-info-library-source library))
         (version (epl-package-version (epl-package-from-file source))))
    (pkg-info--show-version-and-return version show)))

;;;###autoload
(defun pkg-info-defining-library-original-version (function &optional show)
  "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header."
  (interactive (list (pkg-info--read-function) t))
  (pkg-info-library-original-version (pkg-info-defining-library function) show))

;;;###autoload
(defun pkg-info-defining-library-version (function &optional show)
  "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header."
  (interactive (list (pkg-info--read-function) t))
  (pkg-info-library-version (pkg-info-defining-library function) show))

;;;###autoload
(defun pkg-info-package-version (package &optional show)
  "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed."
  (interactive (list (pkg-info--read-package) t))
  (let* ((name (if (stringp package) (intern package) package))
         (package (car (epl-find-installed-packages name))))
    (unless package
      (error "Can't find installed package %s" name))
    (pkg-info--show-version-and-return (epl-package-version package) show)))

;;;###autoload
(defun pkg-info-version-info (library &optional package show)
  "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version."
  (interactive (list (pkg-info--read-library)
                     (when current-prefix-arg
                       (pkg-info--read-package))
                     t))
  (let* ((package (or package (if (stringp library) (intern library) library)))
         (orig-version (condition-case nil
                           (pkg-info-library-original-version library)
                         (error nil)))
         ;; If we have X-Original-Version, we assume that MELPA replaced the
         ;; library version with its generated version, so we use the
         ;; X-Original-Version header instead, and ignore the library version
         ;; header
         (lib-version (or orig-version (pkg-info-library-version library)))
         (pkg-version (condition-case nil
                          (pkg-info-package-version package)
                        (error nil)))
         (version (if (and pkg-version
                           (not (version-list-= lib-version pkg-version)))
                      (format "%s (package: %s)"
                              (pkg-info-format-version lib-version)
                              (pkg-info-format-version pkg-version))
                    (pkg-info-format-version lib-version))))
    (pkg-info--show-version-and-return version show)))

(defconst pkg-info-melpa-recipe-url "http://melpa.org/recipes.json"
  "The URL from which to fetch MELPA recipes.")

(defvar pkg-info-melpa-recipes nil
  "An alist of MELPA recipes.")

(defun pkg-info-retrieve-melpa-recipes ()
  "Retrieve MELPA recipes from MELPA archive."
  (let ((buffer (url-retrieve-synchronously pkg-info-melpa-recipe-url)))
    (with-current-buffer buffer
      (unwind-protect
          (let ((response-code (url-http-parse-response)))
            (unless (equal response-code 200)
              (error "Failed to retrieve MELPA recipes from %s (code %s)"
                     pkg-info-melpa-recipe-url response-code))
            (goto-char url-http-end-of-headers)
            (json-read))
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))))))

(defun pkg-info-get-melpa-recipes ()
  "Get MELPA recipes."
  (setq pkg-info-melpa-recipes
        (or pkg-info-melpa-recipes
            (pkg-info-retrieve-melpa-recipes))))

(defun pkg-info-get-melpa-recipe (package)
  "Get the MELPA recipe for PACKAGE.

Return nil if PACKAGE is not on MELPA."
  (cdr (assq package (pkg-info-get-melpa-recipes))))

(defun pkg-info-get-melpa-fetcher (package)
  "Get the MELPA fetcher for PACKAGE."
  (cdr (assq 'fetcher (pkg-info-get-melpa-recipe package))))

(defun pkg-info-wiki-package-p (package)
  "Determine whether PACKAGE is build from the EmacsWiki."
  (equal (pkg-info-get-melpa-fetcher package) "wiki"))

(provide 'pkg-info)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; pkg-info.el ends here
