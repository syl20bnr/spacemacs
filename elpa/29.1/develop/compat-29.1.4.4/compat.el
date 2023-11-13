;;; compat.el --- Emacs Lisp Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Compat Development <~pkal/compat-devel@lists.sr.ht>
;; Version: 29.1.4.4
;; URL: https://github.com/emacs-compat/compat
;; Package-Requires: ((emacs "24.4") (seq "2.23"))
;; Keywords: lisp, maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Compat is the Elisp forwards compatibility library, which provides
;; definitions introduced in newer Emacs versions.  The definitions
;; are only installed if necessary for your current Emacs version.  If
;; Compat is compiled on a recent version of Emacs, all of the
;; definitions are disabled at compile time, such that no negative
;; performance impact is incurred.  The provided compatibility
;; implementations of functions and macros are at least subsets of the
;; actual implementations.  Be sure to read the documentation string
;; and the Compat manual.
;;
;; Not every function provided in newer versions of Emacs is provided
;; here.  Some depend on new features from the C core, others cannot
;; be implemented to a meaningful degree.  Please consult the Compat
;; manual for details regarding the usage of the Compat library and
;; the provided functionality.

;; The main audience for this library are not regular users, but
;; package maintainers.  Therefore no commands, user-facing modes or
;; user options are implemented here.

;;; Code:

;; Ensure that the newest compatibility layer is required at compile
;; time and runtime, but only if needed.
(eval-when-compile
  (defmacro compat--maybe-require-29 ()
    (when (version< emacs-version "29.1")
      (require 'compat-29)
      '(require 'compat-29))))
(compat--maybe-require-29)

;;;; Macros for extended compatibility function calls

(defmacro compat-function (fun)
  "Return compatibility function symbol for FUN.

If the Emacs version provides a sufficiently recent version of
FUN, the symbol FUN is returned itself.  Otherwise the macro
returns the symbol of a compatibility function which supports the
behavior and calling convention of the current stable Emacs
version.  For example Compat 29.1 will provide compatibility
functions which implement the behavior and calling convention of
Emacs 29.1.

See also `compat-call' to directly call compatibility functions."
  (let ((compat (intern (format "compat--%s" fun))))
    `#',(if (fboundp compat) compat fun)))

(defmacro compat-call (fun &rest args)
  "Call compatibility function or macro FUN with ARGS.

A good example function is `plist-get' which was extended with an
additional predicate argument in Emacs 29.1.  The compatibility
function, which supports this additional argument, can be
obtained via (compat-function plist-get) and called
via (compat-call plist-get plist prop predicate).  It is not
possible to directly call (plist-get plist prop predicate) on
Emacs older than 29.1, since the original `plist-get' function
does not yet support the predicate argument.  Note that the
Compat library never overrides existing functions.

See also `compat-function' to lookup compatibility functions."
  (let ((compat (intern (format "compat--%s" fun))))
    `(,(if (fboundp compat) compat fun) ,@args)))

(provide 'compat)
;;; compat.el ends here
