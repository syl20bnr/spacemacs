;;; compat-macs.el --- Compatibility Macros -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

;; WARNING: This file provides *internal* macros.  The macros are used
;; by Compat to facilitate the definition of compatibility functions,
;; compatibility macros and compatibility variables.  The
;; `compat-macs' feature should never be loaded at runtime in your
;; Emacs and will only be used during byte compilation.  Every
;; definition provided here is internal, may change any time between
;; Compat releases and must not be used by other packages.

;;; Code:

;; We always require subr-x at compile time for the fboundp check
;; since definitions have been moved around. The cl-lib macros are
;; needed by compatibility definitions.
(require 'subr-x)
(require 'cl-lib)

(defvar compat-macs--version nil
  "Version of the currently defined compatibility definitions.")

(defun compat-macs--strict (cond &rest error)
  "Assert strict COND, otherwise fail with ERROR."
  (when (bound-and-true-p compat-strict)
    (apply #'compat-macs--assert cond error)))

(defun compat-macs--assert (cond &rest error)
  "Assert COND, otherwise fail with ERROR."
  (unless cond (apply #'error error)))

(defun compat-macs--docstring (type name docstring)
  "Format DOCSTRING for NAME of TYPE.
Prepend compatibility notice to the actual documentation string."
  (with-temp-buffer
    (insert
     (format
      "[Compatibility %s for `%s', defined in Emacs %s. \
See (compat) Emacs %s' for more details.]\n\n%s"
      type name compat-macs--version compat-macs--version docstring))
    (let ((fill-column 80))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun compat-macs--check-attributes (attrs preds)
  "Check ATTRS given PREDS predicate plist and return rest."
  (while (keywordp (car attrs))
    (compat-macs--assert (cdr attrs) "Attribute list length is odd")
    (compat-macs--assert (let ((p (plist-get preds (car attrs))))
                           (and p (or (eq p t) (funcall p (cadr attrs)))))
                         "Invalid attribute %s" (car attrs))
    (setq attrs (cddr attrs)))
  attrs)

(defun compat-macs--guard (attrs preds fun)
  "Guard compatibility definition generation.
The version constraints specified by ATTRS are checked.  PREDS is
a plist of predicates for arguments which are passed to FUN."
  (declare (indent 2))
  (compat-macs--assert compat-macs--version "No `compat-version' was declared")
  (let* ((body (compat-macs--check-attributes
                attrs `(,@preds :feature symbolp)))
         (feature (plist-get attrs :feature))
         (attrs `(:body ,body ,@attrs))
         args)
    ;; Require feature at compile time
    (when feature
      (compat-macs--assert (not (eq feature 'subr-x)) "Invalid feature subr-x")
      (require feature))
    ;; The current Emacs must be older than the currently declared version.
    (when (version< emacs-version compat-macs--version)
      (while preds
        (push (plist-get attrs (car preds)) args)
        (setq preds (cddr preds)))
      (setq body (apply fun (nreverse args)))
      (if (and feature body)
          `(with-eval-after-load ',feature ,@body)
        (macroexp-progn body)))))

(defun compat-macs--defun (type name arglist docstring rest)
  "Define function NAME of TYPE with ARGLIST and DOCSTRING.
REST are attributes and the function BODY."
  (compat-macs--guard
     rest (list :extended (lambda (x) (or (booleanp x) (version-to-list x)))
                :obsolete (lambda (x) (or (booleanp x) (stringp x)))
                :body t)
    (lambda (extended obsolete body)
      (when (stringp extended)
        (compat-macs--assert
         (and (version< extended compat-macs--version) (version< "24.4" extended))
         "Invalid :extended version %s for %s %s" extended type name)
        (setq extended (version<= extended emacs-version)))
      (compat-macs--strict (eq extended (fboundp name))
                           "Wrong :extended flag for %s %s" type name)
      ;; Remove unsupported declares.  It might be possible to set these
      ;; properties otherwise.  That should be looked into and implemented
      ;; if it is the case.
      (when (and (listp (car-safe body)) (eq (caar body) 'declare) (<= emacs-major-version 25))
        (setcar body (assq-delete-all 'pure (assq-delete-all
                                             'side-effect-free (car body)))))
      ;; Use `:extended' name if the function is already defined.
      (let* ((defname (if (and extended (fboundp name))
                          (intern (format "compat--%s" name))
                        name))
             (def `(,(if (memq '&key arglist)
                         (if (eq type 'macro) 'cl-defmacro 'cl-defun)
                       (if (eq type 'macro) 'defmacro 'defun))
                    ,defname ,arglist
                    ,(compat-macs--docstring type name docstring)
                    ,@body)))
        `(,@(if (eq defname name)
                ;; An additional fboundp check is performed at runtime to make
                ;; sure that we never redefine an existing definition if Compat
                ;; is loaded on a newer Emacs version.  Declare the function,
                ;; such that the byte compiler does not complain about possibly
                ;; missing functions at runtime. The warnings are generated due
                ;; to the fboundp check.
                `((declare-function ,name nil)
                  (unless (fboundp ',name) ,def))
              (list def))
          ,@(when obsolete
              `((make-obsolete
                 ',defname ,(if (stringp obsolete) obsolete "No substitute")
                 ,compat-macs--version))))))))

(defmacro compat-guard (cond &rest rest)
  "Guard definition with a runtime COND and a version check.
The runtime condition must make sure that no definition is
overriden.  REST is an attribute plist followed by the definition
body.  The attributes specify the conditions under which the
definition is generated.

- :feature :: Wrap the definition with `with-eval-after-load' for
  the given feature."
  (declare (debug ([&rest keywordp sexp] def-body))
           (indent 1))
  (compat-macs--guard rest '(:body t)
    (lambda (body)
      (compat-macs--assert body "The guarded body is empty")
      (if (eq cond t)
          body
        (compat-macs--strict (eval cond t) "Guard %S failed" cond)
        `((when ,cond ,@body))))))

(defmacro compat-defalias (name def &rest attrs)
  "Define compatibility alias NAME as DEF.
ATTRS is a plist of attributes, which specify the conditions
under which the definition is generated.

- :obsolete :: Mark the alias as obsolete if t.

- :feature :: See `compat-guard'."
  (declare (debug (name symbolp [&rest keywordp sexp])))
  (compat-macs--guard attrs '(:obsolete booleanp)
    (lambda (obsolete)
      (compat-macs--strict (not (fboundp name)) "%s already defined" name)
      ;; The fboundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (fboundp ',name)
          (defalias ',name ',def
            ,(compat-macs--docstring 'function name
                                (get name 'function-documentation)))
          ,@(when obsolete
              `((make-obsolete ',name ',def ,compat-macs--version))))))))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define compatibility function NAME with arguments ARGLIST.
The function must be documented in DOCSTRING.  REST is an
attribute plist followed by the function body.  The attributes
specify the conditions under which the definition is generated.

- :extended :: Mark the function as extended if t.  The function
  must be called explicitly via `compat-call'.  This attribute
  should be used for functions which extend already existing
  functions, e.g., functions which changed their calling
  convention or their behavior.  The value can also be a version
  string, which specifies the Emacs version when the original
  version of the function was introduced.

- :obsolete :: Mark the function as obsolete if t, can be a
  string describing the obsoletion.

- :feature :: See `compat-guard'."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat-macs--defun 'function name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define compatibility macro NAME with arguments ARGLIST.
The macro must be documented in DOCSTRING.  REST is an attribute
plist followed by the macro body.  See `compat-defun' for
details."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat-macs--defun 'macro name arglist docstring rest))

(defmacro compat-defvar (name initval docstring &rest attrs)
  "Define compatibility variable NAME with initial value INITVAL.
The variable must be documented in DOCSTRING.  ATTRS is a plist
of attributes, which specify the conditions under which the
definition is generated.

- :constant :: Mark the variable as constant if t.

- :local :: Make the variable buffer-local if t.  If the value is
  `permanent' make the variable additionally permanently local.

- :obsolete :: Mark the variable as obsolete if t, can be a
  string describing the obsoletion.

- :feature :: See `compat-guard'."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (compat-macs--guard
      attrs (list :constant #'booleanp
                  :local (lambda (x) (memq x '(nil t permanent)))
                  :obsolete (lambda (x) (or (booleanp x) (stringp x))))
    (lambda (constant local obsolete)
      (compat-macs--strict (not (boundp name)) "%s already defined" name)
      (compat-macs--assert (not (and constant local)) "Both :constant and :local")
      ;; The boundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (boundp ',name)
          (,(if constant 'defconst 'defvar)
           ,name ,initval
           ,(compat-macs--docstring 'variable name docstring))
          ,@(when obsolete
              `((make-obsolete-variable
                 ',name ,(if (stringp obsolete) obsolete "No substitute")
                 ,compat-macs--version))))
        ,@(and local `((make-variable-buffer-local ',name)))
        ,@(and (eq local 'permanent) `((put ',name 'permanent-local t)))))))

(defmacro compat-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  (setq compat-macs--version version)
  nil)

(defmacro compat-require (feature version)
  "Require FEATURE if the Emacs version is less than VERSION."
  (when (version< emacs-version version)
    (require feature)
    `(require ',feature)))

(provide 'compat-macs)
;;; compat-macs.el ends here
