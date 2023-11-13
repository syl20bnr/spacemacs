;;; sly-cl-indent.el --- enhanced lisp-indent mode  -*- lexical-binding: t; -*-

;; Copyright (C) 1987, 2000-2011 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Created: July 1987
;; Maintainer: FSF
;; Keywords: lisp, tools
;; Package: emacs

;; This file is forked from cl-indent.el, which is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies a single entry point, `sly-common-lisp-indent-function',
;; which performs indentation in the preferred style for Common Lisp code.
;; To enable it:
;;
;; (setq lisp-indent-function 'sly-common-lisp-indent-function)
;;
;; This file is substantially patched from original cl-indent.el,
;; which is in Emacs proper. Although it is named after the SLY
;; library, it DOES NOT require it.  sly-cl-indent is instead required
;; by one of SLY's contribs, `sly-indentation'.
;;
;; Before making modifications to this file, consider adding them to
;; Emacs's own `cl-indent' and refactoring this file to be an
;; extension of Emacs's.
;;
;;; Code:
(require 'cl-lib)

(defgroup sly-lisp-indent nil
  "Indentation in Common Lisp."
  :group 'sly
  :group 'lisp-indent)

(defcustom sly-lisp-indent-maximum-backtracking 6
  "Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is 0, no backtracking will occur and forms such as `flet'
may not be correctly indented if this value is less than 4."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-tag-indentation 1
  "Indentation of tags relative to containing list.
This variable is used by the function `sly--lisp-indent-tagbody'."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-tag-body-indentation 3
  "Indentation of non-tagged lines relative to containing list.
This variable is used by the function `sly--lisp-indent-tagbody' to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by `lisp-body-indent'."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-backquote-indentation t
  "Whether or not to indent backquoted lists as code.
If nil, indent backquoted lists as data, i.e., like quoted lists."
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-loop-indent-subclauses t
  "Whether or not to indent loop subclauses."
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-simple-loop-indentation 2
  "Indentation of forms in simple loop forms."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-loop-clauses-indentation 2
  "Indentation of loop clauses if `loop' is immediately followed by a newline."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-loop-indent-body-forms-relative-to-loop-start nil
  "When true, indent loop body clauses relative to the open paren of the loop
form, instead of the keyword position."
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-loop-body-forms-indentation 3
  "Indentation of loop body clauses."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-loop-indent-forms-like-keywords nil
  "Whether or not to indent loop subforms just like
loop keywords. Only matters when `sly-lisp-loop-indent-subclauses'
is nil."
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-align-keywords-in-calls t
  "Whether to align keyword arguments vertically or not.
If t (the default), keywords in contexts where no other
indentation rule takes precedence are aligned like this:

\(make-instance 'foo :bar t
                    :quux 42)

If nil, they are indented like any other function
call arguments:

\(make-instance 'foo :bar t
               :quux 42)"
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-lambda-list-indentation t
  "Whether to indent lambda-lists specially. Defaults to t. Setting this to
nil makes `sly-lisp-lambda-list-keyword-alignment',
`sly-lisp-lambda-list-keyword-parameter-alignment', and
`sly-lisp-lambda-list-keyword-parameter-indentation' meaningless, causing
lambda-lists to be indented as if they were data:

\(defun example (a b &optional o1 o2
                o3 o4
                &rest r
                &key k1 k2
                k3 k4)
  #|...|#)"
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-lambda-list-keyword-alignment nil
  "Whether to vertically align lambda-list keywords together.
If nil (the default), keyworded lambda-list parts are aligned
with the initial mandatory arguments, like this:

\(defun foo (arg1 arg2 &rest rest
            &key key1 key2)
  #|...|#)

If non-nil, alignment is done with the first keyword
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &rest rest
                      &key key1 key2)
  #|...|#)"
  :type 'boolean
  :group 'sly-lisp-indent)

(defcustom sly-lisp-lambda-list-keyword-parameter-indentation 2
  "Indentation of lambda list keyword parameters.
See `sly-lisp-lambda-list-keyword-parameter-alignment'
for more information."
  :type 'integer
  :group 'sly-lisp-indent)

(defcustom sly-lisp-lambda-list-keyword-parameter-alignment nil
  "Whether to vertically align lambda-list keyword parameters together.
If nil (the default), the parameters are aligned
with their corresponding keyword, plus the value of
`sly-lisp-lambda-list-keyword-parameter-indentation', like this:

\(defun foo (arg1 arg2 &key key1 key2
                        key3 key4)
  #|...|#)

If non-nil, alignment is done with the first parameter
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &key key1 key2
                           key3 key4)
  #|...|#)"
  :type 'boolean
  :group 'sly-lisp-indent)


;; should this be a defcustom?
(defvar sly-lisp-indent-defun-method '(4 &lambda &body)
  "Defun-like indentation method.
This applies when the value of the `sly-common-lisp-indent-function' property
is set to `defun'.")


;;;; Named styles.
;;;;
;;;; -*- common-lisp-style: foo -*-
;;;;
;;;; sets the style for the buffer.
;;;;
;;;; A Common Lisp style is a list of the form:
;;;;
;;;;  (NAME INHERIT VARIABLES INDENTATION HOOK DOCSTRING)
;;;;
;;;; where NAME is a symbol naming the style, INHERIT is the name of the style
;;;; it inherits from, VARIABLES is an alist specifying buffer local variables
;;;; for the style, and INDENTATION is an alist specifying non-standard
;;;; indentations for Common Lisp symbols. HOOK is a function to call when
;;;; activating the style. DOCSTRING is the documentation for the style.
;;;;
;;;; Convenience accessors `sly--common-lisp-style-name', &co exist.
;;;;
;;;; `sly-common-lisp-style' stores the name of the current style.
;;;;
;;;; `sly-common-lisp-style-default' stores the name of the style to use when none
;;;; has been specified.
;;;;
;;;; `sly--lisp-indent-active-style' stores a cons of the list specifying the
;;;; current style, and a hash-table containing all indentation methods of that
;;;; style and any styles it inherits from. Whenever we're indenting, we check
;;;; that this is up to date, and recompute when necessary.
;;;;
;;;; Just setting the buffer local sly-common-lisp-style will be enough to have
;;;; the style take effect. `sly-common-lisp-set-style' can also be called
;;;; explicitly, however, and offers name completion, etc.

(cl-defstruct (sly--common-lisp-style
                (:type list)
                (:copier nil)
                (:predicate nil)
                (:constructor nil)
                (:constructor sly--common-lisp-make-style
                    (name inherits variables
                     indentation hook docstring)))
  name inherits variables indentation hook docstring)

;;; Convenience accessors
(defalias 'sly--lisp-indent-parse-state-start #'cl-second)
(defalias 'sly--lisp-indent-parse-state-prev  #'cl-third)

(defvar-local sly-common-lisp-style nil)

;;; `sly-define-common-lisp-style' updates the docstring of
;;; `sly-common-lisp-style', using this as the base.
(put 'sly-common-lisp-style 'sly-common-lisp-style-base-doc
     "Name of the Common Lisp indentation style used in the current buffer.
Set this by giving eg.

  ;; -*- common-lisp-style: sbcl -*-

in the first line of the file, or by calling `sly-common-lisp-set-style'. If
buffer has no style specified, but `sly-common-lisp-style-default' is set, that
style is used instead. Use `sly-define-common-lisp-style' to define new styles.")

;;; `lisp-mode' kills all buffer-local variables. Setting the
;;; `permanent-local' property allows us to retain the style.
(put 'sly-common-lisp-style 'permanent-local t)

;;; Mark as safe when the style doesn't evaluate arbitrary code.
(put 'sly-common-lisp-style 'safe-local-variable 'sly--lisp-indent-safe-style-p)

;;; Common Lisp indentation style specifications.
(defvar sly--common-lisp-styles (make-hash-table :test 'equal))

;; unused
(defsubst sly--lisp-indent-delete-style (stylename)
  (remhash stylename sly--common-lisp-styles))

(defun sly--lisp-indent-find-style (stylename)
  (let ((name (if (symbolp stylename)
                  (symbol-name stylename)
                stylename)))
    (or (gethash name sly--common-lisp-styles)
        (error "Unknown Common Lisp style: %s" name))))

(defun sly--lisp-indent-safe-style-p (stylename)
  "True for known Common Lisp style without an :EVAL option.
Ie. styles that will not evaluate arbitrary code on activation."
  (let* ((style (ignore-errors (sly--lisp-indent-find-style stylename)))
         (base (sly--common-lisp-style-inherits style)))
    (and style
         (not (sly--common-lisp-style-hook style))
         (or (not base)
             (sly--lisp-indent-safe-style-p base)))))

(defun sly--lisp-indent-add-style (stylename inherits variables
                                   indentation hooks documentation)
  ;; Invalidate indentation methods cached in common-lisp-active-style.
  (maphash (lambda (k v)
             (puthash k (cl-copy-list v) sly--common-lisp-styles))
           sly--common-lisp-styles)
  ;; Add/Redefine the specified style.
  (puthash stylename
           (sly--common-lisp-make-style
            stylename inherits
            variables indentation
            hooks documentation)
           sly--common-lisp-styles)
  ;; Frob `sly-common-lisp-style' docstring.
  (let ((doc (get 'sly-common-lisp-style
                  'sly-common-lisp-style-base-doc))
        (all nil))
    (setq doc (concat doc "\n\nAvailable styles are:\n"))
    (maphash (lambda (name style)
               (push (list name (sly--common-lisp-style-docstring style)) all))
             sly--common-lisp-styles)
    (dolist (info (sort all (lambda (a b) (string< (car a) (car b)))))
      (let ((style-name (cl-first info))
            (style-doc  (cl-second info)))
        (if style-doc
            (setq doc (concat doc
                              "\n " style-name "\n"
                              "   " style-doc "\n"))
          (setq doc (concat doc "\n " style-name " (undocumented)\n")))))
    (put 'sly-common-lisp-style 'variable-documentation doc))
  stylename)

;;; Activate STYLENAME, adding its indentation methods to METHODS -- and
;;; recurse on style inherited from.
(defun sly--lisp-indent-activate-style (stylename methods)
  (let* ((style (sly--lisp-indent-find-style stylename))
         (basename (sly--common-lisp-style-inherits style)))
    ;; Recurse on parent.
    (when basename
      (sly--lisp-indent-activate-style basename methods))
    ;; Copy methods
    (dolist (spec (sly--common-lisp-style-indentation style))
      (puthash (cl-first spec) (cl-second spec) methods))
    ;; Bind variables.
    (dolist (var (sly--common-lisp-style-variables style))
      (set (make-local-variable (cl-first var)) (cl-second var)))
    ;; Run hook.
    (let ((hook (sly--common-lisp-style-hook style)))
      (when hook
        (funcall hook)))))

;;; When a style is being used, `sly--lisp-indent-active-style' holds a cons
;;;
;;;   (STYLE . METHODS)
;;;
;;; where STYLE is the list specifying the currently active style, and
;;; METHODS is the table of indentation methods --  including inherited
;;; ones -- for it. `sly--lisp-indent-active-style-methods' is reponsible
;;; for keeping this up to date.
(defvar-local sly--lisp-indent-active-style nil)

;;; Makes sure sly--lisp-indent-active-style corresponds to sly-common-lisp-style, and
;;; pick up redefinitions, etc. Returns the method table for the currently
;;; active style.
(defun sly--lisp-indent-active-style-methods ()
  (let* ((name (or sly-common-lisp-style (bound-and-true-p common-lisp-style)))
         (style (when name (sly--lisp-indent-find-style name))))
    (if (eq style (car sly--lisp-indent-active-style))
        (cdr sly--lisp-indent-active-style)
      (when style
        (let ((methods (make-hash-table :test 'equal)))
          (sly--lisp-indent-activate-style name methods)
          (setq sly--lisp-indent-active-style (cons style methods))
          methods)))))

(defvar sly--lisp-indent-set-style-history nil)

(defun sly--lisp-indent-style-names ()
  (let (names)
    (maphash (lambda (k v)
               (push (cons k v) names))
             sly--common-lisp-styles)
    names))

;;;###autoload
(defun sly-common-lisp-set-style (stylename)
  "Set current buffer to use the Common Lisp style STYLENAME.
STYLENAME, a string, must be an existing Common Lisp style. Styles
are added (and updated) using `sly-define-common-lisp-style'.

The buffer-local variable `sly-common-lisp-style' will get set to STYLENAME.

A Common Lisp style is composed of local variables, indentation
specifications, and may also contain arbitrary elisp code to run upon
activation."
  (interactive
   (list (let ((completion-ignore-case t)
               (prompt "Specify Common Lisp indentation style: "))
           (completing-read prompt
                            (sly--lisp-indent-style-names) nil t nil
                            'sly--lisp-indent-set-style-history))))
  (setq sly-common-lisp-style (sly--common-lisp-style-name
                               (sly--lisp-indent-find-style stylename))
        sly--lisp-indent-active-style nil)
  ;; Actually activates the style.
  (sly--lisp-indent-active-style-methods)
  stylename)

;;;###autoload
(defmacro sly-define-common-lisp-style (name documentation &rest options)
  "Define a Common Lisp indentation style.

NAME is the name of the style.

DOCUMENTATION is the docstring for the style, automatically added to the
docstring of `sly-common-lisp-style'.

OPTIONS are:

 (:variables (name value) ...)

  Specifying the buffer local variables associated with the style.

 (:indentation (symbol spec) ...)

  Specifying custom indentations associated with the style. SPEC is
  a normal `sly-common-lisp-indent-function' indentation specification.

 (:inherit style)

  Inherit variables and indentations from another Common Lisp style.

 (:eval form ...)

  Lisp code to evaluate when activating the style. This can be used to
  eg. activate other modes. It is possible that over the lifetime of
  a buffer same style gets activated multiple times, so code in :eval
  option should cope with that.
"
  (declare (indent 1))
  (when (consp documentation)
    (setq options (cons documentation options)
          documentation nil))
  `(sly--lisp-indent-add-style ,name
                              ,(cadr (assoc :inherit options))
                              ',(cdr (assoc :variables options))
                              ',(cdr (assoc :indentation options))
                              ,(when (assoc :eval options)
                                 `(lambda ()
                                    ,@(cdr (assoc :eval options))))
                              ,documentation))

(sly-define-common-lisp-style "basic-common"
  (:variables
   (sly-lisp-indent-maximum-backtracking 6)
   (sly-lisp-tag-indentation 1)
   (sly-lisp-tag-body-indentation 3)
   (sly-lisp-backquote-indentation t)
   (sly-lisp-loop-indent-subclauses t)
   (sly-lisp-loop-indent-forms-like-keywords nil)
   (sly-lisp-simple-loop-indentation 2)
   (sly-lisp-align-keywords-in-calls t)
   (sly-lisp-lambda-list-indentation t)
   (sly-lisp-lambda-list-keyword-alignment nil)
   (sly-lisp-lambda-list-keyword-parameter-indentation 2)
   (sly-lisp-lambda-list-keyword-parameter-alignment nil)
   (sly-lisp-indent-defun-method (4 &lambda &body))
   (sly-lisp-loop-clauses-indentation 2)
   (sly-lisp-loop-indent-body-forms-relative-to-loop-start nil)
   (sly-lisp-loop-body-forms-indentation 3)))

(sly-define-common-lisp-style "basic-emacs25"
  "This style adds a workaround needed for Emacs 25"
  (:inherit "basic-common")
  (:variables
   ;; Without these (;;foo would get a space inserted between
   ;; ( and ; by indent-sexp.
   (comment-indent-function (lambda () nil))))

(sly-define-common-lisp-style "basic-emacs26"
  "This style is the same as basic-common. It doesn't need or
   want the workaround used in Emacs 25. In Emacs 26, that
   workaround introduces a weird behavior where a single
   semicolon breaks the mode and causes the cursor to move to the
   start of the line after every character inserted."
  (:inherit "basic-common"))

(sly-define-common-lisp-style "basic"
  "This style merely gives all identation variables their default values,
   making it easy to create new styles that are proof against user
   customizations. It also adjusts comment indentation from default.
   All other predefined modes inherit from basic."
  (:inherit (if (>= emacs-major-version 26)
                "basic-emacs26"
              "basic-emacs25")))

(sly-define-common-lisp-style "classic"
  "This style of indentation emulates the most striking features of 1995
   vintage cl-indent.el once included as part of Slime: IF indented by two
   spaces, and CASE clause bodies indentented more deeply than the keys."
  (:inherit "basic")
  (:variables
   (sly-lisp-lambda-list-keyword-parameter-indentation 0))
  (:indentation
   (case (4 &rest (&whole 2 &rest 3)))
   (if   (4 2 2))))

(sly-define-common-lisp-style "modern"
  "A good general purpose style. Turns on lambda-list keyword and keyword
   parameter alignment, and turns subclause aware loop indentation off.
   (Loop indentation so because simpler style is more prevalent in existing
   sources, not because it is necessarily preferred.)"
  (:inherit "basic")
  (:variables
   (sly-lisp-lambda-list-keyword-alignment t)
   (sly-lisp-lambda-list-keyword-parameter-alignment t)
   (sly-lisp-lambda-list-keyword-parameter-indentation 0)
   (sly-lisp-loop-indent-subclauses nil)))

(sly-define-common-lisp-style "sbcl"
  "Style used in SBCL sources. A good if somewhat intrusive general purpose
   style based on the \"modern\" style. Adds indentation for a few SBCL
   specific constructs, sets indentation to use spaces instead of tabs,
   fill-column to 78, and activates whitespace-mode to show tabs and trailing
   whitespace."
  (:inherit "modern")
  (:eval
   (whitespace-mode 1))
  (:variables
   (whitespace-style (tabs trailing))
   (indent-tabs-mode nil)
   (comment-fill-column nil)
   (fill-column 78))
  (:indentation
   (def!constant            (as defconstant))
   (def!macro               (as defmacro))
   (def!method              (as defmethod))
   (def!struct              (as defstruct))
   (def!type                (as deftype))
   (defmacro-mundanely      (as defmacro))
   (deftransform            (as defmacro))
   (define-source-transform (as defun))
   (!def-type-translator    (as defun))
   (!def-debug-command      (as defun))))

(defcustom sly-common-lisp-style-default nil
  "Name of the Common Lisp indentation style to use in lisp-mode buffers if
none has been specified."
  :type `(choice (const :tag "None" nil)
                 ,@(mapcar (lambda (spec)
                             `(const :tag ,(car spec) ,(car spec)))
                           (sly--lisp-indent-style-names))
                 (string :tag "Other"))
  :group 'sly-lisp-indent)

;;; If style is being used, that's a sufficient invitation to snag
;;; the indentation function.
(defun sly--lisp-indent-lisp-mode-hook ()
  (let ((style (or sly-common-lisp-style
                   (bound-and-true-p common-lisp-style)
                   sly-common-lisp-style-default)))
    (when style
      (setq-local lisp-indent-function #'sly-common-lisp-indent-function)
      (sly-common-lisp-set-style style))))
(add-hook 'lisp-mode-hook #'sly--lisp-indent-lisp-mode-hook)


;;;; The indentation specs are stored at three levels. In order of priority:
;;;;
;;;; 1. Indentation as set by current style, from the indentation table
;;;;    in the current style.
;;;;
;;;; 2. Globally set indentation, from the `sly-common-lisp-indent-function'
;;;;    property of the symbol.
;;;;
;;;; 3. Per-package indentation derived by the system. A live Common Lisp
;;;;    system may (via Slime, eg.) add indentation specs to
;;;;    sly-common-lisp-system-indentation, where they are associated with
;;;;    the package of the symbol. Then we run some lossy heuristics and
;;;;    find something that looks promising.
;;;;
;;;;    FIXME: for non-system packages the derived indentation should probably
;;;;    take precedence.

;;; This maps symbols into lists of (INDENT . PACKAGES) where INDENT is
;;; an indentation spec, and PACKAGES are the names of packages where this
;;; applies.
;;;
;;; We never add stuff here by ourselves: this is for things like Slime to
;;; fill.
(defvar sly-common-lisp-system-indentation (make-hash-table :test 'equal))

(defun sly--lisp-indent-guess-current-package ()
  (save-excursion
    (ignore-errors
      (when (let ((case-fold-search t))
              (search-backward "(in-package "))
        (re-search-forward "[ :\"]+")
        (let ((start (point)))
          (re-search-forward "[\":)]")
          (upcase (buffer-substring-no-properties
                   start (1- (point)))))))))

(defvar sly--lisp-indent-current-package-function
  'sly--lisp-indent-guess-current-package
  "Used to derive the package name to use for indentation at a
given point. Defaults to `sly--lisp-indent-guess-current-package'.")

(defun sly--lisp-indent-symbol-package (string)
  (if (and (stringp string) (string-match ":" string))
      (let ((p (match-beginning 0)))
        (if (eq p 0)
            "KEYWORD"
          (upcase (substring string 0 p))))
    (funcall sly--lisp-indent-current-package-function)))

(defun sly--lisp-indent-get-indentation (name &optional full)
  "Retrieves the indentation information for NAME."
  (let ((method
         (or
          ;; From style
          (let ((methods (sly--lisp-indent-active-style-methods)))
            (and methods (gethash name methods)))
          ;; From global settings.
          (get name 'sly-common-lisp-indent-function)
          (get name 'common-lisp-indent-function)
          ;; From system derived information.
          (let ((system-info (gethash name sly-common-lisp-system-indentation)))
            (if (not (cdr system-info))
                (caar system-info)
              (let ((guess nil)
                    (guess-n 0)
                    (package (sly--lisp-indent-symbol-package full)))
                (cl-dolist (info system-info guess)
                  (let* ((pkgs (cdr info))
                         (n (length pkgs)))
                    (cond ((member package pkgs)
                           ;; This is it.
                           (cl-return (car info)))
                          ((> n guess-n)
                           ;; If we can't find the real thing, go with the one
                           ;; accessible in most packages.
                           (setf guess (car info)
                                 guess-n n)))))))))))
    (if (eq 'as (car-safe method))
        (sly--lisp-indent-get-indentation (cadr method))
      method)))

;;;; LOOP indentation, the simple version

(defun sly--lisp-indent-loop-type (loop-start)
  "Returns the type of the loop form at LOOP-START.
Possible types are SIMPLE, SIMPLE/SPLIT, EXTENDED, and EXTENDED/SPLIT. */SPLIT
refers to extended loops whose body does not start on the same line as the
opening parenthesis of the loop."
  (let (comment-split)
    (condition-case ()
        (save-excursion
          (goto-char loop-start)
          (let ((line (line-number-at-pos))
                (maybe-split t))
            (forward-char 1)
            (forward-sexp 1)
            (save-excursion
              (when (looking-at "\\s-*\\\n*;")
                (search-forward ";")
                (backward-char 1)
                (if (= line (line-number-at-pos))
                    (setq maybe-split nil)
                  (setq comment-split t))))
            (forward-sexp 1)
            (backward-sexp 1)
            (if (eq (char-after) ?\()
                (if (or (not maybe-split) (= line (line-number-at-pos)))
                    'simple
                  'simple/split)
              (if (or (not maybe-split) (= line (line-number-at-pos)))
                  'extended
                'extended/split))))
      (error
       (if comment-split
           'simple/split
         'simple)))))

(defun sly--lisp-indent-trailing-comment ()
  (ignore-errors
    ;; If we had a trailing comment just before this, find it.
    (save-excursion
      (backward-sexp)
      (forward-sexp)
      (when (looking-at "\\s-*;")
        (search-forward ";")
        (1- (current-column))))))

;;;###autoload
(defun sly-common-lisp-indent-function (indent-point state)
  "Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`sly-lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's `sly-common-lisp-indent-function' property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to
  `sly-lisp-indent-defun-method'; i.e., like (4 &lambda &body),
  as explained below.

* any other symbol, meaning a function to call.  The function
  should take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN
  NORMAL-INDENT.  PATH is a list of integers describing the
  position of point in terms of list-structure with respect to
  the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1). In other
  words, to reach foo take the 0th element of the outermost list,
  then the 3rd element of the next list, and finally the 1st
  element. STATE and INDENT-POINT are as in the arguments to
  `sly-common-lisp-indent-function'. SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in. This function should behave like
  `sly--lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list starting with `as' specifies an indirection: indentation
  is done as if the form being indented had started with the
  second element of the list.

* any other list.  The list element in position M specifies how
  to indent the Mth function argument.  If there are fewer
  elements than function arguments, the last list element applies
  to all remaining arguments.  The accepted list elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3."
  (sly--lisp-indent-function-1 indent-point state))

(define-minor-mode sly-lisp-indent-compatibility-mode
    "Replace the definition of `common-lisp-indent-function' with `sly-common-lisp-indent-function'.

For backwards compatibility with the old sly-cl-indent.el, which
used to do this by default."
  :group 'sly-lisp-indent
  (if sly-lisp-indent-compatibility-mode
      (advice-add 'common-lisp-indent-function
                  :override 'sly-common-lisp-indent-function)
    (advice-remove 'common-lisp-indent-function
                   'sly-common-lisp-indent-function)))


(defvar sly--lisp-indent-feature-expr-regexp "#!?\\(+\\|-\\)")

;;; Semi-feature-expression aware keyword check.
(defun sly--lisp-indent-looking-at-keyword ()
  (or (looking-at ":")
      (and (looking-at sly--lisp-indent-feature-expr-regexp)
           (save-excursion
             (forward-sexp)
             (skip-chars-forward " \t\n")
             (sly--lisp-indent-looking-at-keyword)))))

;;; Semi-feature-expression aware backwards movement for keyword
;;; argument pairs.
(defun sly--lisp-indent-backward-keyword-argument ()
  (ignore-errors
    (backward-sexp 2)
    (when (looking-at sly--lisp-indent-feature-expr-regexp)
      (cond ((ignore-errors
               (save-excursion
                 (backward-sexp 2)
                 (looking-at sly--lisp-indent-feature-expr-regexp)))
             (sly--lisp-indent-backward-keyword-argument))
            ((ignore-errors
               (save-excursion
                 (backward-sexp 1)
                 (looking-at ":")))
             (backward-sexp))))
    t))

(defvar sly--lisp-indent-containing-sexp)

(defun sly--lisp-indent-function-1 (indent-point state)
  ;; If we're looking at a splice, move to the first comma.
  (when (or (eq (char-before) ?,)
            (and (eq (char-before) ?@)
                 (eq (char-before (1- (point))) ?,)))
    (when (re-search-backward "[^,@'],")
      (forward-char 1)))
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to containing lists.
          ;; `foo' has a path of (0 3 1) in `((a b c (d foo) f) g)'.
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          ;; If non-nil, this is an indentation to use
          ;; if nothing else specifies it more firmly.
          tentative-calculated
          ;; (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (sly--lisp-indent-parse-state-start state))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))

      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth sly-lisp-indent-maximum-backtracking))
        (let ((sly--lisp-indent-containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem full function method tentative-defun)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil full nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq full (downcase (buffer-substring-no-properties tem (point)))
                    function full)
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (sly--lisp-indent-get-indentation tem))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function (1+ (match-beginning 0)))
                           method (sly--lisp-indent-get-indentation
                                   (intern-soft function) full)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (sly--lisp-indent-get-indentation tem)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (ignore-errors
                           (forward-sexp 1)
                           (if (>= (point) indent-point)
                               nil
                             (parse-partial-sexp (point)
                                                 indent-point 1 t)
                             (setq n (1+ n))
                             t))))
              (setq path (cons n path)))

            ;; Guess.
            (when (and (not method) function (null (cdr path)))
              ;; (package prefix was stripped off above)
              (cond ((and (string-match "\\`def" function)
                          (not (string-match "\\`default" function))
                          (not (string-match "\\`definition" function))
                          (not (string-match "\\`definer" function)))
                     (setq tentative-defun t))
                    ((string-match
                      (eval-when-compile
                        (concat "\\`\\("
                                (regexp-opt '("with" "without" "do"))
                                "\\)-"))
                      function)
                     (setq method '(&lambda &body)))))

            ;; #+ and #- cleverness.
            (save-excursion
              (goto-char indent-point)
              (backward-sexp)
              (let ((indent (current-column)))
                (when
                    (or (looking-at sly--lisp-indent-feature-expr-regexp)
                        (ignore-errors
                          (backward-sexp)
                          (when (looking-at sly--lisp-indent-feature-expr-regexp)
                            (setq indent (current-column))
                            (let ((line (line-number-at-pos)))
                              (while
                                  (ignore-errors
                                    (backward-sexp 2)
                                    (and (= line (line-number-at-pos))
                                         (looking-at sly--lisp-indent-feature-expr-regexp)))
                                (setq indent (current-column))))
                            t)))
                  (setq calculated (list indent containing-form-start)))))

            (cond ((and (or (eq (char-after (1- sly--lisp-indent-containing-sexp)) ?\')
                            (and (not sly-lisp-backquote-indentation)
                                 (eq (char-after (1- sly--lisp-indent-containing-sexp)) ?\`)))
                        (not (eq (char-after (- sly--lisp-indent-containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((eq (char-after (1- sly--lisp-indent-containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method)
                   ;; If this looks like a call to a `def...' form,
                   ;; think about indenting it as one, but do it
                   ;; tentatively for cases like
                   ;; (flet ((defunp ()
                   ;;          nil)))
                   ;; Set both normal-indent and tentative-calculated.
                   ;; The latter ensures this value gets used
                   ;; if there are no relevant containing constructs.
                   ;; The former ensures this value gets used
                   ;; if there is a relevant containing construct
                   ;; but we are nested within the structure levels
                   ;; that it specifies indentation for.
                   (if tentative-defun
                       (setq tentative-calculated
                             (sly--lisp-indent-call-method
                              function sly-lisp-indent-defun-method
                              path state indent-point
                              sexp-column normal-indent)
                             normal-indent tentative-calculated)
                     (when sly-lisp-align-keywords-in-calls
                       ;; No method so far. If we're looking at a keyword,
                       ;; align with the first keyword in this expression.
                       ;; This gives a reasonable indentation to most things
                       ;; with keyword arguments.
                       (save-excursion
                         (goto-char indent-point)
                         (back-to-indentation)
                         (when (sly--lisp-indent-looking-at-keyword)
                           (while (sly--lisp-indent-backward-keyword-argument)
                             (when (sly--lisp-indent-looking-at-keyword)
                               (setq calculated
                                     (list (current-column)
                                           containing-form-start)))))))))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-function)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path) normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column lisp-body-indent))
                                          (t
                                           ;; other body form
                                           normal-indent))))
                  (t
                   (setq calculated
                         (sly--lisp-indent-call-method
                          function method path state indent-point
                          sexp-column normal-indent)))))
          (goto-char sly--lisp-indent-containing-sexp)
          ;; (setq last-point sly--lisp-indent-containing-sexp)
          (unless calculated
            (condition-case ()
                (progn (backward-up-list 1)
                       (setq depth (1+ depth)))
              (error
               (setq depth sly-lisp-indent-maximum-backtracking))))))

      (or calculated tentative-calculated
          ;; Fallback.
          ;;
          ;; Instead of punting directly to calculate-lisp-indent we
          ;; handle a few of cases it doesn't deal with:
          ;;
          ;; A: (foo (
          ;;          bar zot
          ;;          quux))
          ;;
          ;;    would align QUUX with ZOT.
          ;;
          ;; B:
          ;;   (foo (or x
          ;;            y) t
          ;;        z)
          ;;
          ;;   would align the Z with Y.
          ;;
          ;; C:
          ;;   (foo ;; Comment
          ;;        (bar)
          ;;        ;; Comment 2
          ;;        (quux))
          ;;
          ;;   would indent BAR and QUUX by one.
          (ignore-errors
            (save-excursion
              (goto-char indent-point)
              (back-to-indentation)
              (let ((p (point)))
                (goto-char containing-form-start)
                (down-list)
                (let ((one (current-column)))
                  (skip-chars-forward " \t")
                  (if (or (eolp) (looking-at ";"))
                      ;; A.
                      (list one containing-form-start)
                    (forward-sexp 2)
                    (backward-sexp)
                    (if (/= p (point))
                        ;; B.
                        (list (current-column) containing-form-start)
                      (backward-sexp)
                      (forward-sexp)
                      (let ((tmp (+ (current-column) 1)))
                        (skip-chars-forward " \t")
                        (if (looking-at ";")
                            ;; C.
                            (list tmp containing-form-start)))))))))))))



;; Dynamically bound in `sly--lisp-indent-call-method'.
(defvar sly--lisp-indent-error-function)

(defun sly--lisp-indent-call-method (function method path state indent-point
                                     sexp-column normal-indent)
  (let ((sly--lisp-indent-error-function function))
    (if (symbolp method)
        (funcall method
                 path state indent-point
                 sexp-column normal-indent)
      (sly--lisp-indent-259 method path state indent-point
                            sexp-column normal-indent))))

(defun sly--lisp-indent-report-bad-format (m)
  (error "%s has a badly-formed %s property: %s"
         ;; Love those free variable references!!
         sly--lisp-indent-error-function
         'sly-common-lisp-indent-function m))


;; Lambda-list indentation is now done in `sly--lisp-indent-lambda-list'.
;; See also `sly-lisp-lambda-list-keyword-alignment',
;; `sly-lisp-lambda-list-keyword-parameter-alignment' and
;; `sly-lisp-lambda-list-keyword-parameter-indentation' -- dvl

(defvar sly--lisp-indent-lambda-list-keywords-regexp
  "&\\(\
optional\\|rest\\|key\\|allow-other-keys\\|aux\\|whole\\|body\\|\
environment\\|more\
\\)\\>"
  "Regular expression matching lambda-list keywords.")

(defun sly--lisp-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (if (not sly-lisp-lambda-list-indentation)
      (1+ sexp-column)
    (sly--lisp-indent-properly-indent-lambda-list
     indent-point sexp-column containing-form-start)))

(defun sly--lisp-indent-properly-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (cond
    ((save-excursion
       (goto-char indent-point)
       (back-to-indentation)
       (looking-at sly--lisp-indent-lambda-list-keywords-regexp))
     ;; We're facing a lambda-list keyword.
     (if sly-lisp-lambda-list-keyword-alignment
         ;; Align to the first keyword if any, or to the beginning of
         ;; the lambda-list.
         (save-excursion
           (goto-char containing-form-start)
           (down-list)
           (let ((key-indent nil)
                 (next t))
             (while (and next (< (point) indent-point))
               (if (looking-at sly--lisp-indent-lambda-list-keywords-regexp)
                   (setq key-indent (current-column)
                         next nil)
                 (setq next (ignore-errors (forward-sexp) t))
                 (if next
                     (ignore-errors
                       (forward-sexp)
                       (backward-sexp)))))
             (or key-indent
                 (1+ sexp-column))))
       ;; Align to the beginning of the lambda-list.
       (1+ sexp-column)))
    (t
     ;; Otherwise, align to the first argument of the last lambda-list
     ;; keyword, the keyword itself, or the beginning of the
     ;; lambda-list.
     (save-excursion
       (goto-char indent-point)
       (let ((indent nil)
             (next t))
         (while (and next (> (point) containing-form-start))
           (setq next (ignore-errors (backward-sexp) t))
           (let* ((col (current-column))
                  (pos
                   (save-excursion
                     (ignore-errors (forward-sexp))
                     (skip-chars-forward " \t")
                     (if (eolp)
                         (+ col sly-lisp-lambda-list-keyword-parameter-indentation)
                       col))))
             (if (looking-at sly--lisp-indent-lambda-list-keywords-regexp)
                 (setq indent
                       (if sly-lisp-lambda-list-keyword-parameter-alignment
                           (or indent pos)
                         (+ col sly-lisp-lambda-list-keyword-parameter-indentation))
                       next nil)
               (setq indent col))))
         (or indent (1+ sexp-column)))))))

(defun sly--lisp-indent-lambda-list-initial-value-form-p (point)
  (let ((state 'x)
        (point (save-excursion
                 (goto-char point)
                 (back-to-indentation)
                 (point))))
    (save-excursion
      (backward-sexp)
      (ignore-errors (down-list 1))
      (while (and point (< (point) point))
        (cond ((looking-at "&\\(key\\|optional\\|aux\\)")
               (setq state 'key))
              ((looking-at sly--lisp-indent-lambda-list-keywords-regexp)
               (setq state 'x)))
        (if (not (ignore-errors (forward-sexp) t))
            (setq point nil)
          (ignore-errors
            (forward-sexp)
            (backward-sexp))
          (cond ((> (point) point)
                 (backward-sexp)
                 (when (eq state 'var)
                   (setq state 'x))
                 (or (ignore-errors
                       (down-list 1)
                       (cond ((> (point) point)
                              (backward-up-list))
                             ((eq 'key state)
                              (setq state 'var)))
                       t)
                     (setq point nil)))
                ((eq state 'var)
                 (setq state 'form))))))
    (eq 'form state)))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun sly--lisp-indent-259
    (method path state indent-point sexp-column normal-indent)
  (catch 'exit
    (let* ((p (cdr path))
           (containing-form-start (elt state 1))
           (n (1- (car path)))
           tem tail)
      (if (not (consp method))
          (sly--lisp-indent-report-bad-format method))
      (while n
        ;; This while loop is for advancing along a method
        ;; until the relevant (possibly &rest/&body) pattern
        ;; is reached.
        ;; n is set to (1- n) and method to (cdr method)
        ;; each iteration.
        (setq tem (car method))

        (or (eq tem 'nil)             ;default indentation
            (eq tem '&lambda)         ;lambda list
            (and (eq tem '&body) (null (cdr method)))
            (and (eq tem '&rest)
                 (consp (cdr method))
                 (null (cddr method)))
            (integerp tem)            ;explicit indentation specified
            (and (consp tem)          ;destructuring
                 (or (consp (car tem))
                     (and (eq (car tem) '&whole)
                          (or (symbolp (cadr tem))
                              (integerp (cadr tem))))))
            (and (symbolp tem)        ;a function to call to do the work.
                 (null (cdr method)))
            (sly--lisp-indent-report-bad-format method))
        (cond ((eq tem '&body)
               ;; &body means (&rest <lisp-body-indent>)
               (throw 'exit
                      (if (null p)
                          (+ sexp-column lisp-body-indent)
                        normal-indent)))
              ((eq tem '&rest)
               ;; this pattern holds for all remaining forms
               (setq tail (> n 0)
                     n 0
                     method (cdr method)))
              ((> n 0)
               ;; try next element of pattern
               (setq n (1- n)
                     method (cdr method))
               (if (< n 0)
                   ;; Too few elements in pattern.
                   (throw 'exit normal-indent)))
              ((eq tem 'nil)
               (throw 'exit (if (consp normal-indent)
                                normal-indent
                              (list normal-indent containing-form-start))))
              ((eq tem '&lambda)
               (throw 'exit
                      (cond ((not (eq (char-before) ?\)))
                             ;; If it's not a list at all, indent it
                             ;; like body instead.
                             (if (null p)
                                 (+ sexp-column lisp-body-indent)
                               normal-indent))
                            ((sly--lisp-indent-lambda-list-initial-value-form-p indent-point)
                             (if (consp normal-indent)
                                 normal-indent
                               (list normal-indent containing-form-start)))
                            ((null p)
                             (list (+ sexp-column 4) containing-form-start))
                            (t
                             ;; Indentation within a lambda-list. -- dvl
                             (list (sly--lisp-indent-lambda-list
                                    indent-point
                                    sexp-column
                                    containing-form-start)
                                   containing-form-start)))))
              ((integerp tem)
               (throw 'exit
                      (if (null p)         ;not in subforms
                          (list (+ sexp-column tem) containing-form-start)
                        normal-indent)))
              ((symbolp tem)          ;a function to call
               (throw 'exit
                      (funcall tem path state indent-point
                               sexp-column normal-indent)))
              (t
               ;; must be a destructing frob
               (if p
                   ;; descend
                   (setq method (cddr tem)
                         n (car p)
                         p (cdr p)
                         tail nil)
                 (let ((wholep (eq '&whole (car tem))))
                   (setq tem (cadr tem))
                   (throw 'exit
                          (cond (tail
                                 (if (and wholep (integerp tem)
                                          (save-excursion
                                            (goto-char indent-point)
                                            (back-to-indentation)
                                            (looking-at "\\sw")))
                                     ;; There's a further level of
                                     ;; destructuring, but we're looking at a
                                     ;; word -- indent to sexp.
                                     (+ sexp-column tem)
                                   normal-indent))
                                ((not tem)
                                 (list normal-indent
                                       containing-form-start))
                                ((integerp tem)
                                 (list (+ sexp-column tem)
                                       containing-form-start))
                                (t
                                 (funcall tem path state indent-point
                                          sexp-column normal-indent))))))))))))

(defun sly--lisp-indent-tagbody (path state indent-point sexp-column normal-indent)
  (if (cdr path)
      normal-indent
    (save-excursion
      (goto-char indent-point)
      (back-to-indentation)
      (list (cond ((looking-at "\\sw\\|\\s_")
                   ;; a tagbody tag
                   (+ sexp-column sly-lisp-tag-indentation))
                  ((integerp sly-lisp-tag-body-indentation)
                   (+ sexp-column sly-lisp-tag-body-indentation))
                  ((eq sly-lisp-tag-body-indentation 't)
                   (condition-case ()
                       (progn (backward-sexp 1) (current-column))
                     (error (1+ sexp-column))))
                  (t (+ sexp-column lisp-body-indent)))
            (nth 1 state)))))

(defun sly--lisp-indent-do (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 3)
      (let ((sly-lisp-tag-body-indentation lisp-body-indent))
        (sly--lisp-indent-tagbody
         path state indent-point sexp-column normal-indent))
    (sly--lisp-indent-259
     '((&whole nil &rest
        ;; the following causes weird indentation
        ;;(&whole 1 1 2 nil)
        )
       (&whole nil &rest 1))
     path state indent-point sexp-column normal-indent)))

(defun sly--lisp-indent-defsetf
    (path state indent-point sexp-column normal-indent)
  (ignore normal-indent)
  (let ((form-start (nth 1 state)))
    (list
     (cond
       ;; Inside the lambda-list in a long-form defsetf.
       ((and (eq 2 (car path)) (cdr path))
        (sly--lisp-indent-lambda-list indent-point sexp-column form-start))
       ;; Long form: has a lambda-list.
       ((or (cdr path)
            (save-excursion
              (goto-char form-start)
              (ignore-errors
                (down-list)
                (forward-sexp 3)
                (backward-sexp)
                (looking-at "nil\\|("))))
        (+ sexp-column (if (<= 1 (car path) 3) 4 2)))
       ;; Short form.
       (t (+ sexp-column (if (<= 1 (car path) 2) 4 2))))
     form-start)))

(defun sly--lisp-indent-beginning-of-defmethod-qualifiers ()
  (let ((case-fold-search t)
        (regexp "(\\(?:\\(def\\)\\|\\(:\\)\\)method"))
    (ignore-errors
      (while (not (looking-at regexp)) (backward-up-list))
      (cond ((match-string 1)
             (forward-char)
             ;; Skip name.
             (forward-sexp 2)
             1)
            ((match-string 2)
             (forward-char)
             (forward-sexp 1)
             0)))))

;; LISP-INDENT-DEFMETHOD now supports the presence of more than one method
;; qualifier and indents the method's lambda list properly. -- dvl
(defun sly--lisp-indent-defmethod
    (path state indent-point sexp-column normal-indent)
  (sly--lisp-indent-259
   (let ((nskip nil))
     (if (save-excursion
           (when (setq nskip (sly--lisp-indent-beginning-of-defmethod-qualifiers))
             (skip-chars-forward " \t\n")
             (while (looking-at "\\sw\\|\\s_")
               (cl-incf nskip)
               (forward-sexp)
               (skip-chars-forward " \t\n"))
             t))
         (nconc (make-list nskip 4) '(&lambda &body))
       (sly--lisp-indent-get-indentation 'defun)))
   path state indent-point sexp-column normal-indent))

(defun sly--lisp-indent-function-lambda-hack (path state indent-point
                                              sexp-column normal-indent)
  (ignore indent-point state)
  ;; indent (function (lambda () <newline> <body-forms>)) kludgily.
  (if (or (cdr path) ; wtf?
          (> (car path) 3))
      ;; line up under previous body form
      normal-indent
    ;; line up under function rather than under lambda in order to
    ;;  conserve horizontal space.  (Which is what #' is for.)
    (condition-case ()
        (save-excursion
          (backward-up-list 2)
          (forward-char 1)
          (if (looking-at "\\(\\(common-lisp\\|cl\\)::?\\)?function\\(\\Sw\\|\\S_\\)")
              (+ lisp-body-indent -1 (current-column))
            (+ sexp-column lisp-body-indent)))
      (error (+ sexp-column lisp-body-indent)))))

(defun sly--lisp-indent-loop (path state indent-point sexp-column normal-indent)
  (ignore sexp-column)
  (if (cdr path)
      normal-indent
    (let* ((loop-start (elt state 1))
           (type (sly--lisp-indent-loop-type loop-start)))
      (cond ((and sly-lisp-loop-indent-subclauses
                  (memq type '(extended extended/split)))
             (list (sly--lisp-indent-loop-macro-1 state indent-point)
                   (sly--lisp-indent-parse-state-start state)))
            (t
             (sly--lisp-indent-loop-part-indentation indent-point state type))))))

;;;; LOOP indentation, the complex version -- handles subclause indentation

;; Regexps matching various varieties of loop macro keyword ...
(defvar sly--common-lisp-body-introducing-loop-macro-keyword
  (concat "\\(?:\\_<\\|#?:\\)"
          (regexp-opt '("do" "doing" "finally" "initially"))
          "\\_>")
  "Regexp matching loop macro keywords which introduce body forms.")

;; Not currently used
(defvar sly--common-lisp-accumulation-loop-macro-keyword
  (concat "\\(?:\\_<\\|#?:\\)"
          (regexp-opt '("collect" "collecting"
                        "append" "appending"
                        "nconc" "nconcing"
                        "sum" "summing"
                        "count" "counting"
                        "maximize" "maximizing"
                        "minimize" "minimizing"))
          "\\_>")
  "Regexp matching loop macro keywords which introduce accumulation clauses.")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar sly--common-lisp-prefix-loop-macro-keyword
  (concat "\\(?:\\_<\\|#?:\\)" (regexp-opt '("and" "else")) "\\_>")
  "Regexp matching loop macro keywords which are prefixes.")

(defvar sly--common-lisp-indent-clause-joining-loop-macro-keyword
  "\\(?:\\_<\\|#?:\\)and\\_>"
  "Regexp matching 'and', and anything else there ever comes to be like it.")

(defvar sly--common-lisp-indent-indented-loop-macro-keyword
  (concat "\\(?:\\_<\\|#?:\\)"
          (regexp-opt '("upfrom" "downfrom" "upto" "downto" "below" "above"
                        "into" "in" "on" "by" "from" "to" "by" "across" "being"
                        "each" "the" "then" "hash-key" "hash-keys" "hash-value"
                        "hash-values" "present-symbol" "present-symbols"
                        "external-symbol" "external-symbols" "using" "symbol"
                        "symbols" "float" "fixnum" "t" "nil" "of-type" "of" "="))
          "\\_>")
  "Regexp matching keywords introducing loop subclauses.
Always indented two.")

(defvar sly--common-lisp-indenting-loop-macro-keyword
  (concat "\\(?:\\_<\\|#?:\\)" (regexp-opt '("when" "unless" "if")) "\\_>")
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented.")

(defvar sly--lisp-indent-loop-macro-else-keyword
  "\\(?:\\_<\\|#?:\\)else\\_>")

;;; Attempt to indent the loop macro ...
(defun sly--lisp-indent-loop-part-indentation (indent-point state type)
  "Compute the indentation of loop form constituents."
  (let* ((loop-start (nth 1 state))
         (loop-indentation (save-excursion
                             (goto-char loop-start)
                             (if (eq type 'extended/split)
                                 (- (current-column) 4)
                               (current-column))))
         (indent nil)
         (re "\\(\\(#?:\\)?\\sw+\\|)\\|\n\\)"))
    (goto-char indent-point)
    (back-to-indentation)
    (cond ((eq type 'simple/split)
           (+ loop-indentation sly-lisp-simple-loop-indentation))
          ((eq type 'simple)
           (+ loop-indentation 6))
          ;; We are already in a body, with forms in it.
          ((and (not (looking-at re))
                (save-excursion
                  (while (and (ignore-errors (backward-sexp) t)
                              (not (looking-at re)))
                    (setq indent (current-column)))
                  (and indent
                       (looking-at sly--common-lisp-body-introducing-loop-macro-keyword))))
           (list indent loop-start))
          ;; Keyword-style or comment outside body
          ((or sly-lisp-loop-indent-forms-like-keywords
               (looking-at re)
               (looking-at ";"))
           (if (and (looking-at ";")
                    (let ((p (sly--lisp-indent-trailing-comment)))
                      (when p
                        (setq loop-indentation p))))
               (list loop-indentation loop-start)
             (list (+ loop-indentation 6) loop-start)))
          ;; Form-style
          (t
           (list (+ loop-indentation 9) loop-start)))))

(defun sly--lisp-indent-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (unless (eolp)
    (current-column)))

(defun sly--lisp-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      (goto-char (sly--lisp-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
        (sly--lisp-indent-loop-advance-past-keyword-on-line)

        (when (eolp)
          (forward-line 1)
          (end-of-line)
          ;; If indenting first line after "(loop <newline>"
          ;; cop out ...
          (if (<= indent-point (point))
              (throw 'return-indentation
                     (+ loop-start-column
                        sly-lisp-loop-clauses-indentation)))
          (back-to-indentation))

        (let* ((case-fold-search t)
               (loop-macro-first-clause (point))
               (previous-expression-start
                (sly--lisp-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (loop-body-p nil)
               (loop-body-indentation nil)
               (indented-clause-indentation (+ 2 default-value)))
          ;; Determine context of this loop clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)

          ;; Handle a body-introducing-clause which ends a line specially.
          (if (looking-at sly--common-lisp-body-introducing-loop-macro-keyword)
              (let ((keyword-position (current-column)))
                (setq loop-body-p t)
                (setq loop-body-indentation
                      (if (sly--lisp-indent-loop-advance-past-keyword-on-line)
                          (current-column)
                        (back-to-indentation)
                        (if (/= (current-column) keyword-position)
                            (+ 2 (current-column))
                          (+ sly-lisp-loop-body-forms-indentation
                             (if sly-lisp-loop-indent-body-forms-relative-to-loop-start
                                 loop-start-column
                               keyword-position))))))

            (back-to-indentation)
            (if (< (point) loop-macro-first-clause)
                (goto-char loop-macro-first-clause))
            ;; If there's an "and" or "else," advance over it.
            ;; If it is alone on the line, the next "cond" will treat it
            ;; as if there were a "when" and indent under it ...
            (let ((exit nil))
              (while (and (null exit)
                          (looking-at sly--common-lisp-prefix-loop-macro-keyword))
                (if (null (sly--lisp-indent-loop-advance-past-keyword-on-line))
                    (progn (setq exit t)
                           (back-to-indentation)))))

            ;; Found start of loop clause preceding the one we're
            ;; trying to indent. Glean context ...
            (cond
             ((looking-at "(")
              ;; We're in the middle of a clause body ...
              (setq loop-body-p t)
              (setq loop-body-indentation (current-column)))
             ((looking-at sly--common-lisp-body-introducing-loop-macro-keyword)
              (setq loop-body-p t)
              ;; Know there's something else on the line (or would
              ;; have been caught above)
              (sly--lisp-indent-loop-advance-past-keyword-on-line)
              (setq loop-body-indentation (current-column)))
             (t
              (setq loop-body-p nil)
              (if (or (looking-at sly--common-lisp-indenting-loop-macro-keyword)
                      (looking-at sly--common-lisp-prefix-loop-macro-keyword))
                  (setq default-value (+ 2 (current-column))))
              (setq indented-clause-indentation (+ 2 (current-column)))
              ;; We still need loop-body-indentation for "syntax errors" ...
              (goto-char previous-expression-start)
              (setq loop-body-indentation (current-column)))))

          ;; Go to first non-blank character of the line we're trying
          ;; to indent. (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
           ((looking-at "(")
            ;; Clause body ...
            loop-body-indentation)
           ((or (eolp) (looking-at ";"))
            ;; Blank line.  If body-p, indent as body, else indent as
            ;; vanilla clause.
            (if loop-body-p
                loop-body-indentation
              (or (and (looking-at ";") (sly--lisp-indent-trailing-comment))
                  default-value)))
           ((looking-at sly--common-lisp-indent-indented-loop-macro-keyword)
            indented-clause-indentation)
           ((looking-at sly--common-lisp-indent-clause-joining-loop-macro-keyword)
            (let ((stolen-indent-column nil))
              (forward-line -1)
              (while (and (null stolen-indent-column)
                          (> (point) loop-macro-first-clause))
                (back-to-indentation)
                (if (and (< (current-column) loop-body-indentation)
                         (looking-at "\\(#?:\\)?\\sw"))
                    (progn
                      (if (looking-at sly--lisp-indent-loop-macro-else-keyword)
                          (sly--lisp-indent-loop-advance-past-keyword-on-line))
                      (setq stolen-indent-column (current-column)))
                  (forward-line -1)))
              (or stolen-indent-column default-value)))
           (t default-value)))))))

(defalias 'sly--lisp-indent-if*-advance-past-keyword-on-line
  #'sly--lisp-indent-loop-advance-past-keyword-on-line)

;;;; IF* is not standard, but a plague upon the land
;;;; ...let's at least try to indent it.

(defvar sly--lisp-indent-if*-keyword
  "thenret\\|elseif\\|then\\|else"
  "Regexp matching if* keywords")

(defun sly--lisp-indent-if*
    (path parse-state indent-point sexp-column normal-indent)
  (ignore normal-indent path sexp-column)
  (list (sly--lisp-indent-if*-1 parse-state indent-point)
        (sly--lisp-indent-parse-state-start parse-state)))

(defun sly--lisp-indent-if*-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of if* macro, and use it to establish
      ;; base column for indentation
      (goto-char (sly--lisp-indent-parse-state-start parse-state))
      (let ((if*-start-column (current-column)))
        (sly--lisp-indent-if*-advance-past-keyword-on-line)
        (let* ((case-fold-search t)
               (if*-first-clause (point))
               (previous-expression-start
                (sly--lisp-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (if*-body-p nil)
               (if*-body-indentation nil))
          ;; Determine context of this if* clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)
          ;; Handle a body-introducing-clause which ends a line specially.
          (back-to-indentation)
          (if (< (point) if*-first-clause)
              (goto-char if*-first-clause))
          ;; Found start of if* clause preceding the one we're trying
          ;; to indent. Glean context ...
          (cond
            ((looking-at sly--lisp-indent-if*-keyword)
             (setq if*-body-p t)
             ;; Know there's something else on the line (or would
             ;; have been caught above)
             (sly--lisp-indent-if*-advance-past-keyword-on-line)
             (setq if*-body-indentation (current-column)))
            ((looking-at "#'\\|'\\|(")
             ;; We're in the middle of a clause body ...
             (setq if*-body-p t)
             (setq if*-body-indentation (current-column)))
            (t
             (setq if*-body-p nil)
             ;; We still need if*-body-indentation for "syntax errors" ...
             (goto-char previous-expression-start)
             (setq if*-body-indentation (current-column))))

          ;; Go to first non-blank character of the line we're trying
          ;; to indent. (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
            ((or (eolp) (looking-at ";"))
             ;; Blank line.  If body-p, indent as body, else indent as
             ;; vanilla clause.
             (if if*-body-p
                 if*-body-indentation
               default-value))
            ((not (looking-at sly--lisp-indent-if*-keyword))
             ;; Clause body ...
             if*-body-indentation)
            (t (- (+ 7 if*-start-column)
                  (- (match-end 0) (match-beginning 0))))))))))


;;;; Indentation specs for standard symbols, and a few semistandard ones.
(defun sly--lisp-indent-init-standard-indentation ()
  (let ((l '((block 1)
             (case (4 &rest (&whole 2 &rest 1)))
             (ccase (as case))
             (ecase (as case))
             (typecase (as case))
             (etypecase (as case))
             (ctypecase (as case))
             (catch 1)
             (cond (&rest (&whole 2 &rest nil)))
             ;; for DEFSTRUCT
             (:constructor (4 &lambda))
             (defvar (4 2 2))
             (defclass (6 (&whole 4 &rest 1)
                          (&whole 2 &rest 1)
                          (&whole 2 &rest 1)))
             (defconstant (as defvar))
             (defcustom (4 2 2 2))
             (defparameter (as defvar))
             (defconst (as defcustom))
             (define-condition (as defclass))
             (define-modify-macro (4 &lambda &body))
             (defsetf sly--lisp-indent-defsetf)
             (defun (4 &lambda &body))
             (defgeneric (4 &lambda &body))
             (define-setf-method (as defun))
             (define-setf-expander (as defun))
             (defmacro (as defun))
             (defsubst (as defun))
             (deftype (as defun))
             (defmethod sly--lisp-indent-defmethod)
             (defpackage (4 2))
             (defstruct ((&whole 4 &rest (&whole 2 &rest 1))
                          &rest (&whole 2 &rest 1)))
             (destructuring-bind (&lambda 4 &body))
             (do sly--lisp-indent-do)
             (do* (as do))
             (dolist ((&whole 4 2 1) &body))
             (dotimes (as dolist))
             (eval-when 1)
             (flet ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
             (labels (as flet))
             (macrolet (as flet))
             (generic-flet (as flet))
             (generic-labels (as flet))
             (handler-case (4 &rest (&whole 2 &lambda &body)))
             (restart-case (as handler-case))
             ;; single-else style (then and else equally indented)
             (if (&rest nil))
             (if* sly--lisp-indent-if*)
             (lambda (&lambda &rest sly--lisp-indent-function-lambda-hack))
             (let ((&whole 4 &rest (&whole 1 1 2)) &body))
             (let* (as let))
             (compiler-let (as let))
             (handler-bind (as let))
             (restart-bind (as let))
             (locally 1)
             (loop sly--lisp-indent-loop)
             (:method sly--lisp-indent-defmethod) ; in `defgeneric'
             (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
             (multiple-value-call (4 &body))
             (multiple-value-prog1 1)
             (multiple-value-setq (4 2))
             (multiple-value-setf (as multiple-value-setq))
             (named-lambda (4 &lambda &rest sly--lisp-indent-function-lambda-hack))
             (pprint-logical-block (4 2))
             (print-unreadable-object ((&whole 4 1 &rest 1) &body))
             ;; Combines the worst features of BLOCK, LET and TAGBODY
             (prog (&lambda &rest sly--lisp-indent-tagbody))
             (prog* (as prog))
             (prog1 1)
             (prog2 2)
             (progn 0)
             (progv (4 4 &body))
             (return 0)
             (return-from (nil &body))
             (symbol-macrolet (as let))
             (tagbody sly--lisp-indent-tagbody)
             (throw 1)
             (unless 1)
             (unwind-protect (5 &body))
             (when 1)
             (with-slots (as multiple-value-bind))
             (with-accessors (as multiple-value-bind))
             (with-condition-restarts (as multiple-value-bind))
             (with-compilation-unit ((&whole 4 &rest 1) &body))
             (with-output-to-string (4 2))
             (with-standard-io-syntax (2)))))
    (dolist (el l)
      (let* ((name (car el))
             (indentation (cadr el)))
        (put name 'sly-common-lisp-indent-function indentation)))))

(sly--lisp-indent-init-standard-indentation)

(provide 'sly-cl-indent)

;;; sly-cl-indent.el ends here
