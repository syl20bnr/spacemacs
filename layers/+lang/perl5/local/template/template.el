;;; template.el --- use templates, decorate comments, auto-update buffers

;; Copyright (C) 1995-2003, 2008, 2009, 2012, 2015 Free Software Foundation, Inc.
;;
;; Author: Christoph Wedler <wedler@users.sourceforge.net>
;; Version: 3.3b
;; Keywords: template, comment decoration, auto-updating, data, tools
;; X-URL: http://emacs-template.sourceforge.net/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; When you create a new file with Emacs, package Template supplies an initial
;; buffer content via a template: a file with normal text and expansion
;; forms. There is a menu to easily create such templates. You can also use new
;; commands to decorate comments and update the buffer contents.

;; The main difference between Template and other similar packages is that you
;; can define very flexible templates without having to learn Lisp or changing
;; your Emacs init file. This package does not help Lisp programmers to define
;; complex macros.

;; For details, check <http://emacs-template.sourceforge.net/> or, if you
;; prefer the manual style, the documentation of the following commands and
;; variables:
;;
;;  * for templates: \\[template-new-file], `template-auto-insert',
;;    `template-derivation-alist', `template-default-expansion-alist' and
;;    `template-definition-start',
;;  * for comment decoration: \\[template-single-comment] and
;;    \\[template-block-comment], `template-comment-specification-alist'
;;  * for updating: \\[template-update-buffer], `template-auto-update',
;;    `template-update-buffer-alist' and `template-header-regexp-alist'.

;; Bug fixes, bug reports, improvements, and suggestions for the newest version
;; are strongly appreciated.

;;; Installation:

;;  1. Make sure to use Emacs-22.2, XEmacs-20.2 or higher.
;;  2. Put this file into your load-path, i.e. any directory mentioned in the
;;     value of `load-path'.
;;  3. Byte-compile this file.
;;  4. Load this package by M-x load-library RET template RET
;;  5. Customize via M-x customize-variable RET template-use-package RET
;;  6. Toggle the [Template Use Package] option to "in use".
;;  7. Save your customization via [Save for future sessions].

;; The steps 4-7 add the following to your custom file:
;;   (custom-set-variables '(template-use-package t nil (template)))

;; Remark: adding some code (invoking template-initialize) to your init file
;; like in previous versions of session.el still works.

;; To customize, use `M-x customize-group RET template RET' or the customize
;; entry in menu Options.

;;; Code:

(require 'custom)

;; General Emacs/XEmacs-compatibility compile-time macros
(eval-when-compile
  (require 'cl)
  (defmacro cond-emacs-xemacs (&rest args)
    (cond-emacs-xemacs-macfn
     args "`cond-emacs-xemacs' must return exactly one element"))
  (defun cond-emacs-xemacs-macfn (args &optional msg)
    (if (atom args) args
      (and (eq (car args) :@) (null msg) ; (:@ ...spliced...)
	   (setq args (cdr args)
		 msg "(:@ ....) must return exactly one element"))
      (let ((ignore (if (string-match "XEmacs" emacs-version) :EMACS :XEMACS))
	    (mode :BOTH) code)
	(while (consp args)
	  (if (memq (car args) '(:EMACS :XEMACS :BOTH)) (setq mode (pop args)))
	  (if (atom args)
	      (or args (error "Used selector %s without elements" mode))
	    (or (eq ignore mode)
		(push (cond-emacs-xemacs-macfn (car args)) code))
	    (pop args)))
	(cond (msg (if (or args (cdr code)) (error msg) (car code)))
	      ((or (null args) (eq ignore mode)) (nreverse code))
	      (t (nconc (nreverse code) args))))))
  ;; Emacs/XEmacs-compatibility `defun': remove interactive "_" for Emacs, use
  ;; existing functions when they are `fboundp', provide shortcuts if they are
  ;; known to be defined in a specific Emacs branch (for short .elc)
  (defmacro defunx (name arglist &rest definition)
    (let ((xemacsp (string-match "XEmacs" emacs-version)) reuses first)
      (while (memq (setq first (car definition))
		   '(:try :emacs-and-try :xemacs-and-try
			  :emacs-only :xemacs-only))
	(if (memq first (if xemacsp
			    '(:xemacs-and-try :xemacs-only)
			  '(:emacs-and-try :emacs-only)))
	    (setq reuses (cadr definition)
		  definition nil)
	  (unless (memq first '(:emacs-only :xemacs-only))
	    (push (cadr definition) reuses)))
	(setq definition (cddr definition)))
      (if (and reuses (symbolp reuses))
	  `(defalias ',name ',reuses)
	(let* ((docstring (if (stringp (car definition)) (pop definition)))
	       (spec (and (not xemacsp)
			  (eq (car-safe (car definition)) 'interactive)
			  (null (cddar definition))
			  (cadar definition))))
	  (if (and (stringp spec)
		   (not (string-equal spec ""))
		   (eq (aref spec 0) ?_))
	      (setq definition
		    (cons (if (string-equal spec "_")
			      '(interactive)
			    `(interactive ,(substring spec 1)))
			  (cdr definition))))
	  (if (null reuses)
	      `(defun ,name ,arglist ,docstring
		 ,@(cond-emacs-xemacs-macfn definition))
	    ;; no dynamic docstring in this case
	    `(eval-and-compile		; no warnings in Emacs
	       (defalias ',name
		 (cond ,@(mapcar (lambda (func) `((fboundp ',func) ',func))
				 (nreverse reuses))
		       (t ,(if definition
			       `(lambda ,arglist ,docstring
				  ,@(cond-emacs-xemacs-macfn definition))
			     'ignore)))))))))))

(eval-when-compile
  (require 'cl)
  (defvar init-file-loaded)		; would be useful in Emacs, too...
  (defvar file-name-buffer-file-type-alist))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(defconst template-version "3.3b"
  "Current version of package template.
Check <http://emacs-template.sourceforge.net/> for the newest.")


;;;===========================================================================
;;;  Customization and initialization
;;;===========================================================================

(defgroup template nil
  "Use templates, decorate comments, auto-update buffers."
  :group 'data
  :link '(emacs-commentary-link "template.el")
  :link '(url-link "http://emacs-template.sourceforge.net/")
  :prefix "template-")

(defgroup template-comments nil
  "Comment decorations in package template."
  :group 'template
  :prefix "template-")

(defgroup template-updating nil
  "Updating with package template."
  :group 'template
  :prefix "template-")

(defgroup template-derivation nil
  "Deriving templates for new files."
  :group 'template
  :prefix "template-")

(defgroup template-expansion nil
  "Expanding the expansion forms in templates."
  :group 'template
  :prefix "template-")

(defgroup template-miscellaneous nil
  "Miscellaneous configurations of package template."
  :group 'template
  :prefix "template-")

(defcustom template-initialize t
  "Whether/what to initialize with function `template-initialize'.
If t, do full initialization.  Otherwise, the value should be a list
with elements.  To enable, include

 * `auto' to enable `template-auto-update' and `template-auto-insert',
 * `ffap' to make sure that `auto' works with `find-file-at-point',
 * `cc-mode' to enable correct C/C++/Java/Antlr comment filling, i.e.,
   to add `template-c-init-fill-function' to `c-mode-common-hook',
 * `de-html-helper' to disable `html-helper's template and time-stamps,
 * `keys' to setup the default key bindings,
 * `menus' to setup the menus."
  :group 'template-miscellaneous
  :type '(choice (const :tag "All" t)
		 (set :value (auto cc-mode keys menus)
		      (const :tag "Auto Updating/Inserting" auto)
		      (const :tag "Correct Auto Inserting with Ffap" ffap)
		      (const :tag "Correct C Comment Filling" cc-mode)
		      (const :tag "Deactivate html-helper" de-html-helper)
		      (const :tag "Setup Key Bindings" keys)
		      (const :tag "Setup Menus" menus))))


;;;===========================================================================
;;;  Menu
;;;===========================================================================

(defvar template-comment-menu
  '("Comment"
    ["Decorate Comment Line" template-single-comment
     :active (and (not buffer-read-only)
		  (memq (template-comment-at-point)
			'(none delimited single cont)))]
    ["Decorate Comment Block" template-block-comment
     :active (and (not buffer-read-only)
		  (memq (template-comment-at-point)
			'(single block)))]
    "---"
    ["Indent for Comment" indent-for-comment
     :active (and comment-start (not buffer-read-only))]
    ["Continue Comment" indent-new-comment-line
     :active (and  comment-start (not buffer-read-only))]
    ["Comment Region" comment-region
     :active (and comment-start (not buffer-read-only) (mark))]
    ["Comment Region 2" (comment-region 2)
     :active (and comment-start (not buffer-read-only) (mark))]
    ["Comment Region 3" (comment-region 3)
     :active (and comment-start (not buffer-read-only) (mark))]
    "---"
    ["Update Buffer" template-update-buffer
     :active (and template-update-buffer-alist (not buffer-read-only))])
  "Menu for comment functions.")

(defvar template-creation-menu
  '("Template Creation"
    :filter template-menu-filter
    ["Open Template" template-open-template
     :active (null (template-buffer-template-p))]
    "--"
    ["Define User Input" template-define-prompt t]
    ["Define Text Register" template-define-register t]
    ["Define Message" template-define-message t]
    "---"
    ["Insert Expansion Form" template-insert-form t])
  "Menu for template creation.")


;;;===========================================================================
;;;  Commenting
;;;===========================================================================

(defcustom template-max-column -1
  "*Width of the separator line, to use with an empty `comment-end'.
If the value is zero or negative, it is added to `fill-column'.  See
also `template-max-column-with-end'."
  :group 'template-comments
  :type 'integer)

(defcustom template-max-column-with-end 0
  "*Width of the separator line including a non-empty `comment-end'.
If the value is zero or negative, it is added to `fill-column'.  See
also `template-max-column'."
  :group 'template-comments
  :type 'integer)

(defcustom template-alt-comment-syntax-alist
  '((t "/* " " */"))
  "Alternative comment syntax for languages with \"mixed\" comments.
Used by function `template-comment-syntax'.  Elements look like
  (MODES-OR-REGEXP COMMENT-START COMMENT-END)

If the current `major-mode' has a empty `comment-end' and a commenting
command does not work at `point' with the usual `comment-start', we
search for the first matching alternative comment syntax in this alist.

Each element must \"pass\" MODES-OR-REGEXP.  If this is a list, it must
include the current major-mode, if this is a regexp, it must match the
variable `buffer-file-name' without version, otherwise it must be
non-nil.

Then, COMMENT-START and COMMENT-END is used as the alternative comment
syntax if `comment-start-skip' matches COMMENT-START."
  :group 'template-comments
  :type '(repeat (group (choice (repeat :tag "In major modes" :value nil
					function)
				(regexp :tag "Buffer matching" :value "")
				(sexp :tag "Always" :value t))
			(string :tag "Alt comment start" :value "/* ")
			(string :tag "Alt comment end" :value " */"))))

(defcustom template-comment-indent t
  "Non-nil means, indent single-line/block comments.
Commands \\[template-single-comment] and \\[template-block-comment]
indent the comment lines if this value is non-nil and the current major
mode is not a member of `template-indent-mode-disable-list' or if this
value is nil and the current major mode is a member of
`template-indent-mode-enable-list'."
  :group 'template-comments
  :type 'boolean)

(defcustom template-indent-mode-disable-list '(sh-mode makefile-mode)
  "Major modes not having indented single-line/block comments.
Used if `template-comment-indent' is non-nil.  Major modes in which
pressing TAB twice is different from pressing TAB once are good
candidates for this list."
  :group 'template-comments
  :type '(repeat (function :tag "Major mode")))

(defcustom template-indent-mode-enable-list nil
  "Major modes having indented single-line/block comments.
Used if `template-comment-indent' is nil."
  :group 'template-comments
  :type '(repeat (function :tag "Major mode")))

(defcustom template-comment-specification-alist
  '(("-" "" "" 0)
    ("-" "" "" 0)
    ("=" "\n\n" "\n" 1)
    ("#" "\n\n\f\n" "\n\n" 2))
  "List of specifications for comment functions.
Each specification at LEVEL, starting at 1, is a list
  (SEPARATOR BEFORE-BLOCK AFTER-BLOCK DELETE-LINES)

SEPARATOR is the string which is inserted repeatedly by commands
\\[template-single-comment] and \\[template-block-comment] up to
`template-max-column'.

After that, \\[template-block-comment] deletes DELETE-LINES after the
comment block and inserts string AFTER-BLOCK at the end of the block and
BEFORE-BLOCK at the front of the block.

The specification LEVEL to use is determined by:
 (1) If the prefix argument is non-nil and its numeric value is > 0,
     this value is the LEVEL.
 (2) If the prefix argument is nil, and there is an old comment style,
     use old comment style.
 (3) If `template-comment-specification-special' is a function or the
     current major mode has a property with this name and its value is a
     function, this function returns the specification.
 (4) If `comment-end' is empty and `comment-start' is a string of length
     1: LEVEL is number of repetitions of `comment-start' at the
     beginning of the line.  Otherwise, if the correctly indented line
     starts at the beginning of the line, LEVEL=3, else LEVEL=2."
  :group 'template-comments
  :type '(repeat (group (string :tag "Separator" :value "-")
			(string :tag "Before block" :value "")
			(string :tag "After block" :value "")
			(integer :tag "Delete lines" :value 0))))

(defcustom template-comment-specification-special nil
  "Function used for special commenting styles or nil.
See `template-comment-specification-alist' for details."
  :group 'template-comments
  :type '(choice (const nil) function))


;;;===========================================================================
;;;  Auto updating
;;;===========================================================================

(defcustom template-auto-update 'query
  "*Whether to update parts of the content when saving the buffer.
When non-nil and `template-auto-update-disable-regexp' does not match
the file name, automatically updates parts of the buffer, see
`template-update-buffer-alist'.  With value t or if the entry in the
alist has no prompt, do not ask for confirmation.

Automatic updating only works if this feature has been enabled
and this package is in use, see variables `template-use-package'
and `template-initialize'."
  :group 'template-updating
  :type '(radio (const :tag "No" nil)
		(const :tag "Without confirmation" t)
		(sexp :tag "With confirmation" :format "%t" :value query)))

(defcustom template-auto-update-disable-regexp nil
  "*Regexp matching files not to automatically update.
Value nil matches no file.  See `template-auto-update'."
  :group 'template-updating
  :type '(choice (const :tag "none" nil) regexp))

(defcustom template-update-buffer-alist
  '((t "Update header in %s? "
       (template-update-header t)
       (file-name-sans-versions (file-name-nondirectory buffer-file-name)))
    ((html-mode) "Update date inside <address> in %s? "
     (-2000
      "\\([0-9]+[ \t]+[A-Za-z][A-Za-z][A-Za-z][ \t]+[0-9]+\\)[ \t\n]*</address>"
      1)
     (format-time-string "%d %b %Y")))
  "Alist used how to update parts of the buffer.
Used by function `template-update-buffer'.  Elements look like
  (MODES-OR-REGEXP PROMPT TEST NEW REPLACEMENT-FUN)

Each element must \"pass\" MODES-OR-REGEXP.  If this is a list, it must
include the current major-mode, if this is a regexp, it must match the
variable `buffer-file-name' without version, otherwise it must be
non-nil.

Then, TEST is `eval'd and must return the region = (BEG . END) to be
replaced or nil if nothing should be updated according to the current
element.  If TEST is a list and the `car' of TEST is not a function,
`template-update-buffer-region' is used as the default function, i.e.,
TEST looks like (LIMIT REGEXP GROUP).  Then, check first/last
LIMIT characters in buffer and return region according to GROUP's regexp
group in REGEXP.

Then, NEW is `eval'd.  If it is a string, it is considered as
replacement for the region, otherwise REPLACE-FUN must be non-nil.

Then, ask user for confirmation with PROMPT where %s is substituted by
the buffer name if PROMPT is a string and `template-auto-update' is not
t.

Finally, REPLACEMENT-FUN is called the `eval'd NEW and the beginning and
the end of the region returned by TEST.  If REPLACEMENT-FUN is nil, just
replace the region by the `eval'd NEW."
  :group 'template-updating
  :type '(repeat (group (choice (repeat :tag "In major modes" :value nil
					function)
				(regexp :tag "Buffer matching" :value "")
				(sexp :tag "Always" :value t))
			(string :tag "Prompt" :value "Update in %s? ")
			(choice (list :tag "Default test"
				      (choice (const :tag "No limit" nil)
					      (integer :tag "Limit" -1000))
				      regexp
				      (integer :tag "Regexp group" :value 0))
				(sexp :tag "Eval sexp"))
			(sexp :tag "Eval New string")
			(option (function :tag "Replacement function")))))

(defcustom template-header-lines 3
  "*Last line number which is checked by \\[template-update-header]."
  :group 'template-updating
  :type 'integer)

(put 'template-header-lines 'template-secure-value #'integerp)

(defcustom template-header-regexp-alist
  '(("@(#)\\([^ \t\n]+\\)" . 1)
    ("^%s[ \t]*\\([^ \t\n%s][^ \t\n]*\\)[ \t]+--" . 1))
  "Alist of regexps matching the file name in the header.
The `car' of each element is the REGEXP with %s, if present, substituted
by the comment start.  A second %s, if present, is substitud by a single
letter non-alpha comment start, or the empty string otherwise.

The `cdr' is the regexp group to be replaced.  Used by
\\[template-update-header].

The comment start is evaluated from `comment-start', the first character
in the buffer or \"#\".  It is assumed that a non-alpha single character
comment start may be repeated.  For example, the substituted regexp in
`emacs-lisp-mode' is \"\;+\", in `c++-mode' \"//\"."
  :group 'template-updating
  :type '(repeat (cons :format "%v"
		       regexp
		       (integer :tag "Regexp group" :value 0))))


;;;===========================================================================
;;;  Templates: finding templates
;;;===========================================================================

(defcustom template-auto-insert 'query
  "*Whether to automatically use template files for new files.
Used if the user gave a non-existent file as argument to a command in
`template-find-file-commands'.  When non-nil and a matching template
file can be found, use a template like in `template-new-file'.  File
name refinement is never performed, see `template-derivation-alist'.

With value t, do not ask for confirmation.

Automatic using a template only works if this feature has been enabled
and this package is in use, see variables `template-use-package'
and `template-initialize'."
  :group 'template-derivation
  :type '(radio (const :tag "No" nil)
		(const :tag "Without confirmation" t)
		(sexp :tag "With confirmation" :format "%t" :value query)))

(defcustom template-find-file-commands
  '(find-file find-file-other-frame find-file-other-screen
	      find-file-other-window find-file-at-point ffap nil)
  "*Commands which use templates as last resort, see `template-auto-insert'.
See also `template-file-select-commands'.

Include nil if you want to use templates for non-existing files as
command line arguments when starting Emacs."
  :group 'template-derivation
  :type '(repeat (function :tag "Command")))

(defcustom template-file-select-commands
  '(exit-minibuffer minibuffer-complete-and-exit
		    list-mode-item-mouse-selected
		    list-mode-item-keyboard-selected
		    ido-exit-minibuffer)
  "*Commands which select the file name via minibuffer/completions.
Checked with commands in `template-find-file-commands'."
  :group 'template-derivation
  :type '(repeat (function :tag "Command")))

(defface template-message-face
  '((((class color) (background light)) (:background "pink"))
    (t (:bold t)))
  "Face for temporary message at point.  This only works with XEmacs."
  :group 'template-miscellaneous)

(defcustom template-extension ".tpl"
  "*Extension used for template files."
  :group 'template-derivation
  :type 'string)

(defcustom template-subdirectories '("./" "Templates/")
  "*List of subdirectories for template files.
See `template-derivation-alist' for details."
  :group 'template-derivation
  :type '(repeat directory))

(defcustom template-stop-derivation
  (cond ((fboundp 'file-remote-p) 'file-remote-p)
	((fboundp 'efs-ftp-path) 'efs-ftp-path)
	((fboundp 'ange-ftp-ftp-path) 'ange-ftp-ftp-path))
  "If non-nil, function used to determine whether to stop derivation.
If non-nil, function is called with argument DIR.  If it returns t,
`template-derivation' stops to search for more project specific
templates, i\.e\., just searches in `template-default-directories'."
  :group 'template-derivation
  :type '(choice (const :tag "Never" nil)
		 function))

(defconst template-load-file-name
  (and (boundp 'load-file-name) load-file-name))

(defcustom template-default-directories
  (cons (if (and (not (file-directory-p "~/.templates/"))
		 (file-directory-p "~/lib/templates"))
	    (expand-file-name "~/lib/templates/")
	  (expand-file-name "~/.templates/"))
        (let ((dir (if (fboundp 'locate-data-directory) ; XEmacs
                       (locate-data-directory "template")
                     (when template-load-file-name
                       (expand-file-name
                        "templates/"
                        (file-name-directory template-load-file-name))))))
          (and dir (list dir))))
  "*List of default directories for template files.
See `template-derivation-alist' for details."
  :group 'template-derivation
  :type '(repeat directory))

(defcustom template-derivation-alist
  '(;;(("00readme" "" ".txt" "\\`00") . ("00readme" "" ".txt"))
    ((t "" t))
    ((t nil null) . (nil nil t 1))
    (("TEMPLATE" "" t)))
  "Alist for template file name derivation and file name refinement.
Template derivation searches for the most specific readable template
file.  By default, files with the same RAW part as the name of the new
file are considered to be more specific than files with just the same
EXT part.  Also files in the same directory are considered to be more
specific than files in their parent directory or any default template
directory.  This behavior can be changed by this alist.

Each FORM in this alist has the form (TEMPLATE . REFINEMENT).  If
TEMPLATE matches, we have found a valid template file and the
corresponding REFINEMENT is used for the file name refinement.

Before the derivation, the given file name is split into the directory
part DIR, the file name without directory FILE, and the raw part RAW of
FILE, the numbering NUM and the extension EXT.  The result is stored in
`template-file'.

TEMPLATE can have the form (FUNCTION ARG...).  If TEMPLATE matches,
FUNCTION, called with arguments ARGs, should return the split template
file name, see `template-split-filename'.

TEMPLATE can also have the form (T-RAW T-NUM T-EXT F-REGEXP) where all
elements are optional, i.e., have value nil as default.  For TEMPLATE to
match, all conditions T-RAW, T-NUM and T-EXT must be met and F-REGEXP,
if non-nil, should match FILE, the non-directory part of the given file
name.  If a condition is a string, the corresponding part of the
template file must be equal to it.  If t, the part must be equal to
RAW/NUM/EXT of the given file name.  If nil, any value will do it.  Any
other value acts like t when the part of the given file name is
non-empty, as nil otherwise.

REFINEMENT can have the form (FUNCTION ARG...).  FUNCTION, called with
the list of the split template filename and ARGs as arguments, should
set `template-file' if the file name should be refined.

REFINEMENT can also have the form (F-RAW F-NUM F-EXT AUTO-NUM) where all
elements are optional, i.e., have value nil as default.  If F-RAW, F-NUM
and F-EXT are non-nil, they change RAW/NUM/EXT of `template-file'.  A
string will be used as the new part.  If t, the corresponding part of
the template name will be used.

We will use auto numbering in the following two cases: if NUM is
non-empty and the file exists already, or if NUM is empty and AUTO-NUM
is non-nil.  Auto numbering looks at the file names in DIR to generate
the next unique number which is at least as high as NUM in the first
case and AUTO-NUM in the second.

Let us use parts of the default value as examples:

Use a template with the same RAW part of the given file name and the
same EXT part if provided, e.g., for \"exercise2\" use template
\"exercise.tex.tpl\".  Refine file name to use the extension of the
template file, also use auto numbering, e.g., if files \"exercise2.tex\"
and \"exercise3.tex\" exist, refine name to \"exercise4.tex\":
  ((t nil null) \. (nil nil t 1))

For a file with extension EXT, use TEMPLATE.EXT:
  ((\"TEMPLATE\" \"\" t))

We could define: If the given file name starts with \"00\", use template
\"00readme.txt.tpl\".  Refine file name to \"00readme.txt\":
  ((\"00readme\" \"\" \".txt\" \"\\\\`00\") \. (\"00readme\" \"\" \".txt\"))

Since more than one template file could meet this conditions, the
template derivation searches for first readable file with extension
`template-extension' which is found by the following algorithm:

  forall FORMs in `template-derivation-alist' do
    for directory BASE from DIR
           while not stopped according to `template-stop-derivation' do
      forall subdirectories DIRs in `template-subdirectories'
             relative to BASE do
        forall TEMPLATEs in DIR do
          if check_form (FORM, FULL, TEMPLATE) return TEMPLATE
    forall directories DIRs in `template-default-directories' do
      forall TEMPLATEs in DIR do
        if check_form (FORM, FULL, TEMPLATE) return TEMPLATE
  if not used via `template-auto-insert'
    forall TEMPLATEs in `template-default-directories'
           where name_nondir (TEMPLATE) = \"DEFAULT.tpl\" do
      if readable (TEMPLATE) return TEMPLATE
    return TEMPLATE in first (`template-default-directories')
           where name_nondir (TEMPLATE) = \"DEFAULT.tpl\""
  :group 'template-derivation
  :type '(repeat (cons :format "%v"
		       (sexp :tag "Derivation" :value ("TEMPLATE" nil t))
		       (sexp :tag "Refinement" :value nil))))


;;;===========================================================================
;;;  Templates: expanding templates
;;;===========================================================================

(defcustom template-confirm-insecure t
  "*Non-nil means, ask whether to use insecure template expansions.
Only set this to nil if you ALWAYS check template files before using
it!"
  :group 'template-expansion
  :type 'boolean)

(put 'template-confirm-insecure 'risky-local-variable t)

(defcustom template-message-buffer "*Template Message*"
  "If non-nil, name of buffer where messages are shown.
The following messages will be displayed in definition sequence before
the expansion has taken place:
 - :before messages, see `template-definition-start',
 - user defined prompts if `template-message-prompt-format' is non-nil,
 - user defined registers when there has been a :before message before,
   see `template-message-register-format'.

The following messages will be displayed in definition sequence after
the expansion has taken place:
 - :after messages, see `template-definition-start',
 - user defined registers, display them at point if the value of this
   variable is nil, see `template-message-register-format'."
  :group 'template-miscellaneous
  :type '(choice (const :tag "None" nil)
		 (string :tag "Buffer Name")))

(defcustom template-message-prompt-intro
  "Template expansion will ask for input with the following prompts:"
  "Default intro message used before listing user defined prompts.
Used with :before messages, see `template-message-prompt-format'."
  :group 'template-miscellaneous
  :type '(choice (const :tag "None" nil)
		 (string :tag "Intro text")))

(defcustom template-message-prompt-format "   %s"
  "If non-nil, format string for user defined prompts.
If non-nil and `template-message-buffer' is non-nil, user defined
prompts will be listed before starting the expansions.  Prompts can be
defined as specified in the docstring of `template-definition-start'.
For each PROMPT, this format string will be used with substitution
PROMPT/%s.

If no :before message has been defined before, use, if non-nil,
`template-message-prompt-intro' as the first :before message."
  :group 'template-miscellaneous
  :type '(choice (const :tag "No prompt" nil)
		 (string :tag "Format string")))

(defcustom template-message-register-intro
  "Template has defined the following registers:"
  "Default intro message used before listing user defined prompts.
Used with :after messages, see `template-message-register-format'."
  :group 'template-miscellaneous
  :type '(choice (const :tag "None" nil)
		 (string :tag "Intro text")))

(defcustom template-message-register-format "  %c:\t\"%s\"\t%s"
  "If non-nil, format string for user defined registers.
If non-nil, user defined text registers will be listed.  Registers can
be defined as specified in the docstring of `template-definition-start'.
For each register CHAR with contents CONTENTS and optional comment
COMMENT, this format string will be used with substitution CHAR/%c,
CONTENT/%s and \(COMMENT/%s or \"\"/%s).

The list of register definitions will be displayed:
 - at point if `template-message-buffer' is nil,
 - with :before messages if there has been at least one :before message
   defined before and if `template-message-buffer' is non-nil,
 - with :after messages if `template-message-buffer' is non-nil.
   If no :after message has been defined before, use, if non-nil,
   `template-message-register-intro' as the first :after message."
  :group 'template-miscellaneous
  :type '(choice (const :tag "No register content" nil)
		 (string :tag "Format string")))

(defcustom template-message-timeout 600
  "*Maximum duration the temporary message will be displayed at point.
Any user event will also make the temporary message disappear.  The
temporary message uses face in `template-message-face'."
  :group 'template-miscellaneous
  :type 'integer)

(put 'template-message-timeout 'template-secure-value #'integerp)

(defcustom template-date-format "%d %b %Y"
  "*Date/time format used with the expansion form (>>>DATE<<<).
See `template-default-expansion-alist' and `format-time-string'.  See
also `template-time-format'."
  :group 'template-expansion
  :type 'string)

(put 'template-date-format 'template-secure-value #'stringp)

(defcustom template-time-format "%T"
  "*Date/time format used with the expansion form (>>>TIME<<<).
See `template-default-expansion-alist' and `format-time-string'.  See
also `template-date-format'."
  :group 'template-expansion
  :type 'string)

(put 'template-time-format 'template-secure-value #'stringp)

(defcustom template-string-default "%0.0S"
  "*Format string used for non-string variable extensions.
If SYMBOL in (\"KEY\" \. SYMBOL) is not a string, use string with
substitution SYMBOL/%S.  Default value \"%0.0S\" causes to print
nothing.  See `template-definition-start'."
  :group 'template-expansion
  :type 'string)

(put 'template-string-default 'template-secure-value #'stringp)

(defcustom template-expansion-format "(>>>%s<<<)"
  "Format string for expansion forms.
Is a expansion form with substitution KEY/%s.  The value should
correspond with `template-expansion-regexp'.  Used by
`template-insert-form'."
  :group 'template-expansion
  :type 'string)

(put 'template-expansion-format 'template-secure-value #'stringp)

(defcustom template-expansion-regexp "(>>>\\([-A-Za-z0-9_]+\\)<<<)"
  "Regexp matching strings which are replaced by their expansions.
The first regexp group contains the KEY used by the per-template
expansion, see `template-definition-start' and the global expansions in
`template-expansion-alist' and `template-default-expansion-alist'.  The
value should correspond with `template-expansion-alist'.

If there is no defined expansion for the key, ask the user for a
replacement, see `template-read'.  If the key is matched by
`template-register-regexp', store buffer position in register, see
`template-register', .

If you want to use a text literally which is matched by this regexp, use
the zero expansion form (>>>ZERO_FORM<<<)."
  :group 'template-expansion
  :type 'regexp)

(put 'template-expansion-regexp 'template-secure-value #'stringp)

(defcustom template-literal-environment '("LITERAL" . "/LITERAL")
  "Environment for literal text in template.
Looks like (OPEN . CLOSE).  Text between expansion forms with keys OPEN
and CLOSE is not expanded.  If you change OPEN, you should change key
\"LITERAL\" in `template-default-expansion-alist' accordingly."
  :group 'template-expansion
  :type '(cons (string :tag "Open tag") (string :tag "Close tag")))

(defcustom template-register-regexp "\\`[0-9]\\'"
  "*Regexp matching keys for storing point positions in registers.
These keys use function `template-register' as the default expansion
instead of function `template-read'.  The register in which position is
stored is the one identified by the last character in the matched
string.  This regexp should usually match only strings with length 1,
and its value is therefore specified like \"\\\\`[...]\\\\'\".

If a register is used twice, it is marked by a \"*\" in the echo area
after the expansion.  See also `template-expansion-regexp'."
  :group 'template-expansion
  :type 'regexp)

(put 'template-register-regexp 'template-secure-value #'stringp)

(defcustom template-expansion-alist nil
  "User defined expansions forms.
Predefined expansion forms for `template-expansion-regexp'.  Each entry
has the form (KEY . SEXP).  These expansion forms shadow those in
`template-default-expansion-alist' and are shadowed by those in the
per-template definition section.  See `template-definition-start'."
  :group 'template-expansion
  :type '(repeat (cons :format "%v"
		       (string :tag "Key" :value "")
		       (repeat :tag "Evaluate all" sexp))))

(put 'template-expansion-alist 'risky-local-variable t)

(defvar template-default-expansion-alist
  '(("POINT" (setq template-point (point-marker))) ; point
    ("MARK" (setq template-mark (point-marker))) ; mark
    ("DIR" (insert (car template-file))) ; directory
    ("PACKAGE" (template-insert-package "."))
    ("FILE" (insert (cadr template-file))) ; file name without directory
    ("FILE_SANS" (insert (nth 2 template-file)
			 (nth 3 template-file)))
    ("FILE_RAW" (insert (nth 2 template-file))) ; raw file name without number
    ("FILE_NUM" (insert (nth 3 template-file))) ; number
    ("FILE_UPCASE" (insert (upcase (nth 2 template-file))
			   (nth 3 template-file)))
    ("FILE_EXT" (or (string= (nth 4 template-file) "")	; extension
		    (insert (substring (nth 4 template-file) 1))))
    ("DATE" (template-insert-time template-date-format))
    ("TIME" (template-insert-time template-time-format))
    ("VC_DATE" (set-time-zone-rule "UTC")
     (template-insert-time "%Y/%m/%d %T" "0000/00/00 00:00:00")
     ;; using saved `current-time-zone' doesn't work, but nil does
     (set-time-zone-rule nil))
    ("YEAR" (template-insert-time "%Y" "0000"))
    ("ISO_DATE" (template-insert-time "%Y-%m-%d" "0000-00-00"))
    ("COMMENT" (template-read "Initial comment: ")) ; comment
    ("AUTHOR" (insert (or user-mail-address	; author
			  (and (fboundp 'user-mail-address)
			       (user-mail-address))
			  (concat (user-login-name) "@" (system-name)))))
    ("USER_NAME" (insert (or (and (boundp 'user-full-name) ; user name
				  user-full-name)
			     (user-full-name))))
    ("LOGIN_NAME" (insert (user-login-name)))	; login name
    ("HOST_ADDR" (insert (or (and (boundp 'mail-host-address) ; host address
				  (stringp mail-host-address)
				  mail-host-address)
			     (system-name))))
    ("LITERAL" (if (search-forward (format template-expansion-format
					   (cdr template-literal-environment))
				   nil 'limit)
		   (delete-region (match-beginning 0) (match-end 0))))
    ("ZERO_FORM"))			; zero form
  "Predefined default expansions forms.
Predefined expansion forms for `template-expansion-regexp'.  Each entry
has the form (KEY . SEXP).  These expansion forms are shadowed by those
in `template-expansion-alist' and by those in the per-template
definition section.  See `template-definition-start'.

The default predefined expansion forms are --default is inserting--:
  (>>>POINT<<<)       set point
  (>>>MARK<<<)        set mark, jump to it with \\[exchange-point-and-mark]
  (>>>DIR<<<)         directory: /home/clstaff/wedler/lib/
  (>>>PACKAGE<<<)     package path, i.e. path from template directory
      to file directory, separated by dots instead slashes
  (>>>FILE<<<)        file w/o directory: text1.txt
  (>>>FILE_SANS<<<)   file name w/o extension: text1
  (>>>FILE_RAW<<<)    raw file name: text
  (>>>FILE_NUM<<<)    number in name: 1
  (>>>FILE_EXT<<<)    extension: txt
  (>>>FILE_UPCASE<<<) upcase file name w/o extension: TEXT1
  (>>>DATE<<<)        date using `template-date-format': 11 Jan 1999
  (>>>TIME<<<)        time using `template-time-format': 11:58:49
  (>>>YEAR<<<)        the year: 1999
  (>>>ISO_DATE<<<)    ISO 8601 date: 1999-01-11
  (>>>VC_DATE<<<)     UTC date/time for vc: 1999/01/11 10:58:49
  (>>>COMMENT<<<)     ask user for initial comment
  (>>>AUTHOR<<<)      author, i.e., `user-mail-address'
  (>>>USER_NAME<<<)   user name: Christoph Wedler
  (>>>LOGIN_NAME<<<)  login name: wedler
  (>>>HOST_ADDR<<<)   Host address: fmi.uni-passau.de
  (>>>LITERAL<<<)     literal text up to (>>>/LITERAL<<<)
  (>>>ZERO_FORM<<<)   zero form, i.e., insert nothing.  Useful to insert
      a text part matched by `template-expansion-regexp' literally.

There are aliases with one-letter keys, see `template-key-alias-alist'.

It is useful to follow the following conventions: upper case keys for
predefined extensions, lower case and digits for per-template and the
following default expansions:
  (>>>0<<<) to (>>>9<<<)  set registers 0 to 9, jump to it with
      \\[jump-to-register] 0 etc., see `template-register-regexp'
  (>>>x<<<) where x is any unused letter sequence: ask user.")

(put 'template-default-expansion-alist 'risky-local-variable t)

(defvar template-key-alias-alist
  '(("P" . "POINT")
    ("M" . "MARK")
    ("D" . "DIR")
    ("F" . "FILE")
    ("R" . "FILE_RAW")
    ("N" . "FILE_NUM")
    ("B" . "FILE_UPCASE")
    ("E" . "FILE_EXT")
    ("T" . "DATE")
    ("V" . "VC_DATE")
    ("Y" . "YEAR")
    ("I" . "ISO_DATE")
    ("C" . "COMMENT")
    ("A" . "AUTHOR")
    ("U" . "USER_NAME")
    ("L" . "LOGIN_NAME")
    ("H" . "HOST_ADDR")
    ("Z" . "ZERO_FORM"))
  "Alist to support the old one-letter predefined expansion forms.
Used for `template-expansion-alist' and
`template-default-expansion-alist'.")

(defcustom template-definition-start
  ">>>TEMPLATE-DEFINITION-SECTION<<<"
  "Header for the per-template definition section.
The region following the the first match of this string defines the
per-template definition section.  The region will be deleted before the
actual expansion, see `template-new-file'.  If you use the \"Local
Variables:\" section, define it before this region.

The definition section defines expansion forms for strings KEYs matched
by `template-expansion-regexp' which might shadow those in
`template-expansion-alist' and `template-default-expansion-alist':

  (\"KEY\"): zero form, same as (>>>ZERO_FORM<<<) in default value of
`template-default-expansion-alist', useful for inserting text matched by
`template-expansion-regexp' literally.

  (\"KEY\". CHAR): CHAR is the register where the current buffer
position is stored, see `template-register-regexp'.

  (\"KEY\" \"PROMPT\" \"PREFIX\" \"SUFFIX\" \"DEFAULT\" POLICY) where
the last four arguments are optional: ask user with PROMPT for a STRING.
If STRING is not \"\", insert PREFIX STRING SUFFIX, otherwise DEFAULT.
If POLICY is `immediately', ask immediately before starting the
expansion.  If POLICY is nil, ask with the first occurrence.  If POLICY
is `expand', ask with every occurrence and try to find expansion forms
in PREFIX, SUFFIX and DEFAULT.  If POLICY has any other value, just ask
with every occurrence.  To define, use \\[template-define-prompt].

  (\"KEY\" \"PROMPT\" (\"ANSWER\" \. \"TEXT\")...): ask user with PROMPT
for an input with completion over all ANSWERs and insert corresponding
TEXT.  Expansion forms in TEXT will be expanded.

  (\"KEY\" \"PROMPT\" (t \. \"TEXT-y\")  (nil \. \"TEXT-n\")): ask user
with PROMPT a \"y or n\" question with `y-or-n-p' and insert TEXT-y or
TEXT-n, correspondingly.  Expansion forms in TEXT-X will be expanded.
The y-case and the n-case are optional and can be exchanged.

  (\"KEY\" \. SYMBOL): insert value of SYMBOL; if value is no string at
the time of the replacement, use `template-string-default' as format
string for SYMBOL.

  (\"KEY\" COMMAND \. PREFIX): COMMAND is a symbol or a vector and is
called with `command-execute' after setting `prefix-arg' to PREFIX, not
evaluated.  If COMMANDs symbol property `template-secure-command' is
nil, the form is insecure.  If that symbol property is a function, it is
called with PREFIX to check whether COMMAND could be called directly
with PREFIX as remaining arguments.

  (\"KEY\" SEXPR...): evaluate SEXPR during the expansion, see
`template-expansion-alist' for examples.  This form is insecure.

There are other per-template definitions:

  \"MESSAGE\": additional line displayed at point until first user event
or after `template-message-timeout' seconds.  The lines are displayed
with face in `template-message-face'.  With active form selector
:before, define a message which is displayed in
`template-message-buffer' before the exansion has started.  With active
form selector :after, define a message which is displayed in
`template-message-buffer' after the exansion has taken place.  To
define interactively, use \\[template-define-message].

  (CHAR \. \"CONTENTS\"): Set register CHAR to have contents CONTENTS.
CONTENTS can then be inserted into a buffer with \\[insert-register] CHAR.

  (CHAR \"CONTENTS\" COMMENT) where COMMENT is optional: Set register
CHAR to have contents CONTENTS.  CONTENTS can then be inserted into a
buffer with \\[insert-register] CHAR.  Also display an additional line
at point to show the contents with COMMENT.  To define, use
\\[template-define-register].

The following forms depend on the active form selector which is the last
of the following expansion forms:
 - :before: \"MESSAGE\" will be displayed before the expansion
 - :after: \"MESSAGE\" will be displayed after the expansion
 - :eval-before: execute COMMAND and SEXPR before expansion
 - :eval-after: execute COMMAND and SEXPR after expansion
 - nil, deprecated: with the first form, the active form selector is
   :eval-before, with the second, it is :eval-after.

  (VARIABLE . VALUE): set SYMBOL's local value to VALUE, not evaluated.
This form is only secure if VARIABLE has a symbol property
`template-secure-value' which returns non-nil when applied to VALUE, not
evaluated.  This form is useful for variables which determine the
expansion, like `template-time-format' and `template-date-format'.  For
local variables in your new file, use the normal way via the \"Local
Variables:\" section.  The active form selector must not be :eval-before
or :eval-after.

  COMMAND: COMMAND is a symbol or a vector and is called with
`command-execute' before the expansion with form selector :eval-before,
and after the expansion with form selector :eval-after.  If COMMANDs
symbol property `template-secure-command' is nil, the form is insecure.
You should use the safe command `normal-mode' in the pre-expansion forms
if the expansion forms depend on the correct major mode.

  SEXPR: evaluate SEXPR before the expansion with form selector
:eval-before, and after the expansion with form selector :eval-after.
This form is insecure.

If any insecure forms have been used, the user of the template will be
asked whether to use the template, see `template-confirm-insecure'."
  :group 'template-expansion
  :type 'string)



;;;;##########################################################################
;;;;  Commenting
;;;;##########################################################################


(defunx template-point-at-bol (&optional count)
  :emacs-only line-beginning-position
  :xemacs-only point-at-bol)

(defunx template-point-at-eol (&optional count)
  :emacs-only line-end-position
  :xemacs-only point-at-eol)

(defunx template-char-or-char-int-p (object)
  :emacs-only integerp
  :xemacs-only char-or-char-int-p)

(defunx template-char-or-int-to-char (object)
  :emacs-only identity
  "Convert character or integer OBJECT into the equivalent character."
  (if (characterp object) object (int-to-char object)))


;;;===========================================================================
;;;  Main functions
;;;===========================================================================

;;;###autoload
(defun template-single-comment (&optional arg)
  "Decorate the comment in the current line with dashes and alike.
The line must be a comment-only line or must contain a comment ending by
eol.  That is, jump to the end of the current line and insert the dashes
and the final comment end-string up-to the fill position.  Prefix
argument ARG and `template-comment-specification' determines the comment
style to use.  The length of the resulting line is determined by
`template-max-column' and `template-max-column-with-end'."
  (interactive "*P")
  (let* ((orig (point-marker))
	 (syntax0 (and comment-start comment-start-skip
		       (condition-case nil
			   (template-comment-syntax orig 'boc)
			 (error nil))))
	 (syntax (cond ((cdr syntax0)
			(template-comment-syntax orig))
		       (syntax0
			(condition-case nil
			    (template-comment-syntax (point-marker))
			  (error syntax0)))
		       (t
			(back-to-indentation)
			nil)))
	 (sep (template-comment-separator-regexp syntax))
	 (end (template-point-at-eol))
	 old)
    (save-excursion
      (cond ((re-search-forward sep end t)
	     ;; with sep in current line
	     (setq old (buffer-substring (match-beginning 1) (match-end 1)))
	     (delete-region (match-beginning 0) (match-end 0)))
	    ((cdr syntax)		; with start-end comment
	     (if (looking-at (concat "[ \t]*\\(.+\\)?"
				     (regexp-quote (cadr syntax))
				     "[ \t]*\\(.+\\)?$"))
		 (if (or (match-beginning 1) (match-beginning 2))
		     (error "This line contains non-separator chars and %S"
			    (cadr syntax))
		   ;; Delete comment-end.  Don't delete its first char if it is
		   ;; the same as the second of comment-start.
		   (delete-region (if (and (= (length (car syntax)) 2)
					   (= (length (cadr syntax)) 2)
					   (eq (aref (car syntax) 1)
					       (aref (cadr syntax) 0)))
				      (1+ (match-beginning 0))
				    (match-beginning 0))
				  (match-end 0))))
	     (goto-char (cddr syntax))
	     (if (re-search-forward sep end t)
		 ;; sep in line between comment-start and point-at-eol
		 (setq old (buffer-substring (match-beginning 1)
					     (match-end 1)))))))
    (template-insert-separator
     (car (template-comment-specification arg old syntax))
     nil (cadr syntax))))
(put 'template-single-comment 'template-secure-command t)

;;;###autoload
(defun template-block-comment (&optional arg)
  "Decorate the current block of comment-only lines with dashes and alike.
That is, surround the the contiguous comment-only lines around point
with extra lines containing dashes and alike and to put the correct
number of newlines around the block.

Barf if the comment syntax at point has a non-empty `comment-end' or if
point is not in a comment-only line.

A block comment consists of all neighboring lines which start with
spaces and `comment-start'.  If `comment-start' is a string of length 1,
the number of repetitions of `comment-start' must be the same or larger
than in the line where the command is invoked from, too.

Prefix argument ARG and `template-comment-specification' determines the
comment style to use.  The length of the separator line is determined by
`template-max-column'.

This command can also be used with point in an empty line after a block
comment.  A second invocation of this command directly after a
successful invocation deletes the remaining empty lines from the current
line on."
  (interactive "*P")
  (let* ((orig (point-marker))
	 (syntax (progn
		   (end-of-line)
		   (skip-chars-backward " \t\n\f")
		   (template-comment-syntax orig))))
    (when (cdr syntax)
      (goto-char orig)
      (error "Command only works with comments terminated by end-of-line"))

    (if (and (eq last-command 'template-block-comment-success)
	     (looking-at "[ \t]*$"))
	(template-insert-newline "" nil (1- (template-point-at-bol)))
      (let* ((prefix (concat "[ \t]*" (regexp-quote (car syntax))))
	     (sepline (concat prefix "[ \t]*"
			      (template-comment-separator-regexp syntax)))
	     old block-beg block-end def)
	;; go to the first line with same comment prefix ---------------------
	(beginning-of-line)
	(while (and (not (bobp)) (looking-at prefix))
	  (beginning-of-line 0))
	(or (looking-at prefix) (beginning-of-line 2))
	(while (looking-at sepline)
	  (setq old (buffer-substring (1- (match-end 0)) (match-end 0)))
	  (kill-line 1))
	(setq block-beg (point-marker))
	;; go to the last line with same comment prefix ----------------------
	(while (looking-at prefix)
	  (template-indent-according-to-mode)
	  (beginning-of-line 2))
	(if (eobp) (newline))
	(setq block-end (copy-marker (point) t))
	(while (progn (forward-line -1) (looking-at sepline))
	  (setq old (buffer-substring (1- (match-end 0)) (match-end 0)))
	  (kill-line 1))
	;; insert separator lines --------------------------------------------
	(goto-char block-beg)
	(set-marker block-beg nil)
	(back-to-indentation)
	(setq def (template-comment-specification arg old syntax))
	(beginning-of-line)
	(template-insert-newline (cadr def))
	(template-insert-separator (car def) (car syntax) (cadr syntax))
	(goto-char block-end)
	(set-marker block-end nil)
	(template-insert-separator (car def) (car syntax) (cadr syntax))
	(template-insert-newline (caddr def)
				 (and (cadddr def)
				      (save-excursion
					(forward-line (cadddr def))
					(point))))
	(setq this-command 'template-block-comment-success)))
    (template-indent-according-to-mode)
    (back-to-indentation)))
(put 'template-block-comment 'template-secure-command t)


;;;===========================================================================
;;;  Check comment start, return specification
;;;===========================================================================

(defun template-indent-according-to-mode ()
  "Indent line according to `template-comment-indent'."
  (if (if template-comment-indent
	  (not (memq major-mode template-indent-mode-disable-list))
	(memq major-mode template-indent-mode-enable-list))
      (indent-according-to-mode)))

(defun template-default-comment ()
  "Return default comment according to current position."
  (if comment-start
      (substring comment-start 0 (string-match "[ \t]\\'" comment-start))
    (if (eolp) "#"
      (let ((default (buffer-substring (point) (1+ (point)))))
	(if (string-match "[A-Za-z]" default) "#" default)))))

(defun template-comment-at-point ()
  "Return the comment syntax at the current position.
Return nil, if no commenting command can be used, i.e., if point is not
in a comment-only line.  Return `none' if the `major-mode' doesn't
define a comment syntax.  Return `delimited' if point is between
`comment-start' and a non-empty `comment-end'.  Return `single' if point
is in a comment line where the comment syntax has a empty `comment-end',
return `block' if point is in an empty line after such a comment line."
  (if (and comment-start comment-start-skip)
      (save-excursion
	(let ((orig (point)))
	  (condition-case nil
	      (progn
		(end-of-line)
		(skip-chars-backward " \t\n\f")
		(if (cdr (template-comment-syntax orig t)) 'delimited
		  (if (< (template-point-at-eol) orig) 'block 'single)))
	    (error
	     (condition-case nil
		 (progn
		   (goto-char orig)
		   (beginning-of-line)
		   (when (re-search-forward comment-start-skip
					    (template-point-at-eol) t)
		     (goto-char (or (match-end 1) (match-beginning 0)))
		     (unless (or (cdr (template-comment-syntax orig 'boc))
				 (< (template-point-at-eol) orig))
		       'cont)))
	       (error nil))))))
    'none))

(defun template-comment-syntax (orig &optional no-indent)
  "Return the comment syntax at ORIG.  Signal error if not in comment.
Return (COMMENT-START) if the comment syntax has an empty `comment-end'.
Return (COMMENT-START COMMENT-END . START-POS) if the comment syntax has
a non-empty `comment-end' where START-POS is the position of the first
character inside the comment.  Move point to first character after the
comment start or the first non-whitespace character on this line.

ORIG should be the same as `point' or in a empty line after `point'.

If optional argument NO-INDENT is nil, indents the current line
according to `template-comment-indent' and `indent-according-to-mode'.
If NO-INDENT is `boc', move point to the beginning of the comment.

COMMENT-START is stripped off its final spaces, COMMENT-END off its
initial spaces."
  (unless (and comment-start comment-start-skip)
    (error "No comment syntax has been defined for %s" major-mode))
  (if (eq no-indent 'boc)
      (progn
	(beginning-of-line)
	(if (re-search-forward comment-start-skip (template-point-at-eol) t)
	    (goto-char (or (match-end 1) (match-beginning 0)))))
    (or no-indent (template-indent-according-to-mode))
    (back-to-indentation))
  (let* ((string (template-default-comment)))
    (if (string= comment-end "")
	(if (looking-at (concat (regexp-quote string)
				(and (= (length string) 1)
				     (not (eq no-indent 'boc))
				     "+")))
	    (progn
	      (goto-char (match-end 0))
	      (list (buffer-substring (match-beginning 0) (point))))
	  (let ((alist template-alt-comment-syntax-alist)
		elem c-start c-end)
	    (while alist
	      (setq elem (pop alist))
	      (and (template-match-modes-or-regexp (car elem))
		   (string-match comment-start-skip (cadr elem))
		   (setq c-start (cadr elem)
			 c-end   (caddr elem)
			 alist   nil)))
	    (template-comment-syntax-0 orig c-start c-end string)))
      (template-comment-syntax-0 orig comment-start comment-end))))

(defun template-comment-syntax-0 (orig c-start c-end &optional single)
  ;; checkdoc-params: (orig c-start c-end single)
  "Internal function for `template-comment-syntax'."
  (unless (and (stringp c-start) (stringp c-end) (not (string= c-end "")))
    (goto-char orig)
    (error "Line does not start with %S"
	   (or single (template-default-comment))))
  (setq c-start (substring c-start 0 (string-match "[ \t]\\'" c-start))
	c-end   (if (string-match "\\`[ \t]+" c-end)
		    (substring c-end (match-end 0))
		  c-end))
  (cond ((looking-at (regexp-quote c-start))
	 (goto-char (match-end 0))
	 (cons c-start (cons c-end (point))))
	((save-excursion
	   (when (re-search-backward (concat "^[ \t]*" (regexp-quote c-start))
				     nil t)
	     (goto-char (match-end 0))
	     (let ((match (point)))
	       (unless (when (search-forward c-end orig t)
			 (skip-chars-forward " \t")
			 (< (point) orig))
		 (cons c-start (cons c-end match)))))))
	(t
	 (goto-char orig)
	 (if single
	     (error "Not inside a comment (%S or %S-%S) starting in new line"
		    single c-start c-end)
	   (error "Not inside a comment (%S-%S) starting in new line"
		  c-start c-end)))))


;;;===========================================================================
;;;  Comment specification
;;;===========================================================================

;; A simple `mapconcat' is likely to slow down Emacs' regexp search algorithm
;; considerably (backtracking => near-infloop).
(defun template-comment-separator-regexp (syntax)
  "Return regexp matching separator comment lines.
The regexp also matches if the lines ends with parts of COMMENT-END in
argument SYNTAX, see `template-comment-syntax'."
  (let ((estring (cadr syntax))
	(alist template-comment-specification-alist)
	(chars nil)
	str i c)
    (while alist
      (setq str (car (pop alist)))
      (when str
	(setq i (length str))
	(while (>= (decf i) 0)
	  ;; (pushnew (aref str i) chars), but requires cl at runtime:
	  (or (memq (setq c (aref str i)) chars) (push c chars)))))
    (concat "\\("
	    (mapconcat (lambda (c) (regexp-quote (char-to-string c)))
		       (or chars "#")
		       "\\|")
	    (if estring
		(concat "\\)+[ \t]*"
			(mapconcat (lambda (c)
				     (regexp-quote (char-to-string c)))
				   estring
				   "?")
			"?[ \t]*$")
	      "\\)+[ \t]*$"))))

(defun template-comment-specification (arg old syntax)
  "Return the comment specification to use.
See `template-comment-specification-alist' for details.  ARG is the
prefix argument, OLD the SEPARATOR of the old comment style and SYNTAX
is the comment syntax returned by `template-comment-syntax'."
  (and arg (setq arg (prefix-numeric-value arg)))
  ;; assumes point-at-indentation
  (or (and arg (> arg 0)
	   (if (< (length template-comment-specification-alist) arg)
	       (car (last template-comment-specification-alist))
	     (nth (1- arg) template-comment-specification-alist)))
      (and (null arg) old
	   (assoc old template-comment-specification-alist))
      (and (functionp template-comment-specification-special)
	   (funcall template-comment-specification-special))
      (and (functionp (get major-mode 'template-comment-specification-special))
	   (funcall (get major-mode 'template-comment-specification-special)))
      (and syntax
	   (template-comment-specification
	    (if (or (cdr syntax) (> (length comment-start) 1))
		(save-excursion
		  (if (cddr syntax) (goto-char (cddr syntax)))
		  (beginning-of-line)
		  (if (looking-at "[ \t]") 2 3))
	      (length (car syntax)))
	    nil nil))
      '("-" "" "" 0)))


;;;===========================================================================
;;;  Inserting
;;;===========================================================================

(defun template-insert-newline (string &optional limit start-limit)
  "Deletes blank lines around point and insert STRING.
After optional LIMIT and before optional START-LIMIT, no character will
be deleted."
  (let ((start (save-excursion
		 (skip-chars-backward " \t\n\f" start-limit)
		 (or (bobp) (forward-line 1))
		 (point)))
	(end (save-excursion
	       (skip-chars-forward " \t\n\f" limit)
	       (beginning-of-line)
	       (point))))
    (if (> end start) (delete-region start end)))
  (or (bobp) (insert string)))

(defun template-insert-separator (separator &optional cstring estring)
  "Insert separator line at point.
If CSTRING is not nil, insert in special line which starts with CSTRING.
Insert SEPARATOR repeatedly.
If ESTRING is not nil, insert ESTRING (which is the comment end)."
  (when separator
    (when cstring
      (open-line 1)
      (insert cstring)
      (template-indent-according-to-mode))
    (end-of-line)
    (let* ((max-column (if estring
			   template-max-column-with-end
			 template-max-column))
	   (max (- (if (> max-column 0) max-column (+ fill-column max-column))
		   (length separator)
		   (length estring))))
      (while (<= (current-column) max) (insert separator))
      (if (>= (length separator) (- (current-column) max))
	  (insert (substring separator 0 (- max (current-column)))))
      (if estring (insert estring))
      (if cstring (forward-line 1)))))


;;;===========================================================================
;;;  Adaptations: cc-mode
;;;===========================================================================

;; There isn't really anything I can do against the filling of "/**" in C, C++
;; and Antlr mode (it is correct in Java), it should be done in the cc-mode
;; package.  Similar for filling "*/" with the previous line...

(defun template-c-fill-paragraph (&optional arg)
  ;; checkdoc-params: (arg)
  "Like \\[c-fill-paragraph] but handles comment separator lines."
  (let* ((regexp (concat "\\|[ \t]*\\(/[*/]\\|\\*\\)[ \t]*"
			 (template-comment-separator-regexp '("/*" "*/"))))
	 (paragraph-start (concat paragraph-start regexp)) ;#dynamic
	 (paragraph-separate (concat paragraph-separate regexp)))
    (c-fill-paragraph arg)))

(defun template-c-init-fill-function ()
  "Set `fill-paragraph-function' to use `template-c-fill-paragraph'."
  (when (boundp 'fill-paragraph-function)
    (make-local-variable 'fill-paragraph-function)
    (setq fill-paragraph-function 'template-c-fill-paragraph)))



;;;;##########################################################################
;;;;  Updating (File Name in Header)
;;;;##########################################################################


;;;===========================================================================
;;;  General updating
;;;===========================================================================

(defun template-update-buffer-region (limit regexp group)
  "Return region = (BEG . END) in buffer to be updated.
If LIMIT is positive, check first LIMIT characters in buffer, otherwise
check last -LIMIT characters in buffer for a text to be matched by
REGEXP.  Return region according to GROUP's regexp group in REGEXP."
  (let ((case-fold-search nil))
    (goto-char (if limit
		   (if (natnump limit) (point-min) (+ (point-max) limit))
		 (point-min)))
    (when (re-search-forward regexp
			     (if (natnump limit)
				 (+ (point-min) limit)
			       (point-max))
			     t)
      (cons (match-beginning group) (match-end group)))))

(defun template-match-modes-or-regexp (modes-or-regexp)
  "Return non-nil, if the current buffer passes MODES-OR-REGEXP.
If MODES-OR-REGEXP is a list, it must include the current `major-mode',
if it is a regexp, it must match variable `buffer-file-name' without
version, otherwise it must be non-nil."
  (if (stringp modes-or-regexp)
      (and buffer-file-name
	   (string-match modes-or-regexp
			 (file-name-sans-versions buffer-file-name)))
    (or (nlistp modes-or-regexp) (memq major-mode modes-or-regexp))))

(defun template-update-buffer (&optional arg)
  "Update buffer according to `template-update-buffer-alist'.
Do not do anything if `template-auto-update-disable-regexp' matches the
file name or if `template-auto-update' or `template-use-package' is nil.
When optional ARG is non-nil, i.e., if called interactively *without*
prefix arg, always update."
  (interactive (list (null current-prefix-arg)))
  (when (or arg
	    (and template-auto-update template-use-package
                 buffer-file-name
		 (null (and template-auto-update-disable-regexp
			    (string-match template-auto-update-disable-regexp
					  buffer-file-name)))))
    (save-excursion
      (save-restriction
	(widen)
	(let ((alist template-update-buffer-alist)
	      (case-fold-search (memq system-type '(vax-vms ms-dos windows-nt)))
	      stamp prompt region new)
	  (while alist
	    (setq stamp (pop alist))
	    (condition-case nil
		(and (template-match-modes-or-regexp (pop stamp))
		     ;; Run TEST ---------------------------------------------
		     (setq prompt (pop stamp)
			   region (pop stamp) ; TEST
			   region (eval (if (or (atom region)
						(functionp (car region)))
					    region
					  (cons 'template-update-buffer-region
						region))))
		     (if (stringp (setq new (eval (pop stamp))))
			 (null (string= (buffer-substring (car region)
							  (cdr region))
					new))
		       (car stamp))
		     ;; user confirmation, replacement -----------------------
		     (or (null prompt)
			 arg
			 (eq template-auto-update t)
			 (y-or-n-p (format prompt (buffer-name))))
		     (progn
		       (goto-char (car region))
		       (if (car stamp)
			   (funcall (car stamp) new (car region) (cdr region))
			 (delete-region (car region) (cdr region))
			 (insert new))))
	      (error nil))))))))


;;;===========================================================================
;;;  Update header
;;;===========================================================================

;;;###autoload
(defun template-update-header (&optional show)
  "Replace old file name in header with current file name.
If SHOW is t, just return region of the filename or nil.  Otherwise,
replace filename if possible and signal an error if SHOW is nil and
there is no filename in the header.  See `template-header-lines' and
`template-header-regexp-alist'."
  (interactive "*P")
  (if buffer-file-name
      (save-excursion
	(goto-char (point-min))
	(let ((case-fold-search nil)
	      (comment-regexp (template-default-comment)) ; at `point-min'!
	      (end (progn (forward-line template-header-lines) (point)))
	      (alist template-header-regexp-alist)
	      (disallowed "")
	      group)
	  (if (string-match "[A-Za-z]\\|.." comment-regexp)
	      (setq comment-regexp (regexp-quote comment-regexp)
		    disallowed "")
	    (or (eq comment-regexp '(?\]))
		(setq disallowed comment-regexp))
	    (setq comment-regexp (concat (regexp-quote comment-regexp) "+")))
	  (while alist
	    (goto-char (point-min))
	    (if (re-search-forward (format (caar alist)
					   comment-regexp disallowed)
				   end t)
		(setq group (cdar alist)
		      alist nil)
	      (setq alist (cdr alist))))
	  (if (and group (match-beginning group))
	      (if (eq show t)
		  (cons (match-beginning group) (match-end group))
		(goto-char (match-beginning group))
		(delete-region (point) (match-end group))
		(insert (file-name-sans-versions
			 (file-name-nondirectory buffer-file-name)))
		t)
	    (if show nil (error "No file name in header")))))
    (if show nil (error "Buffer is not visiting a file"))))



;;;;##########################################################################
;;;;  Templates
;;;;##########################################################################


(defvar template-history nil
  "History, used by `template-read'.")

(defvar template-choice-history nil
  "History, used by `template-choice'.")

(put 'normal-mode 'template-secure-command t)

(defvar template-all-templates nil
  "Internal variable.  Template files used for template derivation.")
(defvar template-file nil
  "Partitioned name of new file: (DIR FILE RAW NUMBER EXT).
Internal variable.  DIR is the directory part, FILE the file name
without directory part.  FILE consists of its extension EXT, RAW and a
numbering NUMBER just in front of the extension.  It is used by the
expansions DIR, FILE, FILE_SANS, FILE_EXT and others in
`template-expansion-alist'.  Also useful for user defined functions in
`template-derivation-alist' and the per-template definition section.")

(defvar template-modified nil
  "Internal variable.  Whether user is asked during the expansion process.")
(defvar template-secure t
  "Internal variable.  Whether all per-template definitions are secure.")
(defvar template-point-messages nil
  "Internal variable.  List of lines for temporary message at point.")
(defvar template-before-messages nil
  "Internal variable.  List of lines for temporary message before expansion.")
(defvar template-after-messages nil
  "Internal variable.  List of lines for temporary message after expansion.")

(defvar template-template nil
  "Internal variable.  Name of template file.  Used for expansion form PACKAGE.")
(defvar template-point nil
  "Internal variable.  Position of point.  Set with expansion form POINT.")
(defvar template-mark nil
  "Internal variable.  Position of mark.  Set with expansion form MARK.")

(defvar template-current nil
  "Internal variable.  Current key of expansion form.")
(defvar template-string-alist nil
  "Internal variable.  Alist of user inputs for `template-read'.")
(defvar template-register-alist nil
  "Internal variable.  Alist of used registers.")
(defvar template-local-alist nil
  "Internal variable.  Alist of per-template defined expansions.")

(defvar template-pre-command-list nil
  "Internal variable.  :eval-before commands and sexps.")
(defvar template-post-command-list nil
  "Internal variable.  :eval-after commands and sexps.")
(defvar template-local-variable-list nil
  "Internal variable.  Local variables to set in template expansion.")

(defvar template-ffap-file-finder nil
  "Value used inside `template-ffap-find-file'.
If nil, initialize it to the value of `ffap-file-finder', i.e., this
variable holds the original value of that variable which will be set to
`template-ffap-find-file' in function `template-initialize'.")


;;;===========================================================================
;;;  Functions: `find-file'/`insert-file-contents', hooking into `find-file'
;;;===========================================================================

(defunx template-find-template (filename &optional replace)
  "Switch to a buffer visiting template file FILENAME.
If optional REPLACE is non-nil, replace the current buffer contents with
the contents of file FILENAME.

This function always considers template files as text files."
  (let ((file-name-buffer-file-type-alist nil))	; Emacs on DOS/NT
    (if replace
        (progn
	  (insert-file-contents filename nil nil nil
				;; 5th arg not t with empty accessible part
				;; (XEmacs bug workaround: would infloop)
				(> (point-max) (point-min)))
          ;; now set the coding system to that of the template
          ;; if necessary, we can introduce a user option for this
          :EMACS
	  (set-buffer-file-coding-system
	   (set-auto-coding filename
			    (- (point-max) (point-min)))))
      (let ((template-auto-insert nil))
	(switch-to-buffer (find-file-noselect filename))))))

(defun template-not-found-function ()
  "Use a template when visiting a non-existent file.
See `template-auto-insert' and `template-find-file-commands'.  Function
in `find-file-not-found-hooks'."
  (and template-auto-insert
       template-use-package
       (not buffer-read-only) (bobp) (eobp)
       (or (memq this-command template-find-file-commands)
	   (and (memq this-command template-file-select-commands)
		;; thanks to Dave Love <d.love@dl.ac.uk>:
		(memq (car-safe (car command-history))
		      ;; To always include `find-file-at-point', use ffap
		      ;; initialization (see `template-ffap-find-file')
		      template-find-file-commands)))
       (let ((template (cdr (template-derivation buffer-file-name t t))))
	 (and template
	      (file-readable-p template)
	      (or (eq template-auto-insert t)
		  (y-or-n-p
		   (format "Use template %s? "
			   (cond-emacs-xemacs
			    (abbreviate-file-name template :XEMACS t)))))
	      (progn
		(template-new-file nil template)
		(setq this-command 'session-disable)
		t)))))

(defun template-ffap-find-file (filename)
  "Function to use in `ffap-file-finder'.
Add an entry to variable `command-history' if necessary and call
function in `template-ffap-file-finder' with argument FILENAME."
  (or (memq (car-safe (car command-history))
	    '(ffap find-file-at-point))
      (setq command-history
	    (cons (list 'find-file-at-point filename) command-history)))
  (if (eq template-ffap-file-finder 'template-ffap-find-file)
      (find-file filename)
    (funcall template-ffap-file-finder filename)))


;;;===========================================================================
;;;  Main function
;;;===========================================================================

(defun template-expand-template-interactive ()
  "Interactive specification of command `template-expand-template'."
  (let* ((use (template-derivation (expand-file-name
				    (or buffer-file-name "NONE"))
				   t))
	 (tpl (read-file-name "Insert and expand template: "
			      (file-name-directory (cdr use))
			      (file-name-nondirectory (cdr use))
			      t
			      (file-name-nondirectory (cdr use)))))
    (if (string= tpl "")
	(error "No template file provided"))
    (list (expand-file-name tpl (file-name-directory (cdr use))))))

;;;###autoload
(defun template-expand-template (template)
  "Expand template file TEMPLATE and insert result in current buffer.
Using a template for inserting some text consists of:
  1. Template derivation: suggest a reasonable template file to the user
     according to `buffer-file-name', see `template-derivation-alist'.
  2. Template insertion: insert the template file at point into the
     current buffer.
  3.. as steps 6.. of `template-new-file'."
  (interactive (template-expand-template-interactive))
  (save-restriction
    (narrow-to-region (point) (point))
    (template-new-file nil template t)))

(defun template-new-file-interactive ()
  "Interactive specification for `template-new-file'.
Return \(FILE TEMPLATE)."
  (let* ((inp (read-file-name (if current-prefix-arg
				  "New file (+template, no name change): "
				"New file (+template): ")
			      nil ""))
	 (use (cond ((equal inp "")
		     (error "Empty/no input"))
		    ((file-directory-p inp)
		     (error "%S is a directory" inp))
		    (t (template-derivation (expand-file-name inp)
					    current-prefix-arg))))
	 (tpl (read-file-name (format "File %s uses template: "
				      (file-name-nondirectory (car use)))
			      (file-name-directory (cdr use))
			      (file-name-nondirectory (cdr use))
			      t
			      (file-name-nondirectory (cdr use)))))
    (list (car use)
	  (if (string= tpl "")
	      nil
	    (expand-file-name tpl (file-name-directory (cdr use)))))))

;;;###autoload
(defun template-new-file (file template &optional with-undo)
  "Open a new file FILE by using a TEMPLATE.
Using a template for creating a new file consists of, steps 1 to 3 are
only executed when called interactively:
  1. Prompt for the name of the new file.
  2. Template derivation: suggest a reasonable template file to the user
     see `template-derivation-alist'.
  3. File name refinement: e.g., if the given file name is \"exercise\"
     and there are two files \"exercise1.tex\" and \"exercise2.tex\" in
     the same directory and if we have a template \"exercise.tex.tpl\",
     the file name is refined to \"exercise3.tex\".  This is turned off
     when \\[template-new-file] is called with a prefix argument.
  4. Template insertion: insert the template file into the empty buffer.
  5. Read per-template expansion definition section starting at
     `template-definition-start' and delete it.
  6. Display :before message in `template-message-buffer'.
  7. Execute pre-expansion commands defined in the definition section.
  8. Set local variables defined in the definition section.
  9. Ask user for strings to insert for prompts to be executed
     immediately, see `template-definition-start'.
 10. Expansion: expand the expansion forms (text matched by
     `template-expansion-regexp') They are defined in the definition
     section, in `template-expansion-alist', or provided by default, see
     `template-expansion-regexp' and `template-register-regexp'.
 11. Execute post-expansion commands defined in the definition section.
 12. Run `normal-mode' and functions in `find-file-hooks'.
 13. Update header according to `template-update-header' with argument
    `if-exists'.
 14. Display :after message in `template-message-buffer'.
 15. Report: display a temporary message at point defined in the
     definition section and an automatically generated message in the
     minibuffer area, see `template-message-timeout'.

If optional WITH-UNDO is non-nil, store corresponding changes in
`buffer-undo-list'.  If FILE is nil, the buffer for FILE has already
been created and the accessible part will be replaced by the expanded
template.  If TEMPLATE is nil (empty input when called interactively),
do not use a template."
  (interactive (template-new-file-interactive))
  (setq template-template template)
  ;; check template and file name --------------------------------------------
  (when template
    (if (file-readable-p template)
	(if (file-directory-p template)
	    (error "Template %s is a directory" template))
      (if (null (yes-or-no-p (format "Template %s does not exist.  Create? "
				     template)))
	  (error "No template file to use")
	(template-make-directory (file-name-directory template))
	(template-find-template template)
	(error "You should create this template first"))))
  ;; switch to buffer of new file --------------------------------------------
  (if (not file)
      (switch-to-buffer (current-buffer))
    (and (or (get-file-buffer file) (file-exists-p file))
	 (null (yes-or-no-p (format "File %s exists.  Delete contents? " file)))
	 (error "Cannot use templates for existing files"))
    (let ((auto-mode-alist nil)
	  (enable-local-variables nil)
	  (find-file-not-found-hooks nil)
	  (enable-local-eval nil))
      (switch-to-buffer (find-file-noselect file))))
  ;; insert template and do expansiosn ---------------------------------------
  (when template
    (unless with-undo
      (setq buffer-undo-list t))
    (template-find-template template t)	; Step 4
    (unless with-undo
      (set-buffer-modified-p nil))
    (template-new-file-init)
    (template-new-file-definitions)	; Step 5
    (template-new-file-expansions)	; Steps 6..11
    (save-restriction
      (widen)
      (normal-mode t)			; Step 12
      (unless with-undo
	(template-update-header 'if-exists)) ; Step 13
      (run-hooks 'find-file-hooks))
    (template-new-file-messages with-undo) ; Steps 14..15
    (unless with-undo
      (setq buffer-undo-list nil)
      (set-buffer-modified-p template-modified))))

(defun template-new-file-init ()
  "Initialize internal variables used for template processing."
  (setq template-secure t
	template-point nil
	template-mark nil
	template-modified nil
	template-point-messages nil
	template-before-messages nil
	template-after-messages nil
	template-local-alist nil
	template-register-alist nil
	template-string-alist nil
	template-pre-command-list nil
	template-post-command-list nil
	template-local-variable-list nil))

(defun template-new-file-definitions ()
  "Read the template definitions from the definition section.
Step 5 in `template-new-file'.  See also `template-definition-start'."
  (goto-char (point-min))
  (when (re-search-forward (concat "^[ \t]*"
				   (regexp-quote template-definition-start)
				   "[ \t]*$")
			   nil t)
    (let ((section-start (match-beginning 0))
	  (form-selector nil))
      (condition-case ()
	  (while t
	    (let ((expansion-rule (read (current-buffer))))
	      (cond ((consp expansion-rule)
		     (template-process-cons-definition (car expansion-rule)
						       (cdr expansion-rule)
						       form-selector))
		    ((memq expansion-rule
			   '(:before :after :eval-before :eval-after))
		     (setq form-selector expansion-rule))
		    ((null expansion-rule)
		     (setq form-selector
			   (cond ((null form-selector) :old-before)
				 ((eq form-selector :old-before) :old-after)
				 ((eq form-selector :old-after)
				  (error "More than two (obsolete) nil forms"))
				 (t
				  (error "Used obsolete nil form with new form selectors")))))
		    (t
		     (template-process-definition expansion-rule form-selector)))))
	(error nil))
      (skip-chars-forward " \t\n\f")
      (or (eobp)
	  (error "Invalid definition in line %d (pos %d) of the template file"
		 (count-lines 1 (point)) (point)))
      (or template-secure
	  (null (default-value template-confirm-insecure))
	  (y-or-n-p "Have you checked the template functions? ")
	  (error "Failed security check"))
      (delete-region section-start (point-max)))))

(defun template-new-file-expansions ()
  "Perform template replacements in current buffer.
Steps 6 to 11 in `template-new-file'."
  ;; prepare ---------------------------------------------------------------
  (template-display-messages template-before-messages) ; Step 6
  (eval (cons 'progn (nreverse template-pre-command-list))) ; Step 7
  (dolist (v template-local-variable-list)
    (set (make-local-variable (car v)) (cdr v))) ; Step 8
  (goto-char (point-min))
  (dolist (entry template-local-alist)
    (and (null (cddr entry))
	 (eq (car-safe (cadr entry)) 'template-read)
	 (eval (list* 'template-read-from-minibuffer ; Step 9
		      (car entry) (cdadr entry)))))
  ;; expand ----------------------------------------------------------------
  (while (re-search-forward template-expansion-regexp nil t)
    (setq template-current		; Step 10
	  (buffer-substring (match-beginning 1) (match-end 1)))
    (let ((expansion-rule (assoc template-current template-local-alist)))
      (unless expansion-rule
	(let ((alias (assoc template-current template-key-alias-alist)))
	  (if alias (setq template-current (cdr alias))))
	(setq expansion-rule
	      (or (assoc template-current template-expansion-alist)
		  (assoc template-current template-default-expansion-alist))))
      (delete-region (match-beginning 0) (match-end 0))
      (cond (expansion-rule
	     (eval (cons 'progn (cdr expansion-rule))))
	    ((string-match template-register-regexp template-current)
	     (template-register))
	    (t
	     (template-read (format "Replacement for `%s': "
				    template-current))))))
  (eval (cons 'progn (nreverse template-post-command-list)))) ; Step 11

(defun template-new-file-messages (with-undo)
  "Display messages after template replacements in current buffer.
Steps 14 and 15 in `template-new-file'."
  (template-display-messages template-after-messages)
  (cond ((null template-register-alist)
	 (message "%s, no buffer location in register"
		  (if template-mark "Mark set" "No mark")))
	(t (message "%s, buffer location in register: %s"
		    (if template-mark "Mark set" "No mark")
		    (mapconcat (function
				(lambda (x)
				  (if (cdr x)
				      (concat (char-to-string (car x)) "*")
				    (char-to-string (car x)))))
			       (nreverse template-register-alist)
			       ", "))))
  (or with-undo (set-buffer-modified-p template-modified))
  (goto-char (point-min))
  (when template-point
    (goto-char template-point)
    (set-marker template-point nil))
  (when template-mark
    (push-mark template-mark)
    (set-marker template-mark nil)
    (if (fboundp 'zmacs-activate-region) (zmacs-activate-region)))
  (when (and template-point-messages
	     (or (cdr template-point-messages)
		 (not (string-equal (car template-point-messages) ""))))
    (let ((beg (point))
	  end)
      (if (cdr template-point-messages)
	  (insert (mapconcat 'identity
			     (nreverse template-point-messages)
			     "\n")
		  "\n")
	(insert (car template-point-messages)))
      (setq end (point))
      (goto-char beg)
      (and (fboundp 'make-extent) (fboundp 'set-extent-face)
	   (set-extent-face (make-extent beg end) 'template-message-face))
      (recenter)
      (sit-for template-message-timeout)
      (delete-region beg end)))
  (recenter))

(defun template-display-messages (messages)
  "Display MESSAGES in buffer `template-message-buffer'."
  (when (and messages template-message-buffer)
    (setq messages (nreverse messages))
    (with-output-to-temp-buffer template-message-buffer
      (while messages
	(princ (pop messages))
	(if messages (princ "\n"))))))


;;;===========================================================================
;;;  Determine name of the new file and the template
;;;===========================================================================

(defun template-derivation (full arg &optional no-default)
  "Derive template file name and do file name refinement.
Return (REFINED . TEMPLATE) where REFINED is the refined version of FULL
and TEMPLATE and template file name, see `template-derivation-alist'.
FULL is the initial file name given by the user.  File name refinement
is turned off when ARG is non-nil.  If optional argument NO-DEFAULT is
non-nil, return nil instead (FULL \. \"~/.templates/DEFAULT.tpl\") if no
matching entry can be found in `template-derivation-alist'."
  ;; Get all templates -------------------------------------------------------
  (setq template-all-templates nil)
  (let* ((dir (file-name-directory full))
	 (len (length dir))
	 (case-fold-search (memq system-type '(vax-vms ms-dos windows-nt))))
    (while (and dir
		(not (and template-stop-derivation
			  (fboundp template-stop-derivation)
			  (funcall template-stop-derivation dir))))
      (template-all-templates template-subdirectories dir)
      (setq dir (file-name-directory (directory-file-name dir)))
      (or (> len (setq len (length dir)))
	  (setq dir nil)))
    (template-all-templates template-default-directories)
    (setq template-all-templates (nreverse template-all-templates)))
  ;; Get template file -------------------------------------------------------
  (if (string= (file-name-nondirectory full) "")
      (error "You cannot use templates for directories"))
  (setq template-file (template-split-filename full))
  (let ((tests template-derivation-alist)
	test template file)
    (while tests
      (setq test (caar tests)
	    file (cdar tests))
      (if (setq template
		(if (functionp (car test))
		    (apply (car test) (cdr test))
		  (apply 'template-default-template test)))
	  (setq tests nil)
	(setq tests (cdr tests))))
    (if template
	(or arg
	    (if (functionp (car file))
		(apply (car file) template (cdr file))
	      (apply 'template-unique-file template file)))
      (or no-default
	  (setq template (template-split-filename
			  "DEFAULT"
			  (template-default-directory)))))
    (if template
	(cons (expand-file-name (cadr template-file) (car template-file))
	      (expand-file-name (concat (cadr template) template-extension)
				(car template))))))

(defun template-default-directory ()
  "Return directory of file \"DEFAULT.tpl\"."
  (let ((dirs template-default-directories)
	(name (concat "DEFAULT" template-extension))
	dir)
    (while dirs
      (setq dir (pop dirs))
      (if (file-readable-p (expand-file-name name dir))
	  (setq dirs nil)
	(setq dir nil)))
    (or dir
	(car template-default-directories)
	(expand-file-name "~/.templates/"))))


;;;===========================================================================
;;;  Small functions
;;;===========================================================================

(defun template-make-directory (dir)
  "Create DIR if it does not exists yet."
  (cond ((file-exists-p dir))
	((yes-or-no-p (format "The directory %s does not exist.  Create? " dir))
	 (make-directory dir t))
	(t (error "You should create a directory \"%s\"" dir)))
  dir)

(defun template-split-filename (file &optional dir)
  "Split file name into its parts.
If DIR is nil, FILE is a fully expanded file name, otherwise FILE is a
file name without its directory part DIR.  See `template-file'."
  (or dir (setq dir (template-make-directory (file-name-directory file))
		file (file-name-nondirectory file)))
  (let* ((ext (string-match "\\.[^.]*\\'" file))
	 (raw (substring file 0 ext))
	 (num (string-match "[^0-9][0-9]+\\'" raw)))
    (if num
	(list dir file
	      (substring raw 0 (1+ num))
	      (substring raw (1+ num))
	      (if ext (substring file ext) ""))
      (list dir file raw "" (if ext (substring file ext) "")))))


;;;===========================================================================
;;;  Process and translate definitions
;;;===========================================================================

(defun template-process-cons-definition (rule-head rule-def form-selector)
  "Process definition in per-template definition section.
Definition looks like (RULE-HEAD . RULE-DEF).  Some special definitions
depend on FORM-SELECTOR, see `template-definition-start' for details."
  (cond ((stringp rule-head)	; ("KEY" . xxx): ask user ------------
	 (let ((msg (cond ((null template-message-prompt-format)
			   nil)
			  ((stringp rule-def) rule-def)
			  ((and (consp rule-def) (stringp (car rule-def)))
			   (car rule-def)))))
	   (when msg			; list prompts in before-messages
	     (or template-before-messages
		 (null template-message-prompt-intro)
		 (push template-message-prompt-intro
		       template-before-messages))
	     (push (format template-message-prompt-format msg)
		   template-before-messages))
	   (push (cons rule-head (template-translate-definition rule-def))
		 template-local-alist)))
	;; (CHAR . xxx): set register ------------------------------
	((template-char-or-char-int-p rule-head)
	 (let ((reg (template-char-or-int-to-char rule-head)))
	   (if (atom rule-def)
	       (set-register reg rule-def)
	     (set-register reg (car rule-def))
	     (when template-message-register-format
	       (let ((msg (format template-message-register-format
				  reg (car rule-def)
				  (or (cadr rule-def) ""))))
		 (if template-message-buffer
		     (progn
		       (if template-before-messages
			   (push msg template-before-messages))
		       (or template-after-messages
			   (null template-message-register-intro)
			   (push template-message-register-intro
				 template-after-messages))
		       (push msg template-after-messages))
		   (push msg template-point-messages)))))))
	(t
	 (template-process-definition (cons rule-head rule-def) form-selector))))

(defun template-process-definition (expansion-rule form-selector)
  "Process definition in per-template definition section.
Definition looks EXPANSION_RULE.  Some special definitions depend on
FORM-SELECTOR, see `template-definition-start' for details."
  (cond ((stringp expansion-rule) ; "MESSAGE" --------------------------------
	 (cond ((eq form-selector :before)
		(push expansion-rule template-before-messages))
	       ((eq form-selector :after)
		(push expansion-rule template-after-messages))
	       (t
		(push expansion-rule template-point-messages))))
	;; set var, execute command and sexpr ----------------------
	((and (memq form-selector '(nil :before :after))
	      (consp expansion-rule)
	      (symbolp (car expansion-rule)))
	 ;; [TODO]: also use `risky-local-variable' etc?
	 (or (and (functionp (get (car expansion-rule)
				  'template-secure-value))
		  (funcall (get (car expansion-rule)
				'template-secure-value)
			   (cdr expansion-rule)))
	     (setq template-secure nil))
	 (push expansion-rule template-local-variable-list))
	((memq form-selector '(:eval-before :old-before))
	 (push (template-elisp-in-definition expansion-rule)
	       template-pre-command-list))
	((memq form-selector '(:eval-after :old-after))
	 (push (template-elisp-in-definition expansion-rule)
	       template-post-command-list))
	(t
	 (error "Illegal expansion rule"))))

(defun template-translate-definition (def)
  "Translate DEF of expansion and set `template-secure' accordingly."
  (cond ((null def) ; zero form
	 nil)
	((template-char-or-char-int-p def)
	 `((template-register ,def)))
	((stringp def)
	 `((template-read ,def nil nil nil t)))
	((symbolp def)
	 `((insert (if (stringp ,def) ,def template-string-default))))
	((and (consp def) (stringp (car def)))
	 (if (consp (car-safe (cdr def)))
	     `((template-choice ,(car def) (quote ,(cdr def))))
	   `((template-read ,@(apply 'template-translate-read def)))))
	((consp (car-safe def))
	 (setq template-secure nil)
	 def)
	(t
	 (list (template-elisp-in-definition (car def) (cdr def))))))

(defun template-translate-read (prompt &optional prefix suffix default policy
				       &rest nothing)
  "Check arguments for function `template-read' and return definition.
Optional PREFIX, SUFFIX and DEFAULT must be strings, POLICY must be a symbol
and will be quoted.  PROMPT is already checked, NOTHING must be nil."
  (unless (and (or (null prefix) (stringp prefix))
	       (or (null suffix) (stringp suffix))
	       (or (null default) (stringp default))
	       (symbolp policy)
	       (null nothing))
    (error "Illegal form"))
  (list* prompt prefix suffix default
	 (and policy (list (list 'quote policy)))))

(defun template-elisp-in-definition (def &optional prefix)
  "Return valid elisp definition and set `template-secure' accordingly.
DEF is the elisp form, PREFIX would be the prefix argument if DEF is a
command."
  (cond ((consp def)
	 (setq template-secure nil)
	 def)
	((or (symbolp def) (vectorp def))
	 (unless (and (symbolp def) (get def 'template-secure-command))
	   (setq template-secure nil))
	 (if (and (symbolp def)
		  (functionp (get def 'template-secure-command))
		  (listp prefix)
		  (funcall (get def 'template-secure-command) prefix))
	     `(apply (quote ,def) (quote ,prefix))
	   `(progn (setq prefix-arg (quote ,prefix))
		   (command-execute (quote ,def)))))
	(t
	 (error "Illegal form"))))


;;;===========================================================================
;;;  Compute template name
;;;===========================================================================

(defun template-all-templates (dirs &optional base)
  "Read names of template files in DIRS relatively to BASE.
Insert the names to internal variable `template-all-templates'."
  (let ((regexp (concat (regexp-quote template-extension) "\\'"))
	(endpos (- (length template-extension)))
	dir templates)
    (while dirs
      (setq dir (expand-file-name (car dirs) base)
            ;; TODO: really use `expand-file-name' if BASE is nil?
	    dirs (cdr dirs))
      (cond-emacs-xemacs
       (and (file-accessible-directory-p dir)
	    (file-readable-p dir)
	    (setq templates (directory-files dir t regexp :XEMACS nil t))
	    (while templates
	      (and :EMACS
		   (not (file-directory-p (car templates)))
		   :BOTH
		   (file-readable-p (car templates))
		   (push (template-split-filename (substring (car templates)
							     0
							     endpos))
			 template-all-templates))
	      (setq templates (cdr templates))))))))

(defun template-set-template-part (part file-part)
  "Set template part according to definition PART and FILE-PART.
See `template-derivation-alist' for details."
  (when part
    (cond ((stringp part) part)
	  ((eq part t) file-part)
	  ((null (string= file-part "")) file-part))))

(defun template-default-template (&optional raw num ext regexp)
  "Return template according to RAW, NUM, EXT and REGEXP.
See `template-derivation-alist' for details."
  (if (or (null regexp) (string-match regexp (cadr template-file)))
      (let ((templates template-all-templates)
	    (file-rne (cddr template-file))
	    result template-rne)
	(setq raw (template-set-template-part raw (car file-rne))
	      num (template-set-template-part num (cadr file-rne))
	      ext (template-set-template-part ext (caddr file-rne)))
	(while templates
	  (setq template-rne (cddar templates))
	  (if (and (or (null raw) (string= (car template-rne) raw))
		   (or (null num) (string= (cadr template-rne) num))
		   (or (null ext) (string= (caddr template-rne) ext)))
	      (setq result (car templates)
		    templates nil)
	    (setq templates (cdr templates))))
	result)))


;;;===========================================================================
;;;  File name refinement
;;;===========================================================================

(defun template-default-file (template &optional raw num ext)
  "Refine file name according to TEMPLATE, RAW, NUM and EXT.
The result is in `template-file'.  See `template-derivation-alist'."
  (let ((template-rne (cddr template))
	(file-rne (cddr template-file)))
    (if raw
	(if (eq raw t) (setq raw (car template-rne)))
      (setq raw (car file-rne)))
    (if num
	(if (eq num t) (setq num (cadr template-rne)))
      (setq num (cadr file-rne)))
    (if ext
	(if (eq ext t) (setq ext (caddr template-rne)))
      (setq ext (caddr file-rne)))
    (setcdr template-file (list (concat raw num ext) raw num ext))))

(defunx template-unique-file (template &optional raw num ext auto-num)
  "Refine file name according to TEMPLATE, RAW, NUM, EXT and AUTO-NUM.
Use auto numbering if NUM is not \"\" or AUTO-NUM is non-nil.  The
result is in `template-file'.  See `template-derivation-alist'."
  (template-default-file template raw num ext)
  (let* ((dir (car template-file))
	 (full (expand-file-name (cadr template-file) dir)))
    (when (if (string= (fourth template-file) "")
	      auto-num
	    (setq auto-num
		  (and (or (get-file-buffer full)
			   (file-readable-p full))
		       (string-to-number (fourth template-file)))))
      (setq auto-num (1- auto-num)
	    raw (third template-file)
	    ext (fifth template-file))
      (dolist (buffer (buffer-list))
	(let* ((file1 (buffer-file-name buffer))
	       (dir1 (and file1 (file-name-directory file1))))
	  (when (equal dir1 dir)
	    (setq auto-num
		  (max (template-filename-number
			(cddr (template-split-filename
			       (file-name-nondirectory file1)
			       dir1))
			raw ext)
		       auto-num)))))
      (dolist (file1 (directory-files dir nil nil t :XEMACS t))
	(unless (:EMACS file-directory-p file1)
	  (setq auto-num
		(max (template-filename-number
		      (cddr (template-split-filename file1 dir))
		      raw ext)
		     auto-num))))
      (template-default-file template raw
			     (int-to-string (1+ auto-num))
			     ext))))

(defun template-filename-number (file-rne raw ext)
  "Return numbering in FILE-RNE if the RAW and EXT parts are equal."
  (or (and (string= (car file-rne) raw)
	   (string= (caddr file-rne) ext)
	   (string-to-number (cadr file-rne)))
      0))


;;;===========================================================================
;;;  Safe commands for per-template expansions
;;;===========================================================================

(defun template-insert-time (&optional format default)
  "Insert time into current buffer using time format FORMAT.
If FORMAT is not a string, it uses DEFAULT or the result of function
`current-time-string'."
  (interactive)
  (insert (if (and (stringp format) (fboundp 'format-time-string))
	      (format-time-string format (current-time))
	    (or default (current-time-string)))))
(put 'template-insert-time 'template-secure-command
     (lambda (args)
       (or (null args) (and (stringp (car args)) (null (cdr args))))))

(defun template-insert-package (sep)
  "Insert package name into current buffer using separator SEP.
The package name is considered the directory paths between the new
file and the template file itself.  When SEP is non-nil, replace
slashed and backslashes with SEP in the directory path.

For example, with SEP=\".\" and the following paths:
 - template file: /someroot/TEMPLATE.java,
 - new file:      /someroot/aaa/bbb/File.java,
\"(>>>PACKAGE<<<)\" will be replaced by \"aaa.bbb\"."
  (interactive "sSeparator: ")
  (let ((beg (point))
	(base (file-name-directory template-template)))
    (if (and (> (length (car template-file)) (length base))
	     (string-equal (substring (car template-file) 0 (length base))
			   base))
	(insert (substring (car template-file) (length base) -1))
      (insert (substring (car template-file) 0 -1)))
    (when sep
      (save-excursion
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char beg)
          (while (re-search-forward "[/\\]" nil t)
            (replace-match sep t t)))))))


;;;===========================================================================
;;;  Functions for the predefined expansions
;;;===========================================================================

(defun template-register (&optional register)
  "Set current location in register REGISTER.
That is, \\[jump-to-register] REGISTER jumps to the current position.
If REGISTER is nil, use register corresponding to the last character in
`template-current'."
  (let* ((char (if register
		   (template-char-or-int-to-char register)
		 (aref template-current (1- (length template-current)))))
	 (elem (assoc char template-register-alist)))
    (point-to-register char)
    (if elem
	(setcdr elem t)
      (push (list char) template-register-alist))))

(defun template-read-from-minibuffer (key prompt &optional
					  prefix suffix default policy)
  "Ask user with PROMPT for a STRING.
Do nothing unless KEY is nil or POLICY is `immediately'.  KEY is the
current key of the expansion form.  For the other arguments, see
`template-read'."
  (when (or (null key) (eq policy 'immediately))
    (let* ((input (read-from-minibuffer prompt nil nil nil 'template-history))
	   (elem (cond ((string= input "") (or default ""))
		       ((eq policy 'expand)
			(concat prefix
				(format template-expansion-format
					(car template-literal-environment))
				input
				(format template-expansion-format
					(cdr template-literal-environment))
				suffix
                                ;; now make sure that (>>>LITERAL<<<) in suffix
                                ;; does not disable further expansions
				(format template-expansion-format
					(car template-literal-environment))
				(format template-expansion-format
					(cdr template-literal-environment))))
		       (t
			(concat prefix input suffix)))))
      (when key
	(push (cons key elem) template-string-alist))
      elem)))

(defun template-read (prompt &optional prefix suffix default policy)
  "Ask user with PROMPT for a STRING to be inserted.
If STRING is not \"\", insert PREFIX STRING SUFFIX, otherwise DEFAULT.
If POLICY is nil or `immediately', do not ask if `template-current'
appears another time as key in a expansion form.  If POLICY is `expand',
the inserted region is searched for expansion forms where STRING is
marked as a literal environment, see `template-literal-environment'."
  (setq template-modified t)
  (or policy (setq policy 'immediately))
  (let ((pos (point)))
    (insert (if (eq policy 'immediately)
		(or (cdr (assoc template-current template-string-alist))
		    (template-read-from-minibuffer
		     template-current prompt prefix suffix default policy))
	      (template-read-from-minibuffer nil prompt
					     prefix suffix default policy)))
    (if (eq policy 'expand) (goto-char pos))))

(defun template-choice (prompt table)
  "Ask user with PROMPT for a choice and insert it.
Each element in TABLE looks like (ANSWER . TEXT).  Ask for an input with
completion over all ANSWERs and insert corresponding TEXT if ANSWER is a
string, otherwise ask a \"y or n\" question and use the result of
`y-or-n-p' as ANSWER.  Expansion forms in TEXT will be expanded."
  (setq template-modified t)
  (let ((pos (point)))
    (insert (or (cdr (assoc (if (stringp (caar table))
				(completing-read prompt table nil t nil
						 'template-choice-history)
			      (y-or-n-p prompt))
			    table))
		"")
            ;; now make sure that (>>>LITERAL<<<) in TEXT
            ;; does not disable further expansions
	    (format template-expansion-format
		    (car template-literal-environment))
	    (format template-expansion-format
		    (cdr template-literal-environment)))
    (goto-char pos)))


;;;===========================================================================
;;;  Menu filter
;;;===========================================================================

(defun template-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "Menu filter for `template-creation-menu'."
  (let ((alist (append template-expansion-alist
		       template-default-expansion-alist))
	menu used key)
    (while alist
      (unless (member (setq key (car (pop alist))) used)
	(push key used)
	(push (vector (concat "Insert " key)
		      (list 'template-insert-form current-prefix-arg key)
		      t)
	      menu)))
    (append menu-items (nreverse menu))))


;;;===========================================================================
;;;  Insert and define forms
;;;===========================================================================

(defun template-buffer-template-p ()
  "Return non-nil, if current buffer is likely to be a template file."
  (and buffer-file-name
       (string-match (concat (regexp-quote template-extension) "\\'")
		     (file-name-sans-versions buffer-file-name))))

(defun template-open-template ()
  "If current buffer is no template file, open a new one."
  (interactive)
  (if (template-buffer-template-p)
      (barf-if-buffer-read-only)
    (let (name
	  (dir (and (car template-subdirectories)
		    (expand-file-name (car template-subdirectories)))))
      (if (null buffer-file-name)
	  (setq name (concat "TEMPLATE" template-extension))
	(setq name (file-name-sans-versions
		    (file-name-nondirectory buffer-file-name)))
	(if (string-match ".\\.[^.]*\\'" name)
	    (setq name (concat "TEMPLATE"
			       (substring name (1+ (match-beginning 0)))
			       template-extension))
	  (setq name (concat name template-extension)
		;; dot file => template not specific for directory
		dir (car template-default-directories))))
      (setq name (read-file-name "Open template file (empty=none): "
				 dir nil nil name))
      (or (string= name "")
	  (template-find-template name)))))

(defun template-insert-form (arg key)
  "Insert an expansion form according to KEY into template.
When called interactively, allow completion over all keys in
`template-expansion-alist' and `template-default-expansion-alist'.
If prefix ARG is nil, run `template-open-template' first."
  (interactive
   (list current-prefix-arg
	 (completing-read "Insert key (0-9 for register position): "
			  (append template-expansion-alist
				  template-default-expansion-alist))))
  (or arg (template-open-template))
  (insert (format template-expansion-format key))
  (if (equal key (car template-literal-environment))
      (let ((pos (point)))
	(insert (format template-expansion-format
			(cdr template-literal-environment)))
	(goto-char pos))))

(defun template-define-start (arg &rest args)
  "Insert a definition section and definition into template.
See `template-definition-start'.  If ARGS is non-nil, pass ARGS to
`format' for a new definition.  If prefix ARG is nil, run
`template-open-template' first."
  (interactive "P")
  (or arg (template-open-template))
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (concat "^[ \t]*"
				       (regexp-quote template-definition-start)
				       "[ \t]*$")
			       nil t)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert template-definition-start))
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (if args (insert (apply 'format args) "\n")))
  (message "Put definition at the end of the template"))

(defun template-define-message (arg message)
  "Insert a temporary message MESSAGE definition into template.
For ARG, see `template-define-start'."
  (interactive "P\nsTemporary message: ")
  (template-define-start arg "%S" message))

(defun template-define-prompt (arg key prompt &optional prefix suffix default)
  "Insert a definition for KEY as PROMPT into template.
For ARG, see `template-define-start'."
  (interactive "P\nsExpansion key: \nsExpansion prompt: \nsPrefix for non-empty input: \nsSuffix for non-empty input: \nsDefault for empty input: ")
  (let* ((completion-ignore-case t)
	 (policies '(("immediately" . " immediately")
		     ("with first expansion")
		     ("on every occurrence" . " every")
		     ("on every occurrence, expand" . " expand")))
	 (policy (completing-read "Query policy: " policies nil t)))
    (template-define-start arg "(%S %S %S %S %S%s)"
			   key prompt prefix suffix default
			   (or (cdr (assoc policy policies)) ""))))

(defun template-define-register (arg register)
  "Insert a setting of REGISTER into template.
For ARG, see `template-define-start'."
  (interactive "P\ncDefine register: ")
  (let* ((old (get-register register))
	 (contents (read-from-minibuffer "Register contents: "
				    (and (stringp old)
					 (not (string-match "\n" old))
					 old)))
	 (comment (read-from-minibuffer "Comment (empty=none): ")))
    (if (string= comment "")
	(template-define-start arg "(%S %S)" register contents)
      (template-define-start arg "(%S %S %S)" register contents comment))))
  

;;;===========================================================================
;;;  Initialization
;;;===========================================================================

;; easymenu.el is for top-level menus only...
(defunx template-add-submenu (menu &optional where)
  "Add the submenu MENU to the end of a menu in WHERE in the menubar.
WHERE is a list of menus tried to add MENU to.  If no such menu exist,
no menu is added.  When using Emacs, always add to the \"Edit\" menu.
See `easy-menu-define' for the format of MENU."
  (and menu
       :EMACS
       (>= emacs-major-version 21)
       (boundp 'menu-bar-edit-menu)
       (let ((keymap (easy-menu-create-menu (car menu) (cdr menu))))
	 ;; `easy-menu-get-map' doesn't get the right one => use hard-coded
	 (define-key-after menu-bar-edit-menu (vector (intern (car menu)))
	   (cons 'menu-item
		 (cons (car menu)
		       (if (not (symbolp keymap))
			   (list keymap)
			 (cons (symbol-function keymap)
			       (get keymap 'menu-prop)))))))
       :XEMACS
       (featurep 'menubar)
       (let ((current-menubar default-menubar) path)
	 (while where
	   (setq path (list (pop where)))
	   (if (find-menu-item default-menubar path)
	       (setq where nil)
	     (setq path nil)))
	 (when path (add-submenu path menu)))))

(defunx template-initialize-menus ()
  "Add some submenus for package template."
  (template-add-submenu template-comment-menu :XEMACS '("Edit"))
  (template-add-submenu template-creation-menu :XEMACS '("Cmds" "Edit"))
  :EMACS
  (and (boundp 'menu-bar-files-menu)
       (define-key-after menu-bar-files-menu [template-new-file]
         '(menu-item "New File Using Template..." template-new-file
                     :enable (not (window-minibuffer-p
                                   (frame-selected-window
                                    menu-updating-frame)))
                     :help "Create a new file, using a template")
         'dired))
  :XEMACS
  (and (featurep 'menubar)
       (find-menu-item default-menubar '("File"))
       (let ((current-menubar default-menubar))
         ;; XEmacs-20.4 `add-submenu' does not have 4th arg IN-MENU
         (add-menu-button '("File")
                          ["New File Using Template..." template-new-file
                           :active t]
                          "Insert File..."))))

;;;###autoload
(defun template-initialize-do ()
  "Initialized package template.  See variable `template-initialize'."
  (when (and template-use-package
	     (null (get 'template-initialize :initilized-with)))
    (let ((regexp (concat (regexp-quote template-extension) "\\'")))
      (or (assoc regexp auto-mode-alist)
          (push (list regexp nil 'template-new-file) auto-mode-alist)))
    (when (or (eq template-initialize t)
              (memq 'cc-mode template-initialize))
      (add-hook 'c-mode-common-hook 'template-c-init-fill-function)
      (add-hook 'antlr-mode-hook 'template-c-init-fill-function))
    (when (or (eq template-initialize t)
              (memq 'de-html-helper template-initialize))
      (setq html-helper-build-new-buffer nil)
      (setq html-helper-do-write-file-hooks nil))
    (when (or (eq template-initialize t)
              (memq 'keys template-initialize))
      (condition-case nil			; older Emacses don't understand all
          (progn
            (define-key ctl-x-map "t" 'template-new-file)
            (define-key ctl-x-map [(control =)] 'template-single-comment)
            (define-key ctl-x-map [(control ?\;)] 'template-block-comment))
        (error nil)))
    (when (or (eq template-initialize t)
              (memq 'menus template-initialize))
      (template-initialize-menus))
    (when (or (eq template-initialize t)
              (memq 'auto template-initialize))
      (add-hook 'write-file-hooks 'template-update-buffer)
      (add-hook 'find-file-not-found-hooks 'template-not-found-function t))
    (when (or (eq template-initialize t)
              (memq 'ffap template-initialize))
      (or template-ffap-file-finder
          (setq template-ffap-file-finder
                (if (boundp 'ffap-file-finder)
                    ffap-file-finder
                  (or (get 'ffap-file-finder 'saved-value) 'find-file))))
      (setq ffap-file-finder 'template-ffap-find-file))
    (put 'template-initialize :initilized-with template-initialize)))

(defun template-initialize-and-set (symbol value)
  "Custom :set function for `template-use-package'.
It sets the default value of SYMBOL to VALUE and initializes this
package."
  (set-default symbol value)     ; symbol should be `template-use-package'
  (when value
    (if (cond-emacs-xemacs
	 :EMACS  (and (boundp 'after-init-time) (null after-init-time))
	 :XEMACS (null init-file-loaded))
	;; in the meantime,`template-use-package' could have been reset to nil
	;; (e.g. when using custom theme with t, user setting with nil)
	(add-hook 'after-init-hook 'template-initialize-do)
      (template-initialize-do))))

;; This must be late in this file as set function is called during loading.
(defcustom template-use-package nil
  "Non-nil if package template is in use.
If in use (see variable `template-initialize' for details):
 - use template files when visiting non-existing files,
 - update parts of the content when saving a buffer,
 - add keybindings are menus the first time the package is put into use."
  :group 'template
  :type '(boolean :on "in use" :off "not yet initialized or turned off"
		  :help-echo "Use package Template, initialize if necessary.")
  :require 'template
  :set 'template-initialize-and-set)

;;;###autoload
(defun template-initialize (&rest _dummies)
  "Compatibility command to enable package template."
  (interactive)
  (put 'template-initialize :initilized-with nil)
  (custom-set-variables '(template-use-package t nil (template))))

(provide 'template)

;;; Local IspellPersDict: .ispell_template
;;; template.el ends here
