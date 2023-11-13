;;; yasnippet.el --- Yet another snippet extension for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2023 Free Software Foundation, Inc.
;; Authors: pluskid <pluskid@gmail.com>,
;;          João Távora <joaotavora@gmail.com>,
;;          Noam Postavsky <npostavs@gmail.com>
;; Maintainer: Noam Postavsky <npostavs@gmail.com>
;; Version: 0.14.0
;; X-URL: http://github.com/joaotavora/yasnippet
;; Keywords: convenience, emulation
;; URL: http://github.com/joaotavora/yasnippet
;; Package-Requires: ((cl-lib "0.5") (emacs "24.4"))
;; EmacsWiki: YaSnippetMode

;; This program is free software: you can redistribute it and/or modify
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
;;
;;   Basic steps to setup:
;;
;;    (add-to-list 'load-path
;;                 "~/path-to-yasnippet")
;;    (require 'yasnippet)
;;    (yas-global-mode 1)
;;
;;
;;   Interesting variables are:
;;
;;       `yas-snippet-dirs'
;;
;;           The directory where user-created snippets are to be
;;           stored.  Can also be a list of directories.  In that case,
;;           when used for bulk (re)loading of snippets (at startup or
;;           via `yas-reload-all'), directories appearing earlier in
;;           the list override other dir's snippets.  Also, the first
;;           directory is taken as the default for storing the user's
;;           new snippets.
;;
;;           The deprecated `yas/root-directory' aliases this variable
;;           for backward-compatibility.
;;
;;
;;   Major commands are:
;;
;;       M-x yas-expand
;;
;;           Try to expand snippets before point.  In `yas-minor-mode',
;;           this is normally bound to TAB, but you can customize it in
;;           `yas-minor-mode-map'.
;;
;;       M-x yas-load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas-activate-extra-mode
;;
;;           Prompts you for an extra mode to add snippets for in the
;;           current buffer.
;;
;;       M-x yas-insert-snippet
;;
;;           Prompts you for possible snippet expansion if that is
;;           possible according to buffer-local and snippet-local
;;           expansion conditions.  With prefix argument, ignore these
;;           conditions.
;;
;;       M-x yas-visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas-insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas-new-snippet
;;
;;           Lets you create a new snippet file in the correct
;;           subdirectory of `yas-snippet-dirs', according to the
;;           active major mode.
;;
;;       M-x yas-load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas-tryout-snippet
;;
;;           When editing a snippet, this opens a new empty buffer,
;;           sets it to the appropriate major mode and inserts the
;;           snippet there, so you can see what it looks like.  This is
;;           bound to "C-c C-t" while in `snippet-mode'.
;;
;;       M-x yas-describe-tables
;;
;;           Lists known snippets in a separate buffer.  User is
;;           prompted as to whether only the currently active tables
;;           are to be displayed, or all the tables for all major
;;           modes.
;;
;;   If you have `dropdown-list' installed, you can optionally use it
;;   as the preferred "prompting method", putting in your .emacs file,
;;   for example:
;;
;;       (require 'dropdown-list)
;;       (setq yas-prompt-functions '(yas-dropdown-prompt
;;                                    yas-ido-prompt
;;                                    yas-completing-prompt))
;;
;;   Also check out the customization group
;;
;;        M-x customize-group RET yasnippet RET
;;
;;   If you use the customization group to set variables
;;   `yas-snippet-dirs' or `yas-global-mode', make sure the path to
;;   "yasnippet.el" is present in the `load-path' *before* the
;;   `custom-set-variables' is executed in your .emacs file.
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://github.com/joaotavora/yasnippet

;;; Code:

(require 'cl-lib)
(require 'eldoc) ; Needed for 24.
(declare-function cl-progv-after "cl-extra") ; Needed for 23.4.
(require 'easymenu)
(require 'help-mode)

(defvar yas--editing-template)
(defvar yas--guessed-modes)
(defvar yas--indent-original-column)
(defvar yas--scheduled-jit-loads)
(defvar yas-keymap)
(defvar yas-selected-text)
(defvar yas-verbosity)
(defvar yas--current-template)


;;; User customizable variables

(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :prefix "yas-"
  :group 'editing)

(defconst yas--loaddir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that yasnippet was loaded from.")

(defconst yas-installed-snippets-dir (expand-file-name "snippets" yas--loaddir))
(make-obsolete-variable 'yas-installed-snippets-dir "\
Yasnippet no longer comes with installed snippets" "0.14")

(defconst yas--default-user-snippets-dir
  (expand-file-name "snippets" user-emacs-directory))

(defcustom yas-snippet-dirs (list yas--default-user-snippets-dir)
  "List of top-level snippet directories.

Each element, a string or a symbol whose value is a string,
designates a top-level directory where per-mode snippet
directories can be found.

Elements appearing earlier in the list override later elements'
snippets.

The first directory is taken as the default for storing snippet's
created with `yas-new-snippet'. "
  :type '(choice (directory :tag "Single directory")
                 (repeat :tag "List of directories"
                         (choice (directory) (variable))))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             (unless (or (not (fboundp 'yas-reload-all))
                         (equal old new))
               (yas-reload-all)))))

(defun yas-snippet-dirs ()
  "Return variable `yas-snippet-dirs' as list of strings."
  (cl-loop for e in (if (listp yas-snippet-dirs)
                        yas-snippet-dirs
                      (list yas-snippet-dirs))
           collect
           (cond ((stringp e) e)
                 ((and (symbolp e)
                       (boundp e)
                       (stringp (symbol-value e)))
                  (symbol-value e))
                 (t
                  (error "[yas] invalid element %s in `yas-snippet-dirs'" e)))))

(defcustom yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`"
  "Default snippet to use when creating a new snippet.
If nil, don't use any snippet."
  :type 'string)

(defcustom yas-prompt-functions '(yas-dropdown-prompt
                                  yas-completing-prompt
                                  yas-maybe-ido-prompt
                                  yas-no-prompt)
  "Functions to prompt for keys, templates, etc interactively.

These functions are called with the following arguments:

- PROMPT: A string to prompt the user

- CHOICES: a list of strings or objects.

- optional DISPLAY-FN : A function that, when applied to each of
the objects in CHOICES will return a string.

The return value of any function you put here should be one of
the objects in CHOICES, properly formatted with DISPLAY-FN (if
that is passed).

- To signal that your particular style of prompting is
unavailable at the moment, you can also have the function return
nil.

- To signal that the user quit the prompting process, you can
signal `quit' with

    (signal \\='quit \"user quit!\")"
  :type '(repeat function))

(defcustom yas-indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

- `fixed' Indent the snippet to the current column;

- `auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indentation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto)))

(defcustom yas-also-auto-indent-first-line nil
  "Non-nil means also auto indent first line according to mode.

Naturally this is only valid when `yas-indent-line' is `auto'."
  :type 'boolean)

(defcustom yas-also-indent-empty-lines nil
  "Non-nil means also indent empty lines according to mode."
  :type 'boolean)

(defcustom yas-snippet-revival t
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean)

(defcustom yas-triggers-in-field nil
  "If non-nil, allow stacked expansions (snippets inside snippets).

Otherwise `yas-next-field-or-maybe-expand' just moves on to the
next field"
  :type 'boolean)

(defcustom yas-fallback-behavior 'return-nil
  "This option is obsolete.
Now that the conditional keybinding `yas-maybe-expand' is
available, there's no more need for it."
  :type '(choice (const :tag "Call previous command"  call-other-command)
                 (const :tag "Do nothing"             return-nil)))

(make-obsolete-variable
 'yas-fallback-behavior
 "For `call-other-command' behavior bind to the conditional
command value `yas-maybe-expand', for `return-nil' behavior bind
directly to `yas-expand'."
 "0.12")

(defcustom yas-choose-keys-first nil
  "If non-nil, prompt for snippet key first, then for template.

Otherwise prompts for all possible snippet names.

This affects `yas-insert-snippet' and `yas-visit-snippet-file'."
  :type 'boolean)

(defcustom yas-choose-tables-first nil
  "If non-nil, and multiple eligible snippet tables, prompts user for tables first.

Otherwise, user chooses between the merging together of all
eligible tables.

This affects `yas-insert-snippet', `yas-visit-snippet-file'"
  :type 'boolean)

(defcustom yas-use-menu 'abbreviate
  "Display a YASnippet menu in the menu bar.

When non-nil, submenus for each snippet table will be listed
under the menu \"Yasnippet\".

- If set to `abbreviate', only the current major-mode
menu and the modes set in `yas--extra-modes' are listed.

- If set to `full', every submenu is listed

- If set to nil, hide the menu.

Any other non-nil value, every submenu is listed."
  :type '(choice (const :tag "Full"  full)
                 (const :tag "Abbreviate" abbreviate)
                 (const :tag "No menu" nil)))

(defcustom yas-trigger-symbol (or (and (eq window-system 'mac)
                                       (ignore-errors
                                         (char-to-string ?\x21E5))) ;; little ->| sign
                                  " =>")
  "The text that will be used in menu to represent the trigger."
  :type 'string)

(defcustom yas-wrap-around-region nil
  "What to insert for snippet's $0 field.

If set to a character, insert contents of corresponding register.
If non-nil insert region contents.  This can be overridden on a
per-snippet basis.  A value of `cua' is considered equivalent to
`?0' for backwards compatibility."
  :type '(choice (character :tag "Insert from register")
                 (const t :tag "Insert region contents")
                 (const nil :tag "Don't insert anything")
                 (const cua))) ; backwards compat

(defcustom yas-good-grace t
  "If non-nil, don't raise errors in elisp evaluation.

This affects both the inline elisp in snippets and the hook
variables such as `yas-after-exit-snippet-hook'.

If this variable's value is `inline', an error string \"[yas]
error\" is returned instead of raising the error.  If this
variable's value is `hooks', a message is output to according to
`yas-verbosity-level'.  If this variable's value is t, both are
active."
  :type 'boolean)

(defcustom yas-visit-from-menu nil
  "If non-nil visit snippets's files from menu, instead of expanding them.

This can only work when snippets are loaded from files."
  :type 'boolean)

(defcustom yas-expand-only-for-last-commands nil
  "List of `last-command' values to restrict tab-triggering to, or nil.

Leave this set at nil (the default) to be able to trigger an
expansion simply by placing the cursor after a valid tab trigger,
using whichever commands.

Optionally, set this to something like (self-insert-command) if
you to wish restrict expansion to only happen when the last
letter of the snippet tab trigger was typed immediately before
the trigger key itself."
  :type '(repeat function))

(defcustom yas-alias-to-yas/prefix-p t
  "If non-nil make aliases for the old style yas/ prefixed symbols.
It must be set to nil before loading yasnippet to take effect."
  :type 'boolean)

;; Only two faces, and one of them shouldn't even be used...
;;
(defface yas-field-highlight-face
  '((t (:inherit region)))
  "The face used to highlight the currently active field of a snippet")

(defface yas--field-debug-face
  '()
  "The face used for debugging some overlays normally hidden")


;;; User-visible variables

(defconst yas-maybe-skip-and-clear-field
  '(menu-item "" yas-skip-and-clear-field
              :filter yas--maybe-clear-field-filter)
  "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
`yas-skip-and-clear-field' only when at the beginning of an
unmodified snippet field.")

(defconst yas-maybe-clear-field
    '(menu-item "" yas-clear-field
                :filter yas--maybe-clear-field-filter)
    "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
`yas-clear-field' only when at the beginning of an
unmodified snippet field.")

(defun yas-filtered-definition (def)
  "Return a condition key definition.
The condition will respect the value of `yas-keymap-disable-hook'."
  `(menu-item "" ,def
              :filter ,(lambda (cmd) (unless (run-hook-with-args-until-success
                                         'yas-keymap-disable-hook)
                                  cmd))))

(defvar yas-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]       (yas-filtered-definition 'yas-next-field-or-maybe-expand))
    (define-key map (kbd "TAB")   (yas-filtered-definition 'yas-next-field-or-maybe-expand))
    (define-key map [(shift tab)] (yas-filtered-definition 'yas-prev-field))
    (define-key map [backtab]     (yas-filtered-definition 'yas-prev-field))
    (define-key map (kbd "C-g")   (yas-filtered-definition 'yas-abort-snippet))
    ;; Yes, filters can be chained!
    (define-key map (kbd "C-d")   (yas-filtered-definition yas-maybe-skip-and-clear-field))
    (define-key map (kbd "DEL")   (yas-filtered-definition yas-maybe-clear-field))
    map)
  "The active keymap while a snippet expansion is in progress.")

(defvar yas-key-syntaxes (list #'yas-try-key-from-whitespace
                               "w_.()" "w_." "w_" "w")
  "Syntaxes and functions to help look for trigger keys before point.

Each element in this list specifies how to skip buffer positions
backwards and look for the start of a trigger key.

Each element can be either a string or a function receiving the
original point as an argument. A string element is simply passed
to `skip-syntax-backward' whereas a function element is called
with no arguments and should also place point before the original
position.

The string between the resulting buffer position and the original
point is matched against the trigger keys in the active snippet
tables.

If no expandable snippets are found, the next element is the list
is tried, unless a function element returned the symbol `again',
in which case it is called again from the previous position and
may once more reposition point.

For example, if `yas-key-syntaxes' has the value (\"w\" \"w_\"),
trigger keys composed exclusively of \"word\"-syntax characters
are looked for first. Failing that, longer keys composed of
\"word\" or \"symbol\" syntax are looked for. Therefore,
triggering after

foo-barbaz

will, according to the \"w\" element first try \"barbaz\". If
that isn't a trigger key, \"foo-barbaz\" is tried, respecting the
second \"w_\" element. Notice that even if \"baz\" is a trigger
key for an active snippet, it won't be expanded, unless a
function is added to `yas-key-syntaxes' that eventually places
point between \"bar\" and \"baz\".

See also Info node `(elisp) Syntax Descriptors'.")

(defvar yas-after-exit-snippet-hook
  '()
  "Hook run after a snippet exited.

The functions will be run in an environment where some variables bound to
proper values:

`yas-snippet-beg' : The beginning of the region of the snippet.

`yas-snippet-end' : Similar to beg.

Attention: This hook is not run when exiting nested/stacked snippet expansion!")

(defvar yas-before-expand-snippet-hook
  '()
  "Hook run just before expanding a snippet.")

(defconst yas-not-string-or-comment-condition
  '(if (let ((ppss (syntax-ppss)))
         (or (nth 3 ppss) (nth 4 ppss)))
       '(require-snippet-condition . force-in-comment)
     t)
  "Disables snippet expansion in strings and comments.
To use, set `yas-buffer-local-condition' to this value.")

(defcustom yas-buffer-local-condition t
  "Snippet expanding condition.

This variable is a Lisp form which is evaluated every time a
snippet expansion is attempted:

    * If it evaluates to nil, no snippets can be expanded.

    * If it evaluates to the a cons (require-snippet-condition
      . REQUIREMENT)

       * Snippets bearing no \"# condition:\" directive are not
         considered

       * Snippets bearing conditions that evaluate to nil (or
         produce an error) won't be considered.

       * If the snippet has a condition that evaluates to non-nil
         RESULT:

          * If REQUIREMENT is t, the snippet is considered

          * If REQUIREMENT is `eq' RESULT, the snippet is
            considered

          * Otherwise, the snippet is not considered.

    * If it evaluates to the symbol `always', all snippets are
      considered for expansion, regardless of any conditions.

    * If it evaluates to t or some other non-nil value

       * Snippet bearing no conditions, or conditions that
         evaluate to non-nil, are considered for expansion.

       * Otherwise, the snippet is not considered.

Here's an example preventing snippets from being expanded from
inside comments, in `python-mode' only, with the exception of
snippets returning the symbol `force-in-comment' in their
conditions.

 (add-hook \\='python-mode-hook
           (lambda ()
              (setq yas-buffer-local-condition
                    \\='(if (python-syntax-comment-or-string-p)
                         \\='(require-snippet-condition . force-in-comment)
                       t))))"
  :type
  `(choice
    (const :tag "Disable snippet expansion inside strings and comments"
           ,yas-not-string-or-comment-condition)
    (const :tag "Expand all snippets regardless of conditions" always)
    (const :tag "Expand snippets unless their condition is nil" t)
    (const :tag "Disable all snippet expansion" nil)
    sexp))

(defcustom yas-keymap-disable-hook nil
  "The `yas-keymap' bindings are disabled if any function in this list returns non-nil.
This is useful to control whether snippet navigation bindings
override bindings from other packages (e.g., `company-mode')."
  :type 'hook)

(defcustom yas-overlay-priority 100
  "Priority to use for yasnippets overlays.
This is useful to control whether snippet navigation bindings
override `keymap' overlay property bindings from other packages."
  :type 'integer)

(defcustom yas-inhibit-overlay-modification-protection nil
  "If nil, changing text outside the active field aborts the snippet.
This protection is intended to prevent yasnippet from ending up
in an inconsistent state.  However, some packages (e.g., the
company completion package) may trigger this protection when it
is not needed.  In that case, setting this variable to non-nil
can be useful."
  ;; See also `yas--on-protection-overlay-modification'.
  :type 'boolean)


;;; Internal variables

(defconst yas--version "0.14.0")

(defvar yas--menu-table (make-hash-table)
  "A hash table of MAJOR-MODE symbols to menu keymaps.")

(defvar yas--escaped-characters
  '(?\\ ?` ?\" ?' ?$ ?} ?{ ?\( ?\))
  "List of characters which *might* need to be escaped.")

(defconst yas--field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas--multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas--backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"\\=`lisp-expression\\=`\" expression." )

(defconst yas--transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([ \t\n]*([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas--simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas--snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas--snippet-next-id ()
  (let ((id yas--snippet-id-seed))
    (cl-incf yas--snippet-id-seed)
    id))


;;; Minor mode stuff

(defvar yas--minor-mode-menu nil
  "Holds the YASnippet menu.")

(defvar yas--condition-cache-timestamp nil)

(defun yas-maybe-expand-abbrev-key-filter (cmd)
  "Return CMD if there is an expandable snippet at point.
This function is useful as a `:filter' to a conditional key
definition."
  (when (let ((yas--condition-cache-timestamp (current-time)))
          (yas--templates-for-key-at-point))
    cmd))

(define-obsolete-function-alias 'yas--maybe-expand-key-filter
  #'yas-maybe-expand-abbrev-key-filter "0.14")

(defconst yas-maybe-expand
  '(menu-item "" yas-expand :filter yas-maybe-expand-abbrev-key-filter)
  "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
`yas-expand' only when there is a snippet available to be
expanded.")

(defvar yas-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Modes should always bind to TAB instead of `tab', so as not to override
    ;; bindings that should take higher precedence but which bind to `TAB`
    ;; instead (relying on `function-key-map` to remap `tab` to TAB).
    ;; If this causes problem because of another package that binds to `tab`,
    ;; complain to that other package!
    ;;(define-key map [(tab)]     yas-maybe-expand)
    (define-key map (kbd "TAB") yas-maybe-expand)
    (define-key map "\C-c&\C-s" #'yas-insert-snippet)
    (define-key map "\C-c&\C-n" #'yas-new-snippet)
    (define-key map "\C-c&\C-v" #'yas-visit-snippet-file)
    map)
  "The keymap used when `yas-minor-mode' is active.")

(easy-menu-define yas--minor-mode-menu
      yas-minor-mode-map
      "Menu used when `yas-minor-mode' is active."
  '("YASnippet" :visible yas-use-menu
    "----"
    ["Expand trigger" yas-expand
     :help "Possibly expand tab trigger before point"]
    ["Insert at point..." yas-insert-snippet
     :help "Prompt for an expandable snippet and expand it at point"]
    ["New snippet..." yas-new-snippet
     :help "Create a new snippet in an appropriate directory"]
    ["Visit snippet file..." yas-visit-snippet-file
     :help "Prompt for an expandable snippet and find its file"]
    "----"
    ("Snippet menu behaviour"
     ["Visit snippets" (setq yas-visit-from-menu t)
      :help "Visit snippets from the menu"
      :active t :style radio   :selected yas-visit-from-menu]
     ["Expand snippets" (setq yas-visit-from-menu nil)
      :help "Expand snippets from the menu"
      :active t :style radio :selected (not yas-visit-from-menu)]
     "----"
     ["Show all known modes" (setq yas-use-menu 'full)
      :help "Show one snippet submenu for each loaded table"
      :active t :style radio   :selected (eq yas-use-menu 'full)]
     ["Abbreviate according to current mode" (setq yas-use-menu 'abbreviate)
      :help "Show only snippet submenus for the current active modes"
      :active t :style radio   :selected (eq yas-use-menu 'abbreviate)])
    ("Indenting"
     ["Auto" (setq yas-indent-line 'auto)
      :help "Indent each line of the snippet with `indent-according-to-mode'"
      :active t :style radio   :selected (eq yas-indent-line 'auto)]
     ["Fixed" (setq yas-indent-line 'fixed)
      :help "Indent the snippet to the current column"
      :active t :style radio   :selected (eq yas-indent-line 'fixed)]
     ["None" (setq yas-indent-line 'none)
      :help "Don't apply any particular snippet indentation after expansion"
      :active t :style radio   :selected (not (member yas-indent-line '(fixed auto)))]
     "----"
     ["Also auto indent first line" (setq yas-also-auto-indent-first-line
                                          (not yas-also-auto-indent-first-line))
      :help "When auto-indenting also, auto indent the first line menu"
      :active (eq yas-indent-line 'auto)
      :style toggle :selected yas-also-auto-indent-first-line]
     )
    ("Prompting method"
     ["System X-widget" (setq yas-prompt-functions
                              (cons #'yas-x-prompt
                                    (remove #'yas-x-prompt
                                            yas-prompt-functions)))
      :help "Use your windowing system's (gtk, mac, windows, etc...) default menu"
      :active t :style radio   :selected (eq (car yas-prompt-functions)
                                             #'yas-x-prompt)]
     ["Dropdown-list" (setq yas-prompt-functions
                            (cons #'yas-dropdown-prompt
                                  (remove #'yas-dropdown-prompt
                                          yas-prompt-functions)))
      :help "Use a special dropdown list"
      :active t :style radio   :selected (eq (car yas-prompt-functions)
                                             #'yas-dropdown-prompt)]
     ["Ido" (setq yas-prompt-functions
                  (cons #'yas-ido-prompt
                        (remove #'yas-ido-prompt
                                yas-prompt-functions)))
      :help "Use an ido-style minibuffer prompt"
      :active t :style radio   :selected (eq (car yas-prompt-functions)
                                             #'yas-ido-prompt)]
     ["Completing read" (setq yas-prompt-functions
                              (cons #'yas-completing-prompt
                                    (remove #'yas-completing-prompt
                                            yas-prompt-functions)))
      :help "Use a normal minibuffer prompt"
      :active t :style radio   :selected (eq (car yas-prompt-functions)
                                             #'yas-completing-prompt)]
     )
    ("Misc"
     ["Wrap region in exit marker"
      (setq yas-wrap-around-region
            (not yas-wrap-around-region))
      :help "If non-nil automatically wrap the selected text in the $0 snippet exit"
      :style toggle :selected yas-wrap-around-region]
     ["Allow stacked expansions "
      (setq yas-triggers-in-field
            (not yas-triggers-in-field))
      :help "If non-nil allow snippets to be triggered inside other snippet fields"
      :style toggle :selected yas-triggers-in-field]
     ["Revive snippets on undo "
      (setq yas-snippet-revival
            (not yas-snippet-revival))
      :help "If non-nil allow snippets to become active again after undo"
      :style toggle :selected yas-snippet-revival]
     ["Good grace "
      (setq yas-good-grace
            (not yas-good-grace))
      :help "If non-nil don't raise errors in bad embedded elisp in snippets"
      :style toggle :selected yas-good-grace]
     )
    "----"
    ["Load snippets..."  yas-load-directory
     :help "Load snippets from a specific directory"]
    ["Reload everything" yas-reload-all
     :help "Cleanup stuff, reload snippets, rebuild menus"]
    ["About"            yas-about
     :help "Display some information about YASnippet"]))

(define-obsolete-variable-alias 'yas-extra-modes 'yas--extra-modes "0.9.1")
(defvar yas--extra-modes nil
  "An internal list of modes for which to also lookup snippets.

This variable probably makes more sense as buffer-local, so
ensure your use `make-local-variable' when you set it.")

(defvar yas--tables (make-hash-table)
  "A hash table of mode symbols to `yas--table' objects.")

(defvar yas--parents (make-hash-table)
  "A hash table of mode symbols do lists of direct parent mode symbols.

This list is populated when reading the \".yas-parents\" files
found when traversing snippet directories with
`yas-load-directory'.

There might be additional parenting information stored in the
`derived-mode-parent' property of some mode symbols, but that is
not recorded here.")

(defvar yas--direct-keymaps (list)
  "Keymap alist supporting direct snippet keybindings.

This variable is placed in `emulation-mode-map-alists'.

Its elements looks like (TABLE-NAME . KEYMAP).  They're
instantiated on `yas-reload-all' but KEYMAP is added to only when
loading snippets.  `yas--direct-TABLE-NAME' is then a variable
set buffer-locally when entering `yas-minor-mode'.  KEYMAP binds
all defined direct keybindings to `yas-maybe-expand-from-keymap'
which decides on the snippet to expand.")

(defun yas-direct-keymaps-reload ()
  "Force reload the direct keybinding for active snippet tables."
  (interactive)
  (setq yas--direct-keymaps nil)
  (maphash #'(lambda (name table)
               (push (cons (intern (format "yas--direct-%s" name))
                           (yas--table-direct-keymap table))
                     yas--direct-keymaps))
           yas--tables))

(defun yas--modes-to-activate (&optional mode)
  "Compute list of mode symbols that are active for `yas-expand' and friends."
  (defvar yas--dfs)        ;We rely on dynbind.  We could use `letrec' instead!
  (let* ((explored (if mode (list mode) ; Building up list in reverse.
                     (cons major-mode (reverse yas--extra-modes))))
         (yas--dfs
          (lambda (mode)
            (cl-loop for neighbour
                     in (cl-list* (or (get mode 'derived-mode-parent)
                                      ;; Consider `fundamental-mode'
                                      ;; as ultimate ancestor.
                                      'fundamental-mode)
                                  ;; NOTE: `fboundp' check is redundant
                                  ;; since Emacs 24.4.
                                  (and (fboundp mode) (symbol-function mode))
                                  (gethash mode yas--parents))
                     when (and neighbour
                               (not (memq neighbour explored))
                               (symbolp neighbour))
                     do (push neighbour explored)
                     (funcall yas--dfs neighbour)))))
    (mapc yas--dfs explored)
    (nreverse explored)))

(defvar yas-minor-mode-hook nil
  "Hook run when `yas-minor-mode' is turned on.")

(defun yas--auto-fill-wrapper ()
  (when auto-fill-function ;Turning the mode ON.
    ;; (cl-assert (local-variable-p 'auto-fill-function))
    (add-function :around (local 'auto-fill-function) #'yas--auto-fill)))

;;;###autoload
(define-minor-mode yas-minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}"
  :lighter " yas" ;; The indicator for the mode line.
  (cond ((and yas-minor-mode (featurep 'yasnippet))
         ;; Install the direct keymaps in `emulation-mode-map-alists'
         ;; (we use `add-hook' even though it's not technically a hook,
         ;; but it works). Then define variables named after modes to
         ;; index `yas--direct-keymaps'.
         ;;
         ;; Also install the post-command-hook.
         ;;
         (cl-pushnew 'yas--direct-keymaps emulation-mode-map-alists)
         (add-hook 'post-command-hook #'yas--post-command-handler nil t)
         ;; Set the `yas--direct-%s' vars for direct keymap expansion
         ;;
         (dolist (mode (yas--modes-to-activate))
           (let ((name (intern (format "yas--direct-%s" mode))))
             (set-default name nil)
             (set (make-local-variable name) t)))
         ;; Perform JIT loads
         (yas--load-pending-jits)
         ;; Install auto-fill handler.
         (yas--auto-fill-wrapper)       ; Now...
         (add-hook 'auto-fill-mode-hook #'yas--auto-fill-wrapper)) ; or later.
        (t
         ;; Uninstall the direct keymaps, post-command hook, and
         ;; auto-fill handler.
         (remove-hook 'post-command-hook #'yas--post-command-handler t)
         (remove-hook 'auto-fill-mode-hook #'yas--auto-fill-wrapper)
         (when (local-variable-p 'auto-fill-function)
           (remove-function (local 'auto-fill-function) #'yas--auto-fill))
         (setq emulation-mode-map-alists
               (remove 'yas--direct-keymaps emulation-mode-map-alists)))))

(defun yas-activate-extra-mode (mode)
  "Activates the snippets for the given `mode' in the buffer.

The function can be called in the hook of a minor mode to
activate snippets associated with that mode."
  (interactive
   (let (modes
         symbol)
     (maphash (lambda (k _)
                (setq modes (cons (list k) modes)))
              yas--parents)
     (setq symbol (completing-read
                   "Activate mode: " modes nil t))
     (list
      (when (not (string= "" symbol))
        (intern symbol)))))
  (when mode
    (add-to-list (make-local-variable 'yas--extra-modes) mode)
    (yas--load-pending-jits)))

(defun yas-deactivate-extra-mode (mode)
  "Deactivates the snippets for the given `mode' in the buffer."
  (interactive
   (list (intern
          (completing-read
           "Deactivate mode: " (mapcar #'list yas--extra-modes) nil t))))
  (set (make-local-variable 'yas--extra-modes)
       (remove mode
               yas--extra-modes)))

(defun yas-temp-buffer-p (&optional buffer)
  (eq (aref (buffer-name buffer) 0) ?\s))

(define-obsolete-variable-alias 'yas-dont-activate
  'yas-dont-activate-functions "0.9.2")
(defvar yas-dont-activate-functions (list #'minibufferp #'yas-temp-buffer-p)
  "Special hook to control which buffers `yas-global-mode' affects.
Functions are called with no argument, and should return non-nil to prevent
`yas-global-mode' from enabling yasnippet in this buffer.

In Emacsen < 24, this variable is buffer-local.  Because
`yas-minor-mode-on' is called by `yas-global-mode' after
executing the buffer's major mode hook, setting this variable
there is an effective way to define exceptions to the \"global\"
activation behaviour.

In Emacsen >= 24, only the global value is used.  To define
per-mode exceptions to the \"global\" activation behaviour, call
`yas-minor-mode' with a negative argument directily in the major
mode's hook.")
(unless (> emacs-major-version 23)
  (with-no-warnings
    (make-variable-buffer-local 'yas-dont-activate)))


(defun yas-minor-mode-on ()
  "Turn on YASnippet minor mode.

Honour `yas-dont-activate-functions', which see."
  (interactive)
  (unless (or
           ;; The old behavior used for Emacs<24 was to set
           ;; `yas-dont-activate-functions' to t buffer-locally.
           (not (or (listp yas-dont-activate-functions)
                    (functionp yas-dont-activate-functions)))
           (run-hook-with-args-until-success 'yas-dont-activate-functions))
    (yas-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode yas-global-mode yas-minor-mode yas-minor-mode-on)

(defun yas--global-mode-reload-with-jit-maybe ()
  "Run `yas-reload-all' when `yas-global-mode' is on."
  (when yas-global-mode (yas-reload-all)))

(add-hook 'yas-global-mode-hook #'yas--global-mode-reload-with-jit-maybe)


;;; Major mode stuff

(defvar yas--font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
          (with-temp-buffer
            (let ((prog-mode-hook nil)
                  (emacs-lisp-mode-hook nil))
              (ignore-errors (emacs-lisp-mode)))
            (font-lock-set-defaults)
            (if (eq t (car-safe font-lock-keywords))
                ;; They're "compiled", so extract the source.
                (cadr font-lock-keywords)
              font-lock-keywords))
          '(("\\$\\([0-9]+\\)"
             (0 font-lock-keyword-face)
             (1 font-lock-string-face t))
            ("\\${\\([0-9]+\\):?"
             (0 font-lock-keyword-face)
             (1 font-lock-warning-face t))
            ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
            ("}"
             (0 font-lock-keyword-face)))))

(defvar snippet-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil
      map
      "Menu used when snippet-mode is active."
      (cons "Snippet"
            (mapcar #'(lambda (ent)
                        (when (nth 2 ent)
                          (define-key map (nth 2 ent) (nth 1 ent)))
                        (vector (nth 0 ent) (nth 1 ent) t))
                    '(("Load this snippet" yas-load-snippet-buffer "\C-c\C-l")
                      ("Load and quit window" yas-load-snippet-buffer-and-close "\C-c\C-c")
                      ("Try out this snippet" yas-tryout-snippet "\C-c\C-t")))))
    map)
  "The keymap used when `snippet-mode' is active.")



;;;###autoload(autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil)
(eval-and-compile
  (if (fboundp 'prog-mode)
      ;; `prog-mode' is new in 24.1.
      (define-derived-mode snippet-mode prog-mode "Snippet"
        "A mode for editing yasnippets"
        (setq font-lock-defaults '(yas--font-lock-keywords))
        (set (make-local-variable 'require-final-newline) nil)
        (set (make-local-variable 'comment-start) "#")
        (set (make-local-variable 'comment-start-skip) "#+[\t ]*")
        (add-hook 'after-save-hook #'yas-maybe-load-snippet-buffer nil t))
    (define-derived-mode snippet-mode fundamental-mode "Snippet"
      "A mode for editing yasnippets"
      (setq font-lock-defaults '(yas--font-lock-keywords))
      (set (make-local-variable 'require-final-newline) nil)
      (set (make-local-variable 'comment-start) "#")
      (set (make-local-variable 'comment-start-skip) "#+[\t ]*")
      (add-hook 'after-save-hook #'yas-maybe-load-snippet-buffer nil t))))

(defun yas-snippet-mode-buffer-p ()
  "Return non-nil if current buffer should be in `snippet-mode'.
Meaning it's visiting a file under one of the mode directories in
`yas-snippet-dirs'."
  (when buffer-file-name
    (cl-member buffer-file-name (yas-snippet-dirs)
               :test #'file-in-directory-p)))

;; We're abusing `magic-fallback-mode-alist' here because
;; `auto-mode-alist' doesn't support function matchers.
(add-to-list 'magic-fallback-mode-alist
             `(yas-snippet-mode-buffer-p . snippet-mode))


;;; Internal structs for template management

(cl-defstruct (yas--template
               (:constructor yas--make-template)
               ;; Handles `yas-define-snippets' format, plus the
               ;; initial TABLE argument.
               (:constructor
                yas--define-snippets-2
                (table
                 key content
                 &optional xname condition group
                 expand-env load-file xkeybinding xuuid save-file
                 &aux
                 (name (or xname
                           ;; A little redundant: we always get a name
                           ;; from `yas--parse-template' except when
                           ;; there isn't a file.
                           (and load-file (file-name-nondirectory load-file))
                           (and save-file (file-name-nondirectory save-file))
                           key))
                 (keybinding (yas--read-keybinding xkeybinding))
                 (uuid (or xuuid name))
                 (old (gethash uuid (yas--table-uuidhash table)))
                 (menu-binding-pair
                  (and old (yas--template-menu-binding-pair old)))
                 (perm-group
                  (and old (yas--template-perm-group old))))))
  "A template for a snippet."
  key
  content
  name
  condition
  expand-env
  load-file
  save-file
  keybinding
  uuid
  menu-binding-pair
  group      ;; as dictated by the #group: directive or .yas-make-groups
  perm-group ;; as dictated by `yas-define-menu'
  table
  )

(cl-defstruct (yas--table (:constructor yas--make-snippet-table (name)))
  "A table to store snippets for a particular mode.

Has the following fields:

`yas--table-name'

  A symbol name normally corresponding to a major mode, but can
  also be a pseudo major-mode to be used in
  `yas-activate-extra-mode', for example.

`yas--table-hash'

  A hash table (KEY . NAMEHASH), known as the \"keyhash\". KEY is
  a string or a vector, where the former is the snippet's trigger
  and the latter means it's a direct keybinding. NAMEHASH is yet
  another hash of (NAME . TEMPLATE) where NAME is the snippet's
  name and TEMPLATE is a `yas--template' object.

`yas--table-direct-keymap'

  A keymap for the snippets in this table that have direct
  keybindings. This is kept in sync with the keyhash, i.e., all
  the elements of the keyhash that are vectors appear here as
  bindings to `yas-maybe-expand-from-keymap'.

`yas--table-uuidhash'

  A hash table mapping snippets uuid's to the same `yas--template'
  objects. A snippet uuid defaults to the snippet's name."
  name
  (hash (make-hash-table :test 'equal))
  (uuidhash (make-hash-table :test 'equal))
  (parents nil)
  (direct-keymap (make-sparse-keymap)))

(defun yas--get-template-by-uuid (mode uuid)
  "Find the snippet template in MODE by its UUID."
  (let* ((table (gethash mode yas--tables mode)))
    (when table
      (gethash uuid (yas--table-uuidhash table)))))

;; Apropos storing/updating in TABLE, this works in two steps:
;;
;; 1. `yas--remove-template-by-uuid' removes any
;;    keyhash-namehash-template mappings from TABLE, grabbing the
;;    snippet by its uuid. Also removes mappings from TABLE's
;;    `yas--table-direct-keymap' (FIXME: and should probably take care
;;    of potentially stale menu bindings right?.)
;;
;; 2. `yas--add-template' adds this all over again.
;;
;;    Create a new or add to an existing keyhash-namehash mapping.
;;
;;  For reference on understanding this, consider three snippet
;;  definitions:
;;
;;  A:   # name: The Foo
;;       # key: foo
;;       # binding: C-c M-l
;;
;;  B:   # name: Mrs Foo
;;       # key: foo
;;
;;  C:   # name: The Bar
;;       # binding: C-c M-l
;;
;;  D:   # name: Baz
;;       # key: baz
;;
;;  keyhash       namehashes(3)      yas--template structs(4)
;;  -----------------------------------------------------
;;                                            __________
;;                                           /          \
;;  "foo"      --->  "The Foo" --->  [yas--template A]   |
;;                   "Mrs Foo" --->  [yas--template B]   |
;;                                                      |
;;  [C-c M-l]  --->  "The Foo" -------------------------/
;;                   "The Bar" --->  [yas--template C]
;;
;;  "baz"      --->  "Baz"     --->  [yas--template D]
;;
;; Additionally, since uuid defaults to the name, we have a
;; `yas--table-uuidhash' for TABLE
;;
;; uuidhash       yas--template structs
;; -------------------------------
;; "The Foo" ---> [yas--template A]
;; "Mrs Foo" ---> [yas--template B]
;; "The Bar" ---> [yas--template C]
;; "Baz"     ---> [yas--template D]
;;
;; FIXME: the more I look at this data-structure the more I think I'm
;; stupid. There has to be an easier way (but beware lots of code
;; depends on this).
;;
(defun yas--remove-template-by-uuid (table uuid)
  "Remove from TABLE a template identified by UUID."
  (let ((template (gethash uuid (yas--table-uuidhash table))))
    (when template
      (let* ((name                (yas--template-name template))
             (empty-keys          nil))
        ;; Remove the name from each of the targeted namehashes
        ;;
        (maphash #'(lambda (k v)
                     (let ((template (gethash name v)))
                       (when (and template
                                  (equal uuid (yas--template-uuid template)))
                         (remhash name v)
                         (when (zerop (hash-table-count v))
                           (push k empty-keys)))))
                 (yas--table-hash table))
        ;; Remove the namehash themselves if they've become empty
        ;;
        (dolist (key empty-keys)
          (when (vectorp key)
            (define-key (yas--table-direct-keymap table) key nil))
          (remhash key (yas--table-hash table)))

        ;; Finally, remove the uuid from the uuidhash
        ;;
        (remhash uuid (yas--table-uuidhash table))))))

(defconst yas-maybe-expand-from-keymap
  '(menu-item "" yas-expand-from-keymap
              :filter yas--maybe-expand-from-keymap-filter))

(defun yas--add-template (table template)
  "Store in TABLE the snippet template TEMPLATE.

KEY can be a string (trigger key) of a vector (direct
keybinding)."
  (let ((name (yas--template-name template))
        (key (yas--template-key template))
        (keybinding (yas--template-keybinding template))
        (_menu-binding-pair (yas--template-menu-binding-pair-get-create template)))
    (dolist (k (remove nil (list key keybinding)))
      (puthash name
               template
               (or (gethash k
                            (yas--table-hash table))
                   (puthash k
                            (make-hash-table :test 'equal)
                            (yas--table-hash table))))
      (when (vectorp k)
        (define-key (yas--table-direct-keymap table) k yas-maybe-expand-from-keymap)))

    ;; Update TABLE's `yas--table-uuidhash'
    (puthash (yas--template-uuid template)
             template
             (yas--table-uuidhash table))))

(defun yas--update-template (table template)
  "Add or update TEMPLATE in TABLE.

Also takes care of adding and updating to the associated menu.
Return TEMPLATE."
  ;; Remove from table by uuid
  ;;
  (yas--remove-template-by-uuid table (yas--template-uuid template))
  ;; Add to table again
  ;;
  (yas--add-template table template)
  ;; Take care of the menu
  ;;
  (yas--update-template-menu table template)
  template)

(defun yas--update-template-menu (table template)
  "Update every menu-related for TEMPLATE."
  (let ((menu-binding-pair (yas--template-menu-binding-pair-get-create template))
        (key (yas--template-key template))
        (keybinding (yas--template-keybinding template)))
    ;; The snippet might have changed name or keys, so update
    ;; user-visible strings
    ;;
    (unless (eq (cdr menu-binding-pair) :none)
      ;; the menu item name
      ;;
      (setf (cl-cadar menu-binding-pair) (yas--template-name template))
      ;; the :keys information (also visible to the user)
      (setf (cl-getf (cdr (car menu-binding-pair)) :keys)
            (or (and keybinding (key-description keybinding))
                (and key (concat key yas-trigger-symbol))))))
  (unless (yas--template-menu-managed-by-yas-define-menu template)
    (let ((menu-keymap
           (yas--menu-keymap-get-create (yas--table-mode table)
                                        (mapcar #'yas--table-mode
                                                (yas--table-parents table))))
          (group (yas--template-group template)))
      ;; Remove from menu keymap
      ;;
      (cl-assert menu-keymap)
      (yas--delete-from-keymap menu-keymap (yas--template-uuid template))

      ;; Add necessary subgroups as necessary.
      ;;
      (dolist (subgroup group)
        (let ((subgroup-keymap (lookup-key menu-keymap (vector (make-symbol subgroup)))))
          (unless (and subgroup-keymap
                       (keymapp subgroup-keymap))
            (setq subgroup-keymap (make-sparse-keymap))
            (define-key menu-keymap (vector (make-symbol subgroup))
              `(menu-item ,subgroup ,subgroup-keymap)))
          (setq menu-keymap subgroup-keymap)))

      ;; Add this entry to the keymap
      ;;
      (define-key menu-keymap
        (vector (make-symbol (yas--template-uuid template)))
        (car (yas--template-menu-binding-pair template))))))

(defun yas--namehash-templates-alist (namehash)
  "Return NAMEHASH as an alist."
  (let (alist)
    (maphash #'(lambda (k v)
                 (push (cons k v) alist))
             namehash)
    alist))

(defun yas--fetch (table key)
  "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure."
  (let* ((keyhash (yas--table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas--filter-templates-by-condition (yas--namehash-templates-alist namehash)))))


;;; Filtering/condition logic

(defun yas--eval-condition (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition t))))
    (error (progn
             (yas--message 1 "Error in condition evaluation: %s" (error-message-string err))
             nil))))


(defun yas--filter-templates-by-condition (templates)
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas--template' structure.

This function implements the rules described in
`yas-buffer-local-condition'.  See that variables documentation."
  (let ((requirement (yas--require-template-specific-condition-p)))
    (if (eq requirement 'always)
        templates
      (cl-remove-if-not (lambda (pair)
                          (yas--template-can-expand-p
                           (yas--template-condition (cdr pair)) requirement))
                        templates))))

(defun yas--require-template-specific-condition-p ()
  "Decide if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas-buffer-local-condition)
      'always
    (let ((local-condition (or (and (consp yas-buffer-local-condition)
                                    (yas--eval-condition yas-buffer-local-condition))
                               yas-buffer-local-condition)))
      (when local-condition
        (if (eq local-condition t)
            t
          (and (consp local-condition)
               (eq 'require-snippet-condition (car local-condition))
               (symbolp (cdr local-condition))
               (cdr local-condition)))))))

(defun yas--template-can-expand-p (condition requirement)
  "Evaluate CONDITION and REQUIREMENT and return a boolean."
  (let* ((result (or (null condition)
                     (yas--eval-condition condition))))
    (cond ((eq requirement t)
           result)
          (t
           (eq requirement result)))))

(defun yas--table-templates (table)
  (when table
    (let ((acc (list)))
      (maphash #'(lambda (_key namehash)
                   (maphash #'(lambda (name template)
                                (push (cons name template) acc))
                            namehash))
               (yas--table-hash table))
      (maphash #'(lambda (uuid template)
                   (push (cons uuid template) acc))
               (yas--table-uuidhash table))
      (yas--filter-templates-by-condition acc))))

(defun yas--templates-for-key-at-point ()
  "Find `yas--template' objects for any trigger keys preceding point.
Returns (TEMPLATES START END). This function respects
`yas-key-syntaxes', which see."
  (save-excursion
    (let ((original (point))
          (methods yas-key-syntaxes)
          (templates)
          (method))
      (while (and methods
                  (not templates))
        (unless (eq method (car methods))
          ;; TRICKY: `eq'-ness test means we can only be here if
          ;; `method' is a function that returned `again', and hence
          ;; don't revert back to original position as per
          ;; `yas-key-syntaxes'.
          (goto-char original))
        (setq method (car methods))
        (cond ((stringp method)
               (skip-syntax-backward method)
               (setq methods (cdr methods)))
              ((functionp method)
               (unless (eq (funcall method original)
                           'again)
                 (setq methods (cdr methods))))
              (t
               (setq methods (cdr methods))
               (yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
        (let ((possible-key (buffer-substring-no-properties (point) original)))
          (save-excursion
            (goto-char original)
            (setq templates
                  (cl-mapcan (lambda (table)
                               (yas--fetch table possible-key))
                             (yas--get-snippet-tables))))))
      (when templates
        (list templates (point) original)))))

(defun yas--table-all-keys (table)
  "Get trigger keys of all active snippets in TABLE."
  (let ((acc))
    (maphash #'(lambda (key namehash)
                 (when (yas--filter-templates-by-condition (yas--namehash-templates-alist namehash))
                   (push key acc)))
             (yas--table-hash table))
    acc))

(defun yas--table-mode (table)
  (intern (yas--table-name table)))


;;; Internal functions and macros:

(defun yas--remove-misc-free-from-undo (old-undo-list)
  "Tries to work around Emacs Bug#30931.
Helper function for `yas--save-restriction-and-widen'."
  ;; If Bug#30931 is unfixed, we get (#<Lisp_Misc_Free> . INTEGER)
  ;; entries in the undo list.  If we call `type-of' on the
  ;; Lisp_Misc_Free object then Emacs aborts, so try to find it by
  ;; checking that its type is none of the expected ones.
  (when (consp buffer-undo-list)
    (let* ((prev buffer-undo-list)
           (undo-list prev))
      (while (and (consp undo-list)
                  ;; Only check new entries.
                  (not (eq undo-list old-undo-list)))
        (let ((entry (pop undo-list)))
          (when (consp entry)
            (let ((head (car entry)))
              (unless (or (stringp head)
                          (markerp head)
                          (integerp head)
                          (symbolp head)
                          (not (integerp (cdr entry))))
                ;; (message "removing misc free %S" entry)
                (setcdr prev undo-list)))))
        (setq prev undo-list)))))

(defmacro yas--save-restriction-and-widen (&rest body)
  "Equivalent to (save-restriction (widen) BODY).
Also tries to work around Emacs Bug#30931."
  (declare (debug (body)) (indent 0))
  ;; Disable garbage collection, since it could cause an abort.
  `(let ((gc-cons-threshold most-positive-fixnum)
         (old-undo-list buffer-undo-list))
     (prog1 (save-restriction
              (widen)
              ,@body)
       (yas--remove-misc-free-from-undo old-undo-list))))

(defun yas--eval-for-string (form)
  "Evaluate FORM and convert the result to string."
  (let ((debug-on-error (and (not (memq yas-good-grace '(t inline)))
                             debug-on-error)))
    (condition-case oops
        (save-excursion
          (yas--save-restriction-and-widen
            (save-match-data
              (let ((result (eval form t)))
                (when result
                  (format "%s" result))))))
      ((debug error) (error-message-string oops)))))

(defun yas--eval-for-effect (form)
  (yas--safely-call-fun (apply-partially #'eval form)))

(defun yas--read-lisp (string &optional nil-on-error)
  "Read STRING as a elisp expression and return it.

In case STRING in an invalid expression and NIL-ON-ERROR is nil,
return an expression that when evaluated will issue an error."
  (condition-case err
      (read string)
    (error (and (not nil-on-error)
                `(error (error-message-string ',err))))))

(defun yas--read-keybinding (keybinding)
  "Read KEYBINDING as a snippet keybinding, return a vector."
  (when (and keybinding
             (not (string-match "keybinding" keybinding)))
    (condition-case err
        (let ((res (or (and (string-match "^\\[.*\\]$" keybinding)
                            (read keybinding))
                       (read-kbd-macro keybinding 'need-vector))))
          res)
      (error
       (yas--message 2 "warning: keybinding \"%s\" invalid since %s."
                keybinding (error-message-string err))
       nil))))

(defun yas--table-get-create (mode)
  "Get or create the snippet table corresponding to MODE."
  (let ((table (gethash mode
                        yas--tables)))
    (unless table
      (setq table (yas--make-snippet-table (symbol-name mode)))
      (puthash mode table yas--tables)
      (push (cons (intern (format "yas--direct-%s" mode))
                  (yas--table-direct-keymap table))
            yas--direct-keymaps))
    table))

(defun yas--get-snippet-tables (&optional mode)
  "Get snippet tables for MODE.

MODE defaults to the current buffer's `major-mode'.

Return a list of `yas--table' objects.  The list of modes to
consider is returned by `yas--modes-to-activate'"
  (remove nil
          (mapcar #'(lambda (name)
                      (gethash name yas--tables))
                  (yas--modes-to-activate mode))))

(defun yas--menu-keymap-get-create (mode &optional parents)
  "Get or create the menu keymap for MODE and its PARENTS.

This may very well create a plethora of menu keymaps and arrange
them all in `yas--menu-table'"
  (let* ((menu-keymap (or (gethash mode yas--menu-table)
                          (puthash mode (make-sparse-keymap) yas--menu-table))))
    (mapc #'yas--menu-keymap-get-create parents)
    (define-key yas--minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,menu-keymap
                    :visible (yas--show-menu-p ',mode)))
    menu-keymap))


;;; Template-related and snippet loading functions

(defun yas--parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Optional GROUP is the group where the template is to go,
otherwise we attempt to calculate it from FILE.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP VARS LOAD-FILE KEYBINDING UUID)

If the buffer contains a line of \"# --\" then the contents above
this line are ignored. Directives can set most of these with the syntax:

# directive-name : directive-value

Here's a list of currently recognized directives:

 * type
 * name
 * contributor
 * condition
 * group
 * key
 * expand-env
 * binding
 * uuid"
  (goto-char (point-min))
  (let* ((type 'snippet)
         (name (and file
                    (file-name-nondirectory file)))
         (key nil)
         template
         bound
         condition
         (group (and file
                     (yas--calculate-group file)))
         expand-env
         binding
         uuid)
    (if (re-search-forward "^# --\\s-*\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*?\\)[[:space:]]*$" bound t)
                 (when (string= "uuid" (match-string-no-properties 1))
                   (setq uuid (match-string-no-properties 2)))
                 (when (string= "type" (match-string-no-properties 1))
                   (setq type (if (string= "command" (match-string-no-properties 2))
                                  'command
                                'snippet)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (yas--read-lisp (match-string-no-properties 2))))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "expand-env" (match-string-no-properties 1))
                   (setq expand-env (yas--read-lisp (match-string-no-properties 2)
                                                   'nil-on-error)))
                 (when (string= "binding" (match-string-no-properties 1))
                   (setq binding (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (unless (or key binding)
      (setq key (and file (file-name-nondirectory file))))
    (when (eq type 'command)
      (setq template (yas--read-lisp (concat "(progn" template ")"))))
    (when group
      (setq group (split-string group "\\.")))
    (list key template name condition group expand-env file binding uuid)))

(defun yas--calculate-group (file)
  "Calculate the group for snippet file path FILE."
  (let* ((dominating-dir (locate-dominating-file file
                                                 ".yas-make-groups"))
         (extra-path (and dominating-dir
                          (file-relative-name file dominating-dir)))
         (extra-dir (and extra-path
                         (file-name-directory extra-path)))
         (group (and extra-dir
                     (replace-regexp-in-string "/"
                                               "."
                                               (directory-file-name extra-dir)))))
    group))

(defun yas--subdirs (directory &optional filep)
  "Return subdirs or files of DIRECTORY according to FILEP."
  (cl-remove-if (lambda (file)
                  (or (string-match "\\`\\."
                                    (file-name-nondirectory file))
                      (string-match "\\`#.*#\\'"
                                    (file-name-nondirectory file))
                      (string-match "~\\'"
                                    (file-name-nondirectory file))
                      (if filep
                          (file-directory-p file)
                        (not (file-directory-p file)))))
                (directory-files directory t)))

(defun yas--make-menu-binding (template)
  (let ((mode (yas--table-mode (yas--template-table template))))
    `(lambda () (interactive) (yas--expand-or-visit-from-menu ',mode ,(yas--template-uuid template)))))

(defun yas--expand-or-visit-from-menu (mode uuid)
  (let* ((table (yas--table-get-create mode))
         (yas--current-template (and table
                                    (gethash uuid (yas--table-uuidhash table)))))
    (when yas--current-template
      (if yas-visit-from-menu
          (yas--visit-snippet-file-1 yas--current-template)
        (let ((where (if (region-active-p)
                         (cons (region-beginning) (region-end))
                       (cons (point) (point)))))
          (yas-expand-snippet yas--current-template
                              (car where) (cdr where)))))))

(defun yas--key-from-desc (text)
  "Return a yasnippet key from a description string TEXT."
  (replace-regexp-in-string "\\(\\w+\\).*" "\\1" text))


;;; Popping up for keys and templates

(defun yas--prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas--template'.

Optional PROMPT sets the prompt to use."
  (when templates
    (setq templates
          (sort templates #'(lambda (t1 t2)
                              (< (length (yas--template-name t1))
                                 (length (yas--template-name t2))))))
    (cl-some (lambda (fn)
               (funcall fn (or prompt "Choose a snippet: ")
                        templates
                        #'yas--template-name))
             yas-prompt-functions)))

(defun yas--prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS.

Optional PROMPT sets the prompt to use."
  (when keys
    (cl-some (lambda (fn)
               (funcall fn (or prompt "Choose a snippet key: ") keys))
             yas-prompt-functions)))

(defun yas--prompt-for-table (tables &optional prompt)
  "Interactively choose a table from the list TABLES.

Optional PROMPT sets the prompt to use."
  (when tables
    (cl-some (lambda (fn)
               (funcall fn (or prompt "Choose a snippet table: ")
                        tables
                        #'yas--table-name))
             yas-prompt-functions)))

(defun yas-x-prompt (prompt choices &optional display-fn)
  "Display choices in a x-window prompt."
  (when (and window-system choices)
    ;; Let window position be recalculated to ensure that
    ;; `posn-at-point' returns non-nil.
    (redisplay)
    (or
     (x-popup-menu
      (if (fboundp 'posn-at-point)
          (let ((x-y (posn-x-y (posn-at-point (point)))))
            (list (list (+ (car x-y) 10)
                        (+ (cdr x-y) 20))
                  (selected-window)))
        t)
      `(,prompt ("title"
                 ,@(cl-mapcar (lambda (c d) `(,(concat "   " d) . ,c))
                              choices
                              (if display-fn (mapcar display-fn choices)
                                choices)))))
     (keyboard-quit))))

(defun yas-maybe-ido-prompt (prompt choices &optional display-fn)
  (when (bound-and-true-p ido-mode)
    (yas-ido-prompt prompt choices display-fn)))

(defun yas-ido-prompt (prompt choices &optional display-fn)
  (require 'ido)
  (yas-completing-prompt prompt choices display-fn #'ido-completing-read))

(defun yas-dropdown-prompt (_prompt choices &optional display-fn)
  (when (fboundp 'dropdown-list)
    (let* ((formatted-choices
            (if display-fn (mapcar display-fn choices) choices))
           (n (dropdown-list formatted-choices)))
      (if n (nth n choices)
        (keyboard-quit)))))

(defun yas-completing-prompt (prompt choices &optional display-fn completion-fn)
  (let* ((formatted-choices
          (if display-fn (mapcar display-fn choices) choices))
         (chosen (funcall (or completion-fn #'completing-read)
                          prompt formatted-choices
                          nil 'require-match nil nil)))
    (if (eq choices formatted-choices)
        chosen
      (nth (or (cl-position chosen formatted-choices :test #'string=) 0)
           choices))))

(defun yas-no-prompt (_prompt choices &optional _display-fn)
  (cl-first choices))


;;; Defining snippets
;; This consists of creating and registering `yas--template' objects in the
;; correct tables.
;;

(defvar yas--creating-compiled-snippets nil)

(defun yas--define-snippets-1 (snippet snippet-table)
  "Helper for `yas-define-snippets'."
  ;; Update the appropriate table.  Also takes care of adding the
  ;; key indicators in the templates menu entry, if any.
  (yas--update-template
   snippet-table (apply #'yas--define-snippets-2 snippet-table snippet)))

(defun yas-define-snippets (mode snippets)
  "Define SNIPPETS for MODE.

SNIPPETS is a list of snippet definitions, each taking the
following form

 (KEY TEMPLATE
  NAME CONDITION GROUP EXPAND-ENV LOAD-FILE KEYBINDING UUID SAVE-FILE)

Within these, only KEY and TEMPLATE are actually mandatory.

TEMPLATE might be a Lisp form or a string, depending on whether
this is a snippet or a snippet-command.

CONDITION, EXPAND-ENV and KEYBINDING are Lisp forms, they have
been `yas--read-lisp'-ed and will eventually be
`yas--eval-for-string'-ed.

The remaining elements are strings.

FILE is probably of very little use if you're programatically
defining snippets.

UUID is the snippet's \"unique-id\". Loading a second snippet
file with the same uuid would replace the previous snippet.

You can use `yas--parse-template' to return such lists based on
the current buffers contents."
  (if yas--creating-compiled-snippets
      (let ((print-length nil))
        (insert ";;; Snippet definitions:\n;;;\n")
        (dolist (snippet snippets)
          ;; Fill in missing elements with nil.
          (setq snippet (append snippet (make-list (- 10 (length snippet)) nil)))
          ;; Move LOAD-FILE to SAVE-FILE because we will load from the
          ;; compiled file, not LOAD-FILE.
          (let ((load-file (nth 6 snippet)))
            (setcar (nthcdr 6 snippet) nil)
            (setcar (nthcdr 9 snippet) load-file)))
        (insert (pp-to-string
                 `(yas-define-snippets ',mode ',snippets)))
        (insert "\n\n"))
    ;; Normal case.
    (let ((snippet-table (yas--table-get-create mode))
          (template nil))
      (dolist (snippet snippets)
        (setq template (yas--define-snippets-1 snippet
                                               snippet-table)))
      template)))


;;; Loading snippets from files

(defun yas--template-get-file (template)
  "Return TEMPLATE's LOAD-FILE or SAVE-FILE."
  (or (yas--template-load-file template)
      (let ((file (yas--template-save-file template)))
        (when file
          (yas--message 3 "%s has no load file, using save file, %s, instead."
                        (yas--template-name template) file))
        file)))

(defun yas--load-yas-setup-file (file)
  (if (not yas--creating-compiled-snippets)
      ;; Normal case.
      (load file 'noerror (<= yas-verbosity 4))
    (let ((elfile (concat file ".el")))
      (when (file-exists-p elfile)
        (insert ";;; contents of the .yas-setup.el support file:\n;;;\n")
        (insert-file-contents elfile)
        (goto-char (point-max))))))

(defun yas--define-parents (mode parents)
  "Add PARENTS to the list of MODE's parents."
  (puthash mode (cl-remove-duplicates
                 (append parents
                         (gethash mode yas--parents)))
           yas--parents))

(defun yas-load-directory (top-level-dir &optional use-jit interactive)
  "Load snippets in directory hierarchy TOP-LEVEL-DIR.

Below TOP-LEVEL-DIR each directory should be a mode name.

With prefix argument USE-JIT do jit-loading of snippets."
  (interactive
   (list (read-directory-name "Select the root directory: " nil nil t)
         current-prefix-arg t))
  (unless yas-snippet-dirs
    (setq yas-snippet-dirs top-level-dir))
  (let ((impatient-buffers))
    (dolist (dir (yas--subdirs top-level-dir))
      (let* ((major-mode-and-parents (yas--compute-major-mode-and-parents
                                      (concat dir "/dummy")))
             (mode-sym (car major-mode-and-parents))
             (parents (cdr major-mode-and-parents)))
        ;; Attention: The parents and the menus are already defined
        ;; here, even if the snippets are later jit-loaded.
        ;;
        ;; * We need to know the parents at this point since entering a
        ;;   given mode should jit load for its parents
        ;;   immediately. This could be reviewed, the parents could be
        ;;   discovered just-in-time-as well
        ;;
        ;; * We need to create the menus here to support the `full'
        ;;   option to `yas-use-menu' (all known snippet menus are shown to the user)
        ;;
        (yas--define-parents mode-sym parents)
        (yas--menu-keymap-get-create mode-sym)
        (let ((fun (apply-partially #'yas--load-directory-1 dir mode-sym)))
          (if use-jit
              (yas--schedule-jit mode-sym fun)
            (funcall fun)))
        ;; Look for buffers that are already in `mode-sym', and so
        ;; need the new snippets immediately...
        ;;
        (when use-jit
          (cl-loop for buffer in (buffer-list)
                   do (with-current-buffer buffer
                        (when (eq major-mode mode-sym)
                          (yas--message 4 "Discovered there was already %s in %s" buffer mode-sym)
                          (push buffer impatient-buffers)))))))
    ;; ...after TOP-LEVEL-DIR has been completely loaded, call
    ;; `yas--load-pending-jits' in these impatient buffers.
    ;;
    (cl-loop for buffer in impatient-buffers
             do (with-current-buffer buffer (yas--load-pending-jits))))
  (when interactive
    (yas--message 3 "Loaded snippets from %s." top-level-dir)))

(defun yas--load-directory-1 (directory mode-sym)
  "Recursively load snippet templates from DIRECTORY."
  (if yas--creating-compiled-snippets
      (let ((output-file (expand-file-name ".yas-compiled-snippets.el"
                                           directory)))
        (with-temp-file output-file
          (insert (format ";;; Compiled snippets and support files for `%s'\n"
                          mode-sym))
          (yas--load-directory-2 directory mode-sym)
          (insert (format ";;; Do not edit! File generated at %s\n"
                          (current-time-string)))))
    ;; Normal case.
    (unless (file-exists-p (expand-file-name ".yas-skip" directory))
      (unless (and (load (expand-file-name ".yas-compiled-snippets" directory) 'noerror (<= yas-verbosity 3))
                   (progn (yas--message 4 "Loaded compiled snippets from %s" directory) t))
        (yas--message 4 "Loading snippet files from %s" directory)
        (yas--load-directory-2 directory mode-sym)))))

(defun yas--load-directory-2 (directory mode-sym)
  ;; Load .yas-setup.el files wherever we find them
  ;;
  (yas--load-yas-setup-file (expand-file-name ".yas-setup" directory))
  (let* ((default-directory directory)
         (snippet-defs nil))
    ;; load the snippet files
    ;;
    (with-temp-buffer
      (dolist (file (yas--subdirs directory 'no-subdirs-just-files))
        (when (file-readable-p file)
          ;; Erase the buffer instead of passing non-nil REPLACE to
          ;; `insert-file-contents' (avoids Emacs bug #23659).
          (erase-buffer)
          (insert-file-contents file)
          (push (yas--parse-template file)
                snippet-defs))))
    (when snippet-defs
      (yas-define-snippets mode-sym
                           snippet-defs))
    ;; now recurse to a lower level
    ;;
    (dolist (subdir (yas--subdirs directory))
      (yas--load-directory-2 subdir
                            mode-sym))))

(defun yas--load-snippet-dirs (&optional nojit)
  "Reload the directories listed in `yas-snippet-dirs' or
prompt the user to select one."
  (let (errors)
    (if (null yas-snippet-dirs)
        (call-interactively 'yas-load-directory)
      (when (member yas--default-user-snippets-dir yas-snippet-dirs)
        (make-directory yas--default-user-snippets-dir t))
      (dolist (directory (reverse (yas-snippet-dirs)))
        (cond ((file-directory-p directory)
               (yas-load-directory directory (not nojit))
               (if nojit
                   (yas--message 4 "Loaded %s" directory)
                 (yas--message 4 "Prepared just-in-time loading for %s" directory)))
              (t
               (push (yas--message 1 "Check your `yas-snippet-dirs': %s is not a directory" directory) errors)))))
    errors))

(defun yas-reload-all (&optional no-jit interactive)
  "Reload all snippets and rebuild the YASnippet menu.

When NO-JIT is non-nil force immediate reload of all known
snippets under `yas-snippet-dirs', otherwise use just-in-time
loading.

When called interactively, use just-in-time loading when given a
prefix argument."
  (interactive (list (not current-prefix-arg) t))
  (catch 'abort
    (let ((errors)
          (snippet-editing-buffers
           (cl-remove-if-not (lambda (buffer)
                               (with-current-buffer buffer
                                 yas--editing-template))
                             (buffer-list))))
      ;; Warn if there are buffers visiting snippets, since reloading will break
      ;; any on-line editing of those buffers.
      ;;
      (when snippet-editing-buffers
          (if interactive
              (if (y-or-n-p "Some buffers editing live snippets, close them and proceed with reload? ")
                  (mapc #'kill-buffer snippet-editing-buffers)
                (yas--message 1 "Aborted reload...")
                (throw 'abort nil))
            ;; in a non-interactive use, at least set
            ;; `yas--editing-template' to nil, make it guess it next time around
            (mapc #'(lambda (buffer)
                      (with-current-buffer buffer
                        (kill-local-variable 'yas--editing-template)))
                  (buffer-list))))

      ;; Empty all snippet tables and parenting info
      ;;
      (setq yas--tables (make-hash-table))
      (setq yas--parents (make-hash-table))

      ;; Before killing `yas--menu-table' use its keys to cleanup the
      ;; mode menu parts of `yas--minor-mode-menu' (thus also cleaning
      ;; up `yas-minor-mode-map', which points to it)
      ;;
      (maphash #'(lambda (menu-symbol _keymap)
                   (define-key yas--minor-mode-menu (vector menu-symbol) nil))
               yas--menu-table)
      ;; Now empty `yas--menu-table' as well
      (setq yas--menu-table (make-hash-table))

      ;; Cancel all pending 'yas--scheduled-jit-loads'
      ;;
      (setq yas--scheduled-jit-loads (make-hash-table))

      ;; Reload the directories listed in `yas-snippet-dirs' or prompt
      ;; the user to select one.
      ;;
      (setq errors (yas--load-snippet-dirs no-jit))
      ;; Reload the direct keybindings
      ;;
      (yas-direct-keymaps-reload)

      (run-hooks 'yas-after-reload-hook)
      (let ((no-snippets
             (cl-every (lambda (table) (= (hash-table-count table) 0))
                       (list yas--scheduled-jit-loads
                             yas--parents yas--tables))))
        (yas--message (if (or no-snippets errors) 2 3)
                      (if no-jit "Snippets loaded %s."
                        "Prepared just-in-time loading of snippets %s.")
                      (cond (errors
                             "with some errors.  Check *Messages*")
                            (no-snippets
                             "(but no snippets found)")
                            (t
                             "successfully")))))))

(defvar yas-after-reload-hook nil
  "Hook run after `yas-reload-all'.")

(defun yas--load-pending-jits ()
  (dolist (mode (yas--modes-to-activate))
    (let ((funs (reverse (gethash mode yas--scheduled-jit-loads))))
      ;; must reverse to maintain coherence with `yas-snippet-dirs'
      (dolist (fun funs)
        (yas--message 4 "Loading for `%s', just-in-time: %s!" mode fun)
        (funcall fun))
      (remhash mode yas--scheduled-jit-loads))))

(defun yas-escape-text (text)
  "Escape TEXT for snippet."
  (when text
    (replace-regexp-in-string "[\\$]" "\\\\\\&" text)))


;;; Snippet compilation function

(defun yas-compile-directory (top-level-dir)
  "Create .yas-compiled-snippets.el files under subdirs of TOP-LEVEL-DIR.

This works by stubbing a few functions, then calling
`yas-load-directory'."
  (interactive "DTop level snippet directory?")
  (let ((yas--creating-compiled-snippets t))
    (yas-load-directory top-level-dir nil)))

(defun yas-recompile-all ()
  "Compile every dir in `yas-snippet-dirs'."
  (interactive)
  (mapc #'yas-compile-directory (yas-snippet-dirs)))


;;; JIT loading
;;;

(defvar yas--scheduled-jit-loads (make-hash-table)
  "Alist of mode-symbols to forms to be evaled when `yas-minor-mode' kicks in.")

(defun yas--schedule-jit (mode fun)
  (push fun (gethash mode yas--scheduled-jit-loads)))



;;; Some user level functions

(defun yas-about ()
  (interactive)
  (message "yasnippet (version %s) -- pluskid/joaotavora/npostavs"
           (or (ignore-errors (car (let ((default-directory yas--loaddir))
                                     (process-lines "git" "describe"
                                                    "--tags" "--dirty"))))
               (when (and (featurep 'package)
                          (fboundp 'package-desc-version)
                          (fboundp 'package-version-join))
                 (defvar package-alist)
                 (ignore-errors
                   (let* ((yas-pkg (cdr (assq 'yasnippet package-alist)))
                          (version (package-version-join
                                    (package-desc-version (car yas-pkg)))))
                     ;; Special case for MELPA's bogus version numbers.
                     (if (string-match "\\`20..[01][0-9][0-3][0-9][.][0-9]\\{3,4\\}\\'"
                                       version)
                         (concat yas--version "-snapshot" version)
                       version))))
               yas--version)))


;;; Apropos snippet menu:
;;
;; The snippet menu keymaps are stored by mode in hash table called
;; `yas--menu-table'. They are linked to the main menu in
;; `yas--menu-keymap-get-create' and are initially created empty,
;; reflecting the table hierarchy.
;;
;; They can be populated in two mutually exclusive ways: (1) by
;; reading `yas--template-group', which in turn is populated by the "#
;; group:" directives of the snippets or the ".yas-make-groups" file
;; or (2) by using a separate `yas-define-menu' call, which declares a
;; menu structure based on snippets uuids.
;;
;; Both situations are handled in `yas--update-template-menu', which
;; uses the predicate `yas--template-menu-managed-by-yas-define-menu'
;; that can tell between the two situations.
;;
;; Note:
;;
;; * if `yas-define-menu' is used it must run before
;;   `yas-define-snippets' and the UUIDS must match, otherwise we get
;;   duplicate entries. The `yas--template' objects are created in
;;   `yas-define-menu', holding nothing but the menu entry,
;;   represented by a pair of ((menu-item NAME :keys KEYS) TYPE) and
;;   stored in `yas--template-menu-binding-pair'.  The (menu-item ...)
;;   part is then stored in the menu keymap itself which make the item
;;   appear to the user.  These limitations could probably be revised.
;;
;; * The `yas--template-perm-group' slot is only used in
;;   `yas-describe-tables'.
;;
(defun yas--template-menu-binding-pair-get-create (template &optional type)
  "Get TEMPLATE's menu binding or assign it a new one.

TYPE may be `:stay', signaling this menu binding should be
static in the menu."
  (or (yas--template-menu-binding-pair template)
      (let (;; (key (yas--template-key template))
            ;; (keybinding (yas--template-keybinding template))
            )
        (setf (yas--template-menu-binding-pair template)
              (cons `(menu-item ,(or (yas--template-name template)
                                     (yas--template-uuid template))
                                ,(yas--make-menu-binding template)
                                :keys ,nil)
                    type)))))
(defun yas--template-menu-managed-by-yas-define-menu (template)
  "Non-nil if TEMPLATE's menu entry was included in a `yas-define-menu' call."
  (cdr (yas--template-menu-binding-pair template)))


(defun yas--show-menu-p (mode)
  (cond ((eq yas-use-menu 'abbreviate)
         (cl-find mode
                  (mapcar #'yas--table-mode
                          (yas--get-snippet-tables))))
        (yas-use-menu t)))

(defun yas--delete-from-keymap (keymap uuid)
  "Recursively delete items with UUID from KEYMAP and its submenus."

  ;; XXX: This used to skip any submenus named \"parent mode\"
  ;;
  ;; First of all, recursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;;
  (mapc #'(lambda (item)
            (when (and (consp (cdr-safe item))
                       (keymapp (nth 2 (cdr item))))
              (yas--delete-from-keymap (nth 2 (cdr item)) uuid)))
        (cdr keymap))
  ;; Set the uuid entry to nil
  ;;
  (define-key keymap (vector (make-symbol uuid)) nil)
  ;; Destructively modify keymap
  ;;
  (setcdr keymap (cl-delete-if (lambda (item)
                                 (cond ((not (listp item)) nil)
                                       ((null (cdr item)))
                                       ((and (keymapp (nth 2 (cdr item)))
                                             (null (cdr (nth 2 (cdr item))))))))
                               (cdr keymap))))

(defun yas-define-menu (mode menu &optional omit-items)
  "Define a snippet menu for MODE according to MENU, omitting OMIT-ITEMS.

MENU is a list, its elements can be:

- (yas-item UUID) : Creates an entry the snippet identified with
  UUID.  The menu entry for a snippet thus identified is
  permanent, i.e. it will never move (be reordered) in the menu.

- (yas-separator) : Creates a separator

- (yas-submenu NAME SUBMENU) : Creates a submenu with NAME,
  SUBMENU has the same form as MENU.  NAME is also added to the
  list of groups of the snippets defined thereafter.

OMIT-ITEMS is a list of snippet uuids that will always be
omitted from MODE's menu, even if they're manually loaded."
  (let* ((table (yas--table-get-create mode))
         (hash (yas--table-uuidhash table)))
    (yas--define-menu-1 table
                        (yas--menu-keymap-get-create mode)
                        menu
                        hash)
    (dolist (uuid omit-items)
      (let ((template (or (gethash uuid hash)
                          (puthash uuid
                                   (yas--make-template :table table
                                                       :uuid uuid)
                                   hash))))
        (setf (yas--template-menu-binding-pair template) (cons nil :none))))))

(defun yas--define-menu-1 (table menu-keymap menu uuidhash &optional group-list)
  "Helper for `yas-define-menu'."
  (cl-loop
   for (type name submenu) in (reverse menu)
   collect (cond
            ((or (eq type 'yas-item)
                 (and yas-alias-to-yas/prefix-p
                      (eq type 'yas/item)))
             (let ((template (or (gethash name uuidhash)
                                 (puthash name
                                          (yas--make-template
                                           :table table
                                           :perm-group group-list
                                           :uuid name)
                                          uuidhash))))
               (car (yas--template-menu-binding-pair-get-create
                     template :stay))))
            ((or (eq type 'yas-submenu)
                 (and yas-alias-to-yas/prefix-p
                      (eq type 'yas/submenu)))
             (let ((subkeymap (make-sparse-keymap)))
               (yas--define-menu-1 table subkeymap submenu uuidhash
                                   (append group-list (list name)))
               `(menu-item ,name ,subkeymap)))
            ((or (eq type 'yas-separator)
                 (and yas-alias-to-yas/prefix-p
                      (eq type 'yas/separator)))
             '(menu-item "----"))
            (t (yas--message 1 "Don't know anything about menu entry %s" type)
               nil))
   into menu-entries
   finally do (push (apply #'vector menu-entries) (cdr menu-keymap))))

(defun yas--define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.

NAME is a description to this template.  Also update the menu if
`yas-use-menu' is t.  CONDITION is the condition attached to
this snippet.  If you attach a condition to a snippet, then it
will only be expanded when the condition evaluated to non-nil."
  (yas-define-snippets mode
                       (list (list key template name condition group))))

(defun yas-hippie-try-expand (first-time?)
  "Integrate with hippie expand.

Just put this function in `hippie-expand-try-functions-list'."
  (when yas-minor-mode
    (if (not first-time?)
        (let ((yas-fallback-behavior 'return-nil))
          (yas-expand))
      (undo 1)
      nil)))


;;; Apropos condition-cache:
;;;
;;;
;;;
;;;
(defmacro yas-define-condition-cache (func doc &rest body)
  "Define a function FUNC with doc DOC and body BODY.
BODY is executed at most once every snippet expansion attempt, to check
expansion conditions.

It doesn't make any sense to call FUNC programatically."
  `(defun ,func () ,(if (and doc
                             (stringp doc))
                        (concat doc
"\n\nFor use in snippets' conditions. Within each
snippet-expansion routine like `yas-expand', computes actual
value for the first time then always returns a cached value.")
                      (setq body (cons doc body))
                      nil)
     (let ((timestamp-and-value (get ',func 'yas--condition-cache)))
       (if (equal (car timestamp-and-value) yas--condition-cache-timestamp)
           (cdr timestamp-and-value)
         (let ((new-value (progn
                            ,@body
                            )))
           (put ',func 'yas--condition-cache (cons yas--condition-cache-timestamp new-value))
           new-value)))))

(defalias 'yas-expand #'yas-expand-from-trigger-key)
(defun yas-expand-from-trigger-key (&optional field)
  "Expand a snippet before point.

If no snippet expansion is possible, fall back to the behaviour
defined in `yas-fallback-behavior'.

Optional argument FIELD is for non-interactive use and is an
object satisfying `yas--field-p' to restrict the expansion to."
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (if templates-and-pos
        (yas--expand-or-prompt-for-template
         (nth 0 templates-and-pos)
         ;; Delete snippet key and active region when expanding.
         (min (if (use-region-p) (region-beginning) most-positive-fixnum)
              (nth 1 templates-and-pos))
         (max (if (use-region-p) (region-end) most-negative-fixnum)
              (nth 2 templates-and-pos)))
      (yas--fallback))))

(defun yas--maybe-expand-from-keymap-filter (cmd)
  "Check whether a snippet may be expanded.
If there are expandable snippets, return CMD (this is useful for
conditional keybindings) or the list of expandable snippet
template objects if CMD is nil (this is useful as a more general predicate)."
  (let* ((yas--condition-cache-timestamp (current-time))
         (vec (cl-subseq (this-command-keys-vector)
                         (if current-prefix-arg
                             (length (this-command-keys))
                           0)))
         (templates (cl-mapcan (lambda (table)
                                 (yas--fetch table vec))
                               (yas--get-snippet-tables))))
    (if templates (or cmd templates))))

(defun yas-expand-from-keymap ()
  "Directly expand some snippets, searching `yas--direct-keymaps'."
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let* ((templates (yas--maybe-expand-from-keymap-filter nil)))
    (when templates
      (yas--expand-or-prompt-for-template templates))))

(defun yas--expand-or-prompt-for-template (templates &optional start end)
  "Expand one of TEMPLATES from START to END.

Prompt the user if TEMPLATES has more than one element, else
expand immediately.  Common gateway for
`yas-expand-from-trigger-key' and `yas-expand-from-keymap'."
  (let ((yas--current-template
         (or (and (cl-rest templates) ;; more than one
                  (yas--prompt-for-template (mapcar #'cdr templates)))
             (cdar templates))))
    (when yas--current-template
      (yas-expand-snippet yas--current-template start end))))

;; Apropos the trigger key and the fallback binding:
;;
;; When `yas-minor-mode-map' binds <tab>, that correctly overrides
;; org-mode's <tab>, for example and searching for fallbacks correctly
;; returns `org-cycle'. However, most other modes bind "TAB". TODO,
;; improve this explanation.
;;
(defun yas--fallback ()
  "Fallback after expansion has failed.

Common gateway for `yas-expand-from-trigger-key' and
`yas-expand-from-keymap'."
  (cond ((eq yas-fallback-behavior 'return-nil)
         ;; return nil
         nil)
        ((eq yas-fallback-behavior 'yas--fallback)
         (error (concat "yasnippet fallback loop!\n"
                        "This can happen when you bind `yas-expand' "
                        "outside of the `yas-minor-mode-map'.")))
        ((eq yas-fallback-behavior 'call-other-command)
         (let* ((yas-fallback-behavior 'yas--fallback)
                ;; Also bind `yas-minor-mode' to prevent fallback
                ;; loops when other extensions use mechanisms similar
                ;; to `yas--keybinding-beyond-yasnippet'. (github #525
                ;; and #526)
                ;;
                (yas-minor-mode nil)
                (beyond-yasnippet (yas--keybinding-beyond-yasnippet)))
           (yas--message 4 "Falling back to %s"  beyond-yasnippet)
           (cl-assert (or (null beyond-yasnippet) (commandp beyond-yasnippet)))
           (setq this-command beyond-yasnippet)
           (when beyond-yasnippet
             (call-interactively beyond-yasnippet))))
        ((and (listp yas-fallback-behavior)
              (cdr yas-fallback-behavior)
              (eq 'apply (car yas-fallback-behavior)))
         (let ((command-or-fn (cadr yas-fallback-behavior))
               (args (cddr yas-fallback-behavior))
               (yas-fallback-behavior 'yas--fallback)
               (yas-minor-mode nil))
           (if args
               (apply command-or-fn args)
             (when (commandp command-or-fn)
               (setq this-command command-or-fn)
               (call-interactively command-or-fn)))))
        (t
         ;; also return nil if all the other fallbacks have failed
         nil)))

(defun yas--keybinding-beyond-yasnippet ()
  "Get current keys's binding as if YASsnippet didn't exist."
  (let* ((yas-minor-mode nil)
         (yas--direct-keymaps nil)
         (keys (this-single-command-keys)))
    (or (key-binding keys t)
        (key-binding (yas--fallback-translate-input keys) t))))

(defun yas--fallback-translate-input (keys)
  "Emulate `read-key-sequence', at least what I think it does.

Keys should be an untranslated key vector.  Returns a translated
vector of keys.  FIXME not thoroughly tested."
  (let ((retval [])
        (i 0))
    (while (< i (length keys))
      (let ((j i)
            (translated local-function-key-map))
        (while (and (< j (length keys))
                    translated
                    (keymapp translated))
          (setq translated (cdr (assoc (aref keys j) (remove 'keymap translated)))
                j (1+ j)))
        (setq retval (vconcat retval (cond ((symbolp translated)
                                            `[,translated])
                                           ((vectorp translated)
                                            translated)
                                           (t
                                            (substring keys i j)))))
        (setq i j)))
    retval))


;;; Utils for snippet development:

(defun yas--all-templates (tables)
  "Get `yas--template' objects in TABLES, applicable for buffer and point.

Honours `yas-choose-tables-first', `yas-choose-keys-first' and
`yas-buffer-local-condition'"
  (when yas-choose-tables-first
    (setq tables (list (yas--prompt-for-table tables))))
  (mapcar #'cdr
          (if yas-choose-keys-first
              (let ((key (yas--prompt-for-keys
                          (cl-mapcan #'yas--table-all-keys tables))))
                (when key
                  (cl-mapcan (lambda (table)
                               (yas--fetch table key))
                             tables)))
            (cl-remove-duplicates (cl-mapcan #'yas--table-templates tables)
                                  :test #'equal))))

(defun yas--lookup-snippet-1 (name mode)
  "Get the snippet called NAME in MODE's tables."
  (let ((yas-choose-tables-first nil)   ; avoid prompts
        (yas-choose-keys-first nil))
    (cl-find name (yas--all-templates
                   (yas--get-snippet-tables mode))
             :key #'yas--template-name :test #'string=)))

(defun yas-lookup-snippet (name &optional mode noerror)
  "Get the snippet named NAME in MODE's tables.

MODE defaults to the current buffer's `major-mode'.  If NOERROR
is non-nil, then don't signal an error if there isn't any snippet
called NAME.

Honours `yas-buffer-local-condition'."
  (cond
   ((yas--lookup-snippet-1 name mode))
   (noerror nil)
   (t (error "No snippet named: %s" name))))

(defun yas-insert-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas-prompt-functions'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (setq yas--condition-cache-timestamp (current-time))
  (let* ((yas-buffer-local-condition (or (and no-condition
                                              'always)
                                         yas-buffer-local-condition))
         (templates (yas--all-templates (yas--get-snippet-tables)))
         (yas--current-template (and templates
                                    (or (and (cl-rest templates) ;; more than one template for same key
                                             (yas--prompt-for-template templates))
                                        (car templates))))
         (where (if (region-active-p)
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (if yas--current-template
        (yas-expand-snippet yas--current-template (car where) (cdr where))
      (yas--message 1 "No snippets can be inserted here!"))))

(defun yas-visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas-insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas-buffer-local-condition 'always)
         (templates (yas--all-templates (yas--get-snippet-tables)))
         (template (and templates
                        (or (yas--prompt-for-template templates
                                                     "Choose a snippet template to edit: ")
                            (car templates)))))

    (if template
        (yas--visit-snippet-file-1 template)
      (message "No snippets tables active!"))))

(defun yas--visit-snippet-file-1 (template)
  "Helper for `yas-visit-snippet-file'."
  (let ((file (yas--template-get-file template)))
    (cond ((and file (file-readable-p file))
           (find-file-other-window file)
           (snippet-mode)
           (set (make-local-variable 'yas--editing-template) template))
          (file
           (message "Original file %s no longer exists!" file))
          (t
           (switch-to-buffer (format "*%s*"(yas--template-name template)))
           (let ((type 'snippet))
             (when (listp (yas--template-content template))
               (insert (format "# type: command\n"))
               (setq type 'command))
             (insert (format "# key: %s\n" (yas--template-key template)))
             (insert (format "# name: %s\n" (yas--template-name template)))
             (when (yas--template-keybinding template)
               (insert (format "# binding: %s\n" (yas--template-keybinding template))))
             (when (yas--template-expand-env template)
               (insert (format "# expand-env: %s\n" (yas--template-expand-env template))))
             (when (yas--template-condition template)
               (insert (format "# condition: %s\n" (yas--template-condition template))))
             (insert "# --\n")
             (insert (if (eq type 'command)
                         (pp-to-string (yas--template-content template))
                       (yas--template-content template))))
           (snippet-mode)
           (set (make-local-variable 'yas--editing-template) template)
           (set (make-local-variable 'default-directory)
                (car (cdr (car (yas--guess-snippet-directories (yas--template-table template))))))))))

(defun yas--guess-snippet-directories-1 (table)
  "Guess possible snippet subdirectories for TABLE."
  (cons (file-name-as-directory (yas--table-name table))
        (cl-mapcan #'yas--guess-snippet-directories-1
                   (yas--table-parents table))))

(defun yas--guess-snippet-directories (&optional table)
  "Try to guess suitable directories based on the current active
tables (or optional TABLE).

Returns a list of elements (TABLE . DIRS) where TABLE is a
`yas--table' object and DIRS is a list of all possible directories
where snippets of table might exist."
  (let ((main-dir (car (or (yas-snippet-dirs)
                           (setq yas-snippet-dirs
                                 (list yas--default-user-snippets-dir)))))
        (tables (if table (list table)
                  (yas--get-snippet-tables))))
    ;; HACK! the snippet table created here is actually registered!
    (unless table
      ;; The major mode is probably the best guess, put it first.
      (let ((major-mode-table (yas--table-get-create major-mode)))
        (cl-callf2 delq major-mode-table tables)
        (push major-mode-table tables)))

    (mapcar #'(lambda (table)
                (cons table
                      (mapcar #'(lambda (subdir)
                                  (expand-file-name subdir main-dir))
                              (yas--guess-snippet-directories-1 table))))
            tables)))

(defun yas--make-directory-maybe (table-and-dirs &optional main-table-string)
  "Return a dir inside TABLE-AND-DIRS, prompts for creation if none exists."
  (or (cl-some (lambda (dir) (when (file-directory-p dir) dir))
               (cdr table-and-dirs))
      (let ((candidate (cl-first (cdr table-and-dirs))))
        (unless (file-writable-p (file-name-directory candidate))
          (error (yas--format "%s is not writable." candidate)))
        (if (y-or-n-p (format "Guessed directory (%s) for%s%s table \"%s\" does not exist! Create? "
                              candidate
                              (if (gethash (yas--table-mode (car table-and-dirs))
                                           yas--tables)
                                  ""
                                " brand new")
                              (or main-table-string
                                  "")
                              (yas--table-name (car table-and-dirs))))
            (progn
              (make-directory candidate 'also-make-parents)
              ;; create the .yas-parents file here...
              candidate)))))

;; NOTE: Using the traditional "*new snippet*" stops whitespace mode
;; from activating (it doesn't like the leading "*").
(defconst yas-new-snippet-buffer-name "+new-snippet+")

(defun yas-new-snippet (&optional no-template)
  "Pops a new buffer for writing a snippet.

Expands a snippet-writing snippet, unless the optional prefix arg
NO-TEMPLATE is non-nil."
  (interactive "P")
  (let ((guessed-directories (yas--guess-snippet-directories))
        (yas-selected-text (or yas-selected-text
                               (and (region-active-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end))))))

    (switch-to-buffer yas-new-snippet-buffer-name)
    (erase-buffer)
    (kill-all-local-variables)
    (snippet-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas--guessed-modes)
         (mapcar (lambda (d) (yas--table-mode (car d)))
                 guessed-directories))
    (set (make-local-variable 'default-directory)
         (car (cdr (car guessed-directories))))
    (if (and (not no-template) yas-new-snippet-default)
        (yas-expand-snippet yas-new-snippet-default))))

(defun yas--compute-major-mode-and-parents (file)
  "Given FILE, find the nearest snippet directory for a given mode.

Returns a list (MODE-SYM PARENTS), the mode's symbol and a list
representing one or more of the mode's parents.

Note that MODE-SYM need not be the symbol of a real major mode,
neither do the elements of PARENTS."
  (let* ((file-dir (and file
                        (directory-file-name
                         (or (cl-some (lambda (special)
                                        (locate-dominating-file file special))
                                      '(".yas-setup.el"
                                        ".yas-make-groups"
                                        ".yas-parents"))
                             (directory-file-name (file-name-directory file))))))
         (parents-file-name (concat file-dir "/.yas-parents"))
         (major-mode-name (and file-dir
                               (file-name-nondirectory file-dir)))
         (major-mode-sym (or (and major-mode-name
                                  (intern major-mode-name))))
         (parents (when (file-readable-p parents-file-name)
                         (mapcar #'intern
                                 (split-string
                                  (with-temp-buffer
                                    (insert-file-contents parents-file-name)
                                    (buffer-substring-no-properties (point-min)
                                                                    (point-max))))))))
    (when major-mode-sym
      (cons major-mode-sym (remove major-mode-sym parents)))))

(defvar yas--editing-template nil
  "Supporting variable for `yas-load-snippet-buffer' and `yas--visit-snippet'.")

(defvar yas--current-template nil
  "Holds the current template being expanded into a snippet.")

(defvar yas--guessed-modes nil
  "List of guessed modes supporting `yas-load-snippet-buffer'.")

(defun yas--read-table ()
  "Ask user for a snippet table, help with some guessing."
  (let ((prompt (if (and (featurep 'ido)
                         ido-mode)
                    'ido-completing-read 'completing-read)))
    (unless yas--guessed-modes
      (set (make-local-variable 'yas--guessed-modes)
           (or (yas--compute-major-mode-and-parents buffer-file-name))))
    (intern
     (funcall prompt (format "Choose or enter a table (yas guesses %s): "
                             (if yas--guessed-modes
                                 (cl-first yas--guessed-modes)
                               "nothing"))
              (mapcar #'symbol-name yas--guessed-modes)
              nil
              nil
              nil
              nil
              (if (cl-first yas--guessed-modes)
                  (symbol-name (cl-first yas--guessed-modes)))))))

(defun yas-load-snippet-buffer (table &optional interactive)
  "Parse and load current buffer's snippet definition into TABLE.
TABLE is a symbol name passed to `yas--table-get-create'.  When
called interactively, prompt for the table name.
Return the `yas--template' object created"
  (interactive (list (yas--read-table) t))
  (cond
   ;;  We have `yas--editing-template', this buffer's content comes from a
   ;;  template which is already loaded and neatly positioned,...
   ;;
   (yas--editing-template
    (yas--define-snippets-1 (yas--parse-template (yas--template-load-file yas--editing-template))
                           (yas--template-table yas--editing-template)))
   ;; Try to use `yas--guessed-modes'. If we don't have that use the
   ;; value from `yas--compute-major-mode-and-parents'
   ;;
   (t
    (unless yas--guessed-modes
      (set (make-local-variable 'yas--guessed-modes) (or (yas--compute-major-mode-and-parents buffer-file-name))))
    (let* ((table (yas--table-get-create table)))
      (set (make-local-variable 'yas--editing-template)
           (yas--define-snippets-1 (yas--parse-template buffer-file-name)
                                  table)))))
  (when interactive
    (yas--message 3 "Snippet \"%s\" loaded for %s."
                  (yas--template-name yas--editing-template)
                  (yas--table-name (yas--template-table yas--editing-template))))
  yas--editing-template)

(defun yas-maybe-load-snippet-buffer ()
  "Added to `after-save-hook' in `snippet-mode'."
  (let* ((mode (intern (file-name-sans-extension
                        (file-name-nondirectory
                         (directory-file-name default-directory)))))
         (current-snippet
          (apply #'yas--define-snippets-2 (yas--table-get-create mode)
                 (yas--parse-template buffer-file-name)))
         (uuid (yas--template-uuid current-snippet)))
    (unless (equal current-snippet
                   (if uuid (yas--get-template-by-uuid mode uuid)
                     (yas--lookup-snippet-1
                      (yas--template-name current-snippet) mode)))
      (yas-load-snippet-buffer mode t))))

(defun yas-load-snippet-buffer-and-close (table &optional kill)
  "Load and save the snippet, then `quit-window' if saved.
Loading is performed by `yas-load-snippet-buffer'.  If the
snippet is new, ask the user whether (and where) to save it.  If
the snippet already has a file, just save it.

The prefix argument KILL is passed to `quit-window'.

Don't use this from a Lisp program, call `yas-load-snippet-buffer'
and `kill-buffer' instead."
  (interactive (list (yas--read-table) current-prefix-arg))
  (let ((template (yas-load-snippet-buffer table t)))
    (when (and (buffer-modified-p)
               (y-or-n-p
                (format "[yas] Loaded for %s. Also save snippet buffer?"
                        (yas--table-name (yas--template-table template)))))
      (let ((default-directory (car (cdr (car (yas--guess-snippet-directories
                                               (yas--template-table template))))))
            (default-file-name (yas--template-name template)))
        (unless (or buffer-file-name (not default-file-name))
          (setq buffer-file-name
                (read-file-name "File to save snippet in: "
                                nil nil nil default-file-name))
          (rename-buffer (file-name-nondirectory buffer-file-name) t))
        (save-buffer)))
    (quit-window kill)))

(declare-function yas-debug-snippets "yasnippet-debug")

(defun yas-tryout-snippet (&optional debug)
  "Test current buffer's snippet template in other buffer.
DEBUG is for debugging the YASnippet engine itself."
  (interactive "P")
  (let* ((major-mode-and-parent (yas--compute-major-mode-and-parents buffer-file-name))
         (parsed (yas--parse-template))
         (test-mode (or (and (car major-mode-and-parent)
                             (fboundp (car major-mode-and-parent))
                             (car major-mode-and-parent))
                        (cl-first yas--guessed-modes)
                        (intern (read-from-minibuffer (yas--format "Please input a mode: ")))))
         (yas--current-template
          (and parsed
               (fboundp test-mode)
               (yas--make-template :table       nil ;; no tables for ephemeral snippets
                                   :key         (nth 0 parsed)
                                   :content     (nth 1 parsed)
                                   :name        (nth 2 parsed)
                                   :expand-env  (nth 5 parsed)))))
    (cond (yas--current-template
           (let ((buffer-name
                  (format "*testing snippet: %s*"
                          (yas--template-name yas--current-template))))
             (kill-buffer (get-buffer-create buffer-name))
             (switch-to-buffer (get-buffer-create buffer-name))
             (setq buffer-undo-list nil)
             (condition-case nil (funcall test-mode) (error nil))
	     (yas-minor-mode 1)
             (setq buffer-read-only nil)
             (yas-expand-snippet yas--current-template
                                 (point-min) (point-max))
             (when (and debug
                        (require 'yasnippet-debug nil t))
               (yas-debug-snippets "*YASnippet trace*" 'snippet-navigation)
               (display-buffer "*YASnippet trace*"))))
          (t
           (yas--message 1 "Cannot test snippet for unknown major mode")))))

(defun yas-active-keys ()
  "Return all active trigger keys for current buffer and point."
  (cl-remove-duplicates
   (cl-remove-if-not #'stringp (cl-mapcan #'yas--table-all-keys
                                          (yas--get-snippet-tables)))
   :test #'string=))

(defun yas--template-fine-group (template)
  (car (last (or (yas--template-group template)
                 (yas--template-perm-group template)))))

(defun yas-describe-table-by-namehash ()
  "Display snippet tables by NAMEHASH."
  (interactive)
  (with-current-buffer (get-buffer-create "*YASnippet Tables by NAMEHASH*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "YASnippet tables by NAMEHASH: \n")
      (maphash
       (lambda (_mode table)
         (insert (format "\nSnippet table `%s':\n\n" (yas--table-name table)))
         (maphash
          (lambda (key _v)
            (insert (format "   key %s maps snippets: %s\n" key
                            (let ((names))
                              (maphash #'(lambda (k _v)
                                           (push k names))
                                       (gethash key (yas--table-hash table)))
                              names))))
          (yas--table-hash table)))
       yas--tables))
    (view-mode +1)
    (goto-char 1)
    (display-buffer (current-buffer))))

(defun yas-describe-tables (&optional with-nonactive)
  "Display snippets for each table."
  (interactive "P")
  (let ((original-buffer (current-buffer))
        (tables (yas--get-snippet-tables)))
   (with-current-buffer (get-buffer-create "*YASnippet Tables*")
     (let ((inhibit-read-only t))
       (when with-nonactive
         (maphash #'(lambda (_k v)
                      (cl-pushnew v tables))
                  yas--tables))
       (erase-buffer)
       (insert "YASnippet tables:\n")
       (dolist (table tables)
         (yas--describe-pretty-table table original-buffer))
       (yas--create-snippet-xrefs))
     (help-mode)
     (goto-char 1)
     (display-buffer (current-buffer)))))

(defun yas--describe-pretty-table (table &optional original-buffer)
  (insert (format "\nSnippet table `%s'"
                  (yas--table-name table)))
  (if (yas--table-parents table)
      (insert (format " parents: %s\n"
                      (mapcar #'yas--table-name
                              (yas--table-parents table))))
    (insert "\n"))
  (insert (make-string 100 ?-) "\n")
  (insert "group                   state name                                    key             binding\n")
  (let ((groups-hash (make-hash-table :test #'equal)))
    (maphash #'(lambda (_k v)
                 (let ((group (or (yas--template-fine-group v)
                                  "(top level)")))
                   (when (yas--template-name v)
                     (puthash group
                              (cons v (gethash group groups-hash))
                              groups-hash))))
             (yas--table-uuidhash table))
    (maphash
     #'(lambda (group templates)
         (setq group (truncate-string-to-width group 25 0 ?  "..."))
         (insert (make-string 100 ?-) "\n")
         (dolist (p templates)
           (let* ((name (truncate-string-to-width (propertize (format "\\\\snippet `%s'" (yas--template-name p))
                                                              'yasnippet p)
                                                  50 0 ? "..."))
                  (group (prog1 group
                           (setq group (make-string (length group) ? ))))
                  (condition-string (let ((condition (yas--template-condition p)))
                                      (if (and condition
                                               original-buffer)
                                          (with-current-buffer original-buffer
                                            (if (yas--eval-condition condition)
                                                "(y)"
                                              "(s)"))
                                        "(a)")))
                  (key-description-string (key-description (yas--template-keybinding p)))
                  (template-key-padding (if (string= key-description-string "") nil ? )))
             (insert group " "
                     condition-string " "
                     name (if (string-match "\\.\\.\\.$" name)
                              "'" " ")
                     " "
                     (truncate-string-to-width (or (yas--template-key p) "")
                                               15 0 template-key-padding "...")
                     (or template-key-padding "")
                     (truncate-string-to-width key-description-string
                                               15 0 nil "...")
                     "\n"))))
     groups-hash)))



;;; User convenience functions, for using in `yas-key-syntaxes'

(defun yas-try-key-from-whitespace (_start-point)
  "As `yas-key-syntaxes' element, look for whitespace delimited key.

A newline will be considered whitespace even if the mode syntax
marks it as something else (typically comment ender)."
  (skip-chars-backward "^[:space:]\n"))

(defun yas-shortest-key-until-whitespace (_start-point)
  "Like `yas-longest-key-from-whitespace' but take the shortest key."
  (when (/= (skip-chars-backward "^[:space:]\n" (1- (point))) 0)
    'again))

(defun yas-longest-key-from-whitespace (start-point)
  "Look for longest key between point and whitespace.
For use as `yas-key-syntaxes' element.

A newline will be considered whitespace even if the mode syntax
marks it as something else (typically comment ender)."
  (if (= (point) start-point)
      (yas-try-key-from-whitespace start-point)
    (forward-char))
  (unless (<= start-point (1+ (point)))
    'again))



;;; User convenience functions, for using in snippet definitions

(defvar yas-modified-p nil
  "Non-nil if field has been modified by user or transformation.")

(defvar yas-moving-away-p nil
  "Non-nil if user is about to exit field.")

(defvar yas-text nil
  "Contains current field text.")

(defun yas-substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas-choose-value (&rest possibilities)
  "Prompt for a string in POSSIBILITIES and return it.

The last element of POSSIBILITIES may be a list of strings."
  (unless (or yas-moving-away-p
              yas-modified-p)
    (let* ((last-link (last possibilities))
           (last-elem (car last-link)))
      (when (listp last-elem)
        (setcar last-link (car last-elem))
        (setcdr last-link (cdr last-elem))))
    (cl-some (lambda (fn)
               (funcall fn "Choose: " possibilities))
             yas-prompt-functions)))

(defun yas-completing-read (&rest args)
  "A snippet-aware version of `completing-read'.
This can be used to query the user for the initial value of a
snippet field.  The arguments are the same as `completing-read'.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
  (unless (or yas-moving-away-p
              yas-modified-p)
    (apply #'completing-read args)))

(defun yas--auto-next ()
  "Helper for `yas-auto-next'."
  (cl-loop
   do (progn (remove-hook 'post-command-hook #'yas--auto-next t)
             (yas-next-field))
   ;; The transform in the next field may have requested auto-next as
   ;; well.  Call it ourselves, since the command loop itself won't
   ;; recheck the value of post-command-hook while running it.
   while (memq #'yas--auto-next post-command-hook)))

(defmacro yas-auto-next (&rest body)
  "Automatically advance to next field after eval'ing BODY."
  (declare (indent 0) (debug t))
  `(unless yas-moving-away-p
     (prog1 ,@body
       (add-hook 'post-command-hook #'yas--auto-next nil t))))

(defun yas-key-to-value (alist)
  (unless (or yas-moving-away-p
              yas-modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
        (or (cdr (cl-find key alist :key #'car :test #'string=))
            key)))))

(defun yas-throw (text)
  "Signal `yas-exception' with TEXT as the reason."
  (signal 'yas-exception (list text)))
(define-error 'yas-exception "[yas] Exception")

(defun yas-verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES.
Otherwise signal `yas-exception'."
  (when (and yas-moving-away-p (not (member yas-text possibilities)))
    (yas-throw (format "Field only allows %s" possibilities))))

(defun yas-field-value (number)
  "Get the string for field with NUMBER.

Use this in primary and mirror transformations to get the text of
other fields."
  (let* ((snippet (car (yas-active-snippets)))
         (field (and snippet
                     (yas--snippet-find-field snippet number))))
    (when field
      (yas--field-text-for-display field))))

(defun yas-text ()
  "Return `yas-text' if that exists and is non-empty, else nil."
  (if (and yas-text
           (not (string= "" yas-text)))
      yas-text))

(defun yas-selected-text ()
  "Return `yas-selected-text' if that exists and is non-empty, else nil."
  (if (and yas-selected-text
           (not (string= "" yas-selected-text)))
      yas-selected-text))

(defun yas--get-field-once (number &optional transform-fn)
  (unless yas-modified-p
    (if transform-fn
        (funcall transform-fn (yas-field-value number))
      (yas-field-value number))))

(defun yas-default-from-field (number)
  (unless yas-modified-p
    (yas-field-value number)))

(defun yas-inside-string ()
  "Return non-nil if the point is inside a string according to font-lock."
  (equal 'font-lock-string-face (get-char-property (1- (point)) 'face)))

(defun yas-unimplemented (&optional missing-feature)
  (if yas--current-template
      (if (y-or-n-p (format "This snippet is unimplemented (missing %s) Visit the snippet definition? "
                            (or missing-feature
                                "something")))
          (yas--visit-snippet-file-1 yas--current-template))
    (message "No implementation. Missing %s" (or missing-feature "something"))))


;;; Snippet expansion and field management

(defvar yas--active-field-overlay nil
  "Overlays the currently active field.")

(defvar yas--active-snippets nil
  "List of currently active snippets")
(make-variable-buffer-local 'yas--active-snippets)

(defvar yas--field-protection-overlays nil
  "Two overlays protect the current active field.")

(defvar yas-selected-text nil
  "The selected region deleted on the last snippet expansion.")

(defvar yas--start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas--active-field-overlay)
(make-variable-buffer-local 'yas--field-protection-overlays)
(put 'yas--active-field-overlay 'permanent-local t)
(put 'yas--field-protection-overlays 'permanent-local t)

(cl-defstruct (yas--snippet (:constructor yas--make-snippet (expand-env)))
  "A snippet.

..."
  expand-env
  (fields '())
  (exit nil)
  (id (yas--snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field
  force-exit)

(cl-defstruct (yas--field (:constructor yas--make-field (number start end parent-field)))
  "A field.

NUMBER is the field number.
START and END are mostly buffer markers, but see \"apropos markers-to-points\".
PARENT-FIELD is a `yas--field' this field is nested under, or nil.
MIRRORS is a list of `yas--mirror's
TRANSFORM is a lisp form.
MODIFIED-P is a boolean set to true once user inputs text.
NEXT is another `yas--field' or `yas--mirror' or `yas--exit'.
"
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)


(cl-defstruct (yas--mirror (:constructor yas--make-mirror (start end transform)))
  "A mirror.

START and END are mostly buffer markers, but see \"apropos markers-to-points\".
TRANSFORM is a lisp form.
PARENT-FIELD is a `yas--field' this mirror is nested under, or nil.
NEXT is another `yas--field' or `yas--mirror' or `yas--exit'
DEPTH is a count of how many nested mirrors can affect this mirror"
  start end
  (transform nil)
  parent-field
  next
  depth)

(cl-defstruct (yas--exit (:constructor yas--make-exit (marker)))
  marker
  next)

(defmacro yas--letenv (env &rest body)
  "Evaluate BODY with bindings from ENV.
ENV is a lisp expression that evaluates to list of elements with
the form (VAR FORM), where VAR is a symbol and FORM is a lisp
expression that evaluates to its value."
  (declare (debug (form body)) (indent 1))
  (let ((envvar (make-symbol "envvar")))
    `(let ((,envvar ,env))
       (cl-progv
           (mapcar #'car ,envvar)
           (mapcar (lambda (v-f) (eval (cadr v-f) t)) ,envvar)
         ,@body))))

(defun yas--snippet-map-markers (fun snippet)
  "Apply FUN to all marker (sub)fields in SNIPPET.
Update each field with the result of calling FUN."
  (dolist (field (yas--snippet-fields snippet))
    (setf (yas--field-start field) (funcall fun (yas--field-start field)))
    (setf (yas--field-end field)   (funcall fun (yas--field-end field)))
    (dolist (mirror (yas--field-mirrors field))
      (setf (yas--mirror-start mirror) (funcall fun (yas--mirror-start mirror)))
      (setf (yas--mirror-end mirror)   (funcall fun (yas--mirror-end mirror)))))
  (let ((snippet-exit (yas--snippet-exit snippet)))
    (when snippet-exit
      (setf (yas--exit-marker snippet-exit)
            (funcall fun (yas--exit-marker snippet-exit))))))

(defun yas--snippet-live-p (snippet)
  "Return non-nil if SNIPPET hasn't been committed."
  (catch 'live
    (yas--snippet-map-markers (lambda (m)
                                (if (markerp m) m
                                  (throw 'live nil)))
                              snippet)
    t))

(defun yas--apply-transform (field-or-mirror field &optional empty-on-nil-p)
  "Calculate transformed string for FIELD-OR-MIRROR from FIELD.

If there is no transform for ht field, return nil.

If there is a transform but it returns nil, return the empty
string iff EMPTY-ON-NIL-P is true."
  (let* ((yas-text (yas--field-text-for-display field))
         (yas-modified-p (yas--field-modified-p field))
         (transform (if (yas--mirror-p field-or-mirror)
                        (yas--mirror-transform field-or-mirror)
                      (yas--field-transform field-or-mirror)))
         (start-point (if (yas--mirror-p field-or-mirror)
                          (yas--mirror-start field-or-mirror)
                        (yas--field-start field-or-mirror)))
         (transformed (and transform
                           (save-excursion
                             (goto-char start-point)
                             (let ((ret (yas--eval-for-string transform)))
                               (or ret (and empty-on-nil-p "")))))))
    transformed))

(defsubst yas--replace-all (from to &optional text)
  "Replace all occurrences from FROM to TO.

With optional string TEXT do it in that string."
  (if text
      (replace-regexp-in-string (regexp-quote from) to text t t)
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to t t text))))

(defun yas--snippet-find-field (snippet number)
  (cl-find-if (lambda (field)
                (eq number (yas--field-number field)))
              (yas--snippet-fields snippet)))

(defun yas--snippet-sort-fields (snippet)
  "Sort the fields of SNIPPET in navigation order."
  (setf (yas--snippet-fields snippet)
        (sort (yas--snippet-fields snippet)
              #'yas--snippet-field-compare)))

(defun yas--snippet-field-compare (field1 field2)
  "Compare FIELD1 and FIELD2.

The field with a number is sorted first.  If they both have a
number, compare through the number.  If neither have, compare
through the field's start point"
  (let ((n1 (yas--field-number field1))
        (n2 (yas--field-number field2)))
    (if n1
        (if n2
            (or (zerop n2) (and (not (zerop n1))
                                (< n1 n2)))
          (not (zerop n1)))
      (if n2
          (zerop n2)
        (< (yas--field-start field1)
           (yas--field-start field2))))))

(defun yas--field-probably-deleted-p (snippet field)
  "Guess if SNIPPET's FIELD should be skipped."
  (and
   ;; field must be zero length
   ;;
   (zerop (- (yas--field-start field) (yas--field-end field)))
   ;; field must have been modified
   ;;
   (yas--field-modified-p field)
   ;; either:
   (or
    ;;  1) it's a nested field
    ;;
    (yas--field-parent-field field)
    ;;  2) ends just before the snippet end
    ;;
    (and (eq field (car (last (yas--snippet-fields snippet))))
         (= (yas--field-start field) (overlay-end (yas--snippet-control-overlay snippet)))))
   ;; the field numbered 0, just before the exit marker, should
   ;; never be skipped
   ;;
   (not (and (yas--field-number field)
             (zerop (yas--field-number field))))))

(defun yas-active-snippets (&optional beg end)
  "Return a sorted list of active snippets.
The most recently-inserted snippets are returned first.

Only snippets overlapping the region BEG ... END are returned.
Overlapping has the same meaning as described in `overlays-in'.
If END is omitted, it defaults to (1+ BEG).  If BEG is omitted,
it defaults to point.  A non-nil, non-buffer position BEG is
equivalent to a range covering the whole buffer."
  (unless beg
    (setq beg (point)))
  (cond ((not (or (integerp beg) (markerp beg)))
         (setq beg (point-min) end (point-max)))
        ((not end)
         (setq end (1+ beg))))
  (if (and (eq beg (point-min))
           (eq end (point-max)))
      yas--active-snippets
    ;; Note: don't use `mapcar' here, since it would allocate in
    ;; proportion to the amount of overlays, even though the list of
    ;; active snippets should be very small.
    (let ((snippets nil))
      (dolist (ov (overlays-in beg end))
        (let ((snippet (overlay-get ov 'yas--snippet)))
          ;; Snippets have multiple overlays, so check for dups.
          (when (and snippet (not (memq snippet snippets)))
            (push snippet snippets))))
      (cl-sort snippets #'>= :key #'yas--snippet-id))))

(define-obsolete-function-alias 'yas--snippets-at-point
  #'yas-active-snippets "0.12")

(defun yas-next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point.

Otherwise delegate to `yas-next-field'."
  (interactive)
  (if yas-triggers-in-field
      (let ((yas-fallback-behavior 'return-nil)
            (active-field (overlay-get yas--active-field-overlay 'yas--field)))
        (when active-field
          (unless (yas-expand-from-trigger-key active-field)
            (yas-next-field))))
    (yas-next-field)))

(defun yas-next-field-will-exit-p (&optional arg)
  "Return non-nil if (yas-next-field ARG) would exit the current snippet."
  (let ((snippet (car (yas-active-snippets)))
        (active (overlay-get yas--active-field-overlay 'yas--field)))
    (when snippet
      (not (yas--find-next-field arg snippet active)))))

(defun yas--find-next-field (n snippet active)
  "Return the Nth field after the ACTIVE one in SNIPPET."
  (let ((live-fields (cl-remove-if
                      (lambda (field)
                        (and (not (eq field active))
                             (yas--field-probably-deleted-p snippet field)))
                      (yas--snippet-fields snippet))))
    (nth (abs n) (memq active (if (>= n 0) live-fields (reverse live-fields))))))

(defun yas-next-field (&optional arg)
  "Navigate to the ARGth next field.

If there's none, exit the snippet."
  (interactive)
  (unless arg (setq arg 1))
  (let* ((active-field (overlay-get yas--active-field-overlay 'yas--field))
         (snippet (car (yas-active-snippets (yas--field-start active-field)
                                            (yas--field-end active-field))))
         (target-field (yas--find-next-field arg snippet active-field)))
    (yas--letenv (yas--snippet-expand-env snippet)
      ;; Apply transform to active field.
      (when active-field
        (let ((yas-moving-away-p t))
          (when (yas--field-update-display active-field)
            (yas--update-mirrors snippet))))
      ;; Now actually move...
      (if target-field
          (yas--move-to-field snippet target-field)
        (yas-exit-snippet snippet)))))

(defun yas--place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD."
  (yas--make-move-field-protection-overlays snippet field)
  ;; Only move active field overlays if this is field is from the
  ;; innermost snippet.
  (when (eq snippet (car (yas-active-snippets (1- (yas--field-start field))
                                              (1+ (yas--field-end field)))))
    (yas--make-move-active-field-overlay snippet field)))

(defun yas--move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas--field-start field))
  (yas--place-overlays snippet field)
  (overlay-put yas--active-field-overlay 'yas--snippet snippet)
  (overlay-put yas--active-field-overlay 'yas--field field)
  (let ((number (yas--field-number field)))
    ;; check for the special ${0: ...} field
    (if (and number (zerop number))
        (progn
          (set-mark (yas--field-end field))
          (setf (yas--snippet-force-exit snippet)
                (or (yas--field-transform field)
                    t)))
      ;; make this field active
      (setf (yas--snippet-active-field snippet) field)
      ;; primary field transform: first call to snippet transform
      (unless (yas--field-modified-p field)
        (if (yas--field-update-display field)
            (yas--update-mirrors snippet)
          (setf (yas--field-modified-p field) nil))))))

(defun yas-prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas-next-field -1))

(defun yas-abort-snippet (&optional snippet)
  (interactive)
  (let ((snippet (or snippet
                     (car (yas-active-snippets)))))
    (when snippet
      (setf (yas--snippet-force-exit snippet) t))))

(defun yas-exit-snippet (snippet)
  "Goto exit-marker of SNIPPET."
  (interactive (list (cl-first (yas-active-snippets))))
  (when snippet
    (setf (yas--snippet-force-exit snippet) t)
    (goto-char (if (yas--snippet-exit snippet)
                   (yas--exit-marker (yas--snippet-exit snippet))
                 (overlay-end (yas--snippet-control-overlay snippet))))))

(defun yas-exit-all-snippets ()
  "Exit all snippets."
  (interactive)
  (mapc #'(lambda (snippet)
            (yas-exit-snippet snippet)
            (yas--check-commit-snippet))
        (yas-active-snippets 'all)))


;;; Some low level snippet-routines:

(defvar yas--inhibit-overlay-hooks nil
  "Bind this temporarily to non-nil to prevent running `yas--on-*-modification'.")

(defvar yas-snippet-beg nil "Beginning position of the last snippet committed.")
(defvar yas-snippet-end nil "End position of the last snippet committed.")

(defun yas--commit-snippet (snippet)
  "Commit SNIPPET, but leave point as it is.

This renders the snippet as ordinary text."

  (let ((control-overlay (yas--snippet-control-overlay snippet)))
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas-snippet-beg (overlay-start control-overlay))
      (setq yas-snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay)
      (setf (yas--snippet-control-overlay snippet) nil))

    (let ((yas--inhibit-overlay-hooks t))
      (when yas--active-field-overlay
        (delete-overlay yas--active-field-overlay))
      (when yas--field-protection-overlays
        (mapc #'delete-overlay yas--field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas-snippet-end'...
    ;;
    (let ((previous-field (yas--snippet-previous-active-field snippet)))
      (when (and yas-snippet-end previous-field)
        (yas--advance-end-maybe-previous-fields
         previous-field yas-snippet-end (cdr yas--active-snippets))))

    ;; Convert all markers to points,
    ;;
    (yas--markers-to-points snippet)

    ;; It's no longer an active snippet.
    (cl-callf2 delq snippet yas--active-snippets)

    ;; Take care of snippet revival on undo.
    (if (and yas-snippet-revival (listp buffer-undo-list))
        (push `(apply yas--snippet-revive ,yas-snippet-beg ,yas-snippet-end ,snippet)
              buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas--take-care-of-redo'....
      (setf (yas--snippet-fields snippet) nil)))

  (yas--message 4 "Snippet %s exited." (yas--snippet-id snippet)))

(defvar yas--snippets-to-move nil)
(make-variable-buffer-local 'yas--snippets-to-move)

(defun yas--prepare-snippets-for-move (beg end buf pos)
  "Gather snippets in BEG..END for moving to POS in BUF."
  (let ((to-move nil)
        (snippets (yas-active-snippets beg end))
        (dst-base-line (with-current-buffer buf
                         (count-lines (point-min) pos))))
    (when snippets
      (dolist (snippet snippets)
        (yas--snippet-map-markers
         (lambda (m)
           (prog1 (cons m (yas--snapshot-line-location m))
             (set-marker m nil)))
         snippet)
        (let ((ctrl-ov (yas--snapshot-overlay-line-location
                        (yas--snippet-control-overlay snippet))))
          (push (list ctrl-ov dst-base-line snippet) to-move)
          (delete-overlay (car ctrl-ov))))
      (with-current-buffer buf
        (cl-callf2 nconc to-move yas--snippets-to-move)))))

(defun yas--on-buffer-kill ()
  ;; Org mode uses temp buffers for fontification and "native tab",
  ;; move all the snippets to the original org-mode buffer when it's
  ;; killed.
  (let ((org-marker nil)
        (org-buffer nil))
    (when (and yas-minor-mode
               (or (bound-and-true-p org-edit-src-from-org-mode)
                   (bound-and-true-p org-src--from-org-mode))
               (markerp
                (setq org-marker
                      (or (bound-and-true-p org-edit-src-beg-marker)
                          (bound-and-true-p org-src--beg-marker))))
               ;; If the org source buffer is killed before the temp
               ;; fontification one, org-marker might point nowhere.
               (setq org-buffer (marker-buffer org-marker)))
      (yas--prepare-snippets-for-move
       (point-min) (point-max)
       org-buffer org-marker))))

(add-hook 'kill-buffer-hook #'yas--on-buffer-kill)

(defun yas--finish-moving-snippets ()
  "Finish job started in `yas--prepare-snippets-for-move'."
  (cl-loop for (ctrl-ov base-line snippet) in yas--snippets-to-move
           for base-pos = (progn (goto-char (point-min))
                                 (forward-line base-line) (point))
           do (yas--snippet-map-markers
               (lambda (saved-location)
                 (let ((m (pop saved-location)))
                   (set-marker m (yas--goto-saved-line-location
                                  base-pos saved-location))
                   m))
               snippet)
           (goto-char base-pos)
           (yas--restore-overlay-line-location base-pos ctrl-ov)
           (yas--maybe-move-to-active-field snippet)
           (push snippet yas--active-snippets))
  (setq yas--snippets-to-move nil))

(defun yas--safely-call-fun (fun)
  "Call FUN and catch any errors."
  (condition-case error
      (funcall fun)
    ((debug error)
     (yas--message 2 "Error running %s: %s" fun
                   (error-message-string error)))))

(defun yas--safely-run-hook (hook)
  "Call HOOK's functions.
HOOK should be a symbol, a hook variable, as in `run-hooks'."
  (let ((debug-on-error (and (not (memq yas-good-grace '(t hooks)))
                             debug-on-error)))
    (yas--safely-call-fun (apply-partially #'run-hooks hook))))

(defun yas--check-commit-snippet ()
  "Check if point exited the currently active field of the snippet.

If so cleans up the whole snippet up."
  (let* ((snippet-exit-transform nil)
         (exited-snippets-p nil)
         ;; Record the custom snippet `yas-after-exit-snippet-hook'
         ;; set in the expand-env field.
         (snippet-exit-hook yas-after-exit-snippet-hook))
    (dolist (snippet yas--active-snippets)
      (let ((active-field (yas--snippet-active-field snippet)))
        (yas--letenv (yas--snippet-expand-env snippet)
          ;; Note: the `force-exit' field could be a transform in case of
          ;; ${0: ...}, see `yas--move-to-field'.
          (setq snippet-exit-transform (yas--snippet-force-exit snippet))
          (cond ((or snippet-exit-transform
                     (not (and active-field (yas--field-contains-point-p active-field))))
                 (setf (yas--snippet-force-exit snippet) nil)
                 (setq snippet-exit-hook yas-after-exit-snippet-hook)
                 (yas--commit-snippet snippet)
                 (setq exited-snippets-p t))
                ((and active-field
                      (or (not yas--active-field-overlay)
                          (not (overlay-buffer yas--active-field-overlay))))
                 ;;
                 ;; stacked expansion: this case is mainly for recent
                 ;; snippet exits that place us back int the field of
                 ;; another snippet
                 ;;
                 (save-excursion
                   (yas--move-to-field snippet active-field)
                   (yas--update-mirrors snippet)))
                (t
                 nil)))))
    (unless (or yas--active-snippets (not exited-snippets-p))
      (when snippet-exit-transform
        (yas--eval-for-effect snippet-exit-transform))
      (let ((yas-after-exit-snippet-hook snippet-exit-hook))
        (yas--safely-run-hook 'yas-after-exit-snippet-hook)))))

;; Apropos markers-to-points:
;;
;; This was found useful for performance reasons, so that an excessive
;; number of live markers aren't kept around in the
;; `buffer-undo-list'.  We don't reuse the original marker object
;; because that leaves an unreadable object in the history list and
;; undo-tree persistence has trouble with that.
;;
;; This shouldn't bring horrible problems with undo/redo, but you
;; never know.
;;
(defun yas--markers-to-points (snippet)
  "Save all markers of SNIPPET as positions."
  (yas--snippet-map-markers (lambda (m)
                              (prog1 (marker-position m)
                                (set-marker m nil)))
                            snippet))

(defun yas--points-to-markers (snippet)
  "Restore SNIPPET's marker positions, saved by `yas--markers-to-points'."
  (yas--snippet-map-markers #'copy-marker snippet))

(defun yas--maybe-move-to-active-field (snippet)
  "Try to move to SNIPPET's active (or first) field and return it if found."
  (let ((target-field (or (yas--snippet-active-field snippet)
                          (car (yas--snippet-fields snippet)))))
    (when target-field
      (yas--move-to-field snippet target-field)
      target-field)))

(defun yas--field-contains-point-p (field &optional point)
  (let ((point (or point
                   (point))))
    (and (>= point (yas--field-start field))
         (<= point (yas--field-end field)))))

(defun yas--field-text-for-display (field)
  "Return the propertized display text for field FIELD."
  (buffer-substring (yas--field-start field) (yas--field-end field)))

(defun yas--undo-in-progress ()
  "True if some kind of undo is in progress."
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas--make-control-overlay (snippet start end)
  "Create the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil
                               t)))
    (overlay-put overlay 'keymap yas-keymap)
    (overlay-put overlay 'priority yas-overlay-priority)
    (overlay-put overlay 'yas--snippet snippet)
    overlay))

(defun yas-current-field ()
  "Return the currently active field."
  (and yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (overlay-get yas--active-field-overlay 'yas--field)))

(defun yas--maybe-clear-field-filter (cmd)
  "Return CMD if at start of unmodified snippet field.
Use as a `:filter' argument for a conditional keybinding."
  (let ((field (yas-current-field)))
    (when (and field
               (not (yas--field-modified-p field))
               (eq (point) (marker-position (yas--field-start field))))
      cmd)))

(defun yas-skip-and-clear-field (&optional field)
  "Clears unmodified FIELD if at field start, skips to next tab."
  (interactive)
  (yas--skip-and-clear (or field (yas-current-field)))
  (yas-next-field 1))

(defun yas-clear-field (&optional field)
  "Clears unmodified FIELD if at field start."
  (interactive)
  (yas--skip-and-clear (or field (yas-current-field))))

(defun yas-skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (declare (obsolete "Bind to `yas-maybe-skip-and-clear-field' instead." "0.13"))
  (interactive)
  (cond ((yas--maybe-clear-field-filter t)
         (yas--skip-and-clear (or field (yas-current-field)))
         (yas-next-field 1))
        (t (call-interactively 'delete-char))))

(defun yas--skip-and-clear (field &optional from)
  "Deletes the region of FIELD and sets it's modified state to t.
If given, FROM indicates position to start at instead of FIELD's beginning."
  ;; Just before skipping-and-clearing the field, mark its children
  ;; fields as modified, too. If the children have mirrors-in-fields
  ;; this prevents them from updating erroneously (we're skipping and
  ;; deleting!).
  ;;
  (yas--mark-this-and-children-modified field)
  (unless (= (yas--field-start field) (yas--field-end field))
    (delete-region (or from (yas--field-start field)) (yas--field-end field))))

(defun yas--mark-this-and-children-modified (field)
  (setf (yas--field-modified-p field) t)
  (let ((fom (yas--field-next field)))
    (while (and fom
                (yas--fom-parent-field fom))
      (when (and (eq (yas--fom-parent-field fom) field)
                 (yas--field-p fom))
        (yas--mark-this-and-children-modified fom))
      (setq fom (yas--fom-next fom)))))

(defun yas--make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas--active-field-overlay
           (overlay-buffer yas--active-field-overlay))
      (move-overlay yas--active-field-overlay
                    (yas--field-start field)
                    (yas--field-end field))
    (setq yas--active-field-overlay
          (make-overlay (yas--field-start field)
                        (yas--field-end field)
                        nil nil t))
    (overlay-put yas--active-field-overlay 'priority yas-overlay-priority)
    (overlay-put yas--active-field-overlay 'face 'yas-field-highlight-face)
    (overlay-put yas--active-field-overlay 'yas--snippet snippet)
    (overlay-put yas--active-field-overlay 'modification-hooks '(yas--on-field-overlay-modification))
    (overlay-put yas--active-field-overlay 'insert-in-front-hooks
                 '(yas--on-field-overlay-modification))
    (overlay-put yas--active-field-overlay 'insert-behind-hooks
                 '(yas--on-field-overlay-modification))))

(defun yas--skip-and-clear-field-p (field beg _end length)
  "Tell if newly modified FIELD should be cleared and skipped.
BEG, END and LENGTH like overlay modification hooks."
  (and (= length 0) ; A 0 pre-change length indicates insertion.
       (= beg (yas--field-start field)) ; Insertion at field start?
       (not (yas--field-modified-p field))))


(defun yas--merge-and-drop-dups (list1 list2 cmp key)
  ;; `delete-consecutive-dups' + `cl-merge'.
  (funcall (if (fboundp 'delete-consecutive-dups)
               #'delete-consecutive-dups ; 24.4
             #'delete-dups)
           (cl-merge 'list list1 list2 cmp :key key)))

(defvar yas--before-change-modified-snippets nil)
(make-variable-buffer-local 'yas--before-change-modified-snippets)

(defun yas--gather-active-snippets (overlay beg end then-delete)
  ;; Add active snippets in BEG..END into an OVERLAY keyed entry of
  ;; `yas--before-change-modified-snippets'.  Return accumulated list.
  ;; If THEN-DELETE is non-nil, delete the entry.
  (let ((new (yas-active-snippets beg end))
        (old (assq overlay yas--before-change-modified-snippets)))
    (prog1 (cond ((and new old)
                  (setf (cdr old)
                        (yas--merge-and-drop-dups
                         (cdr old) new
                         ;; Sort like `yas-active-snippets'.
                         #'>= #'yas--snippet-id)))
                 (new (unless then-delete
                        ;; Don't add new entry if we're about to
                        ;; remove it anyway.
                        (push (cons overlay new)
                              yas--before-change-modified-snippets))
                      new)
                 (old (cdr old))
                 (t nil))
      (when then-delete
        (cl-callf2 delq old yas--before-change-modified-snippets)))))

(defvar yas--todo-snippet-indent nil nil)
(make-variable-buffer-local 'yas--todo-snippet-indent)

(defun yas--on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and point is at
field start.  This hook does nothing if an undo is in progress."
  (unless (or yas--inhibit-overlay-hooks
              (not (overlayp yas--active-field-overlay)) ; Avoid Emacs bug #21824.
              ;; If a single change hits multiple overlays of the same
              ;; snippet, then we delete the snippet the first time,
              ;; and then subsequent calls get a deleted overlay.
              ;; Don't delete the snippet again!
              (not (overlay-buffer overlay))
              (yas--undo-in-progress))
    (let* ((inhibit-modification-hooks nil)
           (yas--inhibit-overlay-hooks t)
           (field (overlay-get overlay 'yas--field))
           (snippet (overlay-get yas--active-field-overlay 'yas--snippet)))
      (if (yas--snippet-live-p snippet)
          (if after?
              (save-match-data
                (yas--letenv (yas--snippet-expand-env snippet)
                  (when (yas--skip-and-clear-field-p field beg end length)
                    ;; We delete text starting from the END of insertion.
                    (yas--skip-and-clear field end))
                  (setf (yas--field-modified-p field) t)
                  ;; Adjust any pending active fields in case of stacked
                  ;; expansion.
                  (yas--advance-end-maybe-previous-fields
                   field (overlay-end overlay)
                   (yas--gather-active-snippets overlay beg end t))
                  ;; Update fields now, but delay auto indentation until
                  ;; post-command.  We don't want to run indentation on
                  ;; the intermediate state where field text might be
                  ;; removed (and hence the field could be deleted along
                  ;; with leading indentation).
                  (let ((yas-indent-line nil))
                    (save-excursion
                      (yas--field-update-display field))
                    (yas--update-mirrors snippet))
                  (unless (or (not (eq yas-indent-line 'auto))
                              (memq snippet yas--todo-snippet-indent))
                    (push snippet yas--todo-snippet-indent))))
            ;; Remember active snippets to use for after the change.
            (yas--gather-active-snippets overlay beg end nil))
        (lwarn '(yasnippet zombie) :warning "Killing zombie snippet!")
        (delete-overlay overlay)))))

(defun yas--do-todo-snippet-indent ()
  ;; Do pending indentation of snippet fields, called from
  ;; `yas--post-command-handler'.
  (when yas--todo-snippet-indent
    (save-excursion
      (cl-loop for snippet in yas--todo-snippet-indent
               do (yas--indent-mirrors-of-snippet
                   snippet (yas--snippet-field-mirrors snippet)))
      (setq yas--todo-snippet-indent nil))))

(defun yas--auto-fill (orig-fun &rest args)
  ;; Preserve snippet markers during auto-fill.
  (let* ((orig-point (point))
         (end (progn (forward-paragraph) (point)))
         (beg (progn (backward-paragraph) (point)))
         (snippets (yas-active-snippets beg end))
         (remarkers nil)
         (reoverlays nil))
    (dolist (snippet snippets)
      (dolist (m (yas--collect-snippet-markers snippet))
        (when (and (<= beg m) (<= m end))
          (push (cons m (yas--snapshot-location m beg end)) remarkers)))
      (push (yas--snapshot-overlay-location
             (yas--snippet-control-overlay snippet) beg end)
            reoverlays))
    (goto-char orig-point)
    (let ((yas--inhibit-overlay-hooks t))
      (apply orig-fun args))
    (save-excursion
      (setq end (progn (forward-paragraph) (point)))
      (setq beg (progn (backward-paragraph) (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (dolist (remarker remarkers)
          (set-marker (car remarker)
                      (yas--goto-saved-location (cdr remarker))))
        (mapc #'yas--restore-overlay-location reoverlays))
      (mapc (lambda (snippet)
              (yas--letenv (yas--snippet-expand-env snippet)
                (yas--update-mirrors snippet)))
            snippets))))


;;; Apropos protection overlays:
;;
;; These exist for nasty users who will try to delete parts of the
;; snippet outside the active field. Actual protection happens in
;; `yas--on-protection-overlay-modification'.
;;
;; As of github #537 this no longer inhibits the command by issuing an
;; error: all the snippets at point, including nested snippets, are
;; automatically commited and the current command can proceed.
;;
(defun yas--make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas--field-start field))
        (end (yas--field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;;
    (when (< (buffer-size) end)
      (save-excursion
        (let ((yas--inhibit-overlay-hooks t))
          (goto-char (point-max))
          (newline))))
    ;; go on to normal overlay creation/moving
    ;;
    (cond ((and yas--field-protection-overlays
                (cl-every #'overlay-buffer yas--field-protection-overlays))
           (move-overlay (nth 0 yas--field-protection-overlays)
                         (1- start) start)
           (move-overlay (nth 1 yas--field-protection-overlays) end (1+ end)))
          (t
           (setq yas--field-protection-overlays
                 (list (make-overlay (1- start) start nil t nil)
                       (make-overlay end (1+ end) nil t nil)))
           (dolist (ov yas--field-protection-overlays)
             (overlay-put ov 'face 'yas--field-debug-face)
             (overlay-put ov 'yas--snippet snippet)
             ;; (overlay-put ov 'evaporate t)
             (overlay-put ov 'modification-hooks '(yas--on-protection-overlay-modification)))))))

(defun yas--on-protection-overlay-modification (_overlay after? beg end &optional length)
  "Commit the snippet if the protection overlay is being killed."
  (unless (or yas--inhibit-overlay-hooks
              yas-inhibit-overlay-modification-protection
              (not after?)
              (= length (- end beg)) ; deletion or insertion
              (yas--undo-in-progress))
    (let ((snippets (yas-active-snippets)))
      (yas--message 2 "Committing snippets. Action would destroy a protection overlay.")
      (cl-loop for snippet in snippets
               do (yas--commit-snippet snippet)))))


;;; Snippet expansion and "stacked" expansion:
;;
;; Stacked expansion is when you try to expand a snippet when already
;; inside a snippet expansion.
;;
;; The parent snippet does not run its fields modification hooks
;; (`yas--on-field-overlay-modification' and
;; `yas--on-protection-overlay-modification') while the child snippet
;; is active. This means, among other things, that the mirrors of the
;; parent snippet are not updated, this only happening when one exits
;; the child snippet.
;;
;; Unfortunately, this also puts some ugly (and not fully-tested)
;; bits of code in `yas-expand-snippet' and
;; `yas--commit-snippet'. I've tried to mark them with "stacked
;; expansion:".
;;
;; This was thought to be safer in an undo/redo perspective, but
;; maybe the correct implementation is to make the globals
;; `yas--active-field-overlay' and `yas--field-protection-overlays' be
;; snippet-local and be active even while the child snippet is
;; running. This would mean a lot of overlay modification hooks
;; running, but if managed correctly (including overlay priorities)
;; they should account for all situations...

(defun yas-expand-snippet (snippet &optional start end expand-env)
  "Expand SNIPPET at current point.

Text between START and END will be deleted before inserting
template.  EXPAND-ENV is a list of (SYM VALUE) let-style dynamic
bindings considered when expanding the snippet.  If omitted, use
SNIPPET's expand-env field.

SNIPPET may be a snippet structure (e.g., as returned by
`yas-lookup-snippet'), or just a snippet body (which is a string
for normal snippets, and a list for command snippets)."
  (cl-assert (and yas-minor-mode
                  (memq 'yas--post-command-handler post-command-hook))
             nil
             "[yas] `yas-expand-snippet' needs properly setup `yas-minor-mode'")
  (run-hooks 'yas-before-expand-snippet-hook)

  (let* ((clear-field
          (let ((field (and yas--active-field-overlay
                            (overlay-buffer yas--active-field-overlay)
                            (overlay-get yas--active-field-overlay 'yas--field))))
            (and field (yas--skip-and-clear-field-p
                        field (point) (point) 0)
                 field)))
         (start (cond (start)
                      ((region-active-p)
                       (region-beginning))
                      (clear-field
                       (yas--field-start clear-field))
                      (t (point))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (clear-field
                     (yas--field-end clear-field))
                    (t (point))))
         (to-delete (and (> end start)
                         (buffer-substring-no-properties start end)))
         (yas-selected-text
          (cond (yas-selected-text)
                ((and (region-active-p)
                      (not clear-field))
                 to-delete))))
    (goto-char start)
    (setq yas--indent-original-column (current-column))
    ;; Delete the region to delete, this *does* get undo-recorded.
    (when to-delete
      (delete-region start end))

    (let ((content (if (yas--template-p snippet)
                       (yas--template-content snippet)
                     snippet)))
      (when (and (not expand-env) (yas--template-p snippet))
        (setq expand-env (yas--template-expand-env snippet)))
      (cond ((listp content)
             ;; x) This is a snippet-command.
             (yas--eval-for-effect content))
            (t
             ;; x) This is a snippet-snippet :-)
             (setq yas--start-column (current-column))
             ;; Stacked expansion: also shoosh the overlay modification hooks.
             (let ((yas--inhibit-overlay-hooks t))
               (setq snippet
                     (yas--snippet-create content expand-env start (point))))

             ;; Stacked-expansion: This checks for stacked expansion, save the
             ;; `yas--previous-active-field' and advance its boundary.
             (let ((existing-field (and yas--active-field-overlay
                                        (overlay-buffer yas--active-field-overlay)
                                        (overlay-get yas--active-field-overlay 'yas--field))))
               (when existing-field
                 (setf (yas--snippet-previous-active-field snippet) existing-field)
                 (yas--advance-end-maybe-previous-fields
                  existing-field (overlay-end yas--active-field-overlay)
                  (cdr yas--active-snippets))))

             ;; Exit the snippet immediately if no fields.
             (unless (yas--snippet-fields snippet)
               (yas-exit-snippet snippet))

             ;; Now, schedule a move to the first field.
             (let ((first-field (car (yas--snippet-fields snippet))))
               (when first-field
                 (sit-for 0) ;; fix issue 125
                 (yas--letenv (yas--snippet-expand-env snippet)
                   (yas--move-to-field snippet first-field))
                 (when (and (eq (yas--field-number first-field) 0)
                            (> (length (yas--field-text-for-display
                                        first-field))
                               0))
                   ;; Keep region for ${0:exit text}.
                   (setq deactivate-mark nil))))
             (yas--message 4 "snippet %d expanded." (yas--snippet-id snippet))
             t)))))

(defun yas--take-care-of-redo (snippet)
  "Commits SNIPPET, which in turn pushes an undo action for reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas--snippet-fields snippet)
    (yas--commit-snippet snippet)))

(defun yas--snippet-revive (beg end snippet)
  "Revives SNIPPET and creates a control overlay from BEG to END.

BEG and END are, we hope, the original snippets boundaries.
All the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas--take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  (yas--points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  (when (yas--maybe-move-to-active-field snippet)
    (setf (yas--snippet-control-overlay snippet) (yas--make-control-overlay snippet beg end))
    (overlay-put (yas--snippet-control-overlay snippet) 'yas--snippet snippet)
    (push snippet yas--active-snippets)
    (when (listp buffer-undo-list)
      (push `(apply yas--take-care-of-redo ,snippet)
            buffer-undo-list))))

(defun yas--snippet-create (content expand-env begin end)
  "Create a snippet from a template inserted at BEGIN to END.

Returns the newly created snippet."
  (save-restriction
    (let ((snippet (yas--make-snippet expand-env)))
      (yas--letenv expand-env
        ;; Put a single undo action for the expanded snippet's
        ;; content.
        (let ((buffer-undo-list t))
          (goto-char begin)
          ;; Call before and after change functions manually,
          ;; otherwise cc-mode's cache can get messed up.  Don't use
          ;; `inhibit-modification-hooks' for that, that blocks
          ;; overlay and text property hooks as well!  FIXME: Maybe
          ;; use `combine-change-calls'?  (Requires Emacs 27+ though.)
          (run-hook-with-args 'before-change-functions begin end)
          (let ((before-change-functions nil)
                (after-change-functions nil))
            ;; Some versions of cc-mode (might be the one with Emacs
            ;; 24.3 only) fail when inserting snippet content in a
            ;; narrowed buffer, so make sure to insert before
            ;; narrowing.
            (insert content)
            (narrow-to-region begin (point))
            (goto-char (point-min))
            (yas--snippet-parse-create snippet))
          (run-hook-with-args 'after-change-functions
                              (point-min) (point-max)
                              (- end begin)))
        (when (listp buffer-undo-list)
          (push (cons (point-min) (point-max))
                buffer-undo-list))

        ;; Indent, collecting undo information normally.
        (yas--indent snippet)

        ;; Follow up with `yas--take-care-of-redo' on the newly
        ;; inserted snippet boundaries.
        (when (listp buffer-undo-list)
          (push `(apply yas--take-care-of-redo ,snippet)
                buffer-undo-list))

        ;; Sort and link each field
        (yas--snippet-sort-fields snippet)

        ;; Create keymap overlay for snippet
        (setf (yas--snippet-control-overlay snippet)
              (yas--make-control-overlay snippet (point-min) (point-max)))

        ;; Move to end
        (goto-char (point-max))

        (push snippet yas--active-snippets)
        snippet))))


;;; Apropos adjacencies and "fom's":
;;
;; Once the $-constructs bits like "$n" and "${:n" are deleted in the
;; recently expanded snippet, we might actually have many fields,
;; mirrors (and the snippet exit) in the very same position in the
;; buffer. Therefore we need to single-link the
;; fields-or-mirrors-or-exit (which I have abbreviated to "fom")
;; according to their original positions in the buffer.
;;
;; Then we have operation `yas--advance-end-maybe' and
;; `yas--advance-start-maybe', which conditionally push the starts and
;; ends of these foms down the chain.
;;
;; This allows for like the printf with the magic ",":
;;
;;   printf ("${1:%s}\\n"${1:$(if (string-match "%" text) "," "\);")}  \
;;   $2${1:$(if (string-match "%" text) "\);" "")}$0
;;
(defun yas--fom-start (fom)
  (cond ((yas--field-p fom)
         (yas--field-start fom))
        ((yas--mirror-p fom)
         (yas--mirror-start fom))
        (t
         (yas--exit-marker fom))))

(defun yas--fom-end (fom)
  (cond ((yas--field-p fom)
         (yas--field-end fom))
        ((yas--mirror-p fom)
         (yas--mirror-end fom))
        (t
         (yas--exit-marker fom))))

(defun yas--fom-next (fom)
  (cond ((yas--field-p fom)
         (yas--field-next fom))
        ((yas--mirror-p fom)
         (yas--mirror-next fom))
        (t
         (yas--exit-next fom))))

(defun yas--fom-parent-field (fom)
  (cond ((yas--field-p fom)
         (yas--field-parent-field fom))
        ((yas--mirror-p fom)
         (yas--mirror-parent-field fom))
        (t
         nil)))

(defun yas--calculate-adjacencies (snippet)
  "Calculate adjacencies for fields or mirrors of SNIPPET.

This is according to their relative positions in the buffer, and
has to be called before the $-constructs are deleted."
  (let* ((fom-set-next-fom
         (lambda (fom nextfom)
           (cond ((yas--field-p fom)
                  (setf (yas--field-next fom) nextfom))
                 ((yas--mirror-p fom)
                  (setf (yas--mirror-next fom) nextfom))
                 (t
                  (setf (yas--exit-next fom) nextfom)))))
        (compare-fom-begs
         (lambda (fom1 fom2)
           (if (= (yas--fom-start fom2) (yas--fom-start fom1))
               (yas--mirror-p fom2)
             (>= (yas--fom-start fom2) (yas--fom-start fom1)))))
        (link-foms fom-set-next-fom))
    ;; make some yas--field, yas--mirror and yas--exit soup
    (let ((soup))
      (when (yas--snippet-exit snippet)
        (push (yas--snippet-exit snippet) soup))
      (dolist (field (yas--snippet-fields snippet))
        (push field soup)
        (dolist (mirror (yas--field-mirrors field))
          (push mirror soup)))
      (setq soup
            (sort soup compare-fom-begs))
      (when soup
        (cl-reduce link-foms soup)))))

(defun yas--calculate-simple-fom-parentage (snippet fom)
  "Discover if FOM is parented by some field in SNIPPET.

Use the tightest containing field if more than one field contains
the mirror.  Intended to be called *before* the dollar-regions are
deleted."
  (let ((min (point-min))
        (max (point-max)))
    (dolist (field (remq fom (yas--snippet-fields snippet)))
      (when (and (<= (yas--field-start field) (yas--fom-start fom))
                 (<= (yas--fom-end fom) (yas--field-end field))
               (< min (yas--field-start field))
               (< (yas--field-end field) max))
          (setq min (yas--field-start field)
                max (yas--field-end field))
          (cond ((yas--field-p fom)
                 (setf (yas--field-parent-field fom) field))
                ((yas--mirror-p fom)
                 (setf (yas--mirror-parent-field fom) field))
                (t ; it's an exit, so noop
                 nil ))))))

(defun yas--advance-end-maybe (fom newend)
  "Maybe advance FOM's end to NEWEND if it needs it.

If it does, also:

* call `yas--advance-start-maybe' on FOM's next fom.

* in case FOM is field call `yas--advance-end-maybe' on its parent
  field

Also, if FOM is an exit-marker, always call
`yas--advance-start-maybe' on its next fom.  This is because
exit-marker have identical start and end markers."
  (cond ((and fom (< (yas--fom-end fom) newend))
         (set-marker (yas--fom-end fom) newend)
         (yas--advance-start-maybe (yas--fom-next fom) newend)
         (yas--advance-end-of-parents-maybe (yas--fom-parent-field fom) newend))
        ((yas--exit-p fom)
         (yas--advance-start-maybe (yas--fom-next fom) newend))))

(defun yas--advance-end-maybe-previous-fields (field end snippets)
  "Call `yas--advance-end-maybe' on FIELD, and previous fields on SNIPPETS."
  (dolist (snippet snippets)
    (cl-assert (memq field (yas--snippet-fields snippet)))
    (yas--advance-end-maybe field end)
    (setq field (yas--snippet-previous-active-field snippet))))

(defun yas--advance-start-maybe (fom newstart)
  "Maybe advance FOM's start to NEWSTART if it needs it.

If it does, also call `yas--advance-end-maybe' on FOM."
  (when (and fom (< (yas--fom-start fom) newstart))
    (set-marker (yas--fom-start fom) newstart)
    (yas--advance-end-maybe fom newstart)))

(defun yas--advance-end-of-parents-maybe (field newend)
  "Like `yas--advance-end-maybe' but for parent fields.

Only works for fields and doesn't care about the start of the
next FOM.  Works its way up recursively for parents of parents."
  (when (and field
             (< (yas--field-end field) newend))
    (set-marker (yas--field-end field) newend)
    (yas--advance-end-of-parents-maybe (yas--field-parent-field field) newend)))

(defvar yas--dollar-regions nil
  "When expanding the snippet the \"parse-create\" functions add
cons cells to this var.")

(defvar yas--indent-markers nil
  "List of markers for manual indentation.")

(defun yas--snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((saved-quotes nil)
        (parse-start (point)))
    ;; Avoid major-mode's syntax propertizing function, since we
    ;; change the syntax-table while calling `scan-sexps'.
    (let ((syntax-propertize-function nil))
      (setq yas--dollar-regions nil)  ; Reset the yas--dollar-regions.
      (yas--protect-escapes nil '(?`))  ; Protect just the backquotes.
      (goto-char parse-start)
      (setq saved-quotes (yas--save-backquotes)) ; `expressions`.
      (yas--protect-escapes)            ; Protect escaped characters.
      (goto-char parse-start)
      (yas--indent-parse-create)        ; Parse indent markers: `$>'.
      (goto-char parse-start)
      (yas--field-parse-create snippet) ; Parse fields with {}.
      (goto-char parse-start)
      (yas--simple-fom-create snippet) ; Parse simple mirrors & fields.
      (goto-char parse-start)
      (yas--transform-mirror-parse-create snippet) ; Parse mirror transforms.
      ;; Invalidate any syntax-propertizing done while
      ;; `syntax-propertize-function' was nil.
      (syntax-ppss-flush-cache parse-start))
    ;; Set "next" links of fields & mirrors.
    (yas--calculate-adjacencies snippet)
    (yas--save-restriction-and-widen    ; Delete $-constructs.
      (yas--delete-regions yas--dollar-regions))
    ;; Make sure to do this insertion *after* deleting the dollar
    ;; regions, otherwise we invalidate the calculated positions of
    ;; all the fields following $0.
    (let ((exit (yas--snippet-exit snippet)))
      (goto-char (if exit (yas--exit-marker exit) (point-max))))
    (when (eq yas-wrap-around-region 'cua)
      (setq yas-wrap-around-region ?0))
    (cond ((and yas-wrap-around-region yas-selected-text)
           (insert yas-selected-text))
          ((and (characterp yas-wrap-around-region)
                (get-register yas-wrap-around-region))
           (insert (prog1 (get-register yas-wrap-around-region)
                     (set-register yas-wrap-around-region nil)))))
    (yas--restore-backquotes saved-quotes)  ; Restore `expression` values.
    (goto-char parse-start)
    (yas--restore-escapes)        ; Restore escapes.
    (yas--update-mirrors snippet) ; Update mirrors for the first time.
    (goto-char parse-start)))

;; HACK: Some implementations of `indent-line-function' (called via
;; `indent-according-to-mode') delete text before they insert (like
;; cc-mode), some make complicated regexp replacements (looking at
;; you, org-mode).  To find place where the marker "should" go after
;; indentation, we create a regexp based on what the line looks like
;; before, putting a capture group where the marker is.  The regexp
;; matches any whitespace with [[:space:]]* to allow for the
;; indentation changing whitespace.  Additionally, we try to preserve
;; the amount of whitespace *following* the marker, because
;; indentation generally affects whitespace at the beginning, not the
;; end.
;;
;; Two other cases where we apply a similar strategy:
;;
;; 1. Handling `auto-fill-mode', in this case we need to use the
;; current paragraph instead of line.
;;
;; 2. Moving snippets from an `org-src' temp buffer into the main org
;; buffer, in this case we need to count the relative line number
;; (because org may add indentation on each line making character
;; positions unreliable).
;;
;; Data formats:
;; (LOCATION) = (REGEXP WS-COUNT)
;; MARKER -> (MARKER . (LOCATION))
;; OVERLAY -> (OVERLAY LOCATION-BEG LOCATION-END)
;;
;; For `org-src' temp buffer, add a line number to format:
;; (LINE-LOCATION) = (LINE . (LOCATION))
;; MARKER@LINE -> (MARKER . (LINE-LOCATION))
;; OVERLAY@LINE -> (OVERLAY LINE-LOCATION-BEG LINE-LOCATION-END)
;;
;; This is all best-effort heuristic stuff, but it should cover 99% of
;; use-cases.

(defun yas--snapshot-location (position &optional beg end)
  "Returns info for restoring POSITIONS's location after indent.
The returned value is a list of the form (REGEXP WS-COUNT).
POSITION may be either a marker or just a buffer position.  The
REGEXP matches text between BEG..END which default to the current
line if omitted."
  (goto-char position)
  (unless beg (setq beg (line-beginning-position)))
  (unless end (setq end (line-end-position)))
  (let ((before (split-string (buffer-substring-no-properties beg position)
                              "[[:space:]\n]+" t))
        (after (split-string (buffer-substring-no-properties position end)
                             "[[:space:]\n]+" t)))
    (list (concat "[[:space:]\n]*"
                  (mapconcat (lambda (s)
                               (if (eq s position) "\\(\\)"
                                 (regexp-quote s)))
                             (nconc before (list position) after)
                             "[[:space:]\n]*"))
          (progn (skip-chars-forward "[:space:]\n" end)
                 (- (point) position)))))

(defun yas--snapshot-line-location (position &optional beg end)
  "Like `yas--snapshot-location', but return also line number.
Returned format is (LINE REGEXP WS-COUNT)."
  (goto-char position)
  (cons (count-lines (point-min) (line-beginning-position))
        (yas--snapshot-location position beg end)))

(defun yas--snapshot-overlay-location (overlay beg end)
  "Like `yas--snapshot-location' for overlays.
The returned format is (OVERLAY (RE WS) (RE WS)).  Either of
the (RE WS) lists may be nil if the start or end, respectively,
of the overlay is outside the range BEG .. END."
  (let ((obeg (overlay-start overlay))
        (oend (overlay-end overlay)))
    (list overlay
          (when (and (<= beg obeg) (< obeg end))
            (yas--snapshot-location obeg beg end))
          (when (and (<= beg oend) (< oend end))
            (yas--snapshot-location oend beg end)))))

(defun yas--snapshot-overlay-line-location (overlay)
  "Return info for restoring OVERLAY's line based location.
The returned format is (OVERLAY (LINE RE WS) (LINE RE WS))."
  (list overlay
        (yas--snapshot-line-location (overlay-start overlay))
        (yas--snapshot-line-location (overlay-end overlay))))

(defun yas--goto-saved-location (re-count)
  "Move to and return point saved by `yas--snapshot-location'.
Buffer must be narrowed to BEG..END used to create the snapshot info."
  (let ((regexp (pop re-count))
        (ws-count (pop re-count)))
    (goto-char (point-min))
    (if (not (looking-at regexp))
        (lwarn '(yasnippet re-marker) :warning
               "Couldn't find: %S" regexp)
      (goto-char (match-beginning 1))
      (skip-chars-forward "[:space:]\n")
      (skip-chars-backward "[:space:]\n" (- (point) ws-count)))
    (point)))

(defun yas--restore-overlay-location (ov-locations)
  "Restores marker based on info from `yas--snapshot-overlay-location'.
Buffer must be narrowed to BEG..END used to create the snapshot info."
  (cl-destructuring-bind (overlay loc-beg loc-end) ov-locations
    (move-overlay overlay
                  (if (not loc-beg) (overlay-start overlay)
                    (yas--goto-saved-location loc-beg))
                  (if (not loc-end) (overlay-end overlay)
                    (yas--goto-saved-location loc-end)))))

(defun yas--goto-saved-line-location (base-pos l-re-count)
  "Move to and return point saved by `yas--snapshot-line-location'.
Additionally requires BASE-POS to tell where the line numbers are
relative to."
  (goto-char base-pos)
  (forward-line (pop l-re-count))
  (save-restriction
    (narrow-to-region (line-beginning-position)
                      (line-end-position))
    (yas--goto-saved-location l-re-count)))

(defun yas--restore-overlay-line-location (base-pos ov-locations)
  "Restores marker based on info from `yas--snapshot-overlay-line-location'."
  (cl-destructuring-bind (overlay beg-l-r-w end-l-r-w)
      ov-locations
    (move-overlay overlay
                  (yas--goto-saved-line-location base-pos beg-l-r-w)
                  (yas--goto-saved-line-location base-pos end-l-r-w))))

(defun yas--indent-region (from to snippet)
  "Indent the lines between FROM and TO with `indent-according-to-mode'.
The SNIPPET's markers are preserved."
  (save-excursion
    (yas--save-restriction-and-widen
      (let* ((snippet-markers (yas--collect-snippet-markers snippet))
             (to (set-marker (make-marker) to)))
        (goto-char from)
        (cl-loop for bol = (line-beginning-position)
                 for eol = (line-end-position)
                 if (or yas-also-indent-empty-lines
                        (/= bol eol))
                 do
                 ;; Indent each non-empty line.
                 (let ((remarkers nil))
                   (dolist (m snippet-markers)
                     (when (and (<= bol m) (<= m eol))
                       (push (cons m (yas--snapshot-location m bol eol))
                             remarkers)))
                   (unwind-protect
                       (progn (back-to-indentation)
                              (indent-according-to-mode))
                     (save-restriction
                       (narrow-to-region bol (line-end-position))
                       (dolist (remarker remarkers)
                         (set-marker (car remarker)
                                     (yas--goto-saved-location (cdr remarker)))))))
                 while (and (zerop (forward-line 1))
                            (< (point) to)))))))

(defvar yas--indent-original-column nil)
(defun yas--indent (snippet)
  ;; Indent lines that had indent markers (`$>') on them.
  (save-excursion
    (dolist (marker yas--indent-markers)
      (unless (eq yas-indent-line 'auto)
        (goto-char marker)
        (yas--indent-region (line-beginning-position)
                            (line-end-position)
                            snippet))
      ;; Finished with this marker.
      (set-marker marker nil))
    (setq yas--indent-markers nil))
  ;; Now do stuff for `fixed' and `auto'.
  (save-excursion
    ;; We need to be at end of line, so that `forward-line' will only
    ;; report 0 if it actually moves over a newline.
    (end-of-line)
    (cond ((eq yas-indent-line 'fixed)
           (when (= (forward-line 1) 0)
             (let ((indent-line-function
                    (lambda ()
                      ;; We need to be at beginning of line in order to
                      ;; indent existing whitespace correctly.
                      (beginning-of-line)
                      (indent-to-column yas--indent-original-column))))
               (yas--indent-region (line-beginning-position)
                                   (point-max)
                                   snippet))))
          ((eq yas-indent-line 'auto)
           (when (or yas-also-auto-indent-first-line
                     (= (forward-line 1) 0))
             (yas--indent-region (line-beginning-position)
                                 (point-max)
                                 snippet))))))

(defun yas--collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (yas--snippet-map-markers (lambda (m) (push m markers) m) snippet)
    markers))

(defun yas--escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas--protect-escapes (&optional text escaped)
  "Protect all escaped characters with their numeric ASCII value.

With optional string TEXT do it in string instead of buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas--replace-all (concat "\\" (char-to-string escaped))
                                     (yas--escape-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas--escaped-characters))
    changed-text))

(defun yas--restore-escapes (&optional text escaped)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead of the buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas--replace-all (yas--escape-string escaped)
                                     (char-to-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas--escaped-characters))
    changed-text))

(defun yas--save-backquotes ()
  "Save all \"\\=`(lisp-expression)\\=`\"-style expressions.
Return a list of (MARKER . STRING) entires for each backquoted
Lisp expression."
  (let* ((saved-quotes nil)
         (yas--snippet-buffer (current-buffer))
         (yas--change-detected nil)
         (detect-change (lambda (_beg _end)
                          (when (eq (current-buffer) yas--snippet-buffer)
                            (setq yas--change-detected t)))))
    (while (re-search-forward yas--backquote-lisp-expression-regexp nil t)
      (let ((current-string (match-string-no-properties 1)) transformed)
        (yas--save-restriction-and-widen
          (delete-region (match-beginning 0) (match-end 0)))
        (let ((before-change-functions
               (cons detect-change before-change-functions)))
          (setq transformed (yas--eval-for-string (yas--read-lisp
                                                   (yas--restore-escapes
                                                    current-string '(?`))))))
        (goto-char (match-beginning 0))
        (when transformed
          (let ((marker (make-marker)))
            (yas--save-restriction-and-widen
              (insert "Y") ;; quite horrendous, I love it :)
              (set-marker marker (point))
              (insert "Y"))
            (push (cons marker transformed) saved-quotes)))))
    (when yas--change-detected
      (lwarn '(yasnippet backquote-change) :warning
             "`%s' modified buffer in a backquote expression.
  To hide this warning, add (yasnippet backquote-change) to `warning-suppress-types'."
             (if yas--current-template
                 (yas--template-name yas--current-template)
               "Snippet")))
    saved-quotes))

(defun yas--restore-backquotes (saved-quotes)
  "Replace markers in SAVED-QUOTES with their values.
SAVED-QUOTES is the in format returned by `yas--save-backquotes'."
  (cl-loop for (marker . string) in saved-quotes do
           (save-excursion
             (goto-char marker)
             (yas--save-restriction-and-widen
               (delete-char -1)
               (insert string)
               (delete-char 1))
             (set-marker marker nil))))

(defun yas--scan-sexps (from count)
  (ignore-errors
    (save-match-data ; `scan-sexps' may modify match data.
      ;; Parse using the syntax table corresponding to the yasnippet syntax.
      (with-syntax-table (standard-syntax-table)
        ;; And ignore syntax-table properties that may have been placed by the
        ;; major mode since these aren't related to the yasnippet syntax.
        (let ((parse-sexp-lookup-properties nil))
          (scan-sexps from count))))))

(defun yas--make-marker (pos)
  "Create a marker at POS with nil `marker-insertion-type'."
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

(defun yas--indent-parse-create ()
  "Parse the \"$>\" indentation markers just inserted."
  (setq yas--indent-markers ())
  (while (search-forward "$>" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    ;; Mark the beginning of the line.
    (push (yas--make-marker (line-beginning-position))
          yas--indent-markers))
  (setq yas--indent-markers (nreverse yas--indent-markers)))

(defun yas--scan-for-field-end ()
  (while (progn (re-search-forward "\\${\\|}")
                (when (eq (char-before) ?\{)
                  ;; Nested field.
                  (yas--scan-for-field-end))))
  (point))

(defun yas--field-parse-create (snippet &optional parent-field)
  "Parse most field expressions in SNIPPET, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;

* \"${n: text$(expression)}, the same with a Lisp expression; this is caught
  with the curiously named `yas--multi-dollar-lisp-expression-regexp'

* the same as above but unnumbered, (no N:) and number is calculated
  automatically.

When multiple expressions are found, only the last one counts."
  ;;
  (save-excursion
    (while (re-search-forward yas--field-regexp nil t)
      (let* ((brace-scan (save-match-data
                           (goto-char (match-beginning 2))
                           (yas--scan-for-field-end)))
             ;; if the `brace-scan' didn't reach a brace, we have a
             ;; snippet with invalid escaping, probably a closing
             ;; brace escaped with two backslashes (github#979). But
             ;; be lenient, because we can.
             (real-match-end-0 (if (eq ?} (char-before brace-scan))
                                   brace-scan
                                 (point)))
             (number (and (match-string-no-properties 1)
                          (string-to-number (match-string-no-properties 1))))
             (brand-new-field (and real-match-end-0
                                   ;; break if on "$(" immediately
                                   ;; after the ":", this will be
                                   ;; caught as a mirror with
                                   ;; transform later.
                                   (not (string-match-p "\\`\\$[ \t\n]*("
                                                        (match-string-no-properties 2)))
                                   ;; allow ${0: some exit text}
                                   ;; (not (and number (zerop number)))
                                   (yas--make-field number
                                                   (yas--make-marker (match-beginning 2))
                                                   (yas--make-marker (1- real-match-end-0))
                                                   parent-field))))
        (when brand-new-field
          (goto-char real-match-end-0)
          (push (cons (1- real-match-end-0) real-match-end-0)
                yas--dollar-regions)
          (push (cons (match-beginning 0) (match-beginning 2))
                yas--dollar-regions)
          (push brand-new-field (yas--snippet-fields snippet))
          (save-excursion
            (save-restriction
              (narrow-to-region (yas--field-start brand-new-field) (yas--field-end brand-new-field))
              (goto-char (point-min))
              (yas--field-parse-create snippet brand-new-field)))))))
  ;; if we entered from a parent field, now search for the
  ;; `yas--multi-dollar-lisp-expression-regexp'. This is used for
  ;; primary field transformations
  ;;
  (when parent-field
    (save-excursion
      (while (re-search-forward yas--multi-dollar-lisp-expression-regexp nil t)
        (let* ((real-match-end-1 (yas--scan-sexps (match-beginning 1) 1)))
          ;; commit the primary field transformation if:
          ;;
          ;; 1. we don't find it in yas--dollar-regions (a subnested
          ;; field) might have already caught it.
          ;;
          ;; 2. we really make sure we have either two '$' or some
          ;; text and a '$' after the colon ':'. This is a FIXME: work
          ;; my regular expressions and end these ugly hacks.
          ;;
          (when (and real-match-end-1
                     (not (member (cons (match-beginning 0)
                                        real-match-end-1)
                                  yas--dollar-regions))
                     (not (eq ?:
                              (char-before (1- (match-beginning 1))))))
            (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1)
                                                                          real-match-end-1)))
              (setf (yas--field-transform parent-field)
                    (yas--read-lisp (yas--restore-escapes lisp-expression-string))))
            (push (cons (match-beginning 0) real-match-end-1)
                  yas--dollar-regions)))))))

(defun yas--transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations in SNIPPET."
  (while (re-search-forward yas--transform-mirror-regexp nil t)
    (let* ((real-match-end-0 (yas--scan-sexps (1+ (match-beginning 0)) 1))
           (number (string-to-number (match-string-no-properties 1)))
           (field (and number
                       (not (zerop number))
                       (yas--snippet-find-field snippet number)))
           (brand-new-mirror
            (and real-match-end-0
                 field
                 (yas--make-mirror (yas--make-marker (match-beginning 0))
                                  (yas--make-marker (match-beginning 0))
                                  (yas--read-lisp
                                   (yas--restore-escapes
                                    (buffer-substring-no-properties (match-beginning 2)
                                                                    (1- real-match-end-0))))))))
      (when brand-new-mirror
        (push brand-new-mirror
              (yas--field-mirrors field))
        (yas--calculate-simple-fom-parentage snippet brand-new-mirror)
        (push (cons (match-beginning 0) real-match-end-0) yas--dollar-regions)))))

(defun yas--simple-fom-create (snippet)
  "Parse the simple \"$n\" fields/mirrors/exitmarkers in SNIPPET."
  (while (re-search-forward yas--simple-mirror-regexp nil t)
    (let ((number (string-to-number (match-string-no-properties 1))))
      (cond ((zerop number)
             (setf (yas--snippet-exit snippet)
                   (yas--make-exit (yas--make-marker (match-end 0))))
             (push (cons (match-beginning 0) (yas--exit-marker (yas--snippet-exit snippet)))
                   yas--dollar-regions))
            (t
             (let ((field (yas--snippet-find-field snippet number))
                   (fom))
               (if field
                   (push
                    (setq fom (yas--make-mirror
                               (yas--make-marker (match-beginning 0))
                               (yas--make-marker (match-beginning 0))
                               nil))
                    (yas--field-mirrors field))
                 (push
                  (setq fom (yas--make-field number
                                             (yas--make-marker (match-beginning 0))
                                             (yas--make-marker (match-beginning 0))
                                             nil))
                  (yas--snippet-fields snippet)))
               (yas--calculate-simple-fom-parentage snippet fom))
             (push (cons (match-beginning 0) (match-end 0))
                   yas--dollar-regions))))))

(defun yas--delete-regions (regions)
  "Sort disjuct REGIONS by start point, then delete from the back."
  (mapc #'(lambda (reg)
            (delete-region (car reg) (cdr reg)))
        (sort regions
              #'(lambda (r1 r2)
                  (>= (car r1) (car r2))))))

(defun yas--calculate-mirror-depth (mirror &optional traversed)
  (let* ((parent (yas--mirror-parent-field mirror))
         (parents-mirrors (and parent
                               (yas--field-mirrors parent))))
    (or (yas--mirror-depth mirror)
        (setf (yas--mirror-depth mirror)
              (cond ((memq mirror traversed) 0)
                    ((and parent parents-mirrors)
                     (1+ (cl-reduce
                          #'max parents-mirrors
                          :key (lambda (m)
                                 (yas--calculate-mirror-depth
                                  m (cons mirror traversed))))))
                    (parent 1)
                    (t 0))))))

(defun yas--snippet-field-mirrors (snippet)
  ;; Make a list of (FIELD . MIRROR).
  (cl-sort
   (cl-mapcan (lambda (field)
                (mapcar (lambda (mirror)
                          (cons field mirror))
                        (yas--field-mirrors field)))
              (yas--snippet-fields snippet))
   ;; Then sort this list so that entries with mirrors with
   ;; parent fields appear before.  This was important for
   ;; fixing #290, and also handles the case where a mirror in
   ;; a field causes another mirror to need reupdating.
   #'> :key (lambda (fm) (yas--calculate-mirror-depth (cdr fm)))))

(defun yas--indent-mirrors-of-snippet (snippet &optional f-ms)
  ;; Indent mirrors of SNIPPET.  F-MS is the return value of
  ;; (yas--snippet-field-mirrors SNIPPET).
  (when (eq yas-indent-line 'auto)
    (let ((yas--inhibit-overlay-hooks t))
      (cl-loop for (beg . end) in
               (cl-sort (mapcar (lambda (f-m)
                                  (let ((mirror (cdr f-m)))
                                    (cons (yas--mirror-start mirror)
                                          (yas--mirror-end mirror))))
                                (or f-ms
                                    (yas--snippet-field-mirrors snippet)))
                        #'< :key #'car)
               do (yas--indent-region beg end snippet)))))

(defun yas--update-mirrors (snippet)
  "Update all the mirrors of SNIPPET."
  (yas--save-restriction-and-widen
    (save-excursion
      (let ((f-ms (yas--snippet-field-mirrors snippet)))
        (cl-loop
         for (field . mirror) in f-ms
         ;; Before updating a mirror with a parent-field, maybe advance
         ;; its start (#290).
         do (let ((parent-field (yas--mirror-parent-field mirror)))
              (when parent-field
                (yas--advance-start-maybe mirror (yas--fom-start parent-field))))
         ;; Update this mirror.
         do (yas--mirror-update-display mirror field)
         ;; `yas--place-overlays' is needed since the active field and
         ;; protected overlays might have been changed because of insertions
         ;; in `yas--mirror-update-display'.
         do (let ((active-field (yas--snippet-active-field snippet)))
              (when active-field (yas--place-overlays snippet active-field))))
        ;; Delay indenting until we're done all mirrors.  We must do
        ;; this to avoid losing whitespace between fields that are
        ;; still empty (i.e., they will be non-empty after updating).
        (yas--indent-mirrors-of-snippet snippet f-ms)))))

(defun yas--mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."

  (let* ((mirror-parent-field (yas--mirror-parent-field mirror))
         (reflection (and (not (and mirror-parent-field
                                    (yas--field-modified-p mirror-parent-field)))
                          (or (yas--apply-transform mirror field 'empty-on-nil)
                              (yas--field-text-for-display field)))))
    (when (and reflection
               (not (string= reflection (buffer-substring-no-properties (yas--mirror-start mirror)
                                                                        (yas--mirror-end mirror)))))
      (goto-char (yas--mirror-start mirror))
      (let ((yas--inhibit-overlay-hooks t))
        (insert reflection))
      (if (> (yas--mirror-end mirror) (point))
          (delete-region (point) (yas--mirror-end mirror))
        (set-marker (yas--mirror-end mirror) (point))
        (yas--advance-start-maybe (yas--mirror-next mirror) (point))
        ;; super-special advance
        (yas--advance-end-of-parents-maybe mirror-parent-field (point))))))

(defun yas--field-update-display (field)
  "Much like `yas--mirror-update-display', but for fields."
  (when (yas--field-transform field)
    (let ((transformed (and (not (eq (yas--field-number field) 0))
                            (yas--apply-transform field field))))
      (when (and transformed
                 (not (string= transformed (buffer-substring-no-properties (yas--field-start field)
                                                                           (yas--field-end field)))))
        (setf (yas--field-modified-p field) t)
        (goto-char (yas--field-start field))
        (let ((yas--inhibit-overlay-hooks t))
          (insert transformed)
          (if (> (yas--field-end field) (point))
              (delete-region (point) (yas--field-end field))
            (set-marker (yas--field-end field) (point))
            (yas--advance-start-maybe (yas--field-next field) (point)))
          t)))))


;;; Post-command hook:
;;
(defun yas--post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (yas--do-todo-snippet-indent)
  (condition-case err
      (progn (yas--finish-moving-snippets)
             (cond ((eq 'undo this-command)
                    ;;
                    ;; After undo revival the correct field is sometimes not
                    ;; restored correctly, this condition handles that
                    ;;
                    (let* ((snippet (car (yas-active-snippets)))
                           (target-field
                            (and snippet
                                 (cl-find-if-not
                                  (lambda (field)
                                    (yas--field-probably-deleted-p snippet field))
                                  (remq nil
                                        (cons (yas--snippet-active-field snippet)
                                              (yas--snippet-fields snippet)))))))
                      (when target-field
                        (yas--move-to-field snippet target-field))))
                   ((not (yas--undo-in-progress))
                    ;; When not in an undo, check if we must commit the snippet
                    ;; (user exited it).
                    (yas--check-commit-snippet))))
    ((debug error) (signal (car err) (cdr err)))))

;;; Fancy docs:
;;
;; The docstrings for some functions are generated dynamically
;; depending on the context.
;;
(put 'yas-expand  'function-documentation
     '(yas--expand-from-trigger-key-doc t))
(defun yas--expand-from-trigger-key-doc (context)
  "A doc synthesizer for `yas--expand-from-trigger-key-doc'."
  (let* ((yas-fallback-behavior (and context yas-fallback-behavior))
         (fallback-description
          (cond ((eq yas-fallback-behavior 'call-other-command)
                 (let* ((fallback (yas--keybinding-beyond-yasnippet)))
                   (or (and fallback
                            (format "call command `%s'."
                                    (pp-to-string fallback)))
                       "do nothing (`yas-expand' doesn't override\nanything).")))
                ((eq yas-fallback-behavior 'return-nil)
                 "do nothing.")
                (t "defer to `yas-fallback-behavior' (which see)."))))
    (concat "Expand a snippet before point. If no snippet
expansion is possible, "
            fallback-description
            "\n\nOptional argument FIELD is for non-interactive use and is an
object satisfying `yas--field-p' to restrict the expansion to.")))

(put 'yas-expand-from-keymap 'function-documentation
     '(yas--expand-from-keymap-doc t))
(defun yas--expand-from-keymap-doc (context)
  "A doc synthesizer for `yas--expand-from-keymap-doc'."
  (add-hook 'temp-buffer-show-hook #'yas--snippet-description-finish-runonce)
  (concat "Expand/run snippets from keymaps, possibly falling back to original binding.\n"
          (when (and context (eq this-command 'describe-key))
            (let* ((vec (this-single-command-keys))
                   (templates (cl-mapcan (lambda (table)
                                           (yas--fetch table vec))
                                         (yas--get-snippet-tables)))
                   (yas--direct-keymaps nil)
                   (fallback (key-binding vec)))
              (concat "In this case, "
                      (when templates
                        (concat "these snippets are bound to this key:\n"
                                (yas--template-pretty-list templates)
                                "\n\nIf none of these expands, "))
                      (or (and fallback
                               (format "fallback `%s' will be called." (pp-to-string fallback)))
                          "no fallback keybinding is called."))))))

(defun yas--template-pretty-list (templates)
  (let ((acc)
        (yas-buffer-local-condition 'always))
    (dolist (plate templates)
      (setq acc (concat acc "\n*) "
                        (propertize (concat "\\\\snippet `" (car plate) "'")
                                    'yasnippet (cdr plate)))))
    acc))

(define-button-type 'help-snippet-def
  :supertype 'help-xref
  'help-function (lambda (template) (yas--visit-snippet-file-1 template))
  'help-echo (purecopy "mouse-2, RET: find snippets's definition"))

(defun yas--snippet-description-finish-runonce ()
  "Final adjustments for the help buffer when snippets are concerned."
  (yas--create-snippet-xrefs)
  (remove-hook 'temp-buffer-show-hook
               #'yas--snippet-description-finish-runonce))

(defun yas--create-snippet-xrefs ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\\\\\snippet[ \s\t]+`\\([^']+\\)'" nil t)
      (let ((template (get-text-property (match-beginning 1)
                                         'yasnippet)))
        (when template
          (help-xref-button 1 'help-snippet-def template)
          (delete-region (match-end 1) (match-end 0))
          (delete-region (match-beginning 0) (match-beginning 1)))))))

;;; Eldoc configuration.
(eldoc-add-command 'yas-next-field-or-maybe-expand
                   'yas-next-field 'yas-prev-field
                   'yas-expand 'yas-expand-from-keymap
                   'yas-expand-from-trigger-key)

;;; Utils

(defvar yas-verbosity 3
  "Log level for `yas--message' 4 means trace most anything, 0 means nothing.")

(defun yas--message (level message &rest args)
  "When LEVEL is at or below `yas-verbosity', log MESSAGE and ARGS."
  (when (>= yas-verbosity level)
    (message "%s" (apply #'yas--format message args))))

(defun yas--warning (format-control &rest format-args)
  (let ((msg (apply #'format format-control format-args)))
    (display-warning 'yasnippet msg :warning)
    (yas--message 1 msg)))

(defun yas--format (format-control &rest format-args)
  (apply #'format (concat "[yas] " format-control) format-args))


;;; Unloading

(defvar unload-function-defs-list) ; loadhist.el

(defun yasnippet-unload-function ()
  "Disable minor modes when calling `unload-feature'."
  ;; Disable `yas-minor-mode' everywhere it's enabled.
  (yas-global-mode -1)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when yas-minor-mode
        (yas-minor-mode -1))))
  ;; Remove symbol properties of all our functions, this avoids
  ;; Bug#25088 in Emacs 25.1, where the compiler macro on
  ;; `cl-defstruct' created functions hang around in the symbol plist
  ;; and cause errors when loading again (we don't *need* to clean
  ;; *all* symbol plists, but it's easier than being precise).
  (dolist (def unload-function-defs-list)
    (when (eq (car-safe def) 'defun)
      (setplist (cdr def) nil)))
  ;; Return nil so that `unload-feature' will take of undefining
  ;; functions, and changing any buffers using `snippet-mode'.
  nil)


;;; Backward compatibility to yasnippet <= 0.7

(defun yas-initialize ()
  "For backward compatibility, enable `yas-minor-mode' globally."
  (declare (obsolete "Use (yas-global-mode 1) instead." "0.8"))
  (yas-global-mode 1))

(defvar yas--backported-syms '(;; `defcustom's
                             ;;
                             yas-snippet-dirs
                             yas-prompt-functions
                             yas-indent-line
                             yas-also-auto-indent-first-line
                             yas-snippet-revival
                             yas-triggers-in-field
                             yas-fallback-behavior
                             yas-choose-keys-first
                             yas-choose-tables-first
                             yas-use-menu
                             yas-trigger-symbol
                             yas-wrap-around-region
                             yas-good-grace
                             yas-visit-from-menu
                             yas-expand-only-for-last-commands
                             yas-field-highlight-face

                             ;; these vars can be customized as well
                             ;;
                             yas-keymap
                             yas-verbosity
                             yas-extra-modes
                             yas-key-syntaxes
                             yas-after-exit-snippet-hook
                             yas-before-expand-snippet-hook
                             yas-buffer-local-condition
                             yas-dont-activate

                             ;; prompting functions
                             ;;
                             yas-x-prompt
                             yas-ido-prompt
                             yas-no-prompt
                             yas-completing-prompt
                             yas-dropdown-prompt

                             ;; interactive functions
                             ;;
                             yas-expand
                             yas-minor-mode
                             yas-global-mode
                             yas-direct-keymaps-reload
                             yas-minor-mode-on
                             yas-load-directory
                             yas-reload-all
                             yas-compile-directory
                             yas-recompile-all
                             yas-about
                             yas-expand-from-trigger-key
                             yas-expand-from-keymap
                             yas-insert-snippet
                             yas-visit-snippet-file
                             yas-new-snippet
                             yas-load-snippet-buffer
                             yas-tryout-snippet
                             yas-describe-tables
                             yas-next-field-or-maybe-expand
                             yas-next-field
                             yas-prev-field
                             yas-abort-snippet
                             yas-exit-snippet
                             yas-exit-all-snippets
                             yas-skip-and-clear-or-delete-char
                             yas-initialize

                             ;; symbols that I "exported" for use
                             ;; in snippets and hookage
                             ;;
                             yas-expand-snippet
                             yas-define-snippets
                             yas-define-menu
                             yas-snippet-beg
                             yas-snippet-end
                             yas-modified-p
                             yas-moving-away-p
                             yas-substr
                             yas-choose-value
                             yas-key-to-value
                             yas-throw
                             yas-verify-value
                             yas-field-value
                             yas-text
                             yas-selected-text
                             yas-default-from-field
                             yas-inside-string
                             yas-unimplemented
                             yas-define-condition-cache
                             yas-hippie-try-expand

                             ;; debug definitions
                             ;; yas-debug-snippet-vars
                             ;; yas-exterminate-package
                             ;; yas-debug-test

                             ;; testing definitions
                             ;; yas-should-expand
                             ;; yas-should-not-expand
                             ;; yas-mock-insert
                             ;; yas-make-file-or-dirs
                             ;; yas-variables
                             ;; yas-saving-variables
                             ;; yas-call-with-snippet-dirs
                             ;; yas-with-snippet-dirs
)
  "Backported yasnippet symbols.

They are mapped to \"yas/*\" variants.")

(when yas-alias-to-yas/prefix-p
  (dolist (sym yas--backported-syms)
    (let ((backported (intern (replace-regexp-in-string "\\`yas-" "yas/" (symbol-name sym)))))
      (when (boundp sym)
        (make-obsolete-variable backported sym "yasnippet 0.8")
        (defvaralias backported sym))
      (when (fboundp sym)
        (make-obsolete backported sym "yasnippet 0.8")
        (defalias backported sym))))
  (make-obsolete 'yas/root-directory 'yas-snippet-dirs "yasnippet 0.8")
  (defvaralias 'yas/root-directory 'yas-snippet-dirs))

(defvar yas--exported-syms
  (let (exported)
    (mapatoms (lambda (atom)
                (if (and (or (and (boundp atom)
                                  (not (get atom 'byte-obsolete-variable)))
                             (and (fboundp atom)
                                  (not (get atom 'byte-obsolete-info))))
                         (string-match-p "\\`yas-[^-]" (symbol-name atom)))
                    (push atom exported))))
    exported)
  "Exported yasnippet symbols.

i.e. the ones with \"yas-\" single dash prefix. I will try to
keep them in future yasnippet versions and other elisp libraries
can more or less safely rely upon them.")


(provide 'yasnippet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; yasnippet.el ends here
