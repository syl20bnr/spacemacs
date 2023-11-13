;;; smartparens.el --- Automatic insertion, wrapping and paredit-like navigation with user defined pairs. -*- lexical-binding: t -*-

;; Copyright (C) 2012-2023 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Version: 1.11.0
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Smartparens is minor mode for Emacs that deals with parens pairs
;; and tries to be smart about it.  It started as a unification effort
;; to combine functionality of several existing packages in a single,
;; compatible and extensible way to deal with parentheses, delimiters,
;; tags and the like.  Some of these packages include autopair,
;; textmate, wrap-region, electric-pair-mode, paredit and others.  With
;; the basic features found in other packages it also brings many
;; improvements as well as completely new features.

;; For a basic overview, see github readme at
;; https://github.com/Fuco1/smartparens

;; For the complete documentation visit the documentation wiki located
;; at https://github.com/Fuco1/smartparens/wiki

;; If you like this project, you can donate here:
;; https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C

;;; Code:

(eval-when-compile
  (require 'subr-x) ; for `string-trim'
  (require 'cl-lib))

(require 'dash)
(require 'thingatpt)
(require 'help-mode) ;; for help-xref-following #85
(require 'loadhist)

(declare-function cua-replace-region "cua-base") ; FIXME: remove this when we drop support for old emacs
(declare-function cua-delete-region "cua-base")
(declare-function cua--fallback "cua-base")
(declare-function package-version-join "package")
(declare-function package-desc-version "package")

(declare-function subword-kill "subword")
(declare-function subword-forward "subword")
(declare-function subword-backward "subword")

(declare-function hungry-delete-backward "hungry-delete")
(declare-function hungry-delete-forward "hungry-delete")

(declare-function evil-get-register "evil-common")
(declare-function evil-set-register "evil-common")
(eval-when-compile
  (defvar evil-this-register)
  (defvar package-alist)
  (defvar sp-autoskip-closing-pair)
  (defvar sp-show-enclosing-pair-commands)
  (defvar show-smartparens-mode))


;;; backport for older emacsen

;; introduced in 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var)))))

;;;###autoload
(defun sp-cheat-sheet (&optional arg)
  "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation."
  (interactive "P")
  (setq arg (not arg))
  (let ((do-not-display '(
                          smartparens-mode
                          smartparens-global-mode
                          turn-on-smartparens-mode
                          turn-off-smartparens-mode
                          sp-wrap-cancel
                          sp-remove-active-pair-overlay
                          sp-splice-sexp-killing-around ;; is aliased to `sp-raise-sexp'
                          show-smartparens-mode
                          show-smartparens-global-mode
                          turn-on-show-smartparens-mode
                          turn-off-show-smartparens-mode
                          ))
        (do-not-display-with-arg '(
                                   sp-use-paredit-bindings
                                   sp-use-smartparens-bindings
                                   ))
        (commands (cl-loop for i in (cdr (feature-symbols 'smartparens))
                           if (and (eq (car-safe i) 'defun) (commandp (cdr i)))
                           collect (cdr i))))
    (with-current-buffer (get-buffer-create "*Smartparens cheat sheet*")
      (let ((standard-output (current-buffer))
            (help-xref-following t))
        (read-only-mode -1)
        (erase-buffer)
        (help-mode)
        (smartparens-mode 1)
        (help-setup-xref (list #'sp-cheat-sheet)
                         (called-interactively-p 'interactive))
        (read-only-mode -1)
        (--each (--remove (or (memq it do-not-display)
                              (and arg (memq it do-not-display-with-arg)))
                          commands)
          (unless (equal (symbol-name it) "advice-compilation")
            (let ((start (point)) kill-from)
              (insert (propertize (symbol-name it) 'face 'font-lock-function-name-face))
              (insert " is ")
              (describe-function-1 it)
              (save-excursion
                (when arg
                  (goto-char start)
                  (forward-paragraph 1)
                  (forward-line 1)
                  (if (looking-at "^It is bound")
                      (forward-paragraph 2)
                    (forward-paragraph 1))
                  (setq kill-from (point))
                  (when (re-search-forward "^Examples:" nil t)
                    (delete-region kill-from
                                   (save-excursion
                                     (forward-line 1)
                                     (point))))))
              (insert (propertize (concat
                                   "\n\n"
                                   (make-string 72 ?―)
                                   "\n\n") 'face 'font-lock-function-name-face)))))
        (goto-char (point-min))
        (while (re-search-forward "\\(->\\|​\\)" nil t)
          (let ((thing (bounds-of-thing-at-point 'line)))
            (put-text-property (car thing) (cdr thing) 'face 'font-lock-string-face)))
        (goto-char (point-min))
        (while (re-search-forward "|" nil t)
          (put-text-property (1- (point)) (point) 'face 'font-lock-warning-face))
        (goto-char (point-min))
        (while (re-search-forward "^It is bound to \\(.*?\\)\\." nil t)
          (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face))
        (goto-char (point-min))
        (while (re-search-forward ";;.*?$" nil t)
          (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-comment-face))
        (help-make-xrefs)
        (goto-char (point-min))))
    (pop-to-buffer "*Smartparens cheat sheet*")))

(defun sp-describe-system (starterkit)
  "Describe user's system.

The output of this function can be used in bug reports."
  (interactive
   (list (completing-read "Starterkit/Distribution used: "
                          (list
                           "Spacemacs"
                           "Evil"
                           "Vanilla"
                           ))))
  (let ((text (format "- `smartparens` version: %s
- Active `major-mode`: `%s`
- Smartparens strict mode: %s
- Emacs version (`M-x emacs-version`): %s
- Starterkit/Distribution: %s
- OS: %s"
                      (--if-let (cadr (assoc 'smartparens package-alist))
                          (package-version-join (package-desc-version it))
                        "<Please specify manually>")
                      (symbol-name major-mode)
                      (bound-and-true-p smartparens-strict-mode)
                      (replace-regexp-in-string "\n" "" (emacs-version))
                      starterkit
                      (symbol-name system-type))))
    (pop-to-buffer
     (with-current-buffer (get-buffer-create "*sp-describe-system*")
       (erase-buffer)
       (insert "The content of the buffer underneath the line was
copied to your clipboard.  You can also edit it in this buffer
and then copy the results manually.
------------------------------------------------
")
       (insert text)
       (current-buffer)))
    (kill-new text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar-local sp-forward-bound-fn nil
  "Function to restrict the forward search")

(defvar-local sp-backward-bound-fn nil
  "Function to restrict the backward search")

(defun sp--get-forward-bound ()
  "Get the bound to limit the forward search for looking for pairs.

If it returns nil, the original bound passed to the search
function will be considered."
  (and sp-forward-bound-fn (funcall sp-forward-bound-fn)))

(defun sp--get-backward-bound ()
  "Get the bound to limit the backward search for looking for pairs.

If it returns nil, the original bound passed to the search
function will be considered."
  (and sp-backward-bound-fn (funcall sp-backward-bound-fn)))


(defvaralias 'sp-keymap 'smartparens-mode-map)
(make-obsolete-variable 'sp-keymap 'smartparens-mode-map "2015-01-01")

;;;###autoload
(defvar smartparens-mode-map (make-sparse-keymap)
  "Keymap used for `smartparens-mode'.")

(defvar sp-paredit-bindings '(
                              ("C-M-f" . sp-forward-sexp) ;; navigation
                              ("C-M-b" . sp-backward-sexp)
                              ("C-M-u" . sp-backward-up-sexp)
                              ("C-M-d" . sp-down-sexp)
                              ("C-M-p" . sp-backward-down-sexp)
                              ("C-M-n" . sp-up-sexp)
                              ("M-s" . sp-splice-sexp) ;; depth-changing commands
                              ("M-<up>" . sp-splice-sexp-killing-backward)
                              ("M-<down>" . sp-splice-sexp-killing-forward)
                              ("M-r" . sp-splice-sexp-killing-around)
                              ("M-(" . sp-wrap-round)
                              ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
                              ("C-<right>" . sp-forward-slurp-sexp)
                              ("C-}" . sp-forward-barf-sexp)
                              ("C-<left>" . sp-forward-barf-sexp)
                              ("C-(" . sp-backward-slurp-sexp)
                              ("C-M-<left>" . sp-backward-slurp-sexp)
                              ("C-{" . sp-backward-barf-sexp)
                              ("C-M-<right>" . sp-backward-barf-sexp)
                              ("M-S" . sp-split-sexp) ;; misc
                              ("M-j" . sp-join-sexp)
                              ("M-?" . sp-convolute-sexp)
                              )
  "Paredit inspired bindings.

Alist containing the default paredit bindings to corresponding
smartparens functions.")

(defun sp--populate-keymap (bindings)
  "Populates the `smartparens-mode-map' from the BINDINGS alist."
  (--each bindings
    (define-key smartparens-mode-map (read-kbd-macro (car it)) (cdr it))))

;;;###autoload
(defun sp-use-paredit-bindings ()
  "Initiate `smartparens-mode-map' with `sp-paredit-bindings'."
  (interactive)
  (sp--populate-keymap sp-paredit-bindings))

(defvar sp-smartparens-bindings '(
                                  ("C-M-f" . sp-forward-sexp)
                                  ("C-M-b" . sp-backward-sexp)
                                  ("C-M-d" . sp-down-sexp)
                                  ("C-M-a" . sp-backward-down-sexp)
                                  ("C-S-d" . sp-beginning-of-sexp)
                                  ("C-S-a" . sp-end-of-sexp)
                                  ("C-M-e" . sp-up-sexp)
                                  ("C-M-u" . sp-backward-up-sexp)
                                  ("C-M-n" . sp-next-sexp)
                                  ("C-M-p" . sp-previous-sexp)
                                  ("C-M-k" . sp-kill-sexp)
                                  ("C-M-w" . sp-copy-sexp)
                                  ("M-<delete>" . sp-unwrap-sexp)
                                  ("M-<backspace>" . sp-backward-unwrap-sexp)
                                  ("C-<right>" . sp-forward-slurp-sexp)
                                  ("C-<left>" . sp-forward-barf-sexp)
                                  ("C-M-<left>" . sp-backward-slurp-sexp)
                                  ("C-M-<right>" . sp-backward-barf-sexp)
                                  ("M-D" . sp-splice-sexp)
                                  ("C-M-<delete>" . sp-splice-sexp-killing-forward)
                                  ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
                                  ("C-S-<backspace>" . sp-splice-sexp-killing-around)
                                  ("C-]" . sp-select-next-thing-exchange)
                                  ("C-M-]" . sp-select-next-thing)
                                  ("C-M-SPC" . sp-mark-sexp)
                                  ("M-F" . sp-forward-symbol)
                                  ("M-B" . sp-backward-symbol)
                                  )
  "Alist containing the default smartparens bindings.")

;;;###autoload
(defun sp-use-smartparens-bindings ()
  "Initiate `smartparens-mode-map' with `sp-smartparens-bindings'."
  (interactive)
  (sp--populate-keymap sp-smartparens-bindings))

(defun sp--set-base-key-bindings (&optional symbol value)
  "Set up the default keymap based on `sp-base-key-bindings'.

SYMBOL is the symbol being set, that is `sp-base-key-bindings'.

VALUE is the saved value (as a symbol), can be one of:
- sp
- paredit

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  (cond
   ((eq value 'sp)
    (sp-use-smartparens-bindings))
   ((eq value 'paredit)
    (sp-use-paredit-bindings))))

(defun sp--update-override-key-bindings (&optional symbol value)
  "Override the key bindings with values from `sp-override-key-bindings'.

SYMBOL is `sp-override-key-bindings', VALUE is the value being set.

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  ;; this also needs to reload the base set, if any is present.
  (sp--set-base-key-bindings)
  (sp--populate-keymap value))

(defcustom sp-base-key-bindings nil
  "A default set of key bindings for commands provided by smartparens.

Paredit binding adds the bindings in `sp-paredit-bindings' to the
corresponding smartparens commands.  It does not add bindings to
any other commands, or commands that do not have a paredit
counterpart.

Smartparens binding adds the bindings in
`sp-smartparens-bindings' to most common smartparens commands.
These are somewhat inspired by paredit, but in many cases differ.

Note that neither \"paredit\" nor \"smartparens\" bindings add a
binding for all the provided commands."
  :type '(radio
          (const :tag "Don't use any default set of bindings" nil)
          (const :tag "Use smartparens set of bindings" sp)
          (const :tag "Use paredit set of bindings" paredit))
  :set 'sp--set-base-key-bindings
  :group 'smartparens)

(defcustom sp-override-key-bindings nil
  "An alist of bindings and commands that should override the base key set.

If you wish to override a binding from the base set, set the
value for the binding to the `kbd' recognizable string constant
and command to the command symbol you wish to bind there.

If you wish to disable a binding from the base set, set the value
for the command to nil.

Examples:
 (\"C-M-f\" . sp-forward-sexp)
 (\"C-<right>\" . nil)

See `sp-base-key-bindings'."
  :type '(alist
          :key-type string
          :value-type symbol)
  :set 'sp--update-override-key-bindings
  :group 'smartparens)

(defvar-local sp-escape-char nil
  "Character used to escape quotes inside strings.")

(defvar-local sp-comment-char nil
  "Character used to start comments.")

(defvar-local sp-pair-list nil
  "List of pairs for autoinsertion or wrapping.

Maximum length of opening or closing pair is
`sp-max-pair-length' characters.")

(defvar-local sp-local-pairs nil
  "List of pair definitions used for current buffer.")

(defvar-local sp-last-operation nil
  "Symbol holding the last successful operation.")

(cl-defstruct sp-state
  "Smartparens state for the current buffer."
  ;; A "counter" to track delayed hook.  When a pair is inserted, a
  ;; cons of the form (:next . pair) is stored.  On the next
  ;; (immediately after insertion) invocation of post-command-hook, it
  ;; is changed to (:this . pair).  When the `car' is :this, the
  ;; post-command-hook checks the delayed hooks for `pair' and
  ;; executes them, then reset the "counter".
  delayed-hook
  ;; TODO
  delayed-insertion
  ;; The last point checked by sp--syntax-ppss and its result, used for
  ;; memoization
  last-syntax-ppss-point ;; a list (point point-min point-max)
  last-syntax-ppss-result
  ;; Value of `sp-pair-list' for this buffer.  Note that this might
  ;; differ from `sp-pair-list' which is often changed by dynamic
  ;; binding
  pair-list
  ;; Value of `sp-local-pairs' for this buffer.  Note that this might
  ;; differ from `sp-local-pairs' which is often changed by dynamic
  ;; binding
  local-pairs
  )

(defvar-local sp-state (make-sp-state)
  "Smartparens state for the current buffer.")

;; TODO: get rid of this
(defvar-local sp-previous-point -1
  "Location of point before last command.

This is only updated when some pair-overlay is active.  Do not
rely on the value of this variable anywhere else!")

;; TODO: get rid of this
(defvar-local sp-wrap-point nil
  "Save the value of point before attempt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")

;; TODO: get rid of this
(defvar-local sp-wrap-mark nil
  "Save the value of mark before attempt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")

(defvar-local sp-last-inserted-characters ""
  "Characters typed during the wrapping selection.

If wrapping is cancelled, these characters are re-inserted to the
location of point before the wrapping.")

(defvar-local sp-last-inserted-pair nil
  "Last inserted pair.")

(defvar-local sp-delayed-pair nil
  "The pair whose insertion is being delayed.

The insertion of this pair is delayed to be carried out in
`sp--post-command-hook-handler'.  The format is (opening delim
.  beg of the opening delim)")

(defvar-local sp-last-wrapped-region nil
  "Information about the last wrapped region.
The format is the same as returned by `sp-get-sexp'.")

(defvar sp-point-inside-string nil
  "Non-nil if point is inside a string.

Used to remember the state from before `self-insert-command' is
run.")

(defvar sp-buffer-modified-p nil
  "Non-nil if buffer was modified before `pre-command-hook'.")

(defvar sp-pre-command-point nil
  "Position of `point' before `this-command' gets executed.")

(defconst sp-max-pair-length 10
  "Maximum length of an opening or closing delimiter.

Only the pairs defined by `sp-pair' are considered.  Tag pairs
can be of any length.")

(defconst sp-max-prefix-length 100
  "Maximum length of a pair prefix.

Because prefixes for pairs can be specified using regular
expressions, they can potentially be of arbitrary length.  This
settings solves the problem where the parser would decide to
backtrack the entire buffer which would lock up Emacs.")

(defvar sp-pairs
  '((t
     .
     ((:open "\\\\(" :close "\\\\)" :actions (insert wrap autoskip navigate))
      (:open "\\{"   :close "\\}"   :actions (insert wrap autoskip navigate))
      (:open "\\("   :close "\\)"   :actions (insert wrap autoskip navigate))
      (:open "\\\""  :close "\\\""  :actions (insert wrap autoskip navigate))
      (:open "\""    :close "\""
       :actions (insert wrap autoskip navigate escape)
       :unless (sp-in-string-quotes-p)
       :post-handlers (sp-escape-wrapped-region sp-escape-quotes-after-insert))
      (:open "'"     :close "'"
       :actions (insert wrap autoskip navigate escape)
       :unless (sp-in-string-quotes-p sp-point-after-word-p)
       :post-handlers (sp-escape-wrapped-region sp-escape-quotes-after-insert))
      (:open "("     :close ")"     :actions (insert wrap autoskip navigate))
      (:open "["     :close "]"     :actions (insert wrap autoskip navigate))
      (:open "{"     :close "}"     :actions (insert wrap autoskip navigate))
      (:open "`"     :close "`"     :actions (insert wrap autoskip navigate)))))
  "List of pair definitions.

Maximum length of opening or closing pair is
`sp-max-pair-length' characters.")

(defvar sp-tags nil
  "List of tag definitions.  See `sp-local-tag' for more information.")

(defvar sp-prefix-tag-object nil
  "If non-nil, only consider tags while searching for next thing.")

(defvar sp-prefix-pair-object nil
  "If non-nil, only consider pairs while searching for next thing.

Pairs are defined as expressions delimited by pairs from
`sp-pair-list'.")

(defvar sp-prefix-symbol-object nil
  "If non-nil, only consider symbols while searching for next thing.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.")

(define-obsolete-variable-alias 'sp--lisp-modes 'sp-lisp-modes "2015-11-08")

(defcustom sp-lisp-modes '(
                           cider-repl-mode
                           clojure-mode
                           clojurec-mode
                           clojurescript-mode
                           clojurex-mode
                           common-lisp-mode
                           emacs-lisp-mode
                           eshell-mode
                           fennel-mode
                           fennel-repl-mode
                           geiser-repl-mode
                           gerbil-mode
                           inf-clojure-mode
                           inferior-emacs-lisp-mode
                           inferior-lisp-mode
                           inferior-scheme-mode
                           lisp-interaction-mode
                           lisp-mode
                           monroe-mode
                           racket-mode
                           racket-repl-mode
                           scheme-interaction-mode
                           scheme-mode
                           slime-repl-mode
                           sly-mrepl-mode
                           stumpwm-mode
                           )
  "List of Lisp-related modes."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-clojure-modes '(
                              cider-repl-mode
                              clojure-mode
                              clojurec-mode
                              clojurescript-mode
                              clojurex-mode
                              inf-clojure-mode
                              )
  "List of Clojure-related modes."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-c-modes '(
                        c-mode
                        c++-mode
                        )
  "List of C-related modes."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-no-reindent-after-kill-modes '(
                                             python-mode
                                             coffee-mode
                                             asm-mode
                                             makefile-gmake-mode
                                             haml-mode
                                             )
  "List of modes that should not reindent after kill."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-no-reindent-after-kill-indent-line-functions
  '(
    insert-tab
    )
  "List of `indent-line-function's that should not reindent after kill."
  :type '(repeat symbol)
  :group 'smartparens)

(defvar sp--html-modes '(
                         sgml-mode
                         html-mode
                         rhtml-mode
                         nxhtml-mode
                         nxml-mode
                         web-mode
                         jinja2-mode
                         html-erb-mode
                         js-jsx-mode
                         js2-jsx-mode
                         rjsx-mode
                         tsx-ts-mode
                         )
  "List of HTML modes.")

(defvar sp-message-alist
  '((:unmatched-expression
     "Search failed: there is an unmatched expression somewhere or we are at the beginning/end of file"
     "Unmatched expression")
    (:unbalanced-region
     "Can not kill the region: the buffer would end up in an unbalanced state after deleting the active region"
     "Killing the region would make the buffer unbalanced"
     "Unbalanced region")
    (:delimiter-in-string
     "Ignored: opening or closing pair is inside a string or comment and matching pair is outside (or vice versa)")
    (:no-matching-tag
     "Search failed: no matching tag found"
     "No matching tag")
    (:invalid-context-prev
     "Invalid context: previous h-sexp ends after the next one"
     "Invalid context")
    (:invalid-context-cur
     "Invalid context: current h-sexp starts after the next one"
     "Invalid context")
    (:no-structure-found
     "Previous sexp starts after current h-sexp or no structure was found"
     "No valid structure found")
    (:invalid-structure
     "Ignored: this operation would result in invalid structure"
     "Ignored because of invalid structure")
    (:cant-slurp
     "Ignored: we can not slurp without breaking strictly balanced expression"
     "Can not slurp without breaking balance")
    (:cant-slurp-context
     "Ignored: we can not slurp into different context (comment -> code)"
     "Can not slurp into different context")
    (:cant-insert-closing-delimiter
     "We can not insert unbalanced closing delimiter in strict mode"
     "Can not insert unbalanced delimiter")
    (:blank-sexp
     "Point is in blank sexp, nothing to barf"
     "Point is in blank sexp")
    (:point-not-deep-enough
     "Point has to be at least two levels deep to swap the enclosing delimiters"
     "Point has to be at least two levels deep"
     "Point not deep enough")
    (:different-type
     "The expressions to be joined are of different type"
     "Expressions are of different type"))
  "List of predefined messages to be displayed by `sp-message'.

Each element is a list consisting of a keyword and one or more
strings, which are chosen based on the `sp-message-width'
variable.  If the latter is t, the first string is chosen as
default, which should be the most verbose option available.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize & Mode definitions

(defgroup smartparens ()
  "Smartparens minor mode."
  :group 'editing
  :prefix "sp-")

;;;###autoload
(define-minor-mode smartparens-mode
  "Toggle smartparens mode.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}"
  :init-value nil
  :lighter (" SP" (:eval (if smartparens-strict-mode "/s" "")))
  :group 'smartparens
  :keymap smartparens-mode-map
  (if smartparens-mode
      (progn
        (sp--init)
        (add-hook 'self-insert-uses-region-functions 'sp-wrap--can-wrap-p nil 'local)
        ;; Unfortunately, some modes rebind "inserting" keys to their
        ;; own handlers but do not hand over the insertion back to
        ;; `self-insert-command', rather, they insert via `insert'.
        ;; Therefore, we need to call this handler in
        ;; `post-command-hook' too (inside
        ;; `sp--post-command-hook-handler').  The list
        ;; `sp--special-self-insert-commands' specifies which commands
        ;; to handle specially.
        (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler nil 'local)
        (add-hook 'pre-command-hook 'sp--save-pre-command-state nil 'local)
        (add-hook 'post-command-hook 'sp--post-command-hook-handler nil 'local)
        (run-hooks 'smartparens-enabled-hook))
    (remove-hook 'self-insert-uses-region-functions 'sp-wrap--can-wrap-p 'local)
    (remove-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler 'local)
    (remove-hook 'pre-command-hook 'sp--save-pre-command-state 'local)
    (remove-hook 'post-command-hook 'sp--post-command-hook-handler 'local)
    (run-hooks 'smartparens-disabled-hook)))

(defvar smartparens-strict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-char] 'sp-delete-char)
    (define-key map [remap delete-forward-char] 'sp-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'sp-backward-delete-char)
    (define-key map [remap backward-delete-char] 'sp-backward-delete-char)
    (define-key map [remap delete-backward-char] 'sp-backward-delete-char)
    (define-key map [remap kill-word] 'sp-kill-word)
    (define-key map [remap kill-line] 'sp-kill-hybrid-sexp)
    (define-key map [remap backward-kill-word] 'sp-backward-kill-word)
    (define-key map [remap kill-region] 'sp-kill-region)
    (define-key map [remap delete-region] 'sp-delete-region)
    (define-key map [remap kill-whole-line] 'sp-kill-whole-line)
    map)
  "Keymap used for `smartparens-strict-mode'.")

;;;###autoload
(define-minor-mode smartparens-strict-mode
  "Toggle the strict smartparens mode.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list."
  :init-value nil
  :group 'smartparens
  (if smartparens-strict-mode
      (progn
        (unless smartparens-mode
          (smartparens-mode 1))
        (unless (assq 'smartparens-strict-mode minor-mode-overriding-map-alist)
          (push `(smartparens-strict-mode . ,smartparens-strict-mode-map)
                minor-mode-overriding-map-alist))
        (put 'sp-backward-delete-char 'delete-selection 'sp--delete-selection-supersede-p)
        (put 'sp-delete-char 'delete-selection 'sp--delete-selection-supersede-p)
        (add-hook 'self-insert-uses-region-functions 'sp--self-insert-uses-region-strict-p nil 'local)
        (setq sp-autoskip-closing-pair 'always))
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'smartparens-strict-mode minor-mode-overriding-map-alist))
    (put 'sp-backward-delete-char 'delete-selection 'supersede)
    (put 'sp-delete-char 'delete-selection 'supersede)
    (remove-hook 'self-insert-uses-region-functions 'sp--self-insert-uses-region-strict-p 'local)
    (let ((std-val (car (plist-get (symbol-plist 'sp-autoskip-closing-pair) 'standard-value)))
          (saved-val (car (plist-get (symbol-plist 'sp-autoskip-closing-pair) 'saved-value))))
      (setq sp-autoskip-closing-pair (eval (or saved-val std-val))))))

;;;###autoload
(define-globalized-minor-mode smartparens-global-strict-mode
  smartparens-strict-mode
  turn-on-smartparens-strict-mode
  :group 'smartparens)

(defcustom sp-ignore-modes-list '(
                                  minibuffer-mode
                                  minibuffer-inactive-mode
                                  )
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'smartparens)

;;;###autoload
(defun turn-on-smartparens-strict-mode ()
  "Turn on `smartparens-strict-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (smartparens-strict-mode 1)))

;;;###autoload
(defun turn-off-smartparens-strict-mode ()
  "Turn off `smartparens-strict-mode'."
  (interactive)
  (smartparens-strict-mode -1))

(defun sp--init ()
  "Initialize the buffer local smartparens state.

 This includes pair bindings and other buffer local variables
that depend on the active `major-mode'."
  (setq sp-state (make-sp-state))
  ;; setup local pair replacements
  (sp--update-local-pairs)
  ;; set the escape char
  (dotimes (char 256)
    (unless sp-escape-char
      (when (= ?\\ (char-syntax char))
        (setq sp-escape-char (string char))))
    (unless sp-comment-char
      (when (= ?< (char-syntax char))
        (setq sp-comment-char (string char)))))
  ;; in case no escape syntax is found, just assume the backspace
  (unless sp-escape-char (setq sp-escape-char "\\")))

(defun sp--maybe-init ()
  "Initialize the buffer if it is not already initialized.

See `sp--init'."
  (unless sp-pair-list
    (sp--init)))

(defun sp--remove-local-pair (open)
  "Remove OPEN from `sp-local-pairs'."
  (setq sp-local-pairs
        (--remove (equal (plist-get it :open) open)
                  sp-local-pairs)))

(defun sp--update-sp-pair-list ()
  "Update `sp-pair-list' according to current value of `sp-local-pairs'."
  (setq sp-pair-list
        (->> sp-local-pairs
             (--map (cons (plist-get it :open) (plist-get it :close)))
             (-sort (lambda (x y) (> (length (car x)) (length (car y))))))))

(defun sp--update-local-pairs ()
  "Update local pairs after change or at mode initialization.

This command loads all the parent major mode definitions and
merges them into current buffer's `sp-local-pairs'."
  ;; Combine all the definitions from the most ancient parent to the
  ;; most recent parent
  (-each (nreverse
          (--unfold (when it
                      (cons it (get it 'derived-mode-parent)))
                    major-mode))
    #'sp-update-local-pairs))

(defun sp-update-local-pairs (configuration)
  "Update `sp-local-pairs' with CONFIGURATION.

The pairs are only updated in current buffer not in all buffers
with the same major mode!  If you want to update all buffers of
the specific major-modes use `sp-local-pair'.

CONFIGURATION can be a symbol to be looked up in `sp-pairs' or a
property list corresponding to the arguments of `sp-local-pair'
or a list of such property lists."
  (setq sp-local-pairs
        (cond
         ((symbolp configuration)
          (sp--merge-pair-configurations (cdr (assq configuration sp-pairs))))
         ((plist-member configuration :open)
          (sp--merge-pair-configurations (list configuration)))
         (t
          (sp--merge-pair-configurations configuration))))

  ;; Keep only those which have non-nil :actions
  (setq sp-local-pairs (--filter (plist-get it :actions) sp-local-pairs))

  ;; update the `sp-pair-list'.  This is a list only containing
  ;; (open.close) cons pairs for easier querying.  We also must order
  ;; it by length of opening delimiter in descending order (first
  ;; value is the longest)
  (sp--update-sp-pair-list)
  (setf (sp-state-local-pairs sp-state) sp-local-pairs)
  (setf (sp-state-pair-list sp-state) sp-pair-list))

(defmacro sp-with-buffers-using-mode (mode &rest body)
  "Execute BODY in every existing buffer using `major-mode' MODE."
  (declare (indent 1))
  `(--each (buffer-list)
     (with-current-buffer it
       (when (derived-mode-p ,mode)
         ,@body))))

(defun sp--update-local-pairs-everywhere (&rest modes)
  "Run `sp--update-local-pairs' in all buffers.

This is necessary to update all the buffer-local definitions.  If
MODES is non-nil, only update buffers with `major-mode' equal to
MODES."
  (setq modes (-flatten modes))
  (--each (buffer-list)
    (with-current-buffer it
      (when (and smartparens-mode
                 (or (not modes)
                     (--any? (derived-mode-p it) modes)))
        (sp--update-local-pairs)))))

(defcustom smartparens-enabled-hook nil
  "Called after `smartparens-mode' is turned on."
  :type 'hook
  :group 'smartparens)

(defcustom smartparens-disabled-hook nil
  "Called after `smartparens-mode' is turned off."
  :type 'hook
  :group 'smartparens)

;;;###autoload
(define-globalized-minor-mode smartparens-global-mode
  smartparens-mode
  turn-on-smartparens-mode)

;;;###autoload
(defun turn-on-smartparens-mode ()
  "Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode -1))

;; insert custom
(defcustom sp-autoinsert-pair t
  "If non-nil, autoinsert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

;; TODO: remove this in 1.12
(defcustom sp-autoinsert-quote-if-followed-by-closing-pair nil
  "If non-nil autoinsert quotes when the point is followed by closing delimiter.

This option only changes behaviour of the insertion process if
point is inside a string.  In other words, if string is not
closed and next character is a closing pair.

For example, in a situation like this:

  [\"some text|]

after pressing \", one would probably want to insert the closing
quote, not a nested pair (\\\"\\\"), to close the string literal
in the array.  To enable such behaviour, set this variable to
nil.

Note: the values of this varible seem to be backward, i.e. it is
\"enabled\" when the value is nil.  This was an unfortunate
choice of wording.  It is kept this way to preserve backward
compatibility.  The intended meaning is \"insert the pair if
followed by closing pair?\", t = yes."
  :type 'boolean
  :group 'smartparens)
(make-obsolete-variable
 'sp-autoinsert-quote-if-followed-by-closing-pair
 "the option was removed and no longer has any effect." "1.10")

(defcustom sp-autoskip-closing-pair 'always-end
  "Determine the behaviour when skipping closing delimiters.

If t, skip the following closing pair if the expression is
active (that is right after insertion).  This is controlled by
`sp-cancel-autoskip-on-backward-movement'.

If set to \"always-end\", skip the closing pair even if the
expression is not active and point is at the end of the
expression.  This only works for expressions with
single-character delimiters.

If set to \"always\", `sp-up-sexp' is called whenever the closing
delimiter is typed inside a sexp of the same type.  This is the
paredit-like behaviour.  This setting only works for
single-character delimiters and does not work for string-like
delimiters.

See `sp-autoskip-opening-pair' for similar setting for
string-like delimiters.

See also `sp-skip-closing-pair'."
  :type '(radio
          (const :tag "Never skip closing delimiter" nil)
          (const :tag "Skip closing delimiter in active expressions" t)
          (const :tag "Always skip closing delimiter if at the end of sexp" always-end)
          (const :tag "Always skip closing delimiter" always))
  :group 'smartparens)
(make-variable-buffer-local 'sp-autoskip-closing-pair)

(defcustom sp-autoskip-opening-pair nil
  "Determine the behaviour when skipping opening delimiters.

If non-nil, skip into the following string-like expression
instead of inserting a new pair."
  :type 'boolean
  :group 'smartparens)
(make-variable-buffer-local 'sp-autoskip-opening-pair)

;; TODO: rename to reflect what this actually does
(defcustom sp-cancel-autoskip-on-backward-movement t
  "If non-nil, deactivate the active expression on backward movement.

Note: the name of this variable is a historic coincidence and
will change in some future release to reflect its real purpose.

See also `sp-skip-closing-pair'."
  :type 'boolean
  :group 'smartparens)

;; delete custom
(defcustom sp-autodelete-pair t
  "If non-nil, auto delete pairs.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-closing-pair t
  "If non-nil, auto delete the whole closing-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-opening-pair t
  "If non-nil, auto delete the whole opening-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-undo-pairs-separately nil
  "If non-nil, put an `undo-boundary' before each inserted pair.

Calling undo after smartparens complete a pair will remove only
the pair before undoing any previous insertion.

WARNING: This option is implemented by hacking the
`buffer-undo-list'.  Turning this option on might have
irreversible consequences on the buffer's undo information and in
some cases might remove important information.  Usage of package
`undo-tree' is recommended if you ever need to revert to a state
unreachable by undo."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-successive-kill-preserve-whitespace 1
  "Control the behaviour of `sp-kill-sexp' on successive kills.

In the description, we consider more than one space
\"superfluous\", however, newlines are preserved."
  :type '(radio
          (const :tag "Always preserve the whitespace" 0)
          (const :tag "Remove superfluous whitespace after last kill" 1)
          (const :tag "Remove superfluous whitespace after all kills" 2))
  :group 'smartparens)

;; wrap custom
(defcustom sp-autowrap-region t
  "If non-nil, wrap the active region with pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-show-possible-pairs t
  "If non-nil, show possible pairs which can complete the wrapping."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-wrap t
  "If non-nil, autodelete opening and closing pair of most recent wrapping.

Deletion command must be the very first command after the
insertion, otherwise normal behaviour is applied."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-repeat-last 1
  "Context in which smartparens repeats the last wrap.

If the last operation was a wrap and we insert another pair at
the beginning or end of the last wrapped region, repeat the
wrap on this region with current pair."
  :type '(radio
          (const :tag "Do not repeat wrapping" 0)
          (const :tag "Only repeat if current tag is the same as the last one" 1)
          (const :tag "Always repeat if the point is after the opening/closing delimiter of last wrapped region" 2))
  :group 'smartparens)

(defcustom sp-wrap-entire-symbol nil
  "If non-nil, do NOT wrap the entire symbol, only the part after point.

If set to \"Enable globally\", smart symbol wrapping is active
everywhere.  This is the default option.

If set to \"Disable globally\", smart symbol wrapping is disabled
everywhere.

Otherwise, a list of major modes where smart symbol wrapping is
*disabled* can be supplied.

Examples:

 foo-ba|r-baz -> (|foo-bar-baz) ;; if enabled

 foo-ba|r-baz -> foo-ba(|r-baz) ;; if disabled"
  :type '(choice
          (const :tag "Enable globally" nil)
          (const :tag "Disable globally" globally)
          (repeat :tag "Disable in these major modes" symbol))
  :group 'smartparens)

(defcustom sp-wrap-from-point nil
  "If non-nil, do not wrap from the beginning of next expression but from point.

However, if the point is inside a symbol/word, the entire
symbol/word is wrapped.  To customize this behaviour, see
variable `sp-wrap-entire-symbol'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-respect-direction nil
  "When non-nil respect the wrap direction.

When non-nil, wrapping with opening pair always jumps to the
beginning of the region and wrapping with closing pair always
jumps to the end of the region.

  |fooM -> [ -> |[foo]M
  Mfoo| -> [ -> |[foo]M
  |fooM -> ] -> M[foo]|
  Mfoo| -> ] -> M[foo]|

When nil, closing pair places the point at the end of the region
and the opening pair leaves the point at its original
position (before or after the region).

  |fooM -> [ -> [|fooM]
  Mfoo| -> [ -> M[foo]|
  |fooM -> ] -> M[foo]|
  Mfoo| -> ] -> M[foo]|"
  :type 'boolean
  :group 'smartparens)

;; escaping custom
(defcustom sp-escape-wrapped-region t
  "If non-nil, escape special chars inside the just wrapped region."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-escape-quotes-after-insert t
  "If non-nil, escape string quotes if typed inside string."
  :type 'boolean
  :group 'smartparens)

;; navigation & manip custom
(defcustom sp-barf-move-point-with-delimiter t
  "If non-nil, move point when it would end outside the sexp after barf.

This way, a barf operation can be immediately followed by an
opposite slurp to undo it."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-consider-sgml-tags '(
                                            html-mode
                                            )
  "List of modes where sgml tags are considered to be sexps."
  :type '(repeat symbol)
  :group 'smartparens)


(defcustom sp-navigate-use-textmode-stringlike-parser '((derived . text-mode))
  "List of modes where textmode stringlike parser is used.

See `sp-get-textmode-stringlike-expression'.

Each element of the list can either be a symbol which is then
checked against `major-mode', or a cons (derived . PARENT-MODE),
where PARENT-MODE is checked using `derived-mode-p'."
  :type '(repeat (choice
                  (symbol :tag "Major mode")
                  (cons :tag "Derived mode"
                    (const derived)
                    (symbol :tag "Parent major mode name"))))
  :group 'smartparens)

(defcustom sp-navigate-consider-symbols t
  "If non-nil, consider symbols outside balanced expressions as such.

Symbols are recognized by function `sp-forward-symbol'.  This
setting affect all the navigation and manipulation functions
where it make sense.

Also, special handling of strings is enabled, where the whole
string delimited with \"\" is considered as one token.

WARNING: This is a legacy setting and changing its value to NIL
may break many things.  It is kept only for backward
compatibility and will be removed in the next major release."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-comments-as-sexps t
  "If non-nil, consider comments as sexps in `sp-get-enclosing-sexp'.

If this option is enabled, unbalanced expressions in comments are
never automatically closed (see `sp-navigate-close-if-unbalanced')."
  :type 'boolean
  :group 'smartparens)

;; TODO: add -alist suffix
(defcustom sp-navigate-skip-match `(
                                    (,sp-lisp-modes . sp--elisp-skip-match)
                                    )
  "Major-mode dependent specifications of skip functions.

Alist where the key is a list of major-modes and the value is a
function used to skip over matches in `sp-get-paired-expression'.
This function takes three arguments: the currently matched
delimiter, beginning of match and end of match.  If this function
returns true, the current match will be skipped.

You can use this to skip over expressions that serve multiple
functions, such as if/end pair or unary if in Ruby or * in
markdown when it signifies list item instead of emphasis.  If the
exception is only relevant to one pair, you should rather
use :skip-match option in `sp-local-pair'."
  :type '(alist
          :key-type (repeat symbol)
          :value-type symbol)
  :group 'smartparens)

(defcustom sp-navigate-reindent-after-up `(
                                           (interactive
                                            ,@sp-lisp-modes
                                            )
                                           )
  "Modes where sexps should be reindented after `sp-up-sexp'.

The whitespace between the closing delimiter and last \"thing\"
inside the expression is removed.  It works analogically for the
`sp-backward-up-sexp'.

Note that this also happens when `sp-skip-closing-pair' is
invoked (usually in strict mode when the closing delimiter is
typed) as it calls `sp-up-sexp' internally.  This behaviour can
be customized by various settings of `sp-autoskip-closing-pair'
and `sp-autoskip-opening-pair'.

If the mode is in the list \"interactive\", only reindent the sexp
if the command was called interactively.  This is recommended for
general use.

If the mode is in the list \"always\", reindend the sexp even if the
command was called programatically."
  :type '(alist
          :options (interactive always)
          :value-type (repeat symbol))
  :group 'smartparens)

(defcustom sp-navigate-reindent-after-up-in-string t
  "If non-nil, `sp-up-sexp' will reindent inside strings.

If `sp-navigate-reindent-after-up' is enabled and the point is
inside a string, this setting determines if smartparens should
reindent the current (string) sexp or not."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-close-if-unbalanced nil
  "If non-nil, insert the closing pair of the un-matched pair on `sp-up-sexp'.

The closing delimiter is inserted after the symbol at
point (using `sp-previous-sexp')."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-interactive-always-progress-point nil
  "Make point always move in the direction of navigation.

If non-nil and the function is called interactively,
`sp-next-sexp' and `sp-previous-sexp' will always move the point
to the end/beg of such an expression where the point would end up
being further in the direction of travel.

Note: this behaviour will become default in release 2.0 and will
cease to be configurable."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-sexp-prefix nil
  "Alist of `major-mode' specific prefix specification.

Each item is a list with three properties:
- major mode
- a constant symbol \\='regexp or \\='syntax
- a regexp or a string containing syntax class codes.

If the second argument is \\='regexp, the third argument is
interpreted as a regexp to search backward from the start of an
expression.

If the second argument is \\='syntax, the third argument is
interpreted as string containing syntax codes that will be
skipped.

You can also override this property locally for a specific pair
by specifying its :prefix property."
  :type '(repeat
          (list symbol
                (choice
                 (const :tag "Regexp" regexp)
                 (const :tag "Syntax class codes" syntax))
                string))
  :group 'smartparens)

(defcustom sp-sexp-suffix nil
  "Alist of `major-mode' specific suffix specification.

Each item is a list with three properties:
- major mode
- a constant symbol \\='regexp or \\='syntax
- a regexp or a string containing syntax class codes.

If the second argument is \\='regexp, the third argument is
interpreted as a regexp to search forward from the end of an
expression.

If the second argument is \\='syntax, the third argument is
interpreted as string containing syntax codes that will be
skipped.

You can also override this property locally for a specific pair
by specifying its :suffix property."
  :type '(repeat
          (list symbol
                (choice
                 (const :tag "Regexp" regexp)
                 (const :tag "Syntax class codes" syntax))
                string))
  :group 'smartparens)

(defcustom sp-split-sexp-always-split-as-string t
  "Determine if sexp inside string is split.

If the point is inside a sexp inside a string, the default
behaviour is now to split the string, such that:

  \"foo (|) bar\"

becomes

   \"foo (\"|\") bar\"

instead of

   \"foo ()|() bar\".

Note: the old default behaviour was the reverse, it would split
the sexp, but this is hardly ever what you want.

You can add a post-handler on string pair and check for
\\='split-string action to add concatenation operators of the
language you work in (in each `major-mode' you can have a separate
hook).

For example, in PHP the string concatenation operator is a
dot (.), so you would add:

  (defun my-php-post-split-handler (_ action _)
    (when (eq action \\='split-sexp)
      (just-one-space)
      (insert \".  . \")
      (backward-char 3)))

  (sp-local-pair \\='php-mode \"\\='\" nil
   :post-handlers \\='(my-php-post-split-handler))

Then

  echo \\='foo |baz\\=';

results in

  echo \\='foo\\=' . | . \\='baz\\=';"
  :type 'boolean
  :group 'smartparens)

;; hybrid lines
(defcustom sp-hybrid-kill-excessive-whitespace nil
  "Determine how `sp-kill-hybrid-sexp' kills excessive whitespace.

If non-nil, `sp-kill-hybrid-sexp' will delete all whitespace
up until next hybrid sexp if the point is at the end of line or
on a blank line.

When it is set to \\='kill, whitespace will be appended to the sexp
in kill ring."
  :type '(choice
          (const :tag "Delete" t)
          (const :tag "Kill" kill)
          (const :tag "Off" nil))
  :group 'smartparens)

(defcustom sp-hybrid-kill-entire-symbol nil
  "Governs how symbols under point are treated by `sp-kill-hybrid-sexp'.

If t, always kill the symbol under point.

If nil, never kill the entire symbol and only kill the part after point.

If a function, this should be a zero-arg predicate.  When it
returns non-nil value, we should kill from point."
  :type '(radio
          (const :tag "Always kill entire symbol" t)
          (const :tag "Always kill from point" nil)
          (const :tag "Kill from point only inside strings" sp-point-in-string)
          (function :tag "Custom predicate"))
  :group 'smartparens)

(defcustom sp-comment-string nil
  "String that is inserted after calling `sp-comment'.

It is an alist of list of major modes to a string.

The value of `comment-start' is used if the major mode is not found."
  :type '(alist
          :key-type (repeat symbol)
          :value-type string)
  :group 'smartparens)

;; ui custom
(defcustom sp-highlight-pair-overlay t
  "If non-nil, autoinserted pairs are highlighted while point is inside the pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-overlay t
  "If non-nil, wrap overlays are highlighted during editing of the wrapping pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-tag-overlay t
  "If non-nil, wrap tag overlays are highlighted during editing
of the wrapping tag pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-echo-match-when-invisible t
  "If non-nil, show-smartparens-mode prints the line of the
matching paren in the echo area if not visible on screen."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-message-width 'frame
  "Length of information and error messages to display.

If set to \\='frame (the default), messages are chosen based of the
frame width.  t means chose the default (verbose) message, nil
means mute.  Integers specify the maximum width."
  :type '(choice (const :tag "Fit to frame" frame)
                 (const :tag "Verbose" t)
                 (const :tag "Mute" nil)
                 (integer :tag "Max width"))
  :group 'smartparens)

;; TODO: this should be true by default > then the behaviour is
;; controlled by subword-mode... and this is a hard override
(defcustom sp-use-subword nil
  "Override of `subword-mode' killing behaviour.

If non-nill, `sp-kill-word' and `sp-backward-kill-word' only
kill \"subwords\" when `subword-mode' is active."
  :type 'boolean
  :group 'smartparens)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection mode handling

(defun sp--delete-selection-p ()
  "Return t if `delete-selection-mode' or `cua-delete-selection' is enabled."
  (or (and (boundp 'delete-selection-mode) delete-selection-mode)
      (and (boundp 'cua-delete-selection) cua-delete-selection cua-mode)))

(defun sp--delete-selection-supersede-p ()
  "Decide if the current command should delete the region or not.

This check is used as value of \\='delete-selection property on the
command symbol."
  (if (or (equal current-prefix-arg '(4))
          (sp-region-ok-p (region-beginning) (region-end)))
      'supersede
    (sp-message :unbalanced-region)
    ;; Since this check runs in the pre-command-hook we can change the
    ;; command to be executed... in this case we set it to ignore
    ;; because we don't want to do anything.
    (setq this-command 'ignore)
    nil))

(defun sp--self-insert-uses-region-strict-p ()
  "Decide if the current `self-insert-command' should be able to
replace the region.

This check is added to the special hook
`self-insert-uses-region-functions' which is checked by
`delete-selection-uses-region-p'."
  (if (or (equal current-prefix-arg '(4))
          (sp-region-ok-p (region-beginning) (region-end)))
      ;; region is OK or we are allowed to replace it, just say nil so
      ;; that delsel handles this
      nil
    ;; in case region is bad we interrupt the insertion
    (setq this-command 'ignore)
    t))

;; TODO: this function was removed from Emacs, we should get rid of
;; the advice in time.
(defadvice cua-replace-region (around fix-sp-wrap activate)
  "Fix `sp-wrap' in `cua-selection-mode'."
  (if (and smartparens-mode (sp-wrap--can-wrap-p))
      (cua--fallback)
    ad-do-it))

(defadvice cua-delete-region (around fix-sp-delete-region activate)
  "If `smartparens-strict-mode' is enabled, perform a region
check before deleting."
  (if (and smartparens-mode smartparens-strict-mode)
      (progn
        (unless (or current-prefix-arg
                    (sp-region-ok-p (region-beginning) (region-end)))
          (user-error (sp-message :unbalanced-region :return)))
        ad-do-it)
    ad-do-it))



(cl-eval-when (compile eval load)
  (defun sp--get-substitute (struct list)
    "Only ever call this from sp-get!  This function does the
replacement of all the keywords with actual calls to sp-get."
    (if (listp list)
        (if (eq (car list) 'sp-get)
            list
          (mapcar (lambda (x) (sp--get-substitute struct x))
                  (let ((command (car list)))
                    (cond
                     ((eq command 'sp-do-move-op)
                      (let ((argument (make-symbol "--sp-argument--")))
                        `(let ((,argument ,(cadr list)))
                           (if (< ,argument :beg-prf)
                               (progn
                                 (goto-char :beg-prf)
                                 (delete-char (+ :op-l :prefix-l))
                                 (goto-char ,argument)
                                 (insert :prefix :op))
                             (goto-char ,argument)
                             (insert :prefix :op)
                             (goto-char :beg-prf)
                             (delete-char (+ :op-l :prefix-l))))))
                     ((eq command 'sp-do-move-cl)
                      (let ((argument (make-symbol "--sp-argument--")))
                        `(let ((,argument ,(cadr list)))
                           (if (> ,argument :end-in)
                               (progn
                                 (goto-char ,argument)
                                 (insert :cl :suffix)
                                 (goto-char :end-in)
                                 (delete-char (+ :cl-l :suffix-l)))
                             (goto-char :end-in)
                             (delete-char (+ :cl-l :suffix-l))
                             (goto-char ,argument)
                             (insert :cl :suffix)))))
                     ((eq command 'sp-do-del-op)
                      `(progn
                         (goto-char :beg-prf)
                         (delete-char (+ :op-l :prefix-l))))
                     ((eq command 'sp-do-del-cl)
                      `(progn
                         (goto-char :end-in)
                         (delete-char (+ :cl-l :suffix-l))))
                     ((eq command 'sp-do-put-op)
                      `(progn
                         (goto-char ,(cadr list))
                         (insert :prefix :op)))
                     ((eq command 'sp-do-put-cl)
                      `(progn
                         (goto-char ,(cadr list))
                         (insert :cl :suffix)))
                     (t list)))))
      (if (keywordp list)
          (sp--get-replace-keyword struct list)
        list)))

  (defun sp--get-replace-keyword (struct keyword)
    (cl-case keyword
      ;; point in buffer before the opening delimiter
      (:beg         `(plist-get ,struct :beg))
      ;; point in the buffer after the closing delimiter
      (:end         `(plist-get ,struct :end))
      ;; point in buffer after the opening delimiter
      (:beg-in      `(+ (plist-get ,struct :beg) (length (plist-get ,struct :op))))
      ;; point in buffer before the closing delimiter
      (:end-in      `(- (plist-get ,struct :end) (length (plist-get ,struct :cl))))
      ;; point in buffer before the prefix of this expression
      (:beg-prf     `(- (plist-get ,struct :beg) (length (plist-get ,struct :prefix))))
      ;; point in the buffer after the suffix of this expression
      (:end-suf     `(+ (plist-get ,struct :end) (length (plist-get ,struct :suffix))))
      ;; opening delimiter
      (:op          `(plist-get ,struct :op))
      ;; closing delimiter
      (:cl          `(plist-get ,struct :cl))
      ;; length of the opening pair
      (:op-l        `(length (plist-get ,struct :op)))
      ;; length of the closing pair
      (:cl-l        `(length (plist-get ,struct :cl)))
      ;; length of the entire expression, including enclosing
      ;; delimiters and the prefix and suffix
      (:len         `(- (plist-get ,struct :end)
                        (plist-get ,struct :beg)
                        (- (length (plist-get ,struct :prefix)))
                        (- (length (plist-get ,struct :suffix)))))
      ;; length of the the pair ignoring the prefix, including delimiters
      (:len-out     `(- (plist-get ,struct :end) (plist-get ,struct :beg)))
      ;; length of the pair inside the delimiters
      (:len-in      `(- (plist-get ,struct :end)
                        (plist-get ,struct :beg)
                        (length (plist-get ,struct :op))
                        (length (plist-get ,struct :cl))))
      ;; expression prefix
      (:prefix      `(plist-get ,struct :prefix))
      ;; expression prefix length
      (:prefix-l    `(length (plist-get ,struct :prefix)))
      (:suffix      `(plist-get ,struct :suffix))
      (:suffix-l    `(length (plist-get ,struct :suffix)))
      ;; combined op/cl and suffix/prefix
      (:opp         `(concat (plist-get ,struct :prefix)
                             (plist-get ,struct :op)))
      (:opp-l       `(+ (length (plist-get ,struct :prefix))
                        (length (plist-get ,struct :op))))
      (:cls         `(concat (plist-get ,struct :cl)
                             (plist-get ,struct :suffix)))
      (:cls-l       `(+ (length (plist-get ,struct :cl))
                        (length (plist-get ,struct :suffix))))
      (t keyword))))


;; TODO: rewrite this in terms of `symbol-macrolet' ??
(defmacro sp-get (struct &rest forms)
  "Get a property from a structure.

STRUCT is a plist with the format as returned by `sp-get-sexp'.
Which means this macro also works with `sp-get-symbol',
`sp-get-string' and `sp-get-thing'.

FORMS is an attribute we want to query.  Currently supported
attributes are:

:beg       - point in buffer before the opening delimiter
:end       - point in the buffer after the closing delimiter
:beg-in    - point in buffer after the opening delimiter
:end-in    - point in buffer before the closing delimiter
:beg-prf   - point in buffer before the prefix of this expression
:end-suf   - point in buffer after the suffix of this expression
:op        - opening delimiter
:cl        - closing delimiter
:op-l      - length of the opening pair
:cl-l      - length of the closing pair
:len       - length of the entire expression, including enclosing
             delimiters, the prefix and the suffix
:len-out   - length of the the pair ignoring the prefix and suffix,
             including delimiters
:len-in    - length of the pair inside the delimiters
:prefix    - expression prefix
:prefix-l  - expression prefix length
:suffix    - expression suffix
:suffix-l  - expression suffix length

These special \"functions\" are expanded to do the selected
action in the context of currently queried pair:

Nullary:
\(sp-do-del-op) - remove prefix and opening delimiter
\(sp-do-del-cl) - remove closing delimiter and suffix

Unary:
\(sp-do-move-op p) - move prefix and opening delimiter to point p
\(sp-do-move-cl p) - move closing delimiter and suffix to point p
\(sp-do-put-op p) - put prefix and opening delimiter at point p
\(sp-do-put-cl p) - put closing delimiter and suffix at point p

In addition to these simple queries and commands, this macro
understands arbitrary forms where any of the aforementioned
attributes are used.  Therefore, you can for example query for
\"(+ :op-l :cl-l)\".  This query would return the sum of lengths
of opening and closing delimiter.  A query
\"(concat :prefix :op)\" would return the string containing
expression prefix and the opening delimiter.

Special care is taken to only evaluate the STRUCT argument once."
  (declare (indent 1)
           (debug (form body)))
  (let ((st (make-symbol "struct")))
    (sp--get-substitute st `(let ((,st ,struct)) ,@forms))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc/Utility functions

(defun sp--indent-region (start end &optional column)
  "Call `indent-region' unless `aggressive-indent-mode' is enabled.

START, END and COLUMN are the same as in `indent-region'."
  (unless (bound-and-true-p aggressive-indent-mode)
    ;; Don't issue "Indenting region..." message.
    (cl-letf (((symbol-function 'message) #'ignore))
      (indent-region start end column))))

(defmacro sp-with-modes (arg &rest forms)
  "Add ARG as first argument to each form in FORMS.

This can be used with `sp-local-pair' calls to automatically
insert the modes."
  (declare (indent 1)
           (debug (form body)))
  (let ((modes (make-symbol "modes")))
    `(let ((,modes ,arg))
       (progn
         ,@(mapcar (lambda (form) (append (list (car form) modes) (cdr form))) forms)))))

(defmacro sp--with-case-sensitive (&rest body)
  "Ensure that searching done within BODY is case-sensitive.

Bind `case-fold-search' to nil if it is not already and avoid the
bind if it is already.  Any function that needs to use any of the
sp--looking-* functions more than once should wrap them all in
`sp--with-case-sensitive'."
  (declare (indent 0)
           (debug (body)))
  `(if case-fold-search
       (let ((case-fold-search nil))
         ,@body)
     ,@body))

(defun sp--evil-normal-state-p ()
  "Check to see if the current `evil-state' is in normal mode."
  (and (fboundp 'evil-normal-state-p) (evil-normal-state-p)))

(defun sp--evil-motion-state-p ()
  "Check to see if the current `evil-state' is in motion mode."
  (and (fboundp 'evil-motion-state-p) (evil-motion-state-p)))

(defun sp--evil-visual-state-p ()
  "Check to see if the current `evil-state' is in visual mode."
  (and (fboundp 'evil-visual-state-p) (evil-visual-state-p)))

(defun sp-point-in-blank-line (&optional p)
  "Return non-nil if line at point is blank (whitespace only).

If optional argument P is present test this instead of point."
  (save-excursion
    (when p (goto-char p))
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun sp-point-in-blank-sexp (&optional p)
  "Return non-nil if point is inside blank (whitespace only) sexp.

If optional argument P is present test this instead of point.

Warning: it is only safe to call this when point is inside a
sexp, otherwise the call may be very slow."
  (save-excursion
    (when p (goto-char p))
    (-when-let (enc (sp-get-enclosing-sexp))
      (sp-get enc (string-match-p
                   "\\`[ \t\n]*\\'"
                   (buffer-substring-no-properties :beg-in :end-in))))))

(defun sp-char-is-escaped-p (&optional point)
  "Test if the char at POINT is escaped or not.

POINT defaults to `point'."
  (setq point (or point (point)))
  (save-match-data
    (when (save-excursion
            (goto-char point)
            (looking-back (concat sp-escape-char sp-escape-char "+") nil t))
      (eq (logand (length (match-string 0)) 1) 1))))

(defun sp--syntax-ppss (&optional p)
  "Memoize the last result of `syntax-ppss'.

P is the point at which we run `syntax-ppss'"
  (let ((p (or p (point)))
        (mem-p (sp-state-last-syntax-ppss-point sp-state)))
    (if (and (eq p (nth 0 mem-p))
             (eq (point-min) (nth 1 mem-p))
             (eq (point-max) (nth 2 mem-p)))
        (sp-state-last-syntax-ppss-result sp-state)
      ;; Add hook to reset memoization if necessary
      (unless (sp-state-last-syntax-ppss-point sp-state)
        (add-hook 'before-change-functions 'sp--reset-memoization t t))
      (setf (sp-state-last-syntax-ppss-point sp-state)
            (list p (point-min) (point-max))
            (sp-state-last-syntax-ppss-result sp-state) (syntax-ppss p)))))

(defun sp-point-in-string (&optional p)
  "Return non-nil if point is inside string or documentation string.

This function actually returns the 3rd element of `syntax-ppss'
which can be a number if the string is delimited by that
character or t if the string is delimited by general string
fences.

If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (sp--syntax-ppss p)))))

(defun sp-point-in-comment (&optional p)
  "Return non-nil if point is inside comment.

If optional argument P is present test this instead off point."
  (setq p (or p (point)))
  (ignore-errors
    (save-excursion
      ;; We cannot be in a comment if we are inside a string
      (unless (nth 3 (sp--syntax-ppss p))
        (or (nth 4 (sp--syntax-ppss p))
            ;; this also test opening and closing comment delimiters... we
            ;; need to chack that it is not newline, which is in "comment
            ;; ender" class in elisp-mode, but we just want it to be
            ;; treated as whitespace
            (and (< p (point-max))
                 (memq (char-syntax (char-after p)) '(?< ?>))
                 (not (eq (char-after p) ?\n)))
            ;; we also need to test the special syntax flag for comment
            ;; starters and enders, because `syntax-ppss' does not yet
            ;; know if we are inside a comment or not (e.g. / can be a
            ;; division or comment starter...).
            (-when-let (s (car (syntax-after p)))
              (or (and (/= 0 (logand (lsh 1 16) s))
                       (nth 4 (syntax-ppss (+ p 2))))
                  (and (/= 0 (logand (lsh 1 17) s))
                       (nth 4 (syntax-ppss (+ p 1))))
                  (and (/= 0 (logand (lsh 1 18) s))
                       (nth 4 (syntax-ppss (- p 1))))
                  (and (/= 0 (logand (lsh 1 19) s))
                       (nth 4 (syntax-ppss (- p 2)))))))))))

(defun sp-point-in-string-or-comment (&optional p)
  "Return non-nil if point is inside string, documentation string or a comment.

If optional argument P is present, test this instead of point."
  (or (sp-point-in-string p)
      (sp-point-in-comment p)))

;; TODO: add -p suffix
(defun sp-point-in-symbol (&optional p)
  "Return non-nil if `point' is inside symbol.

If P is non-nil, interpret it as buffer position and test there.

Point is inside symbol if characters on both sides of the point
are in either word or symbol class."
  (setq p (or p (point)))
  (save-excursion
    (goto-char p)
    (and (/= 0 (following-char))
         (memq (char-syntax (following-char)) '(?w ?_))
         (memq (char-syntax (preceding-char)) '(?w ?_)))))

(defun sp--single-key-description (event)
  "Return a description of the last EVENT.

Replace all the function key symbols with garbage character (ň).

TODO: fix this!"
  (let ((original (single-key-description event)))
    (cond
     ((string-match-p "<.*?>" original) "ň")
     ((string-match-p "SPC" original) " ")
     (t original))))

;; see https://github.com/Fuco1/smartparens/issues/125#issuecomment-20356176
(defun sp--current-indentation ()
  "Get the indentation offset of the current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun sp--calculate-indentation-offset (old-column old-indentation)
  "Calculate correct indentation after re-indent.

OLD-COLUMN is the column before reindent.

OLD-INDENTATION is the indentation depth before reindent."
  (let ((indentation (sp--current-indentation)))
    (cond
     ;; Point was in code, so move it along with the re-indented code
     ((>= old-column old-indentation)
      (+ old-column (- indentation old-indentation)))
     ;; Point was indentation, but would be in code now, so move to
     ;; the beginning of indentation
     ((<= indentation old-column) indentation)
     ;; Point was in indentation, and still is, so leave it there
     (:else old-column))))

(defun sp--back-to-indentation (old-column old-indentation)
  "Set the current column to proper value.

See `sp--keep-indentation'.

OLD-COLUMN is the column before reindent.

OLD-INDENTATION is the indentation depth before reindent."
  (let ((offset (sp--calculate-indentation-offset old-column old-indentation)))
    (move-to-column offset)))

;; TODO: rename to preserve-current-column
(defmacro sp--keep-indentation (&rest body)
  "Execute BODY and restore the column.

If point was in code move it along if the line is reinvented so
it is the same distance relative to first code column.

If point was previously in the indentation region but would end
up in code, move it to the first code column.

If point was in the indentation region and is still there after
BODY, do nothing."
  (declare (indent 0)
           (debug (body)))
  (let ((c (make-symbol "c"))
        (i (make-symbol "i")))
    `(let ((,c (current-column))
           (,i (sp--current-indentation)))
       ,@body
       (sp--back-to-indentation ,c ,i))))

;; Please contribute these if you come across some!
(defvar sp--self-insert-commands
  '(self-insert-command
    org-self-insert-command
    LaTeX-insert-left-brace)
   "List of commands that are some sort of `self-insert-command'.

Many modes rebind \"self-inserting\" keys to \"smart\" versions
which do some additional processing before delegating the
insertion to `self-insert-command'.  Smartparens needs to be able
to distinguish these to properly handle insertion and reinsertion
of pairs and wraps.")

;; Please contribute these if you come across some!
(defvar sp--special-self-insert-commands
  '(
    TeX-insert-dollar
    TeX-insert-quote
    quack-insert-opening-paren
    quack-insert-closing-paren
    quack-insert-opening-bracket
    quack-insert-closing-bracket
    racket-insert-closing-paren
    racket-insert-closing-bracket
    racket-insert-closing-brace
    )
   "List of commands which are handled as if they were `self-insert-command's.

Some modes redefine \"self-inserting\" keys to \"smart\" versions
which do some additional processing but do _not_ delegate the
insertion to `self-insert-command', instead inserting via
`insert'.  Smartparens needs to be able to distinguish these to
properly handle insertion and reinsertion of pairs and wraps.

The `sp--post-self-insert-hook-handler' is called in the
`post-command-hook' for these commands.")

(defun sp--self-insert-command-p ()
  "Return non-nil if `this-command' is some sort of `self-insert-command'."
  (memq this-command sp--self-insert-commands))

(defun sp--special-self-insert-command-p ()
  "Return non-nil if `this-command' is \"special\" self insert command.

A special self insert command is one that inserts a character but
does not trigger `post-self-insert-hook'."
  (memq this-command sp--special-self-insert-commands))

(defun sp--signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

;; The structure returned by sp-get-sexp is a plist with following properties:
;;
;; :beg    - point in the buffer before the opening delimiter (ignoring prefix)
;; :end    - point in the buffer after the closing delimiter
;; :op     - opening delimiter
;; :cl     - closing delimiter
;; :prefix - expression prefix
;;
;; This structure should never be accessed directly and should only be
;; exposed by the sp-get macro.  This way, we can later change the
;; internal representation without much trouble.

(defmacro sp-compare-sexps (a b &optional fun what-a what-b)
  "Return non-nil if the expressions A and B are equal.

Two expressions are equal if their :beg property is the same.

If optional argument FUN is non-nil, it is the comparison
function.

If optional argument WHAT-A is non-nil, use it as a keyword on
which to do the comparsion (default to :beg).

If optional argument WHAT-B is non-nil, use it as a keyword on
which to do the comparsion (default to WHAT-A)."
  (declare (debug (form form &optional functionp keywordp keywordp)))
  (setq fun (or fun 'equal))
  (setq what-a (or what-a :beg))
  (setq what-b (or what-b what-a))
  `(,fun (sp-get ,a ,what-a) (sp-get ,b ,what-b)))

(defun sp-message (key &optional return)
  "Display a message.

KEY is either a string or list of strings, or a keyword,
in which case the string list is looked up in
`sp-message-alist'.  The string to be displayed is chosen based on
the `sp-message-width' variable.

If RETURN is non-nil return the string instead of printing it."
  (let ((msgs (cond ((listp key) key)
                    ((stringp key) (list key))
                    (t (cdr (assq key sp-message-alist))))))
    (when (and msgs sp-message-width)
      (if (eq sp-message-width t)
          (if return (car msgs) (message "%s." (car msgs)))
        (let ((maxlen (if (eq sp-message-width 'frame)
                          (frame-width)
                        sp-message-width))
              (s nil))
          (dolist (msg msgs)
            (if (and (<= (length msg) maxlen)
                     (> (length msg) (length s)))
                (setf s msg)))
          (when s
            (if return s (message "%s." s))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding/removing of pairs/bans/allows etc.

(defun sp--merge-prop (prop new-pair old-pair)
  "Merge a property PROP from NEW-PAIR into OLD-PAIR.

The list OLD-PAIR must not be nil."
  (let ((new-val (plist-get new-pair prop)))
    (cl-case prop
      (:close (plist-put old-pair :close new-val))
      (:prefix (plist-put old-pair :prefix new-val))
      (:suffix (plist-put old-pair :suffix new-val))
      (:skip-match (plist-put old-pair :skip-match new-val))
      (:trigger (plist-put old-pair :trigger new-val))
      (:trigger-wrap (plist-put old-pair :trigger-wrap new-val))
      ((:actions :when :unless :pre-handlers :post-handlers)
       (cl-case (car new-val)
         (:add (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr new-val))))
         (:rem (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr new-val))))
         (t
          (cond
           ;; this means we have ((:add ...) (:rem ...)) argument
           ((and new-val
                 (listp (car new-val))
                 (memq (caar new-val) '(:add :rem)))
            (let ((a (assq :add new-val))
                  (r (assq :rem new-val)))
              (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr a)))
              (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr r)))))
           (t
            (plist-put old-pair prop (plist-get new-pair prop))))))))))

(defun sp--merge-pairs (old-pair new-pair)
  "Merge OLD-PAIR and NEW-PAIR.
This modifies the OLD-PAIR by side effect."
  (--each new-pair
    ;; In plists, items at even positions are keys
    (when (= 0 (% it-index 2))
      (sp--merge-prop it new-pair old-pair)))
  old-pair)

(defun sp--update-pair (new-pair old-pair)
  "Copy properties from NEW-PAIR to OLD-PAIR.

The list OLD-PAIR must not be nil."
  (--each new-pair
    ;; In plists, items at even positions are keys
    (when (= 0 (% it-index 2))
      (when (or (not (plist-get old-pair it))
                ;; HACK: we don't want to overwrite list properties
                ;; that aren't just :add with :add because this
                ;; would break the "idempotency".
                (not (equal '(:add) (plist-get new-pair it))))
        (plist-put old-pair it (plist-get new-pair it)))))
  old-pair)

(defun sp--update-pair-list (pair mode)
  "Update the PAIR for major mode MODE.

If this pair is not defined yet for this major mode, add it.  If
this pair is already defined, replace all the properties in the
old definition with values from PAIR."
  ;; get the structure relevant to mode.  t means global setting
  (let ((struct (--first (eq mode (car it)) sp-pairs)))
    (if (not struct)
        (!cons (cons mode (list pair)) sp-pairs)
      ;; this does NOT merge changes, only replace the values at
      ;; properties.  Open delimiter works as ID as usual.
      (let ((old-pair (--first (equal (plist-get pair :open)
                                      (plist-get it :open))
                               (cdr struct)))
            (open (plist-get pair :open)))
        (if (not old-pair)
            (progn
              (unless (or (plist-get pair :close)
                          (sp--get-pair open (assq t sp-pairs)))
                (error "Pair %s was never defined, please specify closing delimiter in instead of passing `nil'" open))
              (setcdr struct (cons pair (cdr struct))))
          (sp--update-pair pair old-pair)))))
  sp-pairs)

(defun sp--get-pair (open list)
  "Get the pair with id OPEN from list LIST."
  (--first (equal open (plist-get it :open)) list))

(defun sp--get-pair-definition (open list &optional prop)
  "Get the definition of a pair identified by OPEN from list LIST.

If PROP is non-nil, return the value of that property instead."
  (let ((pair (sp--get-pair open list)))
    (cl-case prop
      ((nil) pair)
      (:op-l (length (plist-get pair :open)))
      (:cl-l (length (plist-get pair :close)))
      (:len (+ (length (plist-get pair :open))
               (length (plist-get pair :close))))
      (:post-handlers
       (-remove #'listp (plist-get pair :post-handlers)))
      (:post-handlers-cond
       (-filter #'listp (plist-get pair :post-handlers)))
      (:when
       (-remove #'listp (plist-get pair :when)))
      (:when-cond
       (-flatten (-concat (-filter #'listp (plist-get pair :when)))))
      (t (plist-get pair prop)))))

(defun sp-get-pair-definition (open mode &optional prop)
  "Get the definition of pair identified by OPEN.

OPEN is the opening delimiter, MODE is the major mode symbol or t
for global definition.

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open (cdr (assq mode sp-pairs)) prop))

(defun sp-get-pair (open &optional prop)
  "Return the definition of pair defined by OPEN in the current buffer.

The value is fetched from `sp-local-pairs'.

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open sp-local-pairs prop))

(defun sp--merge-pair-configurations (specific &optional current)
  "Merge SPECIFIC pair configuration to the CURRENT configuration.

CURRENT defaults to `sp-local-pairs' if it is non-nil or the
global definition from `sp-pairs' if `sp-local-pairs' is nil."
  (let* ((global (or current sp-local-pairs (cdr (assq t sp-pairs))))
         (local specific)
         (result nil))
    ;; copy the pairs on global list first.  This creates new plists
    ;; so we can modify them without changing the global "template"
    ;; values.
    (dolist (old-pair global)
      (!cons (list :open (plist-get old-pair :open)) result))

    ;; merge the global list with result.  This basically "deep copy"
    ;; global list.  We use `sp--merge-pairs' because it also clones
    ;; the list properties (actions, filters etc.)
    (dolist (new-pair global)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (sp--merge-pairs old-pair new-pair)))

    ;; for each local pair, merge it into the global definition
    (dolist (new-pair local)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (if old-pair
            (sp--merge-pairs old-pair new-pair)
          ;; pair does not have global definition, simply copy it over
          (!cons
           ;; this "deep copy" the new-pair
           (sp--merge-pairs (list :open (plist-get new-pair :open)) new-pair)
           ;; TODO: remove the nil lists from the definitions
           result))))
    result))

(defun sp-wrap-with-pair (pair)
  "Wrap the following expression with PAIR.

This function is a non-interactive helper.  To use this function
interactively, bind the following lambda to a key:

 (lambda (&optional arg) (interactive \"P\") (sp-wrap-with-pair \"(\"))

This lambda accepts the same prefix arguments as
`sp-select-next-thing'.

If region is active and `use-region-p' returns true, the region
is wrapped instead.  This is useful with selection functions in
`evil-mode' to wrap regions with pairs."
  (let* ((arg (or current-prefix-arg 1))
         (sel (and (not (use-region-p))
                   (sp-select-next-thing-exchange
                    arg
                    (cond
                     ;; point is inside symbol and smart symbol wrapping is disabled
                     ((and (sp-point-in-symbol)
                           (or (eq sp-wrap-entire-symbol 'globally)
                               (memq major-mode sp-wrap-entire-symbol)))
                      (point))
                     ;; wrap from point, not the start of the next expression
                     ((and sp-wrap-from-point
                           (not (sp-point-in-symbol)))
                      (point))))))
         (active-pair (--first (equal (car it) pair) sp-pair-list))
         (rb (region-beginning))
         (re (region-end)))
    (goto-char re)
    (insert (cdr active-pair))
    (goto-char rb)
    (insert (car active-pair))
    (if (use-region-p)
        (sp--indent-region rb re)
      (sp-get sel (sp--indent-region :beg :end)))))

(cl-defun sp-pair (open
                   close
                   &key
                   trigger
                   trigger-wrap
                   (actions '(wrap insert autoskip navigate))
                   when
                   unless
                   pre-handlers
                   post-handlers
                   wrap
                   bind
                   insert)
  "Add, remove or update a pair definition.

The pair definition is removed if ACTIONS is :rem, added if it
does not exist, and updated otherwise.

OPEN is the opening delimiter.  Every pair is uniquely determined
by this string.

CLOSE is the closing delimiter.  You can use nil for this
argument if you are updating an existing definition.  In this
case, the old value is retained.

TRIGGER is an optional trigger for the pair.  The pair will be
inserted if either OPEN or TRIGGER is typed.  This is usually
used as a shortcut for longer pairs or for pairs that can't be
typed easily.

TRIGGER-WRAP is the same as TRIGGER but used for wrapping.

ACTIONS is a list of actions that smartparens will perform with
this pair.  Possible values are:

- insert  - autoinsert the closing pair when opening pair is
  typed.
- wrap    - wrap an active region with the pair defined by opening
  delimiter if this is typed while region is active.
- autoskip - if the sexp is active or `sp-autoskip-closing-pair' is
  set to \\='always, skip over the closing delimiter if user types its
  characters in order.
- navigate - enable this pair for navigation/highlight and strictness
  checks
- escape - allow autoescaping of this delimiter in string contexts

If the ACTIONS argument has value :rem, the pair is removed.
This can be used to remove default pairs you don't want to use.
For example: (sp-pair \"[\" nil :actions :rem)

WHEN is a list of predicates that test whether the action
should be performed in current context.  The values in the list
should be names of the predicates (that is symbols, not
lambdas!).  They should accept three arguments: opening
delimiter (which uniquely determines the pair), action and
context.  The context argument can have values:

- string  - if point is inside string.
- comment - if point is inside comment.
- code    - if point is inside code.  This context is only
  recognized in programming modes that define string semantics.

If *any* filter returns t, the action WILL be performed. A number
of filters are predefined: `sp-point-after-word-p',
`sp-point-before-word-p', `sp-in-string-p',
`sp-point-before-eol-p' etc.

When clause also supports a special format for delayed insertion.
The condition is a list with commands, predicates (with three
arguments as regular when form) or strings specifying the last
event.  All three types can be combined in one list.  The pair
will be inserted *after* the next command if it matches the any
command on the list, if the last event matches any string on the
list or if any predicate returns true.  If the pair's :when
clause contains this special form, it will never be immediately
inserted and will always test for delayed insertion.

UNLESS is a list of predicates.  The conventions are the same as
for the WHEN list.  If *any* filter on this list returns t, the
action WILL NOT be performed.  The predicates in the WHEN list
are checked first, and if any of them succeeds, the UNLESS list
is not checked.

Note: the functions on the WHEN/UNLESS lists are also called
\"filters\" in the documentation.

All the filters are run *after* the trigger character is
inserted.

PRE-HANDLERS is a list of functions that are called before there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to the point *before*
the last inserted character.  Because of the nature of the
wrapping operation, this hook is not called if the action is
wrapping.

POST-HANDLERS is a list of functions that are called after there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to current position of
point *after* the closing pair was inserted.

After a wrapping action, the point might end on either side of
the wrapped region, depending on the original direction.  You can
use the variable `sp-last-wrapped-region' to retrieve information
about the wrapped region and position the point to suit your
needs.

A special syntax for conditional execution of hooks is also
supported.  If the added item is a list (function command1
command2...), where function is a 3 argument function described
above and command(s) can be either name of a command or a string
representing an event.  If the last command or event as described
by `single-key-description' matches any on the list, the hook
will be executed.  This means these hooks are run not after the
insertion, but after the *next* command is executed.

Example:
  ((lambda (id act con)
     (save-excursion
       (newline))) \"RET\" newline)

This function will move the closing pair on its own line only if
the next command is `newline' or is triggered by RET.  Otherwise
the pairs stay on the same line.

WRAP is a key binding to which a \"wrapping\" action is bound.
The key should be in format that is accepted by `kbd'.  This
option binds a lambda form:

  `(lambda (&optional arg)
     (interactive \"P\")
     (sp-wrap-with-pair ,OPEN))

to the specified key sequence.  The binding is added to global
keymap.  When executed, it wraps ARG (default 1) expressions with
this pair (like `paredit-wrap-round' and friends).  Additionally,
it accepts the same prefix arguments as `sp-select-next-thing'.

BIND is equivalent to WRAP.  It is a legacy setting and will be
removed soon.

INSERT is a key binding to which an \"insert\" action is bound.
The key should be in format that is accepted by `kbd'.  This is
achieved by binding a lambda form:

 (lambda () (interactive) (sp-insert-pair \"pair-id\"))

to the supplied key, where pair-id is the open delimiter of the
pair.  The binding is added to the global map.  You can also bind
a similar lambda manually.  To only bind this in specific major
modes, use this property on `sp-local-pair' instead."
  (if (eq actions :rem)
      (let ((global-list (assq t sp-pairs)))
        (setcdr global-list (--remove (equal (plist-get it :open) open) (cdr global-list)))
        (--each (buffer-list)
          (with-current-buffer it (sp--remove-local-pair open))))
    (let ((pair nil))
      (setq pair (plist-put pair :open open))
      (when close (plist-put pair :close close))
      (when trigger (plist-put pair :trigger trigger))
      (when trigger-wrap (plist-put pair :trigger-wrap trigger-wrap))
      (dolist (arg `((:actions . ,actions)
                     (:when . ,when)
                     (:unless . ,unless)
                     (:pre-handlers . ,pre-handlers)
                     (:post-handlers . ,post-handlers)))
        ;; We only consider "nil" as a proper value if the property
        ;; already exists in the pair.  In that case, we will set it to
        ;; nil.  This allows for removing properties in global
        ;; definitions.
        (when (or (cdr arg)
                  (sp-get-pair-definition open t (car arg)))
          (plist-put pair (car arg) (cdr arg))))
      (sp--update-pair-list pair t))
    (when (or wrap bind) (global-set-key (read-kbd-macro (or wrap bind))
                                         `(lambda (&optional arg)
                                            (interactive "*P")
                                            (sp-wrap-with-pair ,open))))
    (when insert (global-set-key (kbd insert) `(lambda () (interactive "*") (sp-insert-pair ,open)))))
  (sp--update-local-pairs-everywhere)
  sp-pairs)

(cl-defun sp-local-pair (modes
                         open
                         close
                         &key
                         trigger
                         trigger-wrap
                         (actions '(:add))
                         (when '(:add))
                         (unless '(:add))
                         (pre-handlers '(:add))
                         (post-handlers '(:add))
                         wrap
                         bind
                         insert
                         prefix
                         suffix
                         skip-match)
  "Add a local pair definition or override a global definition.

MODES can be a single mode or a list of modes where these settings
should be applied.

PREFIX is a regular expression matching an optional prefix for
this pair in the specified major modes.  If not specified, the
characters of expression prefix syntax class are automatically
considered instead.  This can be used to attach custom prefixes
to pairs, such as prefix \"\\function\" in \\function{arg} in
`LaTeX-mode'.

SUFFIX is a regular expression matching an optional suffix for
this pair in the specified major modes.  If not specified, the
characters of punctuation syntax class are automatically
considered instead.

The rest of the arguments have same semantics as in `sp-pair'.

If the pair is not defined globally, ACTIONS defaults to (wrap
insert) instead of (:add) (which inherits global settings)

The pairs are uniquely identified by the opening delimiter.  If you
replace the closing one with a different string in the local
definition, this will override the global closing delimiter.

The list arguments can optionally be of form starting with
\":add\" or \":rem\" when these mean \"add to the global list\"
and \"remove from the global list\" respectively.  Otherwise,
the global list is replaced.  If you wish to both add and remove
things with single call, use \"((:add ...) (:rem ...))\" as an
argument.  Therefore,

  :when \\='(:add my-test)

would mean \"use the global settings for this pair, but also this
additional test\". If no value is provided for list arguments,
they default to \"(:add)\" which means they inherit the list from
the global definition.

To disable a pair in a major mode, simply set its actions set to
nil. This will ensure the pair is not even loaded when the mode is
activated.

If WRAP is non-nil, the binding is added into major mode keymap
called \"foo-mode-map\".  If the mode does not follow this
convention, you will need to bind the function manually (see
`sp-pair' to how the function is named for each particular pair).
The bindings are not added into `smartparens-mode-map' to prevent
clashes between different modes.

BIND is equivalent to WRAP.  It is a legacy setting and will be
removed soon.

The binding for INSERT follows the same convention as BIND.  See
`sp-pair' for more info.

You can provide a function SKIP-MATCH, that will take three
arguments: the currently matched delimiter, beginning of match
and end of match.  If this function returns true, the
`sp-get-paired-expression' matcher will ignore this match.  You
can use this to skip over expressions that serve multiple
functions, such as if/end pair or unary if in Ruby or * in
markdown when it signifies list item instead of emphasis.  In
addition, there is a global per major-mode option, see
`sp-navigate-skip-match'."
  (if (eq actions :rem)
      (dolist (m (-flatten (list modes)))
        (let ((mode-pairs (assq m sp-pairs)))
          (setcdr mode-pairs
                  (--remove (equal (plist-get it :open) open)
                            (cdr mode-pairs))))
        (sp-with-buffers-using-mode m
          (sp--remove-local-pair open)))
    (dolist (m (-flatten (list modes)))
      (let* ((pair nil))
        (setq pair (plist-put pair :open open))
        (when close (plist-put pair :close close))
        (when trigger (plist-put pair :trigger trigger))
        (when trigger-wrap (plist-put pair :trigger-wrap trigger-wrap))
        (when prefix (plist-put pair :prefix prefix))
        (when suffix (plist-put pair :suffix suffix))
        (when skip-match (plist-put pair :skip-match skip-match))
        (when (and (not (sp-get-pair-definition open t))
                   (equal actions '(:add)))
          (setq actions '(wrap insert autoskip navigate)))
        (plist-put pair :actions actions)
        (plist-put pair :when when)
        (plist-put pair :unless unless)
        (plist-put pair :pre-handlers pre-handlers)
        (plist-put pair :post-handlers post-handlers)
        (sp--update-pair-list pair m)
        (-when-let* ((symbol (intern (concat (symbol-name m) "-map")))
                     (map (and (boundp symbol) (symbol-value symbol))))
          (when (or wrap bind) (define-key map
                                 (read-kbd-macro (or wrap bind))
                                 `(lambda (&optional arg)
                                    (interactive "*P")
                                    (sp-wrap-with-pair ,open))))
          (when insert (define-key map
                         (kbd insert)
                         `(lambda () (interactive "*") (sp-insert-pair ,open))))))))
  (sp--update-local-pairs-everywhere (-flatten (list modes)))
  sp-pairs)

(cl-defun sp-local-tag (modes trig open close &key
                              (transform 'identity)
                              (actions '(wrap insert))
                              post-handlers)
  "Add a tag definition.

MODES is a mode or a list of modes where this tag should
activate.  It is impossible to define global tags.

TRIG is the trigger sequence.  It can be a string of any length.
If more triggers share a common prefix, the shortest trigger is
executed.

OPEN is the format of the opening tag.  This is inserted before
the active region.

CLOSE is the format of the closing tag.  This is inserted after
the active region.

Opening and closing tags can optionally contain the _ character.

If the opening tag contains the _ character, after you type the
trigger, the region is wrapped with \"skeleton\" tags and a
special tag editing mode is entered.  The text you now type is
substituted for the _ character in the opening tag.

If the closing tag contains the _ character, the text from the
opening pair is mirrored to the closing pair and substituted for
the _ character.

TRANSFORM is a function name (symbol) that is called to perform a
transformation of the opening tag text before this is inserted to
the closing tag.  For example, in html tag it might simply select
the name of the tag and cut off the tag attributes (like
class/style etc.).  Defaults to identity.

ACTIONS is a list of actions this tag should support. Currently,
only \"wrap\" action is supported.  Usually, you don't need to
specify this argument.

POST-HANDLERS is a list of functions that are called after the
tag is inserted.  If the tag does contain the _ character, these
functions are called after the tag editing mode is exited.  Each
function on this list should accept two arguments: the trigger
string and the action."
  (dolist (mode (-flatten (list modes)))
    (let* ((tag-list (assq mode sp-tags))
           (tag (--first (equal trig (plist-get it :trigger)) (cdr tag-list)))
           (new-tag nil))
      (setq new-tag (plist-put new-tag :trigger trig))
      (plist-put new-tag :open open)
      (plist-put new-tag :close close)
      (when transform (plist-put new-tag :transform transform))
      (when actions (plist-put new-tag :actions actions))
      (when post-handlers (plist-put new-tag :post-handlers post-handlers))
      (if tag-list
          (if (not actions)
              (setcdr tag-list (--remove (equal trig (plist-get it :trigger)) (cdr tag-list)))
            (if (not tag)
                (setcdr tag-list (cons new-tag (cdr tag-list)))
              (sp--update-pair new-tag tag)))
        ;; mode doesn't exist
        (when actions
          (!cons (cons mode (list new-tag)) sp-tags))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management

;; burlywood4
(defface sp-pair-overlay-face
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'smartparens)

(defface sp-wrap-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap overlays.

When the user wraps a region with multi-character pair a special
insertion mode is entered.  This face is used for the overlays
where the possible wrappings are displayed.

The opening and closing delimiters use
`sp-wrap-overlay-opening-pair' and `sp-wrap-overlay-closing-pair'
respectively."
  :group 'smartparens)

(defface sp-wrap-overlay-opening-pair
  '((t (:inherit sp-wrap-overlay-face
        :foreground "green")))
  "The face used to highlight opening pairs for wrapping.

See `sp-wrap-overlay-face'."
  :group 'smartparens)

(defface sp-wrap-overlay-closing-pair
  '((t (:inherit sp-wrap-overlay-face
        :foreground "red")))
  "The face used to highlight closing pairs for wrapping.

See `sp-wrap-overlay-face'."
  :group 'smartparens)

(defface sp-wrap-tag-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap tag overlays."
  :group 'smartparens)

(defvar-local sp-pair-overlay-list '()
  "List of overlays used for tracking inserted pairs.

When a pair is inserted, an overlay is created over it.  When the
user starts typing the closing pair we will not insert it again.
If user leaves the overlay, it is canceled and the insertion
works again as usual.")

(defvar-local sp-wrap-overlays nil
  "Cons pair of wrap overlays.")

(defvar-local sp-wrap-tag-overlays nil
  "Cons pair of tag wrap overlays.")

(defvar sp-pair-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g")
      '(menu-item nil sp-remove-active-pair-overlay :filter
                  (lambda (cmd)
                    (unless (bound-and-true-p company-my-keymap)
                      cmd))))
    map)
  "Keymap for the pair overlays.")

(defvar sp-wrap-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'sp-wrap-cancel)
    map)
  "Keymap for the wrap overlays.")

(defun sp--overlays-at (&optional pos)
  "Wrapper around `overlays-at' to get smartparens overlays.

POS is the same as for `overlays-at'.

Smartparens functions must use this function instead of
`overlays-at' directly."
  ;; TODO: we should probably also check the returned value
  (--filter (overlay-get it 'type) (overlays-at (or pos (point)))))

(defun sp--point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun sp--get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun sp--get-active-overlay (&optional type)
  "Get active overlay.

Active overlay is the shortest overlay at point.  Optional
argument TYPE restrict overlays to only those with given type."
  (let ((overlays (sp--overlays-at)))
    (when type
      (setq overlays (--filter (eq (overlay-get it 'type) type) overlays)))
    (cond
     ((not overlays) nil)
     ((not (cdr overlays)) (car overlays))
     (t
      (--reduce (if (< (sp--get-overlay-length it) (sp--get-overlay-length acc)) it acc) overlays)))))

(defun sp--pair-overlay-create (start end id)
  "Create an overlay over the currently inserted pair.

This overlay is used for tracking the position of the point and
marks the active expression.  START and END are the boundaries of
the overlay, ID is the id of the pair."
  (let ((overlay (make-overlay start end)))
    ;; set priority to 99 so that yasnippet with 100 overloads the
    ;; keymap #625
    (overlay-put overlay 'priority 99)
    (overlay-put overlay 'keymap sp-pair-overlay-keymap)
    (overlay-put overlay 'pair-id id)
    (overlay-put overlay 'type 'pair)
    (!cons overlay sp-pair-overlay-list)
    (sp--pair-overlay-fix-highlight)
    (add-hook 'post-command-hook 'sp--pair-overlay-post-command-handler nil t)))

(defun sp-wrap-cancel ()
  "Cancel the active wrapping."
  (interactive)
  (unwind-protect
      (-let (((obeg . oend) sp-wrap-overlays))
        (when (and (not (called-interactively-p 'any))
                   (sp--delete-selection-p))
          (kill-region (overlay-end obeg) (overlay-start oend)))
        (delete-region (overlay-start oend) (overlay-end oend))
        (when (> sp-wrap-point sp-wrap-mark)
          (let ((beg (delete-and-extract-region (overlay-start obeg) (overlay-end obeg))))
            (goto-char (overlay-start oend))
            (insert beg))))
    (sp-wrap--clean-overlays)))

(defun sp-wrap--clean-overlays ()
  "Delete wrap overlays."
  (-let [(obeg . oend) sp-wrap-overlays]
    (delete-overlay obeg)
    (delete-overlay oend)
    (setq sp-wrap-overlays nil)))

(defun sp--pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays.

Only the active overlay should be highlighted."
  (--each (sp--overlays-at) (overlay-put it 'face nil))
  (let* ((active (sp--get-active-overlay))
         (type (and active (overlay-get active 'type))))
    (if active
        (cond
         ((eq 'wrap-tag type)
          (when sp-highlight-wrap-tag-overlay
            (overlay-put active 'face 'sp-wrap-tag-overlay-face)))
         ((eq 'pair type)
          (when sp-highlight-pair-overlay
            (overlay-put active 'face 'sp-pair-overlay-face))))
      ;; edge case where we're at the end of active overlay.  If
      ;; there is a wrap-tag overlay, restore it's face
      (when sp-wrap-tag-overlays
        (overlay-put (car sp-wrap-tag-overlays) 'face 'sp-wrap-tag-overlay-face)))))

(defun sp--pair-overlay-post-command-handler ()
  "Remove all invalid pair overlays.

An invalid overlay is one that doesn't have point inside it or
is of zero length.

Also remove all pair overlays if point moved backwards and
`sp-cancel-autoskip-on-backward-movement' is non-nil."
  ;; if the point moved backwards, remove all overlays
  (if (and sp-cancel-autoskip-on-backward-movement
           (< (point) sp-previous-point))
      (dolist (o sp-pair-overlay-list) (sp--remove-overlay o))
    ;; else only remove the overlays where point is outside them or
    ;; their length is zero
    (dolist (o (--remove (and (sp--point-in-overlay-p it)
                              (> (sp--get-overlay-length it) 0))
                         sp-pair-overlay-list))
      (sp--remove-overlay o)))
  (when sp-pair-overlay-list
    (setq sp-previous-point (point))))

(defun sp--reset-memoization (&rest ignored)
  "Reset memoization as a safety precaution.

IGNORED is a dummy argument used to eat up arguments passed from
the hook where this is executed."
  (setf (sp-state-last-syntax-ppss-point sp-state) nil
        (sp-state-last-syntax-ppss-result sp-state) nil))

(defun sp-remove-active-pair-overlay ()
  "Deactivate the active overlay.  See `sp--get-active-overlay'."
  (interactive)
  (-when-let (active-overlay (sp--get-active-overlay 'pair))
    (sp--remove-overlay active-overlay)))

(defun sp--remove-overlay (overlay)
  "Remove OVERLAY."
  ;; if it's not a pair overlay, nothing happens here anyway
  (setq sp-pair-overlay-list (--remove (equal it overlay) sp-pair-overlay-list))
  ;; if we have zero pair overlays, remove the post-command hook
  (when (not sp-pair-overlay-list)
    (remove-hook 'post-command-hook 'sp--pair-overlay-post-command-handler t)
    ;; this is only updated when sp--pair-overlay-post-command-handler
    ;; is active.  Therefore, we need to reset this to 1.  If not, newly
    ;; created overlay could be removed right after creation - if
    ;; sp-previous-point was greater than actual point
    (setq sp-previous-point -1))
  (delete-overlay overlay)
  (sp--pair-overlay-fix-highlight))

(defun sp--replace-overlay-text (o string)
  "Replace text inside overlay O with STRING."
  (save-excursion
    (goto-char (overlay-start o))
    (insert string)
    (delete-region (point) (overlay-end o))))

(defun sp--get-overlay-text (o)
  "Get text inside overlay O."
  (buffer-substring (overlay-start o) (overlay-end o)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action predicates

(defun sp-in-string-p (_id _action context)
  "Return t if point is inside string or comment, nil otherwise."
  (eq context 'string))

(defun sp-in-string-quotes-p (_id action context)
  "Special string test for quotes.

On insert action, test the string context one character back from
point.  Return nil at `bobp'.

On escape action use the value of CONTEXT."
  (cond
   ((eq action 'insert)
    (if (bobp) nil
      (save-excursion (backward-char 1) (sp-point-in-string))))
   ((eq action 'escape)
    (eq context 'string))))

(defun sp-in-docstring-p (_id _action context)
  "Return t if point is inside elisp docstring, nil otherwise."
  (and (eq context 'string)
       (save-excursion
         (--when-let (car (sp-get-quoted-string-bounds))
           (goto-char it)
           (ignore-errors (backward-sexp 3))
           (looking-at-p (regexp-opt '("defun" "defmacro"
                                       "cl-defun" "cl-defmacro"
                                       "defun*" "defmacro*"
                                       "lambda" "-lambda")))))))

(defun sp-in-code-p (_id _action context)
  "Return t if point is inside code, nil otherwise."
  (eq context 'code))

(defun sp-in-comment-p (_id _action context)
  "Return t if point is inside comment, nil otherwise."
  (eq context 'comment))

(defun sp-in-math-p (_id _action _context)
  "Return t if point is inside code, nil otherwise."
  (when (functionp 'texmathp)
    (texmathp)))

(defun sp-point-before-eol-p (_id action _context)
  "Return t if point is followed by optional white spaces and end of
line, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-at-p "\\s-*$")))

(defun sp-point-after-bol-p (id action _context)
  "Return t if point follows beginning of line and possibly white
spaces, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "^\\s-*" (regexp-quote id)))))

(defun sp-point-at-bol-p (id action _context)
  "Return t if point is at the beginning of line, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "^" (regexp-quote id)))))

(defun sp-point-before-symbol-p (_id action _context)
  "Return t if point is followed by a symbol, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-at-p "\\s_")))

(defun sp-point-before-word-p (_id action _context)
  "Return t if point is followed by a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-at-p "\\sw\\|\\s_")))

(defun sp-point-after-word-p (id action _context)
  "Return t if point is after a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  ;; TODO: remove condition with sp-defpair
  (when (memq action '(insert escape))
    (sp--looking-back-p (concat "\\(\\sw\\|\\s_\\)" (regexp-quote id)))))

(defun sp-point-before-same-p (id action _context)
  "Return t if point is followed by ID, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-at-p (regexp-quote id))))

(defun sp-point-in-empty-line-p (id _action _context)
  "Return t if point is on an empty line, nil otherwise."
  (and (sp--looking-at-p "\\s-*$")
       (sp--looking-back-p (concat "^\\s-*" (regexp-quote id)))))

(defun sp-char-escaped-p (_id action _context)
  "Return non-nil if character before point is escaped with \\."
  (when (eq action 'insert)
    (unless (= (point) (point-min))
      (save-excursion
        (backward-char 1)
        (looking-back "\\\\" 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair insertion/deletion/skipping

(defun sp--do-action-p (id action &optional use-inside-string)
  "Return t if pair ID can perform ACTION.

If ACTION is a list, return t if at least one action from the
list can be performed.

If USE-INSIDE-STRING is non-nil, use value of
`sp-point-inside-string' instead of testing with
`sp-point-in-string-or-comment'."
  (setq action (-flatten (list action)))
  (let* ((actions (sp-get-pair id :actions))
         (when-l (sp-get-pair id :when))
         (unless-l (sp-get-pair id :unless))
         (in-string (if use-inside-string
                        sp-point-inside-string
                      (sp-point-in-string)))
         (context (cond
                   (in-string 'string)
                   ((sp-point-in-comment) 'comment)
                   (t 'code)))
         a r)
    (while (and action (not r))
      (setq a (car action))
      (setq r (when (memq a actions)
                ;;(and (when-clause) (not (unless-clause)))
                (and (or (not when-l)
                         (--some (funcall it id a context) when-l))
                     (or (not unless-l)
                         (not (--some (funcall it id a context) unless-l))))))
      (!cdr action))
    r))

(defun sp--get-handler-context (type)
  "Return the context constant.  TYPE is type of the handler."
  (let ((in-string (cl-case type
                     (:pre-handlers
                      (save-excursion
                        (unless (bobp) (backward-char 1))
                        (sp-point-in-string-or-comment)))
                     (:post-handlers
                      (sp-point-in-string-or-comment)))))
    (if in-string 'string 'code)))

(defun sp--get-context (&optional point in-string in-comment)
  "Return the context of POINT.

If the optional arguments IN-STRING or IN-COMMENT non-nil, their
value is used instead of a test."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((or in-string (sp-point-in-string)) 'string)
     ((or in-comment (sp-point-in-comment)) 'comment)
     (t 'code))))

(defun sp--parse-insertion-spec (fun)
  "Parse the insertion specification FUN and return a form to evaluate."
  (let ((spec nil)
        (after nil)
        (last 1))
    (cl-labels ((push-non-empty
                 (what)
                 (unless (equal (cadr what) "")
                   (push what spec))))
      (with-temp-buffer
        (insert fun)
        (goto-char (point-min))
        (while (re-search-forward "\\(|\\|\\[\\)" nil t)
          (cond
           ((equal (match-string 0) "[")
            (if (save-excursion (backward-char 1) (eq (preceding-char) 92))
                (push-non-empty `(insert ,(concat (buffer-substring-no-properties last (- (point) 2)) "[")))
              (push-non-empty `(insert ,(buffer-substring-no-properties last (1- (point)))))
              (let* ((p (point))
                     (fun-end (progn
                                (re-search-forward "]" nil t)
                                (1- (point))))
                     (fun-spec (buffer-substring-no-properties p fun-end))
                     (instruction (cond
                                   ((equal fun-spec "i")
                                    '(indent-according-to-mode))
                                   ((equal (aref fun-spec 0) ?d)
                                    `(delete-char ,(string-to-number (substring fun-spec 1)))))))
                (when instruction (push instruction spec)))))
           ((equal (match-string 0) "|")
            (cond
             ((save-excursion (backward-char 1) (eq (preceding-char) 92))
              (push-non-empty `(insert ,(concat (buffer-substring-no-properties last (- (point) 2)) "|"))))
             (t
              (push-non-empty `(insert ,(buffer-substring-no-properties last (1- (point)))))
              (push 'save-excursion spec)
              (when (eq (following-char) 124)
                (forward-char 1)
                (setq after '(indent-according-to-mode)))))))
          (setq last (point)))
        (push-non-empty `(insert ,(buffer-substring-no-properties last (point-max)))))
      (let* ((specr (nreverse spec))
             (specsplit (--split-with (not (eq it 'save-excursion)) specr))
             (re (-concat (car specsplit) (if (cadr specsplit) (cdr specsplit) nil))))
        (cons 'progn (if after (-snoc re after) re))))))

(defun sp--run-function-or-insertion (fun id action context)
  "Run a function or insertion.

If FUN is a function, call it with `funcall' with ID, ACTION and
CONTEXT as arguments.

If FUN is a string, interpret it as \"insertion specification\",
see `sp-pair' for description."
  (cond
   ((functionp fun)
    (funcall fun id action context))
   ((stringp fun)
    (eval (sp--parse-insertion-spec fun)))))


(defvar sp-handler-context nil
  "Special variable holding context during handler execution.")

;; TODO: get rid of `sp-handler-context' and make all the handlers (we
;; should call them hooks) take better arguments, what we pass now is
;; useless almost always
(defun sp--run-hook-with-args (id type action &optional context-values)
  "Run all the hooks for pair ID of type TYPE on action ACTION.

CONTEXT-VALUES is a plist with arbitrary values (depending on the
action).  A dynamic varable `sp-handler-context' will be bound to
this value during execution of the handler."
  (ignore-errors
    (let ((hook (sp-get-pair id type))
          (context (sp--get-handler-context type)))
      (if hook
          (let ((sp-handler-context context-values))
            (--each hook (sp--run-function-or-insertion it id action context)))
        (run-hook-with-args 'tag-hook id action context)))))

;; TODO: add a test for a symbol property that would tell this handler
;; not to re=set `sp-last-operation'. Useful for example in "macro
;; functions" like `my-wrap-with-paren'.
(defun sp--post-command-hook-handler ()
  "Handle the situation after some command has executed."
  (sp--with-case-sensitive
    (when (sp--special-self-insert-command-p)
      (sp--post-self-insert-hook-handler))
    (ignore-errors
      (when smartparens-mode
        ;; handle the wrap overlays
        (when sp-wrap-overlays
          (let* ((overlay (car sp-wrap-overlays))
                 (start (overlay-start overlay))
                 (end (overlay-end overlay))
                 (p (point)))
            (when (or (< p sp-previous-point)
                      (> p end)
                      (< p start))
              (sp-wrap-cancel))))
        (when sp-wrap-overlays
          (setq sp-previous-point (point)))

        ;; Here we run the delayed hooks. See issue #80
        (cond
         ((eq (car-safe (sp-state-delayed-hook sp-state)) :next)
          (setf (car (sp-state-delayed-hook sp-state)) :this))
         ((eq (car-safe (sp-state-delayed-hook sp-state)) :this)
          (let* ((pair (cdr (sp-state-delayed-hook sp-state)))
                 (hooks (sp-get-pair pair :post-handlers-cond)))
            (--each hooks
              (let ((fun (car it))
                    (conds (cdr it)))
                (when (or (--any? (eq this-command it) conds)
                          (--any? (equal (single-key-description last-command-event) it) conds))
                  (sp--run-function-or-insertion
                   fun pair 'insert
                   (sp--get-handler-context :post-handlers)))))
            (setf (sp-state-delayed-hook sp-state) nil)
            (setq sp-last-inserted-pair nil))))

        ;; Here we run the delayed insertion. Some details in issue #113
        (when (and (not (eq sp-last-operation 'sp-insert-pair-delayed))
                   sp-delayed-pair)
          (let* ((pair (car sp-delayed-pair))
                 (beg (cdr sp-delayed-pair))
                 (conds (sp-get-pair pair :when-cond))
                 (open-pair pair)
                 (close-pair (sp-get-pair pair :close)))
            (when (and conds
                       (--any? (cond
                                ((and (commandp it)
                                      (not (stringp it)))
                                 (eq this-command it))
                                ((stringp it)
                                 (equal (single-key-description last-command-event) it))
                                ((ignore-errors (funcall it pair 'insert (sp--get-handler-context :post-handlers))))) conds))
              ;; TODO: refactor this and the same code in
              ;; `sp-insert-pair' to a separate function
              (sp--run-hook-with-args open-pair :pre-handlers 'insert)
              (insert close-pair)
              (backward-char (length close-pair))
              (sp--pair-overlay-create beg
                                       (+ (point) (length close-pair))
                                       open-pair)
              ;; no auto-escape here? Should be fairly safe
              (sp--run-hook-with-args open-pair :post-handlers 'insert)
              (setq sp-last-inserted-pair open-pair)
              ;; TODO: this is probably useless
              (setq sp-last-operation 'sp-insert-pair)))
          (setq sp-delayed-pair nil))

        (when (eq sp-last-operation 'sp-insert-pair-delayed)
          (setq sp-last-operation nil))

        (unless (or (sp--self-insert-command-p)
                    (sp--special-self-insert-command-p))
          ;; unless the last command was a self-insert, remove the
          ;; information about the last wrapped region.  It is only used
          ;; for: 1. deleting the wrapping immediately after the wrap,
          ;; 2. re-wrapping region immediatelly after a sucessful wrap.
          ;; Therefore, the deletion should have no ill-effect.  If the
          ;; necessity will arise, we can add a different flag.
          (setq sp-last-wrapped-region nil)
          (setq sp-last-operation nil))

        (when show-smartparens-mode
          (if (member this-command sp-show-enclosing-pair-commands)
              (sp-show--pair-enc-function)
            (when (not (eq this-command 'sp-highlight-current-sexp))
              (sp-show--pair-delete-enc-overlays))))))))

(defmacro sp--setaction (action &rest forms)
  "Use ACTION as a flag to evaluating FORMS.

If ACTION is nil, evaluate FORMS and set it to the value of the
last form; otherwise do nothing."
  (declare (debug (form body)))
  `(unless ,action
     (setq ,action (progn ,@forms))))

;; TODO: this introduces a regression, where doing C-4 [ inserts [[[[]
;; figure out how to detect the argument to self-insert-command that
;; resulted to this insertion
(defun sp--post-self-insert-hook-handler ()
  "Handler for `post-self-insert-hook'."
  (with-demoted-errors "sp--post-self-insert-hook-handler: %S"
    (when smartparens-mode
      (sp--with-case-sensitive
        (catch 'done
          (let (action)
            (when (region-active-p)
              (condition-case err
                  (sp-wrap--initialize)
                (user-error
                 (message (error-message-string err))
                 ;; we need to remove the undo record of the insertion
                 (unless (eq buffer-undo-list t)
                   ;; pop all undo info until we hit an insertion node
                   (sp--undo-pop-to-last-insertion-node)
                   ;; get rid of it and insert an undo boundary marker
                   (pop buffer-undo-list)
                   (undo-boundary))
                 (restore-buffer-modified-p sp-buffer-modified-p)
                 (throw 'done nil))))
            (cond
             (sp-wrap-overlays
              (sp-wrap))
             (t
              ;; TODO: this does not pick correct pair!! it uses insert and not wrapping code
              (sp--setaction
               action
               (-when-let ((_ . open-pairs) (sp--all-pairs-to-insert nil 'wrap))
                 (catch 'done
                   (-each open-pairs
                     (-lambda ((&keys :open open :close close))
                       (--when-let (sp--wrap-repeat-last (cons open close))
                         (throw 'done it)))))))
              (unless overwrite-mode (sp--setaction action (sp-insert-pair)))
              (sp--setaction action (sp-skip-closing-pair))
              (unless action (sp-escape-open-delimiter))
              ;; if nothing happened, we just inserted a character, so
              ;; set the apropriate operation.
              (unless action
                (setq sp-last-operation 'sp-self-insert))))))))))

;; TODO: make a proper data structure for state tracking and describe
;; why we need each of these.
(defun sp--save-pre-command-state ()
  "Save some of the buffer state before `pre-command-hook'."
  (when smartparens-mode
    (setq sp-point-inside-string (sp-point-in-string))
    (setq sp-pre-command-point (point))
    (setq sp-buffer-modified-p (buffer-modified-p))))

(defun sp--get-pair-list ()
  "Get all non-stringlike pairs.

Return all pairs that are recognized in this `major-mode' and do
not have same opening and closing delimiter.  This is used for
navigation functions."
  (--filter (not (string= (car it) (cdr it))) sp-pair-list))

(defun sp--get-stringlike-list ()
  "Get all string-like pairs.

Return all pairs that are recognized in this `major-mode' that
have same opening and closing delimiter."
  (--filter (string= (car it) (cdr it)) sp-pair-list))

(defun sp--get-allowed-pair-list ()
  "Get all allowed non string-like pairs.

Return all pairs that are recognized in this `major-mode', do not
have same opening and closing delimiter and are allowed in the
current context.  See also `sp--get-pair-list'."
  (--filter (and (sp--do-action-p (car it) 'navigate)
                 (not (equal (car it) (cdr it)))) sp-pair-list))

(defun sp--get-allowed-stringlike-list ()
  "Get all allowed string-like pairs.

Return all pairs that are recognized in this `major-mode',
have the same opening and closing delimiter and are allowed in
the current context."
  (--filter (and (sp--do-action-p (car it) 'navigate)
                 (equal (car it) (cdr it))) sp-pair-list))

(defun sp--get-pair-list-context (&optional action)
  "Return all pairs that are recognized in this `major-mode' and
are allowed in the current context."
  (setq action (or action 'insert))
  (--filter (sp--do-action-p (car it) action) sp-pair-list))

(defun sp--get-pair-list-wrap ()
  "Return the list of all pairs that can be used for wrapping."
  (--filter (sp--do-action-p (car it) 'wrap) sp-pair-list))

(defun sp--wrap-regexp (string start end)
  "Wraps regexp with start and end boundary conditions to avoid
matching symbols in symbols."
  (concat "\\(?:" (when start "\\<") string (when end "\\>") "\\)"))

(defun sp--regexp-for-group (parens &rest strings)
  "Generates an optimized regexp matching all string, but with
extra boundary conditions depending on parens."
  (let* ((start (car parens))
         (end (cadr parens)))
    (sp--wrap-regexp (regexp-opt strings) start end)))

(defun sp--strict-regexp-opt (strings &optional ignored)
  "Like regexp-opt, but with extra boundary conditions to ensure
that the strings are not matched in-symbol."
  (if strings
      (with-syntax-table
          ;; HACK: this is a terrible hack to make ' be treated as a
          ;; punctuation.  Many text modes set it as word character which
          ;; messes up the regexps
          (let ((table (make-syntax-table (syntax-table))))
            (modify-syntax-entry ?' "." table)
            table)
        (--> strings
             (-group-by (lambda (string)
                          (list (and (string-match-p "\\`\\<" string) t)
                                (and (string-match-p "\\>\\'" string) t)))
                        it)
             (mapconcat (lambda (g) (apply 'sp--regexp-for-group g)) it "\\|")
             (concat "\\(?:" it "\\)")))
    "^\\<$"))

(defun sp--strict-regexp-quote (string)
  "Like regexp-quote, but make sure that the string is not
matched in-symbol."
  (sp--wrap-regexp (regexp-quote string)
                   (string-match-p "\\`\\<" string)
                   (string-match-p "\\>\\'" string)))

(cl-defun sp--get-opening-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any opening pair."
  (sp--strict-regexp-opt (--map (car it) pair-list)))

(cl-defun sp--get-closing-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any closing pair."
  (sp--strict-regexp-opt (--map (cdr it) pair-list)))

(cl-defun sp--get-allowed-regexp (&optional (pair-list (sp--get-allowed-pair-list)))
  "Return regexp matching any opening or closing
delimiter for any pair allowed in current context."
  (sp--strict-regexp-opt (--mapcat (list (car it) (cdr it)) pair-list)))

(cl-defun sp--get-stringlike-regexp (&optional (pair-list (sp--get-allowed-stringlike-list)))
  "Return a regexp matching any string-like delimiter.

In case PAIR-LIST is empty return a regexp that never matches
anything."
  (if (consp pair-list)
      (regexp-opt (--map (car it) pair-list))
    "^\\<$"))

(defun sp--make-last-wraped-region (beg end open close)
  "Return `sp-get-sexp' style plist about the last wrapped region.

Note: this function does not retrieve the actual value of
`sp-last-wrapped-region', it merely construct the plist from the
provided values."
  (let ((b (make-marker))
        (e (make-marker)))
    (set-marker b beg)
    (set-marker e end)
    `(:beg ,b :end ,e :op ,open :cl ,close :prefix "")))

;; Wrapping is basically the same thing as insertion, only the closing
;; pair is placed at a distance.

;; However, we want to be able to insert the *closing* delimiter and
;; go to the end of block.  This will only work with delimiters which
;; are unique wrt their opening one.  For more complex wrapping, there
;; will probably be an IDO/minibuffer interface.  Openings are checked
;; first.

;; Inserting the opening delimiter should put the point wherever it
;; was when we started insertion.

(defun sp-wrap--can-wrap-p ()
  "Return non-nil if we can wrap a region.

This is used in advices on various pre-command-hooks from
\"selection deleting\" modes to intercept their actions.

Also added to `self-insert-uses-region-functions' to prevent
`delete-selection-mode' from replacing the region."
  (let* ((list (sp--get-pair-list-wrap))
         (desc (sp--single-key-description last-command-event)))
    (--any? (or (string-prefix-p desc (car it))
                (string-prefix-p desc (cdr it)))
            list)))

(defun sp--pair-to-wrap-comparator (prop a b)
  "Comparator for wrapping pair selection.

PROP specifies wrapping-end.  A and B are pairs to be compared."
  (< (length (plist-get a prop)) (length (plist-get b prop))))

(defun sp--pair-to-wrap (&optional prefix)
  "Return information about possible wrapping pairs.

If optional PREFIX is non-nil, this is used to determine the
possible wrapping pairs instead of the text in the wrapping
overlay."
  (let* ((working-pairs
          ;; TODO: abstract this into a new "sp--get-..." hierarchy
          (--filter (sp--do-action-p (plist-get it :open) 'wrap) sp-local-pairs))
         (obeg (car sp-wrap-overlays))
         (prefix (or prefix (sp--get-overlay-text obeg)))
         (opening-pairs (--filter (string-prefix-p prefix (plist-get it :open)) working-pairs))
         ;; HACK: Here, we will add special "trigger pairs" to the
         ;; opening list.  We set the opening delimiter to the
         ;; trigger, leave the rest alone and put the real open into
         ;; :open-real property.  When we get the pair back, we will
         ;; check this property, and if present, fix the pair back to
         ;; the regular form
         (wrapper-pairs (->> (--filter (string-prefix-p prefix (or (plist-get it :trigger-wrap) "")) working-pairs)
                             (-map (-lambda ((pair &as &plist :open open :trigger-wrap trigger-wrap))
                                     (setq pair (copy-sequence pair))
                                     (setq pair (plist-put pair :open trigger-wrap))
                                     (setq pair (plist-put pair :open-real open))
                                     pair))))
         (opening-pairs (-concat wrapper-pairs opening-pairs))
         (closing-pairs (--filter (string-prefix-p prefix (plist-get it :close)) working-pairs))
         (open (car (--sort (sp--pair-to-wrap-comparator :open it other) opening-pairs)))
         ;; TODO: do we need the special sorting here?
         (close (car (--sort (sp--pair-to-wrap-comparator :close it other) closing-pairs))))
    (list :open open
          :close close
          :opening opening-pairs
          :closing closing-pairs)))

(defun sp-wrap--initialize ()
  "Initialize wrapping."
  (when (and sp-autowrap-region
             (sp-wrap--can-wrap-p))
    ;; This is the length of string which was inserted by the last
    ;; "self-insert" action.  Typically this is 1, but sometimes a
    ;; single key inserts two or more characters, such as " in latex
    ;; where it translates into `` or ''.
    (let ((inserted-string-length (- (point) sp-pre-command-point)))
      ;; TODO: get rid of the following variables
      (setq sp-wrap-point (- (point) inserted-string-length))
      (setq sp-wrap-mark (mark))
      ;; balance check
      (with-silent-modifications
        (let ((inserted-string
               (prog1 (delete-and-extract-region sp-wrap-point (point))
                 ;; HACK: in modes with string fences, the insertion
                 ;; of the delimiter causes `syntax-propertize' to
                 ;; fire, but the above deletion doesn't re-run it
                 ;; because the cache tells it the state is OK.  We
                 ;; need to destroy the cache and re-run the
                 ;; `syntax-propertize' on the buffer.  This might be
                 ;; expensive, but we only done this on wrap-init so
                 ;; it's fine, I guess.
                 (setq syntax-propertize--done -1)
                 (syntax-propertize (point-max))))
              (point-string-context (sp-get-quoted-string-bounds sp-wrap-point))
              (mark-string-context (sp-get-quoted-string-bounds (mark))))
          ;; If point and mark are inside the same string, we don't
          ;; need to check if the region is OK.  If both are outisde
          ;; strings, we have to.  If one is inside and the other is
          ;; not, no matter what we would break, so we exit.
          (cond
           ;; inside the same string
           ((and point-string-context mark-string-context
                 (eq (car point-string-context)
                     (car mark-string-context))))
           ;; neither is inside string
           ((and (not point-string-context)
                 (not mark-string-context))
            (unless (sp-region-ok-p sp-wrap-point (mark))
              (user-error "Mismatched sexp state: wrapping would break structure")))
           ;; one is in and the other isn't
           ((if point-string-context (not mark-string-context) mark-string-context)
            (user-error "Mismatched string state: point %sin string, mark %sin string"
                        (if (car-safe point-string-context) "" "not ")
                        (if (car-safe mark-string-context) "" "not ")))
           ;; both are in but in different strings
           (t (user-error "Mismatched string state: point and mark are inside different strings")))
          (insert inserted-string)))
      ;; if point > mark, we need to move point to mark and reinsert the
      ;; just inserted character.
      (when (> (point) (mark))
        (let ((char (delete-and-extract-region (- (point) inserted-string-length) (point))))
          (exchange-point-and-mark)
          (insert char)))
      (let* ((oleft (make-overlay (- (region-beginning) inserted-string-length)
                                  (region-beginning) nil nil t))
             (oright (make-overlay (region-end) (region-end) nil nil t)))
        (setq sp-wrap-overlays (cons oleft oright))
        (when sp-highlight-wrap-overlay
          (overlay-put oleft 'face 'sp-wrap-overlay-face)
          (overlay-put oright 'face 'sp-wrap-overlay-face))
        (overlay-put oleft 'priority 100)
        (overlay-put oright 'priority 100)
        (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
        (overlay-put oleft 'type 'wrap)
        (setq sp-previous-point (point))
        (goto-char (1+ (overlay-start oleft)))))))

(defun sp-wrap--finalize (wrapping-end open close)
  "Finalize a successful wrapping.

WRAPPING-END specifies the wrapping end.  If we wrapped using
opening delimiter it is :open.  If we wrapped using closing
delimiter it is :close.  Position of point after wrapping depends
on this value---if :open, go where the wrapping was initalized,
if :close, go after the newly-formed sexp.

OPEN and CLOSE are the delimiters."
  (-let (((obeg . oend) sp-wrap-overlays))
    (sp--replace-overlay-text obeg open)
    (sp--replace-overlay-text oend close)
    (setq sp-last-operation 'sp-wrap-region)
    (setq sp-last-wrapped-region
          (sp--make-last-wraped-region
           (overlay-start obeg) (overlay-end oend)
           open close))
    (cond
     ((eq wrapping-end :open)
      (if sp-wrap-respect-direction
          (progn
            (set-mark (overlay-end oend))
            (goto-char (overlay-start obeg)))
        (when (> sp-wrap-point sp-wrap-mark)
          (set-mark (overlay-start obeg))
          (goto-char (overlay-end oend)))))
     ((eq wrapping-end :close)
      (set-mark (overlay-start obeg))
      (goto-char (overlay-end oend))))
    (sp-wrap--clean-overlays)
    (sp--run-hook-with-args open :post-handlers 'wrap)))

(defun sp-wrap ()
  "Try to wrap the active region with some pair.

This function is not ment to be used to wrap sexps with pairs
programatically.  Use `sp-wrap-with-pair' instead."
  (-let* (((&plist :open open :close close
                   :opening opening-pairs
                   :closing closing-pairs) (sp--pair-to-wrap))
          ((obeg . oend) sp-wrap-overlays))
    (cond
     (open
      (-let (((&plist :open open :close close :open-real open-real) open))
        (when sp-wrap-show-possible-pairs
          (overlay-put
           oend 'after-string
           (mapconcat (lambda (x)
                        (if sp-highlight-wrap-overlay
                            (concat
                             (propertize
                              (plist-get x :open) 'face
                              'sp-wrap-overlay-opening-pair)
                             (propertize
                              (plist-get x :close)
                              'face 'sp-wrap-overlay-closing-pair))
                          (concat (plist-get x :open) (plist-get x :close))))
                      opening-pairs " ")))
        (when (equal (sp--get-overlay-text obeg) open)
          (sp-wrap--finalize :open (or open-real open) close))))
     ((and close (= 1 (length closing-pairs)))
      (-let (((&plist :open open :close close) close))
        (when (equal (sp--get-overlay-text obeg) close)
          (sp-wrap--finalize :close open close))))
     (t
      (sp-wrap-cancel)))))

(defun sp--escape-region (chars-to-escape beg end)
  "Escape instances of CHARS-TO-ESCAPE between BEG and END.

Return non-nil if at least one escaping was performed."
  (save-excursion
    (goto-char beg)
    (let ((pattern (regexp-opt chars-to-escape))
          (end-marker (set-marker (make-marker) end))
          (re nil))
      (while (re-search-forward pattern end-marker t)
        (setq re t)
        (save-excursion
          (goto-char (match-beginning 0))
          (insert sp-escape-char)))
      re)))

;; TODO: refactor the rewrap-sexp dependent parts out so that this
;; function has less dependencies on the action
;; TODO: add mode-dependent escape/unescape actions?
(defun sp-escape-wrapped-region (id action _context)
  "Escape quotes and special chars when a region is (re)wrapped."
  (when (and sp-escape-wrapped-region
             (memq action '(wrap rewrap-sexp)))
    (sp-get sp-last-wrapped-region
      (let* ((parent-delim (save-excursion
                             (goto-char :beg)
                             (sp-get (sp-get-string)
                               (cond
                                ((and (< :beg (point))
                                      (< (point) :end))
                                 :op)
                                ((eq action 'rewrap-sexp)
                                 (plist-get sp-handler-context :parent)))))))
        (cond
         ((equal parent-delim id)
          (sp--escape-region (list id sp-escape-char) :beg :end))
         (parent-delim
          (sp--escape-region (list id) :beg-in :end-in))
         (t
          (sp--escape-region (list id sp-escape-char) :beg-in :end-in)))))))

(defun sp-escape-quotes-after-insert (id action context)
  "Escape quotes inserted via `sp-insert-pair'."
  (when (and sp-escape-quotes-after-insert
             (eq action 'insert)
             ;; we test not being inside string because if we were
             ;; before inserting the "" pair it is now split into two
             ;; -> which moves us outside the pair
             (not (eq context 'string))
             ;; the inserted character must have string syntax,
             ;; otherwise no "context" flip happens
             (eq (syntax-class
                  (syntax-after
                   (save-excursion
                     (backward-char (length id))
                     (point)))) 7))
    (let ((open id)
          (close (sp-get-pair id :close)))
      (sp--escape-region (list open close)
                         (- (point) (length open))
                         (+ (point) (length close))))))

(defun sp--buffer-is-string-balanced-p ()
  "Check if the buffer is string-balanced.

A string-balanced buffer is one where where is no unclosed
string, that is, the string state at the end of the buffer is
\"closed\"."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((syntax (sp--syntax-ppss)))
        (or (< (car syntax) 0)
            (nth 3 syntax))))))

(defun sp-escape-open-delimiter ()
  "Escape just inserted opening pair if `sp-insert-pair' was skipped.

This is useful for escaping of \" inside strings when its pairing
is disabled.  This way, we can control autoescape and closing
delimiter insertion separately."
  (-when-let (open (plist-get (sp--pair-to-insert 'escape) :open))
    (when (and (sp--do-action-p open 'escape)
               sp-point-inside-string
               ;; do not escape if we are looking at a closing
               ;; delimiter, that means we closed an opened string,
               ;; most likely.
               (sp--buffer-is-string-balanced-p)
               ;; in some text modes like org-mode which do not
               ;; respect escapes, an "escaped" quote will still
               ;; behave as regular quote, but we want to ignore it to
               ;; be logically consistent.  This will prevent a buffer
               ;; with \" followed by newly inserted " auto-escaping
               ;; the inserted quotes (which actually closes the
               ;; string and makes the buffer balanced)
               (save-excursion
                 (backward-char (length open))
                 (-when-let (string-start (nth 8 (sp--syntax-ppss)))
                   (goto-char string-start)
                   (not (sp-char-is-escaped-p)))))
      (sp--escape-region (list open) (- (point) (length open)) (point)))))

;; kept to not break people's config... remove later
(defun sp-match-sgml-tags (tag)
  "Split the html tag TAG at the first space and return its name."
  (let* ((split (split-string tag " "))
         (close (car split)))
    close))
(make-obsolete 'sp-match-sgml-tags "do not use this function as the tag system has been removed." "2015-02-07")

(defun sp--is-number-cons (c)
  "Return non-nil if C is a cons cell with numbers at `car' and `cdr'."
  (and (consp c) (numberp (car c)) (numberp (cdr c))))

;; TODO: more research is needed
(defun sp--undo-pop-to-last-insertion-node ()
  "Pop all undo info until an insertion node (beg . end) is found.

This can potentially remove some undo important information."
  (while (and buffer-undo-list
              (or (null (car buffer-undo-list)) ;; is nil
                  ;; is not undo action we're interested in
                  (not (sp--is-number-cons (car buffer-undo-list)))))
    (pop buffer-undo-list)))

;; modified from: https://github.com/Fuco1/smartparens/issues/90#issuecomment-18800369
(defun sp--split-last-insertion-undo (len)
  "Split the last insertion node in the `buffer-undo-list' to
include separate pair node."
  (sp--undo-pop-to-last-insertion-node)
  (when buffer-undo-list
    (let* ((previous-undo-actions (cdr buffer-undo-list))
           (beg (caar buffer-undo-list))
           (end (cdar buffer-undo-list))
           first-action second-action)
      (unless (< beg (- end len))
        ;; We need to go back more than one action.  Given the pairs
        ;; are limited to 10 chars now and the chunks seem to be 20
        ;; chars, we probably wouldn't need more.
        (pop buffer-undo-list)
        (sp--undo-pop-to-last-insertion-node)
        (when buffer-undo-list
          (setq beg (caar buffer-undo-list))
          (setq previous-undo-actions (cdr buffer-undo-list))))
      (setq first-action (cons beg (- end len)))
      (setq second-action (cons (- end len) end))
      (setq buffer-undo-list
            (append (list nil second-action nil first-action)
                    previous-undo-actions)))))

;; TODO: remove ACTION argument and make the selection process more
;; unified (see also sp--pair-to-wrap which depends on buffer state
;; among other things)
(defun sp--all-pairs-to-insert (&optional looking-fn action)
  "Return all pairs that can be inserted at point.

Return nil if such pair does not exist.

Pairs inserted using a trigger have higher priority over pairs
without a trigger and only one or the other list is returned.

In other words, if any pair can be inserted using a trigger, only
pairs insertable by trigger are returned.

ACTION is an implementation detail.  Usually it has the value
\\='insert when we determine pairs to insert.  On repeated wrapping
however we pass the value 'wrap.  This will be refactored away in
the upcoming version."
  (setq looking-fn (or looking-fn 'sp--looking-back-p))
  (setq action (or action 'insert))
  (let ((working-pairs
         ;; TODO: abstract this into a new "sp--get-..." hierarchy
         (--filter (sp--do-action-p (plist-get it :open) action) sp-local-pairs)))
    (-if-let (trigs (--filter (and (plist-get it :trigger)
                                   (funcall looking-fn (sp--strict-regexp-quote (plist-get it :trigger))))
                              working-pairs))
        (cons :trigger trigs)
      (-when-let (pairs (--filter (funcall looking-fn (sp--strict-regexp-quote (plist-get it :open))) working-pairs))
        (cons :open pairs)))))

(defun sp--pair-to-insert-comparator (prop a b)
  (cond
   ;; in case of triggers shorter always wins
   ((eq prop :trigger)
    (< (length (plist-get a :trigger)) (length (plist-get b :trigger))))
   ;; Shorter wins only if the shorter's closing is a prefix of the
   ;; longer's closing.  In other words, if we are looking at
   ;; shorter's closing and we are trying to nest it.
   (t
    (if (< (length (plist-get a :open)) (length (plist-get b :open)))
        (and (string-prefix-p (plist-get a :close) (plist-get b :close))
             (sp--looking-at-p (plist-get a :close)))
      (not (and (string-prefix-p (plist-get b :close) (plist-get a :close))
                (sp--looking-at-p (plist-get b :close))))))))

(defun sp--pair-to-insert (&optional action)
  "Return pair that can be inserted at point.

Return nil if such pair does not exist.

If more triggers or opening pairs are possible select the
shortest one."
  (-when-let ((property . pairs) (sp--all-pairs-to-insert nil action))
    (car (--sort (sp--pair-to-insert-comparator property it other) pairs))))

(defun sp--longest-prefix-to-insert ()
  "Return pair with the longest :open which can be inserted at point."
  (-when-let (pairs (--filter (sp--looking-back-p (sp--strict-regexp-quote (plist-get it :open))) sp-local-pairs))
    (car (--sort (> (length (plist-get it :open)) (length (plist-get other :open))) pairs))))

(defun sp--pair-to-uninsert ()
  "Return pair to uninsert.

If the current to-be-inserted pair shares a prefix with
another (shorter) pair, we must first remove the effect of
inserting its closing pair before inserting the current one.

The previously inserted pair must be the one with the longest
common prefix excluding the current pair."
  (-when-let (lp (sp--longest-prefix-to-insert))
    (save-excursion
      (backward-char (length (plist-get lp :open)))
      (-when-let ((property . pairs) (sp--all-pairs-to-insert 'sp--looking-at-p))
        (car (--sort (> (length (plist-get it property)) (length (plist-get other property)))
                     ;; remove pairs whose open is longer than the
                     ;; current longest possible prefix---otherwise
                     ;; they would overflow to the closing pair
                     ;; TODO: this ignores the possibility when lp is
                     ;; inserted by trigger.  We assume triggers are
                     ;; shorter than the openings and this situation,
                     ;; if ever, should be very rare
                     (--remove (>= (length (plist-get it :open))
                                   (length (plist-get lp :open))) pairs)))))))

(defun sp--insert-pair-get-pair-info (active-pair)
  "Get basic info about the to-be-inserted pair."
  (let ((open-pair (plist-get active-pair :open)))
    (list
     open-pair
     (plist-get active-pair :close)
     (-if-let (tr (plist-get active-pair :trigger))
         (if (sp--looking-back-p (sp--strict-regexp-quote tr)) tr open-pair)
       open-pair))))

(defun sp-insert-pair (&optional pair)
  "Automatically insert the closing pair if it is allowed in current context.

If PAIR is provided, use this as pair ID instead of looking
through the recent history of pressed keys.

You can disable this feature completely for all modes and all pairs by
setting `sp-autoinsert-pair' to nil.

You can globally disable insertion of closing pair if point is
followed by the matching opening pair.  It is disabled by
default."
  (sp--with-case-sensitive
    (catch 'done
      (-let* ((active-pair (unwind-protect
                               ;; This fake insertion manufactures proper
                               ;; context for the tests below... in effect
                               ;; we must make it look as if the user
                               ;; typed in the opening part themselves
                               ;; TODO: it is duplicated in the test
                               ;; below, maybe it wouldn't hurt to
                               ;; restructure this function a bit
                               (progn
                                 (when pair (insert pair))
                                 (sp--pair-to-insert))
                             (when pair (delete-char (- (length pair))))))
              ((open-pair close-pair trig) (sp--insert-pair-get-pair-info active-pair)))
        ;; We are not looking at a closing delimiter which might mean we
        ;; are in an already existing sexp.  If the to-be-inserted pair
        ;; has a prefix which is also a pair we migth be extending the
        ;; opener of a sexp with this opener.  In which case we should
        ;; probably rewrap.
        (unless (sp--looking-at-p (sp--get-closing-regexp))
          (when (and open-pair
                     (= 1 (- (point) sp-pre-command-point)))
            (-when-let (prefix-pair (sp-get-pair (substring open-pair 0 -1)))
              (let ((last-char-of-open-pair (substring open-pair -1)))
                (unwind-protect
                    (progn
                      (delete-char -1)
                      (--when-let (sp-get-thing t)
                        (save-excursion
                          (sp-get it
                            (delete-region :end-in :end)
                            (goto-char :end-in)
                            (insert close-pair)))
                        (throw 'done t)))
                  (insert last-char-of-open-pair))))))
        (if (not (unwind-protect
                     (progn
                       (when pair (insert pair))
                       ;; TODO: all these tests must go into `sp--pair-to-insert'
                       (and sp-autoinsert-pair
                            active-pair
                            (if (memq sp-autoskip-closing-pair '(always always-end))
                                (or (not (equal open-pair close-pair))
                                    (not (sp-skip-closing-pair nil t)))
                              t)
                            (sp--do-action-p open-pair 'insert t)
                            ;; was sp-autoinsert-if-followed-by-same
                            (or (not (sp--get-active-overlay 'pair))
                                (not (sp--looking-at (sp--strict-regexp-quote open-pair)))
                                (and (equal open-pair close-pair)
                                     (eq sp-last-operation 'sp-insert-pair)
                                     (save-excursion
                                       (backward-char (length trig))
                                       (sp--looking-back (sp--strict-regexp-quote open-pair))))
                                (not (equal open-pair close-pair)))))
                   (when pair (delete-char (- (length pair))))))
            ;; if this pair could not be inserted, we try the procedure
            ;; again with this pair removed from sp-pair-list to give
            ;; chance to other pairs sharing a common suffix (for
            ;; example \[ and [)
            (let ((new-sp-pair-list (--remove (equal (car it) open-pair) sp-pair-list))
                  (new-sp-local-pairs (--remove (equal (plist-get it :open) open-pair) sp-local-pairs)))
              (when (> (length sp-pair-list) (length new-sp-pair-list))
                (let ((sp-pair-list new-sp-pair-list)
                      (sp-local-pairs new-sp-local-pairs))
                  (sp-insert-pair))))
          ;; setup the delayed insertion here.
          (if (sp-get-pair open-pair :when-cond)
              (progn
                (setq sp-delayed-pair (cons open-pair (- (point) (length open-pair))))
                (setq sp-last-operation 'sp-insert-pair-delayed))
            (unless pair (delete-char (- (length trig))))
            (insert open-pair)
            (sp--run-hook-with-args open-pair :pre-handlers 'insert)
            ;; The re-binding of these dynamic variables is a hack to
            ;; combat the similar rebinding in the branch above where
            ;; we retry `sp-insert-pair' with some pairs removed.
            ;; This however causes them to be uninserted improperly,
            ;; so for this one operation we need to restore the state
            ;; to the "full" pair list.  TODO: in the future we might
            ;; want to pass the state around explicitly so we have
            ;; better control.
            (--when-let (let ((sp-pair-list (sp-state-pair-list sp-state))
                              (sp-local-pairs (sp-state-local-pairs sp-state)))
                          (sp--pair-to-uninsert))
              (let ((cl (plist-get it :close)))
                (when (and (sp--looking-at-p (sp--strict-regexp-quote cl))
                           (> (- (length close-pair) (length cl)) 0))
                  (delete-char (length cl)))))
            (insert close-pair)
            (backward-char (length close-pair))
            (sp--pair-overlay-create (- (point) (length open-pair))
                                     (+ (point) (length close-pair))
                                     open-pair)
            (when sp-undo-pairs-separately
              (sp--split-last-insertion-undo (+ (length open-pair) (length close-pair)))
              ;; TODO: abc\{abc\} undo undo \{asd\} . next undo removes the
              ;; entire \{asd\} if we do not insert two nils here.
              ;; Normally, repeated nils are ignored so it shouldn't
              ;; matter.  It would still be useful to inspect further.
              (push nil buffer-undo-list)
              (push nil buffer-undo-list))
            (sp--run-hook-with-args open-pair :post-handlers 'insert)
            (setq sp-last-inserted-pair open-pair)
            (setf (sp-state-delayed-hook sp-state) (cons :next open-pair))
            (setq sp-last-operation 'sp-insert-pair)))))))

(defun sp--wrap-repeat-last (active-pair)
  "If the last operation was a wrap and `sp-wrap-repeat-last' is
non-nil, repeat the wrapping with this pair around the last
active region."
  (unless (= 0 sp-wrap-repeat-last)
    (when sp-last-wrapped-region
      (let* ((b (sp-get sp-last-wrapped-region :beg))
             (e (sp-get sp-last-wrapped-region :end))
             (op (sp-get sp-last-wrapped-region :op))
             (oplen (length op))
             (cllen (sp-get sp-last-wrapped-region :cl-l))
             (acolen (length (car active-pair))))
        (when (and
               (cond
                ((= 1 sp-wrap-repeat-last)
                 (equal (car active-pair) op))
                ((= 2 sp-wrap-repeat-last)))
               (memq sp-last-operation '(sp-self-insert sp-wrap-region))
               (or (= (point) (+ b oplen acolen))
                   (= (point) (+ e acolen))))
          (delete-char (- acolen))
          (if (< (point) e)
              (progn (goto-char (+ b oplen))
                     (insert (car active-pair))
                     (goto-char (- e cllen))
                     (insert (cdr active-pair))
                     (setq sp-last-wrapped-region
                           (sp--make-last-wraped-region
                            (+ b oplen) (point)
                            (car active-pair) (cdr active-pair)))
                     (goto-char (+ b oplen acolen)))
            (goto-char b)
            (insert (car active-pair))
            (goto-char e)
            (insert (cdr active-pair))
            (setq sp-last-wrapped-region
                  (sp--make-last-wraped-region
                   b (+ e acolen) (car active-pair) (cdr active-pair))))
          (setq sp-last-operation 'sp-wrap-region)
          (sp--run-hook-with-args (car active-pair) :post-handlers 'wrap)
          sp-last-operation)))))

(defun sp--char-is-part-of-stringlike (char)
  "Return non-nil if CHAR is part of a string-like delimiter of length 1."
  (->> (sp--get-stringlike-list)
    (--filter (= 1 (length (cdr it))))
    (-map 'car)
    (--any? (string-match-p (regexp-quote char) it))))

(defun sp--char-is-part-of-closing (char &optional pair-list)
  "Return non-nil if CHAR is part of a pair delimiter of length 1.
Specifically, return the pair for which CHAR is the closing
delimiter."
  (let ((regexp (regexp-quote char)))
    (->> (or pair-list (sp--get-pair-list))
         (--filter (= 1 (length (cdr it))))
         (--find (string-match-p regexp (cdr it))))))

;; TODO: this only supports single-char delimiters.  Maybe it should
;; that that way.
(defun sp-skip-closing-pair (&optional last test-only)
  "Automatically skip the closing delimiters of pairs.

If point is inside an inserted pair, and the user only moved
forward with point (that is, only inserted text), if the closing
pair is typed, we shouldn't insert it again but skip forward.  We
call this state \"active sexp\".  The setting
`sp-cancel-autoskip-on-backward-movement' controls when an active
expression become inactive.

For example, pressing ( is followed by inserting the pair (|).  If
we then type \\='word\\=' and follow by ), the result should be (word)|
instead of (word)|).

This behaviour can be customized by various settings of
`sp-autoskip-closing-pair' and `sp-autoskip-opening-pair'.

Additionally, this behaviour can be selectively disabled for
specific pairs by removing their \"autoskip\" action.  You can
achieve this by using `sp-pair' or `sp-local-pair' with
\":actions \\='(:rem autoskip)\"."
  (sp--with-case-sensitive
    (when (or (and (eq sp-autoskip-closing-pair t)
                   sp-pair-overlay-list
                   (sp--get-active-overlay 'pair))
              (memq sp-autoskip-closing-pair '(always always-end)))
      ;; TODO: ugly hack to override 'navigate with 'autoskip.  Each of
      ;; these submodules should set-up their own environment somehow
      ;; and thread it through the entire computation
      (cl-letf (((symbol-function 'sp--get-allowed-stringlike-list)
                 (lambda ()
                   (--filter (and (sp--do-action-p (car it) 'autoskip)
                                  (equal (car it) (cdr it))) sp-pair-list))))
        ;; these two are pretty hackish ~_~
        (cl-labels ((get-sexp
                     (last)
                     (delete-char -1)
                     (insert " ")
                     (prog1 (sp-get-sexp)
                       (delete-char -1)
                       (insert last)))
                    (get-enclosing-sexp
                     (last)
                     (delete-char -1)
                     (insert " ")
                     (prog1 (sp-get-enclosing-sexp)
                       (delete-char -1)
                       (insert last))))
          (let ((last (or last (sp--single-key-description last-command-event))))
            (-if-let (active-sexp
                      (cond
                       ((-when-let* ((ov (sp--get-active-overlay 'pair))
                                     (op (overlay-get ov 'pair-id))
                                     (cl (cdr (assoc op sp-pair-list))))
                          ;; if the sexp is active, we are inside it.
                          (when (and (= 1 (length op))
                                     (equal last cl))
                            (list :beg (overlay-start ov)
                                  :end (overlay-end ov)
                                  :op op
                                  :cl cl
                                  :prefix ""
                                  :suffix ""))))
                       ((sp--char-is-part-of-stringlike last)
                        ;; a part of closing delimiter is typed. There are four
                        ;; options now:
                        ;; - we are inside the sexp, at its end
                        ;; - we are inside the sexp, somewhere in the middle
                        ;; - we are outside, in front of a sexp
                        ;; - we are outside, somewhere between sexps
                        (cond
                         ((and (sp--looking-at (sp--get-stringlike-regexp))
                               (not (sp--skip-match-p (match-string-no-properties 0)
                                                      (match-beginning 0)
                                                      (match-end 0))))
                          ;; if we're looking at the delimiter, and it is valid in
                          ;; current context, get the sexp.
                          (get-sexp last))
                         ;; here comes the feature when we're somewhere in the
                         ;; middle of the sexp (or outside), if ever supported.
                         ))
                       ((sp--char-is-part-of-closing last)
                        (cond
                         ((and (sp--looking-at (sp--get-closing-regexp))
                               (not (sp--skip-match-p (match-string-no-properties 0)
                                                      (match-beginning 0)
                                                      (match-end 0))))
                          (get-sexp last))
                         ((eq sp-autoskip-closing-pair 'always)
                          (get-enclosing-sexp last))))))
                (if (and active-sexp
                         (equal (sp-get active-sexp :cl) last)
                         (sp--do-action-p (sp-get active-sexp :op) 'autoskip)
                         ;; if the point is inside string and preceded
                         ;; by an odd number of `sp-escape-char's, we
                         ;; should not skip as that would leave the
                         ;; string broken.
                         (or (not (sp-point-in-string))
                             (not (sp-char-is-escaped-p (1- (point))))))
                    (-when-let (re (cond
                                    ((= (point) (sp-get active-sexp :beg))
                                     ;; we are in front of a string-like sexp
                                     (when sp-autoskip-opening-pair
                                       (if test-only t
                                         (delete-char -1)
                                         (forward-char)
                                         (setq sp-last-operation 'sp-skip-closing-pair))))
                                    ((= (point) (sp-get active-sexp :end-in))
                                     (if test-only t
                                       (delete-char 1)
                                       (setq sp-last-operation 'sp-skip-closing-pair)))
                                    ((sp-get active-sexp
                                       (and (> (point) :beg-in)
                                            (< (point) :end-in)))
                                     (if test-only t
                                       (delete-char -1)
                                       (sp-up-sexp nil t)))))
                      (unless (or test-only
                                  sp-buffer-modified-p)
                        (set-buffer-modified-p nil))
                      (unless test-only
                        (sp--run-hook-with-args (sp-get active-sexp :op) :post-handlers 'skip-closing-pair))
                      re)
                  ;; if we can't skip and are in strict mode we must not
                  ;; insert anything if it is a closing character
                  (sp--inhibit-insertion-of-closing-delim last))
              (sp--inhibit-insertion-of-closing-delim last))))))))

(defun sp--inhibit-insertion-of-closing-delim (last)
  "Inhibit insertion of closing delimiter in `smartparens-strict-mode'.

If we are not inserting inside string or a comment, and the LAST
inserted character is closing delimiter for a pair that performs
autoskip, and we can not jump out of its enclosing sexp (i.e. it
does not match), we are not allowed to insert it literally
because it would break the balance; so we delete the
just-inserted character."
  (when (and smartparens-strict-mode
             (-when-let (pair (sp--char-is-part-of-closing
                               last (sp--get-allowed-pair-list)))
               (memq 'autoskip (sp-get-pair (car pair) :actions)))
             (not (sp-point-in-string-or-comment)))
    (delete-char -1)
    (set-buffer-modified-p sp-buffer-modified-p)
    (sp-message :cant-insert-closing-delimiter)
    nil))

(defun sp-delete-pair (&optional arg)
  "Automatically delete opening or closing pair, or both, depending on
position of point.

If the point is inside an empty pair, automatically delete both.  That
is, [(|) turns to [|, [{|} turns to [|.  Can be disabled by setting
`sp-autodelete-pair' to nil.

If the point is behind a closing pair or behind an opening pair delete
it as a whole.  That is, {}| turns to {|, {| turns to |.  Can be
disabled by setting `sp-autodelete-closing-pair' and
`sp-autodelete-opening-pair' to nil.

If the last operation was a wrap and `sp-autodelete-wrap' is
enabled, invoking this function will unwrap the expression, that
is remove the just added wrapping."
  ;; NOTE: Only use delete-char inside this function, so we
  ;; don't activate the advice recursively!

  ;; only activate if argument is 1 (this is 0-th argument of the
  ;; delete-backward-char), otherwise the user wants to delete
  ;; multiple character, so let him do that
  (sp--with-case-sensitive
    (when (and (= arg 1)
               smartparens-mode)
      (if (and sp-autodelete-wrap
               (eq sp-last-operation 'sp-wrap-region))
          (let ((p (point))
                (b (sp-get sp-last-wrapped-region :beg))
                (e (sp-get sp-last-wrapped-region :end))
                (o (sp-get sp-last-wrapped-region :op-l))
                (c (sp-get sp-last-wrapped-region :cl-l)))
            ;; if the last operation was `sp-wrap-region', and we are at
            ;; the position of either opening or closing pair, delete the
            ;; just-inserted pair
            (when (or (= p (+ b o))
                      (= p e))
              (insert "x") ;dummy char to account for the regularly deleted one
              (save-excursion
                (goto-char e)
                (delete-char (- c))
                (goto-char b)
                (delete-char o))
              (setq sp-last-operation 'sp-delete-pair-wrap)))
        (let ((p (point))
              (inside-pair (--first (and (sp--looking-back (sp--strict-regexp-quote (car it)))
                                         (sp--looking-at (concat "[ \n\t]*" (sp--strict-regexp-quote (cdr it)))))
                                    sp-pair-list))
              (behind-pair (--first (sp--looking-back (sp--strict-regexp-quote (cdr it))) sp-pair-list))
              (opening-pair (--first (sp--looking-back (sp--strict-regexp-quote (car it))) sp-pair-list)))

          (cond
           ;; we're just before the closing quote of a string.  If there
           ;; is an opening or closing pair behind the point, remove
           ;; it.  This is only really relevant if the pair ends in the
           ;; same character as string quote.  We almost never want to
           ;; delete it as an autopair (it would "open up the string").
           ;; So, word\"|" and <backspace> should produce word\|" or
           ;; word|" (if \" is autopair) instead of word\|.
           ((and (sp-point-in-string)
                 (not (sp-point-in-string (1+ p)))
                 (sp-point-in-string (1- p))) ;; the string isn't empty
            (cond ;; oh, you ugly duplication :/
             ((and behind-pair sp-autodelete-closing-pair)
              (delete-char (- (1- (length (car behind-pair)))))
              (setq sp-last-operation 'sp-delete-pair-closing))
             ((and opening-pair sp-autodelete-opening-pair)
              (delete-char (- (1- (length (car opening-pair)))))
              (setq sp-last-operation 'sp-delete-pair-opening))))
           ;; we're inside a pair
           ((and inside-pair sp-autodelete-pair)
            (let* ((beg (save-excursion
                          (search-backward (car inside-pair))))
                   (end (save-excursion
                          (search-forward (cdr inside-pair))))
                   (cs (sp--get-context p))
                   (ce (sp--get-context end))
                   (current-sexp (sp-get-sexp)))
              (when (and (or (not (eq cs 'comment)) ;; a => b <=> ~a v b
                             (eq ce 'comment))
                         (eq beg (sp-get current-sexp :beg))
                         (eq end (sp-get current-sexp :end))
                         (equal (sp-get current-sexp :op) (car inside-pair))
                         (equal (sp-get current-sexp :cl) (cdr inside-pair)))
                (delete-char (- end p))
                (delete-char (- (1- (length (car inside-pair)))))
                (setq sp-last-operation 'sp-delete-pair))))
           ;; we're behind a closing pair
           ((and behind-pair sp-autodelete-closing-pair)
            (delete-char (- (1- (length (cdr behind-pair)))))
            (setq sp-last-operation 'sp-delete-pair-closing))
           ;; we're behind an opening pair and there's no closing pair
           ((and opening-pair sp-autodelete-opening-pair)
            (delete-char (- (1- (length (car opening-pair)))))
            (setq sp-last-operation 'sp-delete-pair-opening))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun sp--looking-at (regexp)
  "Like `looking-at', but always case sensitive."
  (sp--with-case-sensitive
    (looking-at regexp)))

(defun sp--looking-at-p (regexp)
  "Like `looking-at-p', but always case sensitive."
  (sp--with-case-sensitive
    (looking-at-p regexp)))

(defun sp--looking-back (regexp &optional limit not-greedy)
  "Return non-nil if text before point matches regular expression REGEXP.

With optional argument LIMIT search only that many characters
backward.  If LIMIT is nil, default to `sp-max-pair-length'.

If optional argument NON-GREEDY is t search for any matching
sequence, not necessarily the longest possible."
  (setq limit (or limit sp-max-pair-length))
  (sp--with-case-sensitive
    (let ((from (max 1 (- (point) limit)))
          (to (point))
          (greedy (not not-greedy))
          has-match)
      (if greedy
          (save-excursion
            (goto-char from)
            (save-match-data
              (while (and (not has-match) (< (point) to))
                ;; don't use looking-at because we can't limit that search
                (if (and (save-excursion (re-search-forward regexp to t))
                         (= (match-end 0) to))
                    (setq has-match (match-data))
                  (forward-char 1))))
            (when has-match
              (set-match-data has-match)
              t))
        (save-excursion
          (not (null (search-backward-regexp (concat "\\(?:" regexp "\\)\\=") from t))))))))

(defun sp--looking-back-p (regexp &optional limit not-greedy)
  "Same as `sp--looking-back' but do not change the match data."
  (save-match-data
    (sp--looking-back regexp limit not-greedy)))

(defun sp--search-backward-regexp (regexp &optional bound noerror count)
  "Works just like `search-backward-regexp', but returns the
longest possible match.  That means that searching for
\"defun|fun\" backwards would return \"defun\" instead of
\"fun\", which would be matched first.

This is an internal function.  Only use this for searching for
pairs!"
  (setq count (or count 1))
  (setq bound (or (sp--get-backward-bound) bound))
  (sp--with-case-sensitive
    (let (r)
      (while (> count 0)
        (when (search-backward-regexp regexp bound noerror)
          (goto-char (match-end 0))
          (if (sp--looking-back regexp)
              (setq r (goto-char (match-beginning 0)))
            (if noerror nil (error "Search failed: %s" regexp))))
        (setq count (1- count)))
      r)))

(defun sp--search-forward-regexp (regexp &optional bound noerror count)
  "Just like `search-forward-regexp', but always case sensitive."
  (setq bound (or (sp--get-forward-bound) bound))
  (sp--with-case-sensitive
    (search-forward-regexp regexp bound noerror count)))

(defun sp--search-forward-in-context (regexp &optional bound noerror count)
  "Just like `sp--search-forward-regexp' but only accept results in same context.

The context at point is considered the reference context."
  (let ((context (sp--get-context))
        (re))
    (--dotimes (or count 1)
      (save-excursion
        (while (and (setq re (sp--search-forward-regexp regexp bound noerror))
                    (not (eq (sp--get-context) context)))))
      (when re (goto-char re)))
    re))

(defun sp--search-backward-in-context (regexp &optional bound noerror count)
  "Just like `sp--search-backward-regexp' but only accept results in same context.

The context at point is considered the reference context."
  (let ((context (sp--get-context))
        (re))
    (--dotimes (or count 1)
      (save-excursion
        (while (and (setq re (sp--search-backward-regexp regexp bound noerror))
                    (not (eq (sp--get-context) context))))
        (when re (goto-char re))))
    re))

(defun sp-get-quoted-string-bounds (&optional point)
  "Return the bounds of the string around POINT.

POINT defaults to `point'.

If the point is not inside a quoted string, return nil."
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    (let ((parse-data (syntax-ppss)))
      (when (nth 3 parse-data)
        (let* ((open (nth 8 parse-data))
               (close (save-excursion
                        (parse-partial-sexp
                         (point) (point-max)
                         nil nil parse-data 'syntax-table)
                        (point))))
          (cons open close))))))

;; TODO: the repeated conditions are ugly, refactor this!
(defun sp-get-comment-bounds ()
  "If the point is inside a comment, return its bounds."
  (when (or (sp-point-in-comment)
            (looking-at "[[:space:]]+\\s<"))
    (let ((open (save-excursion
                  (--when-let (nth 8 (sp--syntax-ppss))
                    (goto-char it))
                  (while (and (not (bobp))
                              (or (when (sp-point-in-comment)
                                    (backward-char 1)
                                    t)
                                  (when (save-excursion
                                          (beginning-of-line)
                                          (looking-at "^[[:space:]]+\\s<"))
                                    (when (>= (forward-line -1) 0)
                                      (end-of-line))
                                    t))))
                  ;; this means we got here by `sp-point-in-comment' condition
                  (unless (and (bobp) (sp-point-in-comment))
                    (forward-char))
                  (point)))
          (close (save-excursion
                   (while (and (not (eobp))
                               (or (sp-point-in-comment)
                                   (looking-at "[[:space:]]+\\s<")))
                     (forward-char 1))
                   (let ((pp (1- (point))))
                     (when (not (or (eobp)
                                    (sp-point-in-comment)
                                    (looking-at "[[:space:]]+\\s<")
                                    (and (eq (char-syntax
                                              (char-after pp)) ?>)
                                         (not (eq (char-after pp) ?\n)))
                                    (/= (logand
                                         (lsh 1 18)
                                         (car (syntax-after pp))) 0)
                                    (/= (logand
                                         (lsh 1 19)
                                         (car (syntax-after pp))) 0)))
                       (backward-char 1)))
                   (point))))
      (cons open close))))

(defun sp--get-string-or-comment-bounds ()
  "Get the bounds of string or comment the point is in."
  (or (sp-get-quoted-string-bounds)
      (sp-get-comment-bounds)))

(defmacro sp--search-and-save-match (search-fn pattern bound res beg end str)
  "Save the last match info."
  `(progn
     (setq ,res (funcall ,search-fn ,pattern ,bound t))
     (when ,res
       (setq ,beg (match-beginning 0))
       (setq ,end (match-end 0))
       (setq ,str (match-string 0)))
     ,res))

(cl-defun sp--skip-match-p (ms mb me
                               &key
                               (global-skip (cdr (--first (memq major-mode (car it)) sp-navigate-skip-match)))
                               (pair-skip (sp-get-pair ms :skip-match)))
  "Return non-nil if this match should be skipped.

This function uses two tests, one specified in
`sp-navigate-skip-match' (this is global setting for all pairs in
given major mode) and by a function specified in :skip-match
property of the pair.

If you are calling this function in a heavy loop, you can supply
the test functions as keyword arguments to speed up the lookup."
  (save-match-data
    (or (when global-skip (funcall global-skip ms mb me))
        (when pair-skip (funcall pair-skip ms mb me)))))

(defmacro sp--valid-initial-delimiter-p (form)
  "Test the last match using `sp--skip-match-p'.  The form should
be a function call that sets the match data."
  (declare (debug (form)))
  (let ((match (make-symbol "match"))
        (pair-skip (make-symbol "pair-skip")))
    `(and ,form
          (let* ((,match (match-string 0))
                 (,pair-skip (or (sp-get-pair ,match :skip-match)
                                 (sp-get-pair (car (--first
                                                    (equal (cdr it) ,match)
                                                    sp-pair-list))
                                              :skip-match))))
            (not (sp--skip-match-p
                  ,match
                  (match-beginning 0)
                  (match-end 0)
                  :pair-skip ,pair-skip))))))

(defun sp--elisp-skip-match (ms mb _me)
  "Function used to test for escapes in lisp modes.

Non-nil return value means to skip the result."
  (and ms
       (> mb 1)
       (save-excursion
         (goto-char mb)
         (save-match-data
           (or (and (sp--looking-back "\\\\" 1 t)
                    ;; it might be a part of ?\\ token
                    (not (sp--looking-back "\\?\\\\\\\\" 3 t)))
               (and (not (sp-point-in-string-or-comment))
                    (sp--looking-back "\\?" 1 t) ;;TODO surely we can do better
                    (not (sp--looking-back "\\\\\\?" 2 t))
                    (not (sp--looking-back "\\s_\\?" 2 t))
                    (not (sp--looking-back "\\sw\\?" 2 t))))))))

(defun sp--backslash-skip-match (ms mb _me)
  (and ms
       (save-excursion
         (goto-char mb)
         (sp--looking-back "\\\\" 1 t))))

;; TODO: since this function is used for all the navigation, we should
;; optimize it a lot! Get some elisp profiler! Also, we should split
;; this into smaller functions (esp. the "first expression search"
;; business)
(defun sp-get-paired-expression (&optional back)
  "Find the nearest balanced pair expression after point.

The expressions considered are those delimited by pairs on
`sp-pair-list'."
  (sp--with-case-sensitive
    (save-excursion
      (let* ((search-fn (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
             (global-skip-fn (cdr (--first (memq major-mode (car it)) sp-navigate-skip-match)))
             (pair-list (sp--get-allowed-pair-list))
             ;; TODO UGLY HACK!!!  When the situation is:
             ;; ..)|;; comment
             ;; the context the point gets is the comment.  But if we
             ;; are searching backward, that is incorrect, because in
             ;; that case we want the context of the closing pair.
             ;; Therefore, if the direction is backward, we need to move
             ;; one point backward, then test the comment/string thing,
             ;; then compute the correct bounds, and then restore the
             ;; point so the search will pick up the )

             ;; However, we need to distinguish the cases where we are
             ;; in comment and trying to get out, and when we are in any
             ;; context and we jump into string (in that case, we should
             ;; report code context!).  For example:
             ;;   "foo"|;bar
             ;; or
             ;;   "foo"|bar
             ;; should both report code context
             ;; and "|(foo)" should report string context.

             ;; Beware the case when we have a string inside a comment, like
             ;;   (foo) ;; bar "baz"| qux
             ;; In this case we want to report comment context even when
             ;; backing into the "" (which however is commented)

             ;; Yet another case is when we are not in a comment but
             ;; directly after one and we search backwards, consider:
             ;;   /* foo bar */|
             ;; in C-like language.  In this case, we want to report the
             ;; context as comment.

             ;; In some languages, special paren syntax with a prefix
             ;; serves to mark strings.  This means that regular
             ;; delimiters, like () are used to delimit strings.  For
             ;; example, in ruby the sequence %w(...) signifies a
             ;; string.  If the point is after such a sequence and we
             ;; are searching back, we must use the string context,
             ;; because the paren is now a string delimiter.  This is
             ;; usually implemented with "string fence" syntax, so we
             ;; will simply check for that.

             ;; Thanks for being consistent at handling syntax bounds Emacs!
             (in-string-or-comment (if back
                                       (let ((in-comment (sp-point-in-comment))
                                             (in-string (sp-point-in-string)))
                                         (save-excursion
                                           (unless (= (point) (point-min))
                                             (backward-char)
                                             (cond
                                              ((eq (car (syntax-after (point))) 15) (point))
                                              (in-comment (when (sp-point-in-comment) (1+ (point))))
                                              ((and (not in-comment) (sp-point-in-comment)) (1+ (point)))
                                              ((or in-comment in-string) (1+ (point)))))))
                                     (when (sp-point-in-string-or-comment) (point))))
             (string-bounds (and in-string-or-comment
                                 (progn
                                   (goto-char in-string-or-comment)
                                   (sp--get-string-or-comment-bounds))))
             (fw-bound (if in-string-or-comment (cdr string-bounds) (point-max)))
             (bw-bound (if in-string-or-comment (car string-bounds) (point-min)))
             s e forward mb me ms r done
             possible-pairs possible-interfering-pairs possible-ops possible-cls)
        (while (and (not done)
                    (sp--search-and-save-match
                     search-fn
                     ;; #556 The regexp we use here might exclude or
                     ;; include extra pairs in case the next match is in
                     ;; a different context.  There's no way to know
                     ;; beforehand where we land, so we need to consider
                     ;; *all* pairs in the search and then re-check with
                     ;; a regexp based on the context of the found pair
                     (sp--get-allowed-regexp
                      ;; use all the pairs!
                      (sp--get-pair-list))
                     (if back bw-bound fw-bound)
                     r mb me ms))
          ;; search for the first opening pair.  Here, only consider tags
          ;; that are allowed in the current context.
          (unless (or (not (save-excursion
                             (if back
                                 (progn
                                   (goto-char me)
                                   (sp--looking-back-p (sp--get-allowed-regexp)))
                               (goto-char mb)
                               (sp--looking-at-p (sp--get-allowed-regexp)))))
                      (sp--skip-match-p ms mb me :global-skip global-skip-fn))
            ;; if the point originally wasn't inside of a string or comment
            ;; but now is, jump out of the string/comment and only search
            ;; the code.  This ensures that the comments and strings are
            ;; skipped if we search inside code.
            (if (and (not in-string-or-comment)
                     (if back
                         ;; When searching back, the point lands on the
                         ;; first character of whatever pair we've found
                         ;; and it is in the proper context, for example
                         ;; "|(foo)"
                         (sp-point-in-string-or-comment)
                       ;; However, when searching forward, the point
                       ;; lands after the last char of the pair so to get
                       ;; its context we must back up one character
                       (sp-point-in-string-or-comment (1- (point)))))
                (-if-let (bounds (sp--get-string-or-comment-bounds))
                    (let ((jump-to (if back (car bounds) (cdr bounds))))
                      (goto-char jump-to)
                      ;; Can't move out of comment because eob, #427
                      (when (eobp)
                        (setq done t)))
                  (setq done t))
              (setq done t))))
        (when r
          (setq possible-pairs (--filter (or (equal ms (car it))
                                             (equal ms (cdr it)))
                                         pair-list))
          (setq possible-ops (-map 'car possible-pairs))
          (setq possible-cls (-map 'cdr possible-pairs))
          (setq pair-list (-difference pair-list possible-pairs))
          (setq possible-interfering-pairs pair-list)
          (while possible-interfering-pairs
            (setq possible-interfering-pairs
                  (--filter (or (-contains? possible-ops (car it))
                                (-contains? possible-cls (cdr it)))
                            pair-list))
            (setq pair-list (-difference pair-list possible-interfering-pairs))
            (setq possible-ops (append possible-ops (-map 'car possible-interfering-pairs)))
            (setq possible-cls (append possible-cls (-map 'cdr possible-interfering-pairs))))
          (when (--any? (equal ms it) possible-ops)
            (setq forward t)
            (setq s mb)
            (when back
              (forward-char (length ms))))
          (when (--any? (equal ms it) possible-cls)
            (setq forward nil)
            (setq e me)
            (when (not back)
              (backward-char (length ms))))
          (let* ((opens (if forward possible-ops possible-cls))
                 (closes (if forward possible-cls possible-ops))
                 (needle (sp--strict-regexp-opt (append possible-ops possible-cls)))
                 (search-fn (if forward 'sp--search-forward-regexp 'sp--search-backward-regexp))
                 (depth 1)
                 (eof (if forward 'eobp 'bobp))
                 (b (if forward fw-bound bw-bound))
                 (open (substring-no-properties ms))
                 (close (substring-no-properties ms))
                 (failure (funcall eof))
                 (skip-match-pair-fns (->> possible-ops
                                           (--mapcat (-when-let (smf (sp-get-pair it :skip-match))
                                                       (list (cons it smf) (cons (sp-get-pair it :close) smf)))))))
            (while (and (> depth 0) (not (funcall eof)))
              (sp--search-and-save-match search-fn needle b r mb me ms)
              (if r
                  (unless (or (and (not in-string-or-comment)
                                   (if forward (save-excursion
                                                 (backward-char)
                                                 (sp-point-in-string-or-comment))
                                     (sp-point-in-string-or-comment)))
                              ;; check the individual pair skipper.  We
                              ;; need to test all the possible-ops,
                              ;; which makes it a bit ugly :/
                              (let ((skip-match-pair-fn
                                     (cdr (--first (equal (car it) ms) skip-match-pair-fns))))
                                (sp--skip-match-p ms mb me :global-skip global-skip-fn :pair-skip skip-match-pair-fn)))
                    (when (--any? (equal ms it) opens) (setq depth (1+ depth)))
                    (when (--any? (equal ms it) closes) (setq depth (1- depth))))
                (unless (minibufferp)
                  (sp-message :unmatched-expression))
                (setq depth -1)
                (setq failure t)))
            (if forward
                (setq e me)
              (setq s mb))
            (setq close (substring-no-properties ms))
            (if (or failure
                    (/= depth 0))
                (progn
                  (unless (minibufferp)
                    (sp-message :unmatched-expression))
                  nil)
              (let ((end-in-cos (sp-point-in-string-or-comment (1- e)))) ;; fix the "point on comment" issue
                (cond
                 ((or (and (sp-point-in-string-or-comment s) (not end-in-cos))
                      (and (not (sp-point-in-string-or-comment s)) end-in-cos))
                  (unless (minibufferp)
                    (sp-message :delimiter-in-string))
                  nil)
                 (t
                  (let* ((op (if forward open close)))
                    (list :beg s
                          :end e
                          :op op
                          :cl (if forward close open)
                          :prefix (sp--get-prefix s op)
                          :suffix (sp--get-suffix e op)))))))))))))

;; TODO: this does not consider unbalanced quotes in comments!!!
(defun sp--find-next-stringlike-delimiter (needle search-fn-f &optional limit skip-fn)
  "Find the next string-like delimiter, considering the escapes
and the skip-match predicate."
  (let (hit match)
    (while (and (not hit)
                (funcall search-fn-f needle limit t))
      (save-match-data
        (setq match (match-string-no-properties 0))
        (unless (or (save-match-data
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (or (sp--looking-back-p "\\\\" 2) ;; assumes \ is always the escape... bad?
                            (and (eq major-mode 'emacs-lisp-mode)
                                 (not (sp-point-in-string))
                                 (sp--looking-back-p "?" 1)))))
                    ;; TODO: HACK: global-skip is hack here!!!
                    (sp--skip-match-p match (match-beginning 0) (match-end 0)
                                      :pair-skip (or skip-fn
                                                     (sp-get-pair match :skip-match))
                                      :global-skip nil))
          (setq hit (match-data)))))
    hit))

(defun sp-get-stringlike-expression (&optional back)
  "Find the nearest string-like expression after point.

String-like expression is expression enclosed with the same
opening and closing delimiter, such as *...*, \"...\", `...` etc."
  (sp--with-case-sensitive
    (save-excursion
      (let ((needle (sp--get-stringlike-regexp))
            (search-fn-f (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
            (search-fn-b (if back 'sp--search-forward-regexp 'sp--search-backward-regexp))
            (count 0)
            m b e skip-match-fn limit ok)
        (when (not (equal needle ""))
          (when (sp--find-next-stringlike-delimiter needle search-fn-f)
            ;; assumes \ is always the escape... bad?
            (setq m (match-string-no-properties 0))
            (setq needle (regexp-quote m))
            (setq skip-match-fn (sp-get-pair m :skip-match))
            (cond
             ((sp-point-in-string)
              (setq limit (sp-get-quoted-string-bounds)))
             ((sp-point-in-comment)
              (setq limit (sp-get-comment-bounds))))
            (save-excursion
              (while (sp--find-next-stringlike-delimiter needle 'search-backward-regexp (car limit) skip-match-fn)
                (setq count (1+ count))))
            (when (= (mod count 2) 0)
              (sp--find-next-stringlike-delimiter needle search-fn-b nil))
            (save-excursion
              (setq ok (sp--find-next-stringlike-delimiter needle 'sp--search-backward-regexp (car limit)))
              (setq e (match-beginning 0)))
            (setq ok (and ok (sp--find-next-stringlike-delimiter needle 'search-forward-regexp (cdr limit))))
            (setq b (match-end 0))
            (when ok
              (let ((mb b) (me e))
                (setq b (min mb me))
                (setq e (max mb me)))
              (list :beg b :end e :op m :cl m :prefix (sp--get-prefix b m) :suffix (sp--get-suffix e m)))))))))

(defun sp--textmode-stringlike-regexp (delimiters &optional direction)
  "Get a regexp matching text-mode string-like DELIMITERS.

Capture group 1 or 2 has the delimiter itself, depending on the
direction (forward, backward).

If DIRECTION is :open, create a regexp matching opening only.

If DIRECTION is :close, create a regexp matching closing only.

If DIRECTION is nil, create a regexp matching both directions."
  (let* ((delims (regexp-opt delimiters))
         (re (concat
              (if (or (not direction)
                      (eq direction :open))
                  (concat "\\(?:" "\\(?:\\`\\|[ \t\n\r]\\)" "\\(" delims "\\)" "[^ \t\n\r]\\)") "")
              (if (not direction) "\\|" "")
              (if (or (not direction)
                      (eq direction :close))
                  (concat "\\(?:[^ \t\n\r]" "\\(" delims "\\)" "\\(?:[ \t\n\r[:punct:]]\\|\\'\\)" "\\)") ""))))
    re))

(defun sp--find-next-textmode-stringlike-delimiter (needle search-fn-f &optional limit)
  "Find the next string-like delimiter, considering the escapes
and the skip-match predicate."
  (let (hit)
    (while (and (not hit)
                (funcall search-fn-f needle limit t))
      (save-match-data
        (let* ((group (if (match-string 1) 1 2))
               (match (match-string-no-properties group))
               (mb (match-beginning group))
               (me (match-end group))
               (skip-fn (sp-get-pair match :skip-match)))
          (unless (sp--skip-match-p match mb me :pair-skip skip-fn :global-skip nil)
            (setq hit (list match (if (= group 1) :open :close)))))))
    hit))

(defun sp-get-textmode-stringlike-expression (&optional back)
  "Find the nearest text-mode string-like expression.

If BACK is non-nil search in the backwards direction.

Text-mode string-like expression is one where the delimiters must
be surrounded by whitespace from the outside.  For example,

foo *bar* baz

is a valid expression enclosed in ** pair, but

foo*bar*baz  OR  foo *bar*baz  OR  foo*bar* baz

are not.

This is the case in almost every markup language, and so we will
adjust the parsing to only consider such pairs as delimiters.
This makes the parsing much faster as it transforms the problem
to non-stringlike matching and we can use a simple
counting (stack) algorithm."
  (save-excursion
    (let ((restart-from (point))
          (bounds (or (sp-get-comment-bounds)
                      (cons (point-min) (point-max))))
          hit re)
      (while (not hit)
        (goto-char restart-from)
        (save-excursion
          (ignore-errors
            (if back (forward-char) (backward-char)))
          (let* ((delimiters (-map 'car (sp--get-allowed-stringlike-list)))
                 (needle (sp--textmode-stringlike-regexp delimiters))
                 (search-fn-f (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
                 (limit-f (if (not back) (cdr bounds) (car bounds))))
            (-if-let ((delim type) (sp--find-next-textmode-stringlike-delimiter needle search-fn-f limit-f))
                (let ((search-fn (if (eq type :open) 'sp--search-forward-regexp 'sp--search-backward-regexp))
                      (limit (if (eq type :open) (cdr bounds) (car bounds)))
                      (needle (sp--textmode-stringlike-regexp (list delim) (if (eq type :open) :close :open))))
                  (setq restart-from (point))
                  ;; this adjustments are made because elisp regexp
                  ;; can't do lookahead assertions... so we match and
                  ;; then back up.
                  (ignore-errors
                    (when (and (not back) (eq type :open)) (backward-char (1+ (length delim))))
                    (when (and (not back) (eq type :close) (not (eobp))) (backward-char 1))
                    (when (and back (eq type :close)) (forward-char (1+ (length delim))))
                    (when (and back (eq type :open) (not (bobp))) (forward-char 1)))
                  (let ((other-end (point)))
                    (when (sp--find-next-textmode-stringlike-delimiter needle search-fn limit)
                      ;; Beware, we also need to test the beg/end of
                      ;; buffer, because we have that variant in the
                      ;; regexp.  In that case the match does not
                      ;; consume anything and we needn't do any
                      ;; correction.
                      (let* ((this-end (if (eq type :open)
                                           (max (point-min) (if (eobp) (point) (1- (point))))
                                         (min (point-max) (if (bobp) (point) (1+ (point))))))
                             (b (min this-end other-end))
                             (e (max this-end other-end)))
                        (setq re (list :beg b
                                       :end e
                                       :op delim
                                       :cl delim
                                       :prefix (sp--get-prefix b delim) :suffix (sp--get-suffix e delim)))
                        (setq hit t)
                        ;; We ignore matches that contain two
                        ;; consecutive newlines, as that usually means
                        ;; there's a new paragraph somewhere inbetween
                        ;; TODO: make this customizable
                        (when (sp-get re
                                (save-excursion
                                  (goto-char :beg)
                                  (re-search-forward "\n\n\\|\r\r" :end t)))
                          (setq re nil)
                          (setq hit nil))))))
              (setq hit :no-more)))))
      re)))

(defun sp-get-string-or-nested-string (&optional back)
  "Get a string with `sp-get-string' or `sp-get-stringlike-expression'.

It might happen that the string delimiter we are looking at is
nested inside another string delimited by string fences (for
example nested \" and ' in python).  In this case we can't use
`sp-get-string' parser because it would pick up the outer string.

So if we are inside a string and `syntax-ppss' returns t as
delimiter we need to use `sp-get-stringlike-expression'.  The
same situation happens when the character following the point has
syntax class 7 (string) or char syntax 34 (same thing as syntax
class but less general), but the character different from what
`syntax-ppss' returns as the outer string delimiter."
  (if (let ((in-string (sp-point-in-string)))
        (or (eq t in-string)
            (and (not (eq in-string nil))
                 (not (eq in-string (char-after))))))
      (save-excursion
        (save-restriction
          (widen)
          (-let (((beg . end) (sp-get-quoted-string-bounds)))
            (narrow-to-region beg end))
          (sp-get-stringlike-expression back)))
    (sp-get-string back)))

(defun sp-use-textmode-stringlike-parser-p ()
  "Test if we should use textmode stringlike parser or not."
  (let ((modes (-filter 'symbolp sp-navigate-use-textmode-stringlike-parser))
        (derived (-map 'cdr (-remove 'symbolp sp-navigate-use-textmode-stringlike-parser))))
    (or (--any? (eq major-mode it) modes)
        (apply 'derived-mode-p derived))))

(defun sp-get-stringlike-or-textmode-expression (&optional back delimiter)
  "Return a stringlike expression using stringlike or textmode parser.

DELIMITER is a candidate in case we performed a search before
calling this function and we know it's the closest string
delimiter to try.  This is purely a performance hack, do not rely
on it when calling directly."
  (if (sp-use-textmode-stringlike-parser-p)
      (sp-get-textmode-stringlike-expression back)
    ;; performance hack. If the delimiter is a character in
    ;; syntax class 34, grab the string-like expression using
    ;; `sp-get-string'
    (if (and delimiter
             (= (length delimiter) 1)
             (eq (char-syntax (string-to-char delimiter)) 34))
        (sp-get-string-or-nested-string back)
      (sp-get-stringlike-expression back))))

(defun sp-get-expression (&optional back)
  "Find the nearest balanced expression of any kind.

For markup and text modes a special, more efficient stringlike
parser is available, see `sp-get-textmode-stringlike-expression'.
By default, this is enabled in all modes derived from
`text-mode'.  You can change it by customizing
`sp-navigate-use-textmode-stringlike-parser'."
  (let ((pre (sp--get-allowed-regexp))
        (sre (sp--get-stringlike-regexp))
        (search-fn (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
        (ps (if back (1- (point-min)) (1+ (point-max))))
        (ss (if back (1- (point-min)) (1+ (point-max))))
        (string-delim nil))
    (setq ps (if (equal pre "") ps
               (or (save-excursion (funcall search-fn pre nil t)) ps)))
    (setq ss (if (equal sre "") ss
               (or (--when-let (save-excursion
                                 (sp--find-next-stringlike-delimiter sre search-fn))
                     (setq string-delim (match-string 0))
                     (save-match-data
                       (set-match-data it)
                       (if back (match-beginning 0) (match-end 0))))
                   ss)))
    ;; TODO: simplify this logic somehow... (this really depends
    ;; on a rewrite of the core parser logic: separation of "find
    ;; the valid opening" and "parse it")

    ;; Here, we sacrifice readability for performance.  Because we
    ;; only use regexp to look forward for the closest pair, it
    ;; might occasionally happen that what we picked in fact
    ;; *can't* form a pair and it returns error (for example, it
    ;; is an unclosed pair or a quote between words like'so, which
    ;; doesn't form a pair).  In such a case, or when the pair
    ;; found is further than the other possible pair type (for
    ;; example, we think we should parse stringlike, but we skip
    ;; the first occurrence and the next one is only after a
    ;; regular pair, which we should've picked instead), we must
    ;; try the other parser as well.
    (-let (((type . re) (if (or (and (not back) (< ps ss))
                                (and back (> ps ss)))
                            (cons :regular (sp-get-paired-expression back))
                          (cons :string (sp-get-stringlike-or-textmode-expression back string-delim)))))
      (when re
        (sp-get re
          (cond
           ;; If the returned sexp is regular, but the
           ;; to-be-tried-string-expression is before it, we try
           ;; to parse it as well, it might be a complete sexp in
           ;; which case it should be returned.
           ((and (eq type :regular)
                 (or (and (not back) (< ss :beg))
                     (and back (> ss :end))))
            (or (sp-get-stringlike-or-textmode-expression back string-delim) re))
           ((and (eq type :string)
                 (or (and (not back) (< ps :beg))
                     (and back (> ps :end))))
            (or (sp-get-paired-expression back) re))
           (t re)))))))

(defun sp-get-sexp (&optional back)
  "Find the nearest balanced expression that is after (before) point.

Search backward if BACK is non-nil.  This also means, if the
point is inside an expression, this expression is returned.

If `major-mode' is member of `sp-navigate-consider-sgml-tags',
sgml tags will also be considered as sexps in current buffer.

If the search starts outside a comment, all subsequent comments
are skipped.

If the search starts inside a string or comment, it tries to find
the first balanced expression that is completely contained inside
the string or comment.  If no such expression exist, a warning is
raised (for example, when you comment out imbalanced expression).
However, if you start a search from within a string and the next
complete sexp lies completely outside, this is returned.  Note
that this only works in modes where strings and comments are
properly defined via the syntax tables.

The return value is a plist with following keys:

  :beg    - point in the buffer before the opening
  delimiter (ignoring prefix)
  :end    - point in the buffer after the closing delimiter
  :op     - opening delimiter
  :cl     - closing delimiter
  :prefix - expression prefix
  :suffix - expression suffix

However, you should never access this structure directly as it is
subject to change.  Instead, use the macro `sp-get' which also
provide shortcuts for many commonly used queries (such as length
of opening/closing delimiter or prefix)."
  (sp--maybe-init)
  (sp--with-case-sensitive
    (cond
     (sp-prefix-tag-object
      (sp-get-sgml-tag back))
     (sp-prefix-pair-object
      (sp-get-paired-expression back))
     ((memq major-mode sp-navigate-consider-sgml-tags)
      (let ((paired (sp-get-expression back)))
        (if (and paired
                 (equal "<" (sp-get paired :op)))
            ;; if the point is inside the tag delimiter, return the pair.
            (if (sp-get paired (and (<= :beg-in (point)) (>= :end-in (point))))
                paired
              ;; if the tag can't be completed, we can at least return
              ;; the <> pair
              (or (sp-get-sgml-tag back) paired))
          ;; we can still try the tag if the first < or > is closer than
          ;; the pair.  This is a bit too complicated... seems like a
          ;; more clever solution would be needed in the future, esp if
          ;; we add the python hack.
          (cond
           ((and (not back)
                 (< (save-excursion
                      (or (search-forward "<" nil t) (point-max)))
                    (or (sp-get paired :beg) (point-max))))
            (or (sp-get-sgml-tag) paired))
           ((and back
                 (> (save-excursion
                      (or (search-backward ">" nil t) (point-min)))
                    (or (sp-get paired :end) (point-max))))
            (or (sp-get-sgml-tag t) paired))
           (t paired)))))
     (t (sp-get-expression back)))))

(defun sp--get-hybrid-sexp-beg ()
  "Get the beginning of hybrid sexp.
See `sp-get-hybrid-sexp' for definition."
  (save-excursion
    (cl-labels ((indent-or-beg-of-line
                 (lb)
                 (if (sp-point-in-blank-line)
                     lb
                   (back-to-indentation)
                   (point))))
      (let ((p (progn (when (sp-point-in-symbol) (sp-backward-sexp)) (point)))
            (lb (line-beginning-position))
            (cur (--if-let (save-excursion (sp-backward-sexp)) it (list :end 0))) ;hack
            last)
        (if (< (sp-get cur :end) lb)
            ;; if the line is not empty, we move the beg to the indent
            (indent-or-beg-of-line lb)
          (while (sp-get cur
                   (and cur
                        (> :end lb)
                        (<= :end p)))
            (setq last cur)
            (setq cur (sp-backward-sexp)))
          (if last
              (sp-get last :beg-prf)
            ;; happens when there is no sexp before the opening delim of
            ;; the enclosing sexp.  In case it is on line above, we take
            ;; the maximum wrt lb.
            (sp-get cur (max :beg-in (indent-or-beg-of-line lb)))))))))

(defun sp--narrow-to-line ()
  "Narrow to the current line."
  (narrow-to-region (line-beginning-position) (line-end-position)))

(defun sp--get-hybrid-sexp-end ()
  "Get the end of hybrid sexp.
See `sp-get-hybrid-sexp' for definition."
  (save-excursion
    (cl-labels ((skip-prefix-backward
                 (p)
                 (save-excursion
                   (goto-char p)
                   (save-restriction
                     (sp--narrow-to-line)
                     (skip-syntax-backward " .")
                     (point)))))
      (let ((p (progn (when (sp-point-in-symbol) (sp-backward-sexp)) (point)))
            (le (line-end-position))
            (cur (--if-let (save-excursion (sp-forward-sexp)) it (list :beg (1+ (point-max))))) ;hack
            last)
        (if (> (sp-get cur :beg) le)
            (if (sp-point-in-blank-line) le (skip-prefix-backward le))
          (while (sp-get cur
                   (and cur
                        (< :beg le)
                        (>= :beg p)))
            (setq last cur)
            (setq cur (sp-forward-sexp)))
          (let ((r (skip-prefix-backward
                    (if last
                        (sp-get last :end)
                      ;; happens when there is no sexp before the closing delim of
                      ;; the enclosing sexp.  In case it is on line below, we take
                      ;; the minimum wrt le.
                      (sp-get cur (min :end-in le))))))
            (goto-char r)
            ;; fix the situation when point ends in comment
            (cond
             ((sp-point-in-comment)
              (if (= (line-number-at-pos p)
                     (line-number-at-pos r))
                  (line-end-position)
                (goto-char p)
                (line-end-position)))
             (t r))))))))

(defun sp--get-hybrid-suffix (p)
  "Get the hybrid sexp suffix, which is any punctuation after
the end, possibly preceded by whitespace."
  (save-excursion
    (goto-char p)
    (buffer-substring-no-properties
     p
     (save-restriction
       (sp--narrow-to-line)
       (skip-syntax-forward " ")
       (if (not (looking-at "\\s."))
           p
         (skip-syntax-forward ".")
         (point))))))

(defun sp-get-hybrid-sexp ()
  "Return the hybrid sexp around point.

A hybrid sexp is defined as the smallest balanced region containing
the point while not expanding further than the current line.  That is,
any hanging sexps will be included, but the expansion stops at the
enclosing list boundaries or line boundaries."
  (let ((end (sp--get-hybrid-sexp-end)))
    (list :beg (sp--get-hybrid-sexp-beg)
          :end end
          :op ""
          :cl ""
          :prefix ""
          :suffix (sp--get-hybrid-suffix end))))

(defun sp-get-enclosing-sexp (&optional arg)
  "Return the balanced expression that wraps point at the same level.

With ARG, ascend that many times.  This function expects a positive
argument."
  (setq arg (or arg 1))
  (save-excursion
    (let ((n arg)
          (ok t)
          (okr))
      (while (and (> n 0) ok)
        (setq ok t)
        (setq okr nil)
        ;; if we are inside string, get the string bounds and "string
        ;; expression"
        (when (sp-point-in-string)
          (setq okr (sp-get-string)))
        ;; get the "normal" expression defined by pairs
        (let ((p (point)))
          (setq ok (sp-get-sexp))
          (cond
           ((and ok (= (sp-get ok :beg) p))
            (goto-char (sp-get ok :end))
            (setq n (1+ n)))
           ((and ok (< (sp-get ok :beg) p))
            (goto-char (sp-get ok :end)))
           (t
            (while (and ok (>= (sp-get ok :beg) p))
              (setq ok (sp-get-sexp))
              (when ok (goto-char (sp-get ok :end)))))))
        ;; if the pair expression is enclosed inside a string, return
        ;; the pair expression, otherwise return the string expression
        (when okr
          (unless (and ok
                       (sp-compare-sexps ok okr >=)
                       (sp-compare-sexps ok okr <= :end))
            (setq ok okr)
            (goto-char (sp-get ok :end))))
        (setq n (1- n)))
      (if (not (and (not ok)
                    sp-navigate-comments-as-sexps))
          ok
        (when (sp-point-in-comment)
          (let* ((cb (sp-get-comment-bounds))
                 (b (save-excursion
                      (goto-char (car cb))
                      (sp-skip-backward-to-symbol t)
                      (point)))
                 (e (save-excursion
                      (goto-char (cdr cb))
                      (sp-skip-forward-to-symbol t)
                      (point))))
            (list :beg b :end e :op "" :cl "" :prefix sp-comment-char)))))))

(defun sp-get-list-items (&optional lst)
  "Return the information about expressions inside LST.

LST should be a data structure in format as returned by
`sp-get-sexp'.

The return value is a list of such structures in order as they
occur inside LST describing each expression, with LST itself
prepended to the front.

If LST is nil, the list at point is used (that is the list
following point after `sp-backward-up-sexp' is called)."
  (let ((r nil))
    (save-excursion
      (unless lst
        (setq lst (sp-backward-up-sexp)))
      (when lst
        (goto-char (sp-get lst :beg-in))
        (while (< (point) (sp-get lst :end))
          (!cons (sp-forward-sexp) r))
        (cons lst (nreverse (cdr r)))))))

(cl-defun sp--get-prefix (&optional (p (point)) op)
  "Get the prefix of EXPR.

Prefix is any continuous sequence of characters in \"expression
prefix\" syntax class.  You can also specify a set of syntax code
characters or a regexp for a specific major mode.  See
`sp-sexp-prefix'.

The point is expected to be at the opening delimiter of the sexp
and the prefix is searched backwards.

If the prefix property is defined for OP, the associated regexp
is used to retrieve the prefix instead of the global setting."
  (sp--with-case-sensitive
    (save-excursion
      (goto-char p)
      (let* ((pref (sp-get-pair op :prefix))
             (prefix
              (if pref
                  (if (sp--looking-back pref sp-max-prefix-length)
                      (match-string-no-properties 0)
                    "")
                (-if-let (mmode-prefix (cdr (assoc major-mode sp-sexp-prefix)))
                    (cond
                     ((and (eq (car mmode-prefix) 'regexp)
                           (sp--looking-back (cadr mmode-prefix)))
                      (match-string-no-properties 0))
                     ((eq (car mmode-prefix) 'syntax)
                      (skip-syntax-backward (cadr mmode-prefix))
                      (buffer-substring-no-properties (point) p))
                     (t ""))
                  (backward-prefix-chars)
                  (buffer-substring-no-properties (point) p)))))
        ;; do not consider it a prefix if it matches some opening or
        ;; closing delimiter which is allowed for parsing in current
        ;; context
        (goto-char p)
        (if (and (< 0 (length prefix))
                 (or (sp--do-action-p prefix 'navigate)
                     (sp--do-action-p
                      (car (--first (equal (cdr it) prefix)
                                    sp-pair-list))
                      'navigate)))
            ""
          prefix)))))

(cl-defun sp--get-suffix (&optional (p (point)) op)
  "Get the suffix of EXPR.

Suffix is any continuous sequence of characters in the
\"punctuation suffix\" syntax class.  You can also specify a set
of syntax code characters or a regexp for a specific major mode.
See `sp-sexp-suffix'.

If the suffix property is defined for OP, the associated regexp
is used to retrieve the suffix instead of the global setting."
  (sp--with-case-sensitive
    (save-excursion
      (goto-char p)
      (let* ((suff (sp-get-pair op :suffix))
             (suffix
              (if suff
                  (if (sp--looking-at suff)
                      (match-string-no-properties 0)
                    "")
                (-if-let (mmode-suffix (cdr (assoc major-mode sp-sexp-suffix)))
                    (cond
                     ((and (eq (car mmode-suffix) 'regexp)
                           (sp--looking-at (cadr mmode-suffix)))
                      (match-string-no-properties 0))
                     ((eq (car mmode-suffix) 'syntax)
                      (skip-syntax-forward (cadr mmode-suffix))
                      (buffer-substring-no-properties p (point)))
                     (t ""))
                  (skip-syntax-forward ".")
                  (buffer-substring-no-properties p (point))))))
        ;; do not consider it a suffix if it matches some opening or
        ;; closing delimiter which is allowed for parsing in current
        ;; context
        (goto-char p)
        (if (and (< 0 (length suffix))
                 (or (sp--do-action-p suffix 'navigate)
                     (sp--do-action-p
                      (car (--first (equal (cdr it) suffix)
                                    sp-pair-list))
                      'navigate)))
            ""
          suffix)))))

(defun sp-get-symbol (&optional back)
  "Find the nearest symbol that is after point, or before point if BACK is non-nil.

This also means, if the point is inside a symbol, this symbol is
returned.  Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (sp--maybe-init)
  (let (b e last-or-first)
    (save-excursion
      (if back
          (progn
            (sp-skip-backward-to-symbol)
            (when (= (point) (point-min)) (setq last-or-first t))
            (sp-forward-symbol -1)
            (setq b (point))
            (sp-forward-symbol 1)
            (setq e (point)))
        (sp-skip-forward-to-symbol)
        (when (= (point) (point-max)) (setq last-or-first t))
        (sp-forward-symbol 1)
        (setq e (point))
        (sp-forward-symbol -1)
        (setq b (point))))
    (unless last-or-first
      (list :beg b :end e :op "" :cl "" :prefix (sp--get-prefix b) :suffix (sp--get-suffix e)))))

(defun sp--get-string (bounds)
  "Return the `sp-get-sexp' format info about the string.

This function simply transforms BOUNDS, which is a cons (BEG
. END) into format compatible with `sp-get-sexp'."
  (let* ((op (char-to-string (char-after (car bounds))))
         (cl (char-to-string (char-before (cdr bounds)))))
    ;; if the closing and opening isn't the same token, we should
    ;; return nil
    (when (equal op cl)
      (list :beg (car bounds)
            :end (cdr bounds)
            :op cl
            :cl cl
            :prefix (sp--get-prefix (car bounds) op)
            :suffix (sp--get-suffix (cdr bounds) cl)))))

(defun sp-get-string (&optional back)
  "Find the nearest string after point, or before if BACK is non-nil.

This also means if the point is inside a string, this string is
returned.  If there are another symbols between point and the
string, nil is returned.  That means that this function only
return non-nil if the string is the very next meaningful
expression.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (sp--maybe-init)
  (if (sp-point-in-comment)
      (sp-get-stringlike-expression back)
    (if (sp-point-in-string)
        (let ((r (sp-get-quoted-string-bounds)))
          (sp--get-string r))
      (save-excursion
        (sp-skip-into-string back)
        (--when-let (sp-get-quoted-string-bounds)
          (sp--get-string it))))))

(defun sp-get-whitespace ()
  "Get the whitespace around point.

Whitespace here is defined as any of the characters: space, tab
and newline."
  (list :beg (save-excursion (skip-chars-backward " \t\n") (point))
        :end (save-excursion (skip-chars-forward " \t\n") (point))
        :op ""
        :cl ""
        :prefix ""
        :suffix ""))

(defun sp--sgml-get-tag-name (match)
  (let ((sub (if (equal "/" (substring match 1 2))
                 (substring match 2)
               (substring match 1))))
    (car (split-string sub "\\( \\|>\\)"))))

(defun sp--sgml-opening-p (tag)
  (not (equal "/" (substring tag 1 2))))

(defun sp--sgml-ignore-tag (tag)
  "Return non-nil if tag should be ignored in search, nil otherwise."
  (member tag '("!--" "!DOCTYPE")))

(defun sp-get-sgml-tag (&optional back)
  (sp--maybe-init)
  (sp--with-case-sensitive
    (save-excursion
      (let ((search-fn (if (not back) 'sp--search-forward-regexp 'search-backward-regexp))
            tag tag-name needle
            open-start open-end
            close-start close-end)
        (when (and (funcall search-fn "</?.*?\\s-?.*?>" nil t)
                   (progn
                     (setq tag (substring-no-properties (match-string 0)))
                     (setq tag-name (sp--sgml-get-tag-name tag))
                     (not (sp--sgml-ignore-tag tag-name))))
          (setq needle (concat "</?" tag-name))
          (let* ((forward (sp--sgml-opening-p tag))
                 (search-fn (if forward 'sp--search-forward-regexp 'search-backward-regexp))
                 (depth 1))
            (save-excursion
              (if (not back)
                  (progn
                    (setq open-end (point))
                    (search-backward-regexp "<" nil t)
                    (setq open-start (point)))
                (setq open-start (point))
                (search-forward-regexp ">" nil t)
                (setq open-end (point))))
            (cond
             ((and (not back) (not forward))
              (goto-char (match-beginning 0)))
             ((and back forward)
              (goto-char (match-end 0))))
            (while (> depth 0)
              (if (funcall search-fn needle nil t)
                  (if (sp--sgml-opening-p (match-string 0))
                      (if forward (setq depth (1+ depth)) (setq depth (1- depth)))
                    (if forward (setq depth (1- depth)) (setq depth (1+ depth))))
                (setq depth -1)))
            (if (eq depth -1)
                (progn (sp-message :no-matching-tag) nil)
              (save-excursion
                (if forward
                    (progn
                      (setq close-start (match-beginning 0))
                      (search-forward-regexp ">" nil t)
                      (setq close-end (point)))
                  (setq close-start (point))
                  (search-forward-regexp ">" nil t)
                  (setq close-end (point))))
              (let ((op (buffer-substring-no-properties open-start open-end))
                    (cl (buffer-substring-no-properties close-start close-end)))
                (list :beg (if forward open-start close-start)
                      :end (if forward close-end open-end)
                      :op (if forward op cl)
                      :cl (if forward cl op)
                      :prefix ""
                      :suffix "")))))))))

(defun sp--end-delimiter-closure (pairs pair-list)
  "Compute the \"end-delimiter\" closure of set PAIRS.

PAIRS can be:
- single pair ID
- single cons with opening and closing delimiter
- list of pair IDs
- list of conses of opening and closing delimiters

For example, if we have pairs (if . end) and (def . end), then
the closure of \"if\" pair are both of these because they share
the closing delimiter.  Therefore, in the navigation functions,
both have to be considered by the parser."
  (let* ((pairs (-flatten (list pairs)))
         (pairs (if (consp (car pairs)) (-map 'car pairs) pairs))
         (pairs (--filter (member (car it) pairs) pair-list))
         (closure (-mapcat
                   (lambda (x)
                     (--filter (equal (cdr x) (cdr it)) pair-list))
                   pairs)))
    closure))

(defun sp-restrict-to-pairs (pairs function)
  "Call the FUNCTION restricted to PAIRS.

PAIRS is either an opening delimiter of a list of opening
delimiters.

FUNCTION is a function symbol.

For example, you can restrict function `sp-down-sexp' to the
pair (\"{\" . \"}\") for easier navigation of blocks in C-like
languages."
  (let* ((pairs (-flatten (list pairs)))
         (new-pairs (--filter (member (car it) pairs) sp-pair-list))
         (sp-pair-list (sp--end-delimiter-closure new-pairs sp-pair-list)))
    (call-interactively function)))

(defun sp-restrict-to-object (object function)
  "Call the FUNCTION restricted to OBJECT.

OBJECT is one of following symbols (you have to quote it!):
- `sp-prefix-pair-object'
- `sp-prefix-tag-object'
- `sp-prefix-symbol-object'

This function will enable this prefix and then call FUNCTION.

FUNCTION is a function symbol.

This function is equivalent to doing:

  (let ((sp-prefix-object t))
    (call-interactively function))

For example, you can restrict function `sp-forward-sexp' to just
the pairs for easier navigation of blocks in C-like languages."
  (cl-letf (((symbol-value object) t))
    (call-interactively function)))

;; TODO: add shorter alias?
(defun sp-restrict-to-pairs-interactive (pairs function)
  "Return an interactive lambda that calls FUNCTION restricted to PAIRS.

See `sp-restrict-to-pairs'.

This function implements a \"decorator pattern\", that is, you
can apply another scoping function to the output of this function
and the effects will added together. In particular, you can
combine it with:

- `sp-restrict-to-object-interactive'

You can also bind the output of this function directly to a key, like:

  (global-set-key (kbd ...)
    (sp-restrict-to-pairs-interactive \"{\" \\='sp-down-sexp))

This will be a function that descends down only into { } pair,
ignoring all others."
  (lambda (&optional _arg)
    (interactive "*P")
    (sp-restrict-to-pairs pairs function)))

(defun sp-restrict-to-object-interactive (object function)
  "Return an interactive lambda that calls FUNCTION restricted to OBJECT.

See `sp-restrict-to-object'.

This function implements a \"decorator pattern\", that is, you
can apply another scoping function to the output of this function
and the effects will added together. In particular, you can
combine it with:

- `sp-restrict-to-pairs-interactive'

You can also bind the output of this function directly to a key, like:

  (global-set-key (kbd ...) (sp-restrict-to-object-interactive
                             \\='sp-prefix-pair-object
                             \\='sp-forward-sexp))

This will be a function that navigates only by using paired
expressions, ignoring strings and sgml tags."
  (lambda (&optional _arg)
    (interactive "*P")
    (sp-restrict-to-object object function)))

(defun sp-prefix-tag-object (&optional _arg)
  "Read the command and invoke it on the next tag object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-tag-object] \\[sp-forward-sexp]\" will move two tag
expressions forward, ignoring possible symbols or paired
expressions inbetween.

Tag object is anything delimited by sgml tag."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-tag-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-pair-object (&optional _arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-pair-object] \\[sp-forward-sexp]\" will move two paired
expressions forward, ignoring possible symbols inbetween.

Pair object is anything delimited by pairs from `sp-pair-list'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-pair-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-symbol-object (&optional _arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-symbol-object] \\[sp-forward-sexp]\" will move two symbols
forward, ignoring any structure.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-symbol-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-save-excursion (&optional _arg)
  "Execute the command keeping the point fixed.

If you specify a regular emacs prefix argument this is passed to
the executed command."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd)))
    (sp--keep-indentation
      (save-excursion
        (if (commandp com)
            (call-interactively com)
          (execute-kbd-macro cmd))))))

(defun sp-get-thing (&optional back)
  "Find next thing after point, or before if BACK is non-nil.

Thing is either symbol (`sp-get-symbol'),
string (`sp-get-string') or balanced expression recognized by
`sp-get-sexp'.

If `sp-navigate-consider-symbols' is nil, only balanced
expressions are considered."
  (sp--maybe-init)
  (sp--with-case-sensitive
    (cond
     (sp-prefix-tag-object (sp-get-sgml-tag back))
     (sp-prefix-pair-object (sp-get-paired-expression back))
     (sp-prefix-symbol-object (sp-get-symbol back))
     (t
      (if back
          (if (not sp-navigate-consider-symbols)
              (sp-get-sexp t)
            (save-excursion
              (cond
               ((sp-point-in-empty-string)
                (sp-get-string t))
               (t
                (sp-skip-backward-to-symbol t nil t)
                (cond
                 ;; this is an optimization, we do not need to look up
                 ;; the "pair" expression first. If this fails, follow
                 ;; up with regular sexps
                 ((and (memq major-mode sp-navigate-consider-sgml-tags)
                       (or (sp--looking-back ">")
                           ;; sp-skip-backward-to-symbol moves the
                           ;; point to the end of an element name in
                           ;; js2-jsx-mode
                           (looking-at ">"))
                       (sp-get-sgml-tag t)))
                 ((sp--valid-initial-delimiter-p (sp--looking-back (sp--get-closing-regexp (sp--get-allowed-pair-list)) nil))
                  (sp-get-sexp t))
                 ((sp--valid-initial-delimiter-p (sp--looking-back (sp--get-opening-regexp (sp--get-allowed-pair-list)) nil))
                  (sp-get-sexp t))
                 ((and (eq (syntax-class (syntax-after (1- (point)))) 7)
                       (not (sp-char-is-escaped-p (1- (point)))))
                  (sp-get-string-or-nested-string t))
                 ((sp--valid-initial-delimiter-p (sp--looking-back (sp--get-stringlike-regexp) nil))
                  (sp-get-expression t))
                 ;; We might be somewhere inside the prefix of the
                 ;; sexp after the point.  Since the prefix can be
                 ;; specified as regexp and not syntax class, it might
                 ;; itself by a symbol which would invalidly get
                 ;; picked here.
                 (t (-when-let (sym (sp-get-symbol t))
                      (save-excursion
                        (sp-get sym (goto-char :end))
                        (if (sp--valid-initial-delimiter-p (sp--looking-at (sp--get-opening-regexp (sp--get-allowed-pair-list))))
                            (let* ((ms (match-string 0))
                                   (pref (sp--get-prefix (point) ms)))
                              (if (and pref
                                       (not (equal pref "")))
                                  (sp-get-sexp t)
                                sym))
                          sym)))))))))
        (if (not sp-navigate-consider-symbols)
            (sp-get-sexp nil)
          (save-excursion
            (cond
             ((sp-point-in-empty-string)
              (sp-get-string nil))
             (t
              (sp-skip-forward-to-symbol t nil t)
              (cond
               ((and (memq major-mode sp-navigate-consider-sgml-tags)
                     (or (looking-at "<")
                         ;; sp-skip-forward-to-symbol moves the point
                         ;; to the beginning of an element name in
                         ;; js2-jsx-mode
                         (and (sp--looking-back "</?" (- (point) 2))
                              (goto-char (match-beginning 0))))
                     (sp-get-sgml-tag)))
               ((sp--valid-initial-delimiter-p (sp--looking-at (sp--get-opening-regexp (sp--get-allowed-pair-list))))
                (sp-get-sexp nil))
               ((sp--valid-initial-delimiter-p (sp--looking-at (sp--get-closing-regexp (sp--get-allowed-pair-list))))
                (sp-get-sexp nil))
               ;; TODO: merge the following two conditions and use
               ;; `sp-get-stringlike-or-textmode-expression'
               ((and (eq (syntax-class (syntax-after (point))) 7)
                     (not (sp-char-is-escaped-p)))
                (sp-get-string-or-nested-string nil))
               ((sp--valid-initial-delimiter-p (sp--looking-at (sp--get-stringlike-regexp)))
                (sp-get-expression nil))
               ;; it can still be that we are looking at a /prefix/ of a
               ;; sexp.  We should skip a symbol forward and check if it
               ;; is a sexp, and then maybe readjust the output.
               (t (let* ((sym (sp-get-symbol nil))
                         (sym-string (and sym (sp-get sym (buffer-substring-no-properties :beg :end))))
                         (point-before-prefix (point)))
                    (when sym-string
                      (if (sp--valid-initial-delimiter-p (sp--search-forward-regexp (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)) nil t))
                          (let* ((ms (match-string 0))
                                 (pref (progn
                                         ;; need to move before the
                                         ;; opening, so (point) evals
                                         ;; there.
                                         (backward-char (length ms))
                                         (sp--get-prefix (point) ms))))
                            ;; We use >= because the first skip to
                            ;; symbol might have skipped some prefix
                            ;; chars which make prefix of the symbol
                            ;; which together make prefix of a sexp.
                            ;; For example \foo{} in latex, where \ is
                            ;; prefix of symbol foo and \foo is prefix
                            ;; of {
                            (if (and pref
                                     (not (equal pref ""))
                                     (>= point-before-prefix (- (point) (length pref))))
                                (sp-get-sexp nil)
                              sym))
                        sym))))))))))))))

(defun sp-narrow-to-sexp (arg)
  "Make text outside current balanced expression invisible.
A numeric arg specifies to move up by that many enclosing expressions.

See also `narrow-to-region' and `narrow-to-defun'."
  (interactive "p")
  (-when-let (enc (sp-get-enclosing-sexp arg))
    (sp-get enc (narrow-to-region :beg-prf :end))))

(defun sp-forward-sexp (&optional arg)
  "Move forward across one balanced expression.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.  If there is no forward
expression, jump out of the current one (effectively doing
`sp-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  |(foo bar baz)   -> (foo bar baz)|

  (|foo bar baz)   -> (foo| bar baz)

  (|foo bar baz)   -> (foo bar| baz) ;; 2

  (foo (bar baz|)) -> (foo (bar baz)|)"
  (interactive "^p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-backward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :end))))
      ok)))

(put 'sp-forward-sexp 'CUA 'move)

(defun sp-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).

With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.  If there is no previous
expression, jump out of the current one (effectively doing
`sp-backward-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  (foo bar baz)|   -> |(foo bar baz)

  (foo| bar baz)   -> (|foo bar baz)

  (foo bar| baz)   -> (|foo bar baz) ;; 2

  ((|foo bar) baz) -> (|(foo bar) baz)"
  (interactive "^p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-forward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing t))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :beg))))
      ok)))

(put 'sp-backward-sexp 'CUA 'move)

(defun sp-next-sexp (&optional arg)
  "Move forward to the beginning of next balanced expression.

With ARG, do it that many times.  If there is no next expression
at current level, jump one level up (effectively doing
`sp-backward-up-sexp').  Negative arg -N means move to the
beginning of N-th previous balanced expression.

If `sp-navigate-interactive-always-progress-point' is non-nil,
and this is called interactively, the point will move to the
first expression in forward direction where it will end up
greater than the current location.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

  ((foo) |bar (baz quux)) -> ((foo) bar |(baz quux))

  ((foo) bar |(baz quux)) -> |((foo) bar (baz quux))

and with non-nil `sp-navigate-interactive-always-progress-point'

  (f|oo bar) -> (foo |bar)

  ((fo|o) (bar)) -> ((foo) |(bar))"
  (interactive "^p")
  (setq arg (or arg 1))
  (if (<= arg 0)
      (sp-backward-sexp (- arg))
    (if (and sp-navigate-interactive-always-progress-point
             (called-interactively-p 'any))
        (progn
          (while (< 0 arg)
            (let ((point-start (point)))
              (while (--when-let (sp-forward-sexp)
                       (<= (sp-get it :beg) point-start))))
            (setq arg (1- arg)))
          (goto-char (sp-get (sp-get-thing t) :beg)))
      (if (= arg 1)
          (-when-let (ok (sp-get-thing))
            (if (= (point) (sp-get ok :beg))
                (progn (sp-forward-sexp 2)
                       (sp-backward-sexp))
              (goto-char (sp-get ok :beg))
              ok))
        (sp-forward-sexp arg)
        (sp-backward-sexp)))))

(put 'sp-next-sexp 'CUA 'move)

(defun sp-previous-sexp (&optional arg)
  "Move backward to the end of previous balanced expression.

With ARG, do it that many times.  If there is no next
expression at current level, jump one level up (effectively
doing `sp-up-sexp').  Negative arg -N means move to the end of
N-th following balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

If `sp-navigate-interactive-always-progress-point' is non-nil,
and this is called interactively, the point will move to the
first expression in backward direction where it will end up
less than the current location.

Examples:

  ((foo) bar| (baz quux)) -> ((foo)| bar (baz quux))

  ((foo)| bar (baz quux)) -> ((foo) bar (baz quux))|

and if `sp-navigate-interactive-always-progress-point' is non-nil

  (foo b|ar baz) -> (foo| bar baz)

  (foo (b|ar baz)) -> (foo| (bar baz))"
  (interactive "^p")
  (setq arg (or arg 1))
  (if (<= arg 0)
      (sp-forward-sexp (- arg))
    (if (and sp-navigate-interactive-always-progress-point
             ;; (called-interactively-p 'any)
             )
        (progn
          (while (< 0 arg)
            (let ((point-start (point)))
              (while (--when-let (sp-backward-sexp)
                       (>= (sp-get it :end) point-start))))
            (setq arg (1- arg)))
          (goto-char (sp-get (sp-get-thing) :end)))
      (if (= arg 1)
          (-when-let (ok (sp-get-thing t))
            (if (= (point) (sp-get ok :end))
                (progn (sp-backward-sexp 2)
                       (sp-forward-sexp))
              (goto-char (sp-get ok :end))
              ok))
        (sp-backward-sexp arg)
        (sp-forward-sexp)))))

(put 'sp-previous-sexp 'CUA 'move)

(defun sp-forward-parallel-sexp (&optional arg)
  "Move forward across one balanced expressions at the same depth.

If calling `sp-forward-sexp' at point would result in raising a
level up, loop back to the first expression at current level,
that is the first child of the enclosing sexp as defined by
`sp-get-enclosing-sexp'."
  (interactive "^p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-backward-parallel-sexp (- arg))
    (let (re)
      (while (> arg 0)
        (setq arg (1- arg))
        (let ((next (sp-get-thing))
              (prev (sp-get-thing t)))
          (setq
           re
           (cond
            ((eq next nil)
             (goto-char (point-min))
             (sp-forward-sexp))
            ((eq prev nil)
             (goto-char (sp-get next :end))
             next)
            (t (if (> (sp-get next :beg) (sp-get prev :beg))
                   (progn
                     (goto-char (sp-get next :end))
                     next)
                 (goto-char (sp-get next :beg-in))
                 (sp-forward-sexp)))))))
      re)))

(defun sp-backward-parallel-sexp (&optional arg)
  "Move backward across one balanced expressions at the same depth.

If calling `sp-backward-sexp' at point would result in raising a
level up, loop back to the last expression at current level, that
is the last child of the enclosing sexp as defined by
`sp-get-enclosing-sexp'."
  (interactive "^p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-forward-parallel-sexp (- arg))
    (let (re)
      (while (> arg 0)
        (setq arg (1- arg))
        (let ((next (sp-get-thing))
              (prev (sp-get-thing t)))
          (setq
           re
           (cond
            ((eq prev nil)
             (goto-char (point-max))
             (sp-backward-sexp))
            ((eq next nil)
             (goto-char (sp-get prev :beg))
             prev)
            (t (if (< (sp-get prev :end) (sp-get next :end))
                   (progn
                     (goto-char (sp-get prev :beg))
                     prev)
                 (goto-char (sp-get prev :end-in))
                 (sp-backward-sexp)))))))
      re)))

(defun sp--raw-argument-p (arg)
  "Return t if ARG represents raw argument, that is a non-empty list."
  (and (listp arg) (car arg)))

(defun sp--negate-argument (arg)
  "Return the argument ARG but negated.

If the argument is a raw prefix argument (cons num nil) return a
list with its car negated.  If the argument is just the - symbol,
return 1.  If the argument is nil, return -1.  Otherwise negate
the input number."
  (cond
   ((sp--raw-argument-p arg) (list (- (car arg))))
   ((eq arg '-) 1)
   ((not arg) -1)
   (t (- arg))))

(defun sp-down-sexp (&optional arg)
  "Move forward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move backward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend forward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the beginning of
current list.

If the point is inside sexp and there is no down expression to
descend to, jump to the beginning of current one.  If moving
backwards, jump to end of current one.

Examples:

  |foo (bar (baz quux)) -> foo (|bar (baz quux))

  |foo (bar (baz quux)) -> foo (bar (|baz quux)) ;; 2

  |foo (bar (baz (quux) blab)) -> foo (bar (baz (|quux) blab)) ;; \\[universal-argument]

  (foo (bar baz) |quux) -> (|foo (bar baz) quux)

  (blab foo |(bar baz) quux) -> (|blab foo (bar baz) quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "^P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (last-point -1))
    (if (and raw (= (abs arg) 16))
        ;; jump to the beginning/end of current list
        (-when-let (enc (sp-get-enclosing-sexp))
          (if (> arg 0)
              (goto-char (sp-get enc :beg-in))
            (goto-char (sp-get enc :end-in)))
          (setq ok enc))
      ;; otherwise descend normally
      (while (and ok (> n 0))
        (setq ok (sp-get-sexp (< arg 0)))
        ;; if the prefix was C-u, we do not decrease n and instead set
        ;; it to -1 when (point) == "last ok"
        (if raw
            (when (= (point) last-point)
              (setq n -1))
          (setq n (1- n)))
        (when ok
          (setq last-point (point))
          (if (< arg 0)
              (goto-char (sp-get ok :end-in))
            (goto-char (sp-get ok :beg-in))))))
    ok))

(put 'sp-down-sexp 'CUA 'move)

(defun sp-backward-down-sexp (&optional arg)
  "Move backward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move forward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend backward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the end of current
list.

If the point is inside sexp and there is no down expression to
descend to, jump to the end of current one.  If moving forward,
jump to beginning of current one.

Examples:

  foo (bar (baz quux))| -> foo (bar (baz quux)|)

  (bar (baz quux)) foo| -> (bar (baz quux|)) foo ;; 2

  foo (bar (baz (quux) blab))| -> foo (bar (baz (quux|) blab)) ;; \\[universal-argument]

  (foo| (bar baz) quux) -> (foo (bar baz) quux|)

  (foo (bar baz) |quux blab) -> (foo (bar baz) quux blab|) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "^P")
  (sp-down-sexp (sp--negate-argument arg)))

(put 'sp-backward-down-sexp 'CUA 'move)

(defun sp-beginning-of-sexp (&optional arg)
  "Jump to beginning of the sexp the point is in.

The beginning is the point after the opening delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-down-sexp'

With ARG positive N > 1, move forward out of the current
expression, move N-2 expressions forward and move down one level
into next expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-1 expressions backward and move down one level
into next expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the beginning of enclosing expression.

Examples:

  (foo (bar baz) quux| (blab glob)) -> (|foo (bar baz) quux (blab glob))

  (foo (bar baz|) quux (blab glob)) -> (foo (|bar baz) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (|baz quux) ;; 3

  (foo bar) (baz) (quux|) -> (|foo bar) (baz) (quux) ;; -3

  ((foo bar) (baz |quux) blab) -> (|(foo bar) (baz quux) blab) ;; \\[universal-argument]"
  (interactive "^P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (re (cond
              ((and raw (= arg 4))
               (sp-up-sexp)
               (sp-beginning-of-sexp))
              ((= arg 1)
               (sp-down-sexp '(16)))
              ((< arg 0)
               (sp-backward-up-sexp)
               (sp-forward-sexp (1+ arg))
               (sp-down-sexp))
              ((> arg 0)
               (sp-up-sexp)
               (sp-forward-sexp (- arg 2))
               (sp-down-sexp)))))
    (sp--run-hook-with-args (sp-get re :op) :post-handlers 'beginning-of-sexp)
    re))

(put 'sp-beginning-of-sexp 'CUA 'move)

(defun sp-end-of-sexp (&optional arg)
  "Jump to end of the sexp the point is in.

The end is the point before the closing delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-backward-down-sexp'.

With ARG positive N > 1, move forward out of the current
expression, move N-1 expressions forward and move down backward
one level into previous expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-2 expressions backward and move down backward
one level into previous expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the end of enclosing expression.

Examples:

  (foo |(bar baz) quux (blab glob)) -> (foo (bar baz) quux (blab glob)|)

  (foo (|bar baz) quux (blab glob)) -> (foo (bar baz|) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (baz quux|) ;; 3

  (foo bar) (baz) (quux|) -> (foo bar|) (baz) (quux) ;; -3

  ((foo |bar) (baz quux) blab) -> ((foo bar) (baz quux) blab|) ;; \\[universal-argument]"
  (interactive "^P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (re (cond
              ((and raw (= arg 4))
               (sp-up-sexp)
               (sp-end-of-sexp))
              ((= arg 1)
               (sp-down-sexp '(-16)))
              ((< arg 0)
               (sp-backward-up-sexp)
               (sp-forward-sexp (+ 2 arg))
               (sp-backward-down-sexp))
              ((> arg 0)
               (sp-up-sexp)
               (sp-forward-sexp (1- arg))
               (sp-backward-down-sexp)))))
    (sp--run-hook-with-args (sp-get re :op) :post-handlers 'end-of-sexp)
    re))

(put 'sp-end-of-sexp 'CUA 'move)

(defun sp-beginning-of-next-sexp (&optional arg)
  "Jump to the beginning of next sexp on the same depth.

Optional argument ARG defaults to 1 and means how many times we
should repeat.

This acts exactly as `sp-beginning-of-sexp' but adds 1 to the
numeric argument.

Examples:

  (f|oo) (bar) (baz) -> (foo) (|bar) (baz)

  (f|oo) (bar) (baz) -> (foo) (bar) (|baz) ;; 2"
  (interactive "^P")
  (if (sp--raw-argument-p arg)
      (sp-beginning-of-sexp arg)
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-beginning-of-sexp (1+ arg))
        (sp-beginning-of-sexp (1- arg))))))

(put 'sp-beginning-of-next-sexp 'CUA 'move)

(defun sp-beginning-of-previous-sexp (&optional arg)
  "Jump to the beginning of previous sexp on the same depth.

Optional argument ARG defaults to 1 and means how many times we
should repeat.

This acts exactly as `sp-beginning-of-sexp' with negative
argument but subtracts 1 from it.

Examples:

  (foo) (b|ar) (baz) -> (|foo) (bar) (baz)

  (foo) (bar) (b|az) -> (|foo) (bar) (baz) ;; 2"
  (interactive "^P")
  (if (sp--raw-argument-p arg)
      (sp-beginning-of-sexp (sp--negate-argument arg))
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-beginning-of-sexp (- (1+ arg)))
        (sp-beginning-of-sexp (- (1- arg)))))))

(put 'sp-beginning-of-previous-sexp 'CUA 'move)

(defun sp-end-of-next-sexp (&optional arg)
  "Jump to the end of next sexp on the same depth.

Optional argument ARG defaults to 1 and means how many times we
should repeat.

This acts exactly as `sp-end-of-sexp' but adds 1 to the
numeric argument.

Examples:

  (f|oo) (bar) (baz) -> (foo) (bar|) (baz)

  (f|oo) (bar) (baz) -> (foo) (bar) (baz|) ;; 2"
  (interactive "^P")
  (if (sp--raw-argument-p arg)
      (sp-end-of-sexp arg)
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-end-of-sexp (1+ arg))
        (sp-end-of-sexp (1- arg))))))

(put 'sp-end-of-next-sexp 'CUA 'move)

(defun sp-end-of-previous-sexp (&optional arg)
  "Jump to the end of previous sexp on the same depth.

Optional argument ARG defaults to 1 and means how many times we
should repeat.

This acts exactly as `sp-end-of-sexp' with negative
argument but subtracts 1 from it.

Examples:

  (foo) (b|ar) (baz) -> (foo|) (bar) (baz)

  (foo) (bar) (b|az) -> (foo|) (bar) (baz) ;; 2"
  (interactive "^P")
  (if (sp--raw-argument-p arg)
      (sp-end-of-sexp (sp--negate-argument arg))
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-end-of-sexp (- (1+ arg)))
        (sp-end-of-sexp (- (1- arg)))))))

(put 'sp-end-of-previous-sexp 'CUA 'move)

;; TODO: split the reindent code so we can call it inside strings on
;; sexps like [foo ]... We can't reindent that by default because it
;; can be a regular expression or something where the whitespace
;; matters.  For now, disable reindent in strings if the sexp is not
;; the string quote itself.
(defun sp-up-sexp (&optional arg interactive)
  "Move forward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move backward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
enabled for current major-mode, remove the whitespace between end
of the expression and the last \"thing\" inside the expression.
This behaviour can be suppressed for syntactic string sexps by
setting `sp-navigate-reindent-after-up-in-string' to nil.

If `sp-navigate-close-if-unbalanced' is non-nil, close the
unbalanced expressions automatically.

Examples:

  (foo |(bar baz) quux blab) -> (foo (bar baz) quux blab)|

  (foo (bar |baz) quux blab) -> (foo (bar baz) quux blab)| ;; 2

  (foo bar |baz              -> (foo bar baz)| ;; re-indent the expression
​   )

  (foo  |(bar baz)           -> (foo)| (bar baz) ;; close unbalanced expr."
  (interactive "^p\np")
  (setq arg (or arg 1))
  (sp--with-case-sensitive
    (let ((ok (sp-get-enclosing-sexp (abs arg))))
      (if ok
          (progn
            (if (> arg 0)
                (goto-char (sp-get ok :end))
              (goto-char (sp-get ok :beg)))
            (when (and (= (abs arg) 1)
                       (not (equal (sp-get ok :prefix) sp-comment-char))
                       (or (memq major-mode (assq 'always sp-navigate-reindent-after-up))
                           (and (memq major-mode (assq 'interactive sp-navigate-reindent-after-up))
                                interactive))
                       (or sp-navigate-reindent-after-up-in-string
                           (sp-get ok (not (sp-point-in-string :end-in))))
                       ;; if the sexp to be reindented is not a string
                       ;; but is inside a string, we should rather do
                       ;; nothing than break semantics (in e.g. regexp
                       ;; [...])
                       (let ((str (sp-point-in-string)))
                         (or (not str)
                             ;; op must be the delimiter of the string we're in
                             (eq (sp-get ok :op)
                                 (or (eq str t)
                                     (char-to-string str))))))
              ;; TODO: this needs different indent rules for different
              ;; modes.  Should we concern with such things?  Lisp rules are
              ;; funny in HTML... :/
              (save-excursion
                (if (> arg 0)
                    (progn
                      (goto-char (sp-get ok :end-in))
                      (let ((prev (sp-get-thing t)))
                        ;; if the expression is empty remove everything inside
                        (if (sp-compare-sexps ok prev)
                            (sp-get ok (delete-region :beg-in :end-in))
                          (when (save-excursion
                                  (skip-chars-backward " \t\n")
                                  (= (point) (sp-get prev :end-suf)))
                            (delete-region (sp-get prev :end-suf) (point))))))
                  (goto-char (sp-get ok :beg-in))
                  (let ((next (sp-get-thing)))
                    (if (sp-compare-sexps ok next)
                        (sp-get ok (delete-region :beg-in :end-in))
                      (when (save-excursion
                              (skip-chars-forward " \t\n")
                              (= (point) (sp-get next :beg-prf)))
                        (delete-region (point) (sp-get next :beg-prf)))))))))
        ;; on forward up, we can detect that the pair was not closed.
        ;; Therefore, jump sexps backwards until we hit the error, then
        ;; extract the opening pair and insert it at point.  Only works
        ;; for pairs defined in `sp-pair-list'.
        (when (and (> arg 0)
                   sp-navigate-close-if-unbalanced)
          (let (active-pair)
            (save-excursion
              ;; add support for SGML tags here
              (while (sp-backward-sexp))
              (sp-skip-backward-to-symbol t)
              (when (sp--looking-back (sp--get-opening-regexp))
                (let* ((op (match-string 0)))
                  (setq active-pair (assoc op sp-pair-list)))))
            (when active-pair
              (sp-backward-sexp)
              (sp-forward-sexp)
              (insert (cdr active-pair))))))
      ok)))

(put 'sp-up-sexp 'CUA 'move)

(defun sp-backward-up-sexp (&optional arg interactive)
  "Move backward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move forward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
enabled for current major-mode, remove the whitespace between
beginning of the expression and the first \"thing\" inside the
expression.

Examples:

  (foo (bar baz) quux| blab) -> |(foo (bar baz) quux blab)

  (foo (bar |baz) quux blab) -> |(foo (bar baz) quux blab) ;; 2

  (                  -> |(foo bar baz)
​    foo |bar baz)"
  (interactive "^p\np")
  (setq arg (or arg 1))
  (sp-up-sexp (- arg) interactive))

(put 'sp-backward-up-sexp 'CUA 'move)

(defvar sp-last-kill-whitespace nil
  "Save the whitespace cleaned after the last kill.

If the next command is `sp-kill-sexp', append the whitespace
between the successive kills.")

(defun sp--kill-or-copy-region (beg end &optional dont-kill)
  "Kill or copy region between BEG and END according to DONT-KILL.
If `evil-mode' is active, copying a region will also add it to the 0 register.
Additionally, if command was prefixed with a register, copy the region
to that register."
  (interactive "*")
  (let ((result
         (if dont-kill
             (copy-region-as-kill beg end)
           (kill-region beg end))))
    (when (bound-and-true-p evil-mode)
      (when dont-kill
        (evil-set-register ?0 (evil-get-register ?1)))
      (when evil-this-register
        (evil-set-register evil-this-register (evil-get-register ?1))
        (setq evil-this-register nil)))
    result))

(defun sp-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression following point.

If point is inside an expression and there is no following
expression, kill the topmost enclosing expression.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

With ARG being raw prefix \\[universal-argument], kill all the expressions from
point up until the end of current list.  With raw prefix \\[negative-argument] \\[universal-argument],
kill all the expressions from beginning of current list up until
point.  If point is inside a symbol, this is also killed.  If
there is no expression after/before the point, just delete the
whitespace up until the closing/opening delimiter.

With ARG being raw prefix \\[universal-argument] \\[universal-argument], kill current list (the list
point is inside).

With ARG numeric prefix 0 (zero) kill the insides of the current
list, that is everything from after the opening delimiter to
before the closing delimiter.

If ARG is nil, default to 1 (kill single expression forward)

If second optional argument DONT-KILL is non-nil, save the to be
killed region in the kill ring, but do not kill the region from
buffer.

With `sp-navigate-consider-symbols', symbols and strings are also
considered balanced expressions.

Examples:

 (foo |(abc) bar)  -> (foo | bar) ;; nil, defaults to 1

 (foo (bar) | baz) -> |           ;; 2

 (foo |(bar) baz)  -> |           ;; \\[universal-argument] \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1|)        ;; \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1 | 5 6)   ;; 3

 (1 2 3 4 5| 6)    -> (1 2 3 | 6) ;; -2

 (1 2 3 4| 5 6)    -> (|5 6)      ;; \\[negative-argument] \\[universal-argument]

 (1 2 |   )        -> (1 2|)      ;; \\[universal-argument], kill useless whitespace

 (1 2 3 |4 5 6)    -> (|)         ;; 0

Note: prefix argument is shown after the example in
\"comment\". Assumes `sp-navigate-consider-symbols' equal to t."
  (interactive "*P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point)))
    (cond
     ;; kill to the end or beginning of list
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
            (when (not dont-kill)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end))))
          (if (> arg 0)
              (sp--kill-or-copy-region
               (sp-get next :beg-prf) (sp-get enc :end-in) dont-kill)
            (sp--kill-or-copy-region
             (sp-get next :end) (sp-get enc :beg-in) dont-kill))
          (when (not dont-kill)
            (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end)))))))
     ;; kill the enclosing list
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (sp--kill-or-copy-region
                     :beg-prf :end dont-kill))))
     ;; kill inside of sexp
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (sp--kill-or-copy-region
                     :beg-in :end-in dont-kill)))))
     ;; regular kill
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (sp-get ok
            (when (< :beg-prf b) (setq b :beg-prf))
            (when (> :end e) (setq e :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (if (eq last-command 'kill-region)
              (progn
                (when (member sp-successive-kill-preserve-whitespace '(1 2))
                  (kill-append sp-last-kill-whitespace nil))
                (sp--kill-or-copy-region
                 (if (> b (point)) (point) b) e dont-kill))
            (sp--kill-or-copy-region b e dont-kill))
          ;; kill useless junk whitespace, but only if we're actually
          ;; killing the region
          (when (not dont-kill)
            (sp--cleanup-after-kill)
            ;; kill useless newlines
            (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
              (setq sp-last-kill-whitespace
                    (concat sp-last-kill-whitespace
                            (buffer-substring-no-properties bm (point))))
              (delete-region bm (point)))
            (when (= 0 sp-successive-kill-preserve-whitespace)
              (kill-append sp-last-kill-whitespace nil)))))))))

(defmacro sp-save-kill-ring (&rest forms)
  "Run FORMS while preserving `kill-ring'.

Any changes to `kill-ring' inside this macro will be reversed
after exiting.

The changes are usually done by functions suck as `kill-region',
`copy-region-as-kill', `kill-append' and similar."
  (declare (indent 0))
  `(let* ((kill-ring nil)
          (kill-ring-yank-pointer nil)
          (select-enable-clipboard nil))
     ,@forms))

(defun sp-delete-sexp (&optional arg)
  "Delete the balanced expression following point.

This is exactly like calling `sp-kill-sexp'
except deleted sexp does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

See also `sp-kill-sexp' examples."
  (interactive "*p")
  (sp-save-kill-ring
    (sp-kill-sexp arg)))

(defun sp--cleanup-after-kill ()
  (unless (save-match-data (looking-back "^[\t\s]+" (1- (line-beginning-position))))
    (let ((bdel (save-excursion
                  (when (sp--looking-back-p " " 1)
                    (skip-chars-backward " \t")
                    (when (not (sp--looking-back-p (sp--get-opening-regexp)))
                      (forward-char)))
                  (point)))
          (edel (save-excursion
                  (when (looking-at " ")
                    (skip-chars-forward " \t")
                    (when (not (or (sp--looking-at (sp--get-closing-regexp))
                                   (looking-at "$")))
                      (backward-char)))
                  (point))))
      (when (eq this-command 'kill-region)
        (setq sp-last-kill-whitespace
              (if (/= 2 sp-successive-kill-preserve-whitespace)
                  (buffer-substring-no-properties bdel edel)
                "")))
      (delete-region bdel edel)))
  (if (memq major-mode sp-lisp-modes)
      ;; WARNING: The above white-space killing routine might preserve
      ;; less whitespace than there actually is because the indent
      ;; might further eat some up
      (indent-according-to-mode)
    (unless (or (memq major-mode sp-no-reindent-after-kill-modes)
                (memq indent-line-function
                      sp-no-reindent-after-kill-indent-line-functions))
      (save-excursion
        (sp--indent-region (line-beginning-position) (line-end-position)))
      (when (> (save-excursion
                 (back-to-indentation)
                 (current-indentation))
               (current-column))
        (back-to-indentation)))))

(defun sp-backward-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression preceding point.

This is exactly like calling `sp-kill-sexp' with minus ARG.
In other words, the direction of all commands is reversed.  For
more information, see the documentation of `sp-kill-sexp'.

Examples:

  (foo (abc)| bar)           -> (foo | bar)

  blab (foo (bar baz) quux)| -> blab |

  (1 2 3 |4 5 6)             -> (|4 5 6) ;; \\[universal-argument]"
  (interactive "*P")
  (sp-kill-sexp (sp--negate-argument arg) dont-kill))

(defun sp-backward-delete-sexp (&optional arg)
  "Delete the balanced expression preceding point.

This is exactly like calling `sp-backword-kill-sexp'
except deleted sexp does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

See also `sp-backward-kill-sexp' examples."
  (interactive "*p")
  (sp-save-kill-ring
    (sp-backward-kill-sexp arg)))

(defun sp-copy-sexp (&optional arg)
  "Copy the following ARG expressions to the kill-ring.

This is exactly like calling `sp-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp arg t)))

(defun sp-backward-copy-sexp (&optional arg)
  "Copy the previous ARG expressions to the kill-ring.

This is exactly like calling `sp-backward-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp (sp--negate-argument arg) t)))

(defun sp-clone-sexp ()
  "Clone sexp after or around point.

If the form immediately after point is a sexp, clone it below the
current one and put the point in front of it.

Otherwise get the enclosing sexp and clone it below the current
enclosing sexp."
  (interactive "*")
  (-when-let (ok (let ((sexp (sp-get-thing)))
                   (if (not (equal (sp-get sexp :op) ""))
                       sexp
                     (sp-get-enclosing-sexp))))
    (sp-get ok
      (undo-boundary)
      (if (< :beg-prf (point))
          ;; this is the case where point is inside a sexp, we place
          ;; the "clone" before the current enclosing sexp and move
          ;; the old one below.  Note that the "net result" is the
          ;; same as the other case, but the implementation must
          ;; reflect different relative position of the point wrt
          ;; "current" sexp.
          (save-excursion
            (goto-char :beg-prf)
            (insert-buffer-substring-no-properties
             (current-buffer) :beg-prf :end-suf)
            (newline-and-indent))
        ;; in this case we are in front, so we move after the current
        ;; one, place the clone and move it below
        (goto-char :end-suf)
        (save-excursion
          (insert-buffer-substring-no-properties
           (current-buffer) :beg-prf :end-suf))
        (newline-and-indent))
      (sp-indent-defun))))

(defun sp-kill-hybrid-sexp (arg)
  "Kill a line as if with `kill-line', but respecting delimiters.

With ARG being raw prefix \\[universal-argument] \\[universal-argument], kill the hybrid sexp
the point is in (see `sp-get-hybrid-sexp').

With ARG numeric prefix 0 (zero) just call `kill-line'.

You can customize the behaviour of this command by customizing
`sp-hybrid-kill-excessive-whitespace' and
`sp-hybrid-kill-entire-symbol'.

Examples:

  foo | bar baz               -> foo |               ;; nil

  foo (bar | baz) quux        -> foo (bar |) quux    ;; nil

  foo | bar (baz              -> foo |               ;; nil
             quux)

  foo \"bar |baz quux\" quack   -> foo \"bar |\" quack   ;; nil

  foo (bar
       baz) qu|ux (quack      ->   foo | hoo         ;; \\[universal-argument] \\[universal-argument]
                   zaq) hoo

  foo | (bar                  -> foo |               ;; C-0
         baz)                          baz)"
  (interactive "*P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (orig-indent (save-excursion
                        (back-to-indentation)
                        (current-column)))
         (orig-column (current-column)))
    (cond
     ((= arg 0) (kill-line))
     ((and raw (= arg 16))
      (let ((hl (sp-get-hybrid-sexp)))
        (sp-get hl (kill-region :beg-prf :end-suf))))
     (t
      (let ((hl (sp-get-hybrid-sexp)))
        (save-excursion
          (when (and (or (eq sp-hybrid-kill-entire-symbol t)
                         (and (functionp sp-hybrid-kill-entire-symbol)
                              (not (funcall sp-hybrid-kill-entire-symbol))))
                     (sp-point-in-symbol))
            (sp-backward-sexp))
          (sp-get hl
            (let ((end (min (point-max) (if (looking-at "[ \t]*$")
                                            (1+ :end-suf)
                                          :end-suf))))
              (when sp-hybrid-kill-excessive-whitespace
                (save-excursion
                  (goto-char end)
                  (skip-chars-forward "\n\t\r\s")
                  (cond
                   ((eq 'kill sp-hybrid-kill-excessive-whitespace)
                    (setq end (point)))
                   (t (delete-region end (point))))))
              (kill-region (point) end)))))
      (sp--cleanup-after-kill)
      ;; if we've killed the entire line, do *not* contract the indent
      ;; to just one space
      (when (sp-point-in-blank-line)
        (delete-region (line-beginning-position) (line-end-position))
        (if (and (= 0 orig-column)
                 kill-whole-line)
            (delete-char 1) ;; delete the newline
          (let ((need-indent (- orig-indent (current-column))))
            (when (> need-indent 0)
              (insert (make-string need-indent ?\ ))))))))))

(defun sp-kill-whole-line ()
  "Kill current line in sexp-aware manner.

First, go to the beginning of current line and then try to kill
as much as possible on the current line but without breaking
balance.

If there is a hanging sexp at the end of line the it is killed as
well.

If there is a closing delimiter for a sexp \"up\" current sexp,
the kill is not extended after it.  For more details see
`sp-kill-hybrid-sexp'.

Examples:

  (progn                    (progn
    (some |long sexp))  ->    |)"
  (interactive "*")
  (beginning-of-line)
  (sp-kill-hybrid-sexp nil)
  (let ((empty-last-line (save-excursion (beginning-of-line) (eobp))))
    ;; We can't kill the line if it is empty and the last line
    (when (and (sp-point-in-blank-line) (not empty-last-line))
      (kill-whole-line))))

(defun sp--transpose-objects (first second)
  "Transpose FIRST and SECOND object while preserving the
whitespace between them."
  (save-excursion
    (goto-char (sp-get second :beg-prf))
    (let ((ins (sp-get second (delete-and-extract-region :beg-prf :end)))
          (between (delete-and-extract-region (sp-get first :end) (point))))
      (goto-char (sp-get first :beg-prf))
      (insert ins between))))

(defun sp-transpose-sexp (&optional arg)
  "Transpose the expressions around point.

The operation will move the point after the transposed block, so
the next transpose will \"drag\" it forward.

With arg positive N, apply that many times, dragging the
expression forward.

With arg negative -N, apply N times backward, pushing the word
before cursor backward.  This will therefore not transpose the
expressions before and after point, but push the expression
before point over the one before it.

Examples:

  foo |bar baz     -> bar foo| baz

  foo |bar baz     -> bar baz foo| ;; 2

  (foo) |(bar baz) -> (bar baz) (foo)|

  (foo bar)        ->    (baz quux)   ;; keeps the formatting
​    |(baz quux)            |(foo bar)

  foo bar baz|     -> foo baz| bar ;; -1"
  (interactive "*P")
  (let* ((arg (prefix-numeric-value arg))
         (n (abs arg)))
    ;; if we're inside a symbol, we need to move out of it first
    (when (> arg 0)
      (when (sp-point-in-symbol)
        (sp-forward-symbol)))
    (while (> n 0)
      (when (< arg 0) (sp-backward-sexp))
      (let* ((next (save-excursion (sp-forward-sexp)))
             (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
        (sp--transpose-objects prev next)
        (when (< arg 0)
          (goto-char (+ (sp-get prev :beg-prf) (sp-get next :len))))
        (setq n (1- n))))))

(defun sp-transpose-hybrid-sexp (&optional arg)
  "Transpose the hybrid sexps around point.

`sp-backward-sexp' is used to enter the previous hybrid sexp.

With ARG numeric prefix call `transpose-lines' with this
argument.

The operation will move the point at the next line after the
transposed block if it is at the end of line already.

Examples:

  foo bar            baz (quux
  |baz (quux   ->         quack)
        quack)       foo bar\\n|


  [(foo) (bar) -> [(baz)
  |(baz)]          (foo) (bar)|]

  foo bar baz  -> quux flux
  |quux flux      foo bar baz\\n|"
  (interactive "*P")
  (if (numberp arg)
      (transpose-lines arg)
    (let* ((next (save-excursion
                   (sp-forward-sexp)
                   (sp-backward-sexp)
                   (sp-get-hybrid-sexp)))
           (prev (save-excursion
                   (goto-char (sp-get next :beg))
                   (sp-backward-sexp)
                   (sp-get-hybrid-sexp))))
      (if (sp-compare-sexps prev next > :end)
          (sp-message :invalid-context-prev)
        (sp--transpose-objects prev next))
      (when (looking-at "[\n\t ]+")
        (forward-line)
        (back-to-indentation)))))

(defun sp-push-hybrid-sexp ()
  "Push the hybrid sexp after point over the following one.

`sp-forward-sexp' is used to enter the following hybrid sexp.

Examples:

  |x = big_function_call(a,    |(a,
                         b)      b) = read_user_input()
                           ->
  (a,                          x = big_function_call(a,
   b) = read_user_input()                            b)"
  (interactive "*")
  (let* ((cur (sp-get-hybrid-sexp))
         (next (save-excursion
                 (goto-char (sp-get cur :end))
                 (sp-forward-sexp)
                 (sp-get-hybrid-sexp))))
    (if (sp-compare-sexps cur next >)
        (sp-message :invalid-context-cur)
      (sp--transpose-objects cur next))))

;; The following two functions are inspired by "adjust-parens.el"
;; package available at
;; http://elpa.gnu.org/packages/adjust-parens-1.0.el
(defun sp-indent-adjust-sexp ()
  "Add the hybrid sexp at line into previous sexp.  All forms
between the two are also inserted.  Specifically, if the point is
on empty line, move the closing delimiter there, so the next
typed text will become the last item of the previous sexp.

This acts similarly to `sp-add-to-previous-sexp' but with special
handling of empty lines."
  (interactive "*")
  (let* ((hsexp (sp-get-hybrid-sexp))
         (prev-sexp (save-excursion
                      (goto-char (sp-get hsexp :beg))
                      (sp-get-sexp t))))
    (if (not (and prev-sexp hsexp
                  (sp-compare-sexps prev-sexp hsexp < :end :beg)))
        (sp-message :no-structure-found)
      (save-excursion
        (sp-get prev-sexp
          (goto-char (sp-get hsexp :end))
          (insert :cl)
          (goto-char :end-in)
          (delete-char :cl-l)))
      (sp-get (sp-get-enclosing-sexp) (sp--indent-region :beg :end))
      (indent-according-to-mode)
      (sp--run-hook-with-args (sp-get prev-sexp :op) :post-handlers 'indent-adjust-sexp))))

(defun sp-dedent-adjust-sexp ()
  "Remove the hybrid sexp at line from previous sexp.  All
sibling forms after it are also removed (not deleted, just placed
outside of the enclosing list).  Specifically, if the point is on
empty line followed by closing delimiter of enclosing list, move
the closing delimiter after the last item in the list.

This acts similarly to `sp-forward-barf-sexp' but with special
handling of empty lines."
  (interactive "*")
  (-when-let (enc (sp-get-enclosing-sexp))
    (save-excursion
      ;; if we're looking at whitespace and end of sexp, move the
      ;; closing paren over the whitespace but *after* the last item
      ;; in the list (barf would also go *before* the last item)
      (sp-skip-forward-to-symbol t)
      (if (= (point) (sp-get enc :end-in))
          (let ((prev-sexp (sp-get-thing t)))
            (sp-get enc
              (delete-char :cl-l)
              (goto-char (sp-get prev-sexp :end))
              ;; see next TODO
              (save-restriction
                (sp--narrow-to-line)
                (skip-syntax-forward " ")
                (skip-syntax-forward "."))
              (insert :cl)))
        ;; otherwise just C-u barf
        (sp-skip-backward-to-symbol t)
        (sp-forward-barf-sexp '(4))
        ;; we need to take special care of any hanging
        ;; punctuation. TODO: this should be a sexp suffix? HACK until
        ;; we fix barf to get the info.
        (save-restriction
          (sp-get (sp-backward-down-sexp)
            (goto-char :end)
            (delete-char (- :cl-l))
            (sp--narrow-to-line)
            (skip-syntax-forward " ")
            (skip-syntax-forward ".")
            (insert :cl)))
        (sp-get enc (sp--indent-region :beg :end))))
    (indent-according-to-mode)
    (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'dedent-adjust-sexp)))

;;  "When the hook is called point is *after* the just moved closing delimiter."
;; TODO: add hook
(defun sp-slurp-hybrid-sexp ()
  "Add hybrid sexp following the current list in it by moving the
closing delimiter.

This is philosophically similar to `sp-forward-slurp-sexp' but
works better in \"line-based\" languages like C or Java.

Because the structure is much looser in these languages, this
command currently does not support all the prefix argument
triggers that `sp-forward-slurp-sexp' does."
  (interactive "*")
  (let (slurped-within-line)
    (-if-let* ((enc (sp-get-enclosing-sexp))
               (bsexp (save-excursion
                        (sp-get enc (goto-char :end))
                        (when (sp-compare-sexps (sp-forward-sexp) enc >)
                          (sp-get-hybrid-sexp)))))
        (save-excursion
          (sp-get enc
            (goto-char :end-suf)
            (delete-char (- (+ :cl-l :suffix-l)))
            ;; TODO: move to hook
            (when (sp-point-in-blank-line)
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            (sp-forward-sexp)

            (when (eq (line-number-at-pos :beg)
                      (line-number-at-pos :end))
              (setq slurped-within-line t))
            ;; If we're slurping over multiple lines, include the suffix on the next line.
            ;; I.e. while () {|} -> while () {\n foo(); \n}
            (unless slurped-within-line
              (sp-get (sp-get-hybrid-sexp) (goto-char :end-suf)))
            (insert :cl :suffix))
          ;; TODO: move to hook
          (sp-get (sp--next-thing-selection -1)
            (save-excursion
              (if (save-excursion
                    (goto-char :beg-in)
                    (looking-at "[ \t]*$"))
                  (progn
                    (goto-char :end-in)
                    (newline))
                ;; copy the whitespace after opening delim and put it in
                ;; front of the closing. This will ensure pretty { foo }
                ;; or {foo}
                (goto-char :end-in)
                (insert (buffer-substring-no-properties
                         :beg-in
                         (+ :beg-in (save-excursion
                                      (goto-char :beg-in)
                                      (skip-syntax-forward " ")))))))
            (unless (or (looking-at "[ \t]*$")
                        (looking-at (sp--get-stringlike-regexp))
                        (looking-at (sp--get-closing-regexp))
                        slurped-within-line)
              (newline)))
          (sp-get (sp--next-thing-selection -1) (sp--indent-region :beg :end))
          ;; we need to call this again to get the new structure after
          ;; indent.
          (sp--next-thing-selection -1))
      (sp-message :invalid-structure)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "paredit" operations

(defun sp-forward-slurp-sexp (&optional arg)
  "Add sexp following the current list in it by moving the closing delimiter.

If the current list is the last in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or end of file).

If ARG is N, apply this function that many times.

If ARG is negative -N, extend the opening pair instead (that is,
backward).

If ARG is raw prefix \\[universal-argument], extend all the way to the end of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

See also `sp-slurp-hybrid-sexp' which is similar but handles
C-style syntax better.

Examples:

  (foo |bar) baz        -> (foo |bar baz)

  [(foo |bar)] baz      -> [(foo |bar) baz]

  [(foo |bar) baz]      -> [(foo |bar baz)]

  ((|foo) bar baz quux) -> ((|foo bar baz quux)) ;; with \\[universal-argument]

  \"foo| bar\" \"baz quux\" -> \"foo| bar baz quux\""
  (interactive "*P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            (in-comment (sp-point-in-comment))
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :end-suf))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (goto-char (sp-get next-thing :end-in))
                    (sp--run-hook-with-args (sp-get enc :op) :pre-handlers 'slurp-forward
                                            (list :arg arg :enc enc :next-thing next-thing))
                    (sp-get enc (insert :cl :suffix))
                    (goto-char (sp-get enc :end-suf))
                    (delete-char (sp-get enc (- (+ :cl-l :suffix-l))))
                    (sp--indent-region (sp-get enc :beg-prf) (sp-get next-thing :end))
                    (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'slurp-forward
                                            (list :arg arg :enc enc :next-thing next-thing))))
              (while (> n 0)
                (goto-char (sp-get enc :end-suf))
                (setq ok enc)
                (setq next-thing (sp-get-thing nil))
                (while (sp-compare-sexps next-thing ok <)
                  (goto-char (sp-get next-thing :end-suf))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing nil)))
                ;; do not allow slurping into a different context from
                ;; inside a comment
                (if (and in-comment
                         (save-excursion
                           (sp-get next-thing
                             (goto-char :beg)
                             (not (sp-point-in-comment)))))
                    (progn
                      (sp-message :cant-slurp-context)
                      (setq n -1))
                  (if ok
                      (progn
                        (if (and (equal (sp-get next-thing :cl) "\"")
                                 (equal (sp-get ok :cl) "\""))
                            (progn
                              (sp--join-sexp ok next-thing)
                              (goto-char (- (sp-get next-thing :end) 2))
                              (plist-put enc :end (- (sp-get next-thing :end) 2)))
                          (let ((inner-sexp
                                 (save-excursion
                                   (goto-char (sp-get ok :end-in))
                                   (sp-get-thing t))))
                            (delete-char (sp-get ok (- (+ :cl-l :suffix-l))))
                            ;; this calculation corrects the absence
                            ;; of already deleted cls
                            (goto-char (- (sp-get next-thing :end-suf)
                                          (sp-get ok (+ :cl-l :suffix-l))))
                            ;; only insert space if not inserting it
                            ;; would merge two sexps together
                            (when (and (sp-get ok (/= :len-in 0))
                                       (sp-compare-sexps
                                        inner-sexp
                                        (sp-get-thing t))
                                       (= (sp-get ok :end-suf)
                                          (sp-get next-thing :beg-prf)))
                              (save-excursion
                                (goto-char (sp-get ok :end-in))
                                (insert " "))))
                          (sp--run-hook-with-args
                           (sp-get enc :op) :pre-handlers 'slurp-forward
                           (list :arg arg :enc enc :ok ok :next-thing next-thing))
                          (sp-get ok (insert :cl :suffix))
                          (sp--indent-region (sp-get ok :beg-prf) (point))
                          ;; HACK: update the "enc" data structure if ok==enc
                          (when (= (sp-get enc :beg) (sp-get ok :beg)) (plist-put enc :end (point)))
                          (sp--run-hook-with-args
                           (sp-get enc :op) :post-handlers 'slurp-forward
                           (list :arg arg :enc enc :ok ok :next-thing next-thing)))
                        (setq n (1- n)))
                    (sp-message :cant-slurp)
                    (setq n -1))))))))
    (sp-backward-slurp-sexp (sp--negate-argument arg))))

(defun sp-backward-slurp-sexp (&optional arg)
  "Add the sexp preceding the current list in it by moving the
opening delimiter.

If the current list is the first in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or beginning of file).

If arg is N, apply this function that many times.

If arg is negative -N, extend the closing pair instead (that is,
forward).

If ARG is raw prefix \\[universal-argument], extend all the way
to the beginning of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

  foo (bar| baz)        -> (foo bar| baz)

  foo [(bar| baz)]      -> [foo (bar| baz)]

  [foo (bar| baz)]      -> [(foo bar| baz)]

  (foo bar baz (|quux)) -> ((foo bar baz |quux)) ;; with \\[universal-argument]

  \"foo bar\" \"baz |quux\" -> \"foo bar baz |quux\""
  (interactive "*P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            (in-comment (sp-point-in-comment))
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :beg-prf))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (delete-char (sp-get enc (+ :op-l :prefix-l)))
                    (goto-char (sp-get next-thing :beg-in))
                    (sp--run-hook-with-args
                     (sp-get enc :op) :pre-handlers 'slurp-backward
                     (list :arg arg :enc enc :next-thing next-thing))
                    (sp-get enc (insert :prefix :op))
                    (sp--indent-region (sp-get next-thing :beg-in) (sp-get enc :end))
                    (sp--run-hook-with-args
                     (sp-get enc :op) :post-handlers 'slurp-backward
                     (list :arg arg :enc enc :next-thing next-thing))))
              (while (> n 0)
                (goto-char (sp-get enc :beg-prf))
                (setq ok enc)
                (setq next-thing (sp-get-thing t))
                (while (sp-compare-sexps next-thing ok > :end)
                  (goto-char (sp-get next-thing :beg-prf))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing t)))
                ;; do not allow slurping into a different context from
                ;; inside a comment
                (if (and in-comment
                         (save-excursion
                           (sp-get next-thing
                             (goto-char :beg)
                             (not (sp-point-in-comment)))))
                    (progn
                      (sp-message :cant-slurp-context)
                      (setq n -1))
                  (if ok
                      (progn
                        (if (and (equal (sp-get next-thing :cl) "\"")
                                 (equal (sp-get ok :cl) "\""))
                            (progn
                              (sp--join-sexp next-thing ok)
                              (goto-char (sp-get next-thing :beg-prf))
                              (plist-put enc :beg (sp-get next-thing :beg)))
                          (let ((inner-sexp
                                 (save-excursion
                                   (goto-char (sp-get ok :beg-in))
                                   (sp-get-thing))))
                            (delete-char (sp-get ok (+ :op-l :prefix-l)))
                            (goto-char (sp-get next-thing :beg-prf))
                            ;; only insert space if not inserting it
                            ;; would merge two sexps together
                            (when (and (sp-get ok (/= :len-in 0))
                                       (= (sp-get ok (- (sp-get inner-sexp :end)
                                                        :op-l :prefix-l))
                                          (sp-get (sp-get-thing) :end))
                                       (= (sp-get ok :beg-prf)
                                          (sp-get next-thing :end-suf)))
                              (save-excursion
                                (goto-char (sp-get ok (- :beg-in :op-l :prefix-l)))
                                (insert " "))))
                          (sp--run-hook-with-args
                           (sp-get enc :op) :pre-handlers 'slurp-backward
                           (list :arg arg :enc enc :ok ok :next-thing next-thing))
                          (sp-get ok (insert :prefix :op))
                          (sp--indent-region (point) (sp-get ok :end))
                          ;; HACK: update the "enc" data structure if ok==enc
                          (when (sp-compare-sexps enc ok) (plist-put enc :beg (- (point) (sp-get ok :op-l))))
                          (sp--run-hook-with-args
                           (sp-get enc :op) :post-handlers 'slurp-backward
                           (list :arg arg :enc enc :ok ok :next-thing next-thing)))
                        (setq n (1- n)))
                    (sp-message :cant-slurp)
                    (setq n -1))))))))
    (sp-forward-slurp-sexp (sp--negate-argument arg))))

(defun sp-add-to-previous-sexp (&optional arg)
  "Add the expression around point to the first list preceding point.

With ARG positive N add that many expressions to the preceding
list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the end of enclosing list to the previous list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the previous list.

Examples:

  (foo bar) |baz quux        -> (foo bar |baz) quux

  (foo bar) |baz quux        -> (foo bar |baz quux) ;; 2

  (blab (foo bar) |baz quux) -> (blab (foo bar |baz quux)) ;; \\[universal-argument]

  (foo bar) (baz |quux)      -> (foo bar (baz |quux)) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "*P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-backward-up-sexp)
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp))
     (t
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp arg))))
  (indent-according-to-mode))

(defun sp-add-to-next-sexp (&optional arg)
  "Add the expressions around point to the first list following point.

With ARG positive N add that many expressions to the following
list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the beginning of enclosing list to the following list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the following list.

Examples:

  foo bar| (baz quux)        -> foo (bar| baz quux)

  foo bar| (baz quux)        -> (foo bar| baz quux) ;; 2

  (foo bar |(bar quux) blab) -> ((foo bar |bar quux) blab) ;; \\[universal-argument]

  (foo |bar) (baz quux)      -> ((foo |bar) baz quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "*P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-up-sexp)
      (sp-down-sexp)
      (sp-backward-slurp-sexp))
     (t
      (sp-down-sexp)
      (sp-backward-slurp-sexp arg)))))

(defun sp-forward-barf-sexp (&optional arg)
  "Remove the last sexp in the current list by moving the closing delimiter.

If ARG is positive number N, barf that many expressions.

If ARG is negative number -N, contract the opening pair instead.

If ARG is raw prefix \\[universal-argument], barf all expressions from the one after
point to the end of current list and place the point before the
closing delimiter of the list.

If the current list is empty, do nothing.

If the setting `sp-barf-move-point-with-delimiter' is non-nil,
move the point together with the closing delimiter if the point
would end up outside of the enclosing sexp.  This way the barf
can be always followed by a slurp to undo the change.

Examples: (prefix arg in comment)

  (foo bar| baz)   -> (foo bar|) baz   ;; nil (defaults to 1)

  (foo| [bar baz]) -> (foo|) [bar baz] ;; 1

  (1 2 3| 4 5 6)   -> (1 2 3|) 4 5 6   ;; \\[universal-argument] (or numeric prefix 3)

  (foo bar| baz)   -> foo (bar| baz)   ;; -1"
  (interactive "*P")
  (let* ((raw (sp--raw-argument-p arg))
         (old-arg arg)
         (arg (prefix-numeric-value arg))
         (new-cl-position nil))
    (if (> arg 0)
        (if (sp-point-in-blank-sexp)
            (sp-message :blank-sexp)
          (save-excursion
            (let ((enc (sp-get-enclosing-sexp)))
              (sp-get enc
                (cond
                 ((and raw (= arg 4))
                  (sp-get (sp-get-thing t)
                    (goto-char :end-suf)))
                 (t
                  (goto-char :end-in)
                  (sp-backward-sexp arg)
                  (when (<= (point) :beg)
                    (goto-char :beg-in))))
                ;; we know for sure there is at least one thing in the list
                (let ((back (sp-get-thing t)))
                  (if (sp-compare-sexps back enc)
                      (goto-char :beg-in)
                    (goto-char (sp-get back :end-suf))))
                (sp--run-hook-with-args :op :pre-handlers 'barf-forward
                  (list :arg arg :enc enc)))
              (sp-get (sp-get-enclosing-sexp)
                (sp-do-move-cl (point))
                (sp--keep-indentation
                  (sp--indent-region :beg :end))
                (setq new-cl-position (- (point) :cl-l))
                (sp--run-hook-with-args :op :post-handlers 'barf-forward
                  (list :arg arg :enc enc)))))
          (when (and sp-barf-move-point-with-delimiter
                     (< new-cl-position (point)))
            (goto-char new-cl-position)))
      (sp-backward-barf-sexp (sp--negate-argument old-arg)))))

(defun sp-backward-barf-sexp (&optional arg)
  "This is exactly like calling `sp-forward-barf-sexp' with minus ARG.
In other words, instead of contracting the closing pair, the
opening pair is contracted.  For more information, see the
documentation of `sp-forward-barf-sexp'.

Examples:

  (foo bar| baz) -> foo (bar| baz)

  ([foo bar] |baz) -> [foo bar] (|baz)

  (1 2 3 |4 5 6) -> 1 2 3 (|4 5 6) ;; \\[universal-argument] (or 3)"
  (interactive "*P")
  (let* ((raw (sp--raw-argument-p arg))
         (old-arg arg)
         (arg (prefix-numeric-value arg))
         (new-cl-position nil))
    (if (> arg 0)
        (if (sp-point-in-blank-sexp)
            (sp-message :blank-sexp)
          (save-excursion
            (let ((enc (sp-get-enclosing-sexp)))
              (sp-get enc
                (cond
                 ((and raw (= arg 4))
                  (sp-get (sp-get-thing)
                    (goto-char :beg-prf)))
                 (t
                  (goto-char :beg-in)
                  (sp-forward-sexp arg)
                  (when (>= (point) :end)
                    (goto-char :end-in))))
                ;; we know for sure there is at least one thing in the list
                (let ((next (sp-get-thing)))
                  (if (sp-compare-sexps next enc)
                      (goto-char :end-in)
                    (goto-char (sp-get next :beg-prf))))
                (sp--run-hook-with-args :op :pre-handlers 'barf-backward
                  (list :arg arg :enc enc)))
              (sp-get (sp-get-enclosing-sexp)
                ;; make sure that we end up on the same place, since
                ;; sp-do-move-op might move the point to the start of
                ;; the previous sexp (the one barfed out)
                (save-excursion (sp-do-move-op (point)))
                ;; skip the opening to end up inside the sexp
                (forward-char (+ :op-l :prefix-l))
                (sp--indent-region :beg :end)
                (setq new-cl-position (point))
                (sp--run-hook-with-args :op :post-handlers 'barf-backward
                  (list :arg arg :enc enc)))))
          (when (and sp-barf-move-point-with-delimiter
                     (> new-cl-position (point)))
            (goto-char new-cl-position)))
      (sp-forward-barf-sexp (sp--negate-argument old-arg)))))

;; TODO: get rid of the macro anyway, it's stupid!
(defmacro sp--skip-to-symbol-1 (forward)
  "Generate `sp-skip-forward-to-symbol' or `sp-skip-backward-to-symbol'."
  (let ((inc (if forward '1+ '1-))
        (dec (if forward '1- '1+))
        (forward-fn (if forward 'forward-char 'backward-char))
        (next-char-fn (if forward 'following-char 'preceding-char))
        (looking (if forward 'sp--looking-at 'sp--looking-back))
        (prefix-fn (if forward 'sp--get-suffix 'sp--get-prefix))
        (eob-test (if forward '(eobp) '(bobp)))
        (comment-bound (if forward 'cdr 'car)))
    `(let ((in-comment (sp-point-in-comment))
           ;; HACK: if we run out of current context this might skip a
           ;; pair that was not allowed before.  However, such a call is
           ;; never made in SP, so it's OK for now
           (allowed-pairs (sp--get-allowed-regexp))
           ,(if forward
                '(allowed-close (sp--get-closing-regexp (sp--get-allowed-pair-list)))
              '(allowed-open (sp--get-opening-regexp (sp--get-allowed-pair-list))))
           (allowed-strings (sp--get-stringlike-regexp))
           (prefix nil))
       (while (and (not (or ,eob-test
                            (and stop-after-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,dec (point))))
                            (and stop-at-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,inc (point))))
                            (and stop-inside-string
                                 (sp-point-in-string)
                                 (not (sp-point-in-string (,inc (point)))))
                            (and (,looking allowed-pairs)
                                 (or in-comment (not (sp-point-in-comment))))
                            (and (,looking allowed-strings)
                                 (or in-comment (not (sp-point-in-comment))))))
                   (or (member (char-syntax (,next-char-fn)) '(?< ?> ?! ?| ?\ ?\\ ?\" ?' ?.))
                       (/= 0 (logand (lsh 1 20) (car (syntax-after
                                                      ,(if forward
                                                           '(point)
                                                         '(1- (point)))))))
                       (unless in-comment (sp-point-in-comment))
                       ;; This is the case where we are starting at
                       ;; pair (looking at it) and there is some
                       ;; prefix which is not recognized by syntax,
                       ;; i.e. defined by regexp.  This should only be
                       ;; tested once in principle before the next
                       ;; time we land on a delimiter this whole loop
                       ;; stops based on the first branch of the `and'
                       ;; condition in `while' so using expensive
                       ;; functions here is not a bg deal.
                       (and (or (,(if forward 'sp--looking-back 'sp--looking-at)
                                 ,(if forward 'allowed-close 'allowed-open))
                                (,(if forward 'sp--looking-back 'sp--looking-at) allowed-strings))
                            (let ((op (match-string 0)))
                              (setq prefix (,prefix-fn (point) op))
                              (> (length prefix) 0)))))
         (if (and (not in-comment)
                  (sp-point-in-comment))
             (progn
               (goto-char (,comment-bound (sp-get-comment-bounds)))
               (unless ,eob-test (,forward-fn 1)))
           (unless ,eob-test
             (,forward-fn (max (length prefix) 1))))
         (setq prefix nil)))))

(defun sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string stop-inside-string)
  "Skip whitespace and comments moving forward.

If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).

If STOP-AFTER-STRING is non-nil, stop after exiting a string.

If STOP-INSIDE-STRING is non-nil, stop before exiting a string.

Examples:

  foo|   bar -> foo   |bar

  foo|   [bar baz] -> foo   |[bar baz]"
  (interactive "^")
  (sp--skip-to-symbol-1 t))

(put 'sp-skip-forward-to-symbol 'CUA 'move)

(defun sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string stop-inside-string)
  "Skip whitespace and comments moving backward.
If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).

If STOP-AFTER-STRING is non-nil, stop after exiting a string.

If STOP-INSIDE-STRING is non-nil, stop before exiting a string.

Examples:

  foo   |bar -> foo|   bar

  [bar baz]   |foo -> [bar baz]|   foo"
  (interactive "^")
  (sp--skip-to-symbol-1 nil))

(put 'sp-skip-backward-to-symbol 'CUA 'move)

(defun sp-skip-into-string (&optional back)
  "Move the point into the next string.

With BACK non-nil, move backwards."
  (if back
      (while (and (not (sp-point-in-string)) (> (point) (point-min)))
        (backward-char))
    (while (and (not (sp-point-in-string)) (< (point) (point-max)))
      (forward-char))))

;; TODO: in ruby, "foo |if bar" now moves correctly, but there's a
;; noticable lag before it jumps over "if".  This is probably caused
;; by :skip-match handlers.  Investigate!
(defun sp-forward-symbol (&optional arg)
  "Move point to the next position that is the end of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.  Current
symbol only extend to the possible opening or closing delimiter
as defined by `sp-add-pair' even if part of this delimiter
would match \"symbol\" syntax classes.

Examples:

  |foo bar baz          -> foo| bar baz

  |foo (bar (baz))      -> foo (bar| (baz)) ;; 2

  |foo (bar (baz) quux) -> foo (bar (baz) quux|) ;; 4"
  (interactive "^p")
  (setq arg (or arg 1))
  (sp--with-case-sensitive
    (let* ((n (abs arg))
           (fw (> arg 0))
           (allowed (sp--get-allowed-pair-list))
           (open (sp--get-opening-regexp allowed))
           (close (sp--get-closing-regexp allowed)))
      (if fw
          (while (> n 0)
            ;; First we need to get to the beginning of a symbol.  This means
            ;; skipping all whitespace and pair delimiters until we hit
            ;; something in \sw or \s_
            (while (cond
                    ((eobp) nil)
                    ((not (memq (char-syntax (following-char)) '(?w ?_)))
                     (forward-char)
                     t)
                    ;; if allowed is empty, the regexp matches anything
                    ;; and we go into infinite loop, cf. Issue #400
                    ((and allowed (sp--valid-initial-delimiter-p (sp--looking-at open)))
                     (goto-char (match-end 0)))
                    ((and allowed (sp--valid-initial-delimiter-p (sp--looking-at close)))
                     (goto-char (match-end 0)))))
            (while (and (not (eobp))
                        (or (not allowed)
                            (not (or (sp--valid-initial-delimiter-p (sp--looking-at open))
                                     (sp--valid-initial-delimiter-p (sp--looking-at close)))))
                        (or (memq (char-syntax (following-char)) '(?w ?_))
                            ;; Specifically for lisp, we consider
                            ;; sequences of ?\<ANYTHING> a symbol
                            ;; sequence
                            (and (eq (char-before) ??)
                                 (eq (char-syntax (following-char)) ?\\))
                            (and (eq (char-syntax (char-before)) ?\\))))
              (forward-char))
            (setq n (1- n)))
        (sp-backward-symbol n)))))

(put 'sp-forward-symbol 'CUA 'move)

(defun sp-backward-symbol (&optional arg)
  "Move point to the next position that is the beginning of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

A symbol is any sequence of characters that are in either the word
constituent or symbol constituent syntax class.  Current symbol only
extend to the possible opening or closing delimiter as defined by
`sp-add-pair' even if part of this delimiter would match \"symbol\"
syntax classes.

Examples:

  foo bar| baz            -> foo |bar baz

  ((foo bar) baz)|        -> ((foo |bar) baz) ;; 2

  (quux ((foo) bar) baz)| -> (|quux ((foo) bar) baz) ;; 4"
  (interactive "^p")
  (setq arg (or arg 1))
  (sp--with-case-sensitive
    (let ((n (abs arg))
          (fw (> arg 0))
          (open (sp--get-opening-regexp (sp--get-allowed-pair-list)))
          (close (sp--get-closing-regexp (sp--get-allowed-pair-list))))
      (if fw
          (while (> n 0)
            (while (cond
                    ((bobp) nil)
                    ((not (memq (char-syntax (preceding-char)) '(?w ?_)))
                     (backward-char)
                     t)
                    ((sp--valid-initial-delimiter-p (sp--looking-back open))
                     (goto-char (match-beginning 0)))
                    ((sp--valid-initial-delimiter-p (sp--looking-back close))
                     (goto-char (match-beginning 0)))))
            (while (and (not (bobp))
                        (not (or (sp--valid-initial-delimiter-p (sp--looking-back open))
                                 (sp--valid-initial-delimiter-p (sp--looking-back close))))
                        (or (memq (char-syntax (preceding-char)) '(?w ?_))
                            ;; Specifically for lisp, we consider
                            ;; sequences of ?\<ANYTHING> a symbol
                            ;; sequence
                            (and (eq (char-before (1- (point))) ??)
                                 (eq (char-syntax (preceding-char)) ?\\))
                            ))
              (backward-char))
            ;; skip characters which are symbols with prefix flag
            (while (and (not (eobp))
                        (/= 0 (logand (lsh 1 20) (car (syntax-after (point))))))
              (forward-char 1))
            (setq n (1- n)))
        (sp-forward-symbol n)))))

(put 'sp-backward-symbol 'CUA 'move)

(defun sp-rewrap-sexp (pair &optional keep-old)
  "Rewrap the enclosing expression with a different pair.

PAIR is the new enclosing pair.

If optional argument KEEP-OLD is set, keep old delimiter and wrap
with PAIR on the outside of the current expression.

When used interactively, the new pair is specified in minibuffer
by typing the *opening* delimiter, same way as with pair
wrapping.

When used interactively with raw prefix argument \\[universal-argument], KEEP-OLD
is set to non-nil.

Examples:

  (foo |bar baz) -> [foo |bar baz]   ;; [

  (foo |bar baz) -> [(foo |bar baz)] ;; \\[universal-argument] ["
  (interactive (if buffer-read-only
                   (user-error "Buffer is read-only")
                 (list
                  (catch 'done
                    (let ((available-pairs (sp--get-pair-list-context 'wrap))
                          ev ac (pair-prefix ""))
                      (while (not ac)
                        (setq ev (read-event (format "Rewrap with: %s" pair-prefix) t))
                        (if (and (equal pair-prefix "")
                                 (eq ev 'return))
                            (throw 'done nil))
                        (setq pair-prefix (concat pair-prefix (format-kbd-macro (vector ev))))
                        (unless (--any? (string-prefix-p pair-prefix (car it)) available-pairs)
                          (user-error "Impossible pair prefix selected: %s" pair-prefix))
                        (setq ac (--first (equal pair-prefix (car it)) available-pairs)))
                      ac))
                  current-prefix-arg)))
  (if (not pair)
      (sp-unwrap-sexp)
    (-when-let (enc (sp-get-enclosing-sexp))
      (save-excursion
        (sp-get enc
          (goto-char :end)
          (unless keep-old
            (delete-char (- :cl-l)))
          (insert (cdr pair))
          (goto-char :beg)
          (insert (car pair))
          (unless keep-old
            (delete-char :op-l))
          (setq sp-last-wrapped-region
                (sp--make-last-wraped-region
                 :beg (+ :end
                        (length (car pair))
                        (length (cdr pair))
                        (- :op-l)
                        (- :cl-l))
                 (car pair) (cdr pair)))))
      (sp--run-hook-with-args (car pair) :post-handlers 'rewrap-sexp
                              (list :parent (sp-get enc :op))))))

(defun sp-swap-enclosing-sexp (&optional arg)
  "Swap the enclosing delimiters of this and the parent expression.

With N > 0 numeric argument, ascend that many levels before
swapping.

Examples:

  (foo [|bar] baz)              -> [foo (|bar) baz] ;; 1

  (foo {bar [|baz] quux} quack) -> [foo {bar (|baz) quux} quack] ;; 2"
  (interactive "*p")
  (let ((enc (sp-get-enclosing-sexp))
        (encp (sp-get-enclosing-sexp (1+ arg))))
    (if (and enc encp)
        (save-excursion
          (sp-get encp
            (goto-char :end)
            (delete-char (- :cl-l)))
          (sp-get enc
            (insert :cl)
            (goto-char :end)
            (delete-char (- :cl-l)))
          (sp-get encp (insert :cl))
          (sp-get enc (goto-char :beg-prf))
          (sp-get encp (insert :prefix :op))
          (sp-get enc (delete-char (+ :op-l :prefix-l)))
          (sp-get encp (goto-char :beg-prf))
          (sp-get enc (insert :prefix :op))
          (sp-get encp (delete-char (+ :op-l :prefix-l))))
      (sp-message :point-not-deep-enough))))

(defun sp--unwrap-sexp (sexp &optional no-cleanup)
  "Unwrap expression defined by SEXP.

Warning: this function remove possible empty lines and reindents
the unwrapped sexp, so the SEXP structure will no longer
represent a valid object in a buffer!"
  (sp-get sexp
    (delete-region :end-in :end)
    (delete-region :beg-prf :beg-in))
  ;; if the delimiters were the only thing on the line, we should also
  ;; get rid of the (possible) empty line that will be the result of
  ;; their removal.  This is especially nice in HTML mode or
  ;; long-running tags like \[\] in latex.
  (unless no-cleanup
    (let ((new-start (sp-get sexp :beg-prf))
          (new-end (sp-get sexp (- :end-in :op-l :prefix-l)))
          indent-from indent-to)
      (save-excursion
        (goto-char new-end)
        (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
          (let ((b (bounds-of-thing-at-point 'line)))
            (delete-region (car b) (cdr b))))
        (setq indent-to (point))
        (goto-char new-start)
        (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
          (let ((b (bounds-of-thing-at-point 'line)))
            (delete-region (car b) (cdr b))))
        (setq indent-from (point)))
      (unless (or (memq major-mode sp-no-reindent-after-kill-modes)
                  (memq indent-line-function
                        sp-no-reindent-after-kill-indent-line-functions))
        (sp--keep-indentation
          (sp--indent-region indent-from indent-to))))))

(defun sp-change-inner ()
  "Change the inside of the next expression.

First, kill the inside of the next balanced expression, then move
point just after the opening delimiter.

Examples:

  (f|oo [bar] baz) -> (foo [|] baz)

  {|\\='foo\\=': \\='bar\\='}  -> {\\='|\\=': \\='bar\\='}"
  (interactive "*")
  (-when-let (ok (sp-get-sexp))
    (sp-get ok
      (kill-region :beg-in :end-in)
      (goto-char :beg-in))))

(defun sp-change-enclosing ()
  "Change the inside of the enclosing expression.

Whitespace on both sides of the inner items is preserved if it
contains newlines.  Invoking this function on a blank sexp will
wipe out remaining whitespace (see `sp-point-in-blank-sexp').

Move the point to the beginning of the original content.

Examples:

  (f|oo [bar] baz) -> (|)

  {'f|oo': 'bar'}  -> {'|': 'bar'}"
  (interactive "*")
  (-when-let (ok (sp-get-enclosing-sexp))
    (sp-get ok
      (if (sp-point-in-blank-sexp)
          (progn
            (kill-region :beg-in :end-in)
            (goto-char :beg-in))
        (let ((beg (progn
                     (goto-char :beg-in)
                     (skip-chars-forward "\t\n ")
                     (point)))
              (end (progn
                     (goto-char :end-in)
                     (skip-chars-backward "\t\n ")
                     (point))))
          (kill-region beg end)
          (goto-char beg))))))

(defun sp-unwrap-sexp (&optional arg)
  "Unwrap the following expression.

With ARG N, unwrap Nth expression as returned by
`sp-forward-sexp'.  If ARG is negative -N, unwrap Nth expression
backwards as returned by `sp-backward-sexp'.

Return the information about the just unwrapped expression.  Note
that this structure does not represent a valid expression in the
buffer.

Examples:

  |(foo bar baz)     -> |foo bar baz

  (foo bar| baz)     -> foo bar| baz

  |(foo) (bar) (baz) -> |(foo) bar (baz) ;; 2"
  (interactive "*p")
  (setq arg (or arg 1))
  (let ((sp-navigate-consider-symbols nil))
    (let ((ok (save-excursion (sp-forward-sexp arg))))
      (when ok (sp--unwrap-sexp ok))
      ok)))

(defun sp-backward-unwrap-sexp (&optional arg)
  "Unwrap the previous expression.

With ARG N, unwrap Nth expression as returned by
`sp-backward-sexp'.  If ARG is negative -N, unwrap Nth expression
forward as returned by `sp-forward-sexp'.

Examples:

  (foo bar baz)|     -> foo bar baz|

  (foo bar)| (baz)   -> foo bar| (baz)

  (foo) (bar) (baz)| -> foo (bar) (baz) ;; 3"
  (interactive "*p")
  (sp-unwrap-sexp (- (or arg 1))))

(defun sp-splice-sexp (&optional arg)
  "Unwrap the current list.

With ARG N, unwrap Nth list as returned by applying `sp-up-sexp'
N times.  This function expect positive arg.

Examples:

  (foo (bar| baz) quux) -> (foo bar| baz quux)

  (foo |(bar baz) quux) -> foo |(bar baz) quux

  (foo (bar| baz) quux) -> foo (bar| baz) quux ;; 2"
  (interactive "*p")
  (setq arg (or arg 1))
  (-when-let (ok (sp-get-enclosing-sexp arg))
    (if (equal ";" (sp-get ok :prefix))
        (sp-get ok
          (save-excursion
            (goto-char :beg)
            (-when-let (enc (sp-get-enclosing-sexp arg))
              (sp--unwrap-sexp enc))))
      (sp--unwrap-sexp ok))))

(defun sp--splice-sexp-do-killing (beg end expr &optional jump-end)
  "Save the text in the region between BEG and END inside EXPR,
then delete EXPR and insert the saved text.

If optional argument JUPM-END is equal to the symbol \\='end move
the point after the re-inserted text."
  (let (str p)
    (setq str (buffer-substring-no-properties beg end))
    (delete-region (sp-get expr :beg-prf) (sp-get expr :end))
    (save-excursion
      (insert str)
      (sp--indent-region (sp-get expr :beg-prf) (point))
      (setq p (point)))
    (when (eq jump-end 'end) (goto-char p))))

(defun sp-splice-sexp-killing-backward (&optional arg)
  "Unwrap the current list and kill all the expressions
between start of this list and the point.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (foo (let ((x 5)) |(sqrt n)) bar)  -> (foo |(sqrt n) bar)

​  (when ok|                             |(perform-operation-1)
​    (perform-operation-1)            ->  (perform-operation-2)
​    (perform-operation-2))

​  (save-excursion                    -> |(awesome-stuff-happens) ;; 2
​    (unless (test)
​      |(awesome-stuff-happens)))

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-backward-kill-sexp].
See `sp-backward-kill-sexp' for more information."
  (interactive "*p")
  (while (> arg 0)
    (sp-splice-sexp-killing-around '(4))
    (setq arg (1- arg))))

;; TODO: write in terms of `sp-splice-sexp-killing-around'.
(defun sp-splice-sexp-killing-forward (&optional arg)
  "Unwrap the current list and kill all the expressions between
the point and the end of this list.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (a (b c| d e) f) -> (a b c| f)

  (+ (x |y z) w)   -> (+ x| w)

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-kill-sexp].
See `sp-kill-sexp' for more information."
  (interactive "*p")
  (while (> arg 0)
    (let ((ok (sp-get-enclosing-sexp 1)))
      (if ok
          (let ((next (sp-get-thing t)))
            (if (sp-compare-sexps next ok)
                (sp-kill-sexp '(16))
              (sp--splice-sexp-do-killing
               (sp-get next :end) ;search backward
               (sp-get ok :beg-in)
               ok 'end)))
        (setq arg -1)))
    (setq arg (1- arg))))

(defun sp-splice-sexp-killing-around (&optional arg)
  "Unwrap the current list and kill everything inside except next expression.

With ARG save that many next expressions.  With ARG negative -N,
save that many expressions backward.

If ARG is raw prefix argument \\[universal-argument] this function behaves exactly
the same as `sp-splice-sexp-killing-backward'.

If ARG is negative raw prefix argument \\[negative-argument] \\[universal-argument] this function
behaves exactly the same as `sp-splice-sexp-killing-forward'.

Note that the behaviour with the prefix argument seems to be
reversed.  This is because the backward variant is much more
common and hence deserve shorter binding.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] raise the expression the point
is inside of.  This is the same as `sp-backward-up-sexp' followed by
`sp-splice-sexp-killing-around'.

Examples:

  (a b |(c d) e f)      -> |(c d)     ;; with arg = 1

  (a b |c d e f)        -> |c d       ;; with arg = 2

  (- (car x) |a 3)      -> (car x)|   ;; with arg = -1

  (foo (bar |baz) quux) -> |(bar baz) ;; with arg = \\[universal-argument] \\[universal-argument]"
  (interactive "*P")
  (cond
   ((equal arg '(-4))
    (sp-splice-sexp-killing-forward 1))
   (t
    (if (equal arg '(16))
        (progn
          (sp-backward-up-sexp)
          (setq arg 1)))
    (let* (inside-comment-inside-sexp
           (num-arg (prefix-numeric-value arg))
           (ok ;; (sp-get-enclosing-sexp 1)
            (save-excursion
              (sp-skip-backward-to-symbol)
              ;; if the point is inside a comment, we want to
              ;; operate on the sexp that contains it.  however,
              ;; if we are inside a sexp inside a comment, we
              ;; should operate on that instead.
              (if (sp-point-in-comment)
                  (let ((enc (sp-get-enclosing-sexp 1))
                        (cb (sp-get-comment-bounds)))
                    (if (> (sp-get enc :beg) (car cb))
                        (progn
                          (setq inside-comment-inside-sexp t)
                          enc)
                      (goto-char (cdr cb))
                      ;; todo: replace with something more
                      ;; abstract
                      (skip-chars-forward "\t\n ")
                      (sp-get-enclosing-sexp 1)))
                (sp-get-enclosing-sexp 1)))))
      (when ok
        (when (and (sp-point-in-comment)
                   (not inside-comment-inside-sexp))
          (let ((cb (sp-get-comment-bounds)))
            (goto-char (if (> num-arg 0) (car cb) (cdr cb)))))
        (sp-skip-backward-to-symbol)
        (-let* ((next (sp--next-thing-selection arg))
                ((from . to)
                 (cond
                  ((and (sp-point-in-comment)
                        (not inside-comment-inside-sexp))
                   (if (> num-arg 0)
                       ;; only extends to keep the comment if raising
                       ;; towards the end.
                       (cons (car (sp-get-comment-bounds))
                             (sp-get next :end-suf))
                     (sp-get next (cons :beg-prf :end-suf))))
                  ((and (sp-point-in-comment)
                        inside-comment-inside-sexp)
                   (sp-get next (cons :beg-prf :end-suf)))
                  ;; If we are splicing before a comment, the
                  ;; comment might be connected to the sexp
                  ;; after it, so we better don't kill it.  Only
                  ;; do that if the comment is on its own line
                  ;; though, otherwise it is connected to the
                  ;; sexp before it.
                  ((save-excursion
                     (skip-chars-forward "\t\n ")
                     (when (and (> num-arg 0)
                                (sp-point-in-comment)
                                (save-excursion
                                  (skip-chars-backward "\t ")
                                  (bolp)))
                       (cons (point) (sp-get next :end-suf)))))
                  ;; similarly, if there is a comment before
                  ;; this sexp, keep it.
                  ((save-excursion
                     (sp-backward-symbol)
                     (when (and (> num-arg 0)
                                (sp-point-in-comment)
                                (goto-char (car (sp-get-comment-bounds)))
                                (> (point) (sp-get ok :beg))
                                (save-excursion
                                  (skip-chars-backward "\t ")
                                  (bolp)))
                       (cons (point) (sp-get next :end-suf)))))
                  (t (sp-get next (cons :beg-prf :end-suf))))))
          (sp--splice-sexp-do-killing from to
                                      ok (if (> num-arg 0) nil 'end))))))))

(defalias 'sp-raise-sexp 'sp-splice-sexp-killing-around)

(defun sp-convolute-sexp (&optional arg)
  "Convolute balanced expressions.

Save the expressions preceding point and delete them.  Then
splice the resulting expression.  Wrap the current enclosing list
with the delimiters of the spliced list and insert the saved
expressions.

If point is in a symbol, move to end of symbol before convolving.

With ARG positive N, move up N lists before wrapping.

Examples:

We want to move the `while' before the `let'.

​  (let ((stuff 1)             (while (we-are-good)
​        (other 2))              (let ((stuff 1)
​    (while (we-are-good)  ->          (other 2))
​     |(do-thing 1)               |(do-thing 1)
​      (do-thing 2)                (do-thing 2)
​      (do-thing 3)))              (do-thing 3)))

  (forward-char (sp-get env |:op-l)) -> (sp-get env (forward-char |:op-l))"
  (interactive "*p")
  (save-excursion
    (when (sp-point-in-symbol)
      (sp-forward-symbol))
    (when (looking-at-p " ")
      (just-one-space))
    (let* ((old-buffer-size (buffer-size))
           (enc (sp-get-enclosing-sexp))
           (inner-close (sp-get enc (delete-and-extract-region
                                     (save-excursion
                                       (goto-char :end-in)
                                       (sp-backward-whitespace))
                                     :end)))
           (inner-raise (sp-get enc (delete-and-extract-region
                                     :beg-prf
                                     (save-excursion
                                       (sp-forward-whitespace)))))
           (whitespace (sp-get enc
                         ;; this happens when the entire inside sexp was removed.
                         (when (= old-buffer-size (+ (buffer-size) :len))
                           (delete-and-extract-region
                            (save-excursion
                              (goto-char :beg-prf)
                              (max (line-beginning-position) (sp-backward-whitespace)))
                            :beg-prf))))
           (encp (sp-get-enclosing-sexp arg)))
      (sp-get encp
        (goto-char :end)
        (insert inner-close)
        (goto-char :beg-prf)
        (insert inner-raise (if whitespace whitespace ""))
        (sp-get (sp-get-enclosing-sexp)
          (sp--indent-region :beg :end)))))
  (indent-according-to-mode))

(defun sp-absorb-sexp (&optional arg)
  "Absorb previous expression.

Save the expressions preceding point and delete them.  Then slurp
an expression backward and insert the saved expressions.

With ARG positive N, absorb that many expressions.

Examples:

​  (do-stuff 1)         (save-excursion
​  (save-excursion  ->   |(do-stuff 1)
​   |(do-stuff 2))        (do-stuff 2))

  foo bar (concat |baz quux) -> (concat |foo bar baz quux) ;; 2"
  (interactive "*p")
  (sp-forward-whitespace)
  (let* ((old (point))
         (raise (progn
                  (sp-beginning-of-sexp)
                  (buffer-substring (point) old))))
    (delete-region (point) old)
    (sp-backward-slurp-sexp arg)
    (sp-forward-whitespace)
    (sp-beginning-of-sexp)
    (insert raise)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp)))
  (sp-forward-whitespace))

(defun sp-emit-sexp (&optional arg)
  "Move all expression preceding point except the first one out of
the current list.

With ARG positive N, keep that many expressions from the start of
the current list.

This is similar as `sp-backward-barf-sexp' but it also drags the
first N expressions with the delimiter.

Examples:

​  (save-excursion     ​(do-stuff 1)
​    (do-stuff 1)      (do-stuff 2)
​    (do-stuff 2)  ->  (save-excursion
​   |(do-stuff 3))      |(do-stuff 3))

​  (while not-done-yet       (execute-only-once)
​    (execute-only-once) ->  (while not-done-yet    ;; arg = 2
​   |(execute-in-loop))       |(execute-in-loop))"
  (interactive "*p")
  (let (save-text)
    (save-excursion
      (sp-beginning-of-sexp)
      (let* ((start (point)))
        (sp-forward-sexp arg)
        (sp-skip-forward-to-symbol t)
        (setq save-text (buffer-substring start (point)))
        (delete-region start (point))))
    (save-excursion (sp-backward-barf-sexp '(4)))
    (sp-down-sexp)
    (insert save-text)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp))))

(defun sp-extract-before-sexp (&optional arg)
  "Move the expression after point before the enclosing balanced expression.

The point moves with the extracted expression.

With ARG positive N, extract N expressions after point.

With ARG negative -N, extract N expressions before point.

With ARG being raw prefix argument \\[universal-argument], extract all the expressions
up until the end of enclosing list.

If the raw prefix is negative, this behaves as \\[universal-argument] `sp-backward-barf-sexp'."
  (interactive "*P")
  (if (equal arg '(-4))
      (sp-backward-barf-sexp '(4))
    (sp-select-next-thing arg)
    (let ((enc (sp-get-enclosing-sexp))
          save-text b e nl)
      (save-excursion
        ;; TODO: extract this use pattern into general "get X things
        ;; with or without surrounding whitespace."
        (setq b (region-beginning))
        (setq e (region-end))
        (goto-char (sp-get enc :end-in))
        (if (save-excursion
              (skip-chars-backward "\t ")
              (bolp))
            (let ((whitespace (sp-get-whitespace)))
              (sp-get whitespace (when (= :beg e)
                                   (delete-region :beg :end))))
          (setq nl t))
        (setq save-text (delete-and-extract-region b e))
        (when nl
          (let ((whitespace (sp-get-whitespace)))
            (sp-get whitespace (delete-region :beg :end))))
        (goto-char (sp-get enc :beg-prf))
        (insert save-text "\n")
        (sp-get enc (sp--indent-region :beg-prf :end)))
      ;; if we're at an empty line, remove it
      (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
        (let ((b (bounds-of-thing-at-point 'line)))
          (delete-region (car b) (cdr b))))
      (goto-char (sp-get enc :beg-prf)))))

(defun sp-extract-after-sexp (&optional arg)
  "Move the expression after point after the enclosing balanced expression.

The point moves with the extracted expression.

With ARG positive N, extract N expressions after point.

With ARG negative -N, extract N expressions before point.

With ARG being raw prefix argument \\[universal-argument], extract all the
expressions up until the end of enclosing list.

With ARG being negative raw prefix argument \\[negative-argument] \\[universal-argument], extract all the
expressions up until the start of enclosing list."
  ;; this is uch uglier than the "before" version, since the
  ;; calculations forward have to account for the deleted text. Figure
  ;; out a way to make it smoother.
  (interactive "*P")
  (sp-select-next-thing arg)
  (sp--with-case-sensitive
    (let ((enc (sp-get-enclosing-sexp))
          (dws 0)                       ;length of deleted whitespace
          save-text b e nl)
      (save-excursion
        (setq b (region-beginning))
        (setq e (region-end))
        (goto-char (sp-get enc :end-in))
        (if (save-excursion
              (skip-chars-backward "\t ")
              (bolp))
            (let ((whitespace (sp-get-whitespace)))
              (sp-get whitespace
                (when (= :beg e)
                  (delete-region :beg :end)
                  (setq dws (- :end :beg)))))
          (setq nl t))
        (setq save-text (delete-and-extract-region b e))
        (when nl
          (let ((whitespace (sp-get-whitespace)))
            (sp-get whitespace (delete-region :beg :end))
            (sp-get whitespace (setq dws (+ dws (- :end :beg))))))
        (sp-get enc (goto-char (- :end (length save-text) dws)))
        (insert "\n" save-text)
        (sp-get enc (sp--indent-region :beg-prf :end))
        (setq e (point)))
      ;; if we're at an empty line, remove it
      (setq dws 0)                      ; variable reuse, ugly :/
      (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
        (let ((b (bounds-of-thing-at-point 'line)))
          (delete-region (car b) (cdr b))
          (setq dws (- (cdr b) (car b)))))
      (when (sp--looking-back (sp--get-opening-regexp) nil t)
        (let ((whitespace (sp-get-whitespace)))
          (sp-get whitespace
            (delete-region :beg :end)
            (setq dws (- :end :beg)))))
      (goto-char (- e dws)))))

(defun sp-forward-whitespace (&optional arg)
  "Skip forward past the whitespace characters.
With non-nil ARG return number of characters skipped."
  (interactive "^P")
  (let ((rel-move (skip-chars-forward " \t\n")))
    (if arg rel-move (point))))

(put 'sp-forward-whitespace 'CUA 'move)

(defun sp-backward-whitespace (&optional arg)
  "Skip backward past the whitespace characters.
With non-nil ARG return number of characters skipped."
  (interactive "^P")
  (let ((rel-move (skip-chars-backward " \t\n")))
    (if arg rel-move (point))))

(put 'sp-backward-whitespace 'CUA 'move)

(defun sp-split-sexp (arg)
  "Split the list or string the point is on into two.

If ARG is a raw prefix \\[universal-argument] split all the sexps in current expression
in separate lists enclosed with delimiters of the current
expression.

See also setting `sp-split-sexp-always-split-as-string' which
determines how sexps inside strings are treated and also for a
discussion of how to automatically add concatenation operators to
string splitting.

Examples:

  (foo bar |baz quux)   -> (foo bar) |(baz quux)

  \"foo bar |baz quux\"   -> \"foo bar\" |\"baz quux\"

  ([foo |bar baz] quux) -> ([foo] |[bar baz] quux)

  (foo bar| baz quux) -> (foo) (bar|) (baz) (quux) ;; \\[universal-argument]"
  (interactive "*P")
  (cond
   ((equal arg '(4))
    (-when-let ((first-item . rest-items) (sp-get-list-items))
      (sp-get first-item
        (save-excursion
          (goto-char :end)
          (delete-char (- (length :cl)))
          (--each (nreverse rest-items)
            (goto-char (sp-get it :end))
            (insert :cl)
            (goto-char (sp-get it :beg))
            (insert :op))
          (goto-char :beg)
          (delete-char (length :op))))))
   (t
    (let ((should-split-as-string
           (and sp-split-sexp-always-split-as-string
                (sp-point-in-string))))
      (-when-let (ok (if should-split-as-string
                         (save-excursion
                           (goto-char (car (sp-get-quoted-string-bounds)))
                           (sp-get-sexp))
                       (sp-get-enclosing-sexp 1)))
        (sp-get ok
          (sp--run-hook-with-args :op :pre-handlers 'split-sexp)
          (if should-split-as-string
              (progn
                (insert :cl)
                (save-excursion (insert :op)))
            (forward-char (- (prog1 (sp-backward-whitespace t) (insert :cl))))
            (save-excursion (sp-forward-whitespace) (insert :op)))
          (sp--run-hook-with-args :op :post-handlers 'split-sexp)))))))

(defun sp--join-sexp (prev next)
  "Join the expressions PREV and NEXT if they are of the same type.

The expression with smaller :beg is considered the previous one,
so the input order does not actually matter.

Return the information about resulting expression."
  (if (and (sp-compare-sexps prev next equal :op)
           (sp-compare-sexps prev next equal :cl))
      ;; if there's some prefix on the second expression, remove it.
      ;; We do not move it to the first expression, it is assumed
      ;; there's one already
      (progn
        (if (sp-compare-sexps prev next >)
            (let ((tmp prev))
              (setq prev next)
              (setq next tmp)))
        (sp-get next (delete-region :beg-prf :beg-in))
        (sp-get prev (delete-region :end-in :end))
        (list :beg (sp-get prev :beg)
              :end (- (sp-get next (- :end :op-l :prefix-l)) (sp-get prev :cl-l))
              :op (sp-get prev :op)
              :cl (sp-get prev :cl)
              :prefix (sp-get prev :prefix)))
    (sp-message :different-type)))

(defun sp-join-sexp (&optional arg)
  "Join the sexp before and after point if they are of the same type.

If ARG is positive N, join N expressions after the point with the
one before the point.

If ARG is negative -N, join N expressions before the point with
the one after the point.

If ARG is a raw prefix \\[universal-argument] join all the things up until the end
of current expression.

The joining stops at the first expression of different type.

Examples:

  (foo bar) |(baz)                    -> (foo bar |baz)

  (foo) |(bar) (baz)                  -> (foo |bar baz) ;; 2

  [foo] [bar] |[baz]                  -> [foo bar |baz] ;; -2

  (foo bar (baz)| (quux) (blob bluq)) -> (foo bar (baz| quux blob bluq)) ;; \\[universal-argument]"
  (interactive "*P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (prev (save-excursion (sp-backward-sexp (sp--signum arg))))
         next)
    (save-excursion
      (cond
       ((and raw (= n 4))
        (setq next (sp-forward-sexp (sp--signum arg)))
        (while (cond
                ((> arg 0)
                 (sp-compare-sexps next prev > :beg :end))
                ((< arg 0)
                 (sp-compare-sexps next prev < :end :beg)))
          (setq prev (sp--join-sexp prev next))
          (setq next (sp-forward-sexp (sp--signum arg)))))
       (t (while (> n 0)
            (setq next (sp-forward-sexp (sp--signum arg)))
            (setq prev (sp--join-sexp prev next))
            (setq n (1- n)))))
      prev)))

(defun sp--next-thing-selection (&optional arg point)
  "Return the bounds of selection over next thing.

See `sp-select-next-thing' for the meaning of ARG.

If POINT is non-nil, it is assumed it's a point inside the buffer
from which the selection extends, either forward or backward,
depending on the value of ARG.

The return value has the same format as `sp-get-sexp'.  This does
not necessarily represent a valid balanced expression!"
  (save-excursion
    (let* ((raw (sp--raw-argument-p arg))
           (arg (prefix-numeric-value arg))
           (beg point) (end point)
           (op "") (cl "")
           (prefix "")
           (suffix ""))
      (cond
       ;; select up until end of list
       ((and raw (= arg 4))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :end-in))
              (-when-let (ok (sp-get-thing t))
                (sp-get ok
                  (setq end :end)
                  (setq cl :cl)
                  (setq suffix :suffix)))))
          (unless point
            (-when-let (ok (sp-get-thing))
              (if (sp-compare-sexps ok enc)
                  (progn
                    (setq beg end)
                    (setq end (sp-get enc :end-in)))
                (sp-get ok
                  (setq beg :beg)
                  (setq op :op)
                  (setq prefix :prefix)))))))
       ;; select up until beg of list
       ((and raw (= arg -4))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :beg-in))
              (-when-let (ok (sp-get-thing))
                (sp-get ok
                  (setq beg :beg)
                  (setq op :op)
                  (setq prefix :prefix))))))
        (unless point
          (-when-let (ok (sp-get-thing t))
            (sp-get ok
              (setq end :end)
              (setq cl :cl)
              (setq suffix :suffix)))))
       ;; select the enclosing expression
       ((and raw (= (abs arg) 16))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (sp-get enc (setq beg :beg) (setq end :end)
                    (setq op :op) (setq cl :cl)
                    (setq prefix :prefix)
                    (setq suffix :suffix)))))
       ;; normal selection, select N expressions
       ((> arg 0)
        (let* ((first (sp-forward-sexp))
               (last first))
          (setq arg (1- arg))
          (setq beg (or point (sp-get first :beg)))
          (while (and (> arg 0) last)
            (setq last (sp-forward-sexp))
            (let ((nb (sp-get last :beg))) (when (< nb beg)
                                             (setq first last)
                                             (setq beg nb)))
            (setq arg (1- arg)))
          (unless (and point (= point beg))
            (sp-get first
              (setq beg :beg)
              (setq op :op)
              (setq prefix :prefix)))
          (sp-get last
            (setq end :end)
            (setq cl :cl)
            (setq suffix :suffix))))
       ;; normal select, select -N expressions
       ((< arg 0)
        (let* ((first (sp-backward-sexp))
               (last first))
          (setq arg (1+ arg))
          (setq end (or point (sp-get first :end)))
          (while (and (< arg 0) last)
            (setq last (sp-backward-sexp))
            (let ((ne (sp-get last :end))) (when (> ne end)
                                             (setq first last)
                                             (setq end ne)))
            (setq arg (1+ arg)))
          (sp-get last
            (setq beg :beg)
            (setq op :op)
            (setq prefix :prefix))
          (unless (and point (= point end))
            (sp-get first
              (setq end :end)
              (setq cl :cl)
              (setq suffix :suffix)))))
       ;; N = 0, select insides
       ((= arg 0)
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :beg-in))
              (-when-let (ok (sp-get-thing))
                (sp-get ok
                  (setq beg :beg)
                  (setq op :op)
                  (setq prefix :prefix))))
            (save-excursion
              (goto-char (sp-get enc :end-in))
              (-when-let (ok (sp-get-thing t))
                (sp-get ok
                  (setq end :end)
                  (setq cl :cl)
                  (setq suffix :suffix))))))))
      (list :beg beg :end end :op op :cl cl :prefix prefix :suffix suffix))))

(defun sp-select-next-thing (&optional arg point)
  "Set active region over next thing as recognized by `sp-get-thing'.

If ARG is positive N, select N expressions forward.

If ARG is negative -N, select N expressions backward.

If ARG is a raw prefix \\[universal-argument] select all the things up until the
end of current expression.

If ARG is a raw prefix \\[universal-argument] \\[universal-argument] select the current expression (as
if doing `sp-backward-up-sexp' followed by
`sp-select-next-thing').

If ARG is number 0 (zero), select all the things inside the
current expression.

If POINT is non-nil, it is assumed it's a point inside the buffer
from which the selection extends, either forward or backward,
depending on the value of ARG.

If the currently active region contains a balanced expression,
following invocation of `sp-select-next-thing' will select the
inside of this expression .  Therefore calling this function
twice with no active region will select the inside of the next
expression.

If the point is right in front of the expression any potential
prefix is ignored.  For example, '|(foo) would only select (foo)
and not include ' in the selection.  If you wish to also select
the prefix, you have to move the point backwards.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (let* ((selection (sp--next-thing-selection arg point))
         (p (point))
         (b (sp-get selection :beg))
         (e (sp-get selection :end))
         contracted)
    ;; Show a helpful error if we're trying to move beyond the
    ;; beginning or end of the buffer.
    (when (or (null b) (null e))
      (user-error (if (bobp) "At beginning of buffer" "At end of buffer")))
    ;; if region is active and ready to use, check if this selection
    ;; == old selection.  If so, reselect the insides
    (when (region-active-p)
      (let ((rb (region-beginning))
            (re (region-end)))
        (when (and (sp-get selection
                     (or (= rb :beg)
                         (= rb :beg-prf)))
                   (= re (sp-get selection :end)))
          (sp-get selection
            (setq b :beg-in)
            (setq e :end-in))
          (setq contracted t))))
    ;; if we moved forward check if the old-point was in front of an
    ;; expression and after a prefix. If so, remove the prefix from
    ;; the selection
    (unless (and (> (prefix-numeric-value arg) 0)
                 (not (sp--raw-argument-p arg))
                 (= b p))
      (unless contracted (setq b (sp-get selection :beg-prf))))
    (push-mark b t t)
    (goto-char e)
    selection))

(defun sp-select-previous-thing (&optional arg point)
  "Set active region over ARG previous things as recognized by `sp-get-thing'.

If ARG is negative -N, select that many expressions forward.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (sp-select-next-thing (sp--negate-argument arg) point))

(defun sp-select-next-thing-exchange (&optional arg point)
  "Just like `sp-select-next-thing' but run `exchange-point-and-mark'
afterwards."
  (interactive "P")
  (prog1
      (sp-select-next-thing arg point)
    (exchange-point-and-mark)))

(defun sp-select-previous-thing-exchange (&optional arg point)
  "Just like `sp-select-previous-thing' but run `exchange-point-and-mark'
afterwards."
  (interactive "P")
  (prog1
      (sp-select-previous-thing arg point)
    (exchange-point-and-mark)))

(defun sp-mark-sexp (&optional arg allow-extend)
  "Set mark ARG balanced expressions from point.
The place mark goes is the same place \\[sp-forward-sexp] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG sexps after the ones already marked.
This command assumes point is not in a string or comment."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (let ((p (point)))
              (goto-char (mark))
              (sp-forward-sexp arg)
              (unless (sp-region-ok-p p (point))
                (user-error "Can not extend selection: region invalid"))
              (point)))))
        (t
         (push-mark
          (save-excursion
            (sp-forward-sexp (prefix-numeric-value arg))
            (point))
          nil t))))

(defun sp-delete-char (&optional arg)
  "Delete a character forward or move forward over a delimiter.

If on an opening delimiter, move forward into balanced expression.

If on a closing delimiter, refuse to delete unless the balanced
expression is empty, in which case delete the entire expression.

If the delimiter does not form a balanced expression, it will be
deleted normally.

With a numeric prefix argument N > 0, delete N characters forward.

With a numeric prefix argument N < 0, delete N characters backward.

With a numeric prefix argument N = 0, simply delete a character
forward, without regard for delimiter balancing.

If ARG is raw prefix argument \\[universal-argument], delete
characters forward until a closing delimiter whose deletion would
break the proper pairing is hit.

Examples:

 (quu|x \"zot\") -> (quu| \"zot\")

 (quux |\"zot\") -> (quux \"|zot\") -> (quux \"|ot\")

 (foo (|) bar) -> (foo | bar)

 |(foo bar) -> (|foo bar)"
  (interactive "*P")
  (sp--with-case-sensitive
    (let* ((raw (sp--raw-argument-p arg))
           ;; if you edit 10 gigabyte files in Emacs, you're gonna have
           ;; a bad time.
           (n (if raw 100000000
                (prefix-numeric-value arg))))
      (cond
       ((> n 0)
        (while (> n 0)
          (cond
           ((let ((ok (sp-point-in-empty-sexp)))
              (when ok
                (backward-char (length (car ok)))
                (delete-char (+ (length (car ok)) (length (cdr ok)))))
              ok)
            ;; make this customizable
            (setq n (1- n)))
           ((and (sp-point-in-string)
                 (save-excursion (forward-char) (not (sp-point-in-string))))
            (setq n 0))
           ((sp--looking-at (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
            (-if-let (thing (save-match-data (sp-get-thing)))
                (cond
                 ((= (sp-get thing :end-in) (point))
                  (setq n 0))
                 ((= (sp-get thing :beg) (point))
                  (goto-char (sp-get thing :beg-in)))
                 (t
                  (delete-char (length (match-string 0)))))
              (delete-char (length (match-string 0))))
            ;; make this customizable
            (setq n (1- n)))
           ((and (not (sp-point-in-string))
                 (save-excursion (forward-char) (sp-point-in-string)))
            (forward-char)
            ;; make this customizable
            (setq n (1- n)))
           ((sp--looking-at (sp--get-closing-regexp (sp--get-pair-list-context 'navigate)))
            (if (save-match-data (sp-get-thing))
                ;; make this customizable -- maybe we want to skip and
                ;; continue deleting
                (setq n 0)
              (delete-char (length (match-string 0)))
              (setq n (1- n))))
           ((bound-and-true-p hungry-delete-mode)
            (hungry-delete-forward 1)
            (setq n (1- n)))
           (t
            (delete-char 1)
            (setq n (1- n))))))
       ((= n 0) (delete-char 1))
       (t (sp-backward-delete-char (sp--negate-argument arg)))))))

(defun sp-backward-delete-char (&optional arg)
  "Delete a character backward or move backward over a delimiter.

If on a closing delimiter, move backward into balanced expression.

If on a opening delimiter, refuse to delete unless the balanced
expression is empty, in which case delete the entire expression.

If the delimiter does not form a balanced expression, it will be
deleted normally.

With a numeric prefix argument N > 0, delete N characters backward.

With a numeric prefix argument N < 0, delete N characters forward.

With a numeric prefix argument N = 0, simply delete a character
backward, without regard for delimiter balancing.

If ARG is raw prefix argument \\[universal-argument], delete
characters backward until a opening delimiter whose deletion would
break the proper pairing is hit.

Examples:

 (\"zot\" q|uux) -> (\"zot\" |uux)

 (\"zot\"| quux) -> (\"zot|\" quux) -> (\"zo|\" quux)

 (foo (|) bar) -> (foo | bar)

 (foo bar)| -> (foo bar|)"
  (interactive "*P")
  (if (and sp-autodelete-wrap
           (eq sp-last-operation 'sp-wrap-region))
      (sp-backward-unwrap-sexp)
    (sp--with-case-sensitive
      (let* ((raw (sp--raw-argument-p arg))
             ;; if you edit 10 gigabyte files in Emacs, you're gonna have
             ;; a bad time.
             (n (if raw 100000000
                  (prefix-numeric-value arg))))
        (cond
         ((> n 0)
          (while (> n 0)
            (cond
             ((let ((ok (sp-point-in-empty-sexp)))
                (when ok
                  (backward-char (length (car ok)))
                  (delete-char (+ (length (car ok)) (length (cdr ok)))))
                ok)
              ;; make this customizable
              (setq n (1- n)))
             ((and (sp-point-in-string)
                   (save-excursion (backward-char) (not (sp-point-in-string))))
              (setq n 0))
             ((sp--looking-back (sp--get-closing-regexp (sp--get-pair-list-context 'navigate)))
              (-if-let (thing (save-match-data (sp-get-thing t)))
                  (cond
                   ((= (sp-get thing :end) (point))
                    (goto-char (sp-get thing :end-in)))
                   ((= (sp-get thing :beg-in) (point))
                    (setq n 0))
                   (t
                    (delete-char (- (length (match-string 0))))))
                (delete-char (- (length (match-string 0)))))
              ;; make this customizable
              (setq n (1- n)))
             ((and (not (sp-point-in-string))
                   (save-excursion (backward-char) (sp-point-in-string)))
              (backward-char)
              ;; make this customizable
              (setq n (1- n)))
             ((sp--looking-back (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
              (if (save-match-data (sp-get-thing t))
                  ;; make this customizable -- maybe we want to skip and
                  ;; continue deleting
                  (setq n 0)
                (delete-char (- (length (match-string 0))))
                (setq n (1- n))))
             ((bound-and-true-p hungry-delete-mode)
              (hungry-delete-backward 1)
              (setq n (1- n)))
             (t
              (delete-char -1)
              (setq n (1- n))))))
         ((= n 0) (delete-char -1))
         (t (sp-delete-char (sp--negate-argument arg))))))))

(put 'sp-backward-delete-char 'delete-selection 'supersede)
(put 'sp-delete-char 'delete-selection 'supersede)

(defun sp-point-in-empty-sexp (&optional pos)
  "Return non-nil if point is in empty sexp or string.

The return value is active cons pair of opening and closing sexp
delimiter enclosing this sexp."
  (setq pos (or pos (point)))
  (let (op act)
    (cond
     ((sp--looking-back (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
      (setq op (match-string 0))
      (setq act (--first (equal (car it) op) sp-pair-list))
      (when (sp--looking-at (regexp-quote (cdr act))) act))
     ((sp-point-in-empty-string pos)))))

(defun sp-point-in-empty-string (&optional pos)
  "Return non-nil if point is in empty string.

The return value is actually cons pair of opening and closing
string delimiter enclosing this string."
  (setq pos (or pos (point)))
  (when (and (sp-point-in-string)
             (save-excursion (if (= (point-max) (point))
                                 t
                               (forward-char) (not (sp-point-in-string))))
             (save-excursion (backward-char) (not (sp-point-in-string))))
    (save-excursion
      (let* ((syntax (nth 3 (syntax-ppss pos)))
             (c (char-to-string (if (eq syntax t) (following-char) syntax))))
        (cons c c)))))

(defun sp--use-subword ()
  "Return non-nil if word killing commands should kill subwords.
This is the case if `subword-mode' is enabled and
`sp-use-subword' is non-nil."
  (and sp-use-subword (bound-and-true-p subword-mode)))

(defun sp--kill-word (&optional n)
  "Kill N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-kill n)
      (kill-word n))))

(defun sp--forward-word (&optional n)
  "Move forward N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-forward n)
      (forward-word n))))

(defun sp--backward-word (&optional n)
  "Move backward N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-backward n)
      (backward-word n))))

(defun sp-kill-symbol (&optional arg word)
  "Kill a symbol forward, skipping over any intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

See `sp-forward-symbol' for what constitutes a symbol."
  (interactive "*p")
  (sp--with-case-sensitive
    (if (> arg 0)
        (while (> arg 0)
          (-when-let (s (sp-get-symbol))
            (sp-get s
              (let* ((beg
                      (if (< :beg-prf (point))
                          (if word (point) :beg)
                        (if (= (save-excursion
                                 (sp-skip-forward-to-symbol)
                                 (point))
                               :beg-prf)
                            (point)
                          :beg-prf)))
                     (end (if word
                              (let ((fw-end
                                     (save-excursion
                                       (sp--forward-word)
                                       (point))))
                                (if (sp-region-ok-p beg fw-end)
                                    fw-end
                                  :end-suf))
                            :end-suf)))
                (goto-char beg)
                (kill-region beg end))))
          (sp--cleanup-after-kill)
          (setq arg (1- arg)))
      (sp-backward-kill-symbol (sp--negate-argument arg) word))))

(defun sp-kill-word (&optional arg)
  "Kill a word forward, skipping over intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "*p")
  (sp-kill-symbol arg t))

(defun sp-delete-symbol (&optional arg word)
  "Delete a symbol forward, skipping over any intervening delimiters.

Deleted symbol does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

See `sp-forward-symbol' for what constitutes a symbol."
  (interactive "*p")
  (sp-save-kill-ring
    (sp-kill-symbol arg word)))

(defun sp-delete-word (&optional arg)
  "Delete a word forward, skipping over intervening delimiters.

Deleted word does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "*p")
  (sp-delete-symbol arg t))

(defun sp-backward-kill-symbol (&optional arg word)
  "Kill a symbol backward, skipping over any intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

See `sp-backward-symbol' for what constitutes a symbol."
  (interactive "*p")
  (sp--with-case-sensitive
    (if (> arg 0)
        (while (> arg 0)
          (-when-let (s (sp-get-symbol t))
            (sp-get s
              (let* ((end
                      (if (< (point) :end-suf)
                          (if word (point) :end-suf)
                        (if (= (save-excursion
                                 (sp-skip-backward-to-symbol)
                                 (point))
                               :end-suf)
                            (point)
                          :end-suf)))
                     (beg (if word
                              (let ((bw-start
                                     (save-excursion
                                       (sp--backward-word)
                                       (point))))
                                (if (sp-region-ok-p bw-start end)
                                    bw-start
                                  :beg-prf))
                            :beg-prf)))
                (goto-char end)
                (kill-region end beg))))
          (sp--cleanup-after-kill)
          (setq arg (1- arg)))
      (sp-kill-symbol (sp--negate-argument arg) word))))

(defun sp-backward-kill-word (&optional arg)
  "Kill a word backward, skipping over intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "*p")
  (sp-backward-kill-symbol arg t))

(defun sp-backward-delete-symbol (&optional arg word)
  "Delete a symbol backward, skipping over any intervening delimiters.

Deleted symbol does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

See `sp-backward-symbol' for what constitutes a symbol."
  (interactive "*p")
  (sp-save-kill-ring
    (sp-backward-kill-symbol arg word)))

(defun sp-backward-delete-word (&optional arg)
  "Delete a word backward, skipping over intervening delimiters.

Deleted word does not go to the clipboard or kill ring.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "*p")
  (sp-backward-delete-symbol arg t))

(defun sp-delete-region (beg end)
  "Delete the text between point and mark, like `delete-region'.

BEG and END are the bounds of region to be deleted.

If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "*r")
  (when (or current-prefix-arg
            (sp-region-ok-p beg end)
            (user-error (sp-message :unbalanced-region :return)))
    (setq this-command 'delete-region)
    (delete-region beg end)))

(defun sp-kill-region (beg end)
  "Kill the text between point and mark, like `kill-region'.

BEG and END are the bounds of region to be killed.

If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "*r")
  (when (or current-prefix-arg
            (sp-region-ok-p beg end)
            (user-error (sp-message :unbalanced-region :return)))
    (setq this-command 'kill-region)
    (kill-region beg end)))

(defun sp-indent-defun (&optional arg)
  "Reindent the current defun.

If point is inside a string or comment, fill the current
paragraph instead, and with ARG, justify as well.

Otherwise, reindent the current defun, and adjust the position
of the point."
  (interactive "*P")
  (if (sp-point-in-string-or-comment)
      (fill-paragraph arg)
    (let ((column (current-column))
          (indentation (sp--current-indentation)))
      (save-excursion
        (end-of-defun)
        (beginning-of-defun)
        (indent-sexp))
      (sp--back-to-indentation column indentation))))

(cl-defun sp-region-ok-p (start end)
  "Test if region between START and END is balanced.

A balanced region is one where all opening delimiters are matched
by closing delimiters.

This function does *not* check that the delimiters are correctly
ordered, that is [(]) is correct even though it is not logically
properly balanced."
  (interactive "r")
  (save-excursion
    (save-restriction
      (when (eq (sp-point-in-string start) (sp-point-in-string end))
        (narrow-to-region start end)
        (let ((regex (sp--get-allowed-regexp (-difference sp-pair-list (sp--get-allowed-pair-list)))))
          (goto-char (point-min))
          (while (or (prog1 (sp-forward-sexp)
                       (sp-skip-forward-to-symbol))
                     ;; skip impossible delimiters
                     (when (looking-at-p regex)
                       (goto-char (match-end 0)))))
          (looking-at-p "[[:blank:]\n]*\\'"))))))

(defun sp-newline ()
  "Insert a newline and indent it.

This is like `newline-and-indent', but it not only indents the
line that the point is on but also the S-expression following the
point, if there is one.

If in a string, just insert a literal newline.

If in a comment and if followed by invalid structure, call
`indent-new-comment-line' to keep the invalid structure in a
comment."
  (interactive "*")
  (cond
   ((sp-point-in-string)
    (newline))
   ((sp-point-in-comment)
    (if (sp-region-ok-p (point) (point-at-eol))
        (progn (newline-and-indent) (ignore-errors (indent-sexp)))
      (indent-new-comment-line)))
   (t
    (newline-and-indent)
    (ignore-errors (indent-sexp)))))

(defun sp-comment ()
  "Insert the comment character and adjust hanging sexps such
  that it doesn't break structure."
  (interactive "*")
  (if (sp-point-in-string-or-comment)
      (if (= 1 (length (single-key-description last-command-event))) ;; pretty hacky
          (insert (single-key-description last-command-event))
        (insert comment-start))
    (sp--with-case-sensitive
      (let ((old-point (point))
            (column (current-column))
            (indentation (sp--current-indentation))
            (old-line (line-number-at-pos))
            (hsexp (sp-get-hybrid-sexp))
            (newline-inserted 0))
        (goto-char (sp-get hsexp :end))
        (if (and (sp--looking-at-p (concat "\\s-*" (sp--get-closing-regexp)))
                 (= old-line (line-number-at-pos)))
            (progn
              (setq old-point (point))
              (newline)
              (setq newline-inserted (1+ (- (line-end-position) (point)))))
          (when (/= old-line (line-number-at-pos))
            (sp-backward-sexp)
            (setq old-point (+ old-point (skip-syntax-backward " ")))
            (newline)
            (setq newline-inserted (- (line-end-position) (point)))))
        ;; @{ indenting madness
        (goto-char old-point)
        (sp-get hsexp (sp--indent-region :beg (+ :end newline-inserted)))
        (sp--back-to-indentation column indentation)
        ;; @}
        (let ((comment-delim (or (cdr (--first (memq major-mode (car it)) sp-comment-string))
                                 comment-start)))
          (when (and (/= 0 (current-column))
                     (not (sp--looking-back-p "\\s-")))
            (insert " "))
          (insert comment-delim)
          (when (/= newline-inserted 0)
            (save-excursion
              (forward-line 1)
              (indent-according-to-mode))))))))

(defun sp-wrap-round ()
  "Wrap following sexp in round parentheses."
  (interactive "*")
  (sp-wrap-with-pair "("))

(defun sp-wrap-square ()
  "Wrap following sexp in square brackets."
  (interactive "*")
  (sp-wrap-with-pair "["))

(defun sp-wrap-curly ()
  "Wrap following sexp in curly braces."
  (interactive "*")
  (sp-wrap-with-pair "{"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-smartparens-mode

(defgroup show-smartparens nil
  "Show smartparens minor mode."
  :group 'smartparens)

(defcustom sp-show-pair-delay 0.125
  "Time in seconds to delay before showing a matching pair."
  :type '(number :tag "seconds")
  :group 'show-smartparens)

(defcustom sp-show-enclosing-pair-commands '(
                                             sp-show-enclosing-pair
                                             sp-forward-slurp-sexp
                                             sp-backward-slurp-sexp
                                             sp-forward-barf-sexp
                                             sp-backward-barf-sexp
                                             )
  "List of commands after which the enclosing pair is highlighted.

After the next command the pair will automatically disappear."
  :type '(repeat symbol)
  :group 'show-smartparens)

(defcustom sp-show-pair-from-inside nil
  "If non-nil, highlight the enclosing pair if immediately after
the opening delimiter or before the closing delimiter."
  :type 'boolean
  :group 'show-smartparens)

(defcustom sp-show-pair-match-priority 1000
  "Priority of matching pair highlighting overlays."
  :type 'integer
  :group 'show-smartparens)

(defface sp-show-pair-match-face
  '((t (:inherit show-paren-match)))
  "`show-smartparens-mode' face used for a matching pair."
  :group 'show-smartparens)

(defface sp-show-pair-mismatch-face
  '((t (:inherit show-paren-mismatch)))
  "`show-smartparens-mode' face used for a mismatching pair."
  :group 'show-smartparens)

(defface sp-show-pair-enclosing
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'show-smartparens)

(defface sp-show-pair-match-content-face
  '()
  "`show-smartparens-mode' face used for a matching pair's content."
  :group 'show-smartparens)

(defvar sp-show-pair-idle-timer nil)

(defvar sp-show-pair-overlays nil)

(defvar sp-show-pair-previous-match-positions nil)

(defvar sp-show-pair-previous-point nil)

(defvar sp-show-pair-enc-overlays nil)

;;;###autoload
(define-minor-mode show-smartparens-mode
  "Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs."
  :init-value nil
  :group 'show-smartparens
  (if show-smartparens-mode
      (unless sp-show-pair-idle-timer
        (setq sp-show-pair-idle-timer
              (run-with-idle-timer sp-show-pair-delay t
                                   'sp-show--pair-function)))
    (when sp-show-pair-overlays
      (sp-show--pair-delete-overlays))))

;;;###autoload
(define-globalized-minor-mode show-smartparens-global-mode
  show-smartparens-mode
  turn-on-show-smartparens-mode)

;;;###autoload
(defun turn-on-show-smartparens-mode ()
  "Turn on `show-smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (show-smartparens-mode t)))

;;;###autoload
(defun turn-off-show-smartparens-mode ()
  "Turn off `show-smartparens-mode'."
  (interactive)
  (show-smartparens-mode -1))

(defun sp-show-enclosing-pair ()
  "Highlight the enclosing pair around point."
  (interactive))

(defun sp-highlight-current-sexp (_arg)
  "Highlight the expression returned by the next command, preserving
point position."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd)))
    (if (commandp com)
        (save-excursion
          (let ((ok (call-interactively com)))
            (sp-show--pair-enc-function ok)))
      (execute-kbd-macro cmd))))

(eval-when-compile
  (defalias 'sp--while-no-input 'while-no-input)
  (when (version< emacs-version "27")
    ;; Ripped from Emacs 27.0 subr.el.
    ;; See Github Issue#946 and Emacs bug#31692.
    (defmacro sp--while-no-input (&rest body)
      "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
      (declare (debug t) (indent 0))
      (let ((catch-sym (make-symbol "input")))
       `(with-local-quit
           (catch ',catch-sym
             (let ((throw-on-input ',catch-sym)
                   val)
               (setq val (or (input-pending-p)
                             (progn ,@body)))
               (cond
               ;; When input arrives while throw-on-input is non-nil,
               ;; kbd_buffer_store_buffered_event sets quit-flag to the
               ;; value of throw-on-input.  If, when BODY finishes,
               ;; quit-flag still has the same value as throw-on-input, it
               ;; means BODY never tested quit-flag, and therefore ran to
               ;; completion even though input did arrive before it
               ;; finished.  In that case, we must manually simulate what
               ;; 'throw' in process_quit_flag would do, and we must
               ;; reset quit-flag, because leaving it set will cause us
               ;; quit to top-level, which has undesirable consequences,
               ;; such as discarding input etc.  We return t in that case
               ;; because input did arrive during execution of BODY.
               ((eq quit-flag throw-on-input)
                (setq quit-flag nil)
                t)
               ;; This is for when the user actually QUITs during
               ;; execution of BODY.
               (quit-flag
                nil)
               (t val)))))))))

(defun sp-show--pair-function ()
  "Display the show pair overlays and print the line of the
matching paren in the echo area if not visible on screen."
  (when show-smartparens-mode
    (sp--with-case-sensitive
      (save-match-data
        (sp--while-no-input
          (cl-labels ((scan-and-place-overlays
                       (match &optional back)
                       ;; we can use `sp-get-thing' here because we *are* at some
                       ;; pair opening, and so only the tag or the sexp can trigger.
                       (-if-let (ok (sp-get-thing back))
                           (sp-get ok
                             (when (or (and back
                                            (or (= :end (point))
                                                (= :beg-in (point))))
                                       (and (not back)
                                            (or (= :beg (point))
                                                (= :end-in (point)))))
                               (sp-show--pair-create-overlays :beg :end :op-l :cl-l)
                               (when (and sp-echo-match-when-invisible
                                          (not (or (active-minibuffer-window) cursor-in-echo-area)))
                                 (sp-show--pair-echo-match :beg :end :op-l :cl-l))))
                         (if back
                             (sp-show--pair-create-mismatch-overlay (- (point) (length match))
                                                                    (length match))
                           (sp-show--pair-create-mismatch-overlay (point) (length match)))
                         (setq sp-show-pair-previous-match-positions nil)
                         (setq sp-show-pair-previous-point nil))))
            (let* ((pair-list (sp--get-allowed-pair-list))
                   (opening (sp--get-opening-regexp pair-list))
                   (closing (sp--get-closing-regexp pair-list))
                   (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp))))
              (cond
               ;; if we are in a situation "()|", we should highlight the
               ;; regular pair and not the string pair "from inside"
               ((and (not (sp--evil-normal-state-p))
                     (not (sp--evil-motion-state-p))
                     (not (sp--evil-visual-state-p))
                     (sp--looking-back (if sp-show-pair-from-inside allowed closing)))
                (scan-and-place-overlays (match-string 0) :back))
               ((or (and (or (sp--evil-normal-state-p)
                             (sp--evil-motion-state-p)
                             (sp--evil-visual-state-p))
                         (sp--looking-at (sp--get-allowed-regexp)))
                    (sp--looking-at (if sp-show-pair-from-inside allowed opening))
                    (looking-at (sp--get-stringlike-regexp))
                    (and (memq major-mode sp-navigate-consider-sgml-tags)
                         (looking-at "<")))
                (scan-and-place-overlays (match-string 0)))
               ((or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
                    (sp--looking-back (sp--get-stringlike-regexp))
                    (and (memq major-mode sp-navigate-consider-sgml-tags)
                         (sp--looking-back ">")))
                (scan-and-place-overlays (match-string 0) :back))
               (sp-show-pair-overlays
                (sp-show--pair-delete-overlays)
                (setq sp-show-pair-previous-match-positions nil)
                (setq sp-show-pair-previous-point nil))))))))))

(defun sp-show--pair-enc-function (&optional thing)
  "Display the show pair overlays for enclosing expression."
  (when show-smartparens-mode
    (-when-let (enc (or thing (sp-get-enclosing-sexp)))
      (sp-get enc (sp-show--pair-create-enc-overlays :beg :end :op-l :cl-l)))))

(defun sp-show--pair-create-overlays (start end olen clen)
  "Create the show pair overlays."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (omiddle (make-overlay (+ start olen) (- end clen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-overlays (list oleft omiddle oright))
    (overlay-put oleft 'face 'sp-show-pair-match-face)
    (unless (use-region-p)
      (overlay-put omiddle 'face 'sp-show-pair-match-content-face))
    (overlay-put oright 'face 'sp-show-pair-match-face)
    (overlay-put oleft 'priority sp-show-pair-match-priority)
    (overlay-put omiddle 'priority 1000)
    (overlay-put oright 'priority sp-show-pair-match-priority)
    (overlay-put oleft 'type 'show-pair)))

(defun sp-show--pair-echo-match (start end olen clen)
  "Print the line of the matching paren in the echo area if not
visible on screen. Needs to be called after the show-pair overlay
has been created."
  (let ((match-positions (list start end olen clen)))
    (when (not (and (equal sp-show-pair-previous-match-positions match-positions)
                    (equal sp-show-pair-previous-point (point))))
      (setq sp-show-pair-previous-match-positions match-positions)
      (setq sp-show-pair-previous-point (point))
      (let* ((visible-start (pos-visible-in-window-p start))
             (visible-end (pos-visible-in-window-p end))
             (where (cond
                     ((not visible-start) start)
                     ((not visible-end) end))))
        (when where
          (save-excursion
            (let* ((from (progn (goto-char where) (beginning-of-line) (point)))
                   (to (progn (end-of-line) (point)))
                   (line (buffer-substring from to))
                   (message-log-max)) ;; don't log in messages
              ;; Add smartparens overlay for opening parens
              (let* ((i1 (- start from))
                     (i2 (+ i1 olen)))
                (when (and (< i1 (length line)) (>= i2 0))
                  (add-face-text-property (max i1 0) (min i2 (length line))
                                          'sp-show-pair-match-face nil line)))
              ;; Add smartparens overlay for closing parens
              (let* ((i1 (- end from 1))
                     (i2 (+ i1 clen)))
                (when (and (< i1 (length line)) (>= i2 0))
                  (add-face-text-property (max i1 0) (min i2 (length line))
                                          'sp-show-pair-match-face nil line)))
              ;; echo line of match
              (message "Matches: %s" (string-trim line)))))))))

(defun sp-show--pair-create-enc-overlays (start end olen clen)
  "Create the show pair enclosing overlays"
  (when sp-show-pair-enc-overlays
    (sp-show--pair-delete-enc-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-enc-overlays (cons oleft oright))
    (overlay-put oleft 'face 'sp-show-pair-enclosing)
    (overlay-put oright 'face 'sp-show-pair-enclosing)
    (overlay-put oleft 'priority 1000)
    (overlay-put oright 'priority 1000)
    (overlay-put oleft 'type 'show-pair-enc)))

(defun sp-show--pair-create-mismatch-overlay (start len)
  "Create the mismatch pair overlay."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let ((o (make-overlay start (+ start len) nil t nil)))
    (setq sp-show-pair-overlays (list o))
    (overlay-put o 'face 'sp-show-pair-mismatch-face)
    (overlay-put o 'priority 1000)
    (overlay-put o 'type 'show-pair)))

(defun sp-show--pair-delete-overlays ()
  "Remove both show pair overlays."
  (when sp-show-pair-overlays
    (dolist (overlay sp-show-pair-overlays)
      (delete-overlay overlay))
    (setq sp-show-pair-overlays nil)))

(defun sp-show--pair-delete-enc-overlays ()
  "Remove both show pair enclosing overlays."
  (when sp-show-pair-enc-overlays
    (when (car sp-show-pair-enc-overlays)
      (delete-overlay (car sp-show-pair-enc-overlays)))
    (when (cdr sp-show-pair-enc-overlays)
      (delete-overlay (cdr sp-show-pair-enc-overlays)))
    (setq sp-show-pair-enc-overlays nil)))


;; global initialization
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))
(defadvice haskell-indentation-delete-backward-char (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))
(sp--set-base-key-bindings)
(sp--update-override-key-bindings)

(defadvice company--insert-candidate (after sp-company--insert-candidate activate)
  "If `smartparens-mode' is active, we check if the completed string
has a pair definition.  If so, we insert the closing pair."
  (when smartparens-mode
    (sp-insert-pair))
  ad-return-value)

(defadvice hippie-expand (after sp-auto-complete-advice activate)
  (when smartparens-mode
    (sp-insert-pair)))

(defvar sp--mc/cursor-specific-vars
  '(
    sp-wrap-point
    sp-wrap-mark
    sp-last-wrapped-region
    sp-pair-overlay-list
    sp-wrap-overlays
    sp-wrap-tag-overlays
    sp-last-operation
    sp-previous-point
    )
  "A list of vars that need to be tracked on a per-cursor basis.")

(defvar mc/cursor-specific-vars)
(eval-after-load 'multiple-cursors
  '(dolist (it sp--mc/cursor-specific-vars)
     (add-to-list 'mc/cursor-specific-vars it)))

(provide 'smartparens)

;; Local Variables:
;; coding: utf-8
;; eval: (font-lock-add-keywords nil `((,(concat "(" (regexp-opt '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl") t) "\\_>") 1 'font-lock-variable-name-face)))
;; End:

;;; smartparens.el ends here
