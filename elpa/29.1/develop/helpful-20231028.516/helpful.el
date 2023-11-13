;;; helpful.el --- A better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/helpful
;; Keywords: help, lisp
;; Version: 0.22
;; Package-Requires: ((emacs "25") (dash "2.18.0") (s "1.11.0") (f "0.20.0") (elisp-refs "1.2"))

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

;; Helpful is a replacement for *help* buffers that provides much more
;; contextual information.  To get started, try:
;; `M-x helpful-function RET helpful-function
;;
;; The full set of commands you can try is:
;;
;; * helpful-function
;; * helpful-command
;; * helpful-key
;; * helpful-macro
;; * helpful-callable
;; * helpful-variable
;; * helpful-at-point
;;
;; For more information and screenshots, see
;; https://github.com/wilfred/helpful

;;; Code:

(require 'elisp-refs)
(require 'help)
(require 'help-fns)
(require 'dash)
(require 's)
(require 'f)
(require 'find-func)
(require 'nadvice)
(require 'info-look)
(require 'edebug)
(require 'trace)
(require 'imenu)
(require 'cc-langs)

(declare-function org-link-types "ol" ())
(declare-function org-link-store-props "ol" (&rest plist))
(declare-function org-link-get-parameter "ol" (type key))

(defvar-local helpful--sym nil)
(defvar-local helpful--callable-p nil)
(defvar-local helpful--associated-buffer nil
  "The buffer being used when showing inspecting
buffer-local variables.")
(defvar-local helpful--start-buffer nil
  "The buffer we were originally called from.")
(defvar-local helpful--view-literal nil
  "Whether to show a value as a literal, or a pretty interactive
view.")
(defvar-local helpful--first-display t
  "Whether this is the first time this results buffer has been
displayed.

Nil means that we're refreshing, so we don't want to clobber any
settings changed by the user.")

(defgroup helpful nil
  "A rich help system with contextual information."
  :link '(url-link "https://github.com/Wilfred/helpful")
  :group 'help)

(defcustom helpful-max-buffers
  5
  "Helpful will kill the least recently used Helpful buffer
if there are more than this many.

To disable cleanup entirely, set this variable to nil. See also
`helpful-kill-buffers' for a one-off cleanup."
  :type '(choice (const nil) number)
  :group 'helpful)

(defcustom helpful-switch-buffer-function
  #'pop-to-buffer
  "Function called to display the *Helpful* buffer."
  :type 'function
  :group 'helpful)

(defcustom helpful-set-variable-function
  (if (< 29 emacs-major-version) #'setopt #'setq)
  "Function used by `helpful--set' to interactively set variables."
  :type 'function
  :group 'helpful)

;; TODO: explore whether more basic highlighting is fast enough to
;; handle larger functions. See `c-font-lock-init' and its use of
;; font-lock-keywords-1.
(defconst helpful-max-highlight 5000
  "Don't highlight code with more than this many characters.

This is currently only used for C code, as lisp highlighting
seems to be more efficient. This may change again in future.

See `this-command' as an example of a large piece of C code that
can make Helpful very slow.")

(defun helpful--kind-name (symbol callable-p)
  "Describe what kind of symbol this is."
  (cond
   ((not callable-p) "variable")
   ((commandp symbol) "command")
   ((macrop symbol) "macro")
   ((functionp symbol) "function")
   ((special-form-p symbol) "special form")))

(defun helpful--buffer (symbol callable-p)
  "Return a buffer to show help for SYMBOL in."
  (let* ((current-buffer (current-buffer))
         (buf-name
          (format "*helpful %s*"
                  (if (symbolp symbol)
                      (format "%s: %s"
                              (helpful--kind-name symbol callable-p)
                              symbol)
                    "lambda")))
         (buf (get-buffer buf-name)))
    (unless buf
      ;; If we need to create the buffer, ensure we don't exceed
      ;; `helpful-max-buffers' by killing the least recently used.
      (when (numberp helpful-max-buffers)
        (let* ((buffers (buffer-list))
               (helpful-bufs (--filter (with-current-buffer it
                                         (eq major-mode 'helpful-mode))
                                       buffers))
               ;; `buffer-list' seems to be ordered by most recently
               ;; visited first, so keep those.
               (excess-buffers (-drop (1- helpful-max-buffers) helpful-bufs)))
          ;; Kill buffers so we have one buffer less than the maximum
          ;; before we create a new one.
          (-each excess-buffers #'kill-buffer)))

      (setq buf (get-buffer-create buf-name)))

    ;; Initialise the buffer with the symbol and associated data.
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol)
      (setq helpful--callable-p callable-p)
      (setq helpful--start-buffer current-buffer)
      (setq helpful--associated-buffer current-buffer)
      (setq list-buffers-directory
        (if (symbolp symbol) (format "%s: %s" (helpful--kind-name symbol callable-p) symbol) "lambda"))
      (if (helpful--primitive-p symbol callable-p)
          (setq-local comment-start "//")
        (setq-local comment-start ";")))
    buf))

(defface helpful-heading
  '((t (:weight bold)))
  "Face used for headings in Helpful buffers.")

(defun helpful--heading (text)
  "Propertize TEXT as a heading."
  (propertize (concat text "\n") 'face 'helpful-heading))

(defun helpful--format-closure (sym form)
  "Given a closure, return an equivalent defun form."
  (-let (((_keyword _env args . body) form)
         (docstring nil))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body))
      ;; Ensure that the docstring doesn't have lines starting with (,
      ;; or it breaks indentation.
      (setq docstring
            (s-replace "\n(" "\n\\(" docstring)))
    (if docstring
        `(defun ,sym ,args ,docstring ,@body)
      `(defun ,sym ,args ,@body))))

(defun helpful--pretty-print (value)
  "Pretty-print VALUE.

If VALUE is very big, the user may press \\[keyboard-quit] to
gracefully stop the printing. If VALUE is self-referential, the
error will be caught and displayed."
  ;; Inspired by `ielm-eval-input'.
  (condition-case err
      (s-trim-right (pp-to-string value))
    (error
     (propertize (format "(Display error: %s)" (cadr err))
                 'face 'font-lock-comment-face))
    (quit
     (propertize "(User quit during pretty-printing.)"
                 'face 'font-lock-comment-face))))

(defun helpful--sort-symbols (sym-list)
  "Sort symbols in SYM-LIST alphabetically."
  (--sort
   (string< (symbol-name it) (symbol-name other))
   sym-list))

(defun helpful--button (text type &rest properties)
  ;; `make-text-button' mutates our string to add properties. Copy
  ;; TEXT to prevent mutating our arguments, and to support 'pure'
  ;; strings, which are read-only.
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type type
         properties))

(defun helpful--canonical-symbol (sym callable-p)
  "If SYM is an alias, return the underlying symbol.
Return SYM otherwise."
  (let ((depth 0))
    (if (and (symbolp sym) callable-p)
        (progn
          ;; Follow the chain of symbols until we find a symbol that
          ;; isn't pointing to a symbol.
          (while (and (symbolp (symbol-function sym))
                      (< depth 10))
            (setq sym (symbol-function sym))
            (setq depth (1+ depth)))
          ;; If this is an alias to a primitive, return the
          ;; primitive's symbol.
          (when (subrp (symbol-function sym))
            (setq sym (intern (subr-name (symbol-function sym))))))
      (setq sym (indirect-variable sym))))
  sym)

(defun helpful--aliases (sym callable-p)
  "Return all the aliases for SYM."
  (let ((canonical (helpful--canonical-symbol sym callable-p))
        aliases)
    (mapatoms
     (lambda (s)
       (when (and
              ;; Skip variables that aren't bound, so we're faster.
              (if callable-p (fboundp s) (boundp s))

              ;; If this symbol is a new alias for our target sym,
              ;; add it.
              (eq canonical (helpful--canonical-symbol s callable-p))

              ;; Don't include SYM.
              (not (eq sym s)))
         (push s aliases))))
    (helpful--sort-symbols aliases)))

(defun helpful--obsolete-info (sym callable-p)
  (when (symbolp sym)
    (get sym (if callable-p 'byte-obsolete-info 'byte-obsolete-variable))))

(defun helpful--format-alias (sym callable-p)
  (let ((obsolete-info (helpful--obsolete-info sym callable-p))
        (sym-button (helpful--button
                     (symbol-name sym)
                     'helpful-describe-exactly-button
                     'symbol sym
                     'callable-p callable-p)))
    (cond
     (obsolete-info
      (-if-let (version (-last-item obsolete-info))
          (format "%s (obsolete since %s)" sym-button version)
        (format "%s (obsolete)" sym-button)))
     (t
      sym-button))))

(defun helpful--indent-rigidly (s amount)
  "Indent string S by adding AMOUNT spaces to each line."
  (with-temp-buffer
    (insert s)
    (indent-rigidly (point-min) (point-max) amount)
    (buffer-string)))

(defun helpful--format-properties (symbol)
  "Return a string describing all the properties of SYMBOL."
  (let* ((syms-and-vals
          (-partition 2 (and (symbolp symbol) (symbol-plist symbol))))
         (syms-and-vals
          (-sort (-lambda ((sym1 _) (sym2 _))
                   (string-lessp (symbol-name sym1) (symbol-name sym2)))
                 syms-and-vals))
         (lines
          (--map
           (-let* (((sym val) it)
                   (pretty-val
                    (helpful--pretty-print val)))
             (format "%s\n%s%s"
                     (propertize (symbol-name sym)
                                 'face 'font-lock-constant-face)
                     (helpful--indent-rigidly pretty-val 2)
                     (cond
                      ;; Also offer to disassemble byte-code
                      ;; properties.
                      ((byte-code-function-p val)
                       (format "\n  %s"
                               (helpful--make-disassemble-button val)))
                      ((eq sym 'ert--test)
                       (format "\n  %s"
                               (helpful--make-run-test-button symbol)))
                      (t
                       ""))))
           syms-and-vals)))
    (when lines
      (s-join "\n" lines))))

(define-button-type 'helpful-forget-button
  'action #'helpful--forget
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Unbind this function")

;; TODO: it would be nice to optionally delete the source code too.
(defun helpful--forget (button)
  "Unbind the current symbol."
  (let* ((sym (button-get button 'symbol))
         (callable-p (button-get button 'callable-p))
         (kind (helpful--kind-name sym callable-p)))
    (when (yes-or-no-p (format "Forget %s %s?" kind sym))
      (if callable-p
          (fmakunbound sym)
        (makunbound sym))
      (message "Forgot %s %s." kind sym)
      (kill-buffer (current-buffer)))))

(define-button-type 'helpful-c-source-directory
  'action #'helpful--c-source-directory
  'follow-link t
  'help-echo "Set directory to Emacs C source code")

(defun helpful--c-source-directory (_button)
  "Set `find-function-C-source-directory' so we can show the
source code to primitives."
  (let ((emacs-src-dir (read-directory-name "Path to Emacs source code: ")))
    ;; Let the user specify the source path with or without src/,
    ;; which is a subdirectory in the Emacs tree.
    (unless (equal (f-filename emacs-src-dir) "src")
      (setq emacs-src-dir (f-join emacs-src-dir "src")))
    (setq find-function-C-source-directory emacs-src-dir))
  (helpful-update))

(define-button-type 'helpful-disassemble-button
  'action #'helpful--disassemble
  'follow-link t
  'object nil
  'help-echo "Show disassembled bytecode")

(defun helpful--disassemble (button)
  "Disassemble the current symbol."
  ;; `disassemble' can handle both symbols (e.g. 'when) and raw
  ;; byte-code objects.
  (disassemble (button-get button 'object)))

(define-button-type 'helpful-run-test-button
  'action #'helpful--run-test
  'follow-link t
  'symbol nil
  'help-echo "Run ERT test")

(defun helpful--run-test (button)
  "Disassemble the current symbol."
  (ert (button-get button 'symbol)))

(define-button-type 'helpful-edebug-button
  'action #'helpful--edebug
  'follow-link t
  'symbol nil
  'help-echo "Toggle edebug (re-evaluates definition)")

(defun helpful--kbd-macro-p (sym)
  "Is SYM a keyboard macro?"
  (and (symbolp sym)
       (let ((func (symbol-function sym)))
         (or (stringp func)
             (vectorp func)))))

(defun helpful--edebug-p (sym)
  "Does function SYM have its definition patched by edebug?"
  (let ((fn-def (indirect-function sym)))
    ;; Edebug replaces function source code with a sexp that has
    ;; `edebug-enter', `edebug-after' etc interleaved. This means the
    ;; function is interpreted, so `indirect-function' returns a list.
    (when (and (consp fn-def) (consp (cdr fn-def)))
      (-let [fn-end (-last-item fn-def)]
        (and (consp fn-end)
             (eq (car fn-end) 'edebug-enter))))))

(defun helpful--can-edebug-p (sym callable-p buf pos)
  "Can we use edebug with SYM?"
  (and
   ;; SYM must be a function.
   callable-p
   ;; The function cannot be a primitive, it must be defined in elisp.
   (not (helpful--primitive-p sym callable-p))
   ;; We need to be able to find its definition, or we can't step
   ;; through the source.
   buf pos))

(defun helpful--toggle-edebug (sym)
  "Enable edebug when function SYM is called,
or disable if already enabled."
  (-let ((should-edebug (not (helpful--edebug-p sym)))
         ((buf pos created) (helpful--definition sym t)))
    (if (and buf pos)
        (progn
          (with-current-buffer buf
            (save-excursion
              (save-restriction
                (widen)
                (goto-char pos)

                (let* ((edebug-all-forms should-edebug)
                       (edebug-all-defs should-edebug)
                       (form (edebug-read-top-level-form)))
                  ;; Based on `edebug-eval-defun'.
                  (eval (eval-sexp-add-defvars form) lexical-binding)))))
          ;; If we're enabling edebug, we need the source buffer to
          ;; exist. Otherwise, we can clean it up.
          (when (and created (not should-edebug))
            (kill-buffer buf)))

      (user-error "Could not find source for edebug"))))

(defun helpful--edebug (button)
  "Toggle edebug for the current symbol."
  (helpful--toggle-edebug (button-get button 'symbol))
  (helpful-update))

(define-button-type 'helpful-trace-button
  'action #'helpful--trace
  'follow-link t
  'symbol nil
  'help-echo "Toggle function tracing")

(defun helpful--trace (button)
  "Toggle tracing for the current symbol."
  (let ((sym (button-get button 'symbol)))
    (if (trace-is-traced sym)
        (untrace-function sym)
      (trace-function sym)))
  (helpful-update))

(define-button-type 'helpful-navigate-button
  'action #'helpful--navigate
  'path nil
  'position nil
  'follow-link t
  'help-echo "Navigate to definition")

(defun helpful--goto-char-widen (pos)
  "Move point to POS in the current buffer.
If narrowing is in effect, widen if POS isn't in the narrowed area."
  (when (or (< pos (point-min))
            (> pos (point-max)))
    (widen))
  (goto-char pos))

(defun helpful--navigate (button)
  "Navigate to the path this BUTTON represents."
  (find-file (substring-no-properties (button-get button 'path)))
  ;; We use `get-text-property' to work around an Emacs 25 bug:
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
  (-when-let (pos (get-text-property button 'position
                                     (marker-buffer button)))
    (helpful--goto-char-widen pos)))

(defun helpful--navigate-button (text path &optional pos)
  "Return a button that opens PATH and puts point at POS."
  (helpful--button
   text
   'helpful-navigate-button
   'path path
   'position pos))

(define-button-type 'helpful-buffer-button
  'action #'helpful--switch-to-buffer
  'buffer nil
  'position nil
  'follow-link t
  'help-echo "Switch to this buffer")

(defun helpful--switch-to-buffer (button)
  "Navigate to the buffer this BUTTON represents."
  (let ((buf (button-get button 'buffer))
        (pos (button-get button 'position)))
    (switch-to-buffer buf)
    (when pos
      (helpful--goto-char-widen pos))))

(defun helpful--buffer-button (buffer &optional pos)
  "Return a button that switches to BUFFER and puts point at POS."
  (helpful--button
   (buffer-name buffer)
   'helpful-buffer-button
   'buffer buffer
   'position pos))

(define-button-type 'helpful-customize-button
  'action #'helpful--customize
  'symbol nil
  'follow-link t
  'help-echo "Open Customize for this symbol")

(defun helpful--customize (button)
  "Open Customize for this symbol."
  (customize-variable (button-get button 'symbol)))

(define-button-type 'helpful-associated-buffer-button
  'action #'helpful--associated-buffer
  'symbol nil
  'prompt-p nil
  'follow-link t
  'help-echo "Change associated buffer")

(defun helpful--read-live-buffer (prompt predicate)
  "Read a live buffer name, and return the buffer object.

This is largely equivalent to `read-buffer', but counsel.el
overrides that to include previously opened buffers."
  (let* ((names (-map #'buffer-name (buffer-list)))
         (default
           (cond
            ;; If we're already looking at a buffer-local value, start
            ;; the prompt from the relevant buffer.
            ((and helpful--associated-buffer
                  (buffer-live-p helpful--associated-buffer))
             (buffer-name helpful--associated-buffer))
            ;; If we're looking at the global value, offer the initial
            ;; buffer.
            ((and helpful--start-buffer
                  (buffer-live-p helpful--start-buffer))
             (buffer-name helpful--start-buffer))
            ;; If we're looking at the global value and have no initial
            ;; buffer, choose the first normal buffer.
            (t
             (--first (and (not (s-starts-with-p " " it))
                           (not (s-starts-with-p "*" it)))
                      names))
            )))
    (get-buffer
     (completing-read
      prompt
      names
      predicate
      t
      nil
      nil
      default))))

(defun helpful--associated-buffer (button)
  "Change the associated buffer, so we can see buffer-local values."
  (let ((sym (button-get button 'symbol))
        (prompt-p (button-get button 'prompt-p)))
    (if prompt-p
        (setq helpful--associated-buffer
              (helpful--read-live-buffer
               "View variable in: "
               (lambda (buf-name)
                 (local-variable-p sym (get-buffer buf-name)))))
      (setq helpful--associated-buffer nil)))
  (helpful-update))

(define-button-type 'helpful-toggle-button
  'action #'helpful--toggle
  'symbol nil
  'buffer nil
  'follow-link t
  'help-echo "Toggle this symbol between t and nil")

(defun helpful--toggle (button)
  "Toggle the symbol between nil and t."
  (let ((sym (button-get button 'symbol))
        (buf (button-get button 'buffer)))
    (save-current-buffer
      ;; If this is a buffer-local variable, ensure we're in the right
      ;; buffer.
      (when buf
        (set-buffer buf))
      (set sym (not (symbol-value sym))))
    (helpful-update)))

(define-button-type 'helpful-set-button
  'action #'helpful--set
  'symbol nil
  'buffer nil
  'follow-link t
  'help-echo "Set the value of this symbol")

(defun helpful--set (button)
  "Set the value of this symbol."
  (let* ((sym (button-get button 'symbol))
         (buf (button-get button 'buffer))
         (sym-value (helpful--sym-value sym buf))
         (set-func (symbol-name helpful-set-variable-function))
         ;; Inspired by `counsel-read-setq-expression'.
         (expr
          (minibuffer-with-setup-hook
              (lambda ()
                (add-function :before-until (local 'eldoc-documentation-function)
                              #'elisp-eldoc-documentation-function)
                (run-hooks 'eval-expression-minibuffer-setup-hook)
                (goto-char (minibuffer-prompt-end))
                (forward-char (length (format "(%s %S " set-func sym))))
            (read-from-minibuffer
             "Eval: "
             (format
              (if (or (consp sym-value)
                      (and (symbolp sym-value)
                           (not (null sym-value))
                           (not (keywordp sym-value))))
                  "(%s %s '%S)"
                "(%s %s %S)")
              set-func sym sym-value)
             read-expression-map t
             'read-expression-history))))
    (save-current-buffer
      ;; If this is a buffer-local variable, ensure we're in the right
      ;; buffer.
      (when buf
        (set-buffer buf))
      (eval-expression expr))
    (helpful-update)))

(define-button-type 'helpful-view-literal-button
  'action #'helpful--view-literal
  'help-echo "Toggle viewing as a literal")

(defun helpful--view-literal (_button)
  "Set the value of this symbol."
  (setq helpful--view-literal
        (not helpful--view-literal))
  (helpful-update))

(define-button-type 'helpful-all-references-button
  'action #'helpful--all-references
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Find all references to this symbol")

(defun helpful--all-references (button)
  "Find all the references to the symbol that this BUTTON represents."
  (let ((sym (button-get button 'symbol))
        (callable-p (button-get button 'callable-p)))
    (cond
     ((not callable-p)
      (elisp-refs-variable sym))
     ((functionp sym)
      (elisp-refs-function sym))
     ((macrop sym)
      (elisp-refs-macro sym)))))

(define-button-type 'helpful-callees-button
  'action #'helpful--show-callees
  'symbol nil
  'source nil
  'follow-link t
  'help-echo "Find the functions called by this function/macro")

(defun helpful--display-callee-group (callees)
  "Insert every entry in CALLEES."
  (dolist (sym (helpful--sort-symbols callees))
    (insert "  "
            (helpful--button
             (symbol-name sym)
             'helpful-describe-exactly-button
             'symbol sym
             'callable-p t)
            "\n")))

(defun helpful--show-callees (button)
  "Find all the references to the symbol that this BUTTON represents."
  (let* ((buf (get-buffer-create "*helpful callees*"))
         (sym (button-get button 'symbol))
         (raw-source (button-get button 'source))
         (source
          (if (stringp raw-source)
              (read raw-source)
            raw-source))
         (syms (helpful--callees source))
         (primitives (-filter (lambda (sym) (helpful--primitive-p sym t)) syms))
         (compounds (-remove (lambda (sym) (helpful--primitive-p sym t)) syms)))

    (pop-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)

      ;; TODO: Macros used, special forms used, global vars used.
      (insert (format "Functions called by %s:\n\n" sym))
      (helpful--display-callee-group compounds)

      (when primitives
        (insert "\n")
        (insert (format "Primitives called by %s:\n\n" sym))
        (helpful--display-callee-group primitives))

      (goto-char (point-min))

      (helpful-mode))))

(define-button-type 'helpful-manual-button
  'action #'helpful--manual
  'symbol nil
  'follow-link t
  'help-echo "View this symbol in the Emacs manual")

(defun helpful--manual (button)
  "Open the manual for the system that this BUTTON represents."
  (let ((sym (button-get button 'symbol)))
    (info-lookup 'symbol sym #'emacs-lisp-mode)))

(define-button-type 'helpful-describe-button
  'action #'helpful--describe
  'symbol nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--describe (button)
  "Describe the symbol that this BUTTON represents."
  (let ((sym (button-get button 'symbol)))
    (helpful-symbol sym)))

(define-button-type 'helpful-describe-exactly-button
  'action #'helpful--describe-exactly
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--describe-exactly (button)
  "Describe the symbol that this BUTTON represents.
This differs from `helpful--describe' because here we know
whether the symbol represents a variable or a callable."
  (let ((sym (button-get button 'symbol))
        (callable-p (button-get button 'callable-p)))
    (if callable-p
        (helpful-callable sym)
      (helpful-variable sym))))

(define-button-type 'helpful-info-button
  'action #'helpful--info
  'info-node nil
  'follow-link t
  'help-echo "View this Info node")

(defun helpful--info (button)
  "Describe the symbol that this BUTTON represents."
  (info (button-get button 'info-node)))

(define-button-type 'helpful-shortdoc-button
  'action #'helpful--shortdoc
  'info-node nil
  'follow-link t
  'help-echo "View this Shortdoc group")

(defun helpful--shortdoc (button)
  "Describe the symbol that this BUTTON represents."
  (shortdoc-display-group (button-get button 'shortdoc-group)
                          (button-get button 'symbol)))

(defun helpful--split-first-line (docstring)
  "If the first line is a standalone sentence, ensure we have a
blank line afterwards."
  (let* ((lines (s-lines docstring))
         (first-line (-first-item lines))
         (second-line (when (> (length lines) 1) (nth 1 lines))))
    (if (and (s-ends-with-p "." first-line)
             (stringp second-line)
             (not (equal second-line "")))
        (s-join "\n"
                (-cons* first-line "" (cdr lines)))
      docstring)))

(defun helpful--propertize-sym-ref (sym-name before-txt after-txt)
  "Given a symbol name from a docstring, convert to a button (if
bound) or else highlight."
  (let* ((sym (intern sym-name)))
    (cond
     ;; Highlight keywords.
     ((s-matches-p
       (rx ":"
           symbol-start
           (+? (or (syntax word) (syntax symbol)))
           symbol-end)
       sym-name)
      (propertize sym-name
                  'face 'font-lock-builtin-face))
     ((and (boundp sym) (s-ends-with-p "variable " (downcase before-txt)))
      (helpful--button
       sym-name
       'helpful-describe-exactly-button
       'symbol sym
       'callable-p nil))
     ((and (fboundp sym)
           (or
            (s-starts-with-p " command" (downcase after-txt))
            (s-ends-with-p "command " (downcase before-txt))
            (s-ends-with-p "function " (downcase before-txt))))
      (helpful--button
       sym-name
       'helpful-describe-exactly-button
       'symbol sym
       'callable-p t))
     ;; Only create a link if this is a symbol that is bound as a
     ;; variable or callable.
     ((or (boundp sym) (fboundp sym))
      (helpful--button
       sym-name
       'helpful-describe-button
       'symbol sym))
     ;; If this is already a button, don't modify it.
     ((get-text-property 0 'button sym-name)
      sym-name)
     ;; Highlight the quoted string.
     (t
      (propertize sym-name
                  'face 'font-lock-constant-face)))))

(defun helpful--propertize-info (docstring)
  "Convert info references in DOCSTRING to buttons."
  (replace-regexp-in-string
   ;; Replace all text that looks like a link to an Info page.
   (rx (seq (group
             bow
             (any "Ii")
             "nfo"
             (one-or-more whitespace))
            (group
             (or "node" "anchor")
             (one-or-more whitespace))
            (any "'`‘")
            (group
             (one-or-more
              (not (any "'’"))))
            (any "'’")))
   (lambda (it)
     ;; info-name matches "[Ii]nfo ".
     ;; space matches "node " or "anchor ".
     ;; info-node has the form "(cl)Loop Facility".
     (let ((info-name (match-string 1 it))
           (space (match-string 2 it))
           (info-node (match-string 3 it)))
       ;; If the docstring doesn't specify a manual, assume the Emacs manual.
       (save-match-data
         (unless (string-match "^([^)]+)" info-node)
           (setq info-node (concat "(emacs)" info-node))))
       (concat
        info-name
        space
        (helpful--button
         info-node
         'helpful-info-button
         'info-node info-node))))
   docstring
   t t))

(defun helpful--keymap-keys (keymap)
  "Return all the keys and commands in KEYMAP.
Flattens nested keymaps and follows remapped commands.

Returns a list of pairs (KEYCODES COMMAND), where KEYCODES is a
vector suitable for `key-description', and COMMAND is a smbol."
  (cond
   ;; Prefix keys.
   ((and
     (symbolp keymap)
     (fboundp keymap)
     ;; Prefix keys use a keymap in the function slot of a symbol.
     (keymapp (symbol-function keymap)))
    (helpful--keymap-keys (symbol-function keymap)))
   ;; Other symbols or compiled functions mean we've reached a leaf,
   ;; so this is a command we can call.
   ((or
     (symbolp keymap)
     (functionp keymap)
     ;; Strings or vectors mean a keyboard macro.
     (stringp keymap)
     (vectorp keymap))
    `(([] ,keymap)))
   ((stringp (car keymap))
    (helpful--keymap-keys (cdr keymap)))
   ;; Otherwise, recurse on the keys at this level of the keymap.
   (t
    (let (result)
      (dolist (item (cdr keymap))
        (cond
         ((and (consp item)
               (eq (car item) 'menu-bar))
          ;; Skip menu bar items.
          nil)
         ;; Sparse keymaps are lists.
         ((consp item)
          (-let [(keycode . value) item]
            (-each (helpful--keymap-keys value)
              (-lambda ((keycodes command))
                (push (list (vconcat (vector keycode) keycodes) command)
                      result)))))
         ;; Dense keymaps are char-tables.
         ((char-table-p item)
          (map-char-table
           (lambda (keycode value)
             (-each (helpful--keymap-keys value)
               (-lambda ((keycodes command))
                 (push (list (vconcat (vector keycode) keycodes) command)
                       result))))
           item))))
      ;; For every command `new-func' mapped to a command `orig-func', show `new-func' with
      ;; the key sequence for `orig-func'.
      (setq result
            (-map-when
             (-lambda ((keycodes _))
               (and (> (length keycodes) 1)
                    (eq (elt keycodes 0) 'remap)))
             (-lambda ((keycodes command))
               (list
                (where-is-internal (elt keycodes 1) global-map t)
                command))
             result))
      ;; Preserve the original order of the keymap.
      (nreverse result)))))

(defun helpful--format-hook (hook-val)
  "Given a list value assigned to a hook, format it with links to functions."
  (let ((lines
         (--map
          (if (and (symbolp it) (fboundp it))
              (helpful--button
               (symbol-name it)
               'helpful-describe-exactly-button
               'symbol it
               'callable-p t)
            (helpful--syntax-highlight (helpful--pretty-print it)))
          hook-val)))
    (format "(%s)"
            (s-join "\n " lines))))

;; TODO: unlike `substitute-command-keys', this shows keybindings
;; which are currently shadowed (e.g. a global minor mode map).
(defun helpful--format-keymap (keymap)
  "Format KEYMAP."
  (let* ((keys-and-commands (helpful--keymap-keys keymap))
         ;; Convert keycodes [27 i] to "C-M-i".
         (keys (-map #'-first-item keys-and-commands))
         ;; Add padding so all our strings are the same length.
         (formatted-keys (-map #'key-description keys))
         (max-formatted-length (-max (cons 0 (-map #'length formatted-keys))))
         (aligned-keys (--map (s-pad-right (1+ max-formatted-length)
                                           " " it)
                              formatted-keys))
         ;; Format commands as buttons.
         (commands (-map (-lambda ((_ command)) command)
                         keys-and-commands))
         (formatted-commands
          (--map
           (cond
            ((symbolp it)
             (helpful--button
              (symbol-name it)
              'helpful-describe-button
              'symbol it))
            ((or (stringp it) (vectorp it))
             "Keyboard Macro")
            (t
             "#<anonymous-function>"))
           commands))
         ;; Build lines for display.
         (lines
          (-map (-lambda ((key . command)) (format "%s %s" key command))
                (-zip-pair aligned-keys formatted-commands))))
    ;; The flattened keymap will have normal bindings first, and
    ;; inherited bindings last. Sort so that we group by prefix.
    (s-join "\n" (-sort #'string< lines))))

(defun helpful--format-commands (str keymap)
  "Replace all the \\[ references in STR with buttons."
  (replace-regexp-in-string
   ;; Text of the form \\[foo-command]
   (rx "\\[" (group (+ (not (in "]")))) "]")
   (lambda (it)
     (let* ((button-face (if (>= emacs-major-version 28) 'help-key-binding 'button))
            (symbol-name (match-string 1 it))
            (symbol (intern symbol-name))
            (key (where-is-internal symbol keymap t))
            (key-description
             (if key
                 (key-description key)
               (format "M-x %s" symbol-name))))
       (helpful--button
        key-description
        'helpful-describe-exactly-button
        'symbol symbol
        'callable-p t
        'face button-face)))
   str
   t
   t))

(defun helpful--chars-before (pos n)
  "Return up to N chars before POS in the current buffer.
The string may be shorter than N or empty if out-of-range."
  (buffer-substring
   (max (point-min) (- pos n))
   pos))

(defun helpful--chars-after (pos n)
  "Return up to N chars after POS in the current buffer.
The string may be shorter than N or empty if out-of-range."
  (buffer-substring
   pos
   (min (point-max) (+ pos n))))

(defun helpful--format-command-keys (docstring)
  "Convert command key references and keymap references
in DOCSTRING to buttons.

Emacs uses \\= to escape \\[ references, so replace that
unescaping too."
  ;; Loosely based on `substitute-command-keys', but converts
  ;; references to buttons.
  (let ((keymap nil))
    (with-temp-buffer
      (insert docstring)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at
           ;; Text of the form "foo"
           (rx "\""))
          ;; For literal strings, escape backslashes so our output
          ;; shows copy-pasteable literals.
          (let* ((start-pos (point))
                 (end-pos (progn (forward-char) (search-forward "\"" nil t)))
                 contents)
            (if end-pos
                (progn
                  (setq contents (buffer-substring start-pos end-pos))
                  (delete-region start-pos end-pos)
                  (insert (s-replace "\\" "\\\\" contents)))
              (forward-char 1))))
         ((looking-at
           ;; Text of the form \=X
           (rx "\\="))
          ;; Remove the escaping, then step over the escaped char.
          ;; Step over the escaped character.
          (delete-region (point) (+ (point) 2))
          (forward-char 1))
         ((looking-at
           ;; Text of the form `foo'
           (rx "`"))
          (let* ((start-pos (point))
                 (end-pos (search-forward "'" nil t))
                 (contents
                  (when end-pos
                    (buffer-substring (1+ start-pos) (1- end-pos)))))
            (cond
             ((null contents)
              ;; If there's no closing ' to match the opening `, just
              ;; leave it.
              (goto-char (1+ start-pos)))
             ((s-contains-p "`" contents)
              ;; If we have repeated backticks `foo `bar', leave the
              ;; first one.
              (goto-char (1+ start-pos)))
             ((s-contains-p "\\[" contents)
              (delete-region start-pos end-pos)
              (insert (helpful--format-commands contents keymap)))
             ;; Highlight a normal `foo', extracting the surrounding
             ;; text so we can detect e.g. "function `foo'".
             (t
              (let ((before (helpful--chars-before start-pos 10))
                    (after (helpful--chars-after end-pos 10)))
                (delete-region start-pos end-pos)
                (insert (helpful--propertize-sym-ref contents before after)))))))
         ((looking-at
           ;; Text of the form \\<foo-keymap>
           (rx "\\<" (group (+ (not (in ">")))) ">"
               (? "\n")))
          (let* ((symbol-with-parens (match-string 0))
                 (symbol-name (match-string 1)))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            ;; Set the new keymap.
            (setq keymap (symbol-value (intern symbol-name)))))
         ((looking-at
           ;; Text of the form \\{foo-mode-map}
           (rx "\\{" (group (+ (not (in "}")))) "}"))
          (let* ((symbol-with-parens (match-string 0))
                 (symbol-name (match-string 1))
                 (keymap
                  ;; Gracefully handle variables not being defined.
                  (ignore-errors
                    (symbol-value (intern symbol-name)))))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            (if keymap
                (insert (helpful--format-keymap keymap))
              (insert (format "Keymap %s is not currently defined."
                              symbol-name)))))
         ((looking-at
           ;; Text of the form \\[foo-command]
           (rx "\\[" (group (+ (not (in "]")))) "]"))
          (let* ((symbol-with-parens (match-string 0)))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            ;; Add a button.
            (insert (helpful--format-commands symbol-with-parens keymap))))
         ;; Don't modify other characters.
         (t
          (forward-char 1))))
      (buffer-string))))

;; TODO: fix upstream Emacs bug that means `-map' is not highlighted
;; in the docstring for `--map'.
(defun helpful--format-docstring (docstring)
  "Replace cross-references with links in DOCSTRING."
  (-> docstring
      (helpful--split-first-line)
      (helpful--propertize-info)
      (helpful--propertize-links)
      (helpful--propertize-bare-links)
      (helpful--format-command-keys)
      (s-trim)))

(define-button-type 'helpful-link-button
  'action #'helpful--follow-link
  'follow-link t
  'help-echo "Follow this link")

(defun helpful--propertize-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx "URL `" (group (*? any)) "'")
   (lambda (match)
     (let ((url (match-string 1 match)))
       (concat "URL "
               (helpful--button
                url
                'helpful-link-button
                'url url))))
   docstring))

(defun helpful--propertize-bare-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx (group (or string-start space "<"))
       (group "http" (? "s") "://" (+? (not (any space))))
       (group (? (any "." ">" ")"))
              (or space string-end ">")))
   (lambda (match)
     (let ((space-before (match-string 1 match))
           (url (match-string 2 match))
           (after (match-string 3 match)))
       (concat
        space-before
        (helpful--button
         url
         'helpful-link-button
         'url url)
        after)))
   docstring))

(defun helpful--follow-link (button)
  "Follow the URL specified by BUTTON."
  (browse-url (button-get button 'url)))

(defconst helpful--highlighting-funcs
  '(ert--activate-font-lock-keywords
    highlight-quoted-mode
    rainbow-delimiters-mode)
  "Highlighting functions that are safe to run in a temporary buffer.
This is used in `helpful--syntax-highlight' to support extra
highlighting that the user may have configured in their mode
hooks.")

;; TODO: crashes on `backtrace-frame' on a recent checkout.

(defun helpful--syntax-highlight (source &optional mode)
  "Return a propertized version of SOURCE in MODE."
  (unless mode
    (setq mode #'emacs-lisp-mode))
  (if (or
       (< (length source) helpful-max-highlight)
       (eq mode 'emacs-lisp-mode))
      (with-temp-buffer
        (insert source)

        ;; Switch to major-mode MODE, but don't run any hooks.
        (delay-mode-hooks (funcall mode))

        ;; `delayed-mode-hooks' contains mode hooks like
        ;; `emacs-lisp-mode-hook'. Build a list of functions that are run
        ;; when the mode hooks run.
        (let (hook-funcs)
          (dolist (hook delayed-mode-hooks)
            (let ((funcs (symbol-value hook)))
              (setq hook-funcs (append hook-funcs funcs))))

          ;; Filter hooks to those that relate to highlighting, and run them.
          (setq hook-funcs (-intersection hook-funcs helpful--highlighting-funcs))
          (-map #'funcall hook-funcs))

        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string))
    ;; SOURCE was too long to highlight in a reasonable amount of
    ;; time.
    (concat
     (propertize
      "// Skipping highlighting due to "
      'face 'font-lock-comment-face)
     (helpful--button
      "helpful-max-highlight"
      'helpful-describe-exactly-button
      'symbol 'helpful-max-highlight
      'callable-p nil)
     (propertize
      ".\n"
      'face 'font-lock-comment-face)
     source)))

(defun helpful--source (sym callable-p buf pos)
  "Return the source code of SYM.
If the source code cannot be found, return the sexp used."
  (catch 'source
    (unless (symbolp sym)
      (throw 'source sym))

    (let ((source nil))
      (when (and buf pos)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (goto-char pos)

              (if (and (helpful--primitive-p sym callable-p)
                       (not callable-p))
                  ;; For variables defined in .c files, only show the
                  ;; DEFVAR expression rather than the huge containing
                  ;; function.
                  (progn
                    (setq pos (line-beginning-position))
                    (forward-list)
                    (forward-char)
                    (narrow-to-region pos (point)))
                ;; Narrow to the top-level definition.
                (let ((parse-sexp-ignore-comments t))
                  (narrow-to-defun t)))

              ;; If there was a preceding comment, POS will be
              ;; after that comment. Move the position to include that comment.
              (setq pos (point-min))

              (setq source (buffer-substring-no-properties (point-min) (point-max))))))
        (setq source (s-trim-right source))
        (when (and source (buffer-file-name buf))
          (setq source (propertize source
                                   'helpful-path (buffer-file-name buf)
                                   'helpful-pos pos
                                   'helpful-pos-is-start t)))
        (throw 'source source)))

    (when callable-p
      ;; Could not find source -- probably defined interactively, or via
      ;; a macro, or file has changed.
      ;; TODO: verify that the source hasn't changed before showing.
      ;; TODO: offer to download C sources for current version.
      (throw 'source (indirect-function sym)))))

(defun helpful--has-shortdoc-p (sym)
  "Return non-nil if shortdoc.el is available and SYM is in a shortdoc group."
  (and (featurep 'shortdoc)
       (shortdoc-function-groups sym)))

(defun helpful--in-manual-p (sym)
  "Return non-nil if SYM is in an Info manual."
  (let ((completions
         (cl-letf (((symbol-function #'message)
                    (lambda (_format-string &rest _args))))
           (info-lookup->completions 'symbol 'emacs-lisp-mode))))
    (-when-let (buf (get-buffer " temp-info-look"))
      (kill-buffer buf))
    (or (assoc sym completions)
        (assoc-string sym completions))))

(defun helpful--version-info (sym)
  "If SYM has version information, format and return it.
Return nil otherwise."
  (when (symbolp sym)
    (let ((package-version
           (get sym 'custom-package-version))
          (emacs-version
           (get sym 'custom-version)))
      (cond
       (package-version
        (format
         "This variable was added, or its default value changed, in %s version %s."
         (car package-version)
         (cdr package-version)))
       (emacs-version
        (format
         "This variable was added, or its default value changed, in Emacs %s."
         emacs-version))))))

(defun helpful--library-path (library-name)
  "Find the absolute path for the source of LIBRARY-NAME.

LIBRARY-NAME takes the form \"foo.el\" , \"foo.el\" or
\"src/foo.c\".

If .elc files exist without the corresponding .el, return nil."
  (when (member (f-ext library-name) '("c" "rs"))
    (setq library-name
          (f-expand library-name
                    (f-parent find-function-C-source-directory))))
  (condition-case nil
      (find-library-name library-name)
    (error nil)))

(defun helpful--macroexpand-try (form)
  "Try to fully macroexpand FORM.
If it fails, attempt to partially macroexpand FORM."
  (catch 'result
    (ignore-errors
      ;; Happy path: we can fully expand the form.
      (throw 'result (macroexpand-all form)))
    (ignore-errors
      ;; Attempt one level of macroexpansion.
      (throw 'result (macroexpand-1 form)))
    ;; Fallback: just return the original form.
    form))

(defun helpful--tree-any-p (pred tree)
  "Walk TREE, applying PRED to every subtree.
Return t if PRED ever returns t."
  (catch 'found
    (let ((stack (list tree)))
      (while stack
        (let ((next (pop stack)))
          (cond
           ((funcall pred next)
            (throw 'found t))
           ((consp next)
            (push (car next) stack)
            (push (cdr next) stack))))))
    nil))

(defun helpful--find-by-macroexpanding (buf sym callable-p)
  "Search BUF for the definition of SYM by macroexpanding
interesting forms in BUF."
  (catch 'found
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer)))
                    (var-def-p
                     (lambda (sexp)
                       (and (eq (car-safe sexp) 'defvar)
                            (eq (car-safe (cdr sexp)) sym))))
                    (fn-def-p
                     (lambda (sexp)
                       ;; `defun' ultimately expands to `defalias'.
                       (and (eq (car-safe sexp) 'defalias)
                            (equal (car-safe (cdr sexp)) `(quote ,sym))))))
                (setq form (helpful--macroexpand-try form))

                (when (helpful--tree-any-p
                       (if callable-p fn-def-p var-def-p)
                       form)
                  ;; `read' puts point at the end of the form, so go
                  ;; back to the start.
                  (throw 'found (scan-sexps (point) -1)))))
          (end-of-file nil))))))

(defun helpful--open-if-needed (path)
  "Return a list (BUF OPENED) where BUF is a buffer visiting PATH.
If a buffer already exists, return that. If not, open PATH with
the `emacs-lisp-mode' syntax table active but skip any hooks."
  (let ((initial-buffers (buffer-list))
        (buf nil)
        (opened nil)
        ;; Skip running hooks that may prompt the user.
        (find-file-hook nil)
        ;; If we end up opening a buffer, don't bother with file
        ;; variables. It prompts the user, and we discard the buffer
        ;; afterwards anyway.
        (enable-local-variables nil))
    ;; Opening large .c files can be slow (e.g. when looking at
    ;; `defalias'), especially if the user has configured mode hooks.
    ;;
    ;; Bind `auto-mode-alist' to nil, so we open the buffer in
    ;; `fundamental-mode' if it isn't already open.
    (let ((auto-mode-alist nil))
      (setq buf (find-file-noselect path)))

    (unless (-contains-p initial-buffers buf)
      (setq opened t)

      (let ((syntax-table emacs-lisp-mode-syntax-table))
        (when (s-ends-with-p ".c" path)
          (setq syntax-table (make-syntax-table))
          (c-populate-syntax-table syntax-table))

        ;; If it's a freshly opened buffer, we need to set the syntax
        ;; table so we can search correctly.
        (with-current-buffer buf
          (set-syntax-table syntax-table))))

    (list buf opened)))

(defun helpful--definition (sym callable-p)
  "Return a list (BUF POS OPENED) where SYM is defined.

BUF is the buffer containing the definition. If the user wasn't
already visiting this buffer, OPENED is t and callers should kill
the buffer when done.

POS is the position of the start of the definition within the
buffer."
  (let ((primitive-p (helpful--primitive-p sym callable-p))
        (library-name nil)
        (src-path nil)
        (buf nil)
        (pos nil)
        (opened nil))
    ;; We shouldn't be called on primitive functions if we don't have
    ;; a directory of Emacs C sourcecode.
    (cl-assert
     (or find-function-C-source-directory
         (not primitive-p)))

    (when (symbolp sym)
      (if callable-p
          (setq library-name (cdr (find-function-library sym)))
        ;; Based on `find-variable-noselect'.
        (setq library-name
              (or
               (symbol-file sym 'defvar)
               (help-C-file-name sym 'var)))))

    (when library-name
      (setq src-path (helpful--library-path library-name)))

    (cond
     ((and (not (symbolp sym)) (functionp sym))
      (list nil nil nil))
     ((and callable-p library-name)
      (when src-path
        (-let [(src-buf src-opened) (helpful--open-if-needed src-path)]
          (setq buf src-buf)
          (setq opened src-opened))

        ;; Based on `find-function-noselect'.
        (with-current-buffer buf
          ;; `find-function-search-for-symbol' moves point. Prevent
          ;; that.
          (save-excursion
            ;; Narrowing has been fixed upstream:
            ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=abd18254aec76b26e86ae27e91d2c916ec20cc46
            (save-restriction
              (widen)
              (setq pos
                    (cdr (find-function-search-for-symbol sym nil library-name))))))
        ;; If we found the containing buffer, but not the symbol, attempt
        ;; to find it by macroexpanding interesting forms.
        (when (and buf (not pos))
          (setq pos (helpful--find-by-macroexpanding buf sym t)))))
     ;; A function, but no file found.
     (callable-p
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker)))))
     ((and (not callable-p) src-path)
      (-let [(src-buf src-opened) (helpful--open-if-needed src-path)]
        (setq buf src-buf)
        (setq opened src-opened)

        (with-current-buffer buf
          ;; `find-function-search-for-symbol' moves point. Prevent
          ;; that.
          (save-excursion
            (condition-case _err
                (setq pos (cdr (find-variable-noselect sym library-name)))
              (search-failed nil)
              ;; If your current Emacs instance doesn't match the source
              ;; code configured in find-function-C-source-directory, we can
              ;; get an error about not finding source. Try
              ;; `default-tab-width' against Emacs trunk.
              (error nil)))))))

    (list buf pos opened)))

(defun helpful--reference-positions (sym callable-p buf)
  "Return all the buffer positions of references to SYM in BUF."
  (-let* ((forms-and-bufs
           (elisp-refs--search-1
            (list buf)
            (lambda (buf)
              (elisp-refs--read-and-find
               buf sym
               (if callable-p
                   #'elisp-refs--function-p
                 #'elisp-refs--variable-p)))))
          ;; Since we only searched one buffer, we know that
          ;; forms-and-bufs has only one item.
          (forms-and-buf (-first-item forms-and-bufs))
          ((forms . _buf) forms-and-buf))
    (-map
     (-lambda ((_code start-pos _end-pos)) start-pos)
     forms)))

(defun helpful--all-keymap-syms ()
  "Return all keymaps defined in this Emacs instance."
  (let (keymaps)
    (mapatoms
     (lambda (sym)
       (when (and (boundp sym) (keymapp (symbol-value sym)))
         (push sym keymaps))))
    keymaps))

(defun helpful--key-sequences (command-sym keymap global-keycodes)
  "Return all the key sequences of COMMAND-SYM in KEYMAP."
  (let* ((keycodes
          ;; Look up this command in the keymap, its parent and the
          ;; global map. We need to include the global map to find
          ;; remapped commands.
          (where-is-internal command-sym keymap nil t))
         ;; Look up this command in the parent keymap.
         (parent-keymap (keymap-parent keymap))
         (parent-keycodes
          (when parent-keymap
            (where-is-internal
             command-sym (list parent-keymap) nil t)))
         ;; Look up this command in the global map.
         (global-keycodes
          (unless (eq keymap global-map)
            global-keycodes)))
    (->> keycodes
         ;; Ignore keybindings from the parent or global map.
         (--remove (or (-contains-p global-keycodes it)
                       (-contains-p parent-keycodes it)))
         ;; Convert raw keycode vectors into human-readable strings.
         (-map #'key-description))))

(defun helpful--keymaps-containing (command-sym)
  "Return a list of pairs listing keymap names that contain COMMAND-SYM,
along with the keybindings in each keymap.

Keymap names are typically variable names, but may also be
descriptions of values in `minor-mode-map-alist'.

We ignore keybindings that are menu items, and ignore keybindings
from parent keymaps.

`widget-global-map' is also ignored as it generally contains the
same bindings as `global-map'."
  (let* ((keymap-syms (helpful--all-keymap-syms))
         (keymap-sym-vals (-map #'symbol-value keymap-syms))
         (global-keycodes (where-is-internal
                           command-sym (list global-map) nil t))
         matching-keymaps)
    ;; Look for this command in all keymaps bound to variables.
    (-map
     (-lambda ((keymap-sym . keymap))
       (let ((key-sequences (helpful--key-sequences command-sym keymap global-keycodes)))
         (when (and key-sequences (not (eq keymap-sym 'widget-global-map)))
           (push (cons (symbol-name keymap-sym) key-sequences)
                 matching-keymaps))))
     (-zip-pair keymap-syms keymap-sym-vals))

    ;; Look for this command in keymaps used by minor modes that
    ;; aren't bound to variables.
    (-map
     (-lambda ((minor-mode . keymap))
       ;; Only consider this keymap if we didn't find it bound to a variable.
       (when (and (keymapp keymap)
                  (not (memq keymap keymap-sym-vals)))
         (let ((key-sequences (helpful--key-sequences command-sym keymap global-keycodes)))
           (when key-sequences
             (push (cons (format "minor-mode-map-alist (%s)" minor-mode)
                         key-sequences)
                   matching-keymaps)))))
     ;; TODO: examine `minor-mode-overriding-map-alist' too.
     minor-mode-map-alist)

    matching-keymaps))

(defun helpful--merge-alists (l1 l2)
  "Given two alists mapping symbols to lists, return a single
alist with the lists concatenated."
  (let* ((l1-keys (-map #'-first-item l1))
         (l2-keys (-map #'-first-item l2))
         (l2-extra-keys (-difference l2-keys l1-keys))
         (l2-extra-values
          (--map (assoc it l2) l2-extra-keys))
         (l1-with-values
          (-map (-lambda ((key . values))
                  (cons key (append values
                                    (cdr (assoc key l2)))))
                l1)))
    (append l1-with-values l2-extra-values)))

(defun helpful--keymaps-containing-aliases (command-sym aliases)
  "Return a list of pairs mapping keymap symbols to the
keybindings for COMMAND-SYM in each keymap.

Includes keybindings for aliases, unlike
`helpful--keymaps-containing'."
  (let* ((syms (cons command-sym aliases))
         (syms-keymaps (-map #'helpful--keymaps-containing syms)))
    (-reduce #'helpful--merge-alists syms-keymaps)))

(defun helpful--format-keys (command-sym aliases)
  "Describe all the keys that call COMMAND-SYM."
  (let (mode-lines
        global-lines)
    (--each (helpful--keymaps-containing-aliases command-sym aliases)
      (-let [(map . keys) it]
        (dolist (key keys)
          (push
           (format "%s %s"
                   (propertize map 'face 'font-lock-variable-name-face)
                   (if (>= emacs-major-version 28)
                       (propertize key 'face 'help-key-binding)
                     key))
           (if (eq map 'global-map) global-lines mode-lines)))))
    (setq global-lines (-sort #'string< global-lines))
    (setq mode-lines (-sort #'string< mode-lines))
    (-let [lines (-concat global-lines mode-lines)]
      (if lines
          (s-join "\n" lines)
        "This command is not in any keymaps."))))

(defun helpful--outer-sexp (buf pos)
  "Find position POS in BUF, and return the name of the outer sexp,
along with its position.

Moves point in BUF."
  (with-current-buffer buf
    (goto-char pos)
    (let* ((ppss (syntax-ppss))
           (outer-sexp-posns (nth 9 ppss)))
      (when outer-sexp-posns
        (goto-char (car outer-sexp-posns))))
    (list (point) (-take 2 (read buf)))))

(defun helpful--count-values (items)
  "Return an alist of the count of each value in ITEMS.
E.g. (x x y z y) -> ((x . 2) (y . 2) (z . 1))"
  (let (counts)
    (dolist (item items (nreverse counts))
      (-if-let (item-and-count (assoc item counts))
          (setcdr item-and-count (1+ (cdr item-and-count)))
        (push (cons item 1) counts)))))

(defun helpful--without-advice (sym)
  "Given advised function SYM, return the function object
without the advice. Assumes function has been loaded."
  (advice--cd*r
   (advice--symbol-function sym)))

(defun helpful--advised-p (sym)
  "Does SYM have advice associated with it?"
  (and (symbolp sym)
       (advice--p (advice--symbol-function sym))))

(defun helpful--format-head (head)
  "Given a 'head' (the first two symbols of a sexp) format and
syntax highlight it."
  (-let* (((def name) head)
          (formatted-name
           (if (and (consp name) (eq (car name) 'quote))
               (format "'%S" (cadr name))
             (format "%S" name)))
          (formatted-def
           (format "(%s %s ...)" def formatted-name))
          )
    (helpful--syntax-highlight formatted-def)))

(defun helpful--format-reference (head longest-head ref-count position path)
  "Return a syntax-highlighted version of HEAD, with a link
to its source location."
  (let ((formatted-count
         (format "%d reference%s"
                 ref-count (if (> ref-count 1) "s" ""))))
    (propertize
     (format
      "%s %s"
      (s-pad-right longest-head " " (helpful--format-head head))
      (propertize formatted-count 'face 'font-lock-comment-face))
     'helpful-path path
     'helpful-pos position)))

(defun helpful--format-position-heads (position-heads path)
  "Given a list of outer sexps, format them for display.
POSITION-HEADS takes the form ((123 (defun foo)) (456 (defun bar)))."
  (let ((longest-head
         (->> position-heads
              (-map (-lambda ((_pos head)) (helpful--format-head head)))
              (-map #'length)
              (-max))))
    (->> (helpful--count-values position-heads)
         (-map (-lambda (((pos head) . count))
                 (helpful--format-reference head longest-head count pos path)))
         (s-join "\n"))))

(defun helpful--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (let ((subrp (if (fboundp 'subr-primitive-p)
                   #'subr-primitive-p
                 #'subrp)))
    (cond
     ((and callable-p (helpful--advised-p sym))
      (funcall subrp (helpful--without-advice sym)))
     (callable-p
      (funcall subrp (indirect-function sym)))
     (t
      (let ((filename (find-lisp-object-file-name sym 'defvar)))
        (or (eq filename 'C-source)
            (and (stringp filename)
                 (let ((ext (file-name-extension filename)))
                   (or (equal ext "c")
                       (equal ext "rs"))))))))))

(defun helpful--sym-value (sym buf)
  "Return the value of SYM in BUF."
  (cond
   ;; If we're given a buffer, look up the variable in that buffer.
   (buf
    (with-current-buffer buf
      (symbol-value sym)))
   ;; If we don't have a buffer, and this is a buffer-local variable,
   ;; ensure we return the default value.
   ((local-variable-if-set-p sym)
    (default-value sym))
   ;; Otherwise, just return the value in the current buffer, which is
   ;; the global value.
   (t
    (symbol-value sym))))

(defun helpful--insert-section-break ()
  "Insert section break into helpful buffer."
  (insert "\n\n"))

(defun helpful--insert-implementations ()
  "When `helpful--sym' is a generic method, insert its implementations."
  (let ((func helpful--sym)
        (content))
    (when (fboundp #'cl--generic-describe)
      (with-temp-buffer
        (declare-function cl--generic-describe "cl-generic" (function))
        (cl--generic-describe func)
        (goto-char (point-min))
        (when (re-search-forward "^Implementations:$" nil t)
          (setq content (buffer-substring (point) (point-max)))))
      (when content
        (helpful--insert-section-break)
        (insert (helpful--heading "Implementations") (s-trim content))))))

(defun helpful--calculate-references (sym callable-p source-path)
  "Calculate references for SYM in SOURCE-PATH."
  (when source-path
    (let* ((primitive-p (helpful--primitive-p sym callable-p))
           (buf (elisp-refs--contents-buffer source-path))
           (positions
            (if primitive-p
                nil
              (helpful--reference-positions
               helpful--sym helpful--callable-p buf)))
           (return-value (--map (helpful--outer-sexp buf it) positions)))
      (kill-buffer buf)
      return-value)))

(defun helpful--make-shortdoc-sentence (sym)
  "Make a line for shortdoc groups of SYM."
  (when (featurep 'shortdoc)
    (-when-let (groups (--map (helpful--button
                               (symbol-name it)
                               'helpful-shortdoc-button
                               'shortdoc-group it)
                              (shortdoc-function-groups sym)))
      (if (= 1 (length groups))
          (format "Other relevant functions are documented in the %s group."
                  (car groups))
        (format "Other relevant functions are documented in the %s groups."
                (concat (s-join ", " (butlast groups))
                        " and " (car (last groups))))))))

(defun helpful--make-manual-button (sym)
  "Make manual button for SYM."
  (helpful--button
   "View in manual"
   'helpful-manual-button
   'symbol sym))

(defun helpful--make-toggle-button (sym buffer)
  "Make toggle button for SYM in BUFFER."
  (helpful--button
   "Toggle"
   'helpful-toggle-button
   'symbol sym
   'buffer buffer))

(defun helpful--make-set-button (sym buffer)
  "Make set button for SYM in BUFFER."
  (helpful--button
   "Set"
   'helpful-set-button
   'symbol sym
   'buffer buffer))

(defun helpful--make-toggle-literal-button ()
  "Make set button for SYM in BUFFER."
  (helpful--button
   (if helpful--view-literal
       ;; TODO: only offer for strings that have newlines, tabs or
       ;; properties.
       "Pretty view"
     "View as literal")
   'helpful-view-literal-button))

(defun helpful--make-customize-button (sym)
  "Make customize button for SYM."
  (helpful--button
   "Customize"
   'helpful-customize-button
   'symbol sym))

(defun helpful--make-references-button (sym callable-p)
  "Make references button for SYM."
  (helpful--button
   "Find all references"
   'helpful-all-references-button
   'symbol sym
   'callable-p callable-p))

(defun helpful--make-edebug-button (sym)
  "Make edebug button for SYM."
  (helpful--button
   (format "%s edebug"
           (if (helpful--edebug-p sym)
               "Disable" "Enable"))
   'helpful-edebug-button
   'symbol sym))

(defun helpful--make-tracing-button (sym)
  "Make tracing button for SYM."
  (helpful--button
   (format "%s tracing"
           (if (trace-is-traced sym)
               "Disable" "Enable"))
   'helpful-trace-button
   'symbol sym))

(defun helpful--make-disassemble-button (obj)
  "Make disassemble button for OBJ.
OBJ may be a symbol or a compiled function object."
  (helpful--button
   "Disassemble"
   'helpful-disassemble-button
   'object obj))

(defun helpful--make-run-test-button (sym)
  "Make an ERT test button for SYM."
  (helpful--button
   "Run test"
   'helpful-run-test-button
   'symbol sym))

(defun helpful--make-forget-button (sym callable-p)
  "Make forget button for SYM."
  (helpful--button
   "Forget"
   'helpful-forget-button
   'symbol sym
   'callable-p callable-p))

(defun helpful--make-callees-button (sym source)
  (helpful--button
   (format "Functions used by %s" sym)
   'helpful-callees-button
   'symbol sym
   'source source))

;; TODO: this only reports if a function is autoloaded because we
;; autoloaded it. This ignores newly defined functions that are
;; autoloaded. Built-in help has this limitation too, but if we can
;; find the source, we should instead see if there's an autoload
;; cookie.
(defun helpful--autoloaded-p (sym buf)
  "Return non-nil if function SYM is autoloaded."
  (-when-let (file-name (buffer-file-name buf))
    (setq file-name (s-chop-suffix ".gz" file-name))
    (condition-case nil
        (help-fns--autoloaded-p sym file-name)
      ; new in Emacs 29.0.50
      ; see https://github.com/Wilfred/helpful/pull/283
      (error (help-fns--autoloaded-p sym)))))

(defun helpful--compiled-p (sym)
  "Return non-nil if function SYM is byte-compiled"
  (and (symbolp sym)
       (byte-code-function-p (symbol-function sym))))

(defun helpful--native-compiled-p (sym)
  "Return non-nil if function SYM is native-compiled"
  (and (symbolp sym)
       (fboundp 'subr-native-elisp-p)
       (subr-native-elisp-p (symbol-function sym))))

(defun helpful--join-and (items)
  "Join a list of strings with commas and \"and\"."
  (cond
   ((= (length items) 0)
    "")
   ((= (length items) 1)
    (car items))
   (t
    (format "%s and %s"
            (s-join ", " (-drop-last 1 items))
            (-last-item items)))))

(defun helpful--summary (sym callable-p buf pos)
  "Return a one sentence summary for SYM."
  (-let* ((primitive-p (helpful--primitive-p sym callable-p))
          (canonical-sym (helpful--canonical-symbol sym callable-p))
          (alias-p (not (eq canonical-sym sym)))
          (alias-button
           (if callable-p
               ;; Show a link to 'defalias' in the manual.
               (helpful--button
                "function alias"
                'helpful-manual-button
                'symbol 'defalias)
             ;; Show a link to the variable aliases section in the
             ;; manual.
             (helpful--button
              "alias"
              'helpful-info-button
              'info-node "(elisp)Variable Aliases")))
          (special-form-button
           (helpful--button
            "special form"
            'helpful-info-button
            'info-node "(elisp)Special Forms"))
          (user-option-button
           (helpful--button
            "customizable"
            'helpful-info-button
            'info-node "(elisp)Variable Definitions"))
          (keyboard-macro-button
           (helpful--button
            "keyboard macro"
            'helpful-info-button
            'info-node "(elisp)Keyboard Macros"))
          (interactive-button
           (helpful--button
            "interactive"
            'helpful-info-button
            'info-node "(elisp)Using Interactive"))
          (autoload-button
           (helpful--button
            "autoloaded"
            'helpful-info-button
            'info-node "(elisp)Autoload"))
          (compiled-button
           (helpful--button
            "byte-compiled"
            'helpful-info-button
            'info-node "(elisp)Byte Compilation"))
          (native-compiled-button
           (helpful--button
            "natively compiled"
            'helpful-describe-button
            'symbol 'native-compile))
          (buffer-local-button
           (helpful--button
            "buffer-local"
            'helpful-info-button
            'info-node "(elisp)Buffer-Local Variables"))
          (autoloaded-p
           (and callable-p buf (helpful--autoloaded-p sym buf)))
          (compiled-p
           (and callable-p (helpful--compiled-p sym)))
          (native-compiled-p
           (and callable-p (helpful--native-compiled-p sym)))
          (buttons
           (list
            (if alias-p alias-button)
            (if (and callable-p autoloaded-p) autoload-button)
            (if (and callable-p (commandp sym)) interactive-button)
            (if compiled-p compiled-button)
            (if native-compiled-p native-compiled-button)
            (if (and (not callable-p) (custom-variable-p sym))
                user-option-button)
            (if (and (not callable-p) (local-variable-if-set-p sym))
                buffer-local-button)))
          (description
           (helpful--join-and (-non-nil buttons)))
          (kind
           (cond
            ((special-form-p sym)
             special-form-button)
            (alias-p
             (format "for %s,"
                     (helpful--button
                      (symbol-name canonical-sym)
                      'helpful-describe-exactly-button
                      'symbol canonical-sym
                      'callable-p callable-p)))
            ((not callable-p) "variable")
            ((macrop sym) "macro")
            ((helpful--kbd-macro-p sym) keyboard-macro-button)
            (t "function")))
          (defined
            (cond
             (buf
              (let ((path (buffer-file-name buf)))
                (if path
                    (format
                     "defined in %s"
                     (helpful--navigate-button
                      (file-name-nondirectory path) path pos))
                  (format "defined in buffer %s"
                          (helpful--buffer-button buf pos)))))
             (primitive-p
              "defined in C source code")
             ((helpful--kbd-macro-p sym) nil)
             (t
              "without a source file"))))

    (s-word-wrap
     70
     (format "%s is %s %s %s%s."
             (if (symbolp sym)
                 (helpful--format-symbol sym)
               "This lambda")
             (if (string-match-p
                  (rx bos (or "a" "e" "i" "o" "u"))
                  description)
                 "an"
               "a")
             description
             kind
             (if defined (concat " " defined) "")))))

(defun helpful--callees (form)
  "Given source code FORM, return a list of all the functions called."
  (let* ((expanded-form (macroexpand-all form))
         ;; Find all the functions called after macro expansion.
         (all-fns (helpful--callees-1 expanded-form))
         ;; Only consider the functions that were in the original code
         ;; before macro expansion.
         (form-syms (-filter #'symbolp (-flatten form)))
         (form-fns (--filter (memq it form-syms) all-fns)))
    (-distinct form-fns)))

(defun helpful--callees-1 (form)
  "Return a list of all the functions called in FORM.
Assumes FORM has been macro expanded. The returned list
may contain duplicates."
  (cond
   ((not (consp form))
    nil)
   ;; See `(elisp)Special Forms'. For these special forms, we recurse
   ;; just like functions but ignore the car.
   ((memq (car form) '(and catch defconst defvar if interactive
                           or prog1 prog2 progn save-current-buffer
                           save-restriction setq setq-default
                           track-mouse unwind-protect while))
    (-flatten
     (-map #'helpful--callees-1 (cdr form))))

   ((eq (car form) 'cond)
    (let* ((clauses (cdr form))
           (clause-fns
            ;; Each clause is a list of forms.
            (--map
             (-map #'helpful--callees-1 it) clauses)))
      (-flatten clause-fns)))

   ((eq (car form) 'condition-case)
    (let* ((protected-form (nth 2 form))
           (protected-form-fns (helpful--callees-1 protected-form))
           (handlers (-drop 3 form))
           (handler-bodies (-map #'cdr handlers))
           (handler-fns
            (--map
             (-map #'helpful--callees-1 it) handler-bodies)))
      (append
       protected-form-fns
       (-flatten handler-fns))))

   ;; Calling a function with a well known higher order function, for
   ;; example (funcall 'foo 1 2).
   ((and
     (memq (car form) '(funcall apply call-interactively
                                mapcar mapc mapconcat -map))
     (eq (car-safe (nth 1 form)) 'quote))
    (cons
     (cadr (nth 1 form))
     (-flatten
      (-map #'helpful--callees-1 (cdr form)))))

   ((eq (car form) 'function)
    (let ((arg (nth 1 form)))
      (if (symbolp arg)
          ;; #'foo, which is the same as (function foo), is a function
          ;; reference.
          (list arg)
        ;; Handle (function (lambda ...)).
        (helpful--callees-1 arg))))

   ((eq (car form) 'lambda)
    ;; Only consider the body, not the param list.
    (-flatten (-map #'helpful--callees-1 (-drop 2 form))))

   ((eq (car form) 'closure)
    ;; Same as lambda, but has an additional argument of the
    ;; closed-over variables.
    (-flatten (-map #'helpful--callees-1 (-drop 3 form))))

   ((memq (car form) '(let let*))
    ;; Extract function calls used to set the let-bound variables.
    (let* ((var-vals (-second-item form))
           (var-val-callees
            (--map
             (if (consp it)
                 (-map #'helpful--callees-1 it)
               nil)
             var-vals)))
      (append
       (-flatten var-val-callees)
       ;; Function calls in the let body.
       (-map #'helpful--callees-1 (-drop 2 form)))))

   ((eq (car form) 'quote)
    nil)
   (t
    (cons
     (car form)
     (-flatten
      (-map #'helpful--callees-1 (cdr form)))))))

(defun helpful--ensure-loaded ()
  "Ensure the symbol associated with the current buffer has been loaded."
  (when (and helpful--callable-p
             (symbolp helpful--sym))
    (let ((fn-obj (symbol-function helpful--sym)))
      (when (autoloadp fn-obj)
        (autoload-do-load fn-obj)))))

(defun helpful--hook-p (symbol value)
  "Does SYMBOL look like a hook?"
  (and
   (or
    (s-ends-with-p "-hook" (symbol-name symbol))
    ;; E.g. `after-change-functions', which can be used with
    ;; `add-hook'.
    (s-ends-with-p "-functions" (symbol-name symbol)))
   (consp value)))

(defun helpful--format-value (sym value)
  "Format VALUE as a string."
  (cond
   (helpful--view-literal
    (helpful--syntax-highlight (helpful--pretty-print value)))
   ;; Allow strings to be viewed with properties rendered in
   ;; Emacs, rather than as a literal.
   ((stringp value)
    value)
   ;; Allow keymaps to be viewed with keybindings shown and
   ;; links to the commands bound.
   ((keymapp value)
    (helpful--format-keymap value))
   ((helpful--hook-p sym value)
    (helpful--format-hook value))
   (t
    (helpful--pretty-print value))))

(defun helpful--original-value (sym)
  "Return the original value for SYM, if any.

If SYM has an original value, return it in a list. Return nil
otherwise."
  (let* ((orig-val-expr (get sym 'standard-value)))
    (when (consp orig-val-expr)
      (ignore-errors
        (list
         (eval (car orig-val-expr)))))))

(defun helpful--original-value-differs-p (sym)
  "Return t if SYM has an original value, and its current
value is different."
  (let ((orig-val-list (helpful--original-value sym)))
    (and (consp orig-val-list)
         (not (eq (car orig-val-list)
                  (symbol-value sym))))))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (cl-assert (not (null helpful--sym)))
  (unless (buffer-live-p helpful--associated-buffer)
    (setq helpful--associated-buffer nil))
  (helpful--ensure-loaded)
  (-let* ((val
           ;; Look at the value before setting `inhibit-read-only', so
           ;; users can see the correct value of that variable.
           (unless helpful--callable-p
             (helpful--sym-value helpful--sym helpful--associated-buffer)))
          (inhibit-read-only t)
          (start-line (line-number-at-pos))
          (start-column (current-column))
          (primitive-p (helpful--primitive-p helpful--sym helpful--callable-p))
          (canonical-sym (helpful--canonical-symbol helpful--sym helpful--callable-p))
          (look-for-src (or (not primitive-p)
                            find-function-C-source-directory))
          ((buf pos opened)
           (if look-for-src
               (helpful--definition helpful--sym helpful--callable-p)
             '(nil nil nil)))
          (source (when look-for-src
                    (helpful--source helpful--sym helpful--callable-p buf pos)))
          (source-path (when buf
                         (buffer-file-name buf)))
          (references (helpful--calculate-references
                       helpful--sym helpful--callable-p
                       source-path))
          (aliases (helpful--aliases helpful--sym helpful--callable-p)))

    (erase-buffer)

    (insert (helpful--summary helpful--sym helpful--callable-p buf pos))

    (when (helpful--obsolete-info helpful--sym helpful--callable-p)
      (insert
       "\n\n"
       (helpful--format-obsolete-info helpful--sym helpful--callable-p)))

    (when (and helpful--callable-p
               (not (helpful--kbd-macro-p helpful--sym)))
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Signature")
       (helpful--syntax-highlight (helpful--signature helpful--sym))))

    (when (not helpful--callable-p)
      (helpful--insert-section-break)
      (let* ((sym helpful--sym)
             (multiple-views-p
              (or (stringp val)
                  (keymapp val)
                  (helpful--hook-p sym val))))
        (when helpful--first-display
          (if (stringp val)
              ;; For strings, it's more intuitive to display them as
              ;; literals, so "1" and 1 are distinct.
              (setq helpful--view-literal t)
            ;; For everything else, prefer the pretty view if available.
            (setq helpful--view-literal nil)))
        (insert
         (helpful--heading
          (cond
           ;; Buffer-local variable and we're looking at the value in
           ;; a specific buffer.
           ((and
             helpful--associated-buffer
             (local-variable-p sym helpful--associated-buffer))
            (format "Value in %s"
                    (helpful--button
                     (format "#<buffer %s>" (buffer-name helpful--associated-buffer))
                     'helpful-buffer-button
                     'buffer helpful--associated-buffer
                     'position pos)))
           ;; Buffer-local variable but default/global value.
           ((local-variable-if-set-p sym)
            "Global Value")
           ;; This variable is not buffer-local.
           (t "Value")))
         (helpful--format-value sym val)
         "\n\n")
        (when (helpful--original-value-differs-p sym)
          (insert
           (helpful--heading "Original Value")
           (helpful--format-value
            sym
            (car (helpful--original-value sym)))
           "\n\n"))
        (when multiple-views-p
          (insert (helpful--make-toggle-literal-button) " "))

        (when (local-variable-if-set-p sym)
          (insert
           (helpful--button
            "Buffer values"
            'helpful-associated-buffer-button
            'symbol sym
            'prompt-p t)
           " "
           (helpful--button
            "Global value"
            'helpful-associated-buffer-button
            'symbol sym
            'prompt-p nil)
           " "))
        (when (memq (helpful--sym-value helpful--sym helpful--associated-buffer) '(nil t))
          (insert (helpful--make-toggle-button helpful--sym helpful--associated-buffer) " "))
        (insert (helpful--make-set-button helpful--sym helpful--associated-buffer))
        (when (custom-variable-p helpful--sym)
          (insert " " (helpful--make-customize-button helpful--sym)))))

    (let ((docstring (helpful--docstring helpful--sym helpful--callable-p))
          (version-info (unless helpful--callable-p
                          (helpful--version-info helpful--sym))))
      (when (or docstring version-info)
        (helpful--insert-section-break)
        (insert
         (helpful--heading "Documentation"))
        (when docstring
          (insert (helpful--format-docstring docstring)))
        (when version-info
          (insert "\n\n" (s-word-wrap 70 version-info)))
        (when (and (symbolp helpful--sym)
                   helpful--callable-p
                   (helpful--has-shortdoc-p helpful--sym))
          (insert "\n\n")
          (insert (helpful--make-shortdoc-sentence helpful--sym)))
        (when (and (symbolp helpful--sym) (helpful--in-manual-p helpful--sym))
          (insert "\n\n")
          (insert (helpful--make-manual-button helpful--sym)))))

    ;; Show keybindings.
    ;; TODO: allow users to conveniently add and remove keybindings.
    (when (commandp helpful--sym)
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Key Bindings")
       (helpful--format-keys helpful--sym aliases)))

    (helpful--insert-section-break)

    (insert
     (helpful--heading "References")
     (let ((src-button
            (when source-path
              (helpful--navigate-button
               (file-name-nondirectory source-path)
               source-path
               (or pos
                   0)))))
       (cond
        ((and source-path references)
         (format "References in %s:\n%s"
                 src-button
                 (helpful--format-position-heads references source-path)))
        ((and source-path primitive-p)
         (format
          "Finding references in a .%s file is not supported."
          (f-ext source-path)))
        (source-path
         (format "%s is unused in %s."
                 helpful--sym
                 src-button))
        ((and primitive-p (null find-function-C-source-directory))
         "C code is not yet loaded.")
        (t
         "Could not find source file.")))
     "\n\n"
     (helpful--make-references-button helpful--sym helpful--callable-p))

    (when (and
           helpful--callable-p
           (symbolp helpful--sym)
           source
           (not primitive-p))
      (insert
       " "
       (helpful--make-callees-button helpful--sym source)))

    (when (helpful--advised-p helpful--sym)
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Advice")
       (format "This %s is advised."
               (if (macrop helpful--sym) "macro" "function"))))

    (let ((can-edebug
           (helpful--can-edebug-p helpful--sym helpful--callable-p buf pos))
          (can-trace
           (and (symbolp helpful--sym)
                helpful--callable-p
                ;; Tracing uses advice, and you can't apply advice to
                ;; primitive functions that are replaced with special
                ;; opcodes. For example, `narrow-to-region'.
                (not (plist-get (symbol-plist helpful--sym) 'byte-opcode))))
          (can-disassemble
           (and helpful--callable-p (not primitive-p)))
          (can-forget
           (and (not (special-form-p helpful--sym))
                (not primitive-p))))
      (when (or can-edebug can-trace can-disassemble can-forget)
        (helpful--insert-section-break)
        (insert (helpful--heading "Debugging")))
      (when can-edebug
        (insert
         (helpful--make-edebug-button helpful--sym)))
      (when can-trace
        (when can-edebug
          (insert " "))
        (insert
         (helpful--make-tracing-button helpful--sym)))

      (when (and
             (or can-edebug can-trace)
             (or can-disassemble can-forget))
        (insert "\n"))

      (when can-disassemble
        (insert (helpful--make-disassemble-button helpful--sym)))

      (when can-forget
        (when can-disassemble
          (insert " "))
        (insert (helpful--make-forget-button helpful--sym helpful--callable-p))))

    (when aliases
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Aliases")
       (s-join "\n" (--map (helpful--format-alias it helpful--callable-p)
                           aliases))))

    (when helpful--callable-p
      (helpful--insert-implementations))

    (helpful--insert-section-break)

    (when (or source-path primitive-p)
      (insert
       (helpful--heading
        (if (eq helpful--sym canonical-sym)
            "Source Code"
          "Alias Source Code"))
       (cond
        (source-path
         (concat
          (propertize (format "%s Defined in " (if primitive-p "//" ";;"))
                      'face 'font-lock-comment-face)
          (helpful--navigate-button
           (f-abbrev source-path)
           source-path
           pos)
          "\n"))
        (primitive-p
         (concat
          (propertize
           "C code is not yet loaded."
           'face 'font-lock-comment-face)
          "\n\n"
          (helpful--button
           "Set C source directory"
           'helpful-c-source-directory))))))
    (when source
      (insert
       (cond
        ((stringp source)
         (let ((mode (when primitive-p
                       (pcase (file-name-extension source-path)
                         ("c" 'c-mode)
                         ("rs" (when (fboundp 'rust-mode) 'rust-mode))))))
           (helpful--syntax-highlight source mode)))
        ((and (consp source) (eq (car source) 'closure))
         (helpful--syntax-highlight
          (concat ";; Closure converted to defun by helpful.\n"
                  (helpful--pretty-print
                   (helpful--format-closure helpful--sym source)))))
        (t
         (helpful--syntax-highlight
          (concat
           (if (eq helpful--sym canonical-sym)
               ";; Could not find source code, showing raw function object.\n"
             ";; Could not find alias source code, showing raw function object.\n")
           (helpful--pretty-print source)))))))

    (helpful--insert-section-break)

    (-when-let (formatted-props (helpful--format-properties helpful--sym))
      (insert
       (helpful--heading "Symbol Properties")
       formatted-props))

    (goto-char (point-min))
    (forward-line (1- start-line))
    (forward-char start-column)
    (setq helpful--first-display nil)

    (when opened
      (kill-buffer buf))))

;; TODO: this isn't sufficient for `edebug-eval-defun'.
(defun helpful--skip-advice (docstring)
  "Remove mentions of advice from DOCSTRING."
  (let* ((lines (s-lines docstring))
         (relevant-lines
          (--drop-while
           (or (s-starts-with-p ":around advice:" it)
               (s-starts-with-p "This function has :around advice:" it))
           lines)))
    (s-trim (s-join "\n" relevant-lines))))

(defun helpful--format-argument (arg)
  "Format ARG (a symbol) according to Emacs help conventions."
  (let ((arg-str (symbol-name arg)))
    (if (s-starts-with-p "&" arg-str)
        arg-str
      (s-upcase arg-str))))

(defun helpful--format-symbol (sym)
  "Format symbol as a string, escaping as necessary."
  ;; Arguably this is an Emacs bug. We should be able to use
  ;; (format "%S" sym)
  ;; but that converts foo? to "foo\\?". You can see this in other
  ;; parts of the Emacs UI, such as ERT.
  (s-replace " " "\\ " (format "%s" sym)))

;; TODO: this is broken for -any?.
(defun helpful--signature (sym)
  "Get the signature for function SYM, as a string.
For example, \"(some-func FOO &optional BAR)\"."
  (let (docstring-sig
        source-sig
        (advertised-args
         (when (symbolp sym)
           (gethash (symbol-function sym) advertised-signature-table))))
    ;; Get the usage from the function definition.
    (let* ((function-args
            (cond
             ((symbolp sym)
              (help-function-arglist sym))
             ((byte-code-function-p sym)
              ;; argdesc can be a list of arguments or an integer
              ;; encoding the min/max number of arguments. See
              ;; Byte-Code Function Objects in the elisp manual.
              (let ((argdesc (aref sym 0)))
                (if (consp argdesc)
                    argdesc
                  ;; TODO: properly handle argdesc values.
                  nil)))
             (t
              ;; Interpreted function (lambda ...)
              (cadr sym))))
           (formatted-args
            (cond
             (advertised-args
              (-map #'helpful--format-argument advertised-args))
             ((listp function-args)
              (-map #'helpful--format-argument function-args))
             (t
              (list function-args)))))
      (setq source-sig
            (cond
             ;; If it's a function object, just show the arguments.
             ((not (symbolp sym))
              (format "(%s)"
                      (s-join " " formatted-args)))
             ;; If it has multiple arguments, join them with spaces.
             (formatted-args
              (format "(%s %s)"
                      (helpful--format-symbol sym)
                      (s-join " " formatted-args)))
             ;; Otherwise, this function takes no arguments when called.
             (t
              (format "(%s)" (helpful--format-symbol sym))))))

    ;; If the docstring ends with (fn FOO BAR), extract that.
    (-when-let (docstring (documentation sym))
      (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
        (setq docstring-sig (car docstring-with-usage))))

    (cond
     ;; Advertised signature always wins.
     (advertised-args
      source-sig)
     ;; If that's not set, use the usage specification in the
     ;; docstring, if present.
     (docstring-sig
      (replace-regexp-in-string "\\\\=\\(['\\`‘’]\\)" "\\1" docstring-sig t))
     (t
      ;; Otherwise, just use the signature from the source code.
      source-sig))))

(defun helpful--format-obsolete-info (sym callable-p)
  (-let [(use _ date) (helpful--obsolete-info sym callable-p)]
    (helpful--format-docstring
     (s-word-wrap
      70
      (format "This %s is obsolete%s%s"
              (helpful--kind-name sym callable-p)
              (if date (format " since %s" date)
                "")
              (cond ((stringp use) (concat "; " use))
                    (use (format "; use `%s' instead." use))
                    (t ".")))))))

(defun helpful--docstring (sym callable-p)
  "Get the docstring for SYM.
Note that this returns the raw docstring, including \\=\\=
escapes that are used by `substitute-command-keys'."
  (let ((text-quoting-style 'grave)
        docstring)
    (if callable-p
        (progn
          (setq docstring (documentation sym t))
          (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
            (setq docstring (cdr docstring-with-usage))
            (when docstring
              ;; Advice mutates the docstring, see
              ;; `advice--make-docstring'. Undo that.
              ;; TODO: Only do this if the function is advised.
              (setq docstring (helpful--skip-advice docstring)))))
      (setq docstring
            (documentation-property sym 'variable-documentation t)))
    docstring))

(defun helpful--read-symbol (prompt default-val predicate)
  "Read a symbol from the minibuffer, with completion.
Returns the symbol."
  (when (and default-val
             (not (funcall predicate default-val)))
    (setq default-val nil))
  (when default-val
    ;; `completing-read' expects a string.
    (setq default-val (symbol-name default-val))

    ;; TODO: Only modify the prompt when we don't have ido/ivy/helm,
    ;; because the default is obvious for them.
    (setq prompt
          (replace-regexp-in-string
           (rx ": " eos)
           (format " (default: %s): " default-val)
           prompt)))
  (intern (completing-read prompt obarray
                           predicate t nil nil
                           default-val)))

(defun helpful--update-and-switch-buffer (symbol callable-p)
  "Update and switch to help buffer for SYMBOL."
  (let ((buf (helpful--buffer symbol callable-p)))
    (with-current-buffer buf
      (helpful-update))
    (funcall helpful-switch-buffer-function buf)))

;;;###autoload
(defun helpful-function (symbol)
  "Show help for function named SYMBOL.

See also `helpful-macro', `helpful-command' and `helpful-callable'."
  (interactive
   (list (helpful--read-symbol
          "Function: "
          (helpful--callable-at-point)
          #'functionp)))
  (helpful--update-and-switch-buffer symbol t))

;;;###autoload
(defun helpful-command (symbol)
  "Show help for interactive function named SYMBOL.

See also `helpful-function'."
  (interactive
   (list (helpful--read-symbol
          "Command: "
          (helpful--callable-at-point)
          #'commandp)))
  (helpful--update-and-switch-buffer symbol t))

;;;###autoload
(defun helpful-key (key-sequence)
  "Show help for interactive command bound to KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  (key-description key-sequence)))
     ((commandp sym)
      (helpful--update-and-switch-buffer sym t))
     (t
      (user-error "%s is bound to %s which is not a command"
                  (key-description key-sequence)
                  sym)))))

;;;###autoload
(defun helpful-macro (symbol)
  "Show help for macro named SYMBOL."
  (interactive
   (list (helpful--read-symbol
          "Macro: "
          (helpful--callable-at-point)
          #'macrop)))
  (helpful--update-and-switch-buffer symbol t))

;;;###autoload
(defun helpful-callable (symbol)
  "Show help for function, macro or special form named SYMBOL.

See also `helpful-macro', `helpful-function' and `helpful-command'."
  (interactive
   (list (helpful--read-symbol
          "Callable: "
          (helpful--callable-at-point)
          #'fboundp)))
  (helpful--update-and-switch-buffer symbol t))

(defun helpful--variable-p (symbol)
  "Return non-nil if SYMBOL is a variable."
  (or (get symbol 'variable-documentation)
      (and (boundp symbol)
           (not (keywordp symbol))
           (not (eq symbol nil))
           (not (eq symbol t)))))

(defun helpful--bound-p (symbol)
  "Return non-nil if SYMBOL is a variable, callable, or face.

This differs from `boundp' because we do not consider nil, t
or :foo."
  (or (fboundp symbol)
      (helpful--variable-p symbol)
      (facep symbol)))

(defun helpful--bookmark-jump (bookmark)
  "Create and switch to helpful bookmark BOOKMARK."
  (let ((callable-p (bookmark-prop-get bookmark 'callable-p))
        (sym (bookmark-prop-get bookmark 'sym))
        (position (bookmark-prop-get bookmark 'position)))
    (if callable-p
        (helpful-callable sym)
      (helpful-variable sym))
    (goto-char position)))

(defun helpful--bookmark-make-record ()
  "Create a bookmark record for helpful buffers.

See docs of `bookmark-make-record-function'."
  `((sym . ,helpful--sym)
    (callable-p . ,helpful--callable-p)
    (position    . ,(point))
    (handler     . helpful--bookmark-jump)))

(defun helpful--convert-c-name (symbol var)
  "Convert SYMBOL from a C name to an Elisp name.
E.g. convert `Fmake_string' to `make-string' or
`Vgc_cons_percentage' to `gc-cons-percentage'. Interpret
SYMBOL as variable name if VAR, else a function name. Return
nil if SYMBOL doesn't begin with \"F\" or \"V\"."
  (let ((string (symbol-name symbol))
        (prefix (if var "V" "F")))
    (when (s-starts-with-p prefix string)
      (intern
       (s-chop-prefix
        prefix
        (s-replace "_" "-" string))))))

(defun helpful--disambiguate (sym choices)
  "Prompt the user to disambiguate SYM via a `read-char-choice' selection.

CHOICES is a list of tuples of the form (FN DESC CHAR), where

  CHAR is the input character associated with the choice
  DESC is a short description of the choice to display in the prompt.
  FN is the function being chosen, which takes SYM as an argument.

For instance, the choice (#'helpful-variable \"[v]ariable\" ?v)
calls (helpful-variable SYM) when the key `v' is pressed in the prompt."
  (let* ((prompt (format "%s is ambiguous: describe %s ?"
                         (propertize (symbol-name sym) 'face font-lock-keyword-face)
                         (mapconcat (-lambda ((_ desc _)) desc)
                                    choices " / ")))
         (chars (mapcar (-lambda ((_ _ char)) char)
                        choices))
         (lookup (mapcar (-lambda ((fn _ char)) (cons char fn))
                         choices))
         (input (read-char-choice prompt chars)))
    (funcall (alist-get input lookup) sym)))

;;;###autoload
(defun helpful-symbol (symbol)
  "Show help for SYMBOL, a variable, function, macro, or face.

See also `helpful-callable' and `helpful-variable'."
  (interactive
   (list (helpful--read-symbol
          "Symbol: "
          (helpful--symbol-at-point)
          #'helpful--bound-p)))
  (let (choices)
    (when-let (c-var-sym (helpful--convert-c-name symbol t))
      (push (list (lambda (_) (helpful-variable c-var-sym))
                  "c-style [V]ariable" ?V)
            choices))
    (when-let (c-fn-sym (helpful--convert-c-name symbol nil))
      (push (list (lambda (_) (helpful-callable c-fn-sym))
                  "c-style [F]unction" ?F)
            choices))
    (when (fboundp symbol)
      (push (list #'helpful-callable "[c]allable" ?c) choices))
    (when (boundp symbol)
      (push (list #'helpful-variable "[v]ariable" ?v) choices))
    (when (facep symbol)
      (push (list (lambda (face)
                    (describe-face face)
                    (funcall helpful-switch-buffer-function (help-buffer)))
                  "[f]ace" ?f)
            choices))
    (cond
     ((null choices)
      (user-error "Not bound: %S" symbol))
     ((= 1 (length choices))
      (funcall (caar choices) symbol))
     (t (helpful--disambiguate symbol choices)))))

;;;###autoload
(defun helpful-variable (symbol)
  "Show help for variable named SYMBOL."
  (interactive
   (list (helpful--read-symbol
          "Variable: "
          (helpful--variable-at-point)
          #'helpful--variable-p)))
  (helpful--update-and-switch-buffer symbol nil))

(defun helpful--variable-at-point-exactly ()
  "Return the symbol at point, if it's a bound variable."
  (let ((var (variable-at-point)))
    ;; `variable-at-point' uses 0 rather than nil to signify no symbol
    ;; at point (presumably because 'nil is a symbol).
    (unless (symbolp var)
      (setq var nil))
    (when (helpful--variable-p var)
      var)))

(defun helpful--variable-defined-at-point ()
  "Return the variable defined in the form enclosing point."
  ;; TODO: do the same thing if point is just before a top-level form.
  (save-excursion
    (save-restriction
      (widen)
      (let* ((ppss (syntax-ppss))
             (sexp-start (nth 1 ppss))
             sexp)
        (when sexp-start
          (goto-char sexp-start)
          (setq sexp (condition-case nil
                         (read (current-buffer))
                       (error nil)))
          (when (memq (car-safe sexp)
                      (list 'defvar 'defvar-local 'defcustom 'defconst))
            (nth 1 sexp)))))))

(defun helpful--variable-at-point ()
  "Return the variable exactly under point, or defined at point."
  (let ((var (helpful--variable-at-point-exactly)))
    (if var
        var
      (let ((var (helpful--variable-defined-at-point)))
        (when (helpful--variable-p var)
          var)))))

(defun helpful--callable-at-point ()
  (let ((sym (symbol-at-point))
        (enclosing-sym (function-called-at-point)))
    (if (fboundp sym)
        sym
      enclosing-sym)))

(defun helpful--symbol-at-point-exactly ()
  "Return the symbol at point, if it's bound."
  (let ((sym (symbol-at-point)))
    (when (helpful--bound-p sym)
      sym)))

(defun helpful--symbol-at-point ()
  "Find the most relevant symbol at or around point.
Returns nil if nothing found."
  (or
   (helpful--symbol-at-point-exactly)
   (helpful--callable-at-point)
   (helpful--variable-at-point)))

;;;###autoload
(defun helpful-at-point ()
  "Show help for the symbol at point."
  (interactive)
  (-if-let (symbol (helpful--symbol-at-point))
      (helpful-symbol symbol)
    (user-error "There is no symbol at point.")))

(defun helpful--imenu-index ()
  "Return a list of headings in the current buffer, suitable for
imenu."
  (let (headings)
    (goto-char (point-min))
    (while (not (eobp))
      (when (eq (get-text-property (point) 'face)
                'helpful-heading)
        (push
         (cons
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position))
          (line-beginning-position))
         headings))
      (forward-line))
    (nreverse headings)))

(defun helpful--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 1.5 nil 'delete-overlay overlay)))

(defun helpful-visit-reference ()
  "Go to the reference at point."
  (interactive)
  (let* ((sym helpful--sym)
         (path (get-text-property (point) 'helpful-path))
         (pos (get-text-property (point) 'helpful-pos))
         (pos-is-start (get-text-property (point) 'helpful-pos-is-start)))
    (when (and path pos)
      ;; If we're looking at a source excerpt, calculate the offset of
      ;; point, so we don't just go the start of the excerpt.
      (when pos-is-start
        (save-excursion
          (let ((offset 0))
            (while (and
                    (get-text-property (point) 'helpful-pos)
                    (not (eobp)))
              (backward-char 1)
              (setq offset (1+ offset)))
            ;; On the last iteration we moved outside the source
            ;; excerpt, so we overcounted by one character.
            (setq offset (1- offset))

            ;; Set POS so we go to exactly the place in the source
            ;; code where point was in the helpful excerpt.
            (setq pos (+ pos offset)))))

      (find-file path)
      (helpful--goto-char-widen pos)
      (recenter 0)
      (save-excursion
        (let ((defun-end (scan-sexps (point) 1)))
          (while (re-search-forward
                  (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
                  defun-end t)
            (helpful--flash-region (match-beginning 0) (match-end 0))))))))

(defun helpful-kill-buffers ()
  "Kill all `helpful-mode' buffers.

See also `helpful-max-buffers'."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'helpful-mode)
      (kill-buffer buffer))))

(defvar helpful-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'helpful-update)
    (define-key map [remap revert-buffer] #'helpful-update)
    (when (fboundp 'revert-buffer-quick)
      (define-key map [remap revert-buffer-quick] #'helpful-update))

    (define-key map (kbd "RET") #'helpful-visit-reference)

    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)

    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    map)
  "Keymap for `helpful-mode'.")

(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-make-record-default "bookmark"
                  (&optional no-file no-context posn))
;; Ensure this variable is defined even if bookmark.el isn't loaded
;; yet. This follows the pattern in help-mode.el.gz.
;; TODO: find a cleaner solution.
(defvar bookmark-make-record-function)

(defun helpful--add-support-for-org-links ()
  "Improve support for org \"help\" links through helpful."
  (helpful--support-storing-org-links)
  (helpful--prefer-helpful-when-following-org-link))

(defun helpful--support-storing-org-links ()
  "Make `org-store-link' in a helpful buffer return a \"help\" link."
  (when (and (fboundp 'org-link-set-parameters)
             (not (-contains-p (org-link-types) "helpful")))
    (org-link-set-parameters "helpful"
                             :store #'helpful--org-link-store)))

(defun helpful--org-link-store ()
  "Store \"help\" type link when in a helpful buffer."
  (when (derived-mode-p 'helpful-mode)
    ;; Create a "help" link instead of a dedicated "helpful" link: the
    ;; author of the Org document uses helful, but this is not
    ;; necessarily the case of the reader of the document.
    (org-link-store-props :type "help"
                          :link (format "help:%s" helpful--sym)
                          :description nil)))

(defun helpful--prefer-helpful-when-following-org-link ()
  "Prefer helpful when using `org-open-at-point' on a \"help\" link."
  (when (fboundp 'org-link-set-parameters)
    (let ((follow-function (org-link-get-parameter "help" :follow)))
      (when (not (equal follow-function #'helpful--org-link-follow))
        (org-link-set-parameters "help"
                                 :follow #'helpful--org-link-follow)))))

(defun helpful--org-link-follow (link _)
  (helpful-symbol (intern link)))

(defun helpful--outline-function (&optional bound move backward looking-at)
  "`outline-search-function' for `helpful-mode`.
See documentation of `outline-search-function' for BOUND, MOVE,
BACKWARD and LOOKING-AT."
  (if looking-at
      (eq 'helpful-heading (get-text-property (point) 'face))
    (let ((heading-found nil)
          (bound (if bound bound (if backward (point-min) (point-max)))))
      (save-excursion
        (when (eq 'helpful-heading (get-text-property (point) 'face))
          (forward-line (if backward -1 1)))
        (if backward
            (while (not (or (eq (point) bound)
                            (eq 'helpful-heading (get-text-property (point) 'face))))
              (goto-char (or (previous-single-property-change (point) 'face nil bound)
                             bound)))
          (goto-char (or (text-property-any (point) bound 'face 'helpful-heading)
                         bound)))
        (when (eq 'helpful-heading (get-text-property (point) 'face))
          (setq heading-found (point))))
      (if heading-found
          (progn
            (goto-char heading-found)
            (set-match-data (list heading-found heading-found)))
        (when move
          (goto-char bound) nil)))))

(define-derived-mode helpful-mode special-mode "Helpful"
  "Major mode for *Helpful* buffers."
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t)

  (setq imenu-create-index-function #'helpful--imenu-index)
  ;; Prevent imenu converting "Source Code" to "Source.Code".
  (setq-local imenu-space-replacement " ")

  ;; Enable users to bookmark helpful buffers.
  (set (make-local-variable 'bookmark-make-record-function)
       #'helpful--bookmark-make-record)

  ;; Enable outline support for Emacs 29 and newer
  (unless (< emacs-major-version 29)
    (setq-local outline-search-function #'helpful--outline-function))

  ;; This function should normally only be called once after Org and
  ;; helpful are loaded. To avoid using `eval-after-load' (which is
  ;; only recommended in user init files), the function is called each
  ;; time the major mode is used.
  (helpful--add-support-for-org-links))

(provide 'helpful)
;;; helpful.el ends here
