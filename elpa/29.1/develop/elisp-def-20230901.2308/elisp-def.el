;;; elisp-def.el --- macro-aware go-to-definition for elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Wilfred Hughes
;; Version: 1.2

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0") (f "0.19.0") (s "1.11.0") (emacs "24.3"))

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

;; Find the definition of the symbol at point,
;; intelligently.  Understands namespaces, macros, libraries and local
;; bindings.
;;
;; See full docs at https://github.com/Wilfred/elisp-def

;;; Usage:

;; Once this file is installed (e.g. with MELPA), add the following to
;; your Emacs configuration:
;;
;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;   (add-hook hook #'elisp-def-mode))

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'find-func)
(require 'xref)
(require 'ert)

(defun elisp-def--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-def--find-library-name (library)
  "A wrapper around `find-library-name' that returns nil if PATH
has no library with that name.

This can happen when users have installed Emacs without its
source code: they have e.g. org.elc but no org.el."
  (condition-case nil
      (find-library-name library)
    (error nil)))

(defun elisp-def--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (if callable-p
      (if (fboundp 'subr-primitive-p)
          (subr-primitive-p (indirect-function sym))
        (subrp (indirect-function sym)))
    (let ((filename (find-lisp-object-file-name sym 'defvar)))
      (or (eq filename 'C-source)
          (and (stringp filename)
               (equal (file-name-extension filename) "c"))))))

(defun elisp-def--find-face (sym)
  "Find the buffer and position where face SYM is defined."
  (let (buf pos)
    (condition-case nil
        (progn
          (find-face-definition sym)
          (setq buf (current-buffer))
          (setq pos (point)))
      (error nil))
    (list buf pos)))

(defun elisp-def--find-feature (sym)
  "Find the buffer and position where feature SYM is defined."
  (let ((path (elisp-def--find-library-name (symbol-name sym)))
        buf pos)
    (when path
      (setq buf (find-file-noselect path))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            ;; TODO: caller should widen if necessary.
            (widen)

            (goto-char (point-min))
            (re-search-forward
             (rx-to-string
              `(seq "("
                    (0+ whitespace)
                    symbol-start "provide" symbol-end
                    (1+ whitespace)
                    "'" (0+ whitespace)
                    ,(symbol-name sym)))
             nil
             t)
            (setq pos (line-beginning-position))))))
    (list buf pos)))

(defun elisp-def--find-function (sym)
  "Find the buffer and position where function SYM is defined.

This is the function _slot_ of SYM, so SYM may be a function or macro."
  (let ((primitive-p (elisp-def--primitive-p sym t))
        path buf pos)
    (when (fboundp sym)
      (-let [(base-sym . src-path) (find-function-library sym)]
        ;; `base-sym' is the underlying symbol if `sym' is an alias.
        (setq sym base-sym)
        (setq path src-path)))
    (when (and primitive-p path find-function-C-source-directory)
      ;; Convert "src/foo.c" to "".
      (setq path (f-expand path
                           (f-parent find-function-C-source-directory))))

    (cond
     (path
      ;; Convert foo.elc to foo.el.
      (-when-let (src-path (elisp-def--find-library-name path))
        ;; Open `path' ourselves, so we can widen before searching.
        (setq buf (find-file-noselect src-path))

        ;; Based on `find-function-noselect'.
        (with-current-buffer buf
          ;; Temporarily widen to search the whole buffer.
          (save-restriction
            (widen)
            (setq pos
                  (if primitive-p
                      (cdr (find-function-C-source sym path nil))
                    (cdr (find-function-search-for-symbol sym nil path)))))
          ;; If the definition was found outside of the currently
          ;; narrowed region, widen.
          (when (and pos
                     (or (< pos (point-min))
                         (> pos (point-max))))
            (widen)))))
     (t
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker))))))
    (when (and buf (not pos))
      (setq pos (elisp-def--find-by-macroexpanding buf sym t)))
    (list buf pos)))

(defun elisp-def--tree-any-p (pred tree)
  "Walk TREE, applying PRED to every subtree.
Return t if PRED ever returns t."
  (cond
   ((null tree) nil)
   ((funcall pred tree) t)
   ((not (consp tree)) nil)
   (t (or
       (elisp-def--tree-any-p pred (car tree))
       (elisp-def--tree-any-p pred (cdr tree))))))

(defun elisp-def--find-by-macroexpanding (buf sym callable-p)
  "Search BUF for the definition of SYM by macroexpanding
interesting forms in BUF.

Assumes SYM is a variable, not a function."
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
                (setq form (elisp-def--macroexpand-try form))

                (when (elisp-def--tree-any-p
                       (if callable-p fn-def-p var-def-p)
                       form)
                  ;; `read' puts point at the end of the form, so go
                  ;; back to the start.
                  (throw 'found (scan-sexps (point) -1)))))
          (end-of-file nil))))))

(defun elisp-def--find-variable (sym)
  "Find the buffer and position where variable SYM is defined."
  (let (buf pos)
    (condition-case nil
        (-let [(sym-buf . sym-pos) (find-definition-noselect sym 'defvar)]
          (setq buf sym-buf)
          (setq pos sym-pos))
      (search-failed nil)
      ;; If your current Emacs instance doesn't match the source
      ;; code configured in find-function-C-source-directory, we can
      ;; get an error about not finding source. Try
      ;; `default-tab-width' against Emacs trunk.
      (error nil))
    ;; If we found the containing buffer, but not the symbol, attempt
    ;; to find it by macroexpanding interesting forms.
    (when (and buf (not pos))
      (setq pos (elisp-def--find-by-macroexpanding buf sym nil)))
    (list buf pos)))

(defun elisp-def--defined-in (sym)
  "All the namespaces that SYM is globally defined in.
Returns a list containing at most '(function variable face).

Note that macros are in the same namespace as functions."
  (let (result)
    (when (boundp sym)
      (push 'variable result))
    ;; Function or macro.
    (when (fboundp sym)
      (push 'function result))
    (when (facep sym)
      (push 'face result))
    (when (memq sym features)
      (push 'library result))
    result))

(defun elisp-def--sharp-quoted-p ()
  "Is the symbol at point of the form #'foo?"
  (save-excursion
    (re-search-forward (rx symbol-end))
    (backward-sexp)
    (looking-at (rx "#'"))))

(defun elisp-def--syntax-depth ()
  "Return the number of nested parens at point, treating strings
as just another level of nesting."
  (let* ((ppss (syntax-ppss))
         (string-start-pos (nth 8 ppss))
         depth)
    (when string-start-pos
      (save-excursion
        (goto-char string-start-pos)
        (setq ppss (syntax-ppss))))
    (setq depth (syntax-ppss-depth ppss))
    (when string-start-pos
      (setq depth (1+ depth)))
    depth))

(defun elisp-def--top-level-pos ()
  "Return the start and end positions of the form surrounding
point."
  (let* ((ppss (syntax-ppss))
         (in-comment (nth 4 ppss))
         (string-comment-start (nth 8 ppss))
         start-pos end-pos)
    (save-excursion
      (if in-comment
          ;; If we're inside a comment, just return the comment
          ;; contents.
          (progn
            (setq start-pos string-comment-start)
            (setq end-pos (line-end-position)))

        ;; If we're not in a form, we might be in a top-level symbol,
        ;; so move to the beginning.
        (while (and
                (looking-at (rx (or (syntax word) (syntax symbol))))
                (not (looking-at (rx symbol-start))))
          (backward-char))
        ;; Move past any top-level quotes.
        (when (eq (char-before) ?')
          (backward-char))
        (when (eq (char-before) ?`)
          (backward-char))
        (when (eq (char-before) ?#)
          (backward-char))
        
        ;; If we're in a string, move outside of it.
        (when string-comment-start
          (goto-char string-comment-start))
        ;; We can now move out, in sexp increments, until we're
        ;; outside of the top-level form.
        (while (nth 1 (syntax-ppss))
          (goto-char (nth 1 (syntax-ppss))))

        ;; We're now at beginning of the outer sexp, return its
        ;; position.
        (setq start-pos (point))
        (forward-sexp)
        (setq end-pos (point)))
      (list start-pos end-pos))))

(defun elisp-def--macroexpand-try (form)
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

(defun elisp-def--namespace-at-point ()
  "Is the symbol at point a function/macro, a global variable, a
quoted variable, or a let-bound variable?

Variable references in docstrings and comments are treated as
quoted variables, because they aren't being used at point."
  (catch 'done
    ;; If it's a sharp quoted symbol, we know it's a global function
    ;; reference.
    (if (elisp-def--sharp-quoted-p)
        (throw 'done 'function))

    ;; Otherwise, macro expand the source at point and look at how the
    ;; symbol is used.
    (-let* (((form-start form-end) (elisp-def--top-level-pos))
            (placeholder (elisp-def--fresh-placeholder))
            (src (elisp-def--source-with-placeholder form-start form-end placeholder))
            (form (condition-case nil
                      (read src)
                    (end-of-file nil)))
            ;; TODO: what if SYM disappears after expanding? E.g. inside rx.
            (expanded-form (elisp-def--macroexpand-try form))
            (use (elisp-def--use-position expanded-form placeholder)))
      ;; If it's being used as a variable, see if it's let-bound.
      (when (memq use (list 'variable 'string-or-comment))
        (let* ((sym (elisp-def--symbol-at-point))
               (bound-syms (elisp-def--bound-syms
                            expanded-form placeholder)))
          (when (memq sym bound-syms)
            (setq use 'bound))
          (when (eq use 'string-or-comment)
            (setq use 'quoted))))
      use)))

(defun elisp-def--proper-list-p (val)
  "Is VAL a proper list?"
  (if (fboundp 'proper-list-p)
      ;; Function was added in Emacs master:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2fde6275b69fd113e78243790bf112bbdd2fe2bf
      (with-no-warnings (proper-list-p val))
    ;; Emacs 26 only had this function in ERT.
    (with-no-warnings (ert--proper-list-p val))))

(defun elisp-def--use-position (form sym &optional quoted)
  "Is SYM being used as a function, a global variable, a
library/feature, a bound variable definition, or a quoted symbol
in FORM?

Assumes FORM has been macro-expanded."
  (cond
   ((symbolp form)
    (if (eq form sym)
        ;; Normal reference to the variable we're looking for.
        (if quoted 'quoted 'variable)
      ;; Unrelated variable.
      nil))
   ((consp form)
    (cond
     ;; The placeholder SYM was originally in a string or comment.
     ((or (equal `(elisp-def--string ,sym) form)
          (equal `(elisp-def--comment ,sym) form))
      'string-or-comment)

     ;; Used for quoting symbols that are functions. This is used in
     ;; some macros, such as `should'.
     ((equal `(function ,sym) form)
      'function)
     ;; Lambda parameters are variable definitions.
     ((and (eq (car form) 'lambda)
           (memq sym (cadr form)))
      'definition)
     ;; Let forms can introduce definitions too.
     ((and (memq (car form) (list 'let 'let*))
           (-let [(_let bindings . _body) form]
             (--any-p
              (or
               ;; (let (foo ...) ...)
               (eq it sym)
               ;; (let ((foo ...)) ...)
               (and (consp it) (eq (car it) sym)))
              bindings)))
      'definition)
     ;; Explicit call to `require'.
     ((and (eq (car form) 'require)
           (equal (car-safe (cdr form)) `(quote ,sym)))
      'library)
     ;; Explicit call to a function that is known to take a function
     ;; argument.
     ((and (memq (car form) '(funcall apply mapcar mapc))
           (equal (car-safe (cdr form)) `(quote ,sym)))
      'function)
     ((eq (car form) sym)
      ;; Function call for the symbol we're looking for.
      (if quoted 'quoted 'function))
     ;; See if this is a quoted form that contains SYM.
     ((eq (car form) 'quote)
      (if (elisp-def--proper-list-p (cdr form))
          (--any (elisp-def--use-position it sym t) (cdr form))
        (elisp-def--use-position (cdr form) sym t)))
     ;; (cond (x 1) ((foo-p) 2))
     ;; In this case, x is not a function.
     ((eq (car form) 'cond)
      (-let* (((_cond . clauses) form)
              (expressions (apply #'append clauses)))
        (--any (elisp-def--use-position it sym quoted) expressions)))
     ;; Recurse on the form to see if any arguments contain SYM.
     (t
      (if (elisp-def--proper-list-p form)
          (--any (elisp-def--use-position it sym quoted) form)
        (or
         (elisp-def--use-position (car form) sym quoted)
         (elisp-def--use-position (cdr form) sym quoted))))))
   ((vectorp form)
    ;; All elements in a vector are quoted.
    (--any (elisp-def--use-position it sym t)
           (mapcar #'identity form)))))

(defvar elisp-def--placeholder-num 0)

(defun elisp-def--fresh-placeholder ()
  "Generate a symbol that isn't used anywhere, even in
elisp-def's source code itself.

This differs from `make-symbol', as that doesn't guarantee that
the symbol _name_ is unused."
  (setq elisp-def--placeholder-num
        (1+ elisp-def--placeholder-num))
  (intern
   (format
    "elisp-def--fresh-placeholder-%s"
    elisp-def--placeholder-num)))

(defun elisp-def--source-with-placeholder (start end placeholder)
  "Return the source between START and END in the current buffer,
but with the symbol at point replaced by symbol PLACEHOLDER."
  (let* ((start-pos (point)))
    ;; Copy that expression into a separate buffer, so we can modify
    ;; the source.
    (let ((src (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (delay-mode-hooks
          (emacs-lisp-mode))
        (insert src)
        ;; Replace the original symbol at point with a placeholder, so
        ;; we can distinguish it from other occurrences of this symbol within
        ;; the sexp.
        ;;
        ;; The difference of two positions is zero-indexed, but buffer
        ;; positions are one-indexed.
        (goto-char (1+ (- start-pos start)))

        (let* ((ppss (syntax-ppss))
               (string-comment-start (nth 8 ppss))
               (in-comment (nth 4 ppss)))
          (cond
           (in-comment
            (delete-region string-comment-start (line-end-position))
            (insert (format "(elisp-def--comment %s)" placeholder)))
           (string-comment-start
            (let ((string-end
                   (progn
                     (goto-char string-comment-start)
                     (forward-sexp)
                     (point))))
              (delete-region string-comment-start string-end)
              (insert (format "(elisp-def--string %s)" placeholder))))
           (t
            (-let [(sym-start sym-end) (elisp-def--symbol-bounds)]
              (delete-region sym-start sym-end)
              (insert (symbol-name placeholder))))))

        (buffer-string)))))

(defun elisp-def--join-and (items)
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

(defun elisp-def--bound-syms-1 (form sym accum)
  "Return a list of bound symbols around the symbol SYM in FORM.

We only find bindings from special forms, caller is responsible
for macro-expanding."
  (catch 'done
    ;; If we've hit the symbol we're looking for, we can return the
    ;; bound symbols we found.
    (when (eq form sym)
      (throw 'done accum))

    (when (consp form)
      (cond
       ;; If this is a lambda form, the enclosed forms have the parameters
       ;; too.
       ((eq (car form) 'lambda)
        (-let [(_ args . body) form]
          (setq args
                (--remove (member it '(&optional &rest)) args))
          ;; If we found SYM in the parameters, we're done.
          (--each args
            (if (eq it sym)
                (throw 'done accum)))
          ;; Otherwise, add ARGS to the accumulated list and recurse.
          (--each body
            (-when-let (accum (elisp-def--bound-syms-1 it sym (append (reverse args) accum)))
              (throw 'done accum)))))
       ;; (let ((x y)) z)
       ;; We know that x is bound when we evaluate z, but not when we
       ;; evaluate y.
       ((eq (car form) 'let)
        (-let* (((_ var-vals . body) form)
                (vars nil))
          (--each var-vals
            (if (consp it)
                (-let [(var val) it]
                  (when (eq var 'XXX)
                    (throw 'done accum))
                  ;; `x' will be bound in the body.
                  (push var vars)
                  ;; Values in `let' bindings cannot have previous
                  ;; bindings, so pass ACCUM. If we see SYM in the
                  ;; value, we're done.
                  (-when-let (accum (elisp-def--bound-syms-1 val sym accum))
                    (throw 'done accum)))
              ;; Otherwise, a variable without a binding, like `z' in
              ;; our example.
              (when (eq it 'XXX)
                (throw 'done accum))
              (push it vars)))
          ;; Recurse on the body of the let.
          (--each body
            (-when-let (accum (elisp-def--bound-syms-1 it sym (append vars accum)))
              (throw 'done accum)))))
       ;; Handle `let*' forms, including bindings introduced by
       ;; previous vars.
       ((eq (car form) 'let*)
        (-let* (((_ var-vals . body) form))
          (--each var-vals
            ;; E.g. (let* ((x a) (y b) z) c)
            (if (consp it)
                (-let [(var val) it]
                  (when (eq var 'XXX)
                    (throw 'done accum))
                  ;; `a' will be evaluated without `x' bound.
                  (-when-let (accum (elisp-def--bound-syms-1 val sym accum))
                    (throw 'done accum))
                  ;; But `x' will be bound when evaluating `b'.
                  (setq accum (cons var accum)))
              ;; Otherwise, a variable without a binding, like `z' in
              ;; our example.
              (when (eq it 'XXX)
                (throw 'done accum))
              (setq accum (cons it accum))))
          (--each body
            (-when-let (accum (elisp-def--bound-syms-1 it sym accum))
              (throw 'done accum)))))
       ;; Handle `condition-case', the only other special form that
       ;; can introduce bindings.
       ((eq (car form) 'condition-case)
        (-let [(_ var bodyform . handlers) form]
          (when (eq var 'XXX)
            (throw 'done accum))
          ;; VAR is not bound when BODYFORM is evaluated.
          (-when-let (accum (elisp-def--bound-syms-1 bodyform sym accum))
            (throw 'done accum))
          (--each handlers
            (-when-let (accum (elisp-def--bound-syms-1 it sym (cons var accum)))
              (throw 'done accum)))))

       ;; For other forms (`progn' etc) then just recurse to see if it
       ;; contains SYM. We know that it introduces no new bindings. It is
       ;; actually possible to introduce a global with `setq', but we
       ;; ignore that.
       ((elisp-def--proper-list-p form)
        (--each form
          (-when-let (accum (elisp-def--bound-syms-1 it sym accum))
            (throw 'done accum))))))))

(defun elisp-def--bound-syms (form sym)
  "Return a list of bound symbols around the symbol SYM in FORM.

We only find bindings from special forms, caller is responsible
for macro-expanding."
  (nreverse (elisp-def--bound-syms-1 form sym nil)))

(defun elisp-def--symbol-bounds ()
  "Get the bounds of symbol at point.
Ignores unquote-splicing punctuation."
  (let (start end)
    (save-excursion
      (setq end (re-search-forward
                 (rx (or symbol-end buffer-end))))

      (setq start (re-search-backward
                   (rx (or symbol-start buffer-start))))
      ;; See if we're looking at ,@foo and move over the @ if so.
      (condition-case nil
          (save-excursion
            (backward-char)
            (when (looking-at ",@")
              (setq start (1+ start))))
        (beginning-of-buffer nil)))
    (list start end)))

(defun elisp-def--symbol-at-point ()
  "Get the symbol at point, even if we're on a quoted or
sharp-quoted symbol."
  (let* ((sym
          (save-excursion
            (when (looking-at (rx "#"))
              (forward-char))
            (when (looking-at (rx "'" symbol-start))
              (forward-char))
            (symbol-at-point)))
         (symbol-name (symbol-name sym))
         (ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    ;; If we're looking at ,@foo, the @ is not part of the
    ;; symbol. Otherwise, it is.
    (when (s-starts-with-p "@" symbol-name)
      (save-excursion
        (search-backward "@")
        (condition-case nil
            (backward-char)
          (beginning-of-buffer nil))
        (when (looking-at (rx ",@"))
          (setq symbol-name (s-chop-prefix "@" symbol-name))
          (setq sym (intern symbol-name)))))
    (when (or in-string in-comment)
      ;; Ignore a trailing . as it's common in docstrings but rare to
      ;; have a dot in symbols.
      (unless (or (boundp sym) (fboundp sym))
        (setq symbol-name (s-chop-suffix "." symbol-name))
        (setq sym (intern symbol-name)))

      ;; Convert FOO to foo in docstrings.
      (unless (or (boundp sym) (fboundp sym))
        (setq symbol-name (downcase symbol-name))
        (setq sym (intern symbol-name)))

      ;; Discard {}. This is a legal symbol constituent, but more
      ;; likely to be a \\{foo} than a user-defined symbol called
      ;; {foo}.
      (unless (or (boundp sym) (fboundp sym))
        (setq symbol-name
              (s-replace-all '(("{" . "") ("}" . "")) symbol-name))
        (setq sym (intern symbol-name))))
    sym))

(defun elisp-def--enclosing-form (depth)
  "Move up DEPTH sexps from point, and return the start and end
positions of the form."
  (save-excursion
    (--dotimes depth
      (let* ((ppss (syntax-ppss))
             (string-start-pos (nth 8 ppss))
             (enclosing-start-pos (nth 1 ppss)))
        (if string-start-pos
            (goto-char string-start-pos)
          (goto-char enclosing-start-pos))))
    (let (start-pos end-pos)
      (forward-sexp)
      (setq end-pos (point))
      (backward-sexp)
      (setq start-pos (point))
      (list start-pos end-pos))))

(defun elisp-def--binding-form-start ()
  "Return the start position of the form enclosing point
that binds the symbol at point.

For example, where point is shown with |, input:

\(defun foo ()
  (let (bar)
    (setq ba|r 1)))

Output:

\(defun foo ()
  |(let (bar)
     (setq bar 1)))

This an approximation: we incrementally expand macros around
point. If outer macros rewrite inner forms, we may go to the
wrong place. This should be very rare."
  (let* ((sym (elisp-def--symbol-at-point))
         (placeholder (elisp-def--fresh-placeholder)))
    (catch 'found
      ;; Start with the innermost form, and incrementally move outwards.
      (--each (number-sequence 1 (elisp-def--syntax-depth))
        ;; For each enclosing form, see if it binds the symbol at point.
        (-let* (((start end) (elisp-def--enclosing-form it))
                (src (elisp-def--source-with-placeholder
                      start end placeholder))
                (form (read src))
                (expanded-form (elisp-def--macroexpand-try form))
                (bound-syms (elisp-def--bound-syms
                             expanded-form placeholder)))
          ;; If this enclosing form introduces a binding for the
          ;; symbol we want, we've found the innermost binding!
          (when (memq sym bound-syms)
            (throw 'found start)))))))

(defun elisp-def--let-bind-index (form real-sym placeholder)
  "Given a let or let* FORM containing a PLACEHOLDER symbol,
return the 0-based index of the relevant binding.

Examples (assuming REAL-SYM is `x'):

\(let* ((x 1)
       (y 2)
       (x 3)) ; 2, because this is the relevant binding
    (placeholder))

\(let* ((x 1)
       (x (+ placeholder 1)) ; 0, because the previous binding applies
       (x 3))
  z)

We assume that PLACEHOLDER only occurs in the body for `let'
forms, but may occur as a binding value in `let*' forms."
  (catch 'done
    (-let (((_ var-vals . _body) form)
           (index nil))
      (--each-indexed var-vals
        (cond
         ((symbolp it)
          (when (eq it real-sym)
            (setq index it-index)))
         (t
          (-let [(var val) it]
            (when (-contains-p (-flatten val) placeholder)
              (throw 'done index))
            (when (eq var real-sym)
              (setq index it-index))))))
      index)))

(defun elisp-def--go-to-bind-definition (form-start sym-pos)
  "Move point to the symbol after FORM-START that binds the
variable at SYM-POS. Point is put on the first paren before the
variable.

For example, if | is point:

\(defun foo ())
\(fo|o)

=>

|(defun foo ())
\(foo)

Or for let-bound variables:

\(let ((x 1))
  (foo |x))

=>

\(let (|(x 1))
  (foo x))"
  (goto-char sym-pos)
  (let ((sym (elisp-def--symbol-at-point))
        form form-end)
    (save-excursion
      (goto-char form-start)
      (setq form (read (current-buffer)))
      ;; `read' moves point over the form.
      (setq form-end (point)))

    ;; If the enclosing form is a `let', we can calculate exactly which
    ;; binding to move to. Move to the start of that binding.
    (if (memq (car form) (list 'let 'let*))
        (let* ((placeholder (elisp-def--fresh-placeholder))
               (source (elisp-def--source-with-placeholder
                        form-start form-end placeholder))
               (form-with-placeholder (read source))
               (index (elisp-def--let-bind-index
                       form-with-placeholder
                       sym
                       placeholder)))
          (goto-char form-start)
          ;; Move past the "(let"
          (forward-char)
          (forward-sexp)

          ;; Move to the opening paren of the var-vals list, stepping
          ;; over any whitespace or comments.
          (forward-sexp)
          (backward-sexp)

          ;; Now move to the INDEXth var-val pair.
          (forward-char)
          (forward-sexp index)

          ;; Ensure we're on the opening paren.
          (forward-sexp)
          (backward-sexp))

      ;; Otherwise, we have to assume the first occurrence of the
      ;; symbol is the definition. This is a heuristic, but it works
      ;; for many macros like `destructuring-bind'.
      (goto-char form-start))))

(defun elisp-def--show-occurrence (sym)
  "Go to and highlight SYM in the form after point.
Point is placed on the first character of SYM.

If SYM isn't present, use the most relevant symbol."
  (save-match-data
    (let (sym-end-pos)
      (cond
       ((or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode) (derived-mode-p 'objc-mode))
        ;; move to the quoted function/variable name string; the bound is after
        ;; two sexps: one DEFUN/DEFVAR/... followed by a parenthesised list of
        ;; arguments.
        (re-search-forward "\"" (scan-sexps (point) 2))
        (save-excursion
          (backward-char)
          (setq sym-end-pos (1- (scan-sexps (point) 1)))))
       (t
        (let ((form-end-pos (scan-sexps (point) 1)))
          (when
              (re-search-forward
               (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
               form-end-pos
               t)
            (setq sym-end-pos (point)))
          ;; If we couldn't find the symbol, use the second symbol in the
          ;; form. This is the best we can do when the symbol doesn't occur
          ;; (e.g. a foo-mode-hook variable or a make-foo function from a
          ;; struct).
          (unless sym-end-pos
            ;; Move past the opening paren.
            (forward-char)
            ;; Move past the first sexp.
            (forward-sexp)
            (forward-char)
            ;; Move the second symbol.
            (setq
             sym-end-pos
             (re-search-forward (rx symbol-end) form-end-pos t)))

          ;; Put point on the first character of the symbol.
          (goto-char (scan-sexps sym-end-pos -1)))))

      (elisp-def--flash-region (point) sym-end-pos))))

;;;###autoload
(defun elisp-def ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let* ((init-pos (point))
         (sym (elisp-def--symbol-at-point))
         (sym-name (symbol-name sym))
         ;; Try to find the namespace by macro expanding the code.
         (namespace (elisp-def--namespace-at-point)))
    ;; If we couldn't identify a function or variable, see which
    ;; namespaces this symbol is bound in.
    (when (eq namespace 'quoted)
      (-let [namespaces (elisp-def--defined-in sym)]
        (when (null namespaces)
          (user-error "Couldn't identify where %s is defined"
                      sym-name))

        ;; If the symbol is only bound in one namespace, use that.
        (if (= (length namespaces) 1)
            (setq namespace (car namespaces))
          ;; Otherwise, our static analysis has failed, so just ask
          ;; the user.
          (let* ((formatted-namespaces
                  (elisp-def--join-and
                   (--map (format "a %s" it) namespaces)))
                 (prompt (format "%s is %s, choose: "
                                 sym-name
                                 formatted-namespaces)))
            (setq namespace
                  (intern
                   (completing-read prompt namespaces nil t)))))))

    ;; Push the current position, so we can go back.
    (xref-push-marker-stack)
    (when (not (region-active-p))
      (push-mark))
	
    (-let [(buf pos)
           (cond
            ((eq namespace 'bound)
             (list (current-buffer)
                   (elisp-def--binding-form-start)))
            ((eq namespace 'library)
             (elisp-def--find-feature sym))
            ((eq namespace 'variable)
             (elisp-def--find-variable sym))
            ((eq namespace 'function)
             (elisp-def--find-function sym))
            ((eq namespace 'face)
             (elisp-def--find-face sym)))]
      (unless (and buf pos)
        ;; todo: mention if it's due to being a primitive
        (user-error "Couldn't find definition for %s %s"
                    namespace sym))

      (switch-to-buffer buf)
      (goto-char pos))

    (when (eq namespace 'bound)
      (elisp-def--go-to-bind-definition (point) init-pos))

    (elisp-def--show-occurrence sym)))

(defvar elisp-def-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'elisp-def)
    (if (functionp 'xref-go-back)
        (define-key map (kbd "M-,") #'xref-go-back)
      (define-key map (kbd "M-,") #'xref-pop-marker-stack))
    map)
  "Keymap used in command `elisp-def-mode'.")

;;;###autoload
(define-minor-mode elisp-def-mode
  "Minor mode for finding definitions with `elisp-def'.

\\{elisp-def-mode-map}"
  :lighter " ElispDef"
  :keymap elisp-def-mode-map)

(provide 'elisp-def)
;;; elisp-def.el ends here
