;;; compat-29.el --- Functionality added in Emacs 29.1 -*- lexical-binding: t; -*-

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

;; Functionality added in Emacs 29.1, needed by older Emacs versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-require compat-28 "28.1")

;; Preloaded in loadup.el
(compat-require seq "29.1") ;; <compat-tests:seq>

(compat-version "29.1")

;;;; Defined in startup.el

(compat-defvar lisp-directory ;; <compat-tests:lisp-directory>
    (file-truename
     (file-name-directory
      (locate-file "simple" load-path (get-load-suffixes))))
  "Directory where Emacs's own *.el and *.elc Lisp files are installed.")

;;;; Defined in window.c

(compat-defalias window-configuration-equal-p compare-window-configurations) ;; <compat-tests:window-configuration-equal-p>

;;;; Defined in xdisp.c

(compat-defun get-display-property (position prop &optional object properties) ;; <compat-tests:get-display-property>
  "Get the value of the `display' property PROP at POSITION.
If OBJECT, this should be a buffer or string where the property is
fetched from.  If omitted, OBJECT defaults to the current buffer.

If PROPERTIES, look for value of PROP in PROPERTIES instead of
the properties at POSITION."
  (if properties
      (unless (listp properties)
        (signal 'wrong-type-argument (list 'listp properties)))
    (setq properties (get-text-property position 'display object)))
  (cond
   ((vectorp properties)
    (catch 'found
      (dotimes (i (length properties))
        (let ((ent (aref properties i)))
          (when (eq (car ent) prop)
            (throw 'found (cadr ent )))))))
   ((consp (car properties))
    (condition-case nil
        (cadr (assq prop properties))
      ;; Silently handle improper lists:
      (wrong-type-argument nil)))
   ((and (consp (cdr properties))
         (eq (car properties) prop))
    (cadr properties))))

;;;; Defined in fns.c

(compat-defun ntake (n list) ;; <compat-tests:ntake>
  "Modify LIST to keep only the first N elements.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST unmodified.
Otherwise, return LIST after truncating it."
  (and (> n 0) (let ((cons (nthcdr (1- n) list)))
                 (when cons (setcdr cons nil))
                 list)))

(compat-defun take (n list) ;; <compat-tests:take>
  "Return the first N elements of LIST.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST (or a copy)."
  (declare (pure t) (side-effect-free t))
  (let (copy)
    (while (and (< 0 n) list)
      (push (pop list) copy)
      (setq n (1- n)))
    (nreverse copy)))

(compat-defun string-equal-ignore-case (string1 string2) ;; <compat-tests:string-equal-ignore-case>
  "Like `string-equal', but case-insensitive.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (declare (pure t) (side-effect-free t))
  (eq t (compare-strings string1 0 nil string2 0 nil t)))

(compat-defun plist-get (plist prop &optional predicate) ;; <compat-tests:plist-get>
  "Handle optional argument PREDICATE."
  :extended t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-get plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found (cadr plist)))
        (setq plist (cddr plist))))))

(compat-defun plist-put (plist prop val &optional predicate) ;; <compat-tests:plist-get>
  "Handle optional argument PREDICATE."
  :extended t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-put plist prop val)
    (catch 'found
      (let ((tail plist))
        (while (consp tail)
          (when (funcall predicate prop (car tail))
            (setcar (cdr tail) val)
            (throw 'found plist))
          (setq tail (cddr tail))))
      (nconc plist (list prop val)))))

(compat-defun plist-member (plist prop &optional predicate) ;; <compat-tests:plist-get>
  "Handle optional argument PREDICATE."
  :extended t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-member plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found plist))
        (setq plist (cddr plist))))))

;;;; Defined in gv.el

(compat-guard t ;; <compat-tests:plist-get-gv>
  (gv-define-expander compat--plist-get
    (lambda (do plist prop &optional predicate)
      (macroexp-let2 macroexp-copyable-p key prop
        (gv-letplace (getter setter) plist
          (macroexp-let2 nil p `(cdr (compat--plist-member ,getter ,key ,predicate))
            (funcall do
                     `(car ,p)
                     (lambda (val)
                       `(if ,p
                            (setcar ,p ,val)
                          ,(funcall setter
                                    `(cons ,key (cons ,val ,getter)))))))))))
  (unless (get 'plist-get 'gv-expander)
    (put 'plist-get 'gv-expander (get 'compat--plist-get 'gv-expander))))

;;;; Defined in editfns.c

(compat-defun pos-bol (&optional n) ;; <compat-tests:pos-bol>
  "Return the position of the first character on the current line.
With optional argument N, scan forward N - 1 lines first.
If the scan reaches the end of the buffer, return that position.

This function ignores text display directionality; it returns the
position of the first character in logical order, i.e. the smallest
character position on the logical line.  See `vertical-motion' for
movement by screen lines.

This function does not move point.  Also see `line-beginning-position'."
  (declare (side-effect-free t))
  (let ((inhibit-field-text-motion t))
    (line-beginning-position n)))

(compat-defun pos-eol (&optional n) ;; <compat-tests:pos-bol>
  "Return the position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function ignores text display directionality; it returns the
position of the last character in logical order, i.e. the largest
character position on the line.

This function does not move point.  Also see `line-end-position'."
  (declare (side-effect-free t))
  (let ((inhibit-field-text-motion t))
    (line-end-position n)))

;;;; Defined in subr.el

(compat-defmacro with-delayed-message (_args &rest body) ;; <compat-tests:with-delayed-message>
  "Like `progn', but display MESSAGE if BODY takes longer than TIMEOUT seconds.
The MESSAGE form will be evaluated immediately, but the resulting
string will be displayed only if BODY takes longer than TIMEOUT seconds.

NOTE: The compatibility function never displays the message,
which is not problematic since the only effect of the function is
to display a progress message to the user.  Backporting this
feature is not possible, since the implementation is directly
baked into the Elisp interpreter.

\(fn (timeout message) &rest body)"
  (declare (indent 1))
  (macroexp-progn body))

(compat-defun funcall-with-delayed-message (timeout message function) ;; <compat-tests:with-delayed-message>
  "Like `funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.
TIMEOUT is a number of seconds, and can be an integer or a
floating point number.  If FUNCTION takes less time to execute
than TIMEOUT seconds, MESSAGE is not displayed.

NOTE: The compatibility function never displays the message,
which is not problematic since the only effect of the function is
to display a progress message to the user.  Backporting this
feature is not possible, since the implementation is directly
baked into the Elisp interpreter."
  (ignore timeout message)
  (funcall function))

(compat-defun string-lines (string &optional omit-nulls keep-newlines) ;; <compat-tests:string-lines>
  "Handle additional KEEP-NEWLINES argument."
  :extended "28.1"
  (if (equal string "")
      (if omit-nulls
          nil
        (list ""))
    (let ((lines nil)
          (start 0))
      (while (< start (length string))
        (let ((newline (string-search "\n" string start)))
          (if newline
              (progn
                (when (or (not omit-nulls)
                          (not (= start newline)))
                  (let ((line (substring string start
                                         (if keep-newlines
                                             (1+ newline)
                                           newline))))
                    (when (not (and keep-newlines omit-nulls
                                    (equal line "\n")))
                      (push line lines))))
                (setq start (1+ newline)))
            (if (zerop start)
                (push string lines)
              (push (substring string start) lines))
            (setq start (length string)))))
      (nreverse lines))))

(compat-defun readablep (object) ;; <compat-tests:readablep>
  "Say whether OBJECT has a readable syntax.
This means that OBJECT can be printed out and then read back
again by the Lisp reader.  This function returns nil if OBJECT is
unreadable, and the printed representation (from `prin1') of
OBJECT if it is readable."
  (declare (side-effect-free error-free))
  (ignore-errors (equal object (read (prin1-to-string object)))))

(compat-defun buffer-local-restore-state (states) ;; <compat-tests:buffer-local-set-state>
  "Restore values of buffer-local variables recorded in STATES.
STATES should be an object returned by `buffer-local-set-state'."
  (dolist (state states)
    (if (cadr state)
        (set (car state) (caddr state))
      (kill-local-variable (car state)))))

(compat-defun buffer-local-set-state--get (pairs) ;; <compat-tests:buffer-local-set-state>
  "Internal helper function."
  (let ((states nil))
    (while pairs
      (push (list (car pairs)
                  (and (boundp (car pairs))
                       (local-variable-p (car pairs)))
                  (and (boundp (car pairs))
                       (symbol-value (car pairs))))
            states)
      (setq pairs (cddr pairs)))
    (nreverse states)))

(compat-defmacro buffer-local-set-state (&rest pairs) ;; <compat-tests:buffer-local-set-state>
  "Like `setq-local', but allow restoring the previous state of locals later.
This macro returns an object that can be passed to `buffer-local-restore-state'
in order to restore the state of the local variables set via this macro.

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  `(prog1
       (buffer-local-set-state--get ',pairs)
     (,(if (fboundp 'compat--setq-local) 'compat--setq-local 'setq-local)
      ,@pairs)))

(compat-defun list-of-strings-p (object) ;; <compat-tests:list-of-strings-p>
  "Return t if OBJECT is nil or a list of strings."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object) (stringp (car object)))
    (setq object (cdr object)))
  (null object))

(compat-defun plistp (object) ;; <compat-tests:plistp>
  "Non-nil if and only if OBJECT is a valid plist."
  (let ((len (proper-list-p object)))
    (and len (zerop (% len 2)))))

(compat-defun delete-line () ;; <compat-tests:delete-line>
  "Delete the current line."
  (delete-region (pos-bol) (pos-bol 2)))

(compat-defmacro with-restriction (start end &rest rest) ;; <compat-tests:with-restriction>
  "Execute BODY with restrictions set to START and END.

The current restrictions, if any, are restored upon return.

When the optional :label LABEL argument is present, in which
LABEL is a symbol, inside BODY, `narrow-to-region' and `widen'
can be used only within the START and END limits.  To gain access
to other portions of the buffer, use `without-restriction' with the
same LABEL argument.

\(fn START END [:label LABEL] BODY)"
  (declare (indent 0) (debug t))
  `(save-restriction
     (narrow-to-region ,start ,end)
     ;; Locking is ignored
     ,@(if (eq (car rest) :label) (cddr rest) rest)))

(compat-defmacro without-restriction (&rest rest) ;; <compat-tests:without-restriction>
  "Execute BODY without restrictions.

The current restrictions, if any, are restored upon return.

When the optional :label LABEL argument is present, the
restrictions set by `with-restriction' with the same LABEL argument
are lifted.

\(fn [:label LABEL] BODY)"
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     ;; Locking is ignored
     ,@(if (eq (car rest) :label) (cddr rest) rest)))

(compat-defmacro with-memoization (place &rest code) ;; <compat-tests:with-memoization>
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

(compat-defalias string-split split-string) ;; <compat-tests:string-split>

(compat-defun compiled-function-p (object) ;; <compat-tests:compiled-function-p>
  "Return non-nil if OBJECT is a function that has been compiled.
Does not distinguish between functions implemented in machine code
or byte-code."
  (or (subrp object) (byte-code-function-p object)))

(compat-defun function-alias-p (func &optional noerror) ;; <compat-tests:function-alias-p>
  "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain.

If the function alias chain contains loops, an error will be
signalled.  If NOERROR, the non-loop parts of the chain is returned."
  (declare (side-effect-free t))
  (let ((chain nil)
        (orig-func func))
    (nreverse
     (catch 'loop
       (while (and (symbolp func)
                   (setq func (symbol-function func))
                   (symbolp func))
         (when (or (memq func chain)
                   (eq func orig-func))
           (if noerror
               (throw 'loop chain)
             (signal 'cyclic-function-indirection (list orig-func))))
         (push func chain))
       chain))))

(compat-defun buffer-match-p (condition buffer-or-name &optional arg) ;; <compat-tests:buffer-match-p>
  "Return non-nil if BUFFER-OR-NAME matches CONDITION.
CONDITION is either:
- the symbol t, to always match,
- the symbol nil, which never matches,
- a regular expression, to match a buffer name,
- a predicate function that takes a buffer object and ARG as
  arguments, and returns non-nil if the buffer matches,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `derived-mode': the buffer matches if the buffer's major mode
    is derived from the major mode in the cons-cell's cdr.
  * `major-mode': the buffer matches if the buffer's major mode
    is eq to the cons-cell's cdr.  Prefer using `derived-mode'
    instead when both can work.
  * `not': the cadr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive condition, of which at
    least one has to be met."
  (letrec
      ((buffer (get-buffer buffer-or-name))
       (match
        (lambda (conditions)
          (catch 'match
            (dolist (condition conditions)
              (when (cond
                     ((eq condition t))
                     ((stringp condition)
                      (string-match-p condition (buffer-name buffer)))
                     ((functionp condition)
                      (condition-case nil
                          (funcall condition buffer)
                        (wrong-number-of-arguments
                         (funcall condition buffer arg))))
                     ((eq (car-safe condition) 'major-mode)
                      (eq
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'derived-mode)
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'not)
                      (not (funcall match (cdr condition))))
                     ((eq (car-safe condition) 'or)
                      (funcall match (cdr condition)))
                     ((eq (car-safe condition) 'and)
                      (catch 'fail
                        (dolist (c (cdr condition))
                          (unless (funcall match (list c))
                            (throw 'fail nil)))
                        t)))
                (throw 'match t)))))))
    (funcall match (list condition))))

(compat-defun match-buffers (condition &optional buffers arg) ;; <compat-tests:match-buffers>
  "Return a list of buffers that match CONDITION.
See `buffer-match' for details on CONDITION.  By default all
buffers are checked, this can be restricted by passing an
optional argument BUFFERS, set to a list of buffers to check.
ARG is passed to `buffer-match', for predicate conditions in
CONDITION."
  (let (bufs)
    (dolist (buf (or buffers (buffer-list)))
      (when (buffer-match-p condition (get-buffer buf) arg)
        (push buf bufs)))
    bufs))

(compat-defvar set-transient-map-timeout nil ;; <compat-tests:set-transient-map>
  "Timeout in seconds for deactivation of a transient keymap.
If this is a number, it specifies the amount of idle time
after which to deactivate the keymap set by `set-transient-map',
thus overriding the value of the TIMEOUT argument to that function.")

(compat-defvar set-transient-map-timer nil ;; <compat-tests:set-transient-map>
  "Timer for `set-transient-map-timeout'.")

(declare-function format-spec "format-spec")
(compat-defun set-transient-map (map &optional keep-pred on-exit message timeout) ;; <compat-tests:set-transient-map>
  "Handle the optional arguments MESSAGE and TIMEOUT."
  :extended t
  (unless (fboundp 'format-spec)
    (require 'format-spec))
  (let* ((timeout (or set-transient-map-timeout timeout))
         (message
          (when message
            (let (keys)
              (map-keymap (lambda (key cmd) (and cmd (push key keys))) map)
              (format-spec (if (stringp message) message "Repeat with %k")
                           `((?k . ,(mapconcat
                                     (lambda (key)
                                       (substitute-command-keys
                                        (format "\\`%s'"
                                                (key-description (vector key)))))
                                     keys ", ")))))))
         (clearfun (make-symbol "clear-transient-map"))
         (exitfun
          (lambda ()
            (internal-pop-keymap map 'overriding-terminal-local-map)
            (remove-hook 'pre-command-hook clearfun)
            (when message (message ""))
            (when set-transient-map-timer (cancel-timer set-transient-map-timer))
            (when on-exit (funcall on-exit)))))
    (fset clearfun
          (lambda ()
            (with-demoted-errors "set-transient-map PCH: %S"
              (if (cond
                       ((null keep-pred) nil)
                       ((and (not (eq map (cadr overriding-terminal-local-map)))
                             (memq map (cddr overriding-terminal-local-map)))
                        t)
                       ((eq t keep-pred)
                        (let ((mc (lookup-key map (this-command-keys-vector))))
                          (when (and mc (symbolp mc))
                            (setq mc (or (command-remapping mc) mc)))
                          (and mc (eq this-command mc))))
                       (t (funcall keep-pred)))
                  (when message (message "%s" message))
                (funcall exitfun)))))
    (add-hook 'pre-command-hook clearfun)
    (internal-push-keymap map 'overriding-terminal-local-map)
    (when timeout
      (when set-transient-map-timer (cancel-timer set-transient-map-timer))
      (setq set-transient-map-timer (run-with-idle-timer timeout nil exitfun)))
    (when message (message "%s" message))
    exitfun))

;;;; Defined in simple.el

(compat-defun char-uppercase-p (char) ;; <compat-tests:char-uppercase-p>
  "Return non-nil if CHAR is an upper-case character.
If the Unicode tables are not yet available, e.g. during bootstrap,
then gives correct answers only for ASCII characters."
  (cond ((unicode-property-table-internal 'lowercase)
         (characterp (get-char-code-property char 'lowercase)))
        ((and (>= char ?A) (<= char ?Z)))))

(compat-defun use-region-noncontiguous-p () ;; <compat-tests:region-noncontiguous-p>
  "Return non-nil for a non-contiguous region if `use-region-p'."
  (and (use-region-p) (region-noncontiguous-p)))

(compat-defun use-region-beginning () ;; <compat-tests:use-region>
  "Return the start of the region if `use-region-p'."
  (and (use-region-p) (region-beginning)))

(compat-defun use-region-end () ;; <compat-tests:use-region>
  "Return the end of the region if `use-region-p'."
  (and (use-region-p) (region-end)))

(compat-defun get-scratch-buffer-create () ;; <compat-tests:get-scratch-buffer-create>
  "Return the *scratch* buffer, creating a new one if needed."
  (or (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
        ;; Don't touch the buffer contents or mode unless we know that
        ;; we just created it.
        (with-current-buffer scratch
          (when initial-scratch-message
            (insert (substitute-command-keys initial-scratch-message))
            (set-buffer-modified-p nil))
          (funcall initial-major-mode))
        scratch)))

;;;; Defined in subr-x.el

(compat-defmacro with-buffer-unmodified-if-unchanged (&rest body) ;; <compat-tests:with-buffer-unmodified-if-unchanged>
  "Like `progn', but change buffer-modified status only if buffer text changes.
If the buffer was unmodified before execution of BODY, and
buffer text after execution of BODY is identical to what it was
before, ensure that buffer is still marked unmodified afterwards.
For example, the following won't change the buffer's modification
status:

  (with-buffer-unmodified-if-unchanged
    (insert \"a\")
    (delete-char -1))

Note that only changes in the raw byte sequence of the buffer text,
as stored in the internal representation, are monitored for the
purpose of detecting the lack of changes in buffer text.  Any other
changes that are normally perceived as \"buffer modifications\", such
as changes in text properties, `buffer-file-coding-system', buffer
multibyteness, etc. -- will not be noticed, and the buffer will still
be marked unmodified, effectively ignoring those changes."
  (declare (debug t) (indent 0))
  (let ((hash (gensym))
        (buffer (gensym)))
    `(let ((,hash (and (not (buffer-modified-p))
                       (buffer-hash)))
           (,buffer (current-buffer)))
       (prog1
           (progn
             ,@body)
         ;; If we didn't change anything in the buffer (and the buffer
         ;; was previously unmodified), then flip the modification status
         ;; back to "unchanged".
         (when (and ,hash (buffer-live-p ,buffer))
           (with-current-buffer ,buffer
             (when (and (buffer-modified-p)
                        (equal ,hash (buffer-hash)))
               (restore-buffer-modified-p nil))))))))

(compat-defun add-display-text-property (start end prop value ;; <compat-tests:add-display-text-property>
                                               &optional object)
  "Add display property PROP with VALUE to the text from START to END.
If any text in the region has a non-nil `display' property, those
properties are retained.

If OBJECT is non-nil, it should be a string or a buffer.  If nil,
this defaults to the current buffer."
  (let ((sub-start start)
        (sub-end 0)
        disp)
    (while (< sub-end end)
      (setq sub-end (next-single-property-change sub-start 'display object
                                                 (if (stringp object)
                                                     (min (length object) end)
                                                   (min end (point-max)))))
      (if (not (setq disp (get-text-property sub-start 'display object)))
          ;; No old properties in this range.
          (put-text-property sub-start sub-end 'display (list prop value)
                             object)
        ;; We have old properties.
        (let ((vector nil))
          ;; Make disp into a list.
          (setq disp
                (cond
                 ((vectorp disp)
                  (setq vector t)
                  (append disp nil))
                 ((not (consp (car disp)))
                  (list disp))
                 (t
                  disp)))
          ;; Remove any old instances.
          (when-let ((old (assoc prop disp)))
            (setq disp (delete old disp)))
          (setq disp (cons (list prop value) disp))
          (when vector
            (setq disp (vconcat disp)))
          ;; Finally update the range.
          (put-text-property sub-start sub-end 'display disp object)))
      (setq sub-start sub-end))))

(compat-defmacro while-let (spec &rest body) ;; <compat-tests:while-let>
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all bindings are non-nil, eval BODY and repeat.

The variable list SPEC is the same as in `if-let*'."
  (declare (indent 1) (debug if-let))
  (let ((done (gensym "done")))
    `(catch ',done
       (while t
         (if-let* ,spec
             (progn
               ,@body)
           (throw ',done nil))))))

;;;; Defined in files.el

(compat-defun directory-abbrev-make-regexp (directory) ;; <compat-tests:directory-abbrev-make-regexp>
  "Create a regexp to match DIRECTORY for `directory-abbrev-alist'."
  (let ((regexp
         ;; We include a slash at the end, to avoid spurious
         ;; matches such as `/usr/foobar' when the home dir is
         ;; `/usr/foo'.
         (concat "\\`" (regexp-quote directory) "\\(/\\|\\'\\)")))
    ;; The value of regexp could be multibyte or unibyte.  In the
    ;; latter case, we need to decode it.
    (if (multibyte-string-p regexp)
        regexp
      (decode-coding-string regexp
                            (if (eq system-type 'windows-nt)
                                'utf-8
                              locale-coding-system)))))

(compat-defun directory-abbrev-apply (filename) ;; <compat-tests:directory-abbrev-apply>
  "Apply the abbreviations in `directory-abbrev-alist' to FILENAME.
Note that when calling this, you should set `case-fold-search' as
appropriate for the filesystem used for FILENAME."
  (dolist (dir-abbrev directory-abbrev-alist filename)
    (when (string-match (car dir-abbrev) filename)
         (setq filename (concat (cdr dir-abbrev)
                                (substring filename (match-end 0)))))))

(compat-defun file-name-split (filename) ;; <compat-tests:file-name-split>
  "Return a list of all the components of FILENAME.
On most systems, this will be true:

  (equal (string-join (file-name-split filename) \"/\") filename)"
  (let ((components nil))
    ;; If this is a directory file name, then we have a null file name
    ;; at the end.
    (when (directory-name-p filename)
      (push "" components)
      (setq filename (directory-file-name filename)))
    ;; Loop, chopping off components.
    (while (length> filename 0)
      (push (file-name-nondirectory filename) components)
      (let ((dir (file-name-directory filename)))
        (setq filename (and dir (directory-file-name dir)))
        ;; If there's nothing left to peel off, we're at the root and
        ;; we can stop.
        (when (and dir (equal dir filename))
          (push (if (equal dir "") ""
                  ;; On Windows, the first component might be "c:" or
                  ;; the like.
                  (substring dir 0 -1))
                components)
          (setq filename nil))))
    components))

(compat-defun file-attribute-file-identifier (attributes) ;; <compat-tests:file-attribute-getters>
  "The inode and device numbers in ATTRIBUTES returned by `file-attributes'.
The value is a list of the form (INODENUM DEVICE), where DEVICE could be
either a single number or a cons cell of two numbers.
This tuple of numbers uniquely identifies the file."
  (nthcdr 10 attributes))

(compat-defun file-name-parent-directory (filename) ;; <compat-tests:file-name-parent-directory>
  "Return the directory name of the parent directory of FILENAME.
If FILENAME is at the root of the filesystem, return nil.
If FILENAME is relative, it is interpreted to be relative
to `default-directory', and the result will also be relative."
  (let* ((expanded-filename (expand-file-name filename))
         (parent (file-name-directory (directory-file-name expanded-filename))))
    (cond
     ;; filename is at top-level, therefore no parent
     ((or (null parent)
          ;; `equal' is enough, we don't need to resolve symlinks here
          ;; with `file-equal-p', also for performance
          (equal parent expanded-filename))
      nil)
     ;; filename is relative, return relative parent
     ((not (file-name-absolute-p filename))
      (file-relative-name parent))
     (t
      parent))))

(compat-defvar file-has-changed-p--hash-table ;; <compat-tests:file-has-changed-p>
               (make-hash-table :test #'equal)
  "Internal variable used by `file-has-changed-p'.")

(compat-defun file-has-changed-p (file &optional tag) ;; <compat-tests:file-has-changed-p>
  "Return non-nil if FILE has changed.
The size and modification time of FILE are compared to the size
and modification time of the same FILE during a previous
invocation of `file-has-changed-p'.  Thus, the first invocation
of `file-has-changed-p' always returns non-nil when FILE exists.
The optional argument TAG, which must be a symbol, can be used to
limit the comparison to invocations with identical tags; it can be
the symbol of the calling function, for example."
  (let* ((file (directory-file-name (expand-file-name file)))
         (remote-file-name-inhibit-cache t)
         (fileattr (file-attributes file 'integer))
         (attr (and fileattr
                    (cons (file-attribute-size fileattr)
                          (file-attribute-modification-time fileattr))))
         (sym (concat (symbol-name tag) "@" file))
         (cachedattr (gethash sym file-has-changed-p--hash-table)))
    (unless (equal attr cachedattr)
      (puthash sym attr file-has-changed-p--hash-table))))

;;;; Defined in keymap.el

(compat-defun key-valid-p (keys) ;; <compat-tests:key-valid-p>
  "Say whether KEYS is a valid key.
A key is a string consisting of one or more key strokes.
The key strokes are separated by single space characters.

Each key stroke is either a single character, or the name of an
event, surrounded by angle brackets.  In addition, any key stroke
may be preceded by one or more modifier keys.  Finally, a limited
number of characters have a special shorthand syntax.

Here's some example key sequences.

  \"f\"           (the key `f')
  \"S o m\"       (a three key sequence of the keys `S', `o' and `m')
  \"C-c o\"       (a two key sequence of the keys `c' with the control modifier
                 and then the key `o')
  \"H-<left>\"    (the key named \"left\" with the hyper modifier)
  \"M-RET\"       (the \"return\" key with a meta modifier)
  \"C-M-<space>\" (the \"space\" key with both the control and meta modifiers)

These are the characters that have shorthand syntax:
NUL, RET, TAB, LFD, ESC, SPC, DEL.

Modifiers have to be specified in this order:

   A-C-H-M-S-s

which is

   Alt-Control-Hyper-Meta-Shift-super"
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
    (and
     (stringp keys)
     (string-match-p "\\`[^ ]+\\( [^ ]+\\)*\\'" keys)
     (save-match-data
       (catch 'exit
         (let ((prefixes
                "\\(A-\\)?\\(C-\\)?\\(H-\\)?\\(M-\\)?\\(S-\\)?\\(s-\\)?"))
           (dolist (key (split-string keys " "))
             ;; Every key might have these modifiers, and they should be
             ;; in this order.
             (when (string-match (concat "\\`" prefixes) key)
               (setq key (substring key (match-end 0))))
             (unless (or (and (= (length key) 1)
                              ;; Don't accept control characters as keys.
                              (not (< (aref key 0) ?\s))
                              ;; Don't accept Meta'd characters as keys.
                              (or (multibyte-string-p key)
                                  (not (<= 127 (aref key 0) 255))))
                         (and (string-match-p "\\`<[-_A-Za-z0-9]+>\\'" key)
                              ;; Don't allow <M-C-down>.
                              (= (progn
                                   (string-match
                                    (concat "\\`<" prefixes) key)
                                   (match-end 0))
                                 1))
                         (string-match-p
                          "\\`\\(NUL\\|RET\\|TAB\\|LFD\\|ESC\\|SPC\\|DEL\\)\\'"
                          key))
               ;; Invalid.
               (throw 'exit nil)))
           t))))))

(compat-defun keymap--check (key) ;; <compat-tests:keymap--check>
  "Signal an error if KEY doesn't have a valid syntax."
  (unless (key-valid-p key)
    (error "%S is not a valid key definition; see `key-valid-p'" key)))

(compat-defun key-parse (keys) ;; <compat-tests:key-parse>
  "Convert KEYS to the internal Emacs key representation.
See `kbd' for a descripion of KEYS."
  (declare (pure t) (side-effect-free t))
  ;; A pure function is expected to preserve the match data.
  (save-match-data
    (let ((case-fold-search nil)
          (len (length keys)) ; We won't alter keys in the loop below.
          (pos 0)
          (res []))
      (while (and (< pos len)
                  (string-match "[^ \t\n\f]+" keys pos))
        (let* ((word-beg (match-beginning 0))
               (word-end (match-end 0))
               (word (substring keys word-beg len))
               (times 1)
               key)
          ;; Try to catch events of the form "<as df>".
          (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
              (setq word (match-string 0 word)
                    pos (+ word-beg (match-end 0)))
            (setq word (substring keys word-beg word-end)
                  pos word-end))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
                      (progn
                        (setq word (concat (match-string 1 word)
                                           (match-string 3 word)))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" keys pos)))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (setq bits (+ bits
                                   (cdr
                                    (assq (aref word 0)
                                          '((?A . ?\A-\0) (?C . ?\C-\0)
                                            (?H . ?\H-\0) (?M . ?\M-\0)
                                            (?s . ?\s-\0) (?S . ?\S-\0))))))
                     (setq prefix (+ prefix 2))
                     (setq word (substring word 2)))
                   (when (string-match "^\\^.$" word)
                     (setq bits (+ bits ?\C-\0))
                     (setq prefix (1+ prefix))
                     (setq word (substring word 1)))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (let ((n 0))
                       (dolist (ch (cdr (string-to-list word)))
                         (setq n (+ (* n 8) ch -48)))
                       (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\0) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (mapcar (lambda (x) (+ x bits))
                                            (append word nil))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\0) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\0)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (dolist (_ (number-sequence 1 times))
              (setq res (vconcat res key))))))
      res)))

(compat-defun keymap-set (keymap key definition) ;; <compat-tests:defvar-keymap>
  "Set KEY to DEFINITION in KEYMAP.
KEY is a string that satisfies `key-valid-p'.

DEFINITION is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see `make-keymap'),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)"
  (keymap--check key)
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key keymap (key-parse key) definition))

(compat-defun keymap-unset (keymap key &optional remove) ;; <compat-tests:keymap-unset>
  "Remove key sequence KEY from KEYMAP.
KEY is a string that satisfies `key-valid-p'.

If REMOVE, remove the binding instead of unsetting it.  This only
makes a difference when there's a parent keymap.  When unsetting
a key in a child map, it will still shadow the same key in the
parent keymap.  Removing the binding will allow the key in the
parent keymap to be used."
  (keymap--check key)
  (compat--define-key keymap (key-parse key) nil remove))

(compat-defun keymap-global-set (key command) ;; <compat-tests:keymap-global-set>
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function.

NOTE: The compatibility version is not a command."
  (keymap-set (current-global-map) key command))

(compat-defun keymap-local-set (key command) ;; <compat-tests:keymap-local-set>
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.

NOTE: The compatibility version is not a command."
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    (keymap-set map key command)))

(compat-defun keymap-global-unset (key &optional remove) ;; <compat-tests:keymap-global-unset>
  "Remove global binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details.

NOTE: The compatibility version is not a command."
  (keymap-unset (current-global-map) key remove))

(compat-defun keymap-local-unset (key &optional remove) ;; <compat-tests:keymap-local-unset>
  "Remove local binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details.

NOTE: The compatibility version is not a command."
  (when (current-local-map)
    (keymap-unset (current-local-map) key remove)))

(compat-defun keymap-substitute (keymap olddef newdef &optional oldmap prefix) ;; <compat-tests:keymap-substitute>
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys that are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  (define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (unless prefix
    (setq prefix ""))
  (let* ((scan (or oldmap keymap))
         (prefix1 (vconcat prefix [nil]))
         (key-substitution-in-progress
          (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(compat-defun keymap-set-after (keymap key definition &optional after) ;; <compat-tests:keymap-set-after>
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `keymap-set' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (keymap--check key)
  (when (eq after t) (setq after nil)) ; nil and t are treated the same
  (when (stringp after)
    (keymap--check after)
    (setq after (key-parse after)))
  ;; If we're binding this key to another key, then parse that other
  ;; key, too.
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key-after keymap (key-parse key) definition
    after))

(compat-defun keymap-lookup ;; <compat-tests:keymap-lookup>
    (keymap key &optional accept-default no-remap position)
  "Return the binding for command KEY.
KEY is a string that satisfies `key-valid-p'.

If KEYMAP is nil, look up in the current keymaps.  If non-nil, it
should either be a keymap or a list of keymaps, and only these
keymap(s) will be consulted.

The binding is probably a symbol with a function definition.

Normally, `keymap-lookup' ignores bindings for t, which act as
default bindings, used when nothing else in the keymap applies;
this makes it usable as a general function for probing keymaps.
However, if the optional second argument ACCEPT-DEFAULT is
non-nil, `keymap-lookup' does recognize the default bindings,
just as `read-key-sequence' does.

Like the normal command loop, `keymap-lookup' will remap the
command resulting from looking up KEY by looking up the command
in the current keymaps.  However, if the optional third argument
NO-REMAP is non-nil, `keymap-lookup' returns the unmapped
command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used."
  (keymap--check key)
  (when (and keymap position)
    (error "Can't pass in both keymap and position"))
  (if keymap
      (let ((value (lookup-key keymap (key-parse key) accept-default)))
        (if (and (not no-remap)
                   (symbolp value))
            (or (command-remapping value) value)
          value))
    (key-binding (key-parse key) accept-default no-remap position)))

(compat-defun keymap-local-lookup (keys &optional accept-default) ;; <compat-tests:keymap-local-lookup>
  "Return the binding for command KEYS in current local keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this."
  (when-let ((map (current-local-map)))
    (keymap-lookup map keys accept-default)))

(compat-defun keymap-global-lookup (keys &optional accept-default _message) ;; <compat-tests:keymap-global-lookup>
  "Return the binding for command KEYS in current global keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.
This function's return values are the same as those of `keymap-lookup'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this.

NOTE: The compatibility version is not a command."
  (keymap-lookup (current-global-map) keys accept-default))

(compat-defun define-keymap (&rest definitions) ;; <compat-tests:defvar-keymap>
  "Create a new keymap and define KEY/DEFINITION pairs as key bindings.
The new keymap is returned.

Options can be given as keywords before the KEY/DEFINITION
pairs.  Available keywords are:

:full      If non-nil, create a chartable alist (see `make-keymap').
             If nil (i.e., the default), create a sparse keymap (see
             `make-sparse-keymap').

:suppress  If non-nil, the keymap will be suppressed (see `suppress-keymap').
             If `nodigits', treat digits like other chars.

:parent    If non-nil, this should be a keymap to use as the parent
             (see `set-keymap-parent').

:keymap    If non-nil, instead of creating a new keymap, the given keymap
             will be destructively modified instead.

:name      If non-nil, this should be a string to use as the menu for
             the keymap in case you use it as a menu with `x-popup-menu'.

:prefix    If non-nil, this should be a symbol to be used as a prefix
             command (see `define-prefix-command').  If this is the case,
             this symbol is returned instead of the map itself.

KEY/DEFINITION pairs are as KEY and DEF in `keymap-set'.  KEY can
also be the special symbol `:menu', in which case DEFINITION
should be a MENU form as accepted by `easy-menu-define'.

\(fn &key FULL PARENT SUPPRESS NAME PREFIX KEYMAP &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let (full suppress parent name prefix keymap)
    ;; Handle keywords.
    (while (and definitions
                (keywordp (car definitions))
                (not (eq (car definitions) :menu)))
      (let ((keyword (pop definitions)))
        (unless definitions
          (error "Missing keyword value for %s" keyword))
        (let ((value (pop definitions)))
          (pcase keyword
            (:full (setq full value))
            (:keymap (setq keymap value))
            (:parent (setq parent value))
            (:suppress (setq suppress value))
            (:name (setq name value))
            (:prefix (setq prefix value))
            (_ (error "Invalid keyword: %s" keyword))))))

    (when (and prefix
               (or full parent suppress keymap))
      (error "A prefix keymap can't be defined with :full/:parent/:suppress/:keymap keywords"))

    (when (and keymap full)
      (error "Invalid combination: :keymap with :full"))

    (let ((keymap (cond
                   (keymap keymap)
                   (prefix (define-prefix-command prefix nil name))
                   (full (make-keymap name))
                   (t (make-sparse-keymap name))))
          seen-keys)
      (when suppress
        (suppress-keymap keymap (eq suppress 'nodigits)))
      (when parent
        (set-keymap-parent keymap parent))

      ;; Do the bindings.
      (while definitions
        (let ((key (pop definitions)))
          (unless definitions
            (error "Uneven number of key/definition pairs"))
          (let ((def (pop definitions)))
            (if (eq key :menu)
                (easy-menu-define nil keymap "" def)
              (if (member key seen-keys)
                  (error "Duplicate definition for key: %S %s" key keymap)
                (push key seen-keys))
              (keymap-set keymap key def)))))
      keymap)))

(compat-defmacro defvar-keymap (variable-name &rest defs) ;; <compat-tests:defvar-keymap>
  "Define VARIABLE-NAME as a variable with a keymap definition.
See `define-keymap' for an explanation of the keywords and KEY/DEFINITION.

In addition to the keywords accepted by `define-keymap', this
macro also accepts a `:doc' keyword, which (if present) is used
as the variable documentation string.

The `:repeat' keyword can also be specified; it controls the
`repeat-mode' behavior of the bindings in the keymap.  When it is
non-nil, all commands in the map will have the `repeat-map'
symbol property.

More control is available over which commands are repeatable; the
value can also be a property list with properties `:enter' and
`:exit', for example:

     :repeat (:enter (commands ...) :exit (commands ...))

`:enter' specifies the list of additional commands that only
enter `repeat-mode'.  When the list is empty, then only the
commands defined in the map enter `repeat-mode'.  Specifying a
list of commands is useful when there are commands that have the
`repeat-map' symbol property, but don't exist in this specific
map.

`:exit' is a list of commands that exit `repeat-mode'.  When the
list is empty, no commands in the map exit `repeat-mode'.
Specifying a list of commands is useful when those commands exist
in this specific map, but should not have the `repeat-map' symbol
property.

\(fn VARIABLE-NAME &key DOC FULL PARENT SUPPRESS NAME PREFIX KEYMAP REPEAT &rest [KEY DEFINITION]...)"
  (declare (indent 1))
  (let ((opts nil)
        doc repeat props)
    (while (and defs
                (keywordp (car defs))
                (not (eq (car defs) :menu)))
      (let ((keyword (pop defs)))
        (unless defs
          (error "Uneven number of keywords"))
        (cond
         ((eq keyword :doc) (setq doc (pop defs)))
         ((eq keyword :repeat) (setq repeat (pop defs)))
         (t (push keyword opts)
            (push (pop defs) opts)))))
    (unless (zerop (% (length defs) 2))
      (error "Uneven number of key/definition pairs: %s" defs))

    (let ((defs defs)
          key seen-keys)
      (while defs
        (setq key (pop defs))
        (pop defs)
        (unless (eq key :menu)
          (if (member key seen-keys)
              (error "Duplicate definition for key '%s' in keymap '%s'"
                     key variable-name)
            (push key seen-keys)))))

    (when repeat
      (let ((defs defs)
            def)
        (dolist (def (plist-get repeat :enter))
          (push `(put ',def 'repeat-map ',variable-name) props))
        (while defs
          (pop defs)
          (setq def (pop defs))
          (when (and (memq (car def) '(function quote))
                     (not (memq (cadr def) (plist-get repeat :exit))))
            (push `(put ,def 'repeat-map ',variable-name) props)))))

    (let ((defvar-form
           `(defvar ,variable-name
              (define-keymap ,@(nreverse opts) ,@defs)
              ,@(and doc (list doc)))))
      (if props
          `(progn
             ,defvar-form
             ,@(nreverse props))
        defvar-form))))

;;;; Defined in keymap.c

(compat-defun define-key (keymap key def &optional remove) ;; <compat-tests:define-key>
  "Handle optional argument REMOVE."
  :extended t
  (if (not remove)
      (define-key keymap key def)
    ;; Canonicalize key
    (setq key (key-parse (key-description key)))
    (define-key keymap key nil)
    ;; Split M-key in ESC key
    (setq key (mapcan (lambda (k)
                        (if (and (integerp k) (/= (logand k ?\M-\0) 0))
                            (list ?\e (logxor k ?\M-\0))
                          (list k)))
                      key))
    ;; Delete single keys directly
    (if (length= key 1)
        (delete key keymap)
      ;; Lookup submap and delete key from there
      (let ((submap (lookup-key keymap (vconcat (butlast key)))))
        (unless (keymapp submap)
          (error "Not a keymap for %s" key))
        (when (symbolp submap)
          (setq submap (symbol-function submap)))
        (delete (last key) submap)))
    def))

;;;; Defined in help.el

(compat-defun substitute-quotes (string) ;; <compat-tests:substitute-quotes>
  "Substitute quote characters for display.
Each grave accent \\=` is replaced by left quote, and each
apostrophe \\=' is replaced by right quote.  Left and right quote
characters are specified by `text-quoting-style'."
  (cond ((eq (text-quoting-style) 'curve)
         (string-replace "`" "‘"
                         (string-replace "'" "’" string)))
        ((eq (text-quoting-style) 'straight)
         (string-replace "`" "'" string))
        (t string)))

;;;; Defined in button.el

(compat-defun button--properties (callback data help-echo) ;; <compat-tests:buttonize>
  "Helper function."
  (list 'font-lock-face 'button
        'mouse-face 'highlight
        'help-echo help-echo
        'button t
        'follow-link t
        'category t
        'button-data data
        'keymap button-map
        'action callback))

(compat-defun buttonize (string callback &optional data help-echo) ;; <compat-tests:buttonize>
  "Make STRING into a button and return it.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument.

If HELP-ECHO, use that as the `help-echo' property.

Also see `buttonize-region'."
  (let ((string
         (apply #'propertize string
                (button--properties callback data help-echo))))
    ;; Add the face to the end so that it can be overridden.
    (add-face-text-property 0 (length string) 'button t string)
    string))

(compat-defun buttonize-region (start end callback &optional data help-echo) ;; <compat-tests:buttonize-region>
  "Make the region between START and END into a button.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument.

If HELP-ECHO, use that as the `help-echo' property.

Also see `buttonize'."
  (add-text-properties start end (button--properties callback data help-echo))
  (add-face-text-property start end 'button t))

;;;; Defined in rmc.el

(compat-defun read-multiple-choice  ;; <compat-tests:read-multiple-choice>
    (prompt choices &optional _help-str _show-help long-form)
    "Handle LONG-FORM argument."
  :extended t
  (if (not long-form)
      (read-multiple-choice prompt choices)
    (let ((answer
           (completing-read
            (concat prompt " ("
                    (mapconcat #'identity (mapcar #'cadr choices) "/")
                    ") ")
            (mapcar #'cadr choices) nil t)))
      (catch 'found
        (dolist (c choices)
          (when (equal answer (cadr c))
            (throw 'found c)))))))

;;;; Defined in paragraphs.el

(compat-defun count-sentences (start end) ;; <compat-tests:count-sentences>
  "Count sentences in current buffer from START to END."
  (let ((sentences 0)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (ignore-errors (forward-sentence))
          (setq sentences (1+ sentences)))
        (when (/= (skip-chars-backward " \t\n") 0)
          (setq sentences (1- sentences)))
        sentences))))

;;;; Defined in cl-lib.el

(compat-defun cl-constantly (value) ;; <compat-tests:cl-constantly>
  "Return a function that takes any number of arguments, but returns VALUE."
  :feature cl-lib
  (lambda (&rest _) value))

;;;; Defined in cl-macs.el

(compat-defmacro cl-with-gensyms (names &rest body) ;; <compat-tests:cl-with-gensyms>
  "Bind each of NAMES to an uninterned symbol and evaluate BODY."
  ;; No :feature since macro is autoloaded
  (declare (debug (sexp body)) (indent 1))
  `(let ,(cl-loop for name in names collect
                  `(,name (gensym (symbol-name ',name))))
     ,@body))

(compat-defmacro cl-once-only (names &rest body) ;; <compat-tests:cl-once-only>
  "Generate code to evaluate each of NAMES just once in BODY.

This macro helps with writing other macros.  Each of names is
either (NAME FORM) or NAME, which latter means (NAME NAME).
During macroexpansion, each NAME is bound to an uninterned
symbol.  The expansion evaluates each FORM and binds it to the
corresponding uninterned symbol.

For example, consider this macro:

    (defmacro my-cons (x)
      (cl-once-only (x)
        \\=`(cons ,x ,x)))

The call (my-cons (pop y)) will expand to something like this:

    (let ((g1 (pop y)))
      (cons g1 g1))

The use of `cl-once-only' ensures that the pop is performed only
once, as intended.

See also `macroexp-let2'."
  ;; No :feature since macro is autoloaded
  (declare (debug (sexp body)) (indent 1))
  (setq names (mapcar #'ensure-list names))
  (let ((our-gensyms (cl-loop for _ in names collect (gensym))))
    `(let ,(cl-loop for sym in our-gensyms collect `(,sym (gensym)))
       `(let ,(list
               ,@(cl-loop for name in names for gensym in our-gensyms
                          for to-eval = (or (cadr name) (car name))
                          collect ``(,,gensym ,,to-eval)))
          ,(let ,(cl-loop for name in names for gensym in our-gensyms
                          collect `(,(car name) ,gensym))
             ,@body)))))

;;;; Defined in ert-x.el

(compat-defmacro ert-with-temp-file (name &rest body) ;; <compat-tests:ert-with-temp-file>
  "Bind NAME to the name of a new temporary file and evaluate BODY.
Delete the temporary file after BODY exits normally or
non-locally.  NAME will be bound to the file name of the temporary
file.

The following keyword arguments are supported:

:prefix STRING  If non-nil, pass STRING to `make-temp-file' as
                the PREFIX argument.  Otherwise, use the value of
                `ert-temp-file-prefix'.

:suffix STRING  If non-nil, pass STRING to `make-temp-file' as the
                SUFFIX argument.  Otherwise, use the value of
                `ert-temp-file-suffix'; if the value of that
                variable is nil, generate a suffix based on the
                name of the file that `ert-with-temp-file' is
                called from.

:text STRING    If non-nil, pass STRING to `make-temp-file' as
                the TEXT argument.

:buffer SYMBOL  Open the temporary file using `find-file-noselect'
                and bind SYMBOL to the buffer.  Kill the buffer
                after BODY exits normally or non-locally.

:coding CODING  If non-nil, bind `coding-system-for-write' to CODING
                when executing BODY.  This is handy when STRING includes
                non-ASCII characters or the temporary file must have a
                specific encoding or end-of-line format.

See also `ert-with-temp-directory'."
  :feature ert-x
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type name symbol)
  (let (keyw prefix suffix directory text extra-keywords buffer coding)
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:prefix (setq prefix (pop body)))
        (:suffix (setq suffix (pop body)))
        ;; This is only for internal use by `ert-with-temp-directory'
        ;; and is therefore not documented.
        (:directory (setq directory (pop body)))
        (:text (setq text (pop body)))
        (:buffer (setq buffer (pop body)))
        (:coding (setq coding (pop body)))
        (_ (push keyw extra-keywords) (pop body))))
    (when extra-keywords
      (error "Invalid keywords: %s" (mapconcat #'symbol-name extra-keywords " ")))
    (let ((temp-file (make-symbol "temp-file"))
          (prefix (or prefix "emacs-test-"))
          (suffix (or suffix
                      (thread-last
                        (file-name-base (or (macroexp-file-name) buffer-file-name))
                        (replace-regexp-in-string (rx string-start
                                                      (group (+? not-newline))
                                                      (regexp "-?tests?")
                                                      string-end)
                                                  "\\1")
                        (concat "-")))))
      `(let* ((coding-system-for-write ,(or coding coding-system-for-write))
              (,temp-file (,(if directory 'file-name-as-directory 'identity)
                           (,(if (fboundp 'compat--make-temp-file)
                                 'compat--make-temp-file 'make-temp-file)
                            ,prefix ,directory ,suffix ,text)))
              (,name ,(if directory
                          `(file-name-as-directory ,temp-file)
                        temp-file))
              ,@(when buffer
                  (list `(,buffer (find-file-literally ,temp-file)))))
         (unwind-protect
             (progn ,@body)
           (ignore-errors
             ,@(when buffer
                 (list `(with-current-buffer ,buffer
                          (set-buffer-modified-p nil))
                       `(kill-buffer ,buffer))))
           (ignore-errors
             ,(if directory
                  `(delete-directory ,temp-file :recursive)
                `(delete-file ,temp-file))))))))

(compat-defmacro ert-with-temp-directory (name &rest body) ;; <compat-tests:ert-with-temp-directory>
  "Bind NAME to the name of a new temporary directory and evaluate BODY.
Delete the temporary directory after BODY exits normally or
non-locally.

NAME is bound to the directory name, not the directory file
name.  (In other words, it will end with the directory delimiter;
on Unix-like systems, it will end with \"/\".)

The same keyword arguments are supported as in
`ert-with-temp-file' (which see), except for :text."
  :feature ert-x
  (declare (indent 1) (debug (symbolp body)))
  (let ((tail body) keyw)
    (while (keywordp (setq keyw (car tail)))
      (setq tail (cddr tail))
      (pcase keyw (:text (error "Invalid keyword for directory: :text")))))
  `(ert-with-temp-file ,name
     :directory t
     ,@body))

;;;; Defined in wid-edit.el

(compat-guard (not (fboundp 'widget-key-validate)) ;; <compat-tests:widget-key>
  :feature wid-edit
  (defvar widget-key-prompt-value-history nil
    "History of input to `widget-key-prompt-value'.")
  (define-widget 'key 'editable-field
    "A key sequence."
    :prompt-value 'widget-field-prompt-value
    :match 'widget-key-valid-p
    :format "%{%t%}: %v"
    :validate 'widget-key-validate
    :keymap widget-key-sequence-map
    :help-echo "C-q: insert KEY, EVENT, or CODE; RET: enter value"
    :tag "Key")
  (defun widget-key-valid-p (_widget value)
    (key-valid-p value))
  (defun widget-key-validate (widget)
    (unless (and (stringp (widget-value widget))
                 (key-valid-p (widget-value widget)))
      (widget-put widget :error (format "Invalid key: %S"
                                        (widget-value widget)))
      widget)))

(provide 'compat-29)
;;; compat-29.el ends here
