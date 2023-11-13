;;; evil-ex.el --- Ex mode  -*- lexical-binding: t; -*-

;; Author: Frank Fischer <frank fischer at mathematik.tu-chemnitz.de>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `evil-ex-grammar'.  Ex commands are defined with
;; `evil-ex-define-cmd', which creates a binding from a string
;; to an interactive function.  It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `evil-ex-shortcut-map'.

;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `evil-ex-define-argument-type'.  In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

;;; Code:

(require 'evil-common)
(require 'evil-states)

(declare-function evil-goto-line "evil-commands")

(eval-when-compile
  (defconst evil-ex-grammar
    '((expression
       (count command argument #'evil-ex-call-command)
       ((\? range) command argument #'evil-ex-call-command)
       (line #'evil-goto-line)
       (sexp #'eval-expression))
      (count
       number)
      (command #'evil-ex-parse-command)
      (binding
       "[~&*@<>=:#]+\\|[[:alpha:]_]+\\|!")
      (emacs-binding
       "[[:alpha:]-][[:alnum:][:punct:]]*")
      (argument
       ((\? "\\(?:.\\|\n\\)+") #'$1))
      (range
       ("%" #'(evil-ex-full-range))
       ("*" #'(evil-ex-last-visual-range))
       ((\? line) "[,;]" (\? line)
        #'(let ((l1 $1))
            (save-excursion
              (and l1 (string= $2 ";") (goto-line l1))
              (evil-ex-range l1 $3))))
       (line #'evil-ex-range)
       ("`" marker-name ",`" marker-name
        #'(evil-ex-char-marker-range $2 $4)))
      (line
       ((\? base) (\? offset) search (\? offset)
        #'(save-excursion
            (goto-line (evil-ex-line $1 $2))
            (evil-ex-line $3 $4)))
       (base (\? offset) #'evil-ex-line)
       (nil offset #'evil-ex-line))
      (base
       number
       ("'" marker-name #'(evil-ex-marker $2))
       search
       ("\\^" #'(evil-ex-first-line))
       ("\\$" #'(evil-ex-last-line))
       ("\\." #'(evil-ex-current-line)))
      (offset
       (+ signed-number #'+))
      ;; TODO - handle offset & ;next-pattern search elements
      (search
       forward
       backward
       next
       prev
       subst)
      (forward
       ("/" "\\(?:\\\\.\\|[^/]\\)+" "/\\|$" #'(evil-ex-re-fwd $2)))
      (backward
       ("?" "\\(?:\\\\.\\|[^?]\\)+" "?\\|$" #'(evil-ex-re-bwd $2)))
      (marker-name
       "[]\\[-a-zA-Z_<>'}{)(]")
      (next
       "\\\\/" #'(evil-ex-prev-search))
      (prev
       "\\\\\\?" #'(evil-ex-prev-search))
      (subst
       "\\\\&" #'(evil-ex-prev-search))
      (signed-number
       (sign (\? number) #'evil-ex-signed-number))
      (sign
       "[+-]" #'intern)
      (number
       "[0-9]+" #'string-to-number)
      (sexp
       "(.*)?" #'(car (read-from-string $0))))
    "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `evil-parser' for a detailed explanation
of the syntax.")

  (defun evil-parser--dexp (obj)
    "Parse a numerical dollar-sign symbol.
Given e.g. $4, return 4."
    (when (symbolp obj)
      (let ((str (symbol-name obj)))
        (when (string-match "\\`\\$\\([0-9]+\\)\\'" str)
          (string-to-number (match-string 1 str))))))

  (defmacro evil-parser (grammar &rest entrypoints)
    "Construct a parser for GRAMMAR with ENTRYPOINTS.
The result is a function taking the arguments SYMBOL and SYNTAX, that
parses the text after point. SYMBOL should be one of ENTRYPOINTS.

If parsing succeeds, point is moved to the end of the parsed text and
a 1-tuple (RESULT) is returned. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is a list of production rules, which are tried in
succession. A production rule can be one of the following:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

Thus, a simple grammar may look like:

    ((plus \"\\\\+\")       ; plus <- \"+\"
     (minus \"-\")          ; minus <- \"-\"
     (operator plus minus)) ; operator <- plus / minus

All input-consuming rules have a value. A regular expression evaluates
to the text matched, while a list evaluates to a list of values.
The value of a list may be overridden with a semantic action, which is
specified with a #\\='-quoted expression at the end:

    (X Y #\\='foo)

The value of this rule is the result of calling foo with the values
of X and Y as arguments. Alternatively, the function call may be
specified explicitly:

    (X Y #\\='(foo $1 $2))

Here, $1 refers to X and $2 refers to Y. $0 refers to the whole list.
Dollar expressions can also be used directly:

    (X Y #\\='$1)

This matches X followed by Y, but ignores the value of Y;
the value of the list is the same as the value of X.

If the SYNTAX argument is non-nil, then all semantic actions
are ignored, and a syntax tree is constructed instead. The
syntax tree obeys the property that all the leaf nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
    (cl-labels
        ;; Return code for parsing PRODUCTION.
        ((compile
          (production)
          (pcase production
            ('nil '(list (when syntax (point)))) ; Epsilon
            ((and (pred stringp) regexp) ; Token
             `(when (progn
                      (skip-chars-forward " \t\n\r") ; Ignore leading whitespace
                      (looking-at ,regexp))
                (goto-char (match-end 0))
                (list (if syntax (point) (match-string 0)))))
            ((and (pred symbolp) symbol) ; Symbol
             `(let ((result (,symbol syntax)))
                (and syntax result (push ',symbol (car result)))
                result))
            (`(function ,fun) ; Function
             `(let ((result (funcall #',fun)))
                (and syntax result (setcar result (point)))
                result))
            ;; Positive lookahead
            (`(& . ,rule) `(when ,(compile rule) ,(compile nil)))
            ;; Negative lookahead
            (`(! . ,rule) `(unless ,(compile rule) ,(compile nil)))
            ;; Zero or one
            (`(\? . ,(or `(,rule) rule)) (compile `(alt ,rule nil)))
            ;; Zero or more
            (`(* . ,rules) (compile `(alt (+ ,@rules) nil)))
            ;; Lists
            ((or `(,(and (or '+ 'alt 'seq) symbol) . ,rules)
                 (and (pred listp) rules (let symbol 'seq)))
             (let ((func (unless (eq symbol 'alt) #'list)))
               (pcase (when (> (length rules) 1) (car (last rules)))
                 (`(function ,x) (setq func x
                                       rules (butlast rules))))
               `(let ((cell
                       ,(pcase symbol
                          ('+ ; One or more
                           (when (cdr rules) (error "Too many `+' rules"))
                           `(cl-loop for x = ,(compile (car rules))
                                     while x collect (car x) into result until (eobp)
                                     finally return (when result (list result))))
                          ('alt `(or ,@(mapcar #'compile rules)))
                          ('seq
                           (cl-loop
                            for rule in rules collect
                            (macroexp-let2 nil x (compile rule)
                              (if (memq (car-safe rule) '(& !)) x
                                `(when ,x (push (car ,x) result))))
                            into items finally return
                            `(let ((pos (point)) result)
                               (if (and ,@items) (list (nreverse result))
                                 (goto-char pos)
                                 nil)))))))
                  ;; Semantic action
                  (when (and ',func cell (not syntax))
                    (setcar
                     cell
                     (let ((result (car cell)))
                       (ignore result) ; Suppress unused var warning
                       ,(pcase func
                          ;; Dollar expression
                          ((or (pred evil-parser--dexp) (pred listp))
                           (dval func))
                          ((pred symbolp)
                           `(,(if (eq symbol 'alt) #'list #'cons) #',func result))
                          (_ (error "Invalid semantic action `%S'" func))))))
                  cell)))))
         ;; Substitute all dollar-sign symbols in X.
         ;; Each dollar-sign symbol is replaced with the corresponding
         ;; element in RESULT, so that $1 becomes the first element, etc.
         ;; The special value $0 is substituted with the whole list RESULT.
         (dval
          (x)
          (if (listp x) (cons #'list (mapcar #'dval x))
            (let ((num (evil-parser--dexp x)))
              (cond ((null num) `',x)
                    ((eq num 0) 'result)
                    (t `(nth (1- ,num) result)))))))
      `(lambda (symbol &optional syntax)
         (cl-labels
             (,@(cl-loop for (symbol . def) in (eval grammar t) collect
                         `(,symbol (syntax) ,(compile `(alt . ,def))))
              (evil-ex-parse-command
               ()
               (let* ((result (binding nil))
                      (command (car result)))
                 (cond
                  ((not result) nil)
                  ;; Check whether the parsed command is followed by a slash,
                  ;; dash or number and either the part before is NOT known to be
                  ;; a binding, or the complete string IS known to be a binding.
                  ((and (let ((ch (char-after)))
                          (and ch (or (memq ch '(?- ?/)) (<= ?0 ch ?9))))
                        (or (evil-ex-binding
                             (concat command (buffer-substring (point) (point-max))) t)
                            (not (evil-ex-binding command t))))
                   (backward-char (length command))
                   (emacs-binding nil))
                  ;; Parse a following "!" as bang only if the command
                  ;; has the property :ex-bang t.
                  ((when (eq (char-after) ?!)
                     (let ((binding (evil-ex-completed-binding command t)))
                       (and binding (evil-get-command-property binding :ex-bang))))
                   (forward-char)
                   (setcar result (concat command "!"))
                   result)
                  (t result)))))
           (pcase symbol
             ,@(cl-loop for sym in entrypoints collect `(',sym ,(compile sym)))
             (_ (error "Unknown entrypoint `%s'" symbol))))))))

(defvar evil-ex-argument-types nil
  "Association list of argument handlers.")

(defvar evil-ex-commands nil
  "Association list of command bindings and functions.")

(defvar evil-ex-reverse-range nil
  "Whether the current Ex range was entered reversed.")

(defvar evil--ex-expression nil
  "The Ex evaluation tree.")

(defvar evil--ex-cmd nil
  "The current Ex command string.")

(defvar evil--ex-argument-handler nil
  "The argument handler for the current Ex command.")

(define-error 'evil-ex-error "Ex syntax error")

(defun evil-ex-p ()
  "Return non-nil if currently in Ex state."
  (when evil-ex-original-buffer t))

(evil-define-command evil-ex (&optional initial-input)
  "Enter an Ex command.
The Ex command line is initialized with the value of INITIAL-INPUT. If
the command is called interactively the initial input depends on the
current state. In Normal state if a prefix count is given then the
initial input is \".,.+count\", otherwise it is empty. In Visual state
the initial input is the visual region '<,'> or `<,`>. The variable
`evil-ex-initial-input', if non-nil, is appended to the line."
  :keep-visual t
  :repeat abort
  (interactive
   (let ((s (concat
             (cond
              ((and (evil-visual-state-p)
                    evil-ex-visual-char-range
                    (memq (evil-visual-type) '(inclusive exclusive)))
               "`<,`>")
              ((evil-visual-state-p) "'<,'>")
              (current-prefix-arg
               (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (cond ((< arg 0) (setq arg (1+ arg)))
                       ((> arg 0) (setq arg (1- arg))))
                 (if (= arg 0) "." (format ".,.%+d" arg)))))
             evil-ex-initial-input)))
     (list (unless (string= s "") s))))
  (let ((buffer (current-buffer))
        (previous-command (when evil-want-empty-ex-last-command
                            (car evil-ex-history)))
        s evil--ex-expression evil--ex-cmd evil--ex-argument-handler)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local evil-ex-original-buffer buffer)
          (evil-ex-setup)
          (if initial-input (evil--ex-update)
            (when previous-command
              (add-hook 'pre-command-hook #'evil-ex-remove-default nil t))))
      (setq s (read-from-minibuffer
               ":"
               (or initial-input
                   (and previous-command (propertize previous-command 'face 'shadow)))
               evil-ex-completion-map nil 'evil-ex-history nil t)))
    (if evil--ex-expression
        (eval evil--ex-expression t)
      (when (string= s "") (setq s previous-command))
      (unless (= (length s) 0) (evil-ex-execute s)))))

(defun evil-ex-execute (string)
  "Execute STRING as an Ex command."
  (eval (or (evil-ex-parse string) (signal 'evil-ex-error string)) t))

(defun evil-ex-parse (string &optional syntax entrypoint)
  "Parse STRING as an Ex expression and return an evaluation tree.
If STRING is nil, parse the text after point instead.  If SYNTAX is
non-nil, return a syntax tree instead.  ENTRYPOINT is the start
symbol, which defaults to `expression'."
  (if string
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (evil-ex-parse nil syntax entrypoint))
    (let ((result (funcall (evil-parser evil-ex-grammar expression range)
                           (or entrypoint 'expression) syntax)))
      (and result
           ;; Disallow incomplete matches (ignore trailing WS)
           (not (search-forward "[^ \t\n\r]" nil t))
           (car result)))))

(defun evil-ex-delete-backward-char ()
  "Close the minibuffer if it is empty, otherwise call `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (< (minibuffer-prompt-end) (point-max))
       #'delete-backward-char
     #'abort-recursive-edit)))

(cl-defstruct (evil-ex-argument-handler (:type list) (:constructor nil)
                                        (:copier nil) (:predicate nil))
  (runner nil :read-only t) (completer nil :read-only t))

(defun evil-ex-setup ()
  "Initialize Ex minibuffer.
This function registers hooks that are used for the interactive
actions during Ex state."
  (add-hook 'after-change-functions #'evil--ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown nil t)
  (add-hook 'completion-at-point-functions #'evil-ex-completion-at-point nil t))

(defun evil-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `evil-ex-setup'."
  (let ((runner (evil-ex-argument-handler-runner evil--ex-argument-handler)))
    (when runner (funcall runner 'stop))))
(put 'evil-ex-teardown 'permanent-local-hook t)

(defsubst evil--ex-bang-p (command)
  "Return non-nil if the string COMMAND has a \"!\" suffix."
  (and (> (length command) 1) (eq (aref command (1- (length command))) ?!)))

(defun evil--ex-update (&optional beg _end _len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (when (and beg (eq this-command #'self-insert-command))
    (let ((cmd (lookup-key evil-ex-shortcut-map (minibuffer-contents-no-properties))))
      (when (commandp cmd)
        (setq evil--ex-expression `(call-interactively #',cmd))
        (exit-minibuffer))))

  (setq evil--ex-expression (save-excursion (goto-char (minibuffer-prompt-end))
                                            (evil-ex-parse string))
        evil--ex-cmd nil)
  (when (eq (car evil--ex-expression) #'evil-ex-call-command)
    (let (current-prefix-arg func handler evil-ex-range evil-ex-bang evil-ex-argument)
      (with-current-buffer evil-ex-original-buffer
        (let* ((range (eval (nth 1 evil--ex-expression) t))
               (count (when (integerp range) range)))
          (setq current-prefix-arg count
                evil-ex-range (if count (evil-ex-range count count) range)
                evil--ex-cmd (eval (nth 2 evil--ex-expression) t)
                evil-ex-bang (evil--ex-bang-p evil--ex-cmd)
                evil-ex-argument (eval (nth 3 evil--ex-expression) t))))
      (cond
       ((not beg))
       ;; Test the current command when called from `after-change-functions'
       ((setq func (evil-ex-completed-binding evil--ex-cmd t))
        ;; Update argument handler
        (let ((type (evil-get-command-property func :ex-arg)))
          (when type (setq handler (cdr (assq type evil-ex-argument-types)))))
        (if (eq handler evil--ex-argument-handler)
            (let ((runner (evil-ex-argument-handler-runner handler)))
              (when runner (funcall runner 'update evil-ex-argument)))
          (let ((runner (evil-ex-argument-handler-runner
                         evil--ex-argument-handler)))
            (when runner (funcall runner 'stop)))
          (setq evil--ex-argument-handler handler)
          (let ((runner (evil-ex-argument-handler-runner handler)))
            (when runner (funcall runner 'start evil-ex-argument)))))
       (t (let* ((evil-ex-complete-emacs-commands 'in-turn)
                 (prefix (try-completion evil--ex-cmd (evil-ex-completion-table))))
            (cond ((stringp prefix) (evil-ex-echo "Incomplete command"))
                  ((null prefix) (evil-ex-echo "Unknown command")))))))))

(defvar-local evil--ex-echo-overlay nil
  "Overlay for displaying info messages during Ex.")

(defun evil-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  ;; Differs from minibuffer-message, which see, in that it does not
  ;; use sit-for, since it may be called in the middle of a command.
  (unless (or evil-no-display (zerop (length string)))
    (let ((string (concat " [" (apply #'format string args) "]")))
      (put-text-property 0 1 'cursor t string) ; Place cursor before message
      (put-text-property 1 (length string) 'face 'evil-ex-info string)
      (with-selected-window (minibuffer-window)
        (if evil--ex-echo-overlay
            (move-overlay evil--ex-echo-overlay (point-max) (point-max))
          (setq evil--ex-echo-overlay (make-overlay (point-max) (point-max) nil t t)))
        (add-hook 'pre-command-hook #'evil--ex-remove-echo-overlay nil t)
        (overlay-put evil--ex-echo-overlay 'after-string string)))))

(defun evil--ex-remove-echo-overlay ()
  "Remove echo overlay from Ex minibuffer."
  (delete-overlay evil--ex-echo-overlay)
  (setq evil--ex-echo-overlay nil)
  (remove-hook 'pre-command-hook #'evil--ex-remove-echo-overlay t))

(cl-defun evil-ex-completion-at-point ()
  "Function used for `completion-at-point-functions' in Ex state."
  (cl-flet ((fix-beg (b) (min (save-excursion
                                (+ (goto-char b) (skip-chars-forward " \t\n\r")))
                              (point))))
    (pcase (nreverse (evil--ex-syntactic-context))
      ((or (and 'nil (let beg (minibuffer-prompt-end)))
           `((expression) (command . ,beg) . ,_)
           (and `((expression) (line) . ,_)
                (guard (looking-at-p "[ \t\n\r]*\\'"))
                (let beg (point))))
       (list (fix-beg beg) (point) (evil-ex-completion-table)))
      (`((expression) (argument . ,beg))
       (setq beg (fix-beg beg))
       ;; If it's an autoload, load the function; this allows external
       ;; packages to register autoloaded Ex commands which will be
       ;; loaded when ex argument completion is triggered.
       (let ((binding (evil-ex-binding evil--ex-cmd t))) (autoload-do-load binding))

       (let* ((binding (evil-ex-completed-binding evil--ex-cmd))
              (arg-type (evil-get-command-property binding :ex-arg))
              (arg-handler (cdr (assq arg-type evil-ex-argument-types))))
         (pcase (evil-ex-argument-handler-completer arg-handler)
           (`(collection . ,table) (list beg (point-max) table))
           (`(completion-at-point . ,completer)
            (save-restriction (narrow-to-region beg (point-max))
                              (funcall completer))))))
      (`((expression) (sexp . ,_))
       (when (fboundp 'elisp-completion-at-point) (elisp-completion-at-point))))))

(defun evil-ex-completion-table ()
  (let ((ex-cmds
         (cl-loop
          for (cmd . fun) in evil-ex-commands unless (stringp fun)
          collect cmd
          ;; Append ! to all commands that may take a bang argument
          when (evil-get-command-property fun :ex-bang)
          collect (concat cmd "!")))
        (emacs-cmds (lambda (str pred action)
                      (completion-table-with-predicate
                       obarray #'commandp t str pred action))))
    (cond
     ((null evil-ex-complete-emacs-commands) ex-cmds)
     ((eq evil-ex-complete-emacs-commands 'in-turn)
      (completion-table-in-turn ex-cmds emacs-cmds))
     (t (cl-loop for s in-ref ex-cmds do
                 (setf s (propertize s 'face 'evil-ex-commands)))
        (evil-completion-table-concat ex-cmds emacs-cmds)))))

(defun evil-completion-table-concat (table1 table2)
  (lambda (string pred action)
    (cond
     ((eq action nil)
      (let (matches)
        (dolist (table (list table1 table2) (try-completion string matches))
          (let ((x (try-completion string table pred)))
            (when x (push (if (eq x 't) string x) matches))))))
     ((eq action t)
      (delete-dups
       (append (all-completions string table1 pred)
               (all-completions string table2 pred))))
     ((eq action 'lambda)
      (when (or (test-completion string table1 pred)
                (test-completion string table2 pred))
        t))
     ((eq (car-safe action) 'boundaries)
      (or (completion-boundaries string table1 pred (cdr action))
          (completion-boundaries string table2 pred (cdr action))))
     ((eq action 'metadata)
      '(metadata (display-sort-function . evil-ex-sort-completions))))))

(defun evil-ex-sort-completions (completions)
  (sort completions
        (lambda (str1 str2)
          (let ((p1 (eq (get-text-property 0 'face str1) 'evil-ex-commands))
                (p2 (eq (get-text-property 0 'face str2) 'evil-ex-commands)))
            (if (eq p1 p2) (string< str1 str2) p1)))))

(define-obsolete-function-alias 'evil-ex-completion #'completion-at-point "1.15.0")

(define-obsolete-function-alias
  'evil-ex-command-completion-at-point #'evil-ex-completion-at-point "1.15.0")

(defalias 'evil-ex-argument-completion-at-point #'ignore)
(make-obsolete
 'evil-ex-argument-completion-at-point #'evil-ex-completion-at-point "1.15.0")

(define-obsolete-function-alias
  'evil-ex-elisp-completion-at-point #'elisp-completion-at-point "1.15.0")

(defun evil-ex-define-cmd (cmd function)
  "Bind the function FUNCTION to the command CMD."
  (if (string-match "\\[\\(.*\\)\\]" cmd)
      (let ((abbrev (replace-match "" nil t cmd))
            (full (replace-match "\\1" nil nil cmd)))
        (evil--add-to-alist evil-ex-commands
                            full function
                            abbrev full))
    (evil--add-to-alist evil-ex-commands cmd function)))

(defmacro evil-ex-define-argument-type (arg-type doc &rest body)
  "Define a new handler for argument-type ARG-TYPE.
DOC is the documentation string. It is followed by a list of keywords
and function:

:collection COLLECTION

  A collection for completion as required by `all-completions'.

:completion-at-point FUNC

  Function to be called to initialize a potential completion. FUNC
  must match the requirements as described for the variable
  `completion-at-point-functions'. When FUNC is called the minibuffer
  content is narrowed to exactly match the argument.

:runner FUNC

  Function to be called when the type of the current argument changes
  or when the content of this argument changes. This function should
  take one obligatory argument FLAG followed by an optional argument
  ARG. FLAG is one of three symbol `start', `stop' or `update'. When
  the argument type is recognized for the first time and this handler
  is started the FLAG is `start'. If the argument type changes to
  something else or ex state finished the handler FLAG is `stop'. If
  the content of the argument has changed FLAG is `update'. If FLAG is
  either `start' or `update' then ARG is the current value of this
  argument. If FLAG is `stop' then arg is nil."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (unless (stringp doc) (push doc body))
  (let (runner completer)
    (while (keywordp (car body))
      (let ((key (pop body))
            (func (pop body)))
        (cond
         ((eq key :runner)
          (setq runner func))
         ((eq key :collection)
          (setq completer (cons 'collection func)))
         ((eq key :completion-at-point)
          (setq completer (cons 'completion-at-point func))))))
    `(evil--add-to-alist evil-ex-argument-types
                         ',arg-type '(,runner ,completer))))

(evil-ex-define-argument-type file
  "Handle a file argument."
  :collection read-file-name-internal)

(evil-ex-define-argument-type buffer
  "Called to complete a buffer name argument."
  :collection internal-complete-buffer)

(declare-function comint-completion-at-point "comint")
(declare-function shell-completion-vars "shell" ())

(defvar-local evil--ex-shell-argument-initialized nil
  "This variable is set to t if shell command completion has been initialized.
See `evil-ex-init-shell-argument-completion'.")

(defun evil-ex-init-shell-argument-completion (flag &optional _arg)
  "Prepare the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not evil--ex-shell-argument-initialized))
    (setq evil--ex-shell-argument-initialized t)
    (require 'shell)
    ;; Set up Comint for Shell mode, except
    ;; `comint-completion-at-point' will be called manually.
    (let (completion-at-point-functions)
      (shell-completion-vars))))

(evil-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point comint-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-file-or-shell-command-completion-at-point ()
  (if (eq (char-after (point-min)) ?!)
      (save-restriction
        (narrow-to-region (1+ (point-min)) (point-max))
        (comint-completion-at-point))
    (list (point-min) (point-max) #'read-file-name-internal)))

(evil-ex-define-argument-type file-or-shell
  "File or shell argument type.
If the current argument starts with a ! the rest of the argument
is considered a shell command, otherwise a file-name. Completion
works accordingly."
  :completion-at-point evil-ex-file-or-shell-command-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-binding (command &optional noerror)
  "Return the final binding of COMMAND."
  (let ((binding (if (evil--ex-bang-p command) (substring command 0 -1)
                   command)))
    (while (stringp
            (setq binding (cdr (assoc binding evil-ex-commands)))))
    (unless binding (setq binding (intern-soft command)))
    (if (commandp binding)
        (or (command-remapping binding) binding)
      (unless noerror (user-error "Unknown command: `%s'" command)))))

(defun evil-ex-completed-binding (command &optional noerror)
  "Return the final binding of the completion of COMMAND."
  (let ((completion (try-completion command evil-ex-commands)))
    (evil-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;; TODO: extensions like :p :~ <cfile> ...
(defun evil-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((remote (file-remote-p file-name))
        (current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (setq file-name (or (file-remote-p file-name 'localname) file-name))
    (when current-fname
      (setq current-fname (or (file-remote-p current-fname 'localname)
                              current-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq alternate-fname (or (file-remote-p alternate-fname 'localname)
                                alternate-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t))
    (setq file-name (concat remote file-name)))
  file-name)

(defun evil-ex-file-arg ()
  "Return the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (zerop (length evil-ex-argument))
    (evil-ex-replace-special-filenames evil-ex-argument)))

(defun evil-ex-repeat (&optional count)
  "Repeat the last Ex command."
  (interactive "P")
  (when count
    (goto-char (point-min))
    (forward-line (1- count)))
  (cl-loop
   with evil-ex-original-buffer = (current-buffer)
   for last-cmd in evil-ex-history do
   (evil--ex-update nil nil nil last-cmd)
   (let ((expr (or evil--ex-expression (signal 'evil-ex-error last-cmd))))
     (unless (eq (evil-ex-binding evil--ex-cmd) #'evil-ex-repeat)
       (cl-return (eval expr t))))))

(defun evil-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (when evil-ex-reverse-range
    (setq evil-ex-reverse-range nil)
    (unless (y-or-n-p "Backward range given, OK to swap? ")
      (user-error "")))
  (let* ((count (when (integerp range) range))
         (current-prefix-arg count)
         (evil-ex-range (if count (evil-ex-range count count) range))
         (cmd (evil-ex-completed-binding command))
         (evil-ex-bang (evil--ex-bang-p command))
         (evil-ex-argument argument)
         (evil-this-type (evil-type evil-ex-range))
         (evil-ex-point (point))
         (restore-point
          (and evil-ex-range
               (evil-get-command-property cmd :restore-point)
               (if (evil-visual-state-p) evil-visual-beginning evil-ex-point)))
         (evil-called-from-ex-p t))
    (evil-exit-visual-state)
    (deactivate-mark)
    (when evil-ex-range
      ;; Set region if an explicit range has been specified
      (cl-destructuring-bind (beg end &rest) (evil-expand-range evil-ex-range t)
        (goto-char beg)
        (set-marker (mark-marker) end)))
    (setq this-command cmd)
    (let ((mark-active evil-ex-range))
      (run-hooks 'pre-command-hook)
      (call-interactively cmd))
    (run-hooks 'post-command-hook)
    (when restore-point (goto-char restore-point))))

(defun evil-ex-line (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun evil-ex-first-line ()
  "Return the line number of the first line."
  (line-number-at-pos (point-min)))

(defun evil-ex-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos))

(defun evil-ex-last-line ()
  "Return the line number of the last line."
  (save-excursion
    (goto-char (point-max))
    (when (bolp)
      (forward-line -1))
    (line-number-at-pos)))

(defun evil-ex-range (beg-line &optional end-line)
  "Return the first and last position of the current range."
  (let* ((beg (if beg-line (evil-line-position beg-line)
                (line-beginning-position)))
         (end (if end-line (evil-line-position (1+ end-line))
                (save-excursion (goto-char beg) (line-beginning-position 2)))))
    (when (< end beg)
      (setq evil-ex-reverse-range t)
      (evil-swap beg end))
    (evil-range beg end 'line :expanded t)))

(defun evil-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (evil-range (point-min) (point-max) 'line :expanded t))

(defun evil-ex-last-visual-range ()
  "Return a linewise range of the last visual selection."
  (evil-range evil-visual-mark evil-visual-point 'line))

(defun evil-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (setq marker (evil-get-marker
                (if (stringp marker) (aref marker 0) marker)))
  (if (numberp marker)
      (line-number-at-pos marker)
    (user-error "Ex does not support markers in other files")))

(defun evil-ex-char-marker-range (beg end)
  (setq beg (evil-get-marker (if (stringp beg) (aref beg 0) beg))
        end (evil-get-marker (if (stringp end) (aref end 0) end)))
  (unless (and (numberp beg) (numberp end))
    (user-error "Ex does not support markers in other files"))
  (evil-expand-range
   (evil-range beg end
               (if (evil-visual-state-p)
                   (evil-visual-type)
                 'inclusive))))

(declare-function evil-ex-make-search-pattern "evil-search")

(defun evil-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Return the line number of the match."
  (when evil-ex-search-vim-style-regexp
    (setq pattern (evil-transform-vim-style-regexp pattern)))
  (set-text-properties 0 (length pattern) nil pattern)
  (setq evil-ex-search-pattern (evil-ex-make-search-pattern pattern)
        evil-ex-search-direction 'forward)
  (condition-case err
      (save-excursion
        (evil-move-beginning-of-line 2)
        (when (or (re-search-forward pattern nil t)
                  (progn
                    (goto-char (point-min))
                    (re-search-forward pattern nil t)))
          (line-number-at-pos (match-beginning 0))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Return the line number of the match."
  (when evil-ex-search-vim-style-regexp
    (setq pattern (evil-transform-vim-style-regexp pattern)))
  (set-text-properties 0 (length pattern) nil pattern)
  (setq evil-ex-search-pattern (evil-ex-make-search-pattern pattern)
        evil-ex-search-direction 'backward)
  (condition-case err
      (save-excursion
        (evil-move-beginning-of-line)
        (when (or (re-search-backward pattern nil t)
                  (progn
                    (goto-char (point-max))
                    (re-search-backward pattern nil t)))
          (line-number-at-pos (match-beginning 0))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun evil-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

(defun evil-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (declare (obsolete evil-get-command-property "1.15.0"))
  (let ((binding (evil-ex-completed-binding command t)))
    (when binding
      (evil-get-command-property binding :ex-bang))))

(cl-defun evil--ex-syntactic-context
    (&optional (pos (point))
               (tree (save-excursion (goto-char (minibuffer-prompt-end))
                                     (evil-ex-parse nil t)))
               &aux i result)
  "Return the syntactical context in TREE of the character at POS.
POS defaults to the current position of point."
  ;; Iterate over syntax tree leaves (i.e. the strings), and return
  ;; the path to the leaf containing the cursor. Or, if not found,
  ;; e.g. because of trailing whitespace, the leaf at most one char
  ;; past the rightmost non-empty string.
  (cl-labels
      ((traverse
        (node path)
        (while (and (consp node) (symbolp (car node)))
          (push (cons (pop node) i) path))
        (if (listp node)
            (dolist (child node) (traverse child path))
          ;; NODE is the end position of a parsed string
          (when (>= node pos) (cl-return-from evil--ex-syntactic-context path))
          (when (or (null result) (> node i))
            (setq i node
                  result path)))))
    (traverse tree ()))
  result)

(provide 'evil-ex)

;;; evil-ex.el ends here
