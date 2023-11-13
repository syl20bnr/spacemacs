;;; gnuplot-context.el -- context-sensitive help and completion for gnuplot -*- lexical-binding: t -*-

;; Copyright (C) 2012-2013 Jon Oddie <jonxfield@gmail.com>

;; Author:     Jon Oddie <jonxfield@gmail.com>
;; URL:        https://github.com/emacs-gnuplot/gnuplot

;; This file is not part of GNU Emacs.

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
;; This file enhances gnuplot-mode with context-sensitive completion,
;; ElDoc support, and info page lookup for gnuplot script and shell
;; buffers.
;;
;; Usage
;; =====
;;
;; Make sure to byte-compile this file, or things will be noticeably
;; slow.
;;
;; Summary of key bindings:
;;     C-c C-d            read info page for construction at point
;;     C-u C-c C-d        prompt for info page to read
;;     C-c M-h, C-c C-/   pop up multi-line ElDoc string for construction
;;                          at point
;;
;; Gnuplot's context sensitive mode is best controlled using Customize
;; (M-x customize-group gnuplot): simply enable the
;; `gnuplot-context-sensitive-mode' setting. You may also want to turn
;; on `gnuplot-tab-completion' so that the TAB key does auto-completion
;; on lines which are already indented. (This just sets the Emacs
;; variable `tab-always-indent' to `complete' in Gnuplot buffers).
;;
;; If you need to turn context sensitivity on or off from Lisp code
;; for some reason, call the function
;; `gnuplot-context-sensitive-mode', which behaves like a minor mode.
;;
;; With `eldoc-mode' support, gnuplot-mode will show one-line syntax
;; hints automatically in the echo area.  Whether eldoc-mode is active
;; or not, you can always pop up a longer description of syntax using
;; `gnuplot-help-function' (C-c C-/ or C-c M-h).  ElDoc support also
;; requires an additional file of help strings, `gnuplot-eldoc.el',
;; which should be included in recent Gnuplot releases.  If it didn't
;; come with your Gnuplot installation, you'll need to grab a recent
;; source distribution of Gnuplot from http://gnuplot.info, and use
;; the `doc2texi.el' program in the docs/ directory to create it.  So
;; long as the file is on your Emacs load path somewhere it will be
;; loaded automatically when needed.
;;
;; You can customize gnuplot-mode to turn on eldoc mode automatically
;; using variable `gnuplot-eldoc-mode'.  Simply calling `eldoc-mode'
;; will also work.
;;
;; Internal details
;; ================
;;
;; Gnuplot's command language has a fair amount of syntactic
;; complexity, and the only way I could think of to support these
;; features was to do a complete parse of the command line.  So that's
;; what this package does.  Instead of building a parse tree, it
;; matches up until the token at point, and then either makes a list
;; of possible completions, or sets the variables `gnuplot-eldoc' and
;; `gnuplot-info-at-point' based on where it is in the grammar at that
;; point.
;;
;; The parsing/matching process happens in two phases: tokenizing
;; (`gnuplot-tokenize') and matching (`gnuplot-match-pattern').  In
;; order to be able to construct a full list of possible completions
;; via backtracking, the matching algorithm simulates a simple stack
;; machine with continuations.  At byte-compile time, the PEG-like
;; grammar in S-expression notation (`gnuplot-grammar') is compiled
;; down into a vector of "machine code" for the parsing machine (see
;; `gnuplot-compile-pattern', `gnuplot-compile-grammar' and
;; `gnuplot-compiled-grammar'). This is complicated, but it seems to
;; work well enough, and it saves on the Emacs call stack.
;;
;; Compiling the grammar does require increasing `max-lisp-eval-depth'
;; modestly.  This shouldn't cause any problems on modern machines, and
;; it only needs to be done once, at byte-compilation time.
;;
;; The parsing machine and compiler are partially based on the
;; description in Medeiros and Ierusalimschy 2008, "A Parsing Machine
;; for PEGs" (http://dl.acm.org/citation.cfm?doid=1408681.1408683).
;;
;; The pattern-matching language
;; =============================
;;
;; The gnuplot-mode grammar (see `gnuplot-compiled-grammar') is a list
;; of rules (RULE PATTERN), with each pattern written in S-expression
;; notation as follows:
;;
;;    any
;;      Match any token
;;
;;    name, number, string, separator
;;      Match a token of the given type.  "Separator" is semicolon, the
;;      statement separator.
;;
;;    Any other symbol
;;      Match another named rule in the grammar.  May be recursive.
;;
;;    "STRING"
;;      Match literally: a token with exactly the text "STRING".
;;
;;   (kw KEYWORD ALIASES ...)
;;  Match abbreviated Gnuplot keywords.  KEYWORD can be a string or
;;  a cons (PREFIX . SUFFIX).  In the latter case, this pattern
;;  will match PREFIX plus any number of characters from the
;;  beginning of SUFFIX.  Any literal string from ALIASES will
;;  also match.  The token-id of the matching token is mutated to
;;  the canonical value of KEYWORD.
;;  Example:
;;   (kw ("linew" ."idth") "lw") matches "linew", "linewi",
;;  ... "linewidth" as well as "lw".  Any of these tokens will
;;  appear as "linewidth" in subsequent processing.  (This is
;;  important for the "info-keyword" form, see below).
;;
;; The other pattern forms combine simpler patterns, much like regular
;; expressions or PEGs (parsing expression grammars):
;;
;;    (sequence { (:eldoc "eldoc string") }
;;              { (:info "info page") }
;;              { (:no-info) }
;;           PATTERN PATTERN... )
;;      Match all the PATTERNs in sequence or fail.  Sequences can also
;;      have optional ElDoc strings and info pages associated with
;;      them; the innermost ElDoc or info page around point is the one
;;      shown to the user.  Alternatively, either property may be a
;;      symbol, which should be a function to be called to get the
;;      real value.  Finally, if no ElDoc string is specified but the
;;      variable `gnuplot-eldoc-hash' contains a value for the name of
;;      the info page at point, that value is used as the ElDoc string
;;      instead.
;;
;;  For better readability, sequence forms can also be written as
;;  a vector, omitting the `sequence': [PATTERN PATTERN ...]
;;
;;    (either PATTERN PATTERN...)
;;      Match the first PATTERN to succeed, or fail if none
;;      matches.  Like regexp `|'.
;;
;;    (many PATTERN)
;;  Match PATTERN zero or more times, greedily; like regexp
;;  `*'.  Unlike a regular expression matcher, the parsing machine
;;  will not backtrack and try to match fewer times if a later
;;  part of the pattern fails.  This applies equally to the other
;;  non-deterministic forms "either" and "maybe".
;;
;;    (maybe PATTERN)
;;      Match PATTERN zero or one times, like regexp `?'.
;;
;;    (capture NAME PATTERN)
;;      Match PATTERN, capturing the tokens in a capture group named
;;      NAME.  Capture groups are stored in `gnuplot-captures'
;;      and can be retrieved using `gnuplot-capture-group'.  This is
;;      used to store the plotting style, which we need in order to
;;      give the correct ElDoc string for "using" clauses, and for
;;      info keywords (see below)
;;
;;    (info-keyword PATTERN)
;;      Match PATTERN, and use whatever the value of the first token
;;      it matches is to look up info pages for this pattern.  Most
;;      Gnuplot info pages have the same name as the keyword they
;;      document, so by using this we only have to put :info
;;      properties on the few that don't, such as "set".
;;
;;    For convenience, "many", "maybe", "capture" and "info-keyword"
;;    wrap the rest of their arguments in an implicit "sequence" form,
;;    so we can write (maybe "," expression) instead of
;;    (maybe (sequence "," expression))
;;
;;    (delimited-list PATTERN SEPARATOR)
;;      Match a list of PATTERNs separated by SEPARATOR.  Sugar for:
;;      (sequence PATTERN (many (sequence SEPARATOR PATTERN)))
;;
;;   (assert LISP-FORM)
;;  Evaluate LISP-FORM and fail if it returns NIL.  We need this in
;;  the patterns for "plot" and "splot" to check whether the
;;  command at point should be parsed in parametric mode or
;;  not.  See `gnuplot-guess-parametric-p'.
;;
;;
;; Bugs, TODOs, etc.
;; =======================
;;
;; It would be useful to complete on user-defined functions and
;; variables as well as built-ins.
;;
;; Completion probably will not work in continuation lines entered
;; into the gnuplot interaction buffer.
;;
;; It would be better to pop up longer syntax descriptions in a
;; temporary window, rather than making the echo area grow to fit
;; many lines.
;;
;; In ElDoc mode, we parse the whole line every time the user stops
;; typing.  This is wasteful; should cache things in text properties
;; instead.
;;
;; The pattern matching engine uses backtracking, which can take
;; exponential time.  So far it seems "fast enough" in actual use.
;;
;; The patterns don't really distinguish between "plot" and "splot"
;; for things like plot styles, binary arguments, etc.
;;
;; Some other the patterns are probably not quite right, especially for
;; things like abbreviated keywords, and features that I don't use
;; myself like "fit".  Hopefully anyone bothered by this will submit
;; patches ;-)
;;
;; It would be possible to provide more helpful ElDoc strings for
;; sub-parts of complicated options like "cntrparam".  This is a time
;; and maintenance issue rather than a technical one.

;;; Code:

(require 'cl-lib)
(require 'gnuplot)
(require 'eldoc)
(require 'info)
(require 'info-look)


;;;; The tokenizer.

(cl-defstruct gnuplot-token
  start     ; Buffer start position
  end       ; Buffer end position
  id        ; Text
  type)     ; a symbol: name, number, string, operator, separator

(defvar gnuplot-operator-regexp
  (eval-when-compile
    (regexp-opt
     '("(" ")" "(" ")" "{" "," "}" "[" ":" "]" "!" "**" "-" "+" "~" "!" "*" "/"
       "%" "+" "-" "." "<" "<=" ">" ">=" "==" "!=" "eq" "ne" "&" "^" "|" "&&" "||"
       "?" ":" "=" "$")))
  "Regexp to match Gnuplot operators for tokenizing.")

(eval-when-compile
  (defmacro gnuplot-tokenize-by-regexps (&rest rules)
    `(cond ,@(mapcar
              (lambda (rule)
                (let ((regexp (car rule))
                      (token-type (cadr rule)))
                  `((looking-at ,regexp)
                    (let ((str (match-string-no-properties 0)))
                      (forward-char (length str))
                      (make-gnuplot-token :id str
                                          :type ',token-type
                                          :start (match-beginning 0)
                                          :end (match-end 0))))))
              rules))))

(defun gnuplot-tokenize (&optional completing-p)
  "Tokenize the Gnuplot command at point.
Return a list of `gnuplot-token' objects.

If COMPLETING-P is non-nil, omits the token at point if it is a
name; otherwise continues tokenizing up to the token at point.  FIXME."
  (let ((tokens '())
        (stop-point (min (point)
                         (gnuplot-point-at-end-of-command))))
    (save-excursion
      (if (save-excursion               ; HACK FIXME
            (gnuplot-beginning-of-continuation)
            (looking-at "\\s-*if\\s-*("))
          (gnuplot-beginning-of-continuation)
        (gnuplot-beginning-of-command))
      (while
          ;; Skip whitespace and continuation lines
          (progn
            (skip-syntax-forward "-" stop-point)
            (while (looking-at "\\\\\n")
              (forward-line)
              (skip-syntax-forward "-" stop-point))
            ;; Don't tokenize anything starting after point
            (and (not (looking-at "#"))
                 (< (point) stop-point)))
        (let* ((from (point))
               (token
                (cond
                 ((gnuplot-tokenize-by-regexps
                   ("[[:alpha:]_][[:alpha:]0-9_]*" name)
                   ("[0-9]+\\(\\.[0-9]*\\)?\\([eE][+-]?[0-9]+\\)?\\|\\.[0-9]+\\([eE][+-]?[0-9]+\\)?" number)
                   (gnuplot-operator-regexp operator)
                   (";" separator)))

                 ((looking-at "['\"]")
                  (let* ((bounds (bounds-of-thing-at-point 'sexp))
                         (to (or (cdr bounds) stop-point)))
                    (goto-char to)
                    (make-gnuplot-token
                     :id (buffer-substring-no-properties from to)
                     :type 'string
                     :start from :end to)))

                 (t (error
                     "Gnuplot-tokenize: bad token beginning %s"
                     (buffer-substring-no-properties (point) stop-point))))))

          (push token tokens))))

    ;; If we are looking for completions, AND if the last token
    ;; read is a name, AND if point is within the bounds of the
    ;; last token, then discard it. The matching function
    ;; generates a list of all possible tokens that could appear
    ;; in that position for completion.
    (if (and completing-p
             tokens
             (eq (gnuplot-token-type (car tokens)) 'name)
             (<= (point) (gnuplot-token-end (car tokens))))
        (pop tokens))

    (nreverse tokens)))



;;;; The pattern and grammar compiler
;;
;; These functions compile the source S-expression grammar into a
;; vector of instructions for the parsing machine,
;; `gnuplot-match-pattern'. Its state consists of a program counter
;; (PC), a position in the list of tokens, a call stack, and a second
;; stack of backtracking entries (continuations). Its "machine
;; instructions" are the following:
;;
;;    (any)
;;      Match any token (fails only at end of command).
;;
;;    (literal LITERAL NO-COMPLETE)
;;      Match token with `gnuplot-token-id' LITERAL or fail. If we
;;      have reached the token before point, include LITERAL in the
;;      completion list unless NO-COMPLETE is non-`nil'.
;;
;;    (token-type TYPE)
;;      Match a token with `gnuplot-token-type' TYPE, or fail.
;;
;;    (keyword REGEXP NAME)
;;      Match any token whose `gnuplot-token-id' matches REGEXP. Use
;;      NAME for the completion list.
;;
;;    (jump OFFSET FIXED)
;;      Jump to (set PC to) OFFSET if FIXED is non-nil, otherwise to
;;      PC + OFFSET
;;
;;    (call OFFSET FIXED)
;;      Like "jump", but push a return address onto the stack for
;;      (return). (The compiler adds the name of the rule being called
;;      as a fourth element on the end of the list, but this is just a
;;      comment for debugging purposes).
;;
;;    (return)
;;      Return to the PC address on top of the stack, or finish
;;      matching if stack is empty. (Usually this doesn't happen,
;;      because the machine stops as soon as it gets to the token at
;;      point).
;;
;;    (choice OFFSET)
;;      Push a backtracking entry for location PC + OFFSET onto the
;;      backtracking stack. Backtracking entries save the contents of
;;      the call stack, position in the token list, the values of
;;      capture groups, and the record of loop progress (see below).
;;
;;    (check-progress)
;;      Break out of infinite loops, like (many (many ...)).  Checks
;;      an alist of conses (pc . tokens) for the position in the token
;;      stream the last time this instruction was reached, and breaks
;;      out of the loop if stuck in the same place; otherwise pushes a
;;      new entry onto the list.
;;
;;    (fail)
;;      Pop the most recent backtracking entry and continue from
;;      there, or fail the whole match if out of backtrack
;;      points. Failing to match returns the remainder of the token
;;      list, although we don't currently use this for anything.
;;
;;    (commit OFFSET)
;;      Discard one backtracking point and jump to PC + OFFSET. This
;;      is used to make the (either) form non-deterministic.
;;
;;    (push TYPE VALUE)
;;      Push an entry for an eldoc or info string (specified by TYPE)
;;      onto the stack.
;;
;;    (pop TYPE)
;;      Pop something off the stack; checks that it has the expected
;;      TYPE, for safety.
;;
;;    (save-start NAME)
;;      Open a capture group named NAME. Pushes an entry onto
;;      `gnuplot-captures' with current position in token list as the
;;      start of the group.
;;
;;    (save-end NAME)
;;      Close the capture group named NAME. Finds the topmost entry in
;;      `gnuplot-captures' with this name and sets its endpoint to the
;;      current position in token list. Error if no group with that
;;      name is found.
;;
;;    (label NAME)
;;      This should never be reached and will cause an error. The
;;      compiler inserts it at the beginning of compiled rules only
;;      for debugging purposes.
;;


(eval-and-compile
  ;; Compile a single pattern into a list of instructions. Leaves
  ;; calls to other rules as symbolic instructions (call SYMBOL) and
  ;; jumps, commits etc. as relative offsets; these are resolved into
  ;; absolute locations by `gnuplot-compile-grammar', below.
  (defun gnuplot-compile-pattern (pat)
    (cond
     ;; Strings match a single token literally
     ((stringp pat)
      ;; Don't add non-words to completion lists
      (let ((wordp (string-match-p "^\\sw\\(\\sw\\|\\s_\\)*$" pat)))
        `((literal ,pat ,(not wordp)))))

     ;; Symbols match token types or calls to other patterns
     ((symbolp pat)
      (cl-case pat
        ((any) `((any)))
        ((name number string separator) `((token-type ,pat)))
        (t `((call ,pat)))))

     ;; Syntactic sugar: write sequences (sequence ...) as vectors [...]
     ((vectorp pat)
      (gnuplot-compile-pattern
       (append '(sequence) pat '())))

     ;; Other forms combine simpler patterns
     (t
      (let ((type (car pat)))
        (cl-case type
          ;; (sequence...): concatenate patterns, with optional eldoc
          ;; and info strings
          ((sequence)
           (cl-destructuring-bind
               (subpats eldoc info)
               (gnuplot-filter-arg-list (cdr pat))
             (let ((eldoc-push '()) (eldoc-pop '())
                   (info-push '()) (info-pop '())
                   (compiled
                    (mapcar 'gnuplot-compile-pattern subpats)))
               (if eldoc
                   (setq eldoc-push `((push eldoc ,eldoc))
                         eldoc-pop `((pop eldoc))))
               (if info
                   (if (eq info :no-info)
                       (setq info-push '((push no-scan t))
                             info-pop '((pop no-scan)))
                     (setq info-push `((push info ,info))
                           info-pop `((pop info)))))
               (apply 'append
                      `(,info-push
                        ,eldoc-push
                        ,@compiled
                        ,eldoc-pop
                        ,info-pop)))))

          ;; (either...): choose between patterns
          ((either)
           (cond
            ((= (length pat) 2)         ; trivial case
             (gnuplot-compile-pattern (cadr pat)))

            ((> (length pat) 3)         ; could be more efficient...
             (gnuplot-compile-pattern (gnuplot-either-helper pat)))

            (t              ; two patterns
             (let* ((pat1 (cadr pat))
                    (pat2 (cl-caddr pat))
                    (pat1-c (gnuplot-compile-pattern pat1))
                    (pat2-c (gnuplot-compile-pattern pat2))
                    (pat1-l (length pat1-c))
                    (pat2-l (length pat2-c)))
               `((choice ,(+ pat1-l 2))
                 ,@pat1-c
                 (commit ,(+ pat2-l 1))
                 ,@pat2-c)))))

          ;; Repetition (*)
          ((many)
           (let* ((pat1 (cons 'sequence (cdr pat)))
                  (pat1-c (gnuplot-compile-pattern pat1))
                  (pat1-l (length pat1-c)))
             `((choice ,(+ pat1-l 3))
               (check-progress)         ; bail out of infinite loops
               ,@pat1-c
               (commit ,(- (+ pat1-l 2))))))

          ;; Repetition (+)
          ((many1)
           (let* ((pat1 (cdr pat)))
             (gnuplot-compile-pattern
              `(sequence ,@pat1 (many ,@pat1)))))


          ;; Optional (?)
          ((maybe)
           (let* ((pat1 (cons 'sequence (cdr pat)))
                  (pat1-c (gnuplot-compile-pattern pat1))
                  (pat1-l (length pat1-c)))
             `((choice ,(+ pat1-l 1))
               ,@pat1-c)))

          ;; Syntactic sugar for delimited lists
          ((delimited-list)
           (let* ((item (cadr pat))
                  (sep (cl-caddr pat)))
             (gnuplot-compile-pattern
              `(sequence ,item (many (sequence ,sep ,item))))))

          ;; keywords
          ((kw)
           (cl-destructuring-bind (regex name)
               (gnuplot-keyword-helper (cdr pat))
             `((keyword ,regex ,name))))

          ;; Capturing groups
          ((capture)
           (let* ((name (cadr pat))
                  (pat1 (cons 'sequence (cddr pat)))
                  (pat1-c (gnuplot-compile-pattern pat1)))
             `((save-start ,name)
               ,@pat1-c
               (save-end ,name))))

          ;; Use the first token as an info keyword
          ((info-keyword)
           (let* ((pat1 (cons 'sequence (cdr pat)))
                  (pat1-c (gnuplot-compile-pattern pat1)))
             `((push info first-token)
               ,@pat1-c
               (pop info))))

          ;; Assertions
          ((assert)
           (let* ((form (cadr pat)))
             `((assert ,form))))

          (t
           (error "Gnuplot-compile-pattern: bad pattern form %s" pat)))))))

  ;; Helper function for destructuring (sequence ...) forms in patterns
  ;; Takes the cdr of the sequence form, returns a list (PATTERNS ELDOC
  ;; INFO).
  (defun gnuplot-filter-arg-list (args)
    (let ((accum '())
          (eldoc nil) (info nil))
      (dolist (item args)
        (let ((type (car-safe item)))
          (cl-case type
            ((:eldoc) (setq eldoc (cadr item)))
            ((:no-info) (setq info :no-info)) ; inhibit stack scanning
            ((:info) (setq info (cadr item)))
            (t (push item accum)))))
      (list (reverse accum) eldoc info)))

  ;; Helper function for compiling (kw...) patterns
  ;; Takes the cdr of the kw form, returns a list (REGEXP KEYWORD)
  (defun gnuplot-keyword-helper (args)
    (let ((keyword (car args)) (aliases (cdr args)))
      (when (consp keyword)
        (let ((pre (car keyword)) (suf (cdr keyword)))
          (setq keyword (concat pre suf))
          (while (progn
                   (push pre aliases)
                   (not (zerop (length suf))))
            (setq pre (concat pre (substring suf 0 1))
                  suf (substring suf 1)))))
      (let ((regex
             (concat "^"
                     (regexp-opt (cons keyword aliases))
                     "$")))
        (list regex keyword))))

  ;; Helper function for compiling (either ...) patterns. Rewrites
  ;; alternates (either A B C) into (either A (either B (either C D)))
  (defun gnuplot-either-helper (pat)
    (if (= (length pat) 3)
        pat
      `(either ,(cadr pat)
               ,(gnuplot-either-helper
                 (cons 'either (cddr pat))))))

  ;; Compile the grammar (a list of rule-pattern pairs (RULE PATTERN))
  ;; into a single vector of matching-machine instructions. Compiles
  ;; each pattern individually, then "links" them into one vector,
  ;; converting symbolic (call ...) instructions into numeric offsets
  (defun gnuplot-compile-grammar (grammar start-symbol)
    (let ((compiled-pats '())         ; Alist of (name . instructions)
          ;; Reserve space for a jump to the start symbol
          (code-length 1))

      ;; Compile each rule and find the total number of instructions
      (dolist (item grammar)
        (let* ((name (car item))
               (pat (cadr item))
               (code (gnuplot-compile-pattern pat)))
          (push (cons name code) compiled-pats)
          ;; Reserve space for a label at the beginning and (return) at
          ;; the end
          (setq code-length (+ code-length 2 (length code)))))

      ;; Copy instructions into a single vector
      (let ((object-code (make-vector code-length nil))
            (name->offset (make-hash-table))
            (i 1))
        (setf (aref object-code 0) `(jump ,start-symbol))
        (dolist (chunk compiled-pats)
          (let ((name (car chunk))
                (code (cdr chunk)))
            (setf (aref object-code i) `(label ,name))
            (cl-incf i)
            (puthash name i name->offset)
            (while code
              (setf (aref object-code i) (car code)
                    code (cdr code)
                    i (1+ i)))
            (setf (aref object-code i) '(return)
                  i (1+ i))))

        ;; Resolve symbolic and relative jumps
        (let ((pattern-name nil))
          (dotimes (i (length object-code))
            (let ((inst (aref object-code i)))
              (cl-case (car inst)
                ((label)
                 (setq pattern-name (cadr inst)))

                ((jump call choice commit)
                 (cond
                  ((symbolp (cadr inst))
                   (let* ((name (cadr inst))
                          (location (gethash name name->offset)))
                     (if (not location)
                         (error
                          (concat "gnuplot-compile-grammar: "
                                  "No rule found for symbol `%s' in pattern `%s'")
                          name pattern-name))
                     (setcdr inst `(,location ,name))))

                  ((numberp (cadr inst))
                   (let* ((offset (cadr inst))
                          (location (+ offset i)))
                     (setcdr inst `(,location))))

                  (t
                   (error "Gnuplot-compile-grammar: bad instruction %s" inst))))))))
        object-code))))

;;; The grammar.
(defvar gnuplot-compiled-grammar
  (eval-when-compile
    (let ((max-lisp-eval-depth 600))
      (gnuplot-compile-grammar
       '((expression
          [infix-expression (maybe "?" expression ":" expression)])

         (prefix-operator
          (either "!" "~" "-" "+"))

         (infix-operator
          (either "**" "*" "/" "%" "+" "-" "." "<" "<=" ">" ">=" "==" "!=" "eq" "ne"
                  "&" "^" "|" "&&" "||"))

         (infix-expression
          [(many prefix-operator)
           primary-expression
           (many infix-operator expression)])

         (primary-expression
          [(either number string parenthesized-expression
                   column-ref complex-number function-call name)
           (many "!")
           (maybe "**" infix-expression)
           (maybe substring-range)])

         (function-call
          (either
           (info-keyword
            [(either "abs" "acos" "acosh" "arg" "asin" "asinh" "atan" "atan2" "atanh"
                     "besj0" "besj1" "besy0" "besy1" "ceil" "column" "columnhead"
                     "cos" "cosh" "defined" "erf" "erfc" "exists" "exp" "floor"
                     "gamma" "gprintf" "ibeta" "igamma" "imag" "int" "inverf"
                     "invnorm" "lambertw" "lgamma" "log" "log10" "norm" "real"
                     "sgn" "sin" "sinh" "sprintf" "sqrt" "strftime" "stringcolumn"
                     "strlen" "strptime" "strstrt" "substr" "tan" "tanh" "timecolumn"
                     "tm_hour" "tm_mday" "tm_min" "tm_mon" "tm_sec" "tm_wday"
                     "tm_yday" "tm_year" "valid" "value" "word" "words" "rand")
             parenthesized-expression])
           [(:info "elliptic_integrals")
            (either "EllipticK" "EllipticE" "EllipticPi")
            parenthesized-expression]
           [name
            parenthesized-expression]))

         (parenthesized-expression
          ["(" comma-list ")"])

         (complex-number
          ["{" (maybe "-") number "," (maybe "-") number "}"])

         (column-ref
          ["$" number])

         (substring-range-component
          (maybe (either "*" expression)))

         (substring-range
          ["[" (delimited-list substring-range-component ":" 2 2) "]"])

;;; Assignments
         (lhs
          [name (maybe "(" (delimited-list name "," 1) ")")])

         (assignment
          [lhs "=" (either assignment expression)])

;;; Lists of expressions
         (comma-list
          (delimited-list (either assignment expression) ","))

         (colon-list
          (delimited-list expression ":"))

         (tuple
          ["(" (delimited-list expression "," 2 3) ")"])

;;; Commands
         (command
          (info-keyword
           (either plot-command splot-command replot-command fit-command print-command
                   set-command cd-command call-command simple-command
                   eval-command load-command lower-raise-command pause-command
                   save-command system-command test-command undefine-command
                   update-command assignment if-command new-if-command do-command)))

         (command-list
          (delimited-list command separator))

         (block ["{" command-list (maybe separator) "}"])

;;; old-style one-line if(..) command
         (if-command
          (info-keyword
           "if" parenthesized-expression command-list
           (maybe separator "else" command-list)))

;;; new-style block-structured if
         (new-if-command
          (info-keyword
           "if" parenthesized-expression block
           (maybe "else" block)))

;;; block-structured "do"
         (do-command
          (info-keyword "do" iteration-spec block))

;;; PLOT, SPLOT commands
         (plot-command
          [(kw ("pl" . "ot"))

           (either
            ;; Parametric ranges
            [(assert (gnuplot-guess-parametric-p))
             (maybe t-axis-range) (maybe x-axis-range) (maybe y-axis-range)]

            ;; Non-parametric ranges
            [(maybe x-axis-range) (maybe y-axis-range)])

           plot-body])

         (splot-command
          [ ;; This capturing group lets `gnuplot-find-using-eldoc' know
           ;; that this is an splot command
           (capture :splot-command (kw ("spl" . "ot")))

           (either
            ;; Parametric ranges
            [(assert (gnuplot-guess-parametric-p))
             (maybe u-axis-range) (maybe v-axis-range)
             (maybe x-axis-range) (maybe y-axis-range) (maybe z-axis-range)]

            ;; Non-parametric ranges
            [(maybe x-axis-range) (maybe y-axis-range) (maybe z-axis-range)])

           plot-body])

         (replot-command [(kw "replot") plot-body])

         ;; Axis ranges
         (axis-range-component
          (maybe (either "*" expression)))

         (axis-range-body
          (delimited-list axis-range-component ":" 2 3))

         (axis-range
          [(:info "ranges")
           "[" (maybe (maybe name "=") axis-range-body) "]"])

         (x-axis-range [(:eldoc "X RANGE: [{<dummy>=}<min>:<max>]") axis-range])
         (y-axis-range [(:eldoc "Y RANGE: [{<dummy>=}<min>:<max>]") axis-range])
         (z-axis-range [(:eldoc "Z RANGE: [{<dummy>=}<min>:<max>]") axis-range])
         (t-axis-range [(:eldoc "T RANGE: [{<dummy>=}<min>:<max>]") axis-range])
         (u-axis-range [(:eldoc "U RANGE: [{<dummy>=}<min>:<max>]") axis-range])
         (v-axis-range [(:eldoc "V RANGE: [{<dummy>=}<min>:<max>]") axis-range])

         ;; Body of a plot/splot command. Should really be different for
         ;; parametric vs non-parametric, but that's too hard.
         (plot-body
          (delimited-list
           [(maybe iteration-spec) plot-expression plot-modifiers]
           ","))

         ;; Iteration: for [... ]
         (iteration-spec
          [(:info "iteration")
           (many1
            "for" "[" name
            (either ["=" (delimited-list expression ":")]
                    ["in" expression])
            "]")])

         ;; Expressions to plot can be preceded by any number of
         ;; assignments, with or without commas
         (plot-expression
          [(many [(:no-info) assignment (maybe ",")])
           expression])

;;; Plot/splot modifiers
         ;; These should probably be more different for plot and splot ...
         (plot-modifiers (many (either plot-modifier datafile-modifier)))

         (plot-modifier
          (info-keyword
           (either
            ;; simple one-word modifiers
            (kw "nohidden3d") (kw "nocontours") (kw "nosurface")

            ;; word followed by expression
            [(either
              (kw ("lines" . "tyle") "ls")
              (kw ("linet" . "ype") "lt")
              (kw ("linew" . "idth") "lw")
              (kw ("pointt" . "ype") "pt")
              (kw ("points" . "ize") "ps")
              (kw ("pointi" . "nterval") "pi"))
             expression]

            ;; others defined below
            title-modifier notitle-modifier axes-modifier with-modifier
            linecolor-modifier fillstyle-modifier)))

         (title-modifier
          [(kw ("t" . "itle")) expression])

         (notitle-modifier
          [(:info "title")
           (kw ("not" . "itle"))
           (maybe string)])

         (axes-modifier
          [(kw ("ax" . "es")) (either "x1y1" "x1y2" "x2y1" "x2y2")])

         (linecolor-modifier
          [(kw ("linec" . "olor") "lc") color-spec])

         (fillstyle-modifier
          [(kw "fillstyle" "fs")
           ;; fill-style also used by "set style fill"
           fill-style])

         (fill-style
          [(either
            "empty"
            [(maybe "transparent")
             (either "pattern" "solid")
             (maybe (either fill-style-border-clause expression))])
           (maybe fill-style-border-clause)])

         (fill-style-border-clause
          (either "noborder" [(kw ("bo" . "rder")) expression]))

         (color-spec
          [(:info "colorspec")
           (either
            (kw ("var" . "iable"))

            [(kw ("pal" . "ette"))
             (either "z"
                     [(either "frac" "cb") expression])]

            [(kw ("rgb" . "color"))
             (either (kw ("var" . "iable")) string)])])

         (with-modifier
          [(:info "plotting_styles")
           (kw ("w" . "ith"))

           ;; plotting-style also used for "set style data"
           (capture :with-style plotting-style)])

         (plotting-style
          (info-keyword
           (either
            ;; Simple styles that take no arguments
            (kw ("l" . "ines")) (kw ("i" . "mpulses")) (kw ("p" . "oints"))
            (kw ("linesp" . "oints") "lp") (kw ("d" . "ots")) (kw ("yerrorl" . "ines"))
            (kw ("errorl" . "ines")) (kw ("xerrorl" . "ines")) (kw ("xyerrorl" . "ines"))
            (kw ("ye" . "rrorbars")) (kw ("e" . "rrorbars")) (kw ("xe" . "rrorbars"))
            (kw ("xye" . "rrorbars")) (kw "boxes") (kw ("hist" . "ograms"))
            (kw ("boxer" . "rorbars")) (kw ("boxx" . "yerrorbars")) (kw ("st" . "eps"))
            (kw ("fs" . "teps")) (kw ("his" . "teps")) (kw ("fin" . "ancebars"))
            (kw ("can" . "dlesticks")) (kw ("pm" . "3d"))
            (kw ("cir" . "cles"))

            ;; Image styles all use the same info page
            [(:info "image")
             (either (kw ("ima" . "ge"))
                     (kw ("rgbima" . "ge"))
                     (kw ("rgba" . "lpha")))]

            ;; More complicated styles defined below
            labels-style-clause
            filledcurves-style-clause
            vectors-style-clause)))

         (labels-style-clause
          [(kw "labels")
           (maybe textcolor-spec)])

         (filledcurves-style-clause
          [(kw ("filledc" . "urves"))
           (maybe
            (either
             "closed"

             ["xy" "=" expression "," expression]

             [(maybe (either "above" "below"))
              (maybe [(either "x1" "x2" "y1" "y2")
                      (maybe "=" expression)])]))])

         (vectors-style-clause
          [(kw ("vec" . "tors"))
           (many
            (either
             "nohead" "head" "heads" "filled" "empty" "nofilled" "front" "back"
             [(kw "arrowstyle" "as") expression]
             ["size" (delimited-list expression ",")]
             linestyle-spec))])

;;; Various style specifiers, used in different places
         (linestyle-spec
          (many1
           (either
            [(kw ("lines" . "tyle") "ls") expression]
            [(kw ("linet" . "ype") "lt") expression]
            [(kw ("linew" . "idth") "lw") expression])))

         (textcolor-spec
          [(kw "textcolor" "tc")
           (either "default"
                   ["lt" expression]
                   color-spec)])

         (pointsize-spec [(kw "pointsize" "ps") expression])

;;; Datafile modifiers
         (datafile-modifier
          (info-keyword
           (either binary-modifier
                   [(maybe "nonuniform") (kw ("mat" . "rix"))]
                   index-modifier every-modifier
                   thru-modifier using-modifier
                   smooth-modifier
                   "volatile" "noautoscale")))

         (index-modifier
          [(kw ("i" . "ndex"))
           (either string (delimited-list expression ":" 0 2))])

         (every-modifier
          [(kw ("ev" . "ery")) (delimited-list (maybe expression) ":")])

         (thru-modifier
          [(kw "thru") expression])

         (using-modifier
          [(:eldoc gnuplot-find-using-eldoc)
           (kw ("u" . "sing"))
           (either
            string
            [colon-list (maybe string)])])

         (smooth-modifier
          [(kw ("s" . "mooth"))
           (either (kw ("a" . "csplines")) (kw ("b" . "ezier")) (kw ("c" . "splines"))
                   (kw ("s" . "bezier")) (kw ("u" . "nique")) (kw ("f" . "requency"))
                   (kw ("cum" . "ulative")) (kw ("k" . "density")))])

;;; Binary datafile modifiers
         (binary-modifier
          ["binary" (many binary-keyword)])

         (binary-keyword
          (either
           ;; All of these binary keywords are described on the same
           ;; info page
           [(:info "keywords")
            (either
             "transpose" "flipx" "flipy" "flipz"
             ["flip" "=" (either "x" "y" "z")]
             ["scan" "=" name]
             [(either "dx" "dy" "dz") "=" number]
             [(either "origin" "center" "perpendicular") "="
              (delimited-list tuple ":")]
             [(kw ("rot" . "ate") "rotation") "="
              (sequence expression (maybe (kw ("d" . "eg")) (kw ("p" . "i"))))])]

           ;; remaining binary keywords have their own info pages
           (info-keyword
            (either
             [(either "array" "record")
              "="
              (delimited-list expression ":")]

             [(either "skip")
              "="
              (delimited-list expression ":")]

             [(either "format" "endian" "filetype")
              "="
              expression]))))

;;; "fit" command
         (fit-command
          [(:info "fit")
           (kw "fit")
           (many axis-range)
           expression
           string
           (many plot-modifier)
           (kw "via")
           (either string (delimited-list name ","))])

;;; print command
         (print-command
          [(kw ("pr" . "int")) (delimited-list expression ",")])

;;; set commands
         (set-command
          [(:eldoc "set ...")
           (:info "set-show")
           (either (kw "set") (kw "unset") (kw "show"))
           (maybe iteration-spec)
           (info-keyword
            (either set-angles-clause set-arrow-clause
                    set-autoscale-clause set-bars-clause
                    set-border-clause set-boxwidth-clause
                    set-clabel-clause set-clip-clause
                    set-cntrparam-clause set-colorbox-clause
                    set-contour-clause set-datafile-clause
                    set-decimalsign-clause set-dgrid3d-clause
                    set-dummy-clause set-encoding-clause
                    set-fit-clause set-fontpath-clause
                    set-format-clause set-grid-clause
                    set-hidden3d-clause set-historysize-clause
                    set-isosamples-clause set-key-clause
                    set-label-clause set-loadpath-clause
                    set-locale-clause set-logscale-clause
                    set-mapping-clause set-margin-clause
                    set-multiplot-clause set-mxtics-clause
                    set-object-clause set-offsets-clause
                    set-origin-clause set-output-clause
                    set-parametric-clause set-pm3d-clause
                    set-palette-clause set-pointsize-clause
                    set-polar-clause set-print-clause
                    set-samples-clause set-size-clause
                    set-style-clause
                    set-surface-clause set-table-clause
                    set-terminal-clause set-termoption-clause
                    set-tics-clause set-tics-clause-2
                    set-xtics-clause
                    set-timestamp-clause set-timefmt-clause
                    set-title-clause set-view-clause
                    set-data-clause set-dtics-clause
                    set-xlabel-clause
                    set-mtics-clause set-range-clause
                    set-xyplane-clause set-zero-clause
                    set-zeroaxis-clause))])

;;; positions and coordinate systems for set options
         (position-system
          (either "first" "second" "graph" "screen" "character"))

         (dimension [(maybe position-system) expression])

         (position
          [dimension "," dimension (maybe "," dimension)])

         (to (either "to" "rto"))

;;; all the different "set ... " options
         (set-angles-clause
          ["angles" (either "degrees" "radians")])

         (set-arrow-clause
          ["arrow" (maybe number)
           (many
            (either ["from" position] [to position]
                    [(kw "arrowstyle" "as") expression]
                    "nohead" "head" "backhead" "heads"
                    ["size" dimension "," expression (maybe "," expression)]
                    "filled" "empty" "nofilled" "front" "back"
                    linecolor-modifier linestyle-spec))])

         (set-autoscale-clause
          ["autoscale"
           (either "fix"
                   "keepfix"
                   "x" "y" "z" "cb" "x2" "y2" "xy"
                   "xmin" "ymin" "zmin" "cbmin" "x2min" "y2min"
                   "xmax" "ymax" "zmax" "cbmax" "x2max" "y2max"
                   "xfix" "yfix" "zfix" "cbfix" "x2fix" "y2fix"
                   "xfixmax" "yfixmax" "zfixmax" "cbfixmax" "x2fixmax" "y2fixmax"
                   "xfixmin" "yfixmin" "zfixmin" "cbfixmin" "x2fixmin" "y2fixmin")])

         (set-bars-clause
          ["bars"
           (either expression "small" "large" "fullwidth")
           (either "front" "back")])

         (set-border-clause
          ["border"
           (maybe number)
           (maybe (either "front" "back"))
           (maybe (kw "linewidth" "lw") expression)
           (maybe
            (either (kw "linestyle" "ls") (kw "linetype" "lt"))
            expression)])

         (set-boxwidth-clause
          ["boxwidth"
           (maybe expression)
           (maybe (either (kw ("abs" . "olute")) "relative"))])

         (set-clabel-clause
          ["clabel" (maybe string)])

         (set-clip-clause
          ["clip" (maybe (either "points" "one" "two"))])

         (set-cntrparam-clause
          [(kw "cntrparam")
           (either
            "linear" "cubicspline" "bspline"

            [(either "points" "order") number]

            [(kw "levels")
             (either
              number
              (sequence (kw "auto") (maybe number))
              (sequence
               (kw "discrete") comma-list)
              (sequence
               (kw "incremental") (delimited-list expression "," 2 3)))])])

         (set-colorbox-clause
          [(:info "color_box")
           (kw ("colorb" . "ox"))
           (many
            (either
             (kw ("vert" . "ical")) (kw ("horiz" . "ontal"))
             "default" "user"
             ["origin" expression "," expression]
             ["size" expression "," expression]
             "front" "back"
             "noborder" "bdefault"
             ["border" expression]))])

         (set-contour-clause
          ["contour" (either "base" "surface" "both")])

         (set-datafile-clause
          ["datafile"
           (either [(:info "set_datafile_fortran")
                    "fortran"]
                   [(:info "set_datafile_nofpe_trap")
                    "nofpe_trap"]
                   [(:info "set_datafile_missing")
                    "missing" (maybe string)]
                   [(:info "set_datafile_separator")
                    "separator" (either "whitespace" string)]
                   [(:info "set_datafile_commentschars")
                    "commentschars" (maybe string)]
                   [(:info "set_datafile_binary")
                    "binary" (many binary-keyword)])])

         (set-decimalsign-clause
          ["decimalsign"
           (either string ["locale" (maybe string)])])

         (set-dgrid3d-clause
          ["dgrid3d"
           (maybe expression)       ; fixme
           (maybe "," expression)
           (either
            "splines"
            ["qnorm" expression]
            [(either "gauss" "cauchy" "exp" "box" "hann")
             (maybe expression)
             (maybe "," expression)])])

         (set-dummy-clause
          ["dummy"
           name (maybe "," name)])

         (set-encoding-clause
          ["encoding"
           (either "default" "iso_8859_1" "iso_8859_15" "iso_8859_2" "iso_8859_9"
                   "koi8r" "koi8u" "cp437" "cp850" "cp852" "cp1250" "cp1251" "cp1254"
                   "utf8" "locale")])

         (set-fit-clause
          [(:info "fit_")
           "fit"
           (either
            ["logfile" string]
            "errorvariables" "noerrorvariables")])

         (set-fontpath-clause
          ["fontpath" (many string)])

         (set-format-clause
          [(:info "format_")
           "format"
           (maybe (either "x" "y" "xy" "x2" "y2" "z" "cb"))
           string])

         (set-grid-clause
          ["grid"
           (either "nomxtics" "mxtics" "noxtics" "xtics" "nomytics" "mytics"
                   "noytics" "ytics" "nomztics" "mztics" "noztics" "ztics"
                   "nomx2tics" "mx2tics" "nox2tics" "x2tics" "nomy2tics"
                   "my2tics" "noy2tics" "y2tics" "nomcbtics" "mcbtics"
                   "nocbtics" "cbtics" "layerdefault" "front" "back"
                   [linestyle-spec (maybe "," linestyle-spec)])])

         (set-hidden3d-clause
          [(kw ("hidden" . "3d"))
           (many
            (either
             "defaults" "front" "back"
             ["offset" expression] "nooffset"
             ["trianglepattern"
              (either "0" "1" "2" "3" "4" "5" "6" "7")]
             ["undefined" (either "1" "2" "3")]
             ["noundefined"]
             "altdiagonal" "noaltdiagonal"
             "bentover" "nobentover"))])

         (set-historysize-clause
          ["historysize" number])

         (set-isosamples-clause
          [(kw ("isosam" . "ples")) number (maybe "," number)])

         (set-key-clause
          ["key"
           (many
            (either "on" "off" "default"
                    [(either "inside" "outside")
                     (either "lmargin" "rmargin" "tmargin" "bmargin")]
                    ["at" expression "," expression]
                    "left" "right" "center" "top" "bottom" "vertical"
                    "horizontal" "Left" "Right" "reverse" "noreverse" "invert"
                    "noinvert" "above" "over" "below" "under"
                    ["samplen" number]
                    ["spacing" number]
                    ["width" number]
                    [(either "autotitle" "noautotitle") (maybe "columnheader")]
                    ["title" expression] "enhanced" "noenhanced" ["font" string]
                    textcolor-spec
                    [(either "box" "nobox") linestyle-spec]
                    ["maxcols" (either expression "auto")]
                    ["maxrows" (either expression "auto")]))])

         (set-label-clause
          ["label"
           (maybe expression)
           (either label-clause-component expression)
           (many label-clause-component)])

         (label-clause-component
          (either
           ["at" position]
           "left" "center" "right"
           (either "norotate" ["rotate" "by" expression])
           ["font" string]
           "noenhanced"
           "front" "back"
           textcolor-spec
           "nopoint" ["point" (many (either pointsize-spec linestyle-spec))]
           ["offset" position]))

         (set-loadpath-clause
          ["loadpath" (many string)])

         (set-locale-clause
          ["locale" (maybe string)])

         (set-logscale-clause
          ["logscale"
           (either "x" "y" "xy" "x2" "y2" "z" "cb" name)])

         (set-mapping-clause
          ["mapping" (either "cartesian" "spherical" "cylindrical")])

         (set-margin-clause
          [(either "bmargin" "lmargin" "rmargin" "tmargin")
           (maybe "at" "screen") expression])

         ;; TODO: set-mouse-clause

         (set-multiplot-clause
          ["multiplot"
           (maybe
            ["layout" number "," number
             (maybe (either "rowsfirst" "columnsfirst"))
             (maybe (either "downwards" "upwards"))
             (maybe "title" string)
             (maybe "scale" number (maybe "," number))
             (maybe "offset" number (maybe "," number))])])

         (set-mxtics-clause
          [(:info "mxtics")
           (either "mxtics" "mytics" "mztics" "mx2tics" "my2tics" "mcbtics")
           (either "default" number)])

         ;; "set object", objects, dimensions, positions
         (set-object-clause
          ["object"
           (maybe number)
           (info-keyword
            (either rectangle-object ellipse-object circle-object polygon-object))
           (maybe (either "front" "back" "behind"))
           (maybe (kw "fillcolor" "fc") color-spec)
           (maybe "fs" expression)
           (maybe "default")
           (maybe (kw "linewidth" "lw") expression)])

         (rectangle-object
          [(kw ("rect" . "angle"))
           (maybe
            (either
             ["from" position (either "to" "rto") position]
             ["center" position "size" dimension "," dimension]
             ["at" position "size" dimension "," dimension]))])

         (ellipse-object
          ["ellipse"
           (either "at" "center") position
           "size" dimension "," dimension
           (maybe "angle" number)])

         (circle-object
          ["circle"
           (either "at" "center") position
           "size" dimension
           (maybe "arc" "[" number ":" number "]")])

         (polygon-object
          ["polygon"
           "from" position (many (either "to" "rto") position)])

         ;; "set offsets"
         (set-offsets-clause
          ["offsets"
           (delimited-list [(maybe "graph") expression] "," 4 4)])

         (set-origin-clause
          ["origin" expression "," expression])

         (set-output-clause
          ["output" (maybe string)])

         (set-parametric-clause
          [(:info "parametric_")
           (kw ("param" . "etric"))])

         (set-pm3d-clause
          ["pm3d"
           (many
            (either
             ["at" name]
             ["interpolate" number "," number]
             (either "scansautomatic" "scansforward" "scansbackward" "depthorder")
             ["flush" (either "begin" "center" "end")]
             (either "ftriangles" "noftriangles")
             (either "clip1in" "clip4in")
             ["corners2color"
              (either "mean" "geomean" "median" "min" "max" "c1" "c2" "c3" "c4")]
             ["hidden3d" number]
             "nohidden3d"
             "implicit" "explicit" "map"))])

         (set-palette-clause
          ["palette"
           (many
            (either
             "gray" "color"
             ["gamma" number]
             ["rgbformulae" number "," number "," number]
             "defined"          ; not complete
             ["functions" expression "," expression "," expression]
             ["file" string (many datafile-modifier)]
             "RGB" "HSV" "CMY" "YIQ" "XYZ"
             "positive" "negative"
             "nops_allcF" "ps_allcF"
             ["maxcolors" number]))])

         (set-pointsize-clause pointsize-spec)

         (set-polar-clause "polar")

         (set-print-clause
          [(:info "print_")
           "print"
           (maybe string)])

         (set-samples-clause
          ["samples" expression (maybe "," expression)])

         (set-size-clause
          ["size"
           (either
            "square" "nosquare"
            ["ratio" expression]
            "noratio"
            [expression "," expression])])

         (set-style-clause
          ["style"
           (either style-arrow-clause style-data-clause style-fill-clause
                   style-function-clause style-increment-clause
                   style-line-clause style-circle-clause style-rectangle-clause)])

         ;; begin subclauses of "set style ..."
         (style-arrow-clause
          [(:info "set_style_arrow")
           "arrow"
           number
           (either
            "default"
            (many
             (either "nohead" "head" "heads"
                     "filled" "empty" "nofilled"
                     "front" "back"
                     ["size" dimension "," number (maybe "," number)]
                     linestyle-spec)))])

         (style-data-clause
          [(:info "set_style_data")
           "data" plotting-style])

         (style-fill-clause
          [(:info "set_style_fill")
           "fill" fill-style])

         (style-function-clause
          [(:info "set_style_function")
           "function" plotting-style])

         (style-increment-clause
          [(:info "set_style_increment")
           "increment"
           (either (kw ("d" . "efault")) (kw ("u" . "serstyles")))])

         (style-line-clause
          [(:info "set_style_line")
           "line"
           expression
           (either
            "default"
            (many
             (either
              "palette"
              [(kw ("linet" . "ype") "lt")
               (either expression color-spec)]
              [(kw ("linec" . "olor") "lc") color-spec]
              [(either (kw ("linew" . "idth") "lw")
                       (kw ("pointt" . "ype") "pt")
                       (kw ("points" . "ize") "ps")
                       (kw ("pointi" . "nterval") "pi"))
               expression])))])

         (style-circle-clause
          [(:info "set_style_circle")
           "circle" "radius" dimension])

         (style-rectangle-clause
          [(:info "set_style_rectangle")
           "rectangle"
           (many
            (either
             "front" "back"
             [(kw ("linew" . "idth") "lw") expression]
             [(kw "fillcolor" "fc") color-spec]
             ["fs" expression]))])
         ;; end of "set style ..." clauses

         (set-surface-clause "surface")

         (set-table-clause ["table" (maybe string)])

         (set-terminal-clause       ; not sure how to do this...
          ["terminal" (maybe (either "push" "pop"))])

         (set-termoption-clause
          ["termoption"
           (either
            "enhanced" "noenhanced"
            ["font" string]
            "solid" "dashed"
            [(kw "linewidth" "lw") expression])])

         (set-tics-clause
          ["tics"
           (many
            (either
             "axis" "border" "mirror" "nomirror" "in" "out"
             ["scale" (either "default" [expression (maybe "," expression)])]
             [(either "rotate" "norotate") (maybe "by" expression)]
             ["offset" expression] "nooffset"
             ["format" string]
             ["font" string]
             textcolor-spec))])

         (set-tics-clause-2
          ["tics" (either "front" "back")])

         (set-xtics-clause
          [(:info "xtics")
           (either "xtics" "ytics" "ztics" "x2tics" "y2tics" "cbtics")
           (many
            (either
             "axis" "border" "mirror" "nomirror" "in" "out"
             ["scale" (either "default" [expression (maybe "," expression)])]
             [(either "rotate" "norotate") (maybe "by" expression)]
             ["offset" position] "nooffset"
             "add" "autofreq"
             ["(" (delimited-list [(maybe string) expression (maybe number)] ",") ")"]
             ["format" string]
             ["font" string]
             "rangelimited"
             textcolor-spec
             (delimited-list expression ",")))])

         (set-timestamp-clause
          ["timestamp"
           (maybe string)
           (maybe (either "top" "bottom"))
           (maybe (either "rotate" "norotate"))
           (maybe "offset" position)
           (maybe "font" string)])

         (set-timefmt-clause
          ["timefmt" string])

         (set-title-clause
          [(:info "title_")
           "title"
           (maybe expression)
           (many
            (either
             ["offset" position]
             ["font" string]
             textcolor-spec
             "enhanced" "noenhanced"))])

         (set-view-clause
          ["view"
           (either
            "map"
            [(either "equal" "noequal") (maybe (either "xy" "xyz"))]
            (delimited-list (maybe expression) ","))])

         (set-data-clause
          [(:info "xdata")
           (either "xdata" "ydata" "zdata" "x2data" "y2data" "cbdata")
           (maybe (either "time" "geographic"))])

         (set-dtics-clause
          [(:info "xdtics")
           (either "xdtics" "ydtics" "zdtics" "x2dtics" "y2dtics" "cbdtics")])

         (set-xlabel-clause
          [(:info "xlabel")
           (either (kw ("xlab" . "el")) (kw ("ylab" . "el"))
                   (kw ("zlab" . "el")) (kw ("x2lab" . "el"))
                   (kw ("y2lab" . "el")) (kw ("cblab" . "el")))
           (maybe expression)
           (many
            (either
             ["offset" position]
             ["font" string]
             textcolor-spec
             "enhanced" "noenhanced"))])

         (set-mtics-clause
          [(:info "xmtics")
           (either "xmtics" "ymtics" "zmtics" "x2mtics" "y2mtics" "cbmtics")])

         (set-range-clause
          [(:info "xrange")
           (either (kw ("xr" . "ange")) (kw ("yr" . "ange"))
                   (kw ("x2r" . "ange")) (kw ("y2r" . "ange"))
                   (kw ("zr" . "ange")) (kw ("tr" . "ange"))
                   (kw ("ur" . "ange")) (kw ("vr" . "ange"))
                   (kw ("rr" . "ange")) (kw ("cbr" . "ange")))
           (either
            "restore"
            ["[" (maybe
                  [(maybe axis-range-component) ":"
                   (maybe axis-range-component)])
             "]"
             (many (either "reverse" "noreverse" "writeback" "nowriteback"))])])

         (set-xyplane-clause
          ["xyplane" (either "at" "relative") expression])

         (set-zero-clause
          ["zero" expression])

         (set-zeroaxis-clause
          [(:info "zeroaxis")
           (either "zeroaxis" "xzeroaxis" "x2zeroaxis" "yzeroaxis" "y2zeroaxis"
                   "zzeroaxis")
           (maybe linestyle-spec)])


;;; Other commands
         (cd-command
          ["cd" string])

         (call-command
          ["call" string (many expression)])

         (simple-command
          (either "clear" "exit" "quit" "pwd" "refresh" "reread" "reset"
                  "shell"))

         (eval-command
          ["eval" expression])

         (load-command
          ["load" string])

         (lower-raise-command [(either "lower" "raise") number])

         (pause-command
          ["pause"
           (either
            expression
            ["mouse" (maybe endcondition (maybe "," endcondition))])
           string])

         (endcondition (either "keypress" "button1" "button2" "button3" "close" "any"))

         (save-command
          ["save"
           (either "functions" "variables" "terminal" "set")
           string])

         (system-command
          ["system" string])

         (test-command
          ["test"
           (either
            "terminal"
            ["palette"
             (maybe
              (either "rgb" "rbg" "grb" "gbr" "brg" "bgr"))])])

         (undefine-command
          ["undefine" (many name)])

         (update-command
          ["update" string (maybe string)]))

       ;; This is the start symbol
       'command))))


;; The following macros are used for debugging; load
;; gnuplot-debug-context.el and then re-load this file to enable
;; them. For normal use, they compile to no-ops.
(eval-when-compile
  (when (not (featurep 'gnuplot-debug-context))
    (defmacro with-gnuplot-trace-buffer (&rest _) "No-op." '(progn nil))
    (defmacro gnuplot-trace (&rest _) "No-op." '(progn nil))
    (defmacro gnuplot-debug (&rest _) "No-op." '(progn nil))))



;;;; Variables to be set via pattern matching
(defvar gnuplot-completions nil
  "List of possible `gnuplot-mode' completions at point.
This is filled in by `gnuplot-match-pattern' when it reaches the
token before point.")

(defvar gnuplot-info-at-point nil
  "Relevant page of the Gnuplot info manual for the construction at point.

Set by `gnuplot-match-pattern' using information from
`gnuplot-compiled-grammar'.  `gnuplot-match-pattern' pushes ElDoc
and info strings onto the stack as it runs, and scans the stack
for the topmost entry when it reaches the token at point.")

(defvar gnuplot-eldoc nil
  "ElDoc documentation string for the Gnuplot construction at point.

Set by `gnuplot-match-pattern'.  See also `gnuplot-info-at-point'.")

(defvar gnuplot-captures nil
  "Alist of named capture groups for `gnuplot-mode' completion code.

Each entry is of the form (NAME BEGIN END), where NAME is the
name specified in the (capture NAME PATTERN) form in the
`gnuplot-compiled-grammar' source, BEGIN is the tail of the token
list beginning the capture group, and END is the tail of the
token list just after the end of the capture group.")

(defvar gnuplot-eldoc-hash nil
  "ElDoc strings for `gnuplot-mode'.

These have to be compiled from the Gnuplot source tree using
`doc2texi.el'.")


;;;; The pattern matching machine
(defun gnuplot-match-pattern (instructions tokens completing-p
                                           &optional start-symbol)
  "Parse TOKENS, setting completions, info and ElDoc information.

This function parses TOKENS by simulating a stack machine with
unlimited backtracking.  If COMPLETING-P is non-nil, it stops
before the token at point and collects a list of the next tokens
that it would accept in `gnuplot-completions'.  If COMPLETING-P is
nil, it parses up to the token at point and sets `gnuplot-eldoc'
and `gnuplot-info-at-point' based on the contents of the stack
there."
  (catch 'return
    (let ((pc 0)            ; Program counter
          ;; Stack of return addresses (return PC), eldoc strings
          ;; (eldoc STRING) and info pages (info STRING)
          (stack '())
          ;; Stack of backtracking records:
          ;; ((STACK TOKENS RESUME-PC CAPTURES PROGRESS) ...)
          (backtrack '())
          ;; Match failure flag, set to `t' to cause backtracking
          (fail nil)
          ;; Flag set by JUMP and CALL instructions to stop PC advance
          (jump nil)
          ;; Record of progress made within (many ...) loops, an alist
          ;; of conses (pc . tokens)
          (progress '()))

      (with-gnuplot-trace-buffer (erase-buffer))

      (when start-symbol        ; HACK FIXME
        (let ((look-for `(label ,start-symbol)))
          (while (not (equal (aref instructions pc) look-for))
            (cl-incf pc))
          (cl-incf pc)))

      (setq gnuplot-completions nil
            gnuplot-eldoc nil
            gnuplot-info-at-point nil
            gnuplot-captures nil)

      (cl-flet ((advance
              ()
              (pop tokens)
              (if (and (null tokens) (not completing-p))
                  (gnuplot-scan-stack stack tokens)))
             (fail () (setq fail t)))

        ;; Main loop
        (while t
          (let* ((inst (aref instructions pc))
                 (opcode (car inst))
                 (token (car tokens))
                 (end-of-tokens (null tokens)))
            (gnuplot-trace "%s\t%s\t%s\n" pc inst (and token (gnuplot-token-id token)))

            (cl-case opcode
              ;; (literal LITERAL NO-COMPLETE)
              ((literal)
               (let ((expect (cadr inst))
                     (no-complete (cl-caddr inst)))
                 (cond (end-of-tokens
                        (unless no-complete
                          (gnuplot-trace "\tpushing \"%s\" to completions\n" expect)
                          (push expect gnuplot-completions))
                        (fail))

                       ((not (equal (gnuplot-token-id token) expect))
                        (fail))

                       ;; otherwise succeed
                       (t (advance)))))

              ;; (token-type TYPE)
              ((token-type)
               (let ((expect (cadr inst)))
                 (if (or end-of-tokens
                         (not (eq (gnuplot-token-type token) expect)))
                     (fail)
                   (advance))))

              ;; (keyword REGEXP NAME): match any token whose ID
              ;; regexp-matches REGEXP, use NAME for completions
              ((keyword)
               (let ((regexp (cadr inst))
                     (name (cl-caddr inst)))
                 (cond (end-of-tokens
                        (gnuplot-trace "\tpushing \"%s\" to completions\n" name)
                        (push name gnuplot-completions)
                        (fail))

                       ((not (string-match-p regexp (gnuplot-token-id token)))
                        (fail))

                       ;; otherwise succeed
                       (t
                        (setf (gnuplot-token-id token) name)
                        (advance)))))

              ;; (any): match any token
              ((any)
               (if end-of-tokens
                   (fail)
                 (advance)))

              ;; (jump LOCATION): jump to instruction at LOCATION
              ((jump)
               (let ((location (cadr inst)))
                 (setq jump location)))

              ;; (call LOCATION): push the next instruction as a
              ;; return location and jump
              ((call)
               (let ((location (cadr inst)))
                 (push `(return ,(+ pc 1)) stack)
                 (setq jump location)))

              ;; (return): return to address at topmost RETURN record on
              ;; stack, or stop matching and return if stack is empty
              ((return)
               (while (and stack
                           (not (eq (caar stack) 'return)))
                 (pop stack))
               (if (not stack)
                   ;; Successful match
                   (throw 'return (list tokens))
                 ;; Otherwise, return to caller
                 (let* ((r (pop stack))
                        (r-pc (cadr r)))
                   (setq jump r-pc))))

              ;; (choice LOCATION): push LOCATION onto the stack of
              ;; backtracking points and continue at next instruction
              ((choice)
               (let ((location (cadr inst)))
                 (push `(,stack ,tokens ,location ,gnuplot-captures
                                ,progress)
                       backtrack)))

              ;; (commit LOCATION): discard most recent backtrack point
              ;; and jump to LOCATION
              ((commit)
               (let ((location (cadr inst)))
                 (if (not backtrack)
                     (error "No more backtrack points in commit"))
                 (pop backtrack)
                 (setq jump location)))

              ;; (fail): force this match to fail, going back to most
              ;; recent backtrack point
              ((fail)
               (fail))

              ;; (assert): run Lisp code and fail if it returns NIL
              ((assert)
               (let ((form (cadr inst)))
                 (if (not (eval form)) (fail))))

              ;; (push TYPE VALUE): push an info page or eldoc string
              ;; onto the stack
              ((push)
               (let* ((type (cadr inst))
                      (value (cl-caddr inst)))
                 (push `(,type ,value ,tokens) stack)))

              ;; (pop TYPE): pop something off the stack
              ((pop)
               (let ((type (cadr inst)))
                 (if (not (and stack
                               (eq (caar stack) type)))
                     (error "Expected a %s on the stack but found %s" type stack))
                 (pop stack)))

              ;; (save-start NAME): save current token pointer as
              ;; beginning of capture group NAME
              ((save-start)
               (let ((name (cadr inst)))
                 (push `(,name ,tokens nil) gnuplot-captures)))

              ;; (save-end NAME): save current token pointer as end of
              ;; capture group NAME
              ((save-end)
               (let* ((name (cadr inst))
                      (record (assoc name gnuplot-captures)))
                 (if (not record)
                     (error "Gnuplot-match-tokens: no open capture group named %s" name)
                   (setf (cl-caddr record) tokens)
                   (gnuplot-debug (gnuplot-dump-captures)))))

              ;; (check-progress): make sure not stuck in an infinite loop
              ((check-progress)
               (let ((prev-progress (cdr (assoc pc progress))))
                 (if (and prev-progress (eq prev-progress tokens))
                     (fail)
                   (push (cons pc tokens) progress))))

              (t
               (error "Bad instruction: %s" inst)))

            ;; Increment PC or jump
            (setq pc (or jump (1+ pc))
                  jump nil)

            ;; Backtrack on failure
            (when fail
              (if (not backtrack)   ; Out of backtracking stack: failed match
                  (throw 'return nil)
                (gnuplot-trace "\t*fail*\t%s\n" (length backtrack))
                (gnuplot-debug (gnuplot-dump-backtrack backtrack))
                ;; If we got as far as token-at-point before failing,
                ;; scan the stack for eldoc and info strings
                (when (and end-of-tokens (not completing-p))
                  (gnuplot-scan-stack stack tokens))

                (cl-destructuring-bind
                    (bt-stack bt-tokens bt-pc bt-captures bt-progress)
                    (pop backtrack)
                  (setq stack bt-stack
                        tokens bt-tokens
                        pc bt-pc
                        gnuplot-captures bt-captures
                        progress bt-progress
                        fail nil)
                  (gnuplot-debug (gnuplot-dump-progress progress)))))))))))

(defun gnuplot-scan-stack (stack tokens)
  "Scan STACK for the most recently pushed eldoc and info strings."
  (gnuplot-trace "\t* scanning stack *\n")
  (gnuplot-debug (gnuplot-backtrace stack))
  (gnuplot-debug (gnuplot-dump-captures))

  (catch 'no-scan
    (while (and stack
                (not (and gnuplot-info-at-point gnuplot-eldoc)))
      (let* ((item (car stack))
             (type (car item))
             (position (cl-caddr item))) ; must progress by at least one token
        (if (and (memq type '(info eldoc no-scan))
                 (not (eq position tokens)))
            (cl-case type
              ((no-scan)
               (throw 'no-scan nil))

              ((info)
               (when (not gnuplot-info-at-point)
                 (let ((info (cadr item)))
                   (setq gnuplot-info-at-point
                         (cond
                          ((eq info 'first-token)
                           (gnuplot-token-id (car position)))
                          ((functionp info) (funcall info))
                          (t info)))
                   (when gnuplot-info-at-point
                     (gnuplot-trace "\tset info to \"%s\"\n" gnuplot-info-at-point)
                     (when (and (not gnuplot-eldoc) gnuplot-eldoc-hash)
                       (let ((eldoc
                              (car (gethash gnuplot-info-at-point gnuplot-eldoc-hash))))
                         (when eldoc
                           (setq gnuplot-eldoc eldoc)
                           (gnuplot-trace "\tand set eldoc to \"%s\"\n" eldoc))))))))

              ((eldoc)
               (when (not gnuplot-eldoc)
                 (let ((eldoc (cadr item)))
                   (setq gnuplot-eldoc
                         (if (functionp eldoc) (funcall eldoc) eldoc))
                   (gnuplot-trace "\tset eldoc to \"%s\"\n" gnuplot-eldoc)))))))
      (pop stack))))

(defun gnuplot-capture-group (name)
  "Return capture group NAME from the most recent parse, as a list of tokens."
  (let ((record (assoc name gnuplot-captures)))
    (if (not record) nil
      (let ((begin (cadr record))
            (end (cl-caddr record))
            (accum '()))
        (while (and begin (not (eq begin end)))
          (push (pop begin) accum))
        (nreverse accum)))))

(defun gnuplot-capture-group->string (name)
  (let ((tokens (gnuplot-capture-group name)))
    (and tokens
         (mapconcat 'gnuplot-token-id tokens " "))))


;;; Interface to the matching machine
(defun gnuplot-parse-at-point (completing-p)
  (let ((tokens (gnuplot-tokenize completing-p)))
    (gnuplot-match-pattern gnuplot-compiled-grammar tokens completing-p)))

;; Completions
(defun gnuplot-completions ()
  (gnuplot-parse-at-point t)
  gnuplot-completions)

(defun gnuplot-context-completion-at-point ()
  "Return completions of keyword preceding point, using context."
  (list (save-excursion
          (skip-syntax-backward "w_" (gnuplot-point-at-beginning-of-command))
          (point))
        (point)
        (gnuplot-completions)))

;; Eldoc help
(defun gnuplot-eldoc-function ()
  "Return the ElDoc string for the Gnuplot construction at point."
  (gnuplot-parse-at-point nil)
  gnuplot-eldoc)

(defun gnuplot-help-function ()
  "Pop up the extended documentation for the construction at point."
  (interactive)
  (gnuplot-parse-at-point nil)
  (if (and gnuplot-info-at-point gnuplot-eldoc-hash)
      (let ((eldoc
             (cadr (gethash gnuplot-info-at-point gnuplot-eldoc-hash))))
        (if eldoc (message eldoc)))))

;; Info lookup
(defun gnuplot-info-at-point (&optional query)
  "Open the relevant gnuplot info page for the construction at point."
  (interactive "P")
  (setq gnuplot-info-at-point nil)
  (unless query
    (gnuplot-parse-at-point nil))
  (if (or query (not gnuplot-info-at-point))
      (let ((info
             (info-lookup-interactive-arguments 'symbol)))
        (setq gnuplot-info-at-point (car info))))
  (when gnuplot-info-at-point
    (gnuplot--find-info-node gnuplot-info-at-point)))

(defun gnuplot--find-info-node (node)
  (save-window-excursion
    (info (format "(gnuplot)%s" node)))
  (gnuplot--adjust-info-display))


;;; Some context-sensitive hacks

;; ElDoc strings for "using" specs, which depend on other information
;; from the parsed command

(defvar gnuplot-using-eldoc
  '(("boxerrorbars" . "x:y:ydelta{:xdelta} | x:y:ylow:yhigh{:xdelta}")
    ("boxes" . "x:y{:x_width}")
    ("boxxyerrorbars" . "x:y:xdelta:ydelta | x:y:xlow:xhigh:ylow:yhigh")
    ("candlesticks" . "x:box_min:whisker_min:whisker_high:box_high | date:open:low:high:close")
    ("circles" . "x:y:radius")
    ("dots" . "x{:y{:z}}")
    ("filledcurves" . "x:y | x:y1:y2")
    ("financebars" . "date:open:low:high:close")
    ("fsteps" . "y | x:y")
    ("histeps" . "y | x:y")
    ("histograms" . "y:yerr | y:ymin:ymax")
    ("image" . "x:y:value")
    ("rgbimage" . "x:y:r:g:b")
    ("rgbalpha" . "x:y:r:g:b:a")
    ("impulses" . "x{:y{:z}}")
    ("labels" . "x:y:string")
    ("lines" . "y | x:y")
    ("steps" . "y | x:y")
    ("vectors" . "x:y:xdelta:ydelta")
    ("xerrorbars" . "x:y:xdelta | x:y:xlow:xhigh")
    ("xyerrorbars" . "x:y:xdelta:ydelta | x:y:xlow:xhigh:ylow:yhigh")
    ("yerrorbars" . "x:y:ydelta | x:y:ylow:yhigh")
    ("yerrorlines" . "x:y:ydelta | x:y:ylow:yhigh")
    ("xerrorlines" "x:y:xdelta | x:y:xlow:xhigh")
    ("xyerrorlines" . "x:y:xdelta:ydelta | x:y:xlow:xhigh:ylow:yhigh"))
  "Alist of ElDoc strings for Gnuplot \"using\" clauses in \"plot\" commands.")

(defvar gnuplot-using-3d-eldoc
  (append
   '(("fsteps" . "z | x:y:z")
     ("histeps" . "z | x:y:z")
     ("histograms" . "y:yerr | y:ymin:ymax")
     ("image" . "x:y:z:value")
     ("rgbimage" . "x:y:z:r:g:b")
     ("rgbalpha" . "x:y:z:r:g:b:a")
     ("labels" . "x:y:z:string")
     ("lines" . "z | x:y:z")
     ("steps" . "z | x:y:z")
     ("vectors" . "x:y:z:xdelta:ydelta:zdelta"))
   gnuplot-using-eldoc)
  "Alist of ElDoc strings for Gnuplot \"using\" clauses in \"splot\" commands.")

(defun gnuplot-find-using-eldoc ()
  "Return ElDoc string for a Gnuplot \"using\" clause, based on plotting style.

This will fail if the \"using\" clause comes before the \"with\"
clause."
  (let ((with-style (gnuplot-capture-group :with-style))
        (3d-p (gnuplot-capture-group :splot-command))
        (column-description nil))
    (if with-style
        (let ((with-style-string (gnuplot-token-id (car with-style))))
          (setq column-description
                (or (and 3d-p
                         (cdr (assoc with-style-string gnuplot-using-3d-eldoc)))
                    (cdr (assoc with-style-string gnuplot-using-eldoc))
                    "<columns>"))))
    (format "using %s {'format'}" column-description)))

;;; Needed for correctly parsing plot commands
(defun gnuplot-guess-parametric-p (&optional start)
  "Guess whether the command beginning at START is in parametric mode.

Searches backward in current buffer for an \"(un)set parametric\"
command."
  (save-excursion
    (and start (goto-char start))
    (catch 'result
      (while
          (search-backward-regexp "reset\\|set\\s-+parametric" (point-min) t)
        (gnuplot-beginning-of-command)
        (cond ((looking-at "reset\\|unset\\s-+parametric") (throw 'result nil))
              ((looking-at "set\\s-+parametric") (throw 'result t))))
      nil)))


;;;###autoload
(define-minor-mode gnuplot-context-sensitive-mode
  "Use context-sensitive completion and help in `gnuplot-mode'.

When context-sensitive mode is enabled, `gnuplot-mode' tries to
provide more useful completions and help suggestions for built-in
keywords and functions by parsing each command as you type.  It
attempts to take into account Gnuplot's many abbreviated
keywords.  For example, with point at the end of a line reading
\"plot 'datafile' w \", typing \\[completion-at-point] will pop
up a list of plotting styles.

Key bindings:

\\[completion-at-point] will complete the keyword at point based
on its context in the command. To make keyword completion work on
pressing TAB, set `tab-always-indent' to `complete', or customize
`gnuplot-tab-completion' to make this automatic in gnuplot-mode
buffers.

\\[gnuplot-info-at-point] will try to find the most relevant
Gnuplot info node for the construction at point, prompting for a
node name if nothing is found.

\\[gnuplot-help-function] will pop up a brief summary of the
syntax at point in the minibuffer. To have one-line syntax
summaries appear in the echo area as you type, toggle
`eldoc-mode' or customize `gnuplot-eldoc-mode'.

To choose whether to use this mode by default in Gnuplot buffers,
customize the variable
`gnuplot-use-context-sensitive-completion'.

Note: help strings for eldoc-mode and \\[gnuplot-help-function]
need to be provided in an Emacs-readable form by the Gnuplot
distribution. See gnuplot-context.el for details."
  :keymap
  `((,(kbd "C-c C-/") . gnuplot-help-function)
    (,(kbd "C-c C-d") . gnuplot-info-at-point))
  (unless (derived-mode-p 'gnuplot-mode 'gnuplot-comint-mode)
    (message "Gnuplot context-sensitive mode works only in Gnuplot-mode buffers")
    (setq gnuplot-context-sensitive-mode nil))
  (if gnuplot-context-sensitive-mode
      ;; Turn on
      (progn
        (setq gnuplot-completion-at-point-function #'gnuplot-context-completion-at-point)

        ;; Setup Eldoc
        (setq-local eldoc-documentation-function #'gnuplot-eldoc-function)
        (eldoc-add-command 'completion-at-point)     ; Check for eldoc after completion

        ;; Try to load Eldoc strings
        (when gnuplot-eldoc-mode
          (unless gnuplot-eldoc-hash
            (condition-case nil
                (load-library "gnuplot-eldoc")
              (error
               (message "gnuplot-eldoc.el not found. Install it from the Gnuplot distribution.")
               (setq gnuplot-eldoc-hash nil
                     gnuplot-eldoc-mode nil))))

          (if gnuplot-eldoc-hash
              (eldoc-mode 1)
            (eldoc-mode 0)))

        ;; Set up tab-to-complete
        (when gnuplot-tab-completion
          (setq-local tab-always-indent 'complete)))

    ;; Turn off
    (setq gnuplot-completion-at-point-function #'gnuplot-completion-at-point-info-look)
    (setq eldoc-documentation-function nil)
    (eldoc-mode 0)))


;;; All done!
(provide 'gnuplot-context)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gnuplot-context.el ends here
