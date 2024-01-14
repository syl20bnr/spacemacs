;;; emacsql-compile.el --- S-expression SQL compiler  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; SPDX-License-Identifier: Unlicense

;;; Code:

(require 'cl-lib)

;;; Error symbols

(defmacro emacsql-deferror (symbol parents message)
  "Defines a new error symbol for EmacSQL."
  (declare (indent 2))
  (let ((conditions (cl-remove-duplicates
                     (append parents (list symbol 'emacsql-error 'error)))))
    `(prog1 ',symbol
       (put ',symbol 'error-conditions ',conditions)
       (put ',symbol 'error-message ,message))))

(emacsql-deferror emacsql-error () ;; parent condition for all others
  "EmacSQL had an unhandled condition")

(emacsql-deferror emacsql-syntax () "Invalid SQL statement")
(emacsql-deferror emacsql-internal () "Internal error")
(emacsql-deferror emacsql-locked () "Database locked")
(emacsql-deferror emacsql-fatal () "Fatal error")
(emacsql-deferror emacsql-memory () "Out of memory")
(emacsql-deferror emacsql-corruption () "Database corrupted")
(emacsql-deferror emacsql-access () "Database access error")
(emacsql-deferror emacsql-timeout () "Query timeout error")
(emacsql-deferror emacsql-warning () "Warning message")

(defun emacsql-error (format &rest args)
  "Like `error', but signal an emacsql-syntax condition."
  (signal 'emacsql-syntax (list (apply #'format format args))))

;;; Escaping functions

(defvar emacsql-reserved (make-hash-table :test 'equal)
  "Collection of all known reserved words, used for escaping.")

(defun emacsql-register-reserved (seq)
  "Register sequence of keywords as reserved words, returning SEQ."
  (cl-loop for word being the elements of seq
           do (setf (gethash (upcase (format "%s" word)) emacsql-reserved) t)
           finally (cl-return seq)))

(defun emacsql-reserved-p (name)
  "Returns non-nil if string NAME is a SQL keyword."
  (gethash (upcase name) emacsql-reserved))

(defun emacsql-quote-scalar (string)
  "Single-quote (scalar) STRING for use in a SQL expression."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "'" nil t)
      (replace-match "''"))
    (goto-char (point-min))
    (insert "'")
    (goto-char (point-max))
    (insert "'")
    (buffer-string)))

(defun emacsql-quote-character (c)
  "Single-quote character C for use in a SQL expression."
  (if (char-equal c ?')
      "''''"
    (format "'%c'" c)))

(defun emacsql-quote-identifier (string)
  "Double-quote (identifier) STRING for use in a SQL expression."
  (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" string)))

(defun emacsql-escape-identifier (identifier)
  "Escape an identifier, if needed, for SQL."
  (when (or (null identifier)
            (keywordp identifier)
            (not (or (symbolp identifier)
                     (vectorp identifier))))
    (emacsql-error "Invalid identifier: %S" identifier))
  (cond
   ((vectorp identifier)
    (mapconcat #'emacsql-escape-identifier identifier ", "))
   ((eq identifier '*) "*")
   (t
    (let ((name (symbol-name identifier)))
      (if (string-match-p ":" name)
          (mapconcat #'emacsql-escape-identifier
                     (mapcar #'intern (split-string name ":")) ".")
        (let ((print (replace-regexp-in-string "-" "_" (format "%S" identifier)))
              (special "[]-\000-\040!\"#%&'()*+,./:;<=>?@[\\^`{|}~\177]"))
          (if (or (string-match-p special print)
                  (string-match-p "^[0-9$]" print)
                  (emacsql-reserved-p print))
              (emacsql-quote-identifier print)
            print)))))))

(defvar print-escape-control-characters)

(defun emacsql-escape-scalar (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t))
    (cond ((null value) "NULL")
          ((numberp value) (prin1-to-string value))
          ((emacsql-quote-scalar (prin1-to-string value))))))

(defun emacsql-escape-raw (value)
  "Escape VALUE for sending to SQLite."
  (cond ((null value) "NULL")
        ((stringp value) (emacsql-quote-scalar value))
        ((error "Expected string or nil"))))

(defun emacsql-escape-vector (vector)
  "Encode VECTOR into a SQL vector scalar."
  (cl-typecase vector
    (null   (emacsql-error "Empty SQL vector expression."))
    (list   (mapconcat #'emacsql-escape-vector vector ", "))
    (vector (concat "(" (mapconcat #'emacsql-escape-scalar vector ", ") ")"))
    (otherwise (emacsql-error "Invalid vector %S" vector))))

(defun emacsql-escape-format (thing)
  "Escape THING for use as a `format' spec."
  (replace-regexp-in-string "%" "%%" thing))

;;; Schema compiler

(defvar emacsql-type-map
  '((integer "&INTEGER")
    (float "&REAL")
    (object "&TEXT")
    (nil "&NONE"))
  "An alist mapping EmacSQL types to SQL types.")

(defun emacsql--from-keyword (keyword)
  "Convert KEYWORD into SQL."
  (let ((name (substring (symbol-name keyword) 1)))
    (upcase (replace-regexp-in-string "-" " " name))))

(defun emacsql--prepare-constraints (constraints)
  "Compile CONSTRAINTS into a partial SQL expresson."
  (mapconcat
   #'identity
   (cl-loop for constraint in constraints collect
            (cl-typecase constraint
              (null "NULL")
              (keyword (emacsql--from-keyword constraint))
              (symbol (emacsql-escape-identifier constraint))
              (vector (format "(%s)"
                              (mapconcat
                               #'emacsql-escape-identifier
                               constraint
                               ", ")))
              (list (format "(%s)"
                            (car (emacsql--*expr constraint))))
              (otherwise
               (emacsql-escape-scalar constraint))))
   " "))

(defun emacsql--prepare-column (column)
  "Convert COLUMN into a partial SQL string."
  (mapconcat
   #'identity
   (cl-etypecase column
     (symbol (list (emacsql-escape-identifier column)
                   (cadr (assoc nil emacsql-type-map))))
     (list (cl-destructuring-bind (name . constraints) column
             (cl-delete-if
              (lambda (s) (zerop (length s)))
              (list (emacsql-escape-identifier name)
                    (if (member (car constraints) '(integer float object))
                        (cadr (assoc (pop constraints) emacsql-type-map))
                      (cadr (assoc nil emacsql-type-map)))
                    (emacsql--prepare-constraints constraints))))))
   " "))

(defun emacsql-prepare-schema (schema)
  "Compile SCHEMA into a SQL string."
  (if (vectorp schema)
      (emacsql-prepare-schema (list schema))
    (cl-destructuring-bind (columns . constraints) schema
      (mapconcat
       #'identity
       (nconc
        (mapcar #'emacsql--prepare-column columns)
        (mapcar #'emacsql--prepare-constraints constraints))
       ", "))))

;;; Statement compilation

(defvar emacsql-prepare-cache (make-hash-table :test 'equal :weakness 'key)
  "Cache used to memoize `emacsql-prepare'.")

(defvar emacsql--vars ()
  "Used within `emacsql-with-params' to collect parameters.")

(defun emacsql-sql-p (thing)
  "Return non-nil if THING looks like a prepared statement."
  (and (vectorp thing) (> (length thing) 0) (keywordp (aref thing 0))))

(defun emacsql-param (thing)
  "Return the index and type of THING, or nil if THING is not a parameter.
A parameter is a symbol that looks like $i1, $s2, $v3, etc. The
letter refers to the type: identifier (i), scalar (s),
vector (v), raw string (r), schema (S)."
  (and (symbolp thing)
       (let ((name (symbol-name thing)))
         (and (string-match-p "^\\$[isvrS][0-9]+$" name)
              (cons (1- (read (substring name 2)))
                    (cl-ecase (aref name 1)
                      (?i :identifier)
                      (?s :scalar)
                      (?v :vector)
                      (?r :raw)
                      (?S :schema)))))))

(defmacro emacsql-with-params (prefix &rest body)
  "Evaluate BODY, collecting parameters.
Provided local functions: `param', `identifier', `scalar', `raw',
`svector', `expr', `subsql', and `combine'. BODY should return a
string, which will be combined with variable definitions."
  (declare (indent 1))
  `(let ((emacsql--vars ()))
     (cl-flet* ((combine (prepared) (emacsql--*combine prepared))
                (param (thing) (emacsql--!param thing))
                (identifier (thing) (emacsql--!param thing :identifier))
                (scalar (thing) (emacsql--!param thing :scalar))
                (raw (thing) (emacsql--!param thing :raw))
                (svector (thing) (combine (emacsql--*vector thing)))
                (expr (thing) (combine (emacsql--*expr thing)))
                (subsql (thing)
                  (format "(%s)" (combine (emacsql-prepare thing)))))
       (cons (concat ,prefix (progn ,@body)) emacsql--vars))))

(defun emacsql--!param (thing &optional kind)
  "Parse, escape, and store THING.
If optional KIND is not specified, then try to guess it.
Only use within `emacsql-with-params'!"
  (cl-flet ((check (param)
              (when (and kind (not (eq kind (cdr param))))
                (emacsql-error
                 "Invalid parameter type %s, expecting %s" thing kind))))
    (let ((param (emacsql-param thing)))
      (if (null param)
          (emacsql-escape-format
           (if kind
               (cl-case kind
                 (:identifier (emacsql-escape-identifier thing))
                 (:scalar (emacsql-escape-scalar thing))
                 (:vector (emacsql-escape-vector thing))
                 (:raw (emacsql-escape-raw thing))
                 (:schema (emacsql-prepare-schema thing)))
             (if (and (not (null thing))
                      (not (keywordp thing))
                      (symbolp thing))
                 (emacsql-escape-identifier thing)
               (emacsql-escape-scalar thing))))
        (prog1 (if (eq (cdr param) :schema) "(%s)" "%s")
          (check param)
          (setq emacsql--vars (nconc emacsql--vars (list param))))))))

(defun emacsql--*vector (vector)
  "Prepare VECTOR."
  (emacsql-with-params ""
    (cl-typecase vector
      (symbol (emacsql--!param vector :vector))
      (list (mapconcat #'svector vector ", "))
      (vector (format "(%s)" (mapconcat #'scalar vector ", ")))
      (otherwise (emacsql-error "Invalid vector: %S" vector)))))

(defmacro emacsql--generate-op-lookup-defun (name operator-precedence-groups)
  "Generate function to look up predefined SQL operator metadata.

The generated function is bound to NAME and accepts two
arguments, OPERATOR-NAME and OPERATOR-ARGUMENT-COUNT.
OPERATOR-PRECEDENCE-GROUPS should be a number of lists containing
operators grouped by operator precedence (in order of precedence
from highest to lowest). A single operator is represented by a
list of at least two elements: operator name (symbol) and
operator arity (:unary or :binary). Optionally a custom
expression can be included, which defines how the operator is
expanded into an SQL expression (there are two defaults, one for
:unary and one for :binary operators).

An example for OPERATOR-PRECEDENCE-GROUPS:
(((+ :unary (\"+\" :operand)) (- :unary (\"-\" :operand)))
 ((+ :binary) (- :binary)))"
  `(defun ,name (operator-name operator-argument-count)
     "Look up predefined SQL operator metadata.
See `emacsql--generate-op-lookup-defun' for details."
     (cond
      ,@(cl-loop
         for precedence-value from 1
         for precedence-group in (reverse operator-precedence-groups)
         append (cl-loop
                 for (op-name arity custom-expr) in precedence-group
                 for sql-name = (upcase (symbol-name op-name))
                 for sql-expr =
                 (or custom-expr
                     (pcase arity
                       (:unary `(,sql-name " " :operand))
                       (:binary `(:operand " " ,sql-name " " :operand))))

                 collect (list `(and (eq operator-name
                                         (quote ,op-name))
                                     ,(if (eq arity :unary)
                                          `(eql operator-argument-count 1)
                                        `(>= operator-argument-count 2)))
                               `(list ',sql-expr ,arity ,precedence-value))))
      (t (list nil nil nil)))))

(emacsql--generate-op-lookup-defun
 emacsql--get-op
 (((~ :unary ("~" :operand)))
  ((collate :binary))
  ((|| :binary))
  ((* :binary) (/ :binary) (% :binary))
  ((+ :unary ("+" :operand)) (- :unary ("-" :operand)))
  ((+ :binary) (- :binary))
  ((& :binary) (| :binary) (<< :binary) (>> :binary))
  ((escape :binary (:operand " ESCAPE " :operand)))
  ((< :binary) (<= :binary) (> :binary) (>= :binary))

  (;;TODO? (between :binary) (not-between :binary)
   (is :binary) (is-not :binary (:operand " IS NOT " :operand))
   (match :binary) (not-match :binary (:operand " NOT MATCH " :operand))
   (like :binary) (not-like :binary (:operand  " NOT LIKE " :operand))
   (in :binary) (not-in :binary (:operand " NOT IN " :operand))
   (isnull :unary (:operand " ISNULL"))
   (notnull :unary (:operand " NOTNULL"))
   (= :binary) (== :binary)
   (!= :binary) (<> :binary)
   (glob :binary) (not-glob :binary (:operand " NOT GLOB " :operand))
   (regexp :binary) (not-regexp :binary (:operand " NOT REGEXP " :operand)))

  ((not :unary))
  ((and :binary))
  ((or :binary))))

(defun emacsql--expand-format-string (op expr arity argument-count)
  "Create format-string for an SQL operator.
The format-string returned is intended to be used with `format'
to create an SQL expression."
  (and expr
       (cl-labels ((replace-operand (x) (if (eq x :operand) "%s" x))
                   (to-format-string (e) (mapconcat #'replace-operand e "")))
         (cond
          ((and (eq arity :unary) (eql argument-count 1))
           (to-format-string expr))
          ((and (eq arity :binary) (>= argument-count 2))
           (let ((result (reverse expr)))
             (dotimes (_ (- argument-count 2))
               (setq result (nconc (reverse expr) (cdr result))))
             (to-format-string (nreverse result))))
          (t (emacsql-error "Wrong number of operands for %s" op))))))

(defun emacsql--get-op-info (op argument-count parent-precedence-value)
  "Lookup SQL operator information for generating an SQL expression.
Returns the following multiple values when an operator can be
identified: a format string (see `emacsql--expand-format-string')
and a precedence value. If PARENT-PRECEDENCE-VALUE is greater or
equal to the identified operator's precedence, then the format
string returned is wrapped with parentheses."
  (cl-destructuring-bind (format-string arity precedence-value)
      (emacsql--get-op op argument-count)
    (let ((expanded-format-string
           (emacsql--expand-format-string
            op
            format-string
            arity
            argument-count)))
      (cl-values (cond
                  ((null format-string) nil)
                  ((>= parent-precedence-value
                       precedence-value)
                   (format "(%s)" expanded-format-string))
                  (t expanded-format-string))
                 precedence-value))))

(defun emacsql--*expr (expr &optional parent-precedence-value)
  "Expand EXPR recursively."
  (emacsql-with-params ""
    (cond
     ((emacsql-sql-p expr) (subsql expr))
     ((vectorp expr) (svector expr))
     ((atom expr) (param expr))
     ((cl-destructuring-bind (op . args) expr
        (cl-multiple-value-bind (format-string precedence-value)
            (emacsql--get-op-info op
                                  (length args)
                                  (or parent-precedence-value 0))
          (cl-flet ((recur (n)
                      (combine (emacsql--*expr (nth n args)
                                               (or precedence-value 0))))
                    (nops (op)
                      (emacsql-error "Wrong number of operands for %s" op)))
            (cl-case op
              ;; Special cases <= >=
              ((<= >=)
               (cl-case (length args)
                 (2 (format format-string (recur 0) (recur 1)))
                 (3 (format (if (>= (or parent-precedence-value 0)
                                    precedence-value)
                                "(%s BETWEEN %s AND %s)"
                              "%s BETWEEN %s AND %s")
                            (recur 1)
                            (recur (if (eq op '>=) 2 0))
                            (recur (if (eq op '>=) 0 2))))
                 (otherwise (nops op))))
              ;; enforce second argument to be a character
              ((escape)
               (let ((second-arg (cadr args)))
                 (cond
                  ((not (= 2 (length args))) (nops op))
                  ((not (characterp second-arg))
                   (emacsql-error
                    "Second operand of escape has to be a character, got %s"
                    second-arg))
                  (t (format format-string
                             (recur 0)
                             (emacsql-quote-character second-arg))))))
              ;; Ordering
              ((asc desc)
               (format "%s %s" (recur 0) (upcase (symbol-name op))))
              ;; Special case quote
              ((quote) (let ((arg (nth 0 args)))
                         (if (stringp arg)
                             (raw arg)
                           (scalar arg))))
              ;; Special case funcall
              ((funcall)
               (format "%s(%s)" (recur 0)
                       (cond
                        ((and (= 2 (length args))
                              (eq '* (nth 1 args)))
                         "*")
                        ((and (= 3 (length args))
                              (eq :distinct (nth 1 args))
                              (format "DISTINCT %s" (recur 2))))
                        ((mapconcat
                          #'recur (cl-loop for i from 1 below (length args)
                                           collect i)
                          ", ")))))
              ;; Guess
              (otherwise
               (let ((arg-indices (cl-loop for i from 0 below (length args) collect i)))
                 (if format-string
                     (apply #'format format-string (mapcar #'recur arg-indices))
                   (mapconcat
                    #'recur (cl-loop for i from 0 below (length args) collect i)
                    (format " %s " (upcase (symbol-name op)))))))))))))))

(defun emacsql--*idents (idents)
  "Read in a vector of IDENTS identifiers, or just an single identifier."
  (emacsql-with-params ""
    (mapconcat #'expr idents ", ")))

(defun emacsql--*combine (prepared)
  "Append parameters from PREPARED to `emacsql--vars', return the string.
Only use within `emacsql-with-params'!"
  (cl-destructuring-bind (string . vars) prepared
    (setq emacsql--vars (nconc emacsql--vars vars))
    string))

(defun emacsql-prepare--string (string)
  "Create a prepared statement from STRING."
  (emacsql-with-params ""
    (replace-regexp-in-string
     "\\$[isv][0-9]+" (lambda (v) (param (intern v))) string)))

(defun emacsql-prepare--sexp (sexp)
  "Create a prepared statement from SEXP."
  (emacsql-with-params ""
    (cl-loop with items = (cl-coerce sexp 'list)
             and last = nil
             while (not (null items))
             for item = (pop items)
             collect
             (cl-typecase item
               (keyword (if (eq :values item)
                            (concat "VALUES " (svector (pop items)))
                          (emacsql--from-keyword item)))
               (symbol (if (eq item '*)
                           "*"
                         (param item)))
               (vector (if (emacsql-sql-p item)
                           (subsql item)
                         (let ((idents (combine
                                        (emacsql--*idents item))))
                           (if (keywordp last)
                               idents
                             (format "(%s)" idents)))))
               (list (if (vectorp (car item))
                         (emacsql-escape-format
                          (format "(%s)"
                                  (emacsql-prepare-schema item)))
                       (combine (emacsql--*expr item))))
               (otherwise
                (emacsql-escape-format
                 (emacsql-escape-scalar item))))
             into parts
             do (setq last item)
             finally (cl-return
                      (mapconcat #'identity parts " ")))))

(defun emacsql-prepare (sql)
  "Expand SQL (string or sexp) into a prepared statement."
  (let* ((cache emacsql-prepare-cache)
         (key (cons emacsql-type-map sql)))
    (or (gethash key cache)
        (setf (gethash key cache)
              (if (stringp sql)
                  (emacsql-prepare--string sql)
                (emacsql-prepare--sexp sql))))))

(defun emacsql-format (expansion &rest args)
  "Fill in the variables EXPANSION with ARGS."
  (cl-destructuring-bind (format . vars) expansion
    (let ((print-level nil)
          (print-length nil))
      (apply #'format format
             (cl-loop for (i . kind) in vars collect
                      (let ((thing (nth i args)))
                        (cl-case kind
                          (:identifier (emacsql-escape-identifier thing))
                          (:scalar (emacsql-escape-scalar thing))
                          (:vector (emacsql-escape-vector thing))
                          (:raw (emacsql-escape-raw thing))
                          (:schema (emacsql-prepare-schema thing))
                          (otherwise
                           (emacsql-error "Invalid var type %S" kind)))))))))

(provide 'emacsql-compiler)

;;; emacsql-compile.el ends here
