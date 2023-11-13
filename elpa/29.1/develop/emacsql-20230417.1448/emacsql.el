;;; emacsql.el --- High-level SQL database front-end  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; EmacSQL is a high-level Emacs Lisp front-end for SQLite
;; (primarily), PostgreSQL, MySQL, and potentially other SQL
;; databases. On MELPA, each of the backends is provided through
;; separate packages: emacsql-sqlite, emacsql-psql, emacsql-mysql.

;; Most EmacSQL functions operate on a database connection. For
;; example, a connection to SQLite is established with
;; `emacsql-sqlite'. For each such connection a sqlite3 inferior
;; process is kept alive in the background. Connections are closed
;; with `emacsql-close'.

;;     (defvar db (emacsql-sqlite "company.db"))

;; Use `emacsql' to send an s-expression SQL statements to a connected
;; database. Identifiers for tables and columns are symbols. SQL
;; keywords are lisp keywords. Anything else is data.

;;     (emacsql db [:create-table people ([name id salary])])

;; Column constraints can optionally be provided in the schema.

;;     (emacsql db [:create-table people ([name (id integer :unique) salary])])

;; Insert some values.

;;     (emacsql db [:insert :into people
;;                  :values (["Jeff"  1000 60000.0] ["Susan" 1001 64000.0])])

;; Currently all actions are synchronous and Emacs will block until
;; SQLite has indicated it is finished processing the last command.

;; Query the database for results:

;;     (emacsql db [:select [name id] :from employees :where (> salary 60000)])
;;     ;; => (("Susan" 1001))

;; Queries can be templates -- $i1, $s2, etc. -- so they don't need to
;; be built up dynamically:

;;     (emacsql db
;;              [:select [name id] :from employees :where (> salary $s1)]
;;              50000)
;;     ;; => (("Jeff" 1000) ("Susan" 1001))

;; The letter declares the type (identifier, scalar, vector, Schema)
;; and the number declares the argument position.

;; See README.md for much more complete documentation.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

(require 'emacsql-compiler)

(defgroup emacsql nil
  "The EmacSQL SQL database front-end."
  :group 'comm)

(defconst emacsql-version "3.1.1.50-git")

(defvar emacsql-global-timeout 30
  "Maximum number of seconds to wait before bailing out on a SQL command.
If nil, wait forever.")

(defvar emacsql-data-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where EmacSQL is installed.")

;;; Database connection

(defclass emacsql-connection ()
  ((handle :initarg :handle
           :documentation "Internal connection handler.
The value is a record-like object and should not be accessed
directly.  Depending on the concrete implementation, `type-of'
may return `process', `user-ptr' or `sqlite' for this value.")
   (log-buffer :type (or null buffer)
               :initarg :log-buffer
               :initform nil
               :accessor emacsql-log-buffer
               :documentation "Output log (debug).")
   (finalizer :documentation "Object returned from `make-finalizer'.")
   (types :allocation :class
          :initform nil
          :reader emacsql-types
          :documentation "Maps EmacSQL types to SQL types."))
  "A connection to a SQL database."
  :abstract t)

(cl-defgeneric emacsql-close (connection)
  "Close CONNECTION and free all resources.")

(cl-defgeneric emacsql-reconnect (connection)
  "Re-establish CONNECTION with the same parameters.")

(cl-defmethod emacsql-live-p ((connection emacsql-connection))
  "Return non-nil if CONNECTION is still alive and ready."
  (not (null (process-live-p (oref connection handle)))))

(cl-defgeneric emacsql-types (connection)
  "Return an alist mapping EmacSQL types to database types.
This will mask `emacsql-type-map' during expression compilation.
This alist should have four key symbols: integer, float, object,
nil (default type). The values are strings to be inserted into a
SQL expression.")

(cl-defmethod emacsql-buffer ((connection emacsql-connection))
  "Get process buffer for CONNECTION."
  (process-buffer (oref connection handle)))

(cl-defmethod emacsql-enable-debugging ((connection emacsql-connection))
  "Enable debugging on CONNECTION."
  (unless (buffer-live-p (emacsql-log-buffer connection))
    (setf (emacsql-log-buffer connection)
          (generate-new-buffer " *emacsql-log*"))))

(cl-defmethod emacsql-log ((connection emacsql-connection) message)
  "Log MESSAGE into CONNECTION's log.
MESSAGE should not have a newline on the end."
  (let ((buffer (emacsql-log-buffer connection)))
    (when buffer
      (unless (buffer-live-p buffer)
        (setq buffer (emacsql-enable-debugging connection)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (princ (concat message "\n") buffer)))))

(cl-defgeneric emacsql-process (this)
  "Access internal `handle' slot directly, which you shouldn't do.
Using this function to do it anyway, means additionally using a
misnamed and obsolete accessor function."
  (and (slot-boundp this 'handle)
       (eieio-oref this 'handle)))
(cl-defmethod (setf emacsql-process) (value (this emacsql-connection))
  (eieio-oset this 'handle value))
(make-obsolete 'emacsql-process "underlying slot is for internal use only."
               "Emacsql 4.0.0")

(cl-defmethod slot-missing ((connection emacsql-connection)
                            slot-name operation &optional new-value)
  "Treat removed `process' slot-name as an alias for internal `handle' slot."
  (pcase (list operation slot-name)
    ('(oref process)
     (message "EmacSQL: Slot `process' is obsolete")
     (oref connection handle))
    ('(oset process)
     (message "EmacSQL: Slot `process' is obsolete")
     (oset connection handle new-value))
    (_ (cl-call-next-method))))

;;; Sending and receiving

(cl-defgeneric emacsql-send-message (connection message)
  "Send MESSAGE to CONNECTION.")

(cl-defmethod emacsql-send-message :before
  ((connection emacsql-connection) message)
  (emacsql-log connection message))

(cl-defmethod emacsql-clear ((connection emacsql-connection))
  "Clear the connection buffer for CONNECTION-SPEC."
  (let ((buffer (emacsql-buffer connection)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (erase-buffer)))))

(cl-defgeneric emacsql-waiting-p (connection)
  "Return non-nil if CONNECTION is ready for more input.")

(cl-defmethod emacsql-wait ((connection emacsql-connection) &optional timeout)
  "Block until CONNECTION is waiting for further input."
  (let* ((real-timeout (or timeout emacsql-global-timeout))
         (end (when real-timeout (+ (float-time) real-timeout))))
    (while (and (or (null real-timeout) (< (float-time) end))
                (not (emacsql-waiting-p connection)))
      (save-match-data
        (accept-process-output (oref connection handle) real-timeout)))
    (unless (emacsql-waiting-p connection)
      (signal 'emacsql-timeout (list "Query timed out" real-timeout)))))

(cl-defgeneric emacsql-parse (connection)
  "Return the results of parsing the latest output or signal an error.")

(defun emacsql-compile (connection sql &rest args)
  "Compile s-expression SQL for CONNECTION into a string."
  (let* ((mask (when connection (emacsql-types connection)))
         (emacsql-type-map (or mask emacsql-type-map)))
    (concat (apply #'emacsql-format (emacsql-prepare sql) args) ";")))

(cl-defgeneric emacsql (connection sql &rest args)
  "Send SQL s-expression to CONNECTION and return the results.")

(cl-defmethod emacsql ((connection emacsql-connection) sql &rest args)
  (let ((sql-string (apply #'emacsql-compile connection sql args)))
    (emacsql-clear connection)
    (emacsql-send-message connection sql-string)
    (emacsql-wait connection)
    (emacsql-parse connection)))

;;; Helper mixin class

(defclass emacsql-protocol-mixin () ()
  "A mixin for back-ends following the EmacSQL protocol.
The back-end prompt must be a single \"]\" character. This prompt
value was chosen because it is unreadable. Output must have
exactly one row per line, fields separated by whitespace. NULL
must display as \"nil\"."
  :abstract t)

(cl-defmethod emacsql-waiting-p ((connection emacsql-protocol-mixin))
  "Return t if the end of the buffer has a properly-formatted prompt.
Also return t if the connection buffer has been killed."
  (let ((buffer (emacsql-buffer connection)))
    (or (not (buffer-live-p buffer))
        (with-current-buffer buffer
          (and (>= (buffer-size) 2)
               (string= "#\n"
                        (buffer-substring (- (point-max) 2) (point-max))))))))

(cl-defmethod emacsql-handle ((_ emacsql-protocol-mixin) code message)
  "Signal a specific condition for CODE from CONNECTION.
Subclasses should override this method in order to provide more
specific error conditions."
  (signal 'emacsql-error (list message code)))

(cl-defmethod emacsql-parse ((connection emacsql-protocol-mixin))
  "Parse well-formed output into an s-expression."
  (with-current-buffer (emacsql-buffer connection)
    (goto-char (point-min))
    (let* ((standard-input (current-buffer))
           (value (read)))
      (if (eql value 'error)
          (emacsql-handle connection (read) (read))
        (prog1 value
          (unless (eq 'success (read))
            (emacsql-handle connection (read) (read))))))))

(provide 'emacsql) ; end of generic function declarations

;;; Automatic connection cleanup

(defun emacsql-register (connection)
  "Register CONNECTION for automatic cleanup and return CONNECTION."
  (let ((finalizer (make-finalizer (lambda () (emacsql-close connection)))))
    (prog1 connection
      (setf (slot-value connection 'finalizer) finalizer))))

;;; Useful macros

(defmacro emacsql-with-connection (connection-spec &rest body)
  "Open an EmacSQL connection, evaluate BODY, and close the connection.
CONNECTION-SPEC establishes a single binding.

  (emacsql-with-connection (db (emacsql-sqlite \"company.db\"))
    (emacsql db [:create-table foo [x]])
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (emacsql db [:select * :from foo]))"
  (declare (indent 1))
  `(let ((,(car connection-spec) ,(cadr connection-spec)))
     (unwind-protect
         (progn ,@body)
       (emacsql-close ,(car connection-spec)))))

(defvar emacsql--transaction-level 0
  "Keeps track of nested transactions in `emacsql-with-transaction'.")

(defmacro emacsql-with-transaction (connection &rest body)
  "Evaluate BODY inside a single transaction, issuing a rollback on error.
This macro can be nested indefinitely, wrapping everything in a
single transaction at the lowest level.

Warning: BODY should *not* have any side effects besides making
changes to the database behind CONNECTION. Body may be evaluated
multiple times before the changes are committed."
  (declare (indent 1))
  `(let ((emacsql--connection ,connection)
         (emacsql--completed nil)
         (emacsql--transaction-level (1+ emacsql--transaction-level))
         (emacsql--result))
     (unwind-protect
         (while (not emacsql--completed)
           (condition-case nil
               (progn
                 (when (= 1 emacsql--transaction-level)
                   (emacsql emacsql--connection [:begin]))
                 (let ((result (progn ,@body)))
                   (setf emacsql--result result)
                   (when (= 1 emacsql--transaction-level)
                     (emacsql emacsql--connection [:commit]))
                   (setf emacsql--completed t)))
             (emacsql-locked (emacsql emacsql--connection [:rollback])
                             (sleep-for 0.05))))
       (when (and (= 1 emacsql--transaction-level)
                  (not emacsql--completed))
         (emacsql emacsql--connection [:rollback])))
     emacsql--result))

(defmacro emacsql-thread (connection &rest statements)
  "Thread CONNECTION through STATEMENTS.
A statement can be a list, containing a statement with its arguments."
  (declare (indent 1))
  `(let ((emacsql--conn ,connection))
     (emacsql-with-transaction emacsql--conn
       ,@(cl-loop for statement in statements
                  when (vectorp statement)
                  collect (list 'emacsql 'emacsql--conn statement)
                  else
                  collect (append (list 'emacsql 'emacsql--conn) statement)))))

(defmacro emacsql-with-bind (connection sql-and-args &rest body)
  "For each result row bind the column names for each returned row.
Returns the result of the last evaluated BODY.

All column names must be provided in the query ($ and * are not
allowed). Hint: all of the bound identifiers must be known at
compile time. For example, in the expression below the variables
`name' and `phone' will be bound for the body.

  (emacsql-with-bind db [:select [name phone] :from people]
    (message \"Found %s with %s\" name phone))

  (emacsql-with-bind db ([:select [name phone]
                          :from people
                          :where (= name $1)] my-name)
    (message \"Found %s with %s\" name phone))

Each column must be a plain symbol, no expressions allowed here."
  (declare (indent 2))
  (let ((sql (if (vectorp sql-and-args) sql-and-args (car sql-and-args)))
        (args (unless (vectorp sql-and-args) (cdr sql-and-args))))
    (cl-assert (eq :select (elt sql 0)))
    (let ((vars (elt sql 1)))
      (when (eq '* vars)
        (error "Must explicitly list columns in `emacsql-with-bind'."))
      (cl-assert (cl-every #'symbolp vars))
      `(let ((emacsql--results (emacsql ,connection ,sql ,@args))
             (emacsql--final nil))
         (dolist (emacsql--result emacsql--results emacsql--final)
           (setf emacsql--final
                 (cl-destructuring-bind ,(cl-coerce vars 'list) emacsql--result
                   ,@body)))))))

;;; User interaction functions

(defvar emacsql-show-buffer-name "*emacsql-show*"
  "Name of the buffer for displaying intermediate SQL.")

(defun emacsql--indent ()
  "Indent and wrap the SQL expression in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp " [A-Z]+" nil :no-error)
        (when (> (current-column) (* fill-column 0.8))
          (backward-word)
          (insert "\n    "))))))

(defun emacsql-show-sql (string)
  "Fontify and display the SQL expression in STRING."
  (let ((fontified
         (with-temp-buffer
           (insert string)
           (sql-mode)
           (with-no-warnings ;; autoloaded by previous line
             (sql-highlight-sqlite-keywords))
           (if (and (fboundp 'font-lock-flush)
                    (fboundp 'font-lock-ensure))
               (save-restriction
                 (widen)
                 (font-lock-flush)
                 (font-lock-ensure))
             (with-no-warnings
               (font-lock-fontify-buffer)))
           (emacsql--indent)
           (buffer-string))))
    (with-current-buffer (get-buffer-create emacsql-show-buffer-name)
      (if (< (length string) fill-column)
          (message "%s" fontified)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert fontified))
        (special-mode)
        (visual-line-mode)
        (pop-to-buffer (current-buffer))))))

(defun emacsql-flatten-sql (sql)
  "Convert a s-expression SQL into a flat string for display."
  (cl-destructuring-bind (string . vars) (emacsql-prepare sql)
    (concat
     (apply #'format string (cl-loop for i in (mapcar #'car vars)
                                     collect (intern (format "$%d" (1+ i)))))
     ";")))

;;;###autoload
(defun emacsql-show-last-sql (&optional prefix)
  "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer."
  (interactive "P")
  (let ((sexp (if (fboundp 'elisp--preceding-sexp)
                  (elisp--preceding-sexp)
                (with-no-warnings
                  (preceding-sexp)))))
    (if (emacsql-sql-p sexp)
        (let ((sql (emacsql-flatten-sql sexp)))
          (if prefix
              (insert sql)
            (emacsql-show-sql sql)))
      (user-error "Invalid SQL: %S" sexp))))

;;; Fix Emacs' broken vector indentation

(defun emacsql--inside-vector-p ()
  "Return non-nil if point is inside a vector expression."
  (let ((start (point)))
    (save-excursion
      (beginning-of-defun)
      (let ((containing-sexp (elt (parse-partial-sexp (point) start) 1)))
        (when containing-sexp
          (goto-char containing-sexp)
          (looking-at "\\["))))))

(defun emacsql--calculate-vector-indent (fn &optional parse-start)
  "Don't indent vectors in `emacs-lisp-mode' like lists."
  (if (save-excursion (beginning-of-line) (emacsql--inside-vector-p))
      (let ((lisp-indent-offset 1))
        (funcall fn parse-start))
    (funcall fn parse-start)))

(defun emacsql-fix-vector-indentation ()
  "When called, advise `calculate-lisp-indent' to stop indenting vectors.
Once activated, vector contents no longer indent like lists."
  (interactive)
  (advice-add 'calculate-lisp-indent :around
              #'emacsql--calculate-vector-indent))

;;; emacsql.el ends here
