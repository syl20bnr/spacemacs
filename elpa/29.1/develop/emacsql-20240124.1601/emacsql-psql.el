;;; emacsql-psql.el --- EmacSQL back-end for PostgreSQL via psql  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql
;; Package-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "25.1") (emacsql "20230220"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for PostgreSQL, which
;; uses the standard `psql' command line program.

;; (For an alternative back-end for PostgreSQL, see `emacsql-pg'.)

;;; Code:

(require 'emacsql)

(defvar emacsql-psql-executable "psql"
  "Path to the psql (PostgreSQL client) executable.")

(defun emacsql-psql-unavailable-p ()
  "Return a reason if the psql executable is not available.
:no-executable -- cannot find the executable
:cannot-execute -- cannot run the executable
:old-version -- sqlite3 version is too old"
  (let ((psql emacsql-psql-executable))
    (if (null (executable-find psql))
        :no-executable
      (condition-case _
          (with-temp-buffer
            (call-process psql nil (current-buffer) nil "--version")
            (let ((version (cl-third (split-string (buffer-string)))))
              (if (version< version "1.0.0")
                  :old-version
                nil)))
        (error :cannot-execute)))))

(defconst emacsql-psql-reserved
  (emacsql-register-reserved
   '( ALL ANALYSE ANALYZE AND ANY AS ASC AUTHORIZATION BETWEEN BINARY
      BOTH CASE CAST CHECK COLLATE COLUMN CONSTRAINT CREATE CROSS
      CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP CURRENT_USER DEFAULT
      DEFERRABLE DESC DISTINCT DO ELSE END EXCEPT FALSE FOR FOREIGN
      FREEZE FROM FULL GRANT GROUP HAVING ILIKE IN INITIALLY INNER
      INTERSECT INTO IS ISNULL JOIN LEADING LEFT LIKE LIMIT LOCALTIME
      LOCALTIMESTAMP NATURAL NEW NOT NOTNULL NULL OFF OFFSET OLD ON
      ONLY OR ORDER OUTER OVERLAPS PLACING PRIMARY REFERENCES RIGHT
      SELECT SESSION_USER SIMILAR SOME TABLE THEN TO TRAILING TRUE
      UNION UNIQUE USER USING VERBOSE WHEN WHERE))
  "List of all of PostgreSQL's reserved words.
http://www.postgresql.org/docs/7.3/static/sql-keywords-appendix.html")

(defclass emacsql-psql-connection (emacsql-connection)
  ((dbname :reader emacsql-psql-dbname :initarg :dbname)
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "BIGINT")
                      (float "DOUBLE PRECISION")
                      (object "TEXT")
                      (nil "TEXT"))))
  "A connection to a PostgreSQL database via psql.")

(cl-defun emacsql-psql (dbname &key username hostname port debug)
  "Connect to a PostgreSQL server using the psql command line program."
  (let ((args (list dbname)))
    (when username
      (push username args))
    (push "-n" args)
    (when port
      (push "-p" args)
      (push port args))
    (when hostname
      (push "-h" args)
      (push hostname args))
    (setq args (nreverse args))
    (let* ((buffer (generate-new-buffer " *emacsql-psql*"))
           (psql emacsql-psql-executable)
           (command (mapconcat #'shell-quote-argument (cons psql args) " "))
           (process (start-process-shell-command
                     "emacsql-psql" buffer (concat "stty raw && " command)))
           (connection (make-instance 'emacsql-psql-connection
                                      :handle process
                                      :dbname dbname)))
      (setf (process-sentinel process)
            (lambda (proc _) (kill-buffer (process-buffer proc))))
      (set-process-query-on-exit-flag (oref connection handle) nil)
      (when debug (emacsql-enable-debugging connection))
      (mapc (apply-partially #'emacsql-send-message connection)
            '("\\pset pager off"
              "\\pset null nil"
              "\\a"
              "\\t"
              "\\f ' '"
              "SET client_min_messages TO ERROR;"
              "\\set PROMPT1 ]"
              "EMACSQL;")) ; error message flush
      (emacsql-wait connection)
      (emacsql connection
               [:set (= default-transaction-isolation 'SERIALIZABLE)])
      (emacsql-register connection))))

(cl-defmethod emacsql-close ((connection emacsql-psql-connection))
  (let ((process (oref connection handle)))
    (when (process-live-p process)
      (process-send-string process "\\q\n"))))

(cl-defmethod emacsql-send-message ((connection emacsql-psql-connection) message)
  (let ((process (oref connection handle)))
    (process-send-string process message)
    (process-send-string process "\n")))

(cl-defmethod emacsql-waiting-p ((connection emacsql-psql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (cond ((= (buffer-size) 1) (string= "]" (buffer-string)))
          ((> (buffer-size) 1) (string= "\n]" (buffer-substring
                                               (- (point-max) 2)
                                               (point-max)))))))

(cl-defmethod emacsql-check-error ((connection emacsql-psql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (looking-at "error:")
        (let* ((beg (line-beginning-position))
               (end (line-end-position)))
          (signal 'emacsql-error (list (buffer-substring beg end))))))))

(cl-defmethod emacsql-parse ((connection emacsql-psql-connection))
  (emacsql-check-error connection)
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (goto-char (point-min))
      (cl-loop until (looking-at "]")
               collect (read) into row
               when (looking-at "\n")
               collect row into rows
               and do (progn (forward-char 1) (setq row ()))
               finally (cl-return rows)))))

(provide 'emacsql-psql)

;;; emacsql-psql.el ends here
