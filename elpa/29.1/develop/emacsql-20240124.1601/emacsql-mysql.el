;;; emacsql-mysql.el --- EmacSQL back-end for MySQL  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "25.1") (emacsql "20230220"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for MySQL, which uses
;; the standard `msql' command line program.

;;; Code:

(require 'emacsql)

(defvar emacsql-mysql-executable "mysql"
  "Path to the mysql command line executable.")

(defvar emacsql-mysql-sentinel "--------------\n\n--------------\n\n"
  "What MySQL will print when it has completed its output.")

(defconst emacsql-mysql-reserved
  (emacsql-register-reserved
   '( ACCESSIBLE ADD ALL ALTER ANALYZE AND AS ASC ASENSITIVE BEFORE
      BETWEEN BIGINT BINARY BLOB BOTH BY CALL CASCADE CASE CHANGE CHAR
      CHARACTER CHECK COLLATE COLUMN CONDITION CONSTRAINT CONTINUE
      CONVERT CREATE CROSS CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP
      CURRENT_USER CURSOR DATABASE DATABASES DAY_HOUR DAY_MICROSECOND
      DAY_MINUTE DAY_SECOND DEC DECIMAL DECLARE DEFAULT DELAYED DELETE
      DESC DESCRIBE DETERMINISTIC DISTINCT DISTINCTROW DIV DOUBLE DROP
      DUAL EACH ELSE ELSEIF ENCLOSED ESCAPED EXISTS EXIT EXPLAIN FALSE
      FETCH FLOAT FLOAT4 FLOAT8 FOR FORCE FOREIGN FROM FULLTEXT GENERAL
      GRANT GROUP HAVING HIGH_PRIORITY HOUR_MICROSECOND HOUR_MINUTE
      HOUR_SECOND IF IGNORE IGNORE_SERVER_IDS IN INDEX INFILE INNER
      INOUT INSENSITIVE INSERT INT INT1 INT2 INT3 INT4 INT8 INTEGER
      INTERVAL INTO IS ITERATE JOIN KEY KEYS KILL LEADING LEAVE LEFT
      LIKE LIMIT LINEAR LINES LOAD LOCALTIME LOCALTIMESTAMP LOCK LONG
      LONGBLOB LONGTEXT LOOP LOW_PRIORITY MASTER_HEARTBEAT_PERIOD
      MASTER_SSL_VERIFY_SERVER_CERT MATCH MAXVALUE MAXVALUE MEDIUMBLOB
      MEDIUMINT MEDIUMTEXT MIDDLEINT MINUTE_MICROSECOND MINUTE_SECOND
      MOD MODIFIES NATURAL NOT NO_WRITE_TO_BINLOG NULL NUMERIC ON
      OPTIMIZE OPTION OPTIONALLY OR ORDER OUT OUTER OUTFILE PRECISION
      PRIMARY PROCEDURE PURGE RANGE READ READS READ_WRITE REAL
      REFERENCES REGEXP RELEASE RENAME REPEAT REPLACE REQUIRE RESIGNAL
      RESIGNAL RESTRICT RETURN REVOKE RIGHT RLIKE SCHEMA SCHEMAS
      SECOND_MICROSECOND SELECT SENSITIVE SEPARATOR SET SHOW SIGNAL
      SIGNAL SLOW SMALLINT SPATIAL SPECIFIC SQL SQL_BIG_RESULT
      SQL_CALC_FOUND_ROWS SQLEXCEPTION SQL_SMALL_RESULT SQLSTATE
      SQLWARNING SSL STARTING STRAIGHT_JOIN TABLE TERMINATED THEN
      TINYBLOB TINYINT TINYTEXT TO TRAILING TRIGGER TRUE UNDO UNION
      UNIQUE UNLOCK UNSIGNED UPDATE USAGE USE USING UTC_DATE UTC_TIME
      UTC_TIMESTAMP VALUES VARBINARY VARCHAR VARCHARACTER VARYING WHEN
      WHERE WHILE WITH WRITE XOR YEAR_MONTH ZEROFILL))
  "List of all of MySQL's reserved words.
http://dev.mysql.com/doc/refman/5.5/en/reserved-words.html")

(defclass emacsql-mysql-connection (emacsql-connection)
  ((dbname :reader emacsql-psql-dbname :initarg :dbname)
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "BIGINT")
                      (float "DOUBLE")
                      (object "LONGTEXT")
                      (nil "LONGTEXT"))))
  "A connection to a MySQL database.")

(cl-defun emacsql-mysql (database &key user password host port debug)
  "Connect to a MySQL server using the mysql command line program."
  (let* ((mysql (or (executable-find emacsql-mysql-executable)
                    (error "No mysql binary available, aborting")))
         (command (list database "--skip-pager" "-rfBNL" mysql)))
    (when user     (push (format "--user=%s" user) command))
    (when password (push (format "--password=%s" password) command))
    (when host     (push (format "--host=%s" host) command))
    (when port     (push (format "--port=%s" port) command))
    (let* ((process-connection-type t)
           (buffer (generate-new-buffer " *emacsql-mysql*"))
           (command (mapconcat #'shell-quote-argument (nreverse command) " "))
           (process (start-process-shell-command
                     "emacsql-mysql" buffer (concat "stty raw &&" command)))
           (connection (make-instance 'emacsql-mysql-connection
                                      :handle process
                                      :dbname database)))
      (set-process-sentinel process
                            (lambda (proc _) (kill-buffer (process-buffer proc))))
      (set-process-query-on-exit-flag (oref connection handle) nil)
      (when debug (emacsql-enable-debugging connection))
      (emacsql connection
               [:set-session (= sql-mode 'NO_BACKSLASH_ESCAPES\,ANSI_QUOTES)])
      (emacsql connection
               [:set-transaction-isolation-level :serializable])
      (emacsql-register connection))))

(cl-defmethod emacsql-close ((connection emacsql-mysql-connection))
  (let ((process (oref connection handle)))
    (when (process-live-p process)
      (process-send-eof process))))

(cl-defmethod emacsql-send-message ((connection emacsql-mysql-connection) message)
  (let ((process (oref connection handle)))
    (process-send-string process message)
    (process-send-string process "\\c\\p\n")))

(cl-defmethod emacsql-waiting-p ((connection emacsql-mysql-connection))
  (let ((length (length emacsql-mysql-sentinel)))
    (with-current-buffer (emacsql-buffer connection)
      (and (>= (buffer-size) length)
           (progn (goto-char (- (point-max) length))
                  (looking-at emacsql-mysql-sentinel))))))

(cl-defmethod emacsql-parse ((connection emacsql-mysql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (goto-char (point-min))
      (when (looking-at "ERROR")
        (search-forward ": ")
        (signal 'emacsql-error
                (list (buffer-substring (point) (line-end-position)))))
      (cl-loop until (looking-at emacsql-mysql-sentinel)
               collect (read) into row
               when (looking-at "\n")
               collect row into rows
               and do (setq row ())
               and do (forward-char)
               finally (cl-return rows)))))

(provide 'emacsql-mysql)

;;; emacsql-mysql.el ends here
