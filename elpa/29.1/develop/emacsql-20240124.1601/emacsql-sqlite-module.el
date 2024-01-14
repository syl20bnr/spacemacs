;;; emacsql-sqlite-module.el --- EmacSQL back-end for SQLite using a module  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "25") (emacsql "20230220") (sqlite3 "0.16"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for SQLite, which uses
;; the Emacs module provided by the `sqlite3' package.

;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite-common)

(require 'sqlite3 nil t)
(declare-function sqlite3-open "sqlite3-api")
(declare-function sqlite3-exec "sqlite3-api")
(declare-function sqlite3-close "sqlite3-api")
(defvar sqlite-open-readwrite)
(defvar sqlite-open-create)

(emacsql-register-reserved emacsql-sqlite-reserved)

(defclass emacsql-sqlite-module-connection (emacsql--sqlite-base) ()
  "A connection to a SQLite database using a module.")

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-module-connection) &rest _)
  (require (quote sqlite3))
  (oset connection handle
        (sqlite3-open (or (oref connection file) ":memory:")
                      sqlite-open-readwrite
                      sqlite-open-create))
  (when emacsql-global-timeout
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2)))
  (emacsql connection [:pragma (= foreign-keys on)])
  (emacsql-register connection))

(cl-defun emacsql-sqlite-module (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-module-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-module-connection))
  (and (oref connection handle) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-module-connection))
  (sqlite3-close (oref connection handle))
  (oset connection handle nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-module-connection) message)
  (condition-case err
      (let (rows)
        (sqlite3-exec (oref connection handle)
                      message
                      (lambda (_ row __)
                        (push (mapcar (lambda (col)
                                        (cond ((null col) nil)
                                              ((equal col "") "")
                                              (t (read col))))
                                      row)
                              rows)))
        (nreverse rows))
    ((db-error sql-error)
     (pcase-let* ((`(,_ ,errmsg ,errcode) err)
                  (`(,_ ,_ ,signal ,errstr)
                   (assq errcode emacsql-sqlite-error-codes)))
       (signal (or signal 'emacsql-error)
               (list errmsg errcode nil errstr))))
    (error
     (signal 'emacsql-error (cdr err)))))

(cl-defmethod emacsql ((connection emacsql-sqlite-module-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-module)

;;; emacsql-sqlite-module.el ends here
