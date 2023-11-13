;;; emacsql-sqlite-builtin.el --- EmacSQL back-end for SQLite using builtin support  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "29") (emacsql "20230220"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for SQLite, which uses
;; the built-in SQLite support in Emacs 29 an later.

;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite-common)

(require 'sqlite nil t)
(declare-function sqlite-open "sqlite")
(declare-function sqlite-select "sqlite")
(declare-function sqlite-close "sqlite")

(emacsql-register-reserved emacsql-sqlite-reserved)

(defclass emacsql-sqlite-builtin-connection (emacsql--sqlite-base) ()
  "A connection to a SQLite database using builtin support.")

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-builtin-connection) &rest _)
  (require (quote sqlite))
  (oset connection handle
        (sqlite-open (slot-value connection 'file)))
  (when emacsql-global-timeout
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2)))
  (emacsql connection [:pragma (= foreign-keys on)])
  (emacsql-register connection))

(cl-defun emacsql-sqlite-builtin (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-builtin-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-builtin-connection))
  (and (oref connection handle) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-builtin-connection))
  (sqlite-close (oref connection handle))
  (oset connection handle nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-builtin-connection) message)
  (condition-case err
      (mapcar (lambda (row)
                (mapcar (lambda (col)
                          (cond ((null col) nil)
                                ((equal col "") "")
                                ((numberp col) col)
                                (t (read col))))
                        row))
              (sqlite-select (oref connection handle) message nil nil))
    ((sqlite-error sqlite-locked-error)
     (if (stringp (cdr err))
         (signal 'emacsql-error (list (cdr err)))
       (pcase-let* ((`(,_ ,errstr ,errmsg ,errcode ,ext-errcode) err)
                    (`(,_ ,_ ,signal ,_)
                     (assq errcode emacsql-sqlite-error-codes)))
         (signal (or signal 'emacsql-error)
                 (list errmsg errcode ext-errcode errstr)))))
    (error
     (signal 'emacsql-error (cdr err)))))

(cl-defmethod emacsql ((connection emacsql-sqlite-builtin-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-builtin)

;;; emacsql-sqlite-builtin.el ends here
