;;; ht.el --- The missing hash table library for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 2.3
;; Keywords: hash table, hash map, hash
;; Package-Requires: ((dash "2.12.0"))

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

;; The missing hash table library for Emacs.
;;
;; See documentation at https://github.com/Wilfred/ht.el

;;; Code:

(require 'dash)
(require 'gv)
(eval-when-compile
  (require 'inline))

(defmacro ht (&rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)"
  (let* ((table-symbol (make-symbol "ht-temp"))
         (assignments
          (mapcar
           (lambda (pair) `(ht-set! ,table-symbol ,@pair))
           pairs)))
    `(let ((,table-symbol (ht-create)))
       ,@assignments
       ,table-symbol)))

(define-inline ht-set! (table key value)
  "Associate KEY in TABLE with VALUE."
  (inline-quote
   (prog1 nil
     (puthash ,key ,value ,table))))

(defalias 'ht-set 'ht-set!)

(define-inline ht-create (&optional test)
  "Create an empty hash table.

TEST indicates the function used to compare the hash
keys.  Default is `equal'.  It can be `eq', `eql', `equal' or a
user-supplied test created via `define-hash-table-test'."
  (declare (side-effect-free t))
  (inline-quote (make-hash-table :test (or ,test 'equal))))

(defun ht<-alist (alist &optional test)
  "Create a hash table with initial values according to ALIST.

TEST indicates the function used to compare the hash
keys.  Default is `equal'.  It can be `eq', `eql', `equal' or a
user-supplied test created via `define-hash-table-test'."
  (declare (side-effect-free t))
  (let ((h (ht-create test)))
    ;; the first key-value pair in an alist gets precedence, so we
    ;; start from the end of the list:
    (dolist (pair (reverse alist) h)
      (let ((key (car pair))
            (value (cdr pair)))
        (ht-set! h key value)))))

(defalias 'ht-from-alist 'ht<-alist)

(defun ht<-plist (plist &optional test)
  "Create a hash table with initial values according to PLIST.

TEST indicates the function used to compare the hash
keys.  Default is `equal'.  It can be `eq', `eql', `equal' or a
user-supplied test created via `define-hash-table-test'."
  (declare (side-effect-free t))
  (let ((h (ht-create test)))
    (dolist (pair (nreverse (-partition 2 plist)) h)
      (let ((key (car pair))
            (value (cadr pair)))
        (ht-set! h key value)))))

(defalias 'ht-from-plist 'ht<-plist)

(define-inline ht-get (table key &optional default)
  "Look up KEY in TABLE, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (declare (side-effect-free t))
  (inline-quote
   (gethash ,key ,table ,default)))

;; Don't use `ht-set!' here, gv setter was assumed to return the value
;; to be set.
(gv-define-setter ht-get (value table key) `(puthash ,key ,value ,table))

(define-inline ht-get* (table &rest keys)
  "Look up KEYS in nested hash tables, starting with TABLE.
The lookup for each key should return another hash table, except
for the final key, which may return any value."
  (declare (side-effect-free t))
  (inline-letevals (table keys)
    (inline-quote
     (prog1 ,table
       (while ,keys
         (setf ,table (ht-get table (pop ,keys))))))))

(put 'ht-get* 'compiler-macro
     (lambda (_ table &rest keys)
       (--reduce-from `(ht-get ,acc ,it) table keys)))

(defun ht-update! (table from-table)
  "Update TABLE according to every key-value pair in FROM-TABLE."
  (maphash
   (lambda (key value) (puthash key value table))
   from-table)
  nil)

(defalias 'ht-update 'ht-update!)

(defun ht-merge (&rest tables)
  "Crete a new tables that includes all the key-value pairs from TABLES.
If multiple have tables have the same key, the value in the last
table is used."
  (let ((merged (ht-create)))
    (mapc (lambda (table) (ht-update! merged table)) tables)
    merged))

(define-inline ht-remove! (table key)
  "Remove KEY from TABLE."
  (inline-quote (remhash ,key ,table)))

(defalias 'ht-remove 'ht-remove!)

(define-inline ht-clear! (table)
  "Remove all keys from TABLE."
  (inline-quote
   (prog1 nil
     (clrhash ,table))))

(defalias 'ht-clear 'ht-clear!)

(defun ht-map (function table)
  "Apply FUNCTION to each key-value pair of TABLE, and make a list of the results.
FUNCTION is called with two arguments, KEY and VALUE."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall function key value) results))
     table)
    results))

(defmacro ht-amap (form table)
  "Anaphoric version of `ht-map'.
For every key-value pair in TABLE, evaluate FORM with the
variables KEY and VALUE bound.  If you don't use both of
these variables, then use `ht-map' to avoid warnings."
  `(ht-map (lambda (key value) ,form) ,table))

(defun ht-keys (table)
  "Return a list of all the keys in TABLE."
  (declare (side-effect-free t))
  (ht-map (lambda (key _value) key) table))

(defun ht-values (table)
  "Return a list of all the values in TABLE."
  (declare (side-effect-free t))
  (ht-map (lambda (_key value) value) table))

(defun ht-items (table)
  "Return a list of two-element lists '(key value) from TABLE."
  (declare (side-effect-free t))
  (ht-amap (list key value) table))

(defalias 'ht-each 'maphash
  "Apply FUNCTION to each key-value pair of TABLE.
Returns nil, used for side-effects only.")

(defmacro ht-aeach (form table)
  "Anaphoric version of `ht-each'.
For every key-value pair in TABLE, evaluate FORM with the
variables key and value bound."
  `(ht-each (lambda (key value) ,form) ,table))

(defun ht-select-keys (table keys)
  "Return a copy of TABLE with only the specified KEYS."
  (declare (side-effect-free t))
  (let (result)
    (setq result (make-hash-table :test (hash-table-test table)))
    (dolist (key keys result)
      (if (not (equal (gethash key table 'key-not-found) 'key-not-found))
          (puthash key (gethash key table) result)))))

(defun ht->plist (table)
  "Return a flat list '(key1 value1 key2 value2...) from TABLE.

Note that hash tables are unordered, so this cannot be an exact
inverse of `ht<-plist'.  The following is not guaranteed:

\(let ((data '(a b c d)))
  (equalp data
          (ht->plist (ht<-plist data))))"
  (declare (side-effect-free t))
  (apply 'append (ht-items table)))

(defalias 'ht-to-plist 'ht->plist)

(define-inline ht-copy (table)
  "Return a shallow copy of TABLE (keys and values are shared)."
  (declare (side-effect-free t))
  (inline-quote (copy-hash-table ,table)))

(defun ht->alist (table)
  "Return a list of two-element lists '(key . value) from TABLE.

Note that hash tables are unordered, so this cannot be an exact
inverse of `ht<-alist'.  The following is not guaranteed:

\(let ((data '((a . b) (c . d))))
  (equalp data
          (ht->alist (ht<-alist data))))"
  (declare (side-effect-free t))
  (ht-amap (cons key value) table))

(defalias 'ht-to-alist 'ht->alist)

(defalias 'ht? 'hash-table-p)

(defalias 'ht-p 'hash-table-p)

(define-inline ht-contains? (table key)
  "Return 't if TABLE contains KEY."
  (declare (side-effect-free t))
  (inline-quote
   (let ((not-found-symbol (make-symbol "ht--not-found")))
     (not (eq (ht-get ,table ,key not-found-symbol) not-found-symbol)))))

(defalias 'ht-contains-p 'ht-contains?)

(define-inline ht-size (table)
  "Return the actual number of entries in TABLE."
  (declare (side-effect-free t))
  (inline-quote
   (hash-table-count ,table)))

(define-inline ht-empty? (table)
  "Return true if the actual number of entries in TABLE is zero."
  (declare (side-effect-free t))
  (inline-quote
   (zerop (ht-size ,table))))

(defalias 'ht-empty-p 'ht-empty?)

(defun ht-select (function table)
  "Return a hash table containing all entries in TABLE for which
FUNCTION returns a truthy value.

FUNCTION is called with two arguments, KEY and VALUE."
  (let ((results (ht-create)))
    (ht-each
     (lambda (key value)
       (when (funcall function key value)
         (ht-set! results key value)))
     table)
    results))

(defun ht-reject (function table)
  "Return a hash table containing all entries in TABLE for which
FUNCTION returns a falsy value.

FUNCTION is called with two arguments, KEY and VALUE."
  (let ((results (ht-create)))
    (ht-each
     (lambda (key value)
       (unless (funcall function key value)
         (ht-set! results key value)))
     table)
    results))

(defun ht-reject! (function table)
  "Delete entries from TABLE for which FUNCTION returns a falsy value.

FUNCTION is called with two arguments, KEY and VALUE."
  (ht-each
   (lambda (key value)
     (when (funcall function key value)
       (remhash key table)))
   table)
  nil)

(defalias 'ht-delete-if 'ht-reject!)

(defun ht-find (function table)
  "Return (key, value) from TABLE for which FUNCTION returns a truthy value.
Return nil otherwise.

FUNCTION is called with two arguments, KEY and VALUE."
  (catch 'break
    (ht-each
     (lambda (key value)
       (when (funcall function key value)
         (throw 'break (list key value))))
     table)))

(defun ht-equal? (table1 table2)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Does not compare equality predicates."
  (declare (side-effect-free t))
  (let ((keys1 (ht-keys table1))
        (keys2 (ht-keys table2))
        (sentinel (make-symbol "ht-sentinel")))
    (and (equal (length keys1) (length keys2))
         (--all?
          (equal (ht-get table1 it)
                 (ht-get table2 it sentinel))
          keys1))))

(defalias 'ht-equal-p 'ht-equal?)

(provide 'ht)
;;; ht.el ends here
