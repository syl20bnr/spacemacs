;;; kv.el --- key/value data structure functions

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.19
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 7th September 2012

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

;; Some routines for working with key/value data structures like
;; hash-tables and alists and plists.

;; This also takes over the dotassoc stuff and provides it separately.

;;; Code:

(eval-when-compile (require 'cl))


(defun kvalist->hash (alist &rest hash-table-args)
  "Convert ALIST to a HASH.

HASH-TABLE-ARGS are passed to the hash-table creation."
  (let ((table (apply 'make-hash-table hash-table-args)))
    (mapc
     (lambda (pair)
       (puthash (car pair) (cdr pair) table))
     alist)
    table))

(defun kvhash->alist (hash &optional func)
  "Convert HASH to an ALIST.

Optionally filter through FUNC, only non-nil values returned from
FUNC are stored as the resulting value against the converted
key."
  (when hash
    (let (store)
      (maphash
       (lambda (key value)
         (when key
           (if (and (functionp func))
               (let ((res (funcall func key value)))
                 (when res
                   (setq store (acons key res store))))
               ;; else no filtering, just return
               (setq store (acons key value store)))))
       hash)
      store)))

(defun kvfa (key alist receive)
  "Call RECEIVE with whatever comes out of ALIST for KEY.

RECEIVE can do whatever destructuring you want, the first
argument is always the car of the alist pair."
  (apply receive (let ((a (assoc key alist)))
                   (append (list (car a))
                           (if (listp (cdr a))(cdr a)(list (cdr a)))))))

(defun kva (key alist)
  "Retrieve the value assigned to KEY in ALIST.

This uses `assoc' as the lookup mechanism."
  (cdr (assoc key alist)))

(defun kvaq (key alist)
  "Retrieve the value assigned to KEY in ALIST.

This uses `assq' as the lookup mechanism."
  (cdr (assq key alist)))

(defun kvaqc (key alist)
  "Retrieve the value assigned to KEY in ALIST.

This uses first the `assq' and then `assoc' as the lookup
mechanism."
  (cdr (or (assq key alist)
           (assoc key alist))))

(defun kvassoc= (key value alist)
  "Is the value assocd to KEY in ALIST equal to VALUE?

Returns the value looked up by KEY that passes, so normally:

  KEY . VALUE
"
  (let ((v (assoc key alist)))
    (and v (equal (cdr v) value) v)))

(defun kvassoqc (key alist)
  "String or symbol assoc."
  (let ((v (or
            (assq (if (symbolp key) key (intern key)) alist)
            (or (assoc key alist)
                ;; not sure about this behaviour... see test
                (assoc (symbol-name key) alist)))))  v))

(defun kvassoq= (key value alist)
  "Test the VALUE with the value bound to KEY in ALIST.

The lookup mechanism is to ensure the key is a symbol and then
use assq.  Hence the name of the function being a mix of assoc
and assq.

Returns the value looked up by KEY that passes, so normally:

  KEY . VALUE
"
  (let ((v (kvassoqc key alist)))
    (and v (equal (cdr v) value) v)))

(defun kvmatch (key regex alist)
  "Test the value with KEY in ALIST matches REGEX."
  (let ((v (kvassoqc key alist)))
    (and v (string-match regex (cdr v)) v)))

(defun* kvquery->func (query &key
                             (equal-func 'kvassoc=)
                             (match-func 'kvmatch))
  "Turn a simple QUERY expression into a filter function.

EQUAL-FUNC is the function that implements the equality
predicate.

MATCH-FUNC is the function that implements the match predicate.

The query language is:

 | a b  - true if a or b is true
 & a b  - true only if a and b is true
 = a b  - true if a equals b as per the EQUAL-FUNC
 ~ a b  - true if a matches b as per the MATCH-FUNC

So, for example:

 (|(= a b)(= c d))

Means: if `a' equals `b', or if `c' equals `d' then the
expression is true."
  (flet ((query-parse (query)
           (let ((part (car query))
                 (rest (cdr query)))
             (cond
               ((eq part '|)
                (cons 'or
                      (loop for i in rest
                         collect (query-parse i))))
               ((eq part '&)
                (cons 'and
                      (loop for i in rest
                         collect (query-parse i))))
               ((eq part '~)
                (destructuring-bind (field value) rest
                  (list match-func field value (quote record))))
               ((eq part '=)
                (destructuring-bind (field value) rest
                  (list equal-func field value (quote record))))))))
    (eval `(lambda (record) ,(query-parse query)))))

(defun kvplist2get (plist2 keyword value)
  "Get the plist with KEYWORD / VALUE from the list of plists."
  (loop for plist in plist2
     if (equal (plist-get plist keyword) value)
     return plist))

(defun kvthing->keyword (str-or-symbol)
  "Convert STR-OR-SYMBOL into a keyword symbol."
  (let ((str
         (cond
           ((symbolp str-or-symbol) (symbol-name str-or-symbol))
           ((stringp str-or-symbol) str-or-symbol))))
    (intern
     (if (eq (aref str 0) ?:) str (concat ":" str)))))

(defun kvalist->plist (alist)
  "Convert an alist to a plist."
  ;; Why doesn't elisp provide this?
  (loop for pair in alist
     append (list
             (kvthing->keyword
              (car pair))
             (cdr pair))))

(defun kvacons (&rest args)
  "Make an alist from the plist style args."
  (kvplist->alist args))

(defun keyword->symbol (keyword)
  "A keyword is a symbol leading with a :.

Converting to a symbol means dropping the :."
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defun kvplist->alist (plist &optional keys-are-keywords)
  "Convert PLIST to an alist.

The keys are expected to be :prefixed and the colons are removed
unless KEYS-ARE-KEYWORDS is `t'.

The keys in the resulting alist are always symbols."
  (when plist
    (loop for (key value . rest) on plist by 'cddr
       collect
         (cons (if keys-are-keywords
                   key
                   (keyword->symbol key))
               value))))

(defun kvalist2->plist (alist2)
  "Convert a list of alists too a list of plists."
  (loop for alist in alist2
       append
       (list (kvalist->plist alist))))

(defun kvalist->keys (alist)
  "Get just the keys from the alist."
  (mapcar (lambda (pair) (car pair)) alist))

(defun kvalist->values (alist)
  "Get just the values from the alist."
  (mapcar (lambda (pair) (cdr pair)) alist))

(defun kvalist-sort (alist pred)
  "Sort ALIST (by key) with PRED."
  (sort alist (lambda (a b) (funcall pred (car a) (car b)))))

(defun kvalist-sort-by-value (alist pred)
  "Sort ALIST by value with PRED."
  (sort alist (lambda (a b) (funcall pred (cdr a) (cdr b)))))

(defun kvalist->filter-keys (alist &rest keys)
  "Return the ALIST filtered to the KEYS list.

Only pairs where the car is a `member' of KEYS will be returned."
  (loop for a in alist
     if (member (car a) keys)
     collect a))

(defun kvplist->filter-keys (plist &rest keys)
  "Filter the plist to just those matching KEYS.

`kvalist->filter-keys' is actually used to do this work."
  (let ((symkeys
         (loop for k in keys
            collect (let ((strkey (symbol-name k)))
                      (if (equal (substring strkey 0 1) ":")
                          (intern (substring strkey 1))
                          k)))))
    (kvalist->plist
     (apply
      'kvalist->filter-keys
      (cons (kvplist->alist plist) symkeys)))))

(defun kvplist2->filter-keys (plist2 &rest keys)
  "Return the PLIST2 (a list of plists) filtered to the KEYS."
  (loop for plist in plist2
     collect (apply 'kvplist->filter-keys (cons plist keys))))

(defun kvalist2->filter-keys (alist2 &rest keys)
  "Return the ALIST2 (a list of alists) filtered to the KEYS."
  (loop for alist in alist2
     collect (apply 'kvalist->filter-keys (cons alist keys))))

(defun kvalist2->alist (alist2 car-key cdr-key &optional proper)
  "Reduce the ALIST2 (a list of alists) to a single alist.

CAR-KEY is the key of each alist to use as the resulting key and
CDR-KEY is the key of each alist to user as the resulting cdr.

For example, if CAR-KEY is `email' and CDR-KEY is `name' the
records:

  '((user . \"nic\")(name . \"Nic\")(email . \"nic@domain\")
    (user . \"jim\")(name . \"Jim\")(email . \"jim@domain\"))

could be reduced to:

  '((\"nic@domain\" . \"Nic\")
    (\"jim@domain\" . \"Jic\"))

If PROPER is `t' then the alist is a list of proper lists, not
cons cells."
  (loop for alist in alist2
       collect (apply (if proper 'list 'cons)
                      (list
                       (assoc-default car-key alist)
                       (assoc-default cdr-key alist)))))

(defun kvalist-keys->* (alist fn)
  "Convert the keys of ALIST through FN."
  (mapcar
   (lambda (pair)
     (cons
      (funcall fn (car pair))
      (cdr pair)))
   alist))

(defun* kvalist-keys->symbols (alist &key (first-fn 'identity))
  "Convert the keys of ALIST into symbols.

If key parameter FIRST-FN is present it should be a function
which will be used to first transform the string key.  A popular
choice might be `downcase' for example, to cause all symbol keys
to be lower-case."
  (kvalist-keys->*
   alist
   (lambda (key)
     (intern (funcall first-fn (format "%s" key))))))

(defun kvalist2-filter (alist2 fn)
  "Filter the list of alists with FN."
  (let (value)
    (loop for rec in alist2
       do (setq value (funcall fn rec))
       if value
       collect rec)))

(defun kvidentity (a b)
  "Returns a cons of A B."
  (cons a b))

(defun kvcar (a b)
  "Given A B returns A."
  a)

(defun kvcdr (a b)
  "Given A B returns B."
  b)

(defun kvcmp (a b)
  "Do a comparison of the two values using printable syntax.

Use this as the function to pass to `sort'."
  (string-lessp (if a (format "%S" a) "")
                (if b (format "%S" b) "")))

(defun kvqsort (lst)
  "Do a sort using `kvcmp'."
  (sort lst 'kvcmp))

(progn
  (put 'kvalist-key
       'error-conditions
       '(error))
  (put 'kvalist-key
       'error-message
       "No such key found in alist."))

(defun kvalist-set-value! (alist key value)
  "Destructively set the value of KEY to VALUE in ALIST.

If the assoc is not found this adds it to alist."
  (let ((cell (assoc key alist)))
    (if (consp cell)
        (setcdr cell value)
        ;; Else what to do?
        (signal 'kvalist-key (list alist key)))))

(defun kvdotassoc-fn (expr table func)
  "Use the dotted EXPR to access deeply nested data in TABLE.

EXPR is a dot separated expression, either a symbol or a string.
For example:

 \"a.b.c\"

or:

 'a.b.c

If the EXPR is a symbol then the keys of the alist are also
expected to be symbols.

TABLE is expected to be an alist currently.

FUNC is some sort of `assoc' like function."
  (let ((state table)
        (parts
         (if (symbolp expr)
             (mapcar
              'intern
              (split-string (symbol-name expr) "\\."))
             ;; Else it's a string
             (split-string expr "\\."))))
    (catch 'break
      (while (listp parts)
        (let ((traverse (funcall func (car parts) state)))
          (setq parts (cdr parts))
          (if parts
              (setq state (cdr traverse))
              (throw 'break (cdr traverse))))))))

(defun kvdotassoc (expr table)
  "Dotted expression handling with `assoc'."
  (kvdotassoc-fn expr table 'assoc))

(defun kvdotassq (expr table)
  "Dotted expression handling with `assq'."
  (kvdotassoc-fn expr table 'assq))

(defun kvdotassoc= (expr value table)
  (let ((v (kvdotassoc expr table)))
    (and v (equal v value) v)))

(defalias 'dotassoc 'kvdotassoc)
(defalias 'dotassq 'kvdotassq)

;; Thank you taylanub for this wonderful abstraction.
(defmacro kv--destructuring-map (map-function args sequence &rest body)
  "Helper macro for `destructuring-mapcar' and `destructuring-map'."
  (declare (indent 3))
  (let ((entry (gensym)))
    `(,map-function (lambda (,entry)
                      (destructuring-bind ,args ,entry ,@body))
                    ,sequence)))

(defmacro kvmap-bind (args sexp seq)
  "A hybrid of `destructuring-bind' and `mapcar'
ARGS shall be of the form used with `destructuring-bind'

Unlike most other mapping forms this is a macro intended to be
used for structural transformations, so the expected usage will
be that ARGS describes the structure of the items in SEQ, and
SEXP will describe the structure desired."
  (declare (indent 2))
  `(kv--destructuring-map mapcar ,args ,seq ,sexp))

(defalias 'map-bind 'kvmap-bind)

(defun kvplist-merge (&rest plists)
  "Merge the 2nd and subsequent plists into the first.

Values set by lists to the left are clobbered."
  (let ((result (car plists))
        (plists (cdr plists)))
    (loop for plist in plists do
          (loop for (key val) on plist by 'cddr do
                (setq result (plist-put result key val))))
    result))

(provide 'kv)
(provide 'dotassoc)

;;; kv.el ends here
