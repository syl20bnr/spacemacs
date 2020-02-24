;;; dash.el --- A modern list library for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 2.17.0
;; Keywords: lists

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

;; A modern list api for Emacs.
;;
;; See documentation on https://github.com/magnars/dash.el#functions
;;
;; **Please note** The lexical binding in this file is not utilised at the
;; moment. We will take full advantage of lexical binding in an upcoming 3.0
;; release of Dash. In the meantime, we've added the pragma to avoid a bug that
;; you can read more about in https://github.com/magnars/dash.el/issues/130.
;;

;;; Code:

;; TODO: `gv' was introduced in Emacs 24.3, so remove this and all
;; calls to `defsetf' when support for earlier versions is dropped.
(eval-when-compile
  (unless (fboundp 'gv-define-setter)
    (require 'cl)))

(defgroup dash ()
  "Customize group for dash.el"
  :group 'lisp
  :prefix "dash-")

(defun dash--enable-fontlock (symbol value)
  (when value
    (dash-enable-font-lock))
  (set-default symbol value))

(defcustom dash-enable-fontlock nil
  "If non-nil, enable fontification of dash functions, macros and
special values."
  :type 'boolean
  :set 'dash--enable-fontlock
  :group 'dash)

(defmacro !cons (car cdr)
  "Destructive: Set CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Set LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro --each (list &rest body)
  "Anaphoric form of `-each'."
  (declare (debug (form body))
           (indent 1))
  (let ((l (make-symbol "list")))
    `(let ((,l ,list)
           (it-index 0))
       (while ,l
         (let ((it (car ,l)))
           ,@body)
         (setq it-index (1+ it-index))
         (!cdr ,l)))))

(defmacro -doto (eval-initial-value &rest forms)
  "Eval a form, then insert that form as the 2nd argument to other forms.
The EVAL-INITIAL-VALUE form is evaluated once. Its result is
passed to FORMS, which are then evaluated sequentially. Returns
the target form."
  (declare (indent 1))
  (let ((retval (make-symbol "value")))
    `(let ((,retval ,eval-initial-value))
       ,@(mapcar (lambda (form)
                   (if (sequencep form)
                       `(,(-first-item form) ,retval ,@(cdr form))
                     `(funcall form ,retval)))
                 forms)
       ,retval)))

(defmacro --doto (eval-initial-value &rest forms)
  "Anaphoric form of `-doto'.
Note: `it' is not required in each form."
  (declare (indent 1))
  `(let ((it ,eval-initial-value))
     ,@forms
     it))

(defun -each (list fn)
  "Call FN with every item in LIST. Return nil, used for side-effects only."
  (--each list (funcall fn it)))

(put '-each 'lisp-indent-function 1)

(defalias '--each-indexed '--each)

(defun -each-indexed (list fn)
  "Call (FN index item) for each item in LIST.

In the anaphoric form `--each-indexed', the index is exposed as symbol `it-index'.

See also: `-map-indexed'."
  (--each list (funcall fn it-index it)))
(put '-each-indexed 'lisp-indent-function 1)

(defmacro --each-while (list pred &rest body)
  "Anaphoric form of `-each-while'."
  (declare (debug (form form body))
           (indent 2))
  (let ((l (make-symbol "list"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,c t)
           (it-index 0))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if (not ,pred) (setq ,c nil) ,@body))
         (setq it-index (1+ it-index))
         (!cdr ,l)))))

(defun -each-while (list pred fn)
  "Call FN with every item in LIST while (PRED item) is non-nil.
Return nil, used for side-effects only."
  (--each-while list (funcall pred it) (funcall fn it)))

(put '-each-while 'lisp-indent-function 2)

(defmacro --each-r (list &rest body)
  "Anaphoric form of `-each-r'."
  (declare (debug (form body))
           (indent 1))
  (let ((v (make-symbol "vector")))
    ;; Implementation note: building vector is considerably faster
    ;; than building a reversed list (vector takes less memory, so
    ;; there is less GC), plus length comes naturally.  In-place
    ;; 'nreverse' would be faster still, but BODY would be able to see
    ;; that, even if modification was reversed before we return.
    `(let* ((,v (vconcat ,list))
            (it-index (length ,v))
            it)
       (while (> it-index 0)
         (setq it-index (1- it-index))
         (setq it (aref ,v it-index))
         ,@body))))

(defun -each-r (list fn)
  "Call FN with every item in LIST in reversed order.
 Return nil, used for side-effects only."
  (--each-r list (funcall fn it)))

(defmacro --each-r-while (list pred &rest body)
  "Anaphoric form of `-each-r-while'."
  (declare (debug (form form body))
           (indent 2))
  (let ((v (make-symbol "vector")))
    `(let* ((,v (vconcat ,list))
            (it-index (length ,v))
            it)
       (while (> it-index 0)
         (setq it-index (1- it-index))
         (setq it (aref ,v it-index))
         (if (not ,pred)
             (setq it-index -1)
           ,@body)))))

(defun -each-r-while (list pred fn)
  "Call FN with every item in reversed LIST while (PRED item) is non-nil.
Return nil, used for side-effects only."
  (--each-r-while list (funcall pred it) (funcall fn it)))

(defmacro --dotimes (num &rest body)
  "Repeatedly executes BODY (presumably for side-effects) with symbol `it' bound to integers from 0 through NUM-1."
  (declare (debug (form body))
           (indent 1))
  (let ((n (make-symbol "num")))
    `(let ((,n ,num)
           (it 0))
       (while (< it ,n)
         ,@body
         (setq it (1+ it))))))

(defun -dotimes (num fn)
  "Repeatedly calls FN (presumably for side-effects) passing in integers from 0 through NUM-1."
  (--dotimes num (funcall fn it)))

(put '-dotimes 'lisp-indent-function 1)

(defun -map (fn list)
  "Return a new list consisting of the result of applying FN to the items in LIST."
  (mapcar fn list))

(defmacro --map (form list)
  "Anaphoric form of `-map'."
  (declare (debug (form form)))
  `(mapcar (lambda (it) ,form) ,list))

(defmacro --reduce-from (form initial-value list)
  "Anaphoric form of `-reduce-from'."
  (declare (debug (form form form)))
  `(let ((acc ,initial-value))
     (--each ,list (setq acc ,form))
     acc))

(defun -reduce-from (fn initial-value list)
  "Return the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, return INITIAL-VALUE and
do not call FN.

In the anaphoric form `--reduce-from', the accumulated value is
exposed as symbol `acc'.

See also: `-reduce', `-reduce-r'"
  (--reduce-from (funcall fn acc it) initial-value list))

(defmacro --reduce (form list)
  "Anaphoric form of `-reduce'."
  (declare (debug (form form)))
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv ,list))
       (if ,lv
           (--reduce-from ,form (car ,lv) (cdr ,lv))
         (let (acc it) ,form)))))

(defun -reduce (fn list)
  "Return the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, return the result of calling FN with no
arguments. If LIST contains a single item, return that item
and do not call FN.

In the anaphoric form `--reduce', the accumulated value is
exposed as symbol `acc'.

See also: `-reduce-from', `-reduce-r'"
  (if list
      (-reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro --reduce-r-from (form initial-value list)
  "Anaphoric version of `-reduce-r-from'."
  (declare (debug (form form form)))
  `(--reduce-from ,form ,initial-value (reverse ,list)))

(defun -reduce-r-from (fn initial-value list)
  "Replace conses with FN, nil with INITIAL-VALUE and evaluate
the resulting expression. If LIST is empty, INITIAL-VALUE is
returned and FN is not called.

Note: this function works the same as `-reduce-from' but the
operation associates from right instead of from left.

See also: `-reduce-r', `-reduce'"
  (--reduce-r-from (funcall fn it acc) initial-value list))

(defmacro --reduce-r (form list)
  "Anaphoric version of `-reduce-r'."
  (declare (debug (form form)))
  `(--reduce ,form (reverse ,list)))

(defun -reduce-r (fn list)
  "Replace conses with FN and evaluate the resulting expression.
The final nil is ignored. If LIST contains no items, return the
result of calling FN with no arguments. If LIST contains a single
item, return that item and do not call FN.

The first argument of FN is the new item, the second is the
accumulated value.

Note: this function works the same as `-reduce' but the operation
associates from right instead of from left.

See also: `-reduce-r-from', `-reduce'"
  (if list
      (--reduce-r (funcall fn it acc) list)
    (funcall fn)))

(defun -reductions-from (fn init list)
  "Return a list of the intermediate values of the reduction.

See `-reduce-from' for explanation of the arguments.

See also: `-reductions', `-reductions-r', `-reduce-r'"
  (nreverse (--reduce-from (cons (funcall fn (car acc) it) acc) (list init) list)))

(defun -reductions (fn list)
  "Return a list of the intermediate values of the reduction.

See `-reduce' for explanation of the arguments.

See also: `-reductions-from', `-reductions-r', `-reduce-r'"
  (and list (-reductions-from fn (car list) (cdr list))))

(defun -reductions-r-from (fn init list)
  "Return a list of the intermediate values of the reduction.

See `-reduce-r-from' for explanation of the arguments.

See also: `-reductions-r', `-reductions', `-reduce'"
  (--reduce-r-from (cons (funcall fn it (car acc)) acc) (list init) list))

(defun -reductions-r (fn list)
  "Return a list of the intermediate values of the reduction.

See `-reduce-r' for explanation of the arguments.

See also: `-reductions-r-from', `-reductions', `-reduce'"
  (when list
    (let ((rev (reverse list)))
      (--reduce-from (cons (funcall fn it (car acc)) acc)
                     (list (car rev))
                     (cdr rev)))))

(defmacro --filter (form list)
  "Anaphoric form of `-filter'.

See also: `--remove'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun -filter (pred list)
  "Return a new list of the items in LIST for which PRED returns a non-nil value.

Alias: `-select'

See also: `-keep', `-remove'."
  (--filter (funcall pred it) list))

(defalias '-select '-filter)
(defalias '--select '--filter)

(defmacro --remove (form list)
  "Anaphoric form of `-remove'.

See also `--filter'."
  (declare (debug (form form)))
  `(--filter (not ,form) ,list))

(defun -remove (pred list)
  "Return a new list of the items in LIST for which PRED returns nil.

Alias: `-reject'

See also: `-filter'."
  (--remove (funcall pred it) list))

(defalias '-reject '-remove)
(defalias '--reject '--remove)

(defun -remove-first (pred list)
  "Return a new list with the first item matching PRED removed.

Alias: `-reject-first'

See also: `-remove', `-map-first'"
  (let (front)
    (while (and list (not (funcall pred (car list))))
      (push (car list) front)
      (!cdr list))
    (if list
        (-concat (nreverse front) (cdr list))
      (nreverse front))))

(defmacro --remove-first (form list)
  "Anaphoric form of `-remove-first'."
  (declare (debug (form form)))
  `(-remove-first (lambda (it) ,form) ,list))

(defalias '-reject-first '-remove-first)
(defalias '--reject-first '--remove-first)

(defun -remove-last (pred list)
  "Return a new list with the last item matching PRED removed.

Alias: `-reject-last'

See also: `-remove', `-map-last'"
  (nreverse (-remove-first pred (reverse list))))

(defmacro --remove-last (form list)
  "Anaphoric form of `-remove-last'."
  (declare (debug (form form)))
  `(-remove-last (lambda (it) ,form) ,list))

(defalias '-reject-last '-remove-last)
(defalias '--reject-last '--remove-last)

(defun -remove-item (item list)
  "Remove all occurrences of ITEM from LIST.

Comparison is done with `equal'."
  (declare (pure t) (side-effect-free t))
  (--remove (equal it item) list))

(defmacro --keep (form list)
  "Anaphoric form of `-keep'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (!cons ,m ,r))))
       (nreverse ,r))))

(defun -keep (fn list)
  "Return a new list of the non-nil results of applying FN to the items in LIST.

If you want to select the original items satisfying a predicate use `-filter'."
  (--keep (funcall fn it) list))

(defun -non-nil (list)
  "Return all non-nil elements of LIST."
  (declare (pure t) (side-effect-free t))
  (-remove 'null list))

(defmacro --map-indexed (form list)
  "Anaphoric form of `-map-indexed'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list
         (!cons ,form ,r))
       (nreverse ,r))))

(defun -map-indexed (fn list)
  "Return a new list consisting of the result of (FN index item) for each item in LIST.

In the anaphoric form `--map-indexed', the index is exposed as symbol `it-index'.

See also: `-each-indexed'."
  (--map-indexed (funcall fn it-index it) list))

(defmacro --map-when (pred rep list)
  "Anaphoric form of `-map-when'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (!cons (if ,pred ,rep it) ,r))
       (nreverse ,r))))

(defun -map-when (pred rep list)
  "Return a new list where the elements in LIST that do not match the PRED function
are unchanged, and where the elements in LIST that do match the PRED function are mapped
through the REP function.

Alias: `-replace-where'

See also: `-update-at'"
  (--map-when (funcall pred it) (funcall rep it) list))

(defalias '-replace-where '-map-when)
(defalias '--replace-where '--map-when)

(defun -map-first (pred rep list)
  "Replace first item in LIST satisfying PRED with result of REP called on this item.

See also: `-map-when', `-replace-first'"
  (let (front)
    (while (and list (not (funcall pred (car list))))
      (push (car list) front)
      (!cdr list))
    (if list
        (-concat (nreverse front) (cons (funcall rep (car list)) (cdr list)))
      (nreverse front))))

(defmacro --map-first (pred rep list)
  "Anaphoric form of `-map-first'."
  `(-map-first (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

(defun -map-last (pred rep list)
  "Replace last item in LIST satisfying PRED with result of REP called on this item.

See also: `-map-when', `-replace-last'"
  (nreverse (-map-first pred rep (reverse list))))

(defmacro --map-last (pred rep list)
  "Anaphoric form of `-map-last'."
  `(-map-last (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

(defun -replace (old new list)
  "Replace all OLD items in LIST with NEW.

Elements are compared using `equal'.

See also: `-replace-at'"
  (declare (pure t) (side-effect-free t))
  (--map-when (equal it old) new list))

(defun -replace-first (old new list)
  "Replace the first occurrence of OLD with NEW in LIST.

Elements are compared using `equal'.

See also: `-map-first'"
  (declare (pure t) (side-effect-free t))
  (--map-first (equal old it) new list))

(defun -replace-last (old new list)
  "Replace the last occurrence of OLD with NEW in LIST.

Elements are compared using `equal'.

See also: `-map-last'"
  (declare (pure t) (side-effect-free t))
  (--map-last (equal old it) new list))

(defmacro --mapcat (form list)
  "Anaphoric form of `-mapcat'."
  (declare (debug (form form)))
  `(apply 'append (--map ,form ,list)))

(defun -mapcat (fn list)
  "Return the concatenation of the result of mapping FN over LIST.
Thus function FN should return a list."
  (--mapcat (funcall fn it) list))

(defun -flatten (l)
  "Take a nested list L and return its contents as a single, flat list.

Note that because `nil' represents a list of zero elements (an
empty list), any mention of nil in L will disappear after
flattening.  If you need to preserve nils, consider `-flatten-n'
or map them to some unique symbol and then map them back.

Conses of two atoms are considered \"terminals\", that is, they
aren't flattened further.

See also: `-flatten-n'"
  (declare (pure t) (side-effect-free t))
  (if (and (listp l) (listp (cdr l)))
      (-mapcat '-flatten l)
    (list l)))

(defmacro --iterate (form init n)
  "Anaphoric version of `-iterate'."
  (declare (debug (form form form)))
  `(-iterate (lambda (it) ,form) ,init ,n))

(defun -flatten-n (num list)
  "Flatten NUM levels of a nested LIST.

See also: `-flatten'"
  (declare (pure t) (side-effect-free t))
  (-last-item (--iterate (--mapcat (-list it) it) list (1+ num))))

(defun -concat (&rest lists)
  "Return a new list with the concatenation of the elements in the supplied LISTS."
  (declare (pure t) (side-effect-free t))
  (apply 'append lists))

(defalias '-copy 'copy-sequence
  "Create a shallow copy of LIST.

\(fn LIST)")

(defun -splice (pred fun list)
  "Splice lists generated by FUN in place of elements matching PRED in LIST.

FUN takes the element matching PRED as input.

This function can be used as replacement for `,@' in case you
need to splice several lists at marked positions (for example
with keywords).

See also: `-splice-list', `-insert-at'"
  (let (r)
    (--each list
      (if (funcall pred it)
          (let ((new (funcall fun it)))
            (--each new (!cons it r)))
        (!cons it r)))
    (nreverse r)))

(defmacro --splice (pred form list)
  "Anaphoric form of `-splice'."
  `(-splice (lambda (it) ,pred) (lambda (it) ,form) ,list))

(defun -splice-list (pred new-list list)
  "Splice NEW-LIST in place of elements matching PRED in LIST.

See also: `-splice', `-insert-at'"
  (-splice pred (lambda (_) new-list) list))

(defmacro --splice-list (pred new-list list)
  "Anaphoric form of `-splice-list'."
  `(-splice-list (lambda (it) ,pred) ,new-list ,list))

(defun -cons* (&rest args)
  "Make a new list from the elements of ARGS.

The last 2 members of ARGS are used as the final cons of the
result so if the final member of ARGS is not a list the result is
a dotted list."
  (declare (pure t) (side-effect-free t))
  (-reduce-r 'cons args))

(defun -snoc (list elem &rest elements)
  "Append ELEM to the end of the list.

This is like `cons', but operates on the end of list.

If ELEMENTS is non nil, append these to the list as well."
  (-concat list (list elem) elements))

(defmacro --first (form list)
  "Anaphoric form of `-first'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n)
         (when ,form (setq ,n it)))
       ,n)))

(defun -first (pred list)
  "Return the first x in LIST where (PRED x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'.

Alias: `-find'"
  (--first (funcall pred it) list))

(defalias '-find '-first)
(defalias '--find '--first)

(defmacro --some (form list)
  "Anaphoric form of `-some'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n)
         (setq ,n ,form))
       ,n)))

(defun -some (pred list)
  "Return (PRED x) for the first LIST item where (PRED x) is non-nil, else nil.

Alias: `-any'"
  (--some (funcall pred it) list))

(defalias '-any '-some)
(defalias '--any '--some)

(defmacro --last (form list)
  "Anaphoric form of `-last'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each ,list
         (when ,form (setq ,n it)))
       ,n)))

(defun -last (pred list)
  "Return the last x in LIST where (PRED x) is non-nil, else nil."
  (--last (funcall pred it) list))

(defalias '-first-item 'car
  "Return the first item of LIST, or nil on an empty list.

See also: `-second-item', `-last-item'.

\(fn LIST)")

;; Ensure that calls to `-first-item' are compiled to a single opcode,
;; just like `car'.
(put '-first-item 'byte-opcode 'byte-car)
(put '-first-item 'byte-compile 'byte-compile-one-arg)

(defalias '-second-item 'cadr
  "Return the second item of LIST, or nil if LIST is too short.

See also: `-third-item'.

\(fn LIST)")

(defalias '-third-item
  (if (fboundp 'caddr)
      #'caddr
    (lambda (list) (car (cddr list))))
  "Return the third item of LIST, or nil if LIST is too short.

See also: `-fourth-item'.

\(fn LIST)")

(defun -fourth-item (list)
  "Return the fourth item of LIST, or nil if LIST is too short.

See also: `-fifth-item'."
  (declare (pure t) (side-effect-free t))
  (car (cdr (cdr (cdr list)))))

(defun -fifth-item (list)
  "Return the fifth item of LIST, or nil if LIST is too short.

See also: `-last-item'."
  (declare (pure t) (side-effect-free t))
  (car (cdr (cdr (cdr (cdr list))))))

(defun -last-item (list)
  "Return the last item of LIST, or nil on an empty list."
  (declare (pure t) (side-effect-free t))
  (car (last list)))

;; Use `with-no-warnings' to suppress unbound `-last-item' or
;; undefined `gv--defsetter' warnings arising from both
;; `gv-define-setter' and `defsetf' in certain Emacs versions.
(with-no-warnings
  (if (fboundp 'gv-define-setter)
      (gv-define-setter -last-item (val x) `(setcar (last ,x) ,val))
    (defsetf -last-item (x) (val) `(setcar (last ,x) ,val))))

(defun -butlast (list)
  "Return a list of all items in list except for the last."
  ;; no alias as we don't want magic optional argument
  (declare (pure t) (side-effect-free t))
  (butlast list))

(defmacro --count (pred list)
  "Anaphoric form of `-count'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let ((,r 0))
       (--each ,list (when ,pred (setq ,r (1+ ,r))))
       ,r)))

(defun -count (pred list)
  "Counts the number of items in LIST where (PRED item) is non-nil."
  (--count (funcall pred it) list))

(defun ---truthy? (val)
  (declare (pure t) (side-effect-free t))
  (not (null val)))

(defmacro --any? (form list)
  "Anaphoric form of `-any?'."
  (declare (debug (form form)))
  `(---truthy? (--some ,form ,list)))

(defun -any? (pred list)
  "Return t if (PRED x) is non-nil for any x in LIST, else nil.

Alias: `-any-p', `-some?', `-some-p'"
  (--any? (funcall pred it) list))

(defalias '-some? '-any?)
(defalias '--some? '--any?)
(defalias '-any-p '-any?)
(defalias '--any-p '--any?)
(defalias '-some-p '-any?)
(defalias '--some-p '--any?)

(defmacro --all? (form list)
  "Anaphoric form of `-all?'."
  (declare (debug (form form)))
  (let ((a (make-symbol "all")))
    `(let ((,a t))
       (--each-while ,list ,a (setq ,a ,form))
       (---truthy? ,a))))

(defun -all? (pred list)
  "Return t if (PRED x) is non-nil for all x in LIST, else nil.

Alias: `-all-p', `-every?', `-every-p'"
  (--all? (funcall pred it) list))

(defalias '-every? '-all?)
(defalias '--every? '--all?)
(defalias '-all-p '-all?)
(defalias '--all-p '--all?)
(defalias '-every-p '-all?)
(defalias '--every-p '--all?)

(defmacro --none? (form list)
  "Anaphoric form of `-none?'."
  (declare (debug (form form)))
  `(--all? (not ,form) ,list))

(defun -none? (pred list)
  "Return t if (PRED x) is nil for all x in LIST, else nil.

Alias: `-none-p'"
  (--none? (funcall pred it) list))

(defalias '-none-p '-none?)
(defalias '--none-p '--none?)

(defmacro --only-some? (form list)
  "Anaphoric form of `-only-some?'."
  (declare (debug (form form)))
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each-while ,list (not (and ,y ,n))
         (if ,form (setq ,y t) (setq ,n t)))
       (---truthy? (and ,y ,n)))))

(defun -only-some? (pred list)
  "Return `t` if at least one item of LIST matches PRED and at least one item of LIST does not match PRED.
Return `nil` both if all items match the predicate or if none of the items match the predicate.

Alias: `-only-some-p'"
  (--only-some? (funcall pred it) list))

(defalias '-only-some-p '-only-some?)
(defalias '--only-some-p '--only-some?)

(defun -slice (list from &optional to step)
  "Return copy of LIST, starting from index FROM to index TO.

FROM or TO may be negative.  These values are then interpreted
modulo the length of the list.

If STEP is a number, only each STEPth item in the resulting
section is returned.  Defaults to 1."
  (declare (pure t) (side-effect-free t))
  (let ((length (length list))
        (new-list nil))
    ;; to defaults to the end of the list
    (setq to (or to length))
    (setq step (or step 1))
    ;; handle negative indices
    (when (< from 0)
      (setq from (mod from length)))
    (when (< to 0)
      (setq to (mod to length)))

    ;; iterate through the list, keeping the elements we want
    (--each-while list (< it-index to)
      (when (and (>= it-index from)
                 (= (mod (- from it-index) step) 0))
        (push it new-list)))
    (nreverse new-list)))

(defun -take (n list)
  "Return a new list of the first N items in LIST, or all items if there are fewer than N.

See also: `-take-last'"
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--dotimes n
      (when list
        (!cons (car list) result)
        (!cdr list)))
    (nreverse result)))

(defun -take-last (n list)
  "Return the last N items of LIST in order.

See also: `-take'"
  (declare (pure t) (side-effect-free t))
  (copy-sequence (last list n)))

(defalias '-drop 'nthcdr
  "Return the tail of LIST without the first N items.

See also: `-drop-last'

\(fn N LIST)")

(defun -drop-last (n list)
  "Remove the last N items of LIST and return a copy.

See also: `-drop'"
  ;; No alias because we don't want magic optional argument
  (declare (pure t) (side-effect-free t))
  (butlast list n))

(defmacro --take-while (form list)
  "Anaphoric form of `-take-while'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each-while ,list ,form (!cons it ,r))
       (nreverse ,r))))

(defun -take-while (pred list)
  "Return a new list of successive items from LIST while (PRED item) returns a non-nil value."
  (--take-while (funcall pred it) list))

(defmacro --drop-while (form list)
  "Anaphoric form of `-drop-while'."
  (declare (debug (form form)))
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (while (and ,l (let ((it (car ,l))) ,form))
         (!cdr ,l))
       ,l)))

(defun -drop-while (pred list)
  "Return the tail of LIST starting from the first item for which (PRED item) returns nil."
  (--drop-while (funcall pred it) list))

(defun -split-at (n list)
  "Return a list of ((-take N LIST) (-drop N LIST)), in no more than one pass through the list."
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--dotimes n
      (when list
        (!cons (car list) result)
        (!cdr list)))
    (list (nreverse result) list)))

(defun -rotate (n list)
  "Rotate LIST N places to the right.  With N negative, rotate to the left.
The time complexity is O(n)."
  (declare (pure t) (side-effect-free t))
  (when list
    (let* ((len (length list))
           (n-mod-len (mod n len))
           (new-tail-len (- len n-mod-len)))
      (append (-drop new-tail-len list) (-take new-tail-len list)))))

(defun -insert-at (n x list)
  "Return a list with X inserted into LIST at position N.

See also: `-splice', `-splice-list'"
  (declare (pure t) (side-effect-free t))
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons x (cadr split-list)))))

(defun -replace-at (n x list)
  "Return a list with element at Nth position in LIST replaced with X.

See also: `-replace'"
  (declare (pure t) (side-effect-free t))
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons x (cdr (cadr split-list))))))

(defun -update-at (n func list)
  "Return a list with element at Nth position in LIST replaced with `(func (nth n list))`.

See also: `-map-when'"
  (let ((split-list (-split-at n list)))
    (nconc (car split-list) (cons (funcall func (car (cadr split-list))) (cdr (cadr split-list))))))

(defmacro --update-at (n form list)
  "Anaphoric version of `-update-at'."
  (declare (debug (form form form)))
  `(-update-at ,n (lambda (it) ,form) ,list))

(defun -remove-at (n list)
  "Return a list with element at Nth position in LIST removed.

See also: `-remove-at-indices', `-remove'"
  (declare (pure t) (side-effect-free t))
  (-remove-at-indices (list n) list))

(defun -remove-at-indices (indices list)
  "Return a list whose elements are elements from LIST without
elements selected as `(nth i list)` for all i
from INDICES.

See also: `-remove-at', `-remove'"
  (declare (pure t) (side-effect-free t))
  (let* ((indices (-sort '< indices))
         (diffs (cons (car indices) (-map '1- (-zip-with '- (cdr indices) indices))))
         r)
    (--each diffs
      (let ((split (-split-at it list)))
        (!cons (car split) r)
        (setq list (cdr (cadr split)))))
    (!cons list r)
    (apply '-concat (nreverse r))))

(defmacro --split-with (pred list)
  "Anaphoric form of `-split-with'."
  (declare (debug (form form)))
  (let ((l (make-symbol "list"))
        (r (make-symbol "result"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,r nil)
           (,c t))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if (not ,pred)
               (setq ,c nil)
             (!cons it ,r)
             (!cdr ,l))))
       (list (nreverse ,r) ,l))))

(defun -split-with (pred list)
  "Return a list of ((-take-while PRED LIST) (-drop-while PRED LIST)), in no more than one pass through the list."
  (--split-with (funcall pred it) list))

(defmacro -split-on (item list)
  "Split the LIST each time ITEM is found.

Unlike `-partition-by', the ITEM is discarded from the results.
Empty lists are also removed from the result.

Comparison is done by `equal'.

See also `-split-when'"
  (declare (debug (form form)))
  `(-split-when (lambda (it) (equal it ,item)) ,list))

(defmacro --split-when (form list)
  "Anaphoric version of `-split-when'."
  (declare (debug (form form)))
  `(-split-when (lambda (it) ,form) ,list))

(defun -split-when (fn list)
  "Split the LIST on each element where FN returns non-nil.

Unlike `-partition-by', the \"matched\" element is discarded from
the results.  Empty lists are also removed from the result.

This function can be thought of as a generalization of
`split-string'."
  (let (r s)
    (while list
      (if (not (funcall fn (car list)))
          (push (car list) s)
        (when s (push (nreverse s) r))
        (setq s nil))
      (!cdr list))
    (when s (push (nreverse s) r))
    (nreverse r)))

(defmacro --separate (form list)
  "Anaphoric form of `-separate'."
  (declare (debug (form form)))
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each ,list (if ,form (!cons it ,y) (!cons it ,n)))
       (list (nreverse ,y) (nreverse ,n)))))

(defun -separate (pred list)
  "Return a list of ((-filter PRED LIST) (-remove PRED LIST)), in one pass through the list."
  (--separate (funcall pred it) list))

(defun ---partition-all-in-steps-reversed (n step list)
  "Private: Used by -partition-all-in-steps and -partition-in-steps."
  (when (< step 1)
    (error "Step must be a positive number, or you're looking at some juicy infinite loops."))
  (let ((result nil))
    (while list
      (!cons (-take n list) result)
      (setq list (-drop step list)))
    result))

(defun -partition-all-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
The last groups may contain less than N items."
  (declare (pure t) (side-effect-free t))
  (nreverse (---partition-all-in-steps-reversed n step list)))

(defun -partition-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (declare (pure t) (side-effect-free t))
  (let ((result (---partition-all-in-steps-reversed n step list)))
    (while (and result (< (length (car result)) n))
      (!cdr result))
    (nreverse result)))

(defun -partition-all (n list)
  "Return a new list with the items in LIST grouped into N-sized sublists.
The last group may contain less than N items."
  (declare (pure t) (side-effect-free t))
  (-partition-all-in-steps n n list))

(defun -partition (n list)
  "Return a new list with the items in LIST grouped into N-sized sublists.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (declare (pure t) (side-effect-free t))
  (-partition-in-steps n n list))

(defmacro --partition-by (form list)
  "Anaphoric form of `-partition-by'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (s (make-symbol "sublist"))
        (v (make-symbol "value"))
        (n (make-symbol "new-value"))
        (l (make-symbol "list")))
    `(let ((,l ,list))
       (when ,l
         (let* ((,r nil)
                (it (car ,l))
                (,s (list it))
                (,v ,form)
                (,l (cdr ,l)))
           (while ,l
             (let* ((it (car ,l))
                    (,n ,form))
               (unless (equal ,v ,n)
                 (!cons (nreverse ,s) ,r)
                 (setq ,s nil)
                 (setq ,v ,n))
               (!cons it ,s)
               (!cdr ,l)))
           (!cons (nreverse ,s) ,r)
           (nreverse ,r))))))

(defun -partition-by (fn list)
  "Apply FN to each item in LIST, splitting it each time FN returns a new value."
  (--partition-by (funcall fn it) list))

(defmacro --partition-by-header (form list)
  "Anaphoric form of `-partition-by-header'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (s (make-symbol "sublist"))
        (h (make-symbol "header-value"))
        (b (make-symbol "seen-body?"))
        (n (make-symbol "new-value"))
        (l (make-symbol "list")))
    `(let ((,l ,list))
       (when ,l
         (let* ((,r nil)
                (it (car ,l))
                (,s (list it))
                (,h ,form)
                (,b nil)
                (,l (cdr ,l)))
           (while ,l
             (let* ((it (car ,l))
                    (,n ,form))
               (if (equal ,h ,n)
                   (when ,b
                     (!cons (nreverse ,s) ,r)
                     (setq ,s nil)
                     (setq ,b nil))
                 (setq ,b t))
               (!cons it ,s)
               (!cdr ,l)))
           (!cons (nreverse ,s) ,r)
           (nreverse ,r))))))

(defun -partition-by-header (fn list)
  "Apply FN to the first item in LIST. That is the header
value. Apply FN to each item in LIST, splitting it each time FN
returns the header value, but only after seeing at least one
other value (the body)."
  (--partition-by-header (funcall fn it) list))

(defun -partition-after-pred (pred list)
  "Partition directly after each time PRED is true on an element of LIST."
  (when list
    (let ((rest (-partition-after-pred pred
                                       (cdr list))))
      (if (funcall pred (car list))
          ;;split after (car list)
          (cons (list (car list))
                rest)

        ;;don't split after (car list)
        (cons (cons (car list)
                    (car rest))
              (cdr rest))))))

(defun -partition-before-pred (pred list)
  "Partition directly before each time PRED is true on an element of LIST."
  (nreverse (-map #'reverse
                  (-partition-after-pred pred (reverse list)))))

(defun -partition-after-item (item list)
  "Partition directly after each time ITEM appears in LIST."
  (-partition-after-pred (lambda (ele) (equal ele item))
                         list))

(defun -partition-before-item (item list)
  "Partition directly before each time ITEM appears in LIST."
  (-partition-before-pred (lambda (ele) (equal ele item))
                          list))

(defmacro --group-by (form list)
  "Anaphoric form of `-group-by'."
  (declare (debug t))
  (let ((n (make-symbol "n"))
        (k (make-symbol "k"))
        (grp (make-symbol "grp")))
    `(nreverse
      (-map
       (lambda (,n)
         (cons (car ,n)
               (nreverse (cdr ,n))))
       (--reduce-from
        (let* ((,k (,@form))
               (,grp (assoc ,k acc)))
          (if ,grp
              (setcdr ,grp (cons it (cdr ,grp)))
            (push
             (list ,k it)
             acc))
          acc)
        nil ,list)))))

(defun -group-by (fn list)
  "Separate LIST into an alist whose keys are FN applied to the
elements of LIST.  Keys are compared by `equal'."
  (--group-by (funcall fn it) list))

(defun -interpose (sep list)
  "Return a new list of all elements in LIST separated by SEP."
  (declare (pure t) (side-effect-free t))
  (let (result)
    (when list
      (!cons (car list) result)
      (!cdr list))
    (while list
      (setq result (cons (car list) (cons sep result)))
      (!cdr list))
    (nreverse result)))

(defun -interleave (&rest lists)
  "Return a new list of the first item in each list, then the second etc."
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (result)
      (while (-none? 'null lists)
        (--each lists (!cons (car it) result))
        (setq lists (-map 'cdr lists)))
      (nreverse result))))

(defmacro --zip-with (form list1 list2)
  "Anaphoric form of `-zip-with'.

The elements in list1 are bound as symbol `it', the elements in list2 as symbol `other'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result"))
        (l1 (make-symbol "list1"))
        (l2 (make-symbol "list2")))
    `(let ((,r nil)
           (,l1 ,list1)
           (,l2 ,list2))
       (while (and ,l1 ,l2)
         (let ((it (car ,l1))
               (other (car ,l2)))
           (!cons ,form ,r)
           (!cdr ,l1)
           (!cdr ,l2)))
       (nreverse ,r))))

(defun -zip-with (fn list1 list2)
  "Zip the two lists LIST1 and LIST2 using a function FN.  This
function is applied pairwise taking as first argument element of
LIST1 and as second argument element of LIST2 at corresponding
position.

The anaphoric form `--zip-with' binds the elements from LIST1 as symbol `it',
and the elements from LIST2 as symbol `other'."
  (--zip-with (funcall fn it other) list1 list2))

(defun -zip-lists (&rest lists)
  "Zip LISTS together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

The return value is always list of lists, which is a difference
from `-zip-pair' which returns a cons-cell in case two input
lists are provided.

See also: `-zip'"
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (results)
      (while (-none? 'null lists)
        (setq results (cons (mapcar 'car lists) results))
        (setq lists (mapcar 'cdr lists)))
      (nreverse results))))

(defun -zip (&rest lists)
  "Zip LISTS together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

If two lists are provided as arguments, return the groupings as a list
of cons cells. Otherwise, return the groupings as a list of lists.

Use `-zip-lists' if you need the return value to always be a list
of lists.

Alias: `-zip-pair'

See also: `-zip-lists'"
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (results)
      (while (-none? 'null lists)
        (setq results (cons (mapcar 'car lists) results))
        (setq lists (mapcar 'cdr lists)))
      (setq results (nreverse results))
      (if (= (length lists) 2)
          ;; to support backward compatibility, return
          ;; a cons cell if two lists were provided
          (--map (cons (car it) (cadr it)) results)
        results))))

(defalias '-zip-pair '-zip)

(defun -zip-fill (fill-value &rest lists)
  "Zip LISTS, with FILL-VALUE padded onto the shorter lists. The
lengths of the returned groupings are equal to the length of the
longest input list."
  (declare (pure t) (side-effect-free t))
  (apply '-zip (apply '-pad (cons fill-value lists))))

(defun -unzip (lists)
  "Unzip LISTS.

This works just like `-zip' but takes a list of lists instead of
a variable number of arguments, such that

  (-unzip (-zip L1 L2 L3 ...))

is identity (given that the lists are the same length).

Note in particular that calling this on a list of two lists will
return a list of cons-cells such that the aboce identity works.

See also: `-zip'"
  (apply '-zip lists))

(defun -cycle (list)
  "Return an infinite copy of LIST that will cycle through the
elements and repeat from the beginning."
  (declare (pure t) (side-effect-free t))
  (let ((newlist (-map 'identity list)))
    (nconc newlist newlist)))

(defun -pad (fill-value &rest lists)
  "Appends FILL-VALUE to the end of each list in LISTS such that they
will all have the same length."
  (let* ((annotations (-annotate 'length lists))
         (n (-max (-map 'car annotations))))
    (--map (append (cdr it) (-repeat (- n (car it)) fill-value)) annotations)))

(defun -annotate (fn list)
  "Return a list of cons cells where each cell is FN applied to each
element of LIST paired with the unmodified element of LIST."
  (-zip (-map fn list) list))

(defmacro --annotate (form list)
  "Anaphoric version of `-annotate'."
  (declare (debug (form form)))
  `(-annotate (lambda (it) ,form) ,list))

(defun dash--table-carry (lists restore-lists &optional re)
  "Helper for `-table' and `-table-flat'.

If a list overflows, carry to the right and reset the list."
  (while (not (or (car lists)
                  (equal lists '(nil))))
    (setcar lists (car restore-lists))
    (pop (cadr lists))
    (!cdr lists)
    (!cdr restore-lists)
    (when re
      (push (nreverse (car re)) (cadr re))
      (setcar re nil)
      (!cdr re))))

(defun -table (fn &rest lists)
  "Compute outer product of LISTS using function FN.

The function FN should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The dimension of the result is (length lists).

See also: `-table-flat'"
  (let ((restore-lists (copy-sequence lists))
        (last-list (last lists))
        (re (make-list (length lists) nil)))
    (while (car last-list)
      (let ((item (apply fn (-map 'car lists))))
        (push item (car re))
        (setcar lists (cdar lists)) ;; silence byte compiler
        (dash--table-carry lists restore-lists re)))
    (nreverse (car (last re)))))

(defun -table-flat (fn &rest lists)
  "Compute flat outer product of LISTS using function FN.

The function FN should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The results are flattened, ignoring the tensor structure
of the result.  This is equivalent to calling:

  (-flatten-n (1- (length lists)) (apply \\='-table fn lists))

but the implementation here is much more efficient.

See also: `-flatten-n', `-table'"
  (let ((restore-lists (copy-sequence lists))
        (last-list (last lists))
        re)
    (while (car last-list)
      (let ((item (apply fn (-map 'car lists))))
        (push item re)
        (setcar lists (cdar lists)) ;; silence byte compiler
        (dash--table-carry lists restore-lists)))
    (nreverse re)))

(defun -partial (fn &rest args)
  "Take a function FN and fewer than the normal arguments to FN,
and return a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS first and
then additional args."
  (apply 'apply-partially fn args))

(defun -elem-index (elem list)
  "Return the index of the first element in the given LIST which
is equal to the query element ELEM, or nil if there is no
such element."
  (declare (pure t) (side-effect-free t))
  (car (-elem-indices elem list)))

(defun -elem-indices (elem list)
  "Return the indices of all elements in LIST equal to the query
element ELEM, in ascending order."
  (declare (pure t) (side-effect-free t))
  (-find-indices (-partial 'equal elem) list))

(defun -find-indices (pred list)
  "Return the indices of all elements in LIST satisfying the
predicate PRED, in ascending order."
  (apply 'append (--map-indexed (when (funcall pred it) (list it-index)) list)))

(defmacro --find-indices (form list)
  "Anaphoric version of `-find-indices'."
  (declare (debug (form form)))
  `(-find-indices (lambda (it) ,form) ,list))

(defun -find-index (pred list)
  "Take a predicate PRED and a LIST and return the index of the
first element in the list satisfying the predicate, or nil if
there is no such element.

See also `-first'."
  (car (-find-indices pred list)))

(defmacro --find-index (form list)
  "Anaphoric version of `-find-index'."
  (declare (debug (form form)))
  `(-find-index (lambda (it) ,form) ,list))

(defun -find-last-index (pred list)
  "Take a predicate PRED and a LIST and return the index of the
last element in the list satisfying the predicate, or nil if
there is no such element.

See also `-last'."
  (-last-item (-find-indices pred list)))

(defmacro --find-last-index (form list)
  "Anaphoric version of `-find-last-index'."
  `(-find-last-index (lambda (it) ,form) ,list))

(defun -select-by-indices (indices list)
  "Return a list whose elements are elements from LIST selected
as `(nth i list)` for all i from INDICES."
  (declare (pure t) (side-effect-free t))
  (let (r)
    (--each indices
      (!cons (nth it list) r))
    (nreverse r)))

(defun -select-columns (columns table)
  "Select COLUMNS from TABLE.

TABLE is a list of lists where each element represents one row.
It is assumed each row has the same length.

Each row is transformed such that only the specified COLUMNS are
selected.

See also: `-select-column', `-select-by-indices'"
  (declare (pure t) (side-effect-free t))
  (--map (-select-by-indices columns it) table))

(defun -select-column (column table)
  "Select COLUMN from TABLE.

TABLE is a list of lists where each element represents one row.
It is assumed each row has the same length.

The single selected column is returned as a list.

See also: `-select-columns', `-select-by-indices'"
  (declare (pure t) (side-effect-free t))
  (--mapcat (-select-by-indices (list column) it) table))

(defmacro -> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the second item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
second item in second form, etc."
  (declare (debug (form &rest [&or symbolp (sexp &rest form)])))
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))

(defmacro ->> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
last item in second form, etc."
  (declare (debug ->))
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,@form ,x)
                  (list form x)))
   (:else `(->> (->> ,x ,form) ,@more))))

(defmacro --> (x &rest forms)
  "Starting with the value of X, thread each expression through FORMS.

Insert X at the position signified by the symbol `it' in the first
form.  If there are more forms, insert the first form at the position
signified by `it' in in second form, etc."
  (declare (debug (form body)))
  `(-as-> ,x it ,@forms))

(defmacro -as-> (value variable &rest forms)
  "Starting with VALUE, thread VARIABLE through FORMS.

In the first form, bind VARIABLE to VALUE.  In the second form, bind
VARIABLE to the result of the first form, and so forth."
  (declare (debug (form symbolp body)))
  (if (null forms)
      `,value
    `(let ((,variable ,value))
       (-as-> ,(if (symbolp (car forms))
                 (list (car forms) variable)
               (car forms))
            ,variable
              ,@(cdr forms)))))

(defmacro -some-> (x &optional form &rest more)
  "When expr is non-nil, thread it through the first form (via `->'),
and when that result is non-nil, through the next form, etc."
  (declare (debug ->)
           (indent 1))
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some-> (-when-let (,result ,x)
                  (-> ,result ,form))
                ,@more))))

(defmacro -some->> (x &optional form &rest more)
  "When expr is non-nil, thread it through the first form (via `->>'),
and when that result is non-nil, through the next form, etc."
  (declare (debug ->)
           (indent 1))
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some->> (-when-let (,result ,x)
                   (->> ,result ,form))
                 ,@more))))

(defmacro -some--> (x &optional form &rest more)
  "When expr in non-nil, thread it through the first form (via `-->'),
and when that result is non-nil, through the next form, etc."
  (declare (debug ->)
           (indent 1))
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some--> (-when-let (,result ,x)
                   (--> ,result ,form))
                 ,@more))))

(defun -grade-up (comparator list)
  "Grade elements of LIST using COMPARATOR relation, yielding a
permutation vector such that applying this permutation to LIST
sorts it in ascending order."
  ;; ugly hack to "fix" lack of lexical scope
  (let ((comp `(lambda (it other) (funcall ',comparator (car it) (car other)))))
    (->> (--map-indexed (cons it it-index) list)
         (-sort comp)
         (-map 'cdr))))

(defun -grade-down (comparator list)
  "Grade elements of LIST using COMPARATOR relation, yielding a
permutation vector such that applying this permutation to LIST
sorts it in descending order."
  ;; ugly hack to "fix" lack of lexical scope
  (let ((comp `(lambda (it other) (funcall ',comparator (car other) (car it)))))
    (->> (--map-indexed (cons it it-index) list)
         (-sort comp)
         (-map 'cdr))))

(defvar dash--source-counter 0
  "Monotonic counter for generated symbols.")

(defun dash--match-make-source-symbol ()
  "Generate a new dash-source symbol.

All returned symbols are guaranteed to be unique."
  (prog1 (make-symbol (format "--dash-source-%d--" dash--source-counter))
    (setq dash--source-counter (1+ dash--source-counter))))

(defun dash--match-ignore-place-p (symbol)
  "Return non-nil if SYMBOL is a symbol and starts with _."
  (and (symbolp symbol)
       (eq (aref (symbol-name symbol) 0) ?_)))

(defun dash--match-cons-skip-cdr (skip-cdr source)
  "Helper function generating idiomatic shifting code."
  (cond
   ((= skip-cdr 0)
    `(pop ,source))
   (t
    `(prog1 ,(dash--match-cons-get-car skip-cdr source)
       (setq ,source ,(dash--match-cons-get-cdr (1+ skip-cdr) source))))))

(defun dash--match-cons-get-car (skip-cdr source)
  "Helper function generating idiomatic code to get nth car."
  (cond
   ((= skip-cdr 0)
    `(car ,source))
   ((= skip-cdr 1)
    `(cadr ,source))
   (t
    `(nth ,skip-cdr ,source))))

(defun dash--match-cons-get-cdr (skip-cdr source)
  "Helper function generating idiomatic code to get nth cdr."
  (cond
   ((= skip-cdr 0)
    source)
   ((= skip-cdr 1)
    `(cdr ,source))
   (t
    `(nthcdr ,skip-cdr ,source))))

(defun dash--match-cons (match-form source)
  "Setup a cons matching environment and call the real matcher."
  (let ((s (dash--match-make-source-symbol))
        (n 0)
        (m match-form))
    (while (and (consp m)
                (dash--match-ignore-place-p (car m)))
      (setq n (1+ n)) (!cdr m))
    (cond
     ;; when we only have one pattern in the list, we don't have to
     ;; create a temporary binding (--dash-source--) for the source
     ;; and just use the input directly
     ((and (consp m)
           (not (cdr m)))
      (dash--match (car m) (dash--match-cons-get-car n source)))
     ;; handle other special types
     ((> n 0)
      (dash--match m (dash--match-cons-get-cdr n source)))
     ;; this is the only entry-point for dash--match-cons-1, that's
     ;; why we can't simply use the above branch, it would produce
     ;; infinite recursion
     (t
      (cons (list s source) (dash--match-cons-1 match-form s))))))

(defun dash--get-expand-function (type)
  "Get expand function name for TYPE."
  (intern-soft (format "dash-expand:%s" type)))

(defun dash--match-cons-1 (match-form source &optional props)
  "Match MATCH-FORM against SOURCE.

MATCH-FORM is a proper or improper list.  Each element of
MATCH-FORM is either a symbol, which gets bound to the respective
value in source or another match form which gets destructured
recursively.

If the cdr of last cons cell in the list is `nil', matching stops
there.

SOURCE is a proper or improper list."
  (let ((skip-cdr (or (plist-get props :skip-cdr) 0)))
    (cond
     ((consp match-form)
      (cond
       ((cdr match-form)
        (cond
         ((and (symbolp (car match-form))
               (functionp (dash--get-expand-function (car match-form))))
          (dash--match-kv (dash--match-kv-normalize-match-form match-form) (dash--match-cons-get-cdr skip-cdr source)))
         ((dash--match-ignore-place-p (car match-form))
          (dash--match-cons-1 (cdr match-form) source
                              (plist-put props :skip-cdr (1+ skip-cdr))))
         (t
          (-concat (dash--match (car match-form) (dash--match-cons-skip-cdr skip-cdr source))
                   (dash--match-cons-1 (cdr match-form) source)))))
       (t ;; Last matching place, no need for shift
        (dash--match (car match-form) (dash--match-cons-get-car skip-cdr source)))))
     ((eq match-form nil)
      nil)
     (t ;; Handle improper lists.  Last matching place, no need for shift
      (dash--match match-form (dash--match-cons-get-cdr skip-cdr source))))))

(defun dash--vector-tail (seq start)
  "Return the tail of SEQ starting at START."
  (cond
   ((vectorp seq)
    (let* ((re-length (- (length seq) start))
           (re (make-vector re-length 0)))
      (--dotimes re-length (aset re it (aref seq (+ it start))))
      re))
   ((stringp seq)
    (substring seq start))))

(defun dash--match-vector (match-form source)
  "Setup a vector matching environment and call the real matcher."
  (let ((s (dash--match-make-source-symbol)))
    (cond
     ;; don't bind `s' if we only have one sub-pattern
     ((= (length match-form) 1)
      (dash--match (aref match-form 0) `(aref ,source 0)))
     ;; if the source is a symbol, we don't need to re-bind it
     ((symbolp source)
      (dash--match-vector-1 match-form source))
     ;; don't bind `s' if we only have one sub-pattern which is not ignored
     ((let* ((ignored-places (mapcar 'dash--match-ignore-place-p match-form))
             (ignored-places-n (length (-remove 'null ignored-places))))
        (when (= ignored-places-n (1- (length match-form)))
          (let ((n (-find-index 'null ignored-places)))
            (dash--match (aref match-form n) `(aref ,source ,n))))))
     (t
      (cons (list s source) (dash--match-vector-1 match-form s))))))

(defun dash--match-vector-1 (match-form source)
  "Match MATCH-FORM against SOURCE.

MATCH-FORM is a vector.  Each element of MATCH-FORM is either a
symbol, which gets bound to the respective value in source or
another match form which gets destructured recursively.

If second-from-last place in MATCH-FORM is the symbol &rest, the
next element of the MATCH-FORM is matched against the tail of
SOURCE, starting at index of the &rest symbol.  This is
conceptually the same as the (head . tail) match for improper
lists, where dot plays the role of &rest.

SOURCE is a vector.

If the MATCH-FORM vector is shorter than SOURCE vector, only
the (length MATCH-FORM) places are bound, the rest of the SOURCE
is discarded."
  (let ((i 0)
        (l (length match-form))
        (re))
    (while (< i l)
      (let ((m (aref match-form i)))
        (push (cond
               ((and (symbolp m)
                     (eq m '&rest))
                (prog1 (dash--match
                        (aref match-form (1+ i))
                        `(dash--vector-tail ,source ,i))
                  (setq i l)))
               ((and (symbolp m)
                     ;; do not match symbols starting with _
                     (not (eq (aref (symbol-name m) 0) ?_)))
                (list (list m `(aref ,source ,i))))
               ((not (symbolp m))
                (dash--match m `(aref ,source ,i))))
              re)
        (setq i (1+ i))))
    (-flatten-n 1 (nreverse re))))

(defun dash--match-kv-normalize-match-form (pattern)
  "Normalize kv PATTERN.

This method normalizes PATTERN to the format expected by
`dash--match-kv'.  See `-let' for the specification."
  (let ((normalized (list (car pattern)))
        (skip nil)
        (fill-placeholder (make-symbol "--dash-fill-placeholder--")))
    (-each (apply '-zip (-pad fill-placeholder (cdr pattern) (cddr pattern)))
      (lambda (pair)
        (let ((current (car pair))
              (next (cdr pair)))
          (if skip
              (setq skip nil)
            (if (or (eq fill-placeholder next)
                    (not (or (and (symbolp next)
                                  (not (keywordp next))
                                  (not (eq next t))
                                  (not (eq next nil)))
                             (and (consp next)
                                  (not (eq (car next) 'quote)))
                             (vectorp next))))
                (progn
                  (cond
                   ((keywordp current)
                    (push current normalized)
                    (push (intern (substring (symbol-name current) 1)) normalized))
                   ((stringp current)
                    (push current normalized)
                    (push (intern current) normalized))
                   ((and (consp current)
                         (eq (car current) 'quote))
                    (push current normalized)
                    (push (cadr current) normalized))
                   (t (error "-let: found key `%s' in kv destructuring but its pattern `%s' is invalid and can not be derived from the key" current next)))
                  (setq skip nil))
              (push current normalized)
              (push next normalized)
              (setq skip t))))))
    (nreverse normalized)))

(defun dash--match-kv (match-form source)
  "Setup a kv matching environment and call the real matcher.

kv can be any key-value store, such as plist, alist or hash-table."
  (let ((s (dash--match-make-source-symbol)))
    (cond
     ;; don't bind `s' if we only have one sub-pattern (&type key val)
     ((= (length match-form) 3)
      (dash--match-kv-1 (cdr match-form) source (car match-form)))
     ;; if the source is a symbol, we don't need to re-bind it
     ((symbolp source)
      (dash--match-kv-1 (cdr match-form) source (car match-form)))
     (t
      (cons (list s source) (dash--match-kv-1 (cdr match-form) s (car match-form)))))))

(defun dash-expand:&hash (key source)
  "Generate extracting KEY from SOURCE for &hash destructuring."
  `(gethash ,key ,source))

(defun dash-expand:&plist (key source)
  "Generate extracting KEY from SOURCE for &plist destructuring."
  `(plist-get ,source ,key))

(defun dash-expand:&alist (key source)
  "Generate extracting KEY from SOURCE for &alist destructuring."
  `(cdr (assoc ,key ,source)))

(defun dash-expand:&hash? (key source)
  "Generate extracting KEY from SOURCE for &hash? destructuring.
Similar to &hash but check whether the map is not nil."
  (let ((src (make-symbol "src")))
    `(let ((,src ,source))
       (when ,src (gethash ,key ,src)))))

(defalias 'dash-expand:&keys 'dash-expand:&plist)

(defun dash--match-kv-1 (match-form source type)
  "Match MATCH-FORM against SOURCE of type TYPE.

MATCH-FORM is a proper list of the form (key1 place1 ... keyN
placeN).  Each placeK is either a symbol, which gets bound to the
value of keyK retrieved from the key-value store, or another
match form which gets destructured recursively.

SOURCE is a key-value store of type TYPE, which can be a plist,
an alist or a hash table.

TYPE is a token specifying the type of the key-value store.
Valid values are &plist, &alist and &hash."
  (-flatten-n 1 (-map
                 (lambda (kv)
                   (let* ((k (car kv))
                          (v (cadr kv))
                          (getter
                           (funcall (dash--get-expand-function type) k source)))
                     (cond
                      ((symbolp v)
                       (list (list v getter)))
                      (t (dash--match v getter)))))
                 (-partition 2 match-form))))

(defun dash--match-symbol (match-form source)
  "Bind a symbol.

This works just like `let', there is no destructuring."
  (list (list match-form source)))

(defun dash--match (match-form source)
  "Match MATCH-FORM against SOURCE.

This function tests the MATCH-FORM and dispatches to specific
matchers based on the type of the expression.

Key-value stores are disambiguated by placing a token &plist,
&alist or &hash as a first item in the MATCH-FORM."
  (cond
   ((symbolp match-form)
    (dash--match-symbol match-form source))
   ((consp match-form)
    (cond
     ;; Handle the "x &as" bindings first.
     ((and (consp (cdr match-form))
           (symbolp (car match-form))
           (eq '&as (cadr match-form)))
      (let ((s (car match-form)))
        (cons (list s source)
              (dash--match (cddr match-form) s))))
     ((functionp (dash--get-expand-function (car match-form)))
      (dash--match-kv (dash--match-kv-normalize-match-form match-form) source))
     (t (dash--match-cons match-form source))))
   ((vectorp match-form)
    ;; We support the &as binding in vectors too
    (cond
     ((and (> (length match-form) 2)
           (symbolp (aref match-form 0))
           (eq '&as (aref match-form 1)))
      (let ((s (aref match-form 0)))
        (cons (list s source)
              (dash--match (dash--vector-tail match-form 2) s))))
     (t (dash--match-vector match-form source))))))

(defun dash--normalize-let-varlist (varlist)
  "Normalize VARLIST so that every binding is a list.

`let' allows specifying a binding which is not a list but simply
the place which is then automatically bound to nil, such that all
three of the following are identical and evaluate to nil.

  (let (a) a)
  (let ((a)) a)
  (let ((a nil)) a)

This function normalizes all of these to the last form."
  (--map (if (consp it) it (list it nil)) varlist))

(defmacro -let* (varlist &rest body)
  "Bind variables according to VARLIST then eval BODY.

VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
PATTERN is matched against the SOURCE structurally.  SOURCE is
only evaluated once for each PATTERN.

Each SOURCE can refer to the symbols already bound by this
VARLIST.  This is useful if you want to destructure SOURCE
recursively but also want to name the intermediate structures.

See `-let' for the list of all possible patterns."
  (declare (debug ((&rest [&or (sexp form) sexp]) body))
           (indent 1))
  (let* ((varlist (dash--normalize-let-varlist varlist))
         (bindings (--mapcat (dash--match (car it) (cadr it)) varlist)))
    `(let* ,bindings
       ,@body)))

(defmacro -let (varlist &rest body)
  "Bind variables according to VARLIST then eval BODY.

VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
PATTERN is matched against the SOURCE \"structurally\".  SOURCE
is only evaluated once for each PATTERN.  Each PATTERN is matched
recursively, and can therefore contain sub-patterns which are
matched against corresponding sub-expressions of SOURCE.

All the SOURCEs are evalled before any symbols are
bound (i.e. \"in parallel\").

If VARLIST only contains one (PATTERN SOURCE) element, you can
optionally specify it using a vector and discarding the
outer-most parens.  Thus

  (-let ((PATTERN SOURCE)) ..)

becomes

  (-let [PATTERN SOURCE] ..).

`-let' uses a convention of not binding places (symbols) starting
with _ whenever it's possible.  You can use this to skip over
entries you don't care about.  However, this is not *always*
possible (as a result of implementation) and these symbols might
get bound to undefined values.

Following is the overview of supported patterns.  Remember that
patterns can be matched recursively, so every a, b, aK in the
following can be a matching construct and not necessarily a
symbol/variable.

Symbol:

  a - bind the SOURCE to A.  This is just like regular `let'.

Conses and lists:

  (a) - bind `car' of cons/list to A

  (a . b) - bind car of cons to A and `cdr' to B

  (a b) - bind car of list to A and `cadr' to B

  (a1 a2 a3  ...) - bind 0th car of list to A1, 1st to A2, 2nd to A3 ...

  (a1 a2 a3 ... aN . rest) - as above, but bind the Nth cdr to REST.

Vectors:

  [a] - bind 0th element of a non-list sequence to A (works with
        vectors, strings, bit arrays...)

  [a1 a2 a3 ...] - bind 0th element of non-list sequence to A0, 1st to
                   A1, 2nd to A2, ...
                   If the PATTERN is shorter than SOURCE, the values at
                   places not in PATTERN are ignored.
                   If the PATTERN is longer than SOURCE, an `error' is
                   thrown.

  [a1 a2 a3 ... &rest rest] - as above, but bind the rest of
                              the sequence to REST.  This is
                              conceptually the same as improper list
                              matching (a1 a2 ... aN . rest)

Key/value stores:

  (&plist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                 SOURCE plist to aK.  If the
                                 value is not found, aK is nil.
                                 Uses `plist-get' to fetch values.

  (&alist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                 SOURCE alist to aK.  If the
                                 value is not found, aK is nil.
                                 Uses `assoc' to fetch values.

  (&hash key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                SOURCE hash table to aK.  If the
                                value is not found, aK is nil.
                                Uses `gethash' to fetch values.

Further, special keyword &keys supports \"inline\" matching of
plist-like key-value pairs, similarly to &keys keyword of
`cl-defun'.

  (a1 a2 ... aN &keys key1 b1 ... keyN bK)

This binds N values from the list to a1 ... aN, then interprets
the cdr as a plist (see key/value matching above).

A shorthand notation for kv-destructuring exists which allows the
patterns be optionally left out and derived from the key name in
the following fashion:

- a key :foo is converted into `foo' pattern,
- a key 'bar is converted into `bar' pattern,
- a key \"baz\" is converted into `baz' pattern.

That is, the entire value under the key is bound to the derived
variable without any further destructuring.

This is possible only when the form following the key is not a
valid pattern (i.e. not a symbol, a cons cell or a vector).
Otherwise the matching proceeds as usual and in case of an
invalid spec fails with an error.

Thus the patterns are normalized as follows:

   ;; derive all the missing patterns
   (&plist :foo 'bar \"baz\") => (&plist :foo foo 'bar bar \"baz\" baz)

   ;; we can specify some but not others
   (&plist :foo 'bar explicit-bar) => (&plist :foo foo 'bar explicit-bar)

   ;; nothing happens, we store :foo in x
   (&plist :foo x) => (&plist :foo x)

   ;; nothing happens, we match recursively
   (&plist :foo (a b c)) => (&plist :foo (a b c))

You can name the source using the syntax SYMBOL &as PATTERN.
This syntax works with lists (proper or improper), vectors and
all types of maps.

  (list &as a b c) (list 1 2 3)

binds A to 1, B to 2, C to 3 and LIST to (1 2 3).

Similarly:

  (bounds &as beg . end) (cons 1 2)

binds BEG to 1, END to 2 and BOUNDS to (1 . 2).

  (items &as first . rest) (list 1 2 3)

binds FIRST to 1, REST to (2 3) and ITEMS to (1 2 3)

  [vect &as _ b c] [1 2 3]

binds B to 2, C to 3 and VECT to [1 2 3] (_ avoids binding as usual).

  (plist &as &plist :b b) (list :a 1 :b 2 :c 3)

binds B to 2 and PLIST to (:a 1 :b 2 :c 3).  Same for &alist and &hash.

This is especially useful when we want to capture the result of a
computation and destructure at the same time.  Consider the
form (function-returning-complex-structure) returning a list of
two vectors with two items each.  We want to capture this entire
result and pass it to another computation, but at the same time
we want to get the second item from each vector.  We can achieve
it with pattern

  (result &as [_ a] [_ b]) (function-returning-complex-structure)

Note: Clojure programmers may know this feature as the \":as
binding\".  The difference is that we put the &as at the front
because we need to support improper list binding."
  (declare (debug ([&or (&rest [&or (sexp form) sexp])
                        (vector [&rest [sexp form]])]
                   body))
           (indent 1))
  (if (vectorp varlist)
      `(let* ,(dash--match (aref varlist 0) (aref varlist 1))
         ,@body)
    (let* ((varlist (dash--normalize-let-varlist varlist))
           (inputs (--map-indexed (list (make-symbol (format "input%d" it-index)) (cadr it)) varlist))
           (new-varlist (--map (list (caar it) (cadr it)) (-zip varlist inputs))))
      `(let ,inputs
         (-let* ,new-varlist ,@body)))))

(defmacro -lambda (match-form &rest body)
  "Return a lambda which destructures its input as MATCH-FORM and executes BODY.

Note that you have to enclose the MATCH-FORM in a pair of parens,
such that:

  (-lambda (x) body)
  (-lambda (x y ...) body)

has the usual semantics of `lambda'.  Furthermore, these get
translated into normal lambda, so there is no performance
penalty.

See `-let' for the description of destructuring mechanism."
  (declare (doc-string 2) (indent defun)
           (debug (&define sexp
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (cond
   ((not (consp match-form))
    (signal 'wrong-type-argument "match-form must be a list"))
   ;; no destructuring, so just return regular lambda to make things faster
   ((-all? 'symbolp match-form)
    `(lambda ,match-form ,@body))
   (t
    (let* ((inputs (--map-indexed (list it (make-symbol (format "input%d" it-index))) match-form)))
      ;; TODO: because inputs to the lambda are evaluated only once,
      ;; -let* need not to create the extra bindings to ensure that.
      ;; We should find a way to optimize that.  Not critical however.
      `(lambda ,(--map (cadr it) inputs)
         (-let* ,inputs ,@body))))))

(defmacro -setq (&rest forms)
  "Bind each MATCH-FORM to the value of its VAL.

MATCH-FORM destructuring is done according to the rules of `-let'.

This macro allows you to bind multiple variables by destructuring
the value, so for example:

  (-setq (a b) x
         (&plist :c c) plist)

expands roughly speaking to the following code

  (setq a (car x)
        b (cadr x)
        c (plist-get plist :c))

Care is taken to only evaluate each VAL once so that in case of
multiple assignments it does not cause unexpected side effects.

\(fn [MATCH-FORM VAL]...)"
  (declare (debug (&rest sexp form))
           (indent 1))
  (when (= (mod (length forms) 2) 1)
    (error "Odd number of arguments"))
  (let* ((forms-and-sources
          ;; First get all the necessary mappings with all the
          ;; intermediate bindings.
          (-map (lambda (x) (dash--match (car x) (cadr x)))
                (-partition 2 forms)))
         ;; To preserve the logic of dynamic scoping we must ensure
         ;; that we `setq' the variables outside of the `let*' form
         ;; which holds the destructured intermediate values.  For
         ;; this we generate for each variable a placeholder which is
         ;; bound to (lexically) the result of the destructuring.
         ;; Then outside of the helper `let*' form we bind all the
         ;; original variables to their respective placeholders.
         ;; TODO: There is a lot of room for possible optimization,
         ;; for start playing with `special-variable-p' to eliminate
         ;; unnecessary re-binding.
         (variables-to-placeholders
          (-mapcat
           (lambda (bindings)
             (-map
              (lambda (binding)
                (let ((var (car binding)))
                  (list var (make-symbol (concat "--dash-binding-" (symbol-name var) "--")))))
              (--filter (not (string-prefix-p "--" (symbol-name (car it)))) bindings)))
           forms-and-sources)))
    `(let ,(-map 'cadr variables-to-placeholders)
       (let* ,(-flatten-n 1 forms-and-sources)
         (setq ,@(-flatten (-map 'reverse variables-to-placeholders))))
       (setq ,@(-flatten variables-to-placeholders)))))

(defmacro -if-let* (vars-vals then &rest else)
  "If all VALS evaluate to true, bind them to their corresponding
VARS and do THEN, otherwise do ELSE. VARS-VALS should be a list
of (VAR VAL) pairs.

Note: binding is done according to `-let*'.  VALS are evaluated
sequentially, and evaluation stops after the first nil VAL is
encountered."
  (declare (debug ((&rest (sexp form)) form body))
           (indent 2))
  (->> vars-vals
       (--mapcat (dash--match (car it) (cadr it)))
       (--reduce-r-from
        (let ((var (car it))
              (val (cadr it)))
          `(let ((,var ,val))
             (if ,var ,acc ,@else)))
        then)))

(defmacro -if-let (var-val then &rest else)
  "If VAL evaluates to non-nil, bind it to VAR and do THEN,
otherwise do ELSE.

Note: binding is done according to `-let'.

\(fn (VAR VAL) THEN &rest ELSE)"
  (declare (debug ((sexp form) form body))
           (indent 2))
  `(-if-let* (,var-val) ,then ,@else))

(defmacro --if-let (val then &rest else)
  "If VAL evaluates to non-nil, bind it to symbol `it' and do THEN,
otherwise do ELSE."
  (declare (debug (form form body))
           (indent 2))
  `(-if-let (it ,val) ,then ,@else))

(defmacro -when-let* (vars-vals &rest body)
  "If all VALS evaluate to true, bind them to their corresponding
VARS and execute body. VARS-VALS should be a list of (VAR VAL)
pairs.

Note: binding is done according to `-let*'.  VALS are evaluated
sequentially, and evaluation stops after the first nil VAL is
encountered."
  (declare (debug ((&rest (sexp form)) body))
           (indent 1))
  `(-if-let* ,vars-vals (progn ,@body)))

(defmacro -when-let (var-val &rest body)
  "If VAL evaluates to non-nil, bind it to VAR and execute body.

Note: binding is done according to `-let'.

\(fn (VAR VAL) &rest BODY)"
  (declare (debug ((sexp form) body))
           (indent 1))
  `(-if-let ,var-val (progn ,@body)))

(defmacro --when-let (val &rest body)
  "If VAL evaluates to non-nil, bind it to symbol `it' and
execute body."
  (declare (debug (form body))
           (indent 1))
  `(--if-let ,val (progn ,@body)))

(defvar -compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:

  (let ((-compare-fn #\\='=)) (-union numbers1 numbers2 numbers3)")

(defun -distinct (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil.

Alias: `-uniq'"
  ;; Implementation note: The speedup gained from hash table lookup
  ;; starts to outweigh its overhead for lists of length greater than
  ;; 32.  See discussion in PR #305.
  (let* ((len (length list))
         (lut (and (> len 32)
                   ;; Check that `-compare-fn' is a valid hash-table
                   ;; lookup function or `nil'.
                   (memq -compare-fn '(nil equal eq eql))
                   (make-hash-table :test (or -compare-fn #'equal)
                                    :size len))))
    (if lut
        (--filter (unless (gethash it lut)
                    (puthash it t lut))
                  list)
      (--each list (unless (-contains? lut it) (!cons it lut)))
      (nreverse lut))))

(defalias '-uniq '-distinct)

(defun -union (list list2)
  "Return a new list containing the elements of LIST and elements of LIST2 that are not in LIST.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  ;; We fall back to iteration implementation if the comparison
  ;; function isn't one of `eq', `eql' or `equal'.
  (let* ((result (reverse list))
         ;; TODO: get rid of this dynamic variable, pass it as an
         ;; argument instead.
         (-compare-fn (if (bound-and-true-p -compare-fn)
                          -compare-fn
                        'equal)))
    (if (memq -compare-fn '(eq eql equal))
        (let ((ht (make-hash-table :test -compare-fn)))
          (--each list (puthash it t ht))
          (--each list2 (unless (gethash it ht) (!cons it result))))
      (--each list2 (unless (-contains? result it) (!cons it result))))
    (nreverse result)))

(defun -intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (-contains? list2 it) list))

(defun -difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (not (-contains? list2 it)) list))

(defun -powerset (list)
  "Return the power set of LIST."
  (if (null list) '(())
    (let ((last (-powerset (cdr list))))
      (append (mapcar (lambda (x) (cons (car list) x)) last)
              last))))

(defun -permutations (list)
  "Return the permutations of LIST."
  (if (null list) '(())
    (apply #'append
           (mapcar (lambda (x)
                     (mapcar (lambda (perm) (cons x perm))
                             (-permutations (remove x list))))
                   list))))

(defun -inits (list)
  "Return all prefixes of LIST."
  (let ((res (list list)))
    (setq list (reverse list))
    (while list
      (push (reverse (!cdr list)) res))
    res))

(defun -tails (list)
  "Return all suffixes of LIST"
  (-reductions-r-from 'cons nil list))

(defun -common-prefix (&rest lists)
  "Return the longest common prefix of LISTS."
  (declare (pure t) (side-effect-free t))
  (--reduce (--take-while (and acc (equal (pop acc) it)) it)
            lists))

(defun -common-suffix (&rest lists)
  "Return the longest common suffix of LISTS."
  (nreverse (apply #'-common-prefix (mapcar #'reverse lists))))

(defun -contains? (list element)
  "Return non-nil if LIST contains ELEMENT.

The test for equality is done with `equal', or with `-compare-fn'
if that's non-nil.

Alias: `-contains-p'"
  (not
   (null
    (cond
     ((null -compare-fn)    (member element list))
     ((eq -compare-fn 'eq)  (memq element list))
     ((eq -compare-fn 'eql) (memql element list))
     (t
      (let ((lst list))
        (while (and lst
                    (not (funcall -compare-fn element (car lst))))
          (setq lst (cdr lst)))
        lst))))))

(defalias '-contains-p '-contains?)

(defun -same-items? (list list2)
  "Return true if LIST and LIST2 has the same items.

The order of the elements in the lists does not matter.

Alias: `-same-items-p'"
  (let ((length-a (length list))
        (length-b (length list2)))
    (and
     (= length-a length-b)
     (= length-a (length (-intersection list list2))))))

(defalias '-same-items-p '-same-items?)

(defun -is-prefix? (prefix list)
  "Return non-nil if PREFIX is prefix of LIST.

Alias: `-is-prefix-p'"
  (declare (pure t) (side-effect-free t))
  (--each-while list (equal (car prefix) it)
    (!cdr prefix))
  (not prefix))

(defun -is-suffix? (suffix list)
  "Return non-nil if SUFFIX is suffix of LIST.

Alias: `-is-suffix-p'"
  (declare (pure t) (side-effect-free t))
  (-is-prefix? (reverse suffix) (reverse list)))

(defun -is-infix? (infix list)
  "Return non-nil if INFIX is infix of LIST.

This operation runs in O(n^2) time

Alias: `-is-infix-p'"
  (declare (pure t) (side-effect-free t))
  (let (done)
    (while (and (not done) list)
      (setq done (-is-prefix? infix list))
      (!cdr list))
    done))

(defalias '-is-prefix-p '-is-prefix?)
(defalias '-is-suffix-p '-is-suffix?)
(defalias '-is-infix-p '-is-infix?)

(defun -sort (comparator list)
  "Sort LIST, stably, comparing elements using COMPARATOR.
Return the sorted list.  LIST is NOT modified by side effects.
COMPARATOR is called with two elements of LIST, and should return non-nil
if the first element should sort before the second."
  (sort (copy-sequence list) comparator))

(defmacro --sort (form list)
  "Anaphoric form of `-sort'."
  (declare (debug (form form)))
  `(-sort (lambda (it other) ,form) ,list))

(defun -list (&rest args)
  "Return a list with ARGS.

If first item of ARGS is already a list, simply return ARGS.  If
not, return a list with ARGS as elements."
  (declare (pure t) (side-effect-free t))
  (let ((arg (car args)))
    (if (listp arg) arg args)))

(defun -repeat (n x)
  "Return a list with X repeated N times.
Return nil if N is less than 1."
  (declare (pure t) (side-effect-free t))
  (let (ret)
    (--dotimes n (!cons x ret))
    ret))

(defun -sum (list)
  "Return the sum of LIST."
  (declare (pure t) (side-effect-free t))
  (apply '+ list))

(defun -running-sum (list)
  "Return a list with running sums of items in LIST.

LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (unless (consp list)
    (error "LIST must be non-empty"))
  (-reductions '+ list))

(defun -product (list)
  "Return the product of LIST."
  (declare (pure t) (side-effect-free t))
  (apply '* list))

(defun -running-product (list)
  "Return a list with running products of items in LIST.

LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (unless (consp list)
    (error "LIST must be non-empty"))
  (-reductions '* list))

(defun -max (list)
  "Return the largest value from LIST of numbers or markers."
  (declare (pure t) (side-effect-free t))
  (apply 'max list))

(defun -min (list)
  "Return the smallest value from LIST of numbers or markers."
  (declare (pure t) (side-effect-free t))
  (apply 'min list))

(defun -max-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the greatest element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (--reduce (if (funcall comparator it acc) it acc) list))

(defun -min-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the least element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (--reduce (if (funcall comparator it acc) acc it) list))

(defmacro --max-by (form list)
  "Anaphoric version of `-max-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (form form)))
  `(-max-by (lambda (it other) ,form) ,list))

(defmacro --min-by (form list)
  "Anaphoric version of `-min-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (form form)))
  `(-min-by (lambda (it other) ,form) ,list))

(defun -iterate (fun init n)
  "Return a list of iterated applications of FUN to INIT.

This means a list of form:

  (init (fun init) (fun (fun init)) ...)

N is the length of the returned list."
  (if (= n 0) nil
    (let ((r (list init)))
      (--dotimes (1- n)
        (push (funcall fun (car r)) r))
      (nreverse r))))

(defun -fix (fn list)
  "Compute the (least) fixpoint of FN with initial input LIST.

FN is called at least once, results are compared with `equal'."
  (let ((re (funcall fn list)))
    (while (not (equal list re))
      (setq list re)
      (setq re (funcall fn re)))
    re))

(defmacro --fix (form list)
  "Anaphoric form of `-fix'."
  `(-fix (lambda (it) ,form) ,list))

(defun -unfold (fun seed)
  "Build a list from SEED using FUN.

This is \"dual\" operation to `-reduce-r': while -reduce-r
consumes a list to produce a single value, `-unfold' takes a
seed value and builds a (potentially infinite!) list.

FUN should return `nil' to stop the generating process, or a
cons (A . B), where A will be prepended to the result and B is
the new seed."
  (let ((last (funcall fun seed)) r)
    (while last
      (push (car last) r)
      (setq last (funcall fun (cdr last))))
    (nreverse r)))

(defmacro --unfold (form seed)
  "Anaphoric version of `-unfold'."
  (declare (debug (form form)))
  `(-unfold (lambda (it) ,form) ,seed))

(defun -cons-pair? (con)
  "Return non-nil if CON is true cons pair.
That is (A . B) where B is not a list.

Alias: `-cons-pair-p'"
  (declare (pure t) (side-effect-free t))
  (and (listp con)
       (not (listp (cdr con)))))

(defalias '-cons-pair-p '-cons-pair?)

(defun -cons-to-list (con)
  "Convert a cons pair to a list with `car' and `cdr' of the pair respectively."
  (declare (pure t) (side-effect-free t))
  (list (car con) (cdr con)))

(defun -value-to-list (val)
  "Convert a value to a list.

If the value is a cons pair, make a list with two elements, `car'
and `cdr' of the pair respectively.

If the value is anything else, wrap it in a list."
  (declare (pure t) (side-effect-free t))
  (cond
   ((-cons-pair? val) (-cons-to-list val))
   (t (list val))))

(defun -tree-mapreduce-from (fn folder init-value tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce-from' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (-reduce-r-from folder init-value (mapcar (lambda (x) (-tree-mapreduce-from fn folder init-value x)) tree)))
   (t (funcall fn tree))))

(defmacro --tree-mapreduce-from (form folder init-value tree)
  "Anaphoric form of `-tree-mapreduce-from'."
  (declare (debug (form form form form)))
  `(-tree-mapreduce-from (lambda (it) ,form) (lambda (it acc) ,folder) ,init-value ,tree))

(defun -tree-mapreduce (fn folder tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (-reduce-r folder (mapcar (lambda (x) (-tree-mapreduce fn folder x)) tree)))
   (t (funcall fn tree))))

(defmacro --tree-mapreduce (form folder tree)
  "Anaphoric form of `-tree-mapreduce'."
  (declare (debug (form form form)))
  `(-tree-mapreduce (lambda (it) ,form) (lambda (it acc) ,folder) ,tree))

(defun -tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) (funcall fn tree))
   ((listp tree)
    (mapcar (lambda (x) (-tree-map fn x)) tree))
   (t (funcall fn tree))))

(defmacro --tree-map (form tree)
  "Anaphoric form of `-tree-map'."
  (declare (debug (form form)))
  `(-tree-map (lambda (it) ,form) ,tree))

(defun -tree-reduce-from (fn init-value tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to INIT-VALUE and first element of the list,
then on this result and second element from the list etc.

The initial value is ignored on cons pairs as they always contain
two elements."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) tree)
   ((listp tree)
    (-reduce-r-from fn init-value (mapcar (lambda (x) (-tree-reduce-from fn init-value x)) tree)))
   (t tree)))

(defmacro --tree-reduce-from (form init-value tree)
  "Anaphoric form of `-tree-reduce-from'."
  (declare (debug (form form form)))
  `(-tree-reduce-from (lambda (it acc) ,form) ,init-value ,tree))

(defun -tree-reduce (fn tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to first element of the list and second
element, then on this result and third element from the list etc.

See `-reduce-r' for how exactly are lists of zero or one element handled."
  (cond
   ((not tree) nil)
   ((-cons-pair? tree) tree)
   ((listp tree)
    (-reduce-r fn (mapcar (lambda (x) (-tree-reduce fn x)) tree)))
   (t tree)))

(defmacro --tree-reduce (form tree)
  "Anaphoric form of `-tree-reduce'."
  (declare (debug (form form)))
  `(-tree-reduce (lambda (it acc) ,form) ,tree))

(defun -tree-map-nodes (pred fun tree)
  "Call FUN on each node of TREE that satisfies PRED.

If PRED returns nil, continue descending down this node.  If PRED
returns non-nil, apply FUN to this node and do not descend
further."
  (if (funcall pred tree)
      (funcall fun tree)
    (if (and (listp tree)
             (not (-cons-pair? tree)))
        (-map (lambda (x) (-tree-map-nodes pred fun x)) tree)
      tree)))

(defmacro --tree-map-nodes (pred form tree)
  "Anaphoric form of `-tree-map-nodes'."
  `(-tree-map-nodes (lambda (it) ,pred) (lambda (it) ,form) ,tree))

(defun -tree-seq (branch children tree)
  "Return a sequence of the nodes in TREE, in depth-first search order.

BRANCH is a predicate of one argument that returns non-nil if the
passed argument is a branch, that is, a node that can have children.

CHILDREN is a function of one argument that returns the children
of the passed branch node.

Non-branch nodes are simply copied."
  (cons tree
        (when (funcall branch tree)
          (-mapcat (lambda (x) (-tree-seq branch children x))
                   (funcall children tree)))))

(defmacro --tree-seq (branch children tree)
  "Anaphoric form of `-tree-seq'."
  `(-tree-seq (lambda (it) ,branch) (lambda (it) ,children) ,tree))

(defun -clone (list)
  "Create a deep copy of LIST.
The new list has the same elements and structure but all cons are
replaced with new ones.  This is useful when you need to clone a
structure such as plist or alist."
  (declare (pure t) (side-effect-free t))
  (-tree-map 'identity list))

(defun dash-enable-font-lock ()
  "Add syntax highlighting to dash functions, macros and magic values."
  (eval-after-load 'lisp-mode
    '(progn
       (let ((new-keywords '(
                             "!cons"
                             "!cdr"
                             "-each"
                             "--each"
                             "-each-indexed"
                             "--each-indexed"
                             "-each-while"
                             "--each-while"
                             "-doto"
                             "-dotimes"
                             "--dotimes"
                             "-map"
                             "--map"
                             "-reduce-from"
                             "--reduce-from"
                             "-reduce"
                             "--reduce"
                             "-reduce-r-from"
                             "--reduce-r-from"
                             "-reduce-r"
                             "--reduce-r"
                             "-reductions-from"
                             "-reductions-r-from"
                             "-reductions"
                             "-reductions-r"
                             "-filter"
                             "--filter"
                             "-select"
                             "--select"
                             "-remove"
                             "--remove"
                             "-reject"
                             "--reject"
                             "-remove-first"
                             "--remove-first"
                             "-reject-first"
                             "--reject-first"
                             "-remove-last"
                             "--remove-last"
                             "-reject-last"
                             "--reject-last"
                             "-remove-item"
                             "-non-nil"
                             "-keep"
                             "--keep"
                             "-map-indexed"
                             "--map-indexed"
                             "-splice"
                             "--splice"
                             "-splice-list"
                             "--splice-list"
                             "-map-when"
                             "--map-when"
                             "-replace-where"
                             "--replace-where"
                             "-map-first"
                             "--map-first"
                             "-map-last"
                             "--map-last"
                             "-replace"
                             "-replace-first"
                             "-replace-last"
                             "-flatten"
                             "-flatten-n"
                             "-concat"
                             "-mapcat"
                             "--mapcat"
                             "-copy"
                             "-cons*"
                             "-snoc"
                             "-first"
                             "--first"
                             "-find"
                             "--find"
                             "-some"
                             "--some"
                             "-any"
                             "--any"
                             "-last"
                             "--last"
                             "-first-item"
                             "-second-item"
                             "-third-item"
                             "-fourth-item"
                             "-fifth-item"
                             "-last-item"
                             "-butlast"
                             "-count"
                             "--count"
                             "-any?"
                             "--any?"
                             "-some?"
                             "--some?"
                             "-any-p"
                             "--any-p"
                             "-some-p"
                             "--some-p"
                             "-some->"
                             "-some->>"
                             "-some-->"
                             "-all?"
                             "-all-p"
                             "--all?"
                             "--all-p"
                             "-every?"
                             "--every?"
                             "-all-p"
                             "--all-p"
                             "-every-p"
                             "--every-p"
                             "-none?"
                             "--none?"
                             "-none-p"
                             "--none-p"
                             "-only-some?"
                             "--only-some?"
                             "-only-some-p"
                             "--only-some-p"
                             "-slice"
                             "-take"
                             "-drop"
                             "-drop-last"
                             "-take-last"
                             "-take-while"
                             "--take-while"
                             "-drop-while"
                             "--drop-while"
                             "-split-at"
                             "-rotate"
                             "-insert-at"
                             "-replace-at"
                             "-update-at"
                             "--update-at"
                             "-remove-at"
                             "-remove-at-indices"
                             "-split-with"
                             "--split-with"
                             "-split-on"
                             "-split-when"
                             "--split-when"
                             "-separate"
                             "--separate"
                             "-partition-all-in-steps"
                             "-partition-in-steps"
                             "-partition-all"
                             "-partition"
                             "-partition-after-item"
                             "-partition-after-pred"
                             "-partition-before-item"
                             "-partition-before-pred"
                             "-partition-by"
                             "--partition-by"
                             "-partition-by-header"
                             "--partition-by-header"
                             "-group-by"
                             "--group-by"
                             "-interpose"
                             "-interleave"
                             "-unzip"
                             "-zip-with"
                             "--zip-with"
                             "-zip"
                             "-zip-fill"
                             "-zip-lists"
                             "-zip-pair"
                             "-cycle"
                             "-pad"
                             "-annotate"
                             "--annotate"
                             "-table"
                             "-table-flat"
                             "-partial"
                             "-elem-index"
                             "-elem-indices"
                             "-find-indices"
                             "--find-indices"
                             "-find-index"
                             "--find-index"
                             "-find-last-index"
                             "--find-last-index"
                             "-select-by-indices"
                             "-select-columns"
                             "-select-column"
                             "-grade-up"
                             "-grade-down"
                             "->"
                             "->>"
                             "-->"
                             "-as->"
                             "-when-let"
                             "-when-let*"
                             "--when-let"
                             "-if-let"
                             "-if-let*"
                             "--if-let"
                             "-let*"
                             "-let"
                             "-lambda"
                             "-distinct"
                             "-uniq"
                             "-union"
                             "-intersection"
                             "-difference"
                             "-powerset"
                             "-permutations"
                             "-inits"
                             "-tails"
                             "-common-prefix"
                             "-common-suffix"
                             "-contains?"
                             "-contains-p"
                             "-same-items?"
                             "-same-items-p"
                             "-is-prefix-p"
                             "-is-prefix?"
                             "-is-suffix-p"
                             "-is-suffix?"
                             "-is-infix-p"
                             "-is-infix?"
                             "-sort"
                             "--sort"
                             "-list"
                             "-repeat"
                             "-sum"
                             "-running-sum"
                             "-product"
                             "-running-product"
                             "-max"
                             "-min"
                             "-max-by"
                             "--max-by"
                             "-min-by"
                             "--min-by"
                             "-iterate"
                             "--iterate"
                             "-fix"
                             "--fix"
                             "-unfold"
                             "--unfold"
                             "-cons-pair?"
                             "-cons-pair-p"
                             "-cons-to-list"
                             "-value-to-list"
                             "-tree-mapreduce-from"
                             "--tree-mapreduce-from"
                             "-tree-mapreduce"
                             "--tree-mapreduce"
                             "-tree-map"
                             "--tree-map"
                             "-tree-reduce-from"
                             "--tree-reduce-from"
                             "-tree-reduce"
                             "--tree-reduce"
                             "-tree-seq"
                             "--tree-seq"
                             "-tree-map-nodes"
                             "--tree-map-nodes"
                             "-clone"
                             "-rpartial"
                             "-juxt"
                             "-applify"
                             "-on"
                             "-flip"
                             "-const"
                             "-cut"
                             "-orfn"
                             "-andfn"
                             "-iteratefn"
                             "-fixfn"
                             "-prodfn"
                             ))
             (special-variables '(
                                  "it"
                                  "it-index"
                                  "acc"
                                  "other"
                                  )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\_<" (regexp-opt special-variables 'paren) "\\_>")
                                                     1 font-lock-variable-name-face)) 'append)
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\_>")
                                                     1 font-lock-keyword-face)) 'append))
       (--each (buffer-list)
         (with-current-buffer it
           (when (and (eq major-mode 'emacs-lisp-mode)
                      (boundp 'font-lock-mode)
                      font-lock-mode)
             (font-lock-refresh-defaults)))))))

(provide 'dash)
;;; dash.el ends here
