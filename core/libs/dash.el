;;; dash.el --- A modern list library for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 2.17.0
;; Package-Requires: ((emacs "24"))
;; Keywords: extensions, lisp
;; Homepage: https://github.com/magnars/dash.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A modern list API for Emacs.
;;
;; See its overview at https://github.com/magnars/dash.el#functions.

;;; Code:

;; TODO: `gv' was introduced in Emacs 24.3, so remove this and all
;; calls to `defsetf' when support for earlier versions is dropped.
(eval-when-compile
  (unless (fboundp 'gv-define-setter)
    (require 'cl)))

(defgroup dash ()
  "Customize group for Dash, a modern list library."
  :group 'extensions
  :group 'lisp
  :prefix "dash-")

(defmacro !cons (car cdr)
  "Destructive: Set CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Set LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro --each (list &rest body)
  "Evaluate BODY for each element of LIST and return nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating BODY.
This is the anaphoric counterpart to `-each'."
  (declare (debug (form body)) (indent 1))
  (let ((l (make-symbol "list"))
        (i (make-symbol "i")))
    `(let ((,l ,list)
           (,i 0)
           it it-index)
       (ignore it it-index)
       (while ,l
         (setq it (pop ,l) it-index ,i ,i (1+ ,i))
         ,@body))))

(defun -each (list fn)
  "Call FN on each element of LIST.
Return nil; this function is intended for side effects.
Its anaphoric counterpart is `--each'.  For access to the current
element's index in LIST, see `-each-indexed'."
  (declare (indent 1))
  (ignore (mapc fn list)))

(defalias '--each-indexed '--each)

(defun -each-indexed (list fn)
  "Call FN on each index and element of LIST.
For each ITEM at INDEX in LIST, call (funcall FN INDEX ITEM).
Return nil; this function is intended for side effects.
See also: `-map-indexed'."
  (declare (indent 1))
  (--each list (funcall fn it-index it)))

(defmacro --each-while (list pred &rest body)
  "Evaluate BODY for each item in LIST, while PRED evaluates to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating PRED or BODY.  Once
an element is reached for which PRED evaluates to nil, no further
BODY is evaluated.  The return value is always nil.
This is the anaphoric counterpart to `-each-while'."
  (declare (debug (form form body)) (indent 2))
  (let ((l (make-symbol "list"))
        (i (make-symbol "i"))
        (elt (make-symbol "elt")))
    `(let ((,l ,list)
           (,i 0)
           ,elt it it-index)
       (ignore it it-index)
       (while (and ,l (setq ,elt (pop ,l) it ,elt it-index ,i) ,pred)
         (setq it ,elt it-index ,i ,i (1+ ,i))
         ,@body))))

(defun -each-while (list pred fn)
  "Call FN on each ITEM in LIST, while (PRED ITEM) is non-nil.
Once an ITEM is reached for which PRED returns nil, FN is no
longer called.  Return nil; this function is intended for side
effects.
Its anaphoric counterpart is `--each-while'."
  (declare (indent 2))
  (--each-while list (funcall pred it) (funcall fn it)))

(defmacro --each-r (list &rest body)
  "Evaluate BODY for each element of LIST in reversed order.
Each element of LIST in turn, starting at its end, is bound to
`it' and its index within LIST to `it-index' before evaluating
BODY.  The return value is always nil.
This is the anaphoric counterpart to `-each-r'."
  (declare (debug (form body)) (indent 1))
  (let ((v (make-symbol "vector"))
        (i (make-symbol "i")))
    ;; Implementation note: building a vector is considerably faster
    ;; than building a reversed list (vector takes less memory, so
    ;; there is less GC), plus `length' comes naturally.  In-place
    ;; `nreverse' would be faster still, but BODY would be able to see
    ;; that, even if the modification was undone before we return.
    `(let* ((,v (vconcat ,list))
            (,i (length ,v))
            it it-index)
       (ignore it it-index)
       (while (> ,i 0)
         (setq ,i (1- ,i) it-index ,i it (aref ,v ,i))
         ,@body))))

(defun -each-r (list fn)
  "Call FN on each element of LIST in reversed order.
Return nil; this function is intended for side effects.
Its anaphoric counterpart is `--each-r'."
  (--each-r list (funcall fn it)))

(defmacro --each-r-while (list pred &rest body)
  "Eval BODY for each item in reversed LIST, while PRED evals to non-nil.
Each element of LIST in turn, starting at its end, is bound to
`it' and its index within LIST to `it-index' before evaluating
PRED or BODY.  Once an element is reached for which PRED
evaluates to nil, no further BODY is evaluated.  The return value
is always nil.
This is the anaphoric counterpart to `-each-r-while'."
  (declare (debug (form form body)) (indent 2))
  (let ((v (make-symbol "vector"))
        (i (make-symbol "i"))
        (elt (make-symbol "elt")))
    `(let* ((,v (vconcat ,list))
            (,i (length ,v))
            ,elt it it-index)
       (ignore it it-index)
       (while (when (> ,i 0)
                (setq ,i (1- ,i) it-index ,i)
                (setq ,elt (aref ,v ,i) it ,elt)
                ,pred)
         (setq it-index ,i it ,elt)
         ,@body))))

(defun -each-r-while (list pred fn)
  "Call FN on each ITEM in reversed LIST, while (PRED ITEM) is non-nil.
Once an ITEM is reached for which PRED returns nil, FN is no
longer called.  Return nil; this function is intended for side
effects.
Its anaphoric counterpart is `--each-r-while'."
  (--each-r-while list (funcall pred it) (funcall fn it)))

(defmacro --dotimes (num &rest body)
  "Evaluate BODY NUM times, presumably for side effects.
BODY is evaluated with the local variable `it' temporarily bound
to successive integers running from 0, inclusive, to NUM,
exclusive.  BODY is not evaluated if NUM is less than 1.
This is the anaphoric counterpart to `-dotimes'."
  (declare (debug (form body)) (indent 1))
  (let ((n (make-symbol "num"))
        (i (make-symbol "i")))
    `(let ((,n ,num)
           (,i 0)
           it)
       (ignore it)
       (while (< ,i ,n)
         (setq it ,i ,i (1+ ,i))
         ,@body))))

(defun -dotimes (num fn)
  "Call FN NUM times, presumably for side effects.
FN is called with a single argument on successive integers
running from 0, inclusive, to NUM, exclusive.  FN is not called
if NUM is less than 1.
This function's anaphoric counterpart is `--dotimes'."
  (declare (indent 1))
  (--dotimes num (funcall fn it)))

(defun -map (fn list)
  "Apply FN to each item in LIST and return the list of results.
This function's anaphoric counterpart is `--map'."
  (mapcar fn list))

(defmacro --map (form list)
  "Eval FORM for each item in LIST and return the list of results.
Each element of LIST in turn is bound to `it' before evaluating
BODY.
This is the anaphoric counterpart to `-map'."
  (declare (debug (def-form form)))
  `(mapcar (lambda (it) (ignore it) ,form) ,list))

(defmacro --reduce-from (form init list)
  "Accumulate a value by evaluating FORM across LIST.
This macro is like `--each' (which see), but it additionally
provides an accumulator variable `acc' which it successively
binds to the result of evaluating FORM for the current LIST
element before processing the next element.  For the first
element, `acc' is initialized with the result of evaluating INIT.
The return value is the resulting value of `acc'.  If LIST is
empty, FORM is not evaluated, and the return value is the result
of INIT.
This is the anaphoric counterpart to `-reduce-from'."
  (declare (debug (form form form)))
  `(let ((acc ,init))
     (--each ,list (setq acc ,form))
     acc))

(defun -reduce-from (fn init list)
  "Reduce the function FN across LIST, starting with INIT.
Return the result of applying FN to INIT and the first element of
LIST, then applying FN to that result and the second element,
etc.  If LIST is empty, return INIT without calling FN.

This function's anaphoric counterpart is `--reduce-from'.
For other folds, see also `-reduce' and `-reduce-r'."
  (--reduce-from (funcall fn acc it) init list))

(defmacro --reduce (form list)
  "Accumulate a value by evaluating FORM across LIST.
This macro is like `--reduce-from' (which see), except the first
element of LIST is taken as INIT.  Thus if LIST contains a single
item, it is returned without evaluating FORM.  If LIST is empty,
FORM is evaluated with `it' and `acc' bound to nil.
This is the anaphoric counterpart to `-reduce'."
  (declare (debug (form form)))
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv ,list))
       (if ,lv
           (--reduce-from ,form (car ,lv) (cdr ,lv))
         (let (acc it)
           (ignore acc it)
           ,form)))))

(defun -reduce (fn list)
  "Reduce the function FN across LIST.
Return the result of applying FN to the first two elements of
LIST, then applying FN to that result and the third element, etc.
If LIST contains a single element, return it without calling FN.
If LIST is empty, return the result of calling FN with no
arguments.

This function's anaphoric counterpart is `--reduce'.
For other folds, see also `-reduce-from' and `-reduce-r'."
  (if list
      (-reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro --reduce-r-from (form init list)
  "Accumulate a value by evaluating FORM across LIST in reverse.
This macro is like `--reduce-from', except it starts from the end
of LIST.
This is the anaphoric counterpart to `-reduce-r-from'."
  (declare (debug (form form form)))
  `(let ((acc ,init))
     (--each-r ,list (setq acc ,form))
     acc))

(defun -reduce-r-from (fn init list)
  "Reduce the function FN across LIST in reverse, starting with INIT.
Return the result of applying FN to the last element of LIST and
INIT, then applying FN to the second-to-last element and the
previous result of FN, etc.  That is, the first argument of FN is
the current element, and its second argument the accumulated
value.  If LIST is empty, return INIT without calling FN.

This function is like `-reduce-from' but the operation associates
from the right rather than left.  In other words, it starts from
the end of LIST and flips the arguments to FN.  Conceptually, it
is like replacing the conses in LIST with applications of FN, and
its last link with INIT, and evaluating the resulting expression.

This function's anaphoric counterpart is `--reduce-r-from'.
For other folds, see also `-reduce-r' and `-reduce'."
  (--reduce-r-from (funcall fn it acc) init list))

(defmacro --reduce-r (form list)
  "Accumulate a value by evaluating FORM across LIST in reverse order.
This macro is like `--reduce', except it starts from the end of
LIST.
This is the anaphoric counterpart to `-reduce-r'."
  (declare (debug (form form)))
  `(--reduce ,form (reverse ,list)))

(defun -reduce-r (fn list)
  "Reduce the function FN across LIST in reverse.
Return the result of applying FN to the last two elements of
LIST, then applying FN to the third-to-last element and the
previous result of FN, etc.  That is, the first argument of FN is
the current element, and its second argument the accumulated
value.  If LIST contains a single element, return it without
calling FN.  If LIST is empty, return the result of calling FN
with no arguments.

This function is like `-reduce' but the operation associates from
the right rather than left.  In other words, it starts from the
end of LIST and flips the arguments to FN.  Conceptually, it is
like replacing the conses in LIST with applications of FN,
ignoring its last link, and evaluating the resulting expression.

This function's anaphoric counterpart is `--reduce-r'.
For other folds, see also `-reduce-r-from' and `-reduce'."
  (if list
      (--reduce-r (funcall fn it acc) list)
    (funcall fn)))

(defmacro --reductions-from (form init list)
  "Return a list of FORM's intermediate reductions across LIST.
That is, a list of the intermediate values of the accumulator
when `--reduce-from' (which see) is called with the same
arguments.
This is the anaphoric counterpart to `-reductions-from'."
  (declare (debug (form form form)))
  `(nreverse
    (--reduce-from (cons (let ((acc (car acc))) (ignore acc) ,form) acc)
                   (list ,init)
                   ,list)))

(defun -reductions-from (fn init list)
  "Return a list of FN's intermediate reductions across LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce-from' (which see) is called with the same
arguments.
This function's anaphoric counterpart is `--reductions-from'.
For other folds, see also `-reductions' and `-reductions-r'."
  (--reductions-from (funcall fn acc it) init list))

(defmacro --reductions (form list)
  "Return a list of FORM's intermediate reductions across LIST.
That is, a list of the intermediate values of the accumulator
when `--reduce' (which see) is called with the same arguments.
This is the anaphoric counterpart to `-reductions'."
  (declare (debug (form form)))
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv ,list))
       (if ,lv
           (--reductions-from ,form (car ,lv) (cdr ,lv))
         (let (acc it)
           (ignore acc it)
           (list ,form))))))

(defun -reductions (fn list)
  "Return a list of FN's intermediate reductions across LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce' (which see) is called with the same arguments.
This function's anaphoric counterpart is `--reductions'.
For other folds, see also `-reductions' and `-reductions-r'."
  (if list
      (--reductions-from (funcall fn acc it) (car list) (cdr list))
    (list (funcall fn))))

(defmacro --reductions-r-from (form init list)
  "Return a list of FORM's intermediate reductions across reversed LIST.
That is, a list of the intermediate values of the accumulator
when `--reduce-r-from' (which see) is called with the same
arguments.
This is the anaphoric counterpart to `-reductions-r-from'."
  (declare (debug (form form form)))
  `(--reduce-r-from (cons (let ((acc (car acc))) (ignore acc) ,form) acc)
                    (list ,init)
                    ,list))

(defun -reductions-r-from (fn init list)
  "Return a list of FN's intermediate reductions across reversed LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce-r-from' (which see) is called with the same
arguments.
This function's anaphoric counterpart is `--reductions-r-from'.
For other folds, see also `-reductions' and `-reductions-r'."
  (--reductions-r-from (funcall fn it acc) init list))

(defmacro --reductions-r (form list)
  "Return a list of FORM's intermediate reductions across reversed LIST.
That is, a list of the intermediate values of the accumulator
when `--reduce-re' (which see) is called with the same arguments.
This is the anaphoric counterpart to `-reductions-r'."
  (declare (debug (form list)))
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv (reverse ,list)))
       (if ,lv
           (--reduce-from (cons (let ((acc (car acc))) (ignore acc) ,form) acc)
                          (list (car ,lv))
                          (cdr ,lv))
         (let (acc it)
           (ignore acc it)
           (list ,form))))))

(defun -reductions-r (fn list)
  "Return a list of FN's intermediate reductions across reversed LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce-r' (which see) is called with the same arguments.
This function's anaphoric counterpart is `--reductions-r'.
For other folds, see also `-reductions-r-from' and
`-reductions'."
  (if list
      (--reductions-r (funcall fn it acc) list)
    (list (funcall fn))))

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

(defmacro --iterate (form init n)
  "Anaphoric version of `-iterate'."
  (declare (debug (form form form)))
  (let ((res (make-symbol "result")))
    `(let ((it ,init) ,res)
       (dotimes (_ ,n)
         (push it ,res)
         (setq it ,form))
       (nreverse ,res))))

(defun -iterate (fun init n)
  "Return a list of iterated applications of FUN to INIT.

This means a list of the form:

  (INIT (FUN INIT) (FUN (FUN INIT)) ...)

N is the length of the returned list."
  (--iterate (funcall fun it) init n))

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
The last 2 elements of ARGS are used as the final cons of the
result, so if the final element of ARGS is not a list, the result
is a dotted list.  With no ARGS, return nil."
  (declare (pure t) (side-effect-free t))
  (let* ((len (length args))
         (tail (nthcdr (- len 2) args))
         (last (cdr tail)))
    (if (null last)
        (car args)
      (setcdr tail (car last))
      args)))

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

(defun ---truthy? (obj)
  "Return OBJ as a boolean value (t or nil)."
  (declare (pure t) (side-effect-free t))
  (and obj t))

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

(defmacro --take-while (form list)
  "Take successive items from LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.  Return a new
list of the successive elements from the start of LIST for which
FORM evaluates to non-nil.
This is the anaphoric counterpart to `-take-while'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each-while ,list ,form (push it ,r))
       (nreverse ,r))))

(defun -take-while (pred list)
  "Take successive items from LIST for which PRED returns non-nil.
PRED is a function of one argument.  Return a new list of the
successive elements from the start of LIST for which PRED returns
non-nil.
This function's anaphoric counterpart is `--take-while'.
For another variant, see also `-drop-while'."
  (--take-while (funcall pred it) list))

(defmacro --drop-while (form list)
  "Drop successive items from LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.  Return the
tail (not a copy) of LIST starting from its first element for
which FORM evaluates to nil.
This is the anaphoric counterpart to `-drop-while'."
  (declare (debug (form form)))
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (--each-while ,l ,form (pop ,l))
       ,l)))

(defun -drop-while (pred list)
  "Drop successive items from LIST for which PRED returns non-nil.
PRED is a function of one argument.  Return the tail (not a copy)
of LIST starting from its first element for which PRED returns
nil.
This function's anaphoric counterpart is `--drop-while'.
For another variant, see also `-take-while'."
  (--drop-while (funcall pred it) list))

(defun -take (n list)
  "Return a copy of the first N items in LIST.
Return a copy of LIST if it contains N items or fewer.
Return nil if N is zero or less.

See also: `-take-last'."
  (declare (pure t) (side-effect-free t))
  (--take-while (< it-index n) list))

(defun -take-last (n list)
  "Return a copy of the last N items of LIST in order.
Return a copy of LIST if it contains N items or fewer.
Return nil if N is zero or less.

See also: `-take'."
  (declare (pure t) (side-effect-free t))
  (copy-sequence (last list n)))

(defalias '-drop #'nthcdr
  "Return the tail (not a copy) of LIST without the first N items.
Return nil if LIST contains N items or fewer.
Return LIST if N is zero or less.
For another variant, see also `-drop-last'.
\n(fn N LIST)")

(defun -drop-last (n list)
  "Return a copy of LIST without its last N items.
Return a copy of LIST if N is zero or less.
Return nil if LIST contains N items or fewer.

See also: `-drop'."
  (declare (pure t) (side-effect-free t))
  (nbutlast (copy-sequence list) n))

(defun -split-at (n list)
  "Split LIST into two sublists after the Nth element.
The result is a list of two elements (TAKE DROP) where TAKE is a
new list of the first N elements of LIST, and DROP is the
remaining elements of LIST (not a copy).  TAKE and DROP are like
the results of `-take' and `-drop', respectively, but the split
is done in a single list traversal."
  (declare (pure t) (side-effect-free t))
  (let (result)
    (--each-while list (< it-index n)
      (push (pop list) result))
    (list (nreverse result) list)))

(defun -rotate (n list)
  "Rotate LIST N places to the right.  With N negative, rotate to the left.
The time complexity is O(n)."
  (declare (pure t) (side-effect-free t))
  (when list
    (let* ((len (length list))
           (n-mod-len (mod n len))
           (new-tail-len (- len n-mod-len)))
      (append (nthcdr new-tail-len list) (-take new-tail-len list)))))

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

(defun dash--partition-all-in-steps-reversed (n step list)
  "Used by `-partition-all-in-steps' and `-partition-in-steps'."
  (when (< step 1)
    (signal 'wrong-type-argument
            `("Step size < 1 results in juicy infinite loops" ,step)))
  (let (result)
    (while list
      (push (-take n list) result)
      (setq list (nthcdr step list)))
    result))

(defun -partition-all-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
The last groups may contain less than N items."
  (declare (pure t) (side-effect-free t))
  (nreverse (dash--partition-all-in-steps-reversed n step list)))

(defun -partition-in-steps (n step list)
  "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (declare (pure t) (side-effect-free t))
  (let ((result (dash--partition-all-in-steps-reversed n step list)))
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
return a list of cons-cells such that the above identity works.

See also: `-zip'"
  (apply '-zip lists))

(defun -cycle (list)
  "Return an infinite circular copy of LIST.
The returned list cycles through the elements of LIST and repeats
from the beginning."
  (declare (pure t) (side-effect-free t))
  ;; Also works with sequences that aren't lists.
  (let ((newlist (append list ())))
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
  (declare (debug (form body)) (indent 1))
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
  "When expr is non-nil, thread it through the first form (via `-->'),
and when that result is non-nil, through the next form, etc."
  (declare (debug ->)
           (indent 1))
  (if (null form) x
    (let ((result (make-symbol "result")))
      `(-some--> (-when-let (,result ,x)
                   (--> ,result ,form))
                 ,@more))))

(defmacro -doto (init &rest forms)
  "Evaluate INIT and pass it as argument to FORMS with `->'.
The RESULT of evaluating INIT is threaded through each of FORMS
individually using `->', which see.  The return value is RESULT,
which FORMS may have modified by side effect."
  (declare (debug (form body)) (indent 1))
  (let ((retval (make-symbol "result")))
    `(let ((,retval ,init))
       ,@(mapcar (lambda (form) `(-> ,retval ,form)) forms)
       ,retval)))

(defmacro --doto (init &rest forms)
  "Anaphoric form of `-doto'.
This just evaluates INIT, binds the result to `it', evaluates
FORMS, and returns the final value of `it'.
Note: `it' need not be used in each form."
  (declare (debug (form body)) (indent 1))
  `(let ((it ,init))
     ,@forms
     it))

(defun -grade-up (comparator list)
  "Grade elements of LIST using COMPARATOR relation.
This yields a permutation vector such that applying this
permutation to LIST sorts it in ascending order."
  (->> (--map-indexed (cons it it-index) list)
       (-sort (lambda (it other) (funcall comparator (car it) (car other))))
       (mapcar #'cdr)))

(defun -grade-down (comparator list)
  "Grade elements of LIST using COMPARATOR relation.
This yields a permutation vector such that applying this
permutation to LIST sorts it in descending order."
  (->> (--map-indexed (cons it it-index) list)
       (-sort (lambda (it other) (funcall comparator (car other) (car it))))
       (mapcar #'cdr)))

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
                        `(substring ,source ,i))
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
              (dash--match (substring match-form 2) s))))
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

  (-let ((PATTERN SOURCE)) ...)

becomes

  (-let [PATTERN SOURCE] ...).

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

  (a1 a2 a3 ...) - bind 0th car of list to A1, 1st to A2, 2nd to A3...

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
translated into normal `lambda', so there is no performance
penalty.

See `-let' for a description of the destructuring mechanism."
  (declare (doc-string 2) (indent defun)
           (debug (&define sexp
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (cond
   ((nlistp match-form)
    (signal 'wrong-type-argument (list #'listp match-form)))
   ;; No destructuring, so just return regular `lambda' for speed.
   ((-all? #'symbolp match-form)
    `(lambda ,match-form ,@body))
   ((let ((inputs (--map-indexed
                   (list it (make-symbol (format "input%d" it-index)))
                   match-form)))
      ;; TODO: because inputs to the `lambda' are evaluated only once,
      ;; `-let*' need not create the extra bindings to ensure that.
      ;; We should find a way to optimize that.  Not critical however.
      `(lambda ,(mapcar #'cadr inputs)
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
    (signal 'wrong-number-of-arguments (list '-setq (1+ (length forms)))))
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

(defun -list (&optional arg &rest args)
  "Ensure ARG is a list.
If ARG is already a list, return it as is (not a copy).
Otherwise, return a new list with ARG as its only element.

Another supported calling convention is (-list &rest ARGS).
In this case, if ARG is not a list, a new list with all of
ARGS as elements is returned.  This use is supported for
backward compatibility and is otherwise deprecated."
  (declare (advertised-calling-convention (arg) "2.18.0")
           (pure t) (side-effect-free t))
  (if (listp arg) arg (cons arg args)))

(defun -repeat (n x)
  "Return a new list of length N with each element being X.
Return nil if N is less than 1."
  (declare (pure t) (side-effect-free t))
  (and (natnump n) (make-list n x)))

(defun -sum (list)
  "Return the sum of LIST."
  (declare (pure t) (side-effect-free t))
  (apply '+ list))

(defun -running-sum (list)
  "Return a list with running sums of items in LIST.
LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (or list (signal 'wrong-type-argument (list #'consp list)))
  (-reductions #'+ list))

(defun -product (list)
  "Return the product of LIST."
  (declare (pure t) (side-effect-free t))
  (apply '* list))

(defun -running-product (list)
  "Return a list with running products of items in LIST.
LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (or list (signal 'wrong-type-argument (list #'consp list)))
  (-reductions #'* list))

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

(defun -iota (count &optional start step)
  "Return a list containing COUNT numbers.
Starts from START and adds STEP each time.  The default START is
zero, the default STEP is 1.
This function takes its name from the corresponding primitive in
the APL language."
  (declare (pure t) (side-effect-free t))
  (unless (natnump count)
    (signal 'wrong-type-argument (list #'natnump count)))
  (or start (setq start 0))
  (or step (setq step 1))
  (if (zerop step)
      (make-list count start)
    (--iterate (+ it step) start count)))

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

(defun -cons-pair? (obj)
  "Return non-nil if OBJ is a true cons pair.
That is, a cons (A . B) where B is not a list.
Alias: `-cons-pair-p'."
  (declare (pure t) (side-effect-free t))
  (nlistp (cdr-safe obj)))

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

;;; Font lock

(defvar dash--keywords
  `(;; TODO: Do not fontify the following automatic variables
    ;; globally; detect and limit to their local anaphoric scope.
    (,(concat "\\_<" (regexp-opt '("acc" "it" "it-index" "other")) "\\_>")
     0 font-lock-variable-name-face)
    ;; Elisp macro fontification was static prior to Emacs 25.
    ,@(when (< emacs-major-version 25)
        (let ((macs '("!cdr"
                      "!cons"
                      "-->"
                      "--all?"
                      "--annotate"
                      "--any?"
                      "--count"
                      "--dotimes"
                      "--doto"
                      "--drop-while"
                      "--each"
                      "--each-r"
                      "--each-r-while"
                      "--each-while"
                      "--filter"
                      "--find-index"
                      "--find-indices"
                      "--find-last-index"
                      "--first"
                      "--fix"
                      "--group-by"
                      "--if-let"
                      "--iterate"
                      "--keep"
                      "--last"
                      "--map"
                      "--map-first"
                      "--map-indexed"
                      "--map-last"
                      "--map-when"
                      "--mapcat"
                      "--max-by"
                      "--min-by"
                      "--none?"
                      "--only-some?"
                      "--partition-by"
                      "--partition-by-header"
                      "--reduce"
                      "--reduce-from"
                      "--reduce-r"
                      "--reduce-r-from"
                      "--reductions"
                      "--reductions-from"
                      "--reductions-r"
                      "--reductions-r-from"
                      "--remove"
                      "--remove-first"
                      "--remove-last"
                      "--separate"
                      "--some"
                      "--sort"
                      "--splice"
                      "--splice-list"
                      "--split-when"
                      "--split-with"
                      "--take-while"
                      "--tree-map"
                      "--tree-map-nodes"
                      "--tree-mapreduce"
                      "--tree-mapreduce-from"
                      "--tree-reduce"
                      "--tree-reduce-from"
                      "--tree-seq"
                      "--unfold"
                      "--update-at"
                      "--when-let"
                      "--zip-with"
                      "->"
                      "->>"
                      "-as->"
                      "-doto"
                      "-if-let"
                      "-if-let*"
                      "-lambda"
                      "-let"
                      "-let*"
                      "-setq"
                      "-some-->"
                      "-some->"
                      "-some->>"
                      "-split-on"
                      "-when-let"
                      "-when-let*")))
          `((,(concat "(" (regexp-opt macs 'symbols)) . 1)))))
  "Font lock keywords for `dash-fontify-mode'.")

(defcustom dash-fontify-mode-lighter nil
  "Mode line lighter for `dash-fontify-mode'.
Either a string to display in the mode line when
`dash-fontify-mode' is on, or nil to display
nothing (the default)."
  :package-version '(dash . "2.18.0")
  :group 'dash
  :type '(choice (string :tag "Lighter" :value " Dash")
                 (const :tag "Nothing" nil)))

;;;###autoload
(define-minor-mode dash-fontify-mode
  "Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'."
  :group 'dash :lighter dash-fontify-mode-lighter
  (if dash-fontify-mode
      (font-lock-add-keywords nil dash--keywords t)
    (font-lock-remove-keywords nil dash--keywords))
  (cond ((fboundp 'font-lock-flush) ;; Added in Emacs 25.
         (font-lock-flush))
        ;; `font-lock-fontify-buffer' unconditionally enables
        ;; `font-lock-mode' and is marked `interactive-only' in later
        ;; Emacs versions which have `font-lock-flush', so we guard
        ;; and pacify as needed, respectively.
        (font-lock-mode
         (with-no-warnings
           (font-lock-fontify-buffer)))))

(defun dash--turn-on-fontify-mode ()
  "Enable `dash-fontify-mode' if in an Emacs Lisp buffer."
  (when (derived-mode-p #'emacs-lisp-mode)
    (dash-fontify-mode)))

;;;###autoload
(define-globalized-minor-mode global-dash-fontify-mode
  dash-fontify-mode dash--turn-on-fontify-mode
  :group 'dash)

(defcustom dash-enable-fontlock nil
  "If non-nil, fontify Dash macro calls and special variables."
  :group 'dash
  :set (lambda (sym val)
         (set-default sym val)
         (global-dash-fontify-mode (if val 1 0)))
  :type 'boolean)

(make-obsolete-variable
 'dash-enable-fontlock #'global-dash-fontify-mode "2.18.0")

(define-obsolete-function-alias
  'dash-enable-font-lock #'global-dash-fontify-mode "2.18.0")

;;; Info

(defvar dash--info-doc-spec '("(dash) Index" nil "^ -+ .*: " "\\( \\|$\\)")
  "The Dash :doc-spec entry for `info-lookup-alist'.
It is based on that for `emacs-lisp-mode'.")

(defun dash--info-elisp-docs ()
  "Return the `emacs-lisp-mode' symbol docs from `info-lookup-alist'.
Specifically, return the cons containing their
`info-lookup->doc-spec' so that we can modify it."
  (defvar info-lookup-alist)
  (nthcdr 3 (assq #'emacs-lisp-mode (cdr (assq 'symbol info-lookup-alist)))))

;;;###autoload
(defun dash-register-info-lookup ()
  "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]."
  (interactive)
  (require 'info-look)
  (let ((docs (dash--info-elisp-docs)))
    (setcar docs (append (car docs) (list dash--info-doc-spec)))
    (info-lookup-reset)))

(defun dash-unload-function ()
  "Remove Dash from `info-lookup-alist'.
Used by `unload-feature', which see."
  (let ((docs (and (featurep 'info-look)
                   (dash--info-elisp-docs))))
    (when (member dash--info-doc-spec (car docs))
      (setcar docs (remove dash--info-doc-spec (car docs)))
      (info-lookup-reset)))
  nil)

(provide 'dash)
;;; dash.el ends here
