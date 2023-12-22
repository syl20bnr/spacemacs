;;; dash.el --- A modern list library for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 2.19.1
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

(eval-when-compile
  ;; TODO: Emacs 24.3 first introduced `gv', so remove this and all
  ;; calls to `defsetf' when support for earlier versions is dropped.
  (unless (fboundp 'gv-define-setter)
    (require 'cl))

  ;; - 24.3 started complaining about unknown `declare' props.
  ;; - 25 introduced `pure' and `side-effect-free'.
  ;; - 30 introduced `important-return-value'.
  (when (boundp 'defun-declarations-alist)
    (dolist (prop '(important-return-value pure side-effect-free))
      (unless (assq prop defun-declarations-alist)
        (push (list prop #'ignore) defun-declarations-alist)))))

(defgroup dash ()
  "Customize group for Dash, a modern list library."
  :group 'extensions
  :group 'lisp
  :prefix "dash-")

(defmacro !cons (car cdr)
  "Destructive: Set CDR to the cons of CAR and CDR."
  (declare (debug (form symbolp)))
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Set LIST to the cdr of LIST."
  (declare (debug (symbolp)))
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
           (,i 0))
       (while ,l
         (let ((it (pop ,l)) (it-index ,i))
           (ignore it it-index)
           ,@body)
         (setq ,i (1+ ,i))))))

(defun -each (list fn)
  "Call FN on each element of LIST.
Return nil; this function is intended for side effects.

Its anaphoric counterpart is `--each'.

For access to the current element's index in LIST, see
`-each-indexed'."
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
           ,elt)
       (while (when ,l
                (setq ,elt (car-safe ,l))
                (let ((it ,elt) (it-index ,i))
                  (ignore it it-index)
                  ,pred))
         (let ((it ,elt) (it-index ,i))
           (ignore it it-index)
           ,@body)
         (setq ,i (1+ ,i) ,l (cdr ,l))))))

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
  (declare (important-return-value t))
  (mapcar fn list))

(defmacro --map (form list)
  "Eval FORM for each item in LIST and return the list of results.
Each element of LIST in turn is bound to `it' before evaluating
FORM.
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
  (declare (important-return-value t))
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
         ;; Explicit nil binding pacifies lexical "variable left uninitialized"
         ;; warning.  See issue #377 and upstream https://bugs.gnu.org/47080.
         (let ((acc nil) (it nil))
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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
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
         ;; Explicit nil binding pacifies lexical "variable left uninitialized"
         ;; warning.  See issue #377 and upstream https://bugs.gnu.org/47080.
         (let ((acc nil) (it nil))
           (ignore acc it)
           (list ,form))))))

(defun -reductions (fn list)
  "Return a list of FN's intermediate reductions across LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce' (which see) is called with the same arguments.

This function's anaphoric counterpart is `--reductions'.

For other folds, see also `-reductions' and `-reductions-r'."
  (declare (important-return-value t))
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
  (declare (important-return-value t))
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
         ;; Explicit nil binding pacifies lexical "variable left uninitialized"
         ;; warning.  See issue #377 and upstream https://bugs.gnu.org/47080.
         (let ((acc nil) (it nil))
           (ignore acc it)
           (list ,form))))))

(defun -reductions-r (fn list)
  "Return a list of FN's intermediate reductions across reversed LIST.
That is, a list of the intermediate values of the accumulator
when `-reduce-r' (which see) is called with the same arguments.

This function's anaphoric counterpart is `--reductions-r'.

For other folds, see also `-reductions-r-from' and
`-reductions'."
  (declare (important-return-value t))
  (if list
      (--reductions-r (funcall fn it acc) list)
    (list (funcall fn))))

(defmacro --filter (form list)
  "Return a new list of the items in LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-filter'.
For the opposite operation, see also `--remove'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (push it ,r)))
       (nreverse ,r))))

(defun -filter (pred list)
  "Return a new list of the items in LIST for which PRED returns non-nil.

Alias: `-select'.

This function's anaphoric counterpart is `--filter'.

For similar operations, see also `-keep' and `-remove'."
  (declare (important-return-value t))
  (--filter (funcall pred it) list))

(defalias '-select '-filter)
(defalias '--select '--filter)

(defmacro --remove (form list)
  "Return a new list of the items in LIST for which FORM evals to nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-remove'.
For the opposite operation, see also `--filter'."
  (declare (debug (form form)))
  `(--filter (not ,form) ,list))

(defun -remove (pred list)
  "Return a new list of the items in LIST for which PRED returns nil.

Alias: `-reject'.

This function's anaphoric counterpart is `--remove'.

For similar operations, see also `-keep' and `-filter'."
  (declare (important-return-value t))
  (--remove (funcall pred it) list))

(defalias '-reject '-remove)
(defalias '--reject '--remove)

(defmacro --remove-first (form list)
  "Remove the first item from LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.  This is a
non-destructive operation, but only the front of LIST leading up
to the removed item is a copy; the rest is LIST's original tail.
If no item is removed, then the result is a complete copy.
This is the anaphoric counterpart to `-remove-first'."
  (declare (debug (form form)))
  (let ((front (make-symbol "front"))
        (tail (make-symbol "tail")))
    `(let ((,tail ,list) ,front)
       (--each-while ,tail (not ,form)
         (push (pop ,tail) ,front))
       (if ,tail
           (nconc (nreverse ,front) (cdr ,tail))
         (nreverse ,front)))))

(defun -remove-first (pred list)
  "Remove the first item from LIST for which PRED returns non-nil.
This is a non-destructive operation, but only the front of LIST
leading up to the removed item is a copy; the rest is LIST's
original tail.  If no item is removed, then the result is a
complete copy.

Alias: `-reject-first'.

This function's anaphoric counterpart is `--remove-first'.

See also `-map-first', `-remove-item', and `-remove-last'."
  (declare (important-return-value t))
  (--remove-first (funcall pred it) list))

;; TODO: #'-quoting the macro upsets Emacs 24.
(defalias '-reject-first #'-remove-first)
(defalias '--reject-first '--remove-first)

(defmacro --remove-last (form list)
  "Remove the last item from LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' before evaluating
FORM.  The result is a copy of LIST regardless of whether an
element is removed.
This is the anaphoric counterpart to `-remove-last'."
  (declare (debug (form form)))
  `(nreverse (--remove-first ,form (reverse ,list))))

(defun -remove-last (pred list)
  "Remove the last item from LIST for which PRED returns non-nil.
The result is a copy of LIST regardless of whether an element is
removed.

Alias: `-reject-last'.

This function's anaphoric counterpart is `--remove-last'.

See also `-map-last', `-remove-item', and `-remove-first'."
  (declare (important-return-value t))
  (--remove-last (funcall pred it) list))

(defalias '-reject-last '-remove-last)
(defalias '--reject-last '--remove-last)

(defalias '-remove-item #'remove
  "Return a copy of LIST with all occurrences of ITEM removed.
The comparison is done with `equal'.
\n(fn ITEM LIST)")

(defmacro --keep (form list)
  "Eval FORM for each item in LIST and return the non-nil results.
Like `--filter', but returns the non-nil results of FORM instead
of the corresponding elements of LIST.  Each element of LIST in
turn is bound to `it' and its index within LIST to `it-index'
before evaluating FORM.
This is the anaphoric counterpart to `-keep'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (push ,m ,r))))
       (nreverse ,r))))

(defun -keep (fn list)
  "Return a new list of the non-nil results of applying FN to each item in LIST.
Like `-filter', but returns the non-nil results of FN instead of
the corresponding elements of LIST.

Its anaphoric counterpart is `--keep'."
  (declare (important-return-value t))
  (--keep (funcall fn it) list))

(defun -non-nil (list)
  "Return a copy of LIST with all nil items removed."
  (declare (side-effect-free t))
  (--filter it list))

(defmacro --map-indexed (form list)
  "Eval FORM for each item in LIST and return the list of results.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.  This is like
`--map', but additionally makes `it-index' available to FORM.

This is the anaphoric counterpart to `-map-indexed'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list
         (push ,form ,r))
       (nreverse ,r))))

(defun -map-indexed (fn list)
  "Apply FN to each index and item in LIST and return the list of results.
This is like `-map', but FN takes two arguments: the index of the
current element within LIST, and the element itself.

This function's anaphoric counterpart is `--map-indexed'.

For a side-effecting variant, see also `-each-indexed'."
  (declare (important-return-value t))
  (--map-indexed (funcall fn it-index it) list))

(defmacro --map-when (pred rep list)
  "Anaphoric form of `-map-when'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (!cons (if ,pred ,rep it) ,r))
       (nreverse ,r))))

(defun -map-when (pred rep list)
  "Use PRED to conditionally apply REP to each item in LIST.
Return a copy of LIST where the items for which PRED returns nil
are unchanged, and the rest are mapped through the REP function.

Alias: `-replace-where'

See also: `-update-at'"
  (declare (important-return-value t))
  (--map-when (funcall pred it) (funcall rep it) list))

(defalias '-replace-where '-map-when)
(defalias '--replace-where '--map-when)

(defun -map-first (pred rep list)
  "Use PRED to determine the first item in LIST to call REP on.
Return a copy of LIST where the first item for which PRED returns
non-nil is replaced with the result of calling REP on that item.

See also: `-map-when', `-replace-first'"
  (declare (important-return-value t))
  (let (front)
    (while (and list (not (funcall pred (car list))))
      (push (car list) front)
      (!cdr list))
    (if list
        (-concat (nreverse front) (cons (funcall rep (car list)) (cdr list)))
      (nreverse front))))

(defmacro --map-first (pred rep list)
  "Anaphoric form of `-map-first'."
  (declare (debug (def-form def-form form)))
  `(-map-first (lambda (it) (ignore it) ,pred)
               (lambda (it) (ignore it) ,rep)
               ,list))

(defun -map-last (pred rep list)
  "Use PRED to determine the last item in LIST to call REP on.
Return a copy of LIST where the last item for which PRED returns
non-nil is replaced with the result of calling REP on that item.

See also: `-map-when', `-replace-last'"
  (declare (important-return-value t))
  (nreverse (-map-first pred rep (reverse list))))

(defmacro --map-last (pred rep list)
  "Anaphoric form of `-map-last'."
  (declare (debug (def-form def-form form)))
  `(-map-last (lambda (it) (ignore it) ,pred)
              (lambda (it) (ignore it) ,rep)
              ,list))

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
  `(apply #'append (--map ,form ,list)))

(defun -mapcat (fn list)
  "Return the concatenation of the result of mapping FN over LIST.
Thus function FN should return a list."
  (declare (important-return-value t))
  (--mapcat (funcall fn it) list))

(defmacro --iterate (form init n)
  "Anaphoric version of `-iterate'."
  (declare (debug (form form form)))
  (let ((res (make-symbol "result"))
        (len (make-symbol "n")))
    `(let ((,len ,n))
       (when (> ,len 0)
         (let* ((it ,init)
                (,res (list it)))
           (dotimes (_ (1- ,len))
             (push (setq it ,form) ,res))
           (nreverse ,res))))))

(defun -iterate (fun init n)
  "Return a list of iterated applications of FUN to INIT.

This means a list of the form:

  (INIT (FUN INIT) (FUN (FUN INIT)) ...)

N is the length of the returned list."
  (declare (important-return-value t))
  (--iterate (funcall fun it) init n))

(defun -flatten (l)
  "Take a nested list L and return its contents as a single, flat list.

Note that because nil represents a list of zero elements (an
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
  (dotimes (_ num)
    (setq list (apply #'append (mapcar #'-list list))))
  list)

(defalias '-concat #'append
  "Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.

All arguments except the last argument are copied.  The last argument
is just used as the tail of the new list.

\(fn &rest SEQUENCES)")

(defalias '-copy #'copy-sequence
  "Create a shallow copy of LIST.

\(fn LIST)")

(defmacro --splice (pred form list)
  "Splice lists generated by FORM in place of items satisfying PRED in LIST.

Evaluate PRED for each element of LIST in turn bound to `it'.
Whenever the result of PRED is nil, leave that `it' is-is.
Otherwise, evaluate FORM with the same `it' binding still in
place.  The result should be a (possibly empty) list of items to
splice in place of `it' in LIST.

This can be useful as an alternative to the `,@' construct in a
`\\=`' structure, in case you need to splice several lists at
marked positions (for example with keywords).

This is the anaphoric counterpart to `-splice'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list
         (if ,pred
             (--each ,form (push it ,r))
           (push it ,r)))
       (nreverse ,r))))

(defun -splice (pred fun list)
  "Splice lists generated by FUN in place of items satisfying PRED in LIST.

Call PRED on each element of LIST.  Whenever the result of PRED
is nil, leave that `it' as-is.  Otherwise, call FUN on the same
`it' that satisfied PRED.  The result should be a (possibly
empty) list of items to splice in place of `it' in LIST.

This can be useful as an alternative to the `,@' construct in a
`\\=`' structure, in case you need to splice several lists at
marked positions (for example with keywords).

This function's anaphoric counterpart is `--splice'.

See also: `-splice-list', `-insert-at'."
  (declare (important-return-value t))
  (--splice (funcall pred it) (funcall fun it) list))

(defun -splice-list (pred new-list list)
  "Splice NEW-LIST in place of elements matching PRED in LIST.

See also: `-splice', `-insert-at'"
  (declare (important-return-value t))
  (-splice pred (lambda (_) new-list) list))

(defmacro --splice-list (pred new-list list)
  "Anaphoric form of `-splice-list'."
  (declare (debug (def-form form form)))
  `(-splice-list (lambda (it) (ignore it) ,pred) ,new-list ,list))

(defun -cons* (&rest args)
  "Make a new list from the elements of ARGS.
The last 2 elements of ARGS are used as the final cons of the
result, so if the final element of ARGS is not a list, the result
is a dotted list.  With no ARGS, return nil."
  (declare (side-effect-free t))
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

If any ELEMENTS are given, append them to the list as well."
  (declare (side-effect-free t))
  (-concat list (list elem) elements))

(defmacro --first (form list)
  "Return the first item in LIST for which FORM evals to non-nil.
Return nil if no such element is found.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-first'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (or (not ,form)
                               (ignore (setq ,n it))))
       ,n)))

(defun -first (pred list)
  "Return the first item in LIST for which PRED returns non-nil.
Return nil if no such element is found.

To get the first item in the list no questions asked,
use `-first-item'.

Alias: `-find'.

This function's anaphoric counterpart is `--first'."
  (declare (important-return-value t))
  (--first (funcall pred it) list))

(defalias '-find #'-first)
(defalias '--find '--first)

(defmacro --some (form list)
  "Return non-nil if FORM evals to non-nil for at least one item in LIST.
If so, return the first such result of FORM.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-some'."
  (declare (debug (form form)))
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not (setq ,n ,form)))
       ,n)))

(defun -some (pred list)
  "Return (PRED x) for the first LIST item where (PRED x) is non-nil, else nil.

Alias: `-any'.

This function's anaphoric counterpart is `--some'."
  (declare (important-return-value t))
  (--some (funcall pred it) list))

(defalias '-any '-some)
(defalias '--any '--some)

(defmacro --every (form list)
  "Return non-nil if FORM evals to non-nil for all items in LIST.
If so, return the last such result of FORM.  Otherwise, once an
item is reached for which FORM yields nil, return nil without
evaluating FORM for any further LIST elements.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.

This macro is like `--every-p', but on success returns the last
non-nil result of FORM instead of just t.

This is the anaphoric counterpart to `-every'."
  (declare (debug (form form)))
  (let ((a (make-symbol "all")))
    `(let ((,a t))
       (--each-while ,list (setq ,a ,form))
       ,a)))

(defun -every (pred list)
  "Return non-nil if PRED returns non-nil for all items in LIST.
If so, return the last such result of PRED.  Otherwise, once an
item is reached for which PRED returns nil, return nil without
calling PRED on any further LIST elements.

This function is like `-every-p', but on success returns the last
non-nil result of PRED instead of just t.

This function's anaphoric counterpart is `--every'."
  (declare (important-return-value t))
  (--every (funcall pred it) list))

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
  (declare (important-return-value t))
  (--last (funcall pred it) list))

(defalias '-first-item #'car
  "Return the first item of LIST, or nil on an empty list.

See also: `-second-item', `-last-item', etc.

\(fn LIST)")

;; Ensure that calls to `-first-item' are compiled to a single opcode,
;; just like `car'.
(put '-first-item 'byte-opcode 'byte-car)
(put '-first-item 'byte-compile 'byte-compile-one-arg)
(put '-first-item 'pure t)
(put '-first-item 'side-effect-free t)

(defalias '-second-item #'cadr
  "Return the second item of LIST, or nil if LIST is too short.

See also: `-first-item', `-third-item', etc.

\(fn LIST)")

(put '-second-item 'pure t)
(put '-second-item 'side-effect-free t)

(defalias '-third-item
  (if (fboundp 'caddr)
      #'caddr
    (lambda (list) (car (cddr list))))
  "Return the third item of LIST, or nil if LIST is too short.

See also: `-second-item', `-fourth-item', etc.

\(fn LIST)")

(put '-third-item 'pure t)
(put '-third-item 'side-effect-free t)

(defalias '-fourth-item
  (if (fboundp 'cadddr)
      #'cadddr
    (lambda (list) (cadr (cddr list))))
  "Return the fourth item of LIST, or nil if LIST is too short.

See also: `-third-item', `-fifth-item', etc.

\(fn LIST)")

(put '-fourth-item 'pure t)
(put '-fourth-item 'side-effect-free t)

(defun -fifth-item (list)
  "Return the fifth item of LIST, or nil if LIST is too short.

See also: `-fourth-item', `-last-item', etc."
  (declare (pure t) (side-effect-free t))
  (car (cddr (cddr list))))

(defun -last-item (list)
  "Return the last item of LIST, or nil on an empty list.

See also: `-first-item', etc."
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
  (declare (important-return-value t))
  (--count (funcall pred it) list))

(defun ---truthy? (obj)
  "Return OBJ as a boolean value (t or nil)."
  (declare (pure t) (side-effect-free error-free))
  (and obj t))

(defmacro --any? (form list)
  "Anaphoric form of `-any?'."
  (declare (debug (form form)))
  `(and (--some ,form ,list) t))

(defun -any? (pred list)
  "Return t if (PRED X) is non-nil for any X in LIST, else nil.

Alias: `-any-p', `-some?', `-some-p'"
  (declare (important-return-value t))
  (--any? (funcall pred it) list))

(defalias '-some? '-any?)
(defalias '--some? '--any?)
(defalias '-any-p '-any?)
(defalias '--any-p '--any?)
(defalias '-some-p '-any?)
(defalias '--some-p '--any?)

(defmacro --all? (form list)
  "Return t if FORM evals to non-nil for all items in LIST.
Otherwise, once an item is reached for which FORM yields nil,
return nil without evaluating FORM for any further LIST elements.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.

The similar macro `--every' is more widely useful, since it
returns the last non-nil result of FORM instead of just t on
success.

Alias: `--all-p', `--every-p', `--every?'.

This is the anaphoric counterpart to `-all?'."
  (declare (debug (form form)))
  `(and (--every ,form ,list) t))

(defun -all? (pred list)
  "Return t if (PRED X) is non-nil for all X in LIST, else nil.
In the latter case, stop after the first X for which (PRED X) is
nil, without calling PRED on any subsequent elements of LIST.

The similar function `-every' is more widely useful, since it
returns the last non-nil result of PRED instead of just t on
success.

Alias: `-all-p', `-every-p', `-every?'.

This function's anaphoric counterpart is `--all?'."
  (declare (important-return-value t))
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
  "Return t if (PRED X) is nil for all X in LIST, else nil.

Alias: `-none-p'"
  (declare (important-return-value t))
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
  "Return t if different LIST items both satisfy and do not satisfy PRED.
That is, if PRED returns both nil for at least one item, and
non-nil for at least one other item in LIST.  Return nil if all
items satisfy the predicate or none of them do.

Alias: `-only-some-p'"
  (declare (important-return-value t))
  (--only-some? (funcall pred it) list))

(defalias '-only-some-p '-only-some?)
(defalias '--only-some-p '--only-some?)

(defun -slice (list from &optional to step)
  "Return copy of LIST, starting from index FROM to index TO.

FROM or TO may be negative.  These values are then interpreted
modulo the length of the list.

If STEP is a number, only each STEPth item in the resulting
section is returned.  Defaults to 1."
  (declare (side-effect-free t))
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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
  (--drop-while (funcall pred it) list))

(defun -take (n list)
  "Return a copy of the first N items in LIST.
Return a copy of LIST if it contains N items or fewer.
Return nil if N is zero or less.

See also: `-take-last'."
  (declare (side-effect-free t))
  (--take-while (< it-index n) list))

(defun -take-last (n list)
  "Return a copy of the last N items of LIST in order.
Return a copy of LIST if it contains N items or fewer.
Return nil if N is zero or less.

See also: `-take'."
  (declare (side-effect-free t))
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
  (declare (side-effect-free t))
  (nbutlast (copy-sequence list) n))

(defun -split-at (n list)
  "Split LIST into two sublists after the Nth element.
The result is a list of two elements (TAKE DROP) where TAKE is a
new list of the first N elements of LIST, and DROP is the
remaining elements of LIST (not a copy).  TAKE and DROP are like
the results of `-take' and `-drop', respectively, but the split
is done in a single list traversal."
  (declare (side-effect-free t))
  (let (result)
    (--each-while list (< it-index n)
      (push (pop list) result))
    (list (nreverse result) list)))

(defun -rotate (n list)
  "Rotate LIST N places to the right (left if N is negative).
The time complexity is O(n)."
  (declare (pure t) (side-effect-free t))
  (cond ((null list) ())
        ((zerop n) (copy-sequence list))
        ((let* ((len (length list))
                (n-mod-len (mod n len))
                (new-tail-len (- len n-mod-len)))
           (append (nthcdr new-tail-len list) (-take new-tail-len list))))))

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
  "Use FUNC to update the Nth element of LIST.
Return a copy of LIST where the Nth element is replaced with the
result of calling FUNC on it.

See also: `-map-when'"
  (declare (important-return-value t))
  (let ((split-list (-split-at n list)))
    (nconc (car split-list)
           (cons (funcall func (car (cadr split-list)))
                 (cdr (cadr split-list))))))

(defmacro --update-at (n form list)
  "Anaphoric version of `-update-at'."
  (declare (debug (form def-form form)))
  `(-update-at ,n (lambda (it) (ignore it) ,form) ,list))

(defun -remove-at (n list)
  "Return LIST with its element at index N removed.
That is, remove any element selected as (nth N LIST) from LIST
and return the result.

This is a non-destructive operation: parts of LIST (but not
necessarily all of it) are copied as needed to avoid
destructively modifying it.

See also: `-remove-at-indices', `-remove'."
  (declare (pure t) (side-effect-free t))
  (if (zerop n)
      (cdr list)
    (--remove-first (= it-index n) list)))

(defun -remove-at-indices (indices list)
  "Return LIST with its elements at INDICES removed.
That is, for each index I in INDICES, remove any element selected
as (nth I LIST) from LIST.

This is a non-destructive operation: parts of LIST (but not
necessarily all of it) are copied as needed to avoid
destructively modifying it.

See also: `-remove-at', `-remove'."
  (declare (pure t) (side-effect-free t))
  (setq indices (--drop-while (< it 0) (-sort #'< indices)))
  (let ((i (pop indices)) res)
    (--each-while list i
      (pop list)
      (if (/= it-index i)
          (push it res)
        (while (and indices (= (car indices) i))
          (pop indices))
        (setq i (pop indices))))
    (nconc (nreverse res) list)))

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
  "Split LIST into a prefix satisfying PRED, and the rest.
The first sublist is the prefix of LIST with successive elements
satisfying PRED, and the second sublist is the remaining elements
that do not.  The result is like performing

  ((-take-while PRED LIST) (-drop-while PRED LIST))

but in no more than a single pass through LIST."
  (declare (important-return-value t))
  (--split-with (funcall pred it) list))

(defmacro -split-on (item list)
  "Split the LIST each time ITEM is found.

Unlike `-partition-by', the ITEM is discarded from the results.
Empty lists are also removed from the result.

Comparison is done by `equal'.

See also `-split-when'"
  (declare (debug (def-form form)))
  `(-split-when (lambda (it) (equal it ,item)) ,list))

(defmacro --split-when (form list)
  "Anaphoric version of `-split-when'."
  (declare (debug (def-form form)))
  `(-split-when (lambda (it) (ignore it) ,form) ,list))

(defun -split-when (fn list)
  "Split the LIST on each element where FN returns non-nil.

Unlike `-partition-by', the \"matched\" element is discarded from
the results.  Empty lists are also removed from the result.

This function can be thought of as a generalization of
`split-string'."
  (declare (important-return-value t))
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
  "Split LIST into two sublists based on whether items satisfy PRED.
The result is like performing

  ((-filter PRED LIST) (-remove PRED LIST))

but in a single pass through LIST."
  (declare (important-return-value t))
  (--separate (funcall pred it) list))

(defun dash--partition-all-in-steps-reversed (n step list)
  "Like `-partition-all-in-steps', but the result is reversed."
  (when (< step 1)
    (signal 'wrong-type-argument
            `("Step size < 1 results in juicy infinite loops" ,step)))
  (let (result)
    (while list
      (push (-take n list) result)
      (setq list (nthcdr step list)))
    result))

(defun -partition-all-in-steps (n step list)
  "Partition LIST into sublists of length N that are STEP items apart.
Adjacent groups may overlap if N exceeds the STEP stride.
Trailing groups may contain less than N items."
  (declare (pure t) (side-effect-free t))
  (nreverse (dash--partition-all-in-steps-reversed n step list)))

(defun -partition-in-steps (n step list)
  "Partition LIST into sublists of length N that are STEP items apart.
Like `-partition-all-in-steps', but if there are not enough items
to make the last group N-sized, those items are discarded."
  (declare (pure t) (side-effect-free t))
  (let ((result (dash--partition-all-in-steps-reversed n step list)))
    (while (and result (< (length (car result)) n))
      (pop result))
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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
  (--partition-by-header (funcall fn it) list))

(defmacro --partition-after-pred (form list)
  "Partition LIST after each element for which FORM evaluates to non-nil.
Each element of LIST in turn is bound to `it' before evaluating
FORM.

This is the anaphoric counterpart to `-partition-after-pred'."
  (let ((l (make-symbol "list"))
        (r (make-symbol "result"))
        (s (make-symbol "sublist")))
    `(let ((,l ,list) ,r ,s)
       (when ,l
         (--each ,l
           (push it ,s)
           (when ,form
             (push (nreverse ,s) ,r)
             (setq ,s ())))
         (when ,s
           (push (nreverse ,s) ,r))
         (nreverse ,r)))))

(defun -partition-after-pred (pred list)
  "Partition LIST after each element for which PRED returns non-nil.

This function's anaphoric counterpart is `--partition-after-pred'."
  (declare (important-return-value t))
  (--partition-after-pred (funcall pred it) list))

(defun -partition-before-pred (pred list)
  "Partition directly before each time PRED is true on an element of LIST."
  (declare (important-return-value t))
  (nreverse (-map #'reverse
                  (-partition-after-pred pred (reverse list)))))

(defun -partition-after-item (item list)
  "Partition directly after each time ITEM appears in LIST."
  (declare (pure t) (side-effect-free t))
  (-partition-after-pred (lambda (ele) (equal ele item))
                         list))

(defun -partition-before-item (item list)
  "Partition directly before each time ITEM appears in LIST."
  (declare (pure t) (side-effect-free t))
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
  (declare (important-return-value t))
  (--group-by (funcall fn it) list))

(defun -interpose (sep list)
  "Return a new list of all elements in LIST separated by SEP."
  (declare (side-effect-free t))
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
  (declare (side-effect-free t))
  (when lists
    (let (result)
      (while (-none? 'null lists)
        (--each lists (!cons (car it) result))
        (setq lists (-map 'cdr lists)))
      (nreverse result))))

(defmacro --zip-with (form list1 list2)
  "Zip LIST1 and LIST2 into a new list according to FORM.
That is, evaluate FORM for each item pair from the two lists, and
return the list of results.  The result is as long as the shorter
list.

Each element of LIST1 and each element of LIST2 in turn are bound
pairwise to `it' and `other', respectively, and their index
within the list to `it-index', before evaluating FORM.

This is the anaphoric counterpart to `-zip-with'."
  (declare (debug (form form form)))
  (let ((r (make-symbol "result"))
        (l2 (make-symbol "list2")))
    `(let ((,l2 ,list2) ,r)
       (--each-while ,list1 ,l2
         (let ((other (pop ,l2)))
           (ignore other)
           (push ,form ,r)))
       (nreverse ,r))))

(defun -zip-with (fn list1 list2)
  "Zip LIST1 and LIST2 into a new list using the function FN.
That is, apply FN pairwise taking as first argument the next
element of LIST1 and as second argument the next element of LIST2
at the corresponding position.  The result is as long as the
shorter list.

This function's anaphoric counterpart is `--zip-with'.

For other zips, see also `-zip-lists' and `-zip-fill'."
  (declare (important-return-value t))
  (--zip-with (funcall fn it other) list1 list2))

(defun -zip-lists (&rest lists)
  "Zip LISTS together.

Group the head of each list, followed by the second element of
each list, and so on.  The number of returned groupings is equal
to the length of the shortest input list, and the length of each
grouping is equal to the number of input LISTS.

The return value is always a list of proper lists, in contrast to
`-zip' which returns a list of dotted pairs when only two input
LISTS are provided.

See also: `-zip-pair'."
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (results)
      (while (--every it lists)
        (push (mapcar #'car lists) results)
        (setq lists (mapcar #'cdr lists)))
      (nreverse results))))

(defun -zip-lists-fill (fill-value &rest lists)
  "Zip LISTS together, padding shorter lists with FILL-VALUE.
This is like `-zip-lists' (which see), except it retains all
elements at positions beyond the end of the shortest list.  The
number of returned groupings is equal to the length of the
longest input list, and the length of each grouping is equal to
the number of input LISTS."
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (results)
      (while (--some it lists)
        (push (--map (if it (car it) fill-value) lists) results)
        (setq lists (mapcar #'cdr lists)))
      (nreverse results))))

(defun -unzip-lists (lists)
  "Unzip LISTS.

This works just like `-zip-lists' (which see), but takes a list
of lists instead of a variable number of arguments, such that

  (-unzip-lists (-zip-lists ARGS...))

is identity (given that the lists comprising ARGS are of the same
length)."
  (declare (pure t) (side-effect-free t))
  (apply #'-zip-lists lists))

(defalias 'dash--length=
  (if (fboundp 'length=)
      #'length=
    (lambda (list length)
      (cond ((< length 0) nil)
            ((zerop length) (null list))
            ((let ((last (nthcdr (1- length) list)))
               (and last (null (cdr last))))))))
  "Return non-nil if LIST is of LENGTH.
This is a compatibility shim for `length=' in Emacs 28.
\n(fn LIST LENGTH)")

(defun dash--zip-lists-or-pair (_form &rest lists)
  "Return a form equivalent to applying `-zip' to LISTS.
This `compiler-macro' warns about discouraged `-zip' usage and
delegates to `-zip-lists' or `-zip-pair' depending on the number
of LISTS."
  (if (not (dash--length= lists 2))
      (cons #'-zip-lists lists)
    (let ((pair (cons #'-zip-pair lists))
          (msg "Use -zip-pair instead of -zip to get a list of pairs"))
      (if (fboundp 'macroexp-warn-and-return)
          (macroexp-warn-and-return msg pair)
        (message msg)
        pair))))

(defun -zip (&rest lists)
  "Zip LISTS together.

Group the head of each list, followed by the second element of
each list, and so on.  The number of returned groupings is equal
to the length of the shortest input list, and the number of items
in each grouping is equal to the number of input LISTS.

If only two LISTS are provided as arguments, return the groupings
as a list of dotted pairs.  Otherwise, return the groupings as a
list of proper lists.

Since the return value changes form depending on the number of
arguments, it is generally recommended to use `-zip-lists'
instead, or `-zip-pair' if a list of dotted pairs is desired.

See also: `-unzip'."
  (declare (compiler-macro dash--zip-lists-or-pair)
           (pure t) (side-effect-free t))
  ;; For backward compatibility, return a list of dotted pairs if two
  ;; arguments were provided.
  (apply (if (dash--length= lists 2) #'-zip-pair #'-zip-lists) lists))

(defun -zip-pair (&rest lists)
  "Zip LIST1 and LIST2 together.

Make a pair with the head of each list, followed by a pair with
the second element of each list, and so on.  The number of pairs
returned is equal to the length of the shorter input list.

See also: `-zip-lists'."
  (declare (advertised-calling-convention (list1 list2) "2.20.0")
           (pure t) (side-effect-free t))
  (if (dash--length= lists 2)
      (--zip-with (cons it other) (car lists) (cadr lists))
    (apply #'-zip-lists lists)))

(defun -zip-fill (fill-value &rest lists)
  "Zip LISTS together, padding shorter lists with FILL-VALUE.
This is like `-zip' (which see), except it retains all elements
at positions beyond the end of the shortest list.  The number of
returned groupings is equal to the length of the longest input
list, and the length of each grouping is equal to the number of
input LISTS.

Since the return value changes form depending on the number of
arguments, it is generally recommended to use `-zip-lists-fill'
instead, unless a list of dotted pairs is explicitly desired."
  (declare (pure t) (side-effect-free t))
  (cond ((null lists) ())
        ((dash--length= lists 2)
         (let ((list1 (car lists))
               (list2 (cadr lists))
               results)
           (while (or list1 list2)
             (push (cons (if list1 (pop list1) fill-value)
                         (if list2 (pop list2) fill-value))
                   results))
           (nreverse results)))
        ((apply #'-zip-lists-fill fill-value lists))))

(defun -unzip (lists)
  "Unzip LISTS.

This works just like `-zip' (which see), but takes a list of
lists instead of a variable number of arguments, such that

  (-unzip (-zip L1 L2 L3 ...))

is identity (given that the lists are of the same length, and
that `-zip' is not called with two arguments, because of the
caveat described in its docstring).

Note in particular that calling `-unzip' on a list of two lists
will return a list of dotted pairs.

Since the return value changes form depending on the number of
LISTS, it is generally recommended to use `-unzip-lists' instead."
  (declare (pure t) (side-effect-free t))
  (apply #'-zip lists))

(defun -cycle (list)
  "Return an infinite circular copy of LIST.
The returned list cycles through the elements of LIST and repeats
from the beginning."
  (declare (pure t) (side-effect-free t))
  ;; Also works with sequences that aren't lists.
  (let ((newlist (append list ())))
    (nconc newlist newlist)))

(defun -pad (fill-value &rest lists)
  "Pad each of LISTS with FILL-VALUE until they all have equal lengths.

Ensure all LISTS are as long as the longest one by repeatedly
appending FILL-VALUE to the shorter lists, and return the
resulting LISTS."
  (declare (pure t) (side-effect-free t))
  (let* ((lens (mapcar #'length lists))
         (maxlen (apply #'max 0 lens)))
    (--map (append it (make-list (- maxlen (pop lens)) fill-value)) lists)))

(defmacro --annotate (form list)
  "Pair each item in LIST with the result of evaluating FORM.

Return an alist of (RESULT . ITEM), where each ITEM is the
corresponding element of LIST, and RESULT is the value obtained
by evaluating FORM with ITEM bound to `it'.

This is the anaphoric counterpart to `-annotate'."
  (declare (debug (form form)))
  `(--map (cons ,form it) ,list))

(defun -annotate (fn list)
  "Pair each item in LIST with the result of passing it to FN.

Return an alist of (RESULT . ITEM), where each ITEM is the
corresponding element of LIST, and RESULT is the value obtained
by calling FN on ITEM.

This function's anaphoric counterpart is `--annotate'."
  (declare (important-return-value t))
  (--annotate (funcall fn it) list))

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
  (declare (important-return-value t))
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
  (declare (important-return-value t))
  (let ((restore-lists (copy-sequence lists))
        (last-list (last lists))
        re)
    (while (car last-list)
      (let ((item (apply fn (-map 'car lists))))
        (push item re)
        (setcar lists (cdar lists)) ;; silence byte compiler
        (dash--table-carry lists restore-lists)))
    (nreverse re)))

(defmacro --find-index (form list)
  "Return the first index in LIST for which FORM evals to non-nil.
Return nil if no such index is found.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-find-index'."
  (declare (debug (form form)))
  `(--some (and ,form it-index) ,list))

(defun -find-index (pred list)
  "Return the index of the first item satisfying PRED in LIST.
Return nil if no such item is found.

PRED is called with one argument, the current list element, until
it returns non-nil, at which point the search terminates.

This function's anaphoric counterpart is `--find-index'.

See also: `-first', `-find-last-index'."
  (declare (important-return-value t))
  (--find-index (funcall pred it) list))

(defun -elem-index (elem list)
  "Return the first index of ELEM in LIST.
That is, the index within LIST of the first element that is
`equal' to ELEM.  Return nil if there is no such element.

See also: `-find-index'."
  (declare (pure t) (side-effect-free t))
  (--find-index (equal elem it) list))

(defmacro --find-indices (form list)
  "Return the list of indices in LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-find-indices'."
  (declare (debug (form form)))
  `(--keep (and ,form it-index) ,list))

(defun -find-indices (pred list)
  "Return the list of indices in LIST satisfying PRED.

Each element of LIST in turn is passed to PRED.  If the result is
non-nil, the index of that element in LIST is included in the
result.  The returned indices are in ascending order, i.e., in
the same order as they appear in LIST.

This function's anaphoric counterpart is `--find-indices'.

See also: `-find-index', `-elem-indices'."
  (declare (important-return-value t))
  (--find-indices (funcall pred it) list))

(defun -elem-indices (elem list)
  "Return the list of indices at which ELEM appears in LIST.
That is, the indices of all elements of LIST `equal' to ELEM, in
the same ascending order as they appear in LIST."
  (declare (pure t) (side-effect-free t))
  (--find-indices (equal elem it) list))

(defmacro --find-last-index (form list)
  "Return the last index in LIST for which FORM evals to non-nil.
Return nil if no such index is found.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-find-last-index'."
  (declare (debug (form form)))
  (let ((i (make-symbol "index")))
    `(let (,i)
       (--each ,list
         (when ,form (setq ,i it-index)))
       ,i)))

(defun -find-last-index (pred list)
  "Return the index of the last item satisfying PRED in LIST.
Return nil if no such item is found.

Predicate PRED is called with one argument each time, namely the
current list element.

This function's anaphoric counterpart is `--find-last-index'.

See also: `-last', `-find-index'."
  (declare (important-return-value t))
  (--find-last-index (funcall pred it) list))

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

(defmacro -some--> (expr &rest forms)
  "Thread EXPR through FORMS via `-->', while the result is non-nil.
When EXPR evaluates to non-nil, thread the result through the
first of FORMS, and when that result is non-nil, thread it
through the next form, etc."
  (declare (debug (form &rest &or symbolp consp)) (indent 1))
  (if (null forms) expr
    (let ((result (make-symbol "result")))
      `(-some--> (-when-let (,result ,expr)
                   (--> ,result ,(car forms)))
         ,@(cdr forms)))))

(defmacro -doto (init &rest forms)
  "Evaluate INIT and pass it as argument to FORMS with `->'.
The RESULT of evaluating INIT is threaded through each of FORMS
individually using `->', which see.  The return value is RESULT,
which FORMS may have modified by side effect."
  (declare (debug (form &rest &or symbolp consp)) (indent 1))
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
  (declare (important-return-value t))
  (->> (--map-indexed (cons it it-index) list)
       (-sort (lambda (it other) (funcall comparator (car it) (car other))))
       (mapcar #'cdr)))

(defun -grade-down (comparator list)
  "Grade elements of LIST using COMPARATOR relation.
This yields a permutation vector such that applying this
permutation to LIST sorts it in descending order."
  (declare (important-return-value t))
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

If the cdr of last cons cell in the list is nil, matching stops
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
    (-each (-zip-fill fill-placeholder (cdr pattern) (cddr pattern))
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
   ((and (symbolp match-form)
         ;; Don't bind things like &keys as if they were vars (#395).
         (not (functionp (dash--get-expand-function match-form))))
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
- a key \\='bar is converted into `bar' pattern,
- a key \"baz\" is converted into `baz' pattern.

That is, the entire value under the key is bound to the derived
variable without any further destructuring.

This is possible only when the form following the key is not a
valid pattern (i.e. not a symbol, a cons cell or a vector).
Otherwise the matching proceeds as usual and in case of an
invalid spec fails with an error.

Thus the patterns are normalized as follows:

   ;; derive all the missing patterns
   (&plist :foo \\='bar \"baz\") => (&plist :foo foo \\='bar bar \"baz\" baz)

   ;; we can specify some but not others
   (&plist :foo \\='bar explicit-bar) => (&plist :foo foo \\='bar explicit-bar)

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
           (new-varlist (--zip-with (list (car it) (car other))
                                    varlist inputs)))
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

;; TODO: Get rid of this dynamic variable, passing it as an argument
;; instead?
(defvar -compare-fn nil
  "Tests for equality use this function, or `equal' if this is nil.

As a dynamic variable, this should be temporarily bound around
the relevant operation, rather than permanently modified.  For
example:

  (let ((-compare-fn #\\='=))
    (-union \\='(1 2 3) \\='(2 3 4)))")

(defun dash--member-fn ()
  "Return the flavor of `member' that goes best with `-compare-fn'."
  (declare (side-effect-free error-free))
  (let ((cmp -compare-fn))
    (cond ((memq cmp '(nil equal)) #'member)
          ((eq cmp #'eq) #'memq)
          ((eq cmp #'eql) #'memql)
          ((lambda (elt list)
             (while (and list (not (funcall cmp elt (car list))))
               (pop list))
             list)))))

(defun dash--assoc-fn ()
  "Return the flavor of `assoc' that goes best with `-compare-fn'."
  (declare (side-effect-free error-free))
  (let ((cmp -compare-fn))
    (cond ((memq cmp '(nil equal)) #'assoc)
          ((eq cmp #'eq) #'assq)
          ;; Since Emacs 26, `assoc' accepts a custom `testfn'.
          ;; Version testing would be simpler here, but feature
          ;; testing gets more brownie points, I guess.
          ((condition-case nil
               (with-no-warnings (assoc nil () #'eql))
             (wrong-number-of-arguments t))
           (lambda (key alist)
             (--first (and (consp it) (funcall cmp (car it) key)) alist)))
          ((with-no-warnings
             (lambda (key alist)
               (assoc key alist cmp)))))))

(defun dash--hash-test-fn ()
  "Return the hash table test function corresponding to `-compare-fn'.
Return nil if `-compare-fn' is not a known test function."
  (declare (side-effect-free error-free))
  ;; In theory this could also recognize values that are custom
  ;; `hash-table-test's, but too often the :test name is different
  ;; from the equality function, so it doesn't seem worthwhile.
  (car (memq (or -compare-fn #'equal) '(equal eq eql))))

(defvar dash--short-list-length 32
  "Maximum list length considered short, for optimizations.
For example, the speedup afforded by hash table lookup may start
to outweigh its runtime and memory overhead for problem sizes
greater than this value.  See also the discussion in PR #305.")

(defun -distinct (list)
  "Return a copy of LIST with all duplicate elements removed.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil.

Alias: `-uniq'."
  (declare (important-return-value t))
  (let (test len)
    (cond ((null list) ())
          ;; Use a hash table if `-compare-fn' is a known hash table
          ;; test function and the list is long enough.
          ((and (setq test (dash--hash-test-fn))
                (> (setq len (length list)) dash--short-list-length))
           (let ((ht (make-hash-table :test test :size len)))
             (--filter (unless (gethash it ht) (puthash it t ht)) list)))
          ((let ((member (dash--member-fn)) uniq)
             (--each list (unless (funcall member it uniq) (push it uniq)))
             (nreverse uniq))))))

(defalias '-uniq #'-distinct)

(defun dash--size+ (size1 size2)
  "Return the sum of nonnegative fixnums SIZE1 and SIZE2.
Return `most-positive-fixnum' on overflow.  This ensures the
result is a valid size, particularly for allocating hash tables,
even in the presence of bignum support."
  (declare (side-effect-free t))
  (if (< size1 (- most-positive-fixnum size2))
      (+ size1 size2)
    most-positive-fixnum))

(defun -union (list1 list2)
  "Return a new list of distinct elements appearing in either LIST1 or LIST2.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil."
  (declare (important-return-value t))
  (let ((lists (list list1 list2)) test len union)
    (cond ((null (or list1 list2)))
          ;; Use a hash table if `-compare-fn' is a known hash table
          ;; test function and the lists are long enough.
          ((and (setq test (dash--hash-test-fn))
                (> (setq len (dash--size+ (length list1) (length list2)))
                   dash--short-list-length))
           (let ((ht (make-hash-table :test test :size len)))
             (dolist (l lists)
               (--each l (unless (gethash it ht)
                           (puthash it t ht)
                           (push it union))))))
          ((let ((member (dash--member-fn)))
             (dolist (l lists)
               (--each l (unless (funcall member it union) (push it union)))))))
    (nreverse union)))

(defun -intersection (list1 list2)
  "Return a new list of distinct elements appearing in both LIST1 and LIST2.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil."
  (declare (important-return-value t))
  (let (test len)
    (cond ((null (and list1 list2)) ())
          ;; Use a hash table if `-compare-fn' is a known hash table
          ;; test function and either list is long enough.
          ((and (setq test (dash--hash-test-fn))
                (> (setq len (length list2)) dash--short-list-length))
           (let ((ht (make-hash-table :test test :size len)))
             (--each list2 (puthash it t ht))
             ;; Remove visited elements to avoid duplicates.
             (--filter (when (gethash it ht) (remhash it ht) t) list1)))
          ((let ((member (dash--member-fn)) intersection)
             (--each list1 (and (funcall member it list2)
                                (not (funcall member it intersection))
                                (push it intersection)))
             (nreverse intersection))))))

(defun -difference (list1 list2)
  "Return a new list with the distinct members of LIST1 that are not in LIST2.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil."
  (declare (important-return-value t))
  (let (test len1 len2)
    (cond ((null list1) ())
          ((null list2) (-distinct list1))
          ;; Use a hash table if `-compare-fn' is a known hash table
          ;; test function and the subtrahend is long enough.
          ((and (setq test (dash--hash-test-fn))
                (setq len1 (length list1))
                (setq len2 (length list2))
                (> (max len1 len2) dash--short-list-length))
           (let ((ht1 (make-hash-table :test test :size len1))
                 (ht2 (make-hash-table :test test :size len2)))
             (--each list2 (puthash it t ht2))
             ;; Avoid duplicates by tracking visited items in `ht1'.
             (--filter (unless (or (gethash it ht2) (gethash it ht1))
                         (puthash it t ht1))
                       list1)))
          ((let ((member (dash--member-fn)) difference)
             (--each list1
               (unless (or (funcall member it list2)
                           (funcall member it difference))
                 (push it difference)))
             (nreverse difference))))))

(defun -powerset (list)
  "Return the power set of LIST."
  (declare (pure t) (side-effect-free t))
  (if (null list) (list ())
    (let ((last (-powerset (cdr list))))
      (nconc (mapcar (lambda (x) (cons (car list) x)) last)
             last))))

(defun -frequencies (list)
  "Count the occurrences of each distinct element of LIST.

Return an alist of (ELEMENT . N), where each ELEMENT occurs N
times in LIST.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil.

See also `-count' and `-group-by'."
  (declare (important-return-value t))
  (let (test len freqs)
    (cond ((null list))
          ((and (setq test (dash--hash-test-fn))
                (> (setq len (length list)) dash--short-list-length))
           (let ((ht (make-hash-table :test test :size len)))
             ;; Share structure between hash table and returned list.
             ;; This affords a single pass that preserves the input
             ;; order, conses less garbage, and is faster than a
             ;; second traversal (e.g., with `maphash').
             (--each list
               (let ((freq (gethash it ht)))
                 (if freq
                     (setcdr freq (1+ (cdr freq)))
                   (push (puthash it (cons it 1) ht) freqs))))))
          ((let ((assoc (dash--assoc-fn)))
             (--each list
               (let ((freq (funcall assoc it freqs)))
                 (if freq
                     (setcdr freq (1+ (cdr freq)))
                   (push (cons it 1) freqs)))))))
    (nreverse freqs)))

(defun dash--numbers<= (nums)
  "Return non-nil if NUMS is a list of non-decreasing numbers."
  (declare (pure t) (side-effect-free t))
  (or (null nums)
      (let ((prev (pop nums)))
        (and (numberp prev)
             (--every (and (numberp it) (<= prev (setq prev it))) nums)))))

(defun dash--next-lex-perm (array n)
  "Update ARRAY of N numbers with its next lexicographic permutation.
Return nil if there is no such successor.  N should be nonzero.

This implements the salient steps of Algorithm L (Lexicographic
permutation generation) as described in DE Knuth's The Art of
Computer Programming, Volume 4A / Combinatorial Algorithms,
Part I, Addison-Wesley, 2011, § 7.2.1.2, p. 319."
  (setq n (1- n))
  (let* ((l n)
         (j (1- n))
         (al (aref array n))
         (aj al))
    ;; L2. [Find j].
    ;; Decrement j until a[j] < a[j+1].
    (while (and (<= 0 j)
                (<= aj (setq aj (aref array j))))
      (setq j (1- j)))
    ;; Terminate algorithm if j not found.
    (when (>= j 0)
      ;; L3. [Increase a[j]].
      ;; Decrement l until a[j] < a[l].
      (while (>= aj al)
        (setq l (1- l) al (aref array l)))
      ;; Swap a[j] and a[l].
      (aset array j al)
      (aset array l aj)
      ;; L4. [Reverse a[j+1]...a[n]].
      (setq l n)
      (while (< (setq j (1+ j)) l)
        (setq aj (aref array j))
        (aset array j (aref array l))
        (aset array l aj)
        (setq l (1- l)))
      array)))

(defun dash--lex-perms (vec &optional original)
  "Return a list of permutations of VEC in lexicographic order.
Specifically, return only the successors of VEC in lexicographic
order.  Each returned permutation is a list.  VEC should comprise
one or more numbers, and may be destructively modified.

If ORIGINAL is a vector, then VEC is interpreted as a set of
indices into ORIGINAL.  In this case, the indices are permuted,
and the resulting index permutations are used to dereference
elements of ORIGINAL."
  (let ((len (length vec)) perms)
    (while vec
      (push (if original
                (--map (aref original it) vec)
              (append vec ()))
            perms)
      (setq vec (dash--next-lex-perm vec len)))
    (nreverse perms)))

(defun dash--uniq-perms (list)
  "Return a list of permutations of LIST.
LIST is treated as if all its elements are distinct."
  (let* ((vec (vconcat list))
         (idxs (copy-sequence vec)))
    ;; Just construct a vector of the list's indices and permute that.
    (dotimes (i (length idxs))
      (aset idxs i i))
    (dash--lex-perms idxs vec)))

(defun dash--multi-perms (list freqs)
  "Return a list of permutations of the multiset LIST.
FREQS should be an alist describing the frequency of each element
in LIST, as returned by `-frequencies'."
  (let (;; Distinct items in `list', aka the cars of `freqs'.
        (uniq (make-vector (length freqs) nil))
        ;; Indices into `uniq'.
        (idxs (make-vector (length list) nil))
        ;; Current index into `idxs'.
        (i 0))
    (--each freqs
      (aset uniq it-index (car it))
      ;; Populate `idxs' with as many copies of each `it-index' as
      ;; there are corresponding duplicates.
      (dotimes (_ (cdr it))
        (aset idxs i it-index)
        (setq i (1+ i))))
    (dash--lex-perms idxs uniq)))

(defun -permutations (list)
  "Return the distinct permutations of LIST.

Duplicate elements of LIST are determined by `equal', or by
`-compare-fn' if that is non-nil."
  (declare (important-return-value t))
  (cond ((null list) (list ()))
        ;; Optimization: a traversal of `list' is faster than the
        ;; round trip via `dash--uniq-perms' or `dash--multi-perms'.
        ((dash--numbers<= list)
         (dash--lex-perms (vconcat list)))
        ((let ((freqs (-frequencies list)))
           ;; Is each element distinct?
           (unless (--every (= (cdr it) 1) freqs)
             (dash--multi-perms list freqs))))
        ((dash--uniq-perms list))))

(defun -inits (list)
  "Return all prefixes of LIST."
  (declare (pure t) (side-effect-free t))
  (let ((res (list list)))
    (setq list (reverse list))
    (while list
      (push (reverse (!cdr list)) res))
    res))

(defun -tails (list)
  "Return all suffixes of LIST."
  (declare (pure t) (side-effect-free t))
  (-reductions-r-from #'cons nil list))

(defun -common-prefix (&rest lists)
  "Return the longest common prefix of LISTS."
  (declare (pure t) (side-effect-free t))
  (--reduce (--take-while (and acc (equal (pop acc) it)) it)
            lists))

(defun -common-suffix (&rest lists)
  "Return the longest common suffix of LISTS."
  (declare (pure t) (side-effect-free t))
  (nreverse (apply #'-common-prefix (mapcar #'reverse lists))))

(defun -contains? (list element)
  "Return non-nil if LIST contains ELEMENT.

The test for equality is done with `equal', or with `-compare-fn'
if that is non-nil.  As with `member', the return value is
actually the tail of LIST whose car is ELEMENT.

Alias: `-contains-p'."
  (declare (important-return-value t))
  (funcall (dash--member-fn) element list))

(defalias '-contains-p #'-contains?)

(defun -same-items? (list1 list2)
  "Return non-nil if LIST1 and LIST2 have the same distinct elements.

The order of the elements in the lists does not matter.  The
lists may be of different lengths, i.e., contain duplicate
elements.  The test for equality is done with `equal', or with
`-compare-fn' if that is non-nil.

Alias: `-same-items-p'."
  (declare (important-return-value t))
  (let (test len1 len2)
    (cond ((null (or list1 list2)))
          ((null (and list1 list2)) nil)
          ;; Use a hash table if `-compare-fn' is a known hash table
          ;; test function and either list is long enough.
          ((and (setq test (dash--hash-test-fn))
                (setq len1 (length list1))
                (setq len2 (length list2))
                (> (max len1 len2) dash--short-list-length))
           (let ((ht1 (make-hash-table :test test :size len1))
                 (ht2 (make-hash-table :test test :size len2)))
             (--each list1 (puthash it t ht1))
             ;; Move visited elements from `ht1' to `ht2'.  This way,
             ;; if visiting all of `list2' leaves `ht1' empty, then
             ;; all elements from both lists have been accounted for.
             (and (--every (cond ((gethash it ht1)
                                  (remhash it ht1)
                                  (puthash it t ht2))
                                 ((gethash it ht2)))
                           list2)
                  (zerop (hash-table-count ht1)))))
          ((let ((member (dash--member-fn)))
             (and (--all? (funcall member it list2) list1)
                  (--all? (funcall member it list1) list2)))))))

(defalias '-same-items-p #'-same-items?)

(defun -is-prefix? (prefix list)
  "Return non-nil if PREFIX is a prefix of LIST.

Alias: `-is-prefix-p'."
  (declare (pure t) (side-effect-free t))
  (--each-while list (and (equal (car prefix) it)
                          (!cdr prefix)))
  (null prefix))

(defun -is-suffix? (suffix list)
  "Return non-nil if SUFFIX is a suffix of LIST.

Alias: `-is-suffix-p'."
  (declare (pure t) (side-effect-free t))
  (equal suffix (last list (length suffix))))

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
  (declare (important-return-value t))
  (sort (copy-sequence list) comparator))

(defmacro --sort (form list)
  "Anaphoric form of `-sort'."
  (declare (debug (def-form form)))
  `(-sort (lambda (it other) (ignore it other) ,form) ,list))

(defun -list (&optional arg &rest args)
  "Ensure ARG is a list.
If ARG is already a list, return it as is (not a copy).
Otherwise, return a new list with ARG as its only element.

Another supported calling convention is (-list &rest ARGS).
In this case, if ARG is not a list, a new list with all of
ARGS as elements is returned.  This use is supported for
backward compatibility and is otherwise deprecated."
  (declare (advertised-calling-convention (arg) "2.18.0")
           (pure t) (side-effect-free error-free))
  (if (listp arg) arg (cons arg args)))

(defun -repeat (n x)
  "Return a new list of length N with each element being X.
Return nil if N is less than 1."
  (declare (side-effect-free t))
  (and (>= n 0) (make-list n x)))

(defun -sum (list)
  "Return the sum of LIST."
  (declare (pure t) (side-effect-free t))
  (apply #'+ list))

(defun -running-sum (list)
  "Return a list with running sums of items in LIST.
LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (or list (signal 'wrong-type-argument (list #'consp list)))
  (-reductions #'+ list))

(defun -product (list)
  "Return the product of LIST."
  (declare (pure t) (side-effect-free t))
  (apply #'* list))

(defun -running-product (list)
  "Return a list with running products of items in LIST.
LIST must be non-empty."
  (declare (pure t) (side-effect-free t))
  (or list (signal 'wrong-type-argument (list #'consp list)))
  (-reductions #'* list))

(defun -max (list)
  "Return the largest value from LIST of numbers or markers."
  (declare (pure t) (side-effect-free t))
  (apply #'max list))

(defun -min (list)
  "Return the smallest value from LIST of numbers or markers."
  (declare (pure t) (side-effect-free t))
  (apply #'min list))

(defun -max-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the greatest element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (declare (important-return-value t))
  (--reduce (if (funcall comparator it acc) it acc) list))

(defun -min-by (comparator list)
  "Take a comparison function COMPARATOR and a LIST and return
the least element of the list by the comparison function.

See also combinator `-on' which can transform the values before
comparing them."
  (declare (important-return-value t))
  (--reduce (if (funcall comparator it acc) acc it) list))

(defmacro --max-by (form list)
  "Anaphoric version of `-max-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (def-form form)))
  `(-max-by (lambda (it other) (ignore it other) ,form) ,list))

(defmacro --min-by (form list)
  "Anaphoric version of `-min-by'.

The items for the comparator form are exposed as \"it\" and \"other\"."
  (declare (debug (def-form form)))
  `(-min-by (lambda (it other) (ignore it other) ,form) ,list))

(defun -iota (count &optional start step)
  "Return a list containing COUNT numbers.
Starts from START and adds STEP each time.  The default START is
zero, the default STEP is 1.
This function takes its name from the corresponding primitive in
the APL language."
  (declare (side-effect-free t))
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
  (declare (important-return-value t))
  (let ((re (funcall fn list)))
    (while (not (equal list re))
      (setq list re)
      (setq re (funcall fn re)))
    re))

(defmacro --fix (form list)
  "Anaphoric form of `-fix'."
  (declare (debug (def-form form)))
  `(-fix (lambda (it) (ignore it) ,form) ,list))

(defun -unfold (fun seed)
  "Build a list from SEED using FUN.

This is \"dual\" operation to `-reduce-r': while -reduce-r
consumes a list to produce a single value, `-unfold' takes a
seed value and builds a (potentially infinite!) list.

FUN should return nil to stop the generating process, or a
cons (A . B), where A will be prepended to the result and B is
the new seed."
  (declare (important-return-value t))
  (let ((last (funcall fun seed)) r)
    (while last
      (push (car last) r)
      (setq last (funcall fun (cdr last))))
    (nreverse r)))

(defmacro --unfold (form seed)
  "Anaphoric version of `-unfold'."
  (declare (debug (def-form form)))
  `(-unfold (lambda (it) (ignore it) ,form) ,seed))

(defun -cons-pair? (obj)
  "Return non-nil if OBJ is a true cons pair.
That is, a cons (A . B) where B is not a list.

Alias: `-cons-pair-p'."
  (declare (pure t) (side-effect-free error-free))
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
  (if (-cons-pair? val) (-cons-to-list val) (list val)))

(defun -tree-mapreduce-from (fn folder init-value tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce-from' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ((-cons-pair? tree) (funcall fn tree))
   ((consp tree)
    (-reduce-r-from
     folder init-value
     (mapcar (lambda (x) (-tree-mapreduce-from fn folder init-value x)) tree)))
   ((funcall fn tree))))

(defmacro --tree-mapreduce-from (form folder init-value tree)
  "Anaphoric form of `-tree-mapreduce-from'."
  (declare (debug (def-form def-form form form)))
  `(-tree-mapreduce-from (lambda (it) (ignore it) ,form)
                         (lambda (it acc) (ignore it acc) ,folder)
                         ,init-value
                         ,tree))

(defun -tree-mapreduce (fn folder tree)
  "Apply FN to each element of TREE, and make a list of the results.
If elements of TREE are lists themselves, apply FN recursively to
elements of these nested lists.

Then reduce the resulting lists using FOLDER and initial value
INIT-VALUE. See `-reduce-r-from'.

This is the same as calling `-tree-reduce' after `-tree-map'
but is twice as fast as it only traverse the structure once."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ((-cons-pair? tree) (funcall fn tree))
   ((consp tree)
    (-reduce-r folder (mapcar (lambda (x) (-tree-mapreduce fn folder x)) tree)))
   ((funcall fn tree))))

(defmacro --tree-mapreduce (form folder tree)
  "Anaphoric form of `-tree-mapreduce'."
  (declare (debug (def-form def-form form)))
  `(-tree-mapreduce (lambda (it) (ignore it) ,form)
                    (lambda (it acc) (ignore it acc) ,folder)
                    ,tree))

(defun -tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ((-cons-pair? tree) (funcall fn tree))
   ((consp tree)
    (mapcar (lambda (x) (-tree-map fn x)) tree))
   ((funcall fn tree))))

(defmacro --tree-map (form tree)
  "Anaphoric form of `-tree-map'."
  (declare (debug (def-form form)))
  `(-tree-map (lambda (it) (ignore it) ,form) ,tree))

(defun -tree-reduce-from (fn init-value tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to INIT-VALUE and first element of the list,
then on this result and second element from the list etc.

The initial value is ignored on cons pairs as they always contain
two elements."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ((-cons-pair? tree) tree)
   ((consp tree)
    (-reduce-r-from
     fn init-value
     (mapcar (lambda (x) (-tree-reduce-from fn init-value x)) tree)))
   (tree)))

(defmacro --tree-reduce-from (form init-value tree)
  "Anaphoric form of `-tree-reduce-from'."
  (declare (debug (def-form form form)))
  `(-tree-reduce-from (lambda (it acc) (ignore it acc) ,form)
                      ,init-value ,tree))

(defun -tree-reduce (fn tree)
  "Use FN to reduce elements of list TREE.
If elements of TREE are lists themselves, apply the reduction recursively.

FN is first applied to first element of the list and second
element, then on this result and third element from the list etc.

See `-reduce-r' for how exactly are lists of zero or one element handled."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ((-cons-pair? tree) tree)
   ((consp tree)
    (-reduce-r fn (mapcar (lambda (x) (-tree-reduce fn x)) tree)))
   (tree)))

(defmacro --tree-reduce (form tree)
  "Anaphoric form of `-tree-reduce'."
  (declare (debug (def-form form)))
  `(-tree-reduce (lambda (it acc) (ignore it acc) ,form) ,tree))

(defun -tree-map-nodes (pred fun tree)
  "Call FUN on each node of TREE that satisfies PRED.

If PRED returns nil, continue descending down this node.  If PRED
returns non-nil, apply FUN to this node and do not descend
further."
  (cond ((funcall pred tree) (funcall fun tree))
        ((and (listp tree) (listp (cdr tree)))
         (-map (lambda (x) (-tree-map-nodes pred fun x)) tree))
        (tree)))

(defmacro --tree-map-nodes (pred form tree)
  "Anaphoric form of `-tree-map-nodes'."
  (declare (debug (def-form def-form form)))
  `(-tree-map-nodes (lambda (it) (ignore it) ,pred)
                    (lambda (it) (ignore it) ,form)
                    ,tree))

(defun -tree-seq (branch children tree)
  "Return a sequence of the nodes in TREE, in depth-first search order.

BRANCH is a predicate of one argument that returns non-nil if the
passed argument is a branch, that is, a node that can have children.

CHILDREN is a function of one argument that returns the children
of the passed branch node.

Non-branch nodes are simply copied."
  (declare (important-return-value t))
  (cons tree
        (and (funcall branch tree)
             (-mapcat (lambda (x) (-tree-seq branch children x))
                      (funcall children tree)))))

(defmacro --tree-seq (branch children tree)
  "Anaphoric form of `-tree-seq'."
  (declare (debug (def-form def-form form)))
  `(-tree-seq (lambda (it) (ignore it) ,branch)
              (lambda (it) (ignore it) ,children)
              ,tree))

(defun -clone (list)
  "Create a deep copy of LIST.
The new list has the same elements and structure but all cons are
replaced with new ones.  This is useful when you need to clone a
structure such as plist or alist."
  (declare (side-effect-free t))
  (-tree-map #'identity list))

;;; Combinators

(defalias '-partial #'apply-partially)

(defun -rpartial (fn &rest args)
  "Return a function that is a partial application of FN to ARGS.
ARGS is a list of the last N arguments to pass to FN.  The result
is a new function which does the same as FN, except that the last
N arguments are fixed at the values with which this function was
called.  This is like `-partial', except the arguments are fixed
starting from the right rather than the left."
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest args-before) (apply fn (append args-before args))))

(defun -juxt (&rest fns)
  "Return a function that is the juxtaposition of FNS.
The returned function takes a variable number of ARGS, applies
each of FNS in turn to ARGS, and returns the list of results."
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest args) (mapcar (lambda (x) (apply x args)) fns)))

(defun -compose (&rest fns)
  "Compose FNS into a single composite function.
Return a function that takes a variable number of ARGS, applies
the last function in FNS to ARGS, and returns the result of
calling each remaining function on the result of the previous
function, right-to-left.  If no FNS are given, return a variadic
`identity' function."
  (declare (pure t) (side-effect-free error-free))
  (let* ((fns (nreverse fns))
         (head (car fns))
         (tail (cdr fns)))
    (cond (tail
           (lambda (&rest args)
             (--reduce-from (funcall it acc) (apply head args) tail)))
          (fns head)
          ((lambda (&optional arg &rest _) arg)))))

(defun -applify (fn)
  "Return a function that applies FN to a single list of args.
This changes the arity of FN from taking N distinct arguments to
taking 1 argument which is a list of N arguments."
  (declare (pure t) (side-effect-free error-free))
  (lambda (args) (apply fn args)))

(defun -on (op trans)
  "Return a function that calls TRANS on each arg and OP on the results.
The returned function takes a variable number of arguments, calls
the function TRANS on each one in turn, and then passes those
results as the list of arguments to OP, in the same order.

For example, the following pairs of expressions are morally
equivalent:

  (funcall (-on #\\='+ #\\='1+) 1 2 3) = (+ (1+ 1) (1+ 2) (1+ 3))
  (funcall (-on #\\='+ #\\='1+))       = (+)"
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest args)
    ;; This unrolling seems to be a relatively cheap way to keep the
    ;; overhead of `mapcar' + `apply' in check.
    (cond ((cddr args)
           (apply op (mapcar trans args)))
          ((cdr args)
           (funcall op (funcall trans (car args)) (funcall trans (cadr args))))
          (args
           (funcall op (funcall trans (car args))))
          ((funcall op)))))

(defun -flip (fn)
  "Return a function that calls FN with its arguments reversed.
The returned function takes the same number of arguments as FN.

For example, the following two expressions are morally
equivalent:

  (funcall (-flip #\\='-) 1 2) = (- 2 1)

See also: `-rotate-args'."
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest args) ;; Open-code for speed.
    (cond ((cddr args) (apply fn (nreverse args)))
          ((cdr args) (funcall fn (cadr args) (car args)))
          (args (funcall fn (car args)))
          ((funcall fn)))))

(defun -rotate-args (n fn)
  "Return a function that calls FN with args rotated N places to the right.
The returned function takes the same number of arguments as FN,
rotates the list of arguments N places to the right (left if N is
negative) just like `-rotate', and applies FN to the result.

See also: `-flip'."
  (declare (pure t) (side-effect-free t))
  (if (zerop n)
      fn
    (let ((even (= (% n 2) 0)))
      (lambda (&rest args)
        (cond ((cddr args) ;; Open-code for speed.
               (apply fn (-rotate n args)))
              ((cdr args)
               (let ((fst (car args))
                     (snd (cadr args)))
                 (funcall fn (if even fst snd) (if even snd fst))))
              (args
               (funcall fn (car args)))
              ((funcall fn)))))))

(defun -const (c)
  "Return a function that returns C ignoring any additional arguments.

In types: a -> b -> a"
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest _) c))

(defmacro -cut (&rest params)
  "Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See SRFI-26 for detailed description."
  (declare (debug (&optional sexp &rest &or "<>" form)))
  (let* ((i 0)
         (args (--keep (when (eq it '<>)
                         (setq i (1+ i))
                         (make-symbol (format "D%d" i)))
                       params)))
    `(lambda ,args
       ,(let ((body (--map (if (eq it '<>) (pop args) it) params)))
          (if (eq (car params) '<>)
              (cons #'funcall body)
            body)))))

(defun -not (pred)
  "Return a predicate that negates the result of PRED.
The returned predicate passes its arguments to PRED.  If PRED
returns nil, the result is non-nil; otherwise the result is nil.

See also: `-andfn' and `-orfn'."
  (declare (pure t) (side-effect-free error-free))
  (lambda (&rest args) (not (apply pred args))))

(defun -orfn (&rest preds)
  "Return a predicate that returns the first non-nil result of PREDS.
The returned predicate takes a variable number of arguments,
passes them to each predicate in PREDS in turn until one of them
returns non-nil, and returns that non-nil result without calling
the remaining PREDS.  If all PREDS return nil, or if no PREDS are
given, the returned predicate returns nil.

See also: `-andfn' and `-not'."
  (declare (pure t) (side-effect-free error-free))
  ;; Open-code for speed.
  (cond ((cdr preds) (lambda (&rest args) (--some (apply it args) preds)))
        (preds (car preds))
        (#'ignore)))

(defun -andfn (&rest preds)
  "Return a predicate that returns non-nil if all PREDS do so.
The returned predicate P takes a variable number of arguments and
passes them to each predicate in PREDS in turn.  If any one of
PREDS returns nil, P also returns nil without calling the
remaining PREDS.  If all PREDS return non-nil, P returns the last
such value.  If no PREDS are given, P always returns non-nil.

See also: `-orfn' and `-not'."
  (declare (pure t) (side-effect-free error-free))
  ;; Open-code for speed.
  (cond ((cdr preds) (lambda (&rest args) (--every (apply it args) preds)))
        (preds (car preds))
        ;; As a `pure' function, this runtime check may generate
        ;; backward-incompatible bytecode for `(-andfn)' at compile-time,
        ;; but I doubt that's a problem in practice (famous last words).
        ((fboundp 'always) #'always)
        ((lambda (&rest _) t))))

(defun -iteratefn (fn n)
  "Return a function FN composed N times with itself.

FN is a unary function.  If you need to use a function of higher
arity, use `-applify' first to turn it into a unary function.

With n = 0, this acts as identity function.

In types: (a -> a) -> Int -> a -> a.

This function satisfies the following law:

  (funcall (-iteratefn fn n) init) = (-last-item (-iterate fn init (1+ n)))."
  (declare (pure t) (side-effect-free error-free))
  (lambda (x) (--dotimes n (setq x (funcall fn x))) x))

(defun -counter (&optional beg end inc)
  "Return a closure that counts from BEG to END, with increment INC.

The closure will return the next value in the counting sequence
each time it is called, and nil after END is reached. BEG
defaults to 0, INC defaults to 1, and if END is nil, the counter
will increment indefinitely.

The closure accepts any number of arguments, which are discarded."
  (declare (pure t) (side-effect-free error-free))
  (let ((inc (or inc 1))
        (n (or beg 0)))
    (lambda (&rest _)
      (when (or (not end) (< n end))
        (prog1 n
          (setq n (+ n inc)))))))

(defvar -fixfn-max-iterations 1000
  "The default maximum number of iterations performed by `-fixfn'
  unless otherwise specified.")

(defun -fixfn (fn &optional equal-test halt-test)
  "Return a function that computes the (least) fixpoint of FN.

FN must be a unary function. The returned lambda takes a single
argument, X, the initial value for the fixpoint iteration. The
iteration halts when either of the following conditions is satisfied:

 1. Iteration converges to the fixpoint, with equality being
    tested using EQUAL-TEST. If EQUAL-TEST is not specified,
    `equal' is used. For functions over the floating point
    numbers, it may be necessary to provide an appropriate
    approximate comparison test.

 2. HALT-TEST returns a non-nil value. HALT-TEST defaults to a
    simple counter that returns t after `-fixfn-max-iterations',
    to guard against infinite iteration. Otherwise, HALT-TEST
    must be a function that accepts a single argument, the
    current value of X, and returns non-nil as long as iteration
    should continue. In this way, a more sophisticated
    convergence test may be supplied by the caller.

The return value of the lambda is either the fixpoint or, if
iteration halted before converging, a cons with car `halted' and
cdr the final output from HALT-TEST.

In types: (a -> a) -> a -> a."
  (declare (important-return-value t))
  (let ((eqfn   (or equal-test 'equal))
        (haltfn (or halt-test
                    (-not
                     (-counter 0 -fixfn-max-iterations)))))
    (lambda (x)
      (let ((re (funcall fn x))
            (halt? (funcall haltfn x)))
        (while (and (not halt?) (not (funcall eqfn x re)))
          (setq x     re
                re    (funcall fn re)
                halt? (funcall haltfn re)))
        (if halt? (cons 'halted halt?)
          re)))))

(defun -prodfn (&rest fns)
  "Return a function that applies each of FNS to each of a list of arguments.

Takes a list of N functions and returns a function that takes a
list of length N, applying Ith function to Ith element of the
input list.  Returns a list of length N.

In types (for N=2): ((a -> b), (c -> d)) -> (a, c) -> (b, d)

This function satisfies the following laws:

    (-compose (-prodfn f g ...)
              (-prodfn f\\=' g\\=' ...))
  = (-prodfn (-compose f f\\=')
             (-compose g g\\=')
             ...)

    (-prodfn f g ...)
  = (-juxt (-compose f (-partial #\\='nth 0))
           (-compose g (-partial #\\='nth 1))
           ...)

    (-compose (-prodfn f g ...)
              (-juxt f\\=' g\\=' ...))
  = (-juxt (-compose f f\\=')
           (-compose g g\\=')
           ...)

    (-compose (-partial #\\='nth n)
              (-prod f1 f2 ...))
  = (-compose fn (-partial #\\='nth n))"
  (declare (pure t) (side-effect-free t))
  (lambda (x) (--zip-with (funcall it other) fns x)))

;;; Font lock

(defvar dash--keywords
  `(;; TODO: Do not fontify the following automatic variables
    ;; globally; detect and limit to their local anaphoric scope.
    (,(rx symbol-start (| "acc" "it" "it-index" "other") symbol-end)
     0 font-lock-variable-name-face)
    ;; Macros in dev/examples.el.  Based on `lisp-mode-symbol-regexp'.
    (,(rx ?\( (group (| "defexamples" "def-example-group")) symbol-end
          (+ (in "\t "))
          (group (* (| (syntax word) (syntax symbol) (: ?\\ nonl)))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ;; Symbols in dev/examples.el.
    ,(rx symbol-start (| "=>" "~>" "!!>") symbol-end)
    ;; Elisp macro fontification was static prior to Emacs 25.
    ,@(when (< emacs-major-version 25)
        (let ((macs '("!cdr"
                      "!cons"
                      "-->"
                      "--all-p"
                      "--all?"
                      "--annotate"
                      "--any"
                      "--any-p"
                      "--any?"
                      "--count"
                      "--dotimes"
                      "--doto"
                      "--drop-while"
                      "--each"
                      "--each-indexed"
                      "--each-r"
                      "--each-r-while"
                      "--each-while"
                      "--every"
                      "--every-p"
                      "--every?"
                      "--filter"
                      "--find"
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
                      "--none-p"
                      "--none?"
                      "--only-some-p"
                      "--only-some?"
                      "--partition-after-pred"
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
                      "--reject"
                      "--reject-first"
                      "--reject-last"
                      "--remove"
                      "--remove-first"
                      "--remove-last"
                      "--replace-where"
                      "--select"
                      "--separate"
                      "--some"
                      "--some-p"
                      "--some?"
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
                      "-cut"
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
  :lighter dash-fontify-mode-lighter
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
  dash-fontify-mode dash--turn-on-fontify-mode)

(defcustom dash-enable-fontlock nil
  "If non-nil, fontify Dash macro calls and special variables."
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
