;;; seq-24.el --- seq.el implementation for Emacs 24.x -*- lexical-binding: t -*-

;; Copyright (C) 2014-2023 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.

;;; Code:

(defmacro seq-doseq (spec &rest body)
  "Loop over a sequence.
Similar to `dolist' but can be applied to lists, strings, and vectors.

Evaluate BODY with VAR bound to each element of SEQ, in turn.

\(fn (VAR SEQ) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (let ((length (make-symbol "length"))
        (seq (make-symbol "seq"))
        (index (make-symbol "index")))
    `(let* ((,seq ,(cadr spec))
            (,length (if (listp ,seq) nil (seq-length ,seq)))
            (,index (if ,length 0 ,seq)))
       (while (if ,length
                  (< ,index ,length)
                (consp ,index))
         (let ((,(car spec) (if ,length
                                (prog1 (seq-elt ,seq ,index)
                                  (setq ,index (+ ,index 1)))
                              (pop ,index))))
           ,@body)))))

;; Implementation of `seq-let' compatible with Emacs<25.1.
(defmacro seq-let (args sequence &rest body)
  "Bind the variables in ARGS to the elements of SEQUENCE then evaluate BODY.

ARGS can also include the `&rest' marker followed by a variable
name to be bound to the rest of SEQUENCE."
  (declare (indent 2) (debug t))
  (let ((seq-var (make-symbol "seq")))
    `(let* ((,seq-var ,sequence)
            ,@(seq--make-bindings args seq-var))
       ,@body)))

(defun seq-drop (sequence n)
  "Return a subsequence of SEQUENCE without its first N elements.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, SEQUENCE is returned."
  (if (<= n 0)
      sequence
    (if (listp sequence)
        (seq--drop-list sequence n)
      (let ((length (seq-length sequence)))
        (seq-subseq sequence (min n length) length)))))

(defun seq-take (sequence n)
  "Return a subsequence of SEQUENCE with its first N elements.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, an empty sequence is
returned."
  (if (listp sequence)
      (seq--take-list sequence n)
    (seq-subseq sequence 0 (min (max n 0) (seq-length sequence)))))

(defun seq-drop-while (predicate sequence)
  "Return a sequence from the first element for which (PREDICATE element) is nil in SEQUENCE.
The result is a sequence of the same type as SEQUENCE."
  (if (listp sequence)
      (seq--drop-while-list predicate sequence)
    (seq-drop sequence (seq--count-successive predicate sequence))))

(defun seq-take-while (predicate sequence)
  "Return the successive elements for which (PREDICATE element) is non-nil in SEQUENCE.
The result is a sequence of the same type as SEQUENCE."
  (if (listp sequence)
      (seq--take-while-list predicate sequence)
    (seq-take sequence (seq--count-successive predicate sequence))))

(defun seq-filter (predicate sequence)
  "Return a list of all the elements for which (PREDICATE element) is non-nil in SEQUENCE."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (seq-map (lambda (elt)
                             (if (funcall predicate elt)
                                 elt
                               exclude))
                           sequence))))

(defun seq-map-indexed (function sequence)
  "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
  (let ((index 0))
    (seq-map (lambda (elt)
               (prog1
                   (funcall function elt index)
                 (setq index (1+ index))))
             sequence)))

(defun seq-remove (predicate sequence)
  "Return a list of all the elements for which (PREDICATE element) is nil in SEQUENCE."
  (seq-filter (lambda (elt) (not (funcall predicate elt)))
              sequence))

(defun seq-reduce (function sequence initial-value)
  "Reduce the function FUNCTION across SEQUENCE, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result and
the second element of SEQUENCE, then with that result and the third
element of SEQUENCE, etc.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p sequence)
      initial-value
    (let ((acc initial-value))
      (seq-doseq (elt sequence)
        (setq acc (funcall function acc elt)))
      acc)))

(defun seq-some (predicate sequence)
  "Return the first value for which if (PREDICATE element) is non-nil for in SEQUENCE."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (let ((result (funcall predicate elt)))
        (when result
          (throw 'seq--break result))))
    nil))

(defun seq-find (predicate sequence &optional default)
  "Return the first element for which (PREDICATE element) is non-nil in SEQUENCE.
If no element is found, return DEFAULT.

Note that `seq-find' has an ambiguity if the found element is
identical to DEFAULT, as it cannot be known if an element was
found or not."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (when (funcall predicate elt)
        (throw 'seq--break elt)))
    default))

(defun seq-every-p (predicate sequence)
  "Return non-nil if (PREDICATE element) is non-nil for all elements of the sequence SEQUENCE."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (or (funcall predicate elt)
          (throw 'seq--break nil)))
    t))

(defun seq-count (predicate sequence)
  "Return the number of elements for which (PREDICATE element) is non-nil in SEQUENCE."
  (let ((count 0))
    (seq-doseq (elt sequence)
      (when (funcall predicate elt)
        (setq count (+ 1 count))))
    count))

(defun seq-empty-p (sequence)
  "Return non-nil if the sequence SEQUENCE is empty, nil otherwise."
  (if (listp sequence)
      (null sequence)
    (= 0 (seq-length sequence))))

(defun seq-sort (predicate sequence)
  "Return a sorted sequence comparing using PREDICATE the elements of SEQUENCE.
The result is a sequence of the same type as SEQUENCE."
  (if (listp sequence)
      (sort (seq-copy sequence) predicate)
    (let ((result (seq-sort predicate (append sequence nil))))
      (seq-into result (type-of sequence)))))

(defun seq-sort-by (function pred sequence)
  "Sort SEQUENCE using PRED as a comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being
sorted.  FUNCTION must be a function of one argument."
  (seq-sort (lambda (a b)
              (funcall pred
                       (funcall function a)
                       (funcall function b)))
            sequence))

(defun seq-contains (sequence elt &optional testfn)
  "Return the first element in SEQUENCE that equals to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-some (lambda (e)
                (funcall (or testfn #'equal) elt e))
              sequence))

(defun seq-set-equal-p (sequence1 sequence2 &optional testfn)
  "Return non-nil if SEQUENCE1 and SEQUENCE2 contain the same elements, regardless of order.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (and (seq-every-p (lambda (item1) (seq-contains sequence2 item1 testfn)) sequence1)
       (seq-every-p (lambda (item2) (seq-contains sequence1 item2 testfn)) sequence2)))

(defun seq-position (sequence elt &optional testfn)
  "Return the index of the first element in SEQUENCE that is equal to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (let ((index 0))
    (catch 'seq--break
      (seq-doseq (e sequence)
        (when (funcall (or testfn #'equal) e elt)
          (throw 'seq--break index))
        (setq index (1+ index)))
      nil)))

(defun seq-uniq (sequence &optional testfn)
  "Return a list of the elements of SEQUENCE with duplicates removed.
TESTFN is used to compare elements, or `equal' if TESTFN is nil."
  (let ((result '()))
    (seq-doseq (elt sequence)
      (unless (seq-contains result elt testfn)
        (setq result (cons elt result))))
    (nreverse result)))

(defun seq-subseq (sequence start &optional end)
  "Return the subsequence of SEQUENCE from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (cond ((or (stringp sequence) (vectorp sequence)) (substring sequence start end))
        ((listp sequence)
         (let (len (errtext (format "Bad bounding indices: %s, %s" start end)))
           (and end (< end 0) (setq end (+ end (setq len (seq-length sequence)))))
           (if (< start 0) (setq start (+ start (or len (setq len (seq-length sequence))))))
           (when (> start 0)
             (setq sequence (nthcdr (1- start) sequence))
             (or sequence (error "%s" errtext))
             (setq sequence (cdr sequence)))
           (if end
               (let ((res nil))
                 (while (and (>= (setq end (1- end)) start) sequence)
                   (push (pop sequence) res))
                 (or (= (1+ end) start) (error "%s" errtext))
                 (nreverse res))
             (seq-copy sequence))))
        (t (error "Unsupported sequence: %s" sequence))))

(defun seq-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the sequences SEQS.
TYPE must be one of following symbols: vector, string or list.

\n(fn TYPE SEQUENCE...)"
  (pcase type
    (`vector (apply #'vconcat seqs))
    (`string (apply #'concat seqs))
    (`list (apply #'append (append seqs '(nil))))
    (_ (error "Not a sequence type name: %S" type))))

(defun seq-mapcat (function sequence &optional type)
  "Concatenate the result of applying FUNCTION to each element of SEQUENCE.
The result is a sequence of type TYPE, or a list if TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function sequence)))

(defun seq-mapn (function sequence &rest seqs)
  "Like `seq-map' but FUNCTION is mapped over all SEQS.
The arity of FUNCTION must match the number of SEQS, and the
mapping stops on the shortest sequence.
Return a list of the results.

\(fn FUNCTION SEQS...)"
  (let ((result nil)
        (seqs (seq-map (lambda (s)
                         (seq-into s 'list))
                       (cons sequence seqs))))
    (while (not (memq nil seqs))
      (push (apply function (seq-map #'car seqs)) result)
      (setq seqs (seq-map #'cdr seqs)))
    (nreverse result)))

(defun seq-partition (sequence n)
  "Return a list of the elements of SEQUENCE grouped into sub-sequences of length N.
The last sequence may contain less than N elements.  If N is a
negative integer or 0, nil is returned."
  (unless (< n 1)
    (let ((result '()))
      (while (not (seq-empty-p sequence))
        (push (seq-take sequence n) result)
        (setq sequence (seq-drop sequence n)))
      (nreverse result))))

(defun seq-intersection (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in both SEQ1 and SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains seq2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-difference (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in SEQ1 but not in SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains seq2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-group-by (function sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(defalias 'seq-reverse
  (if (ignore-errors (reverse [1 2]))
      #'reverse
    (lambda (sequence)
      "Return the reversed copy of list, vector, or string SEQUENCE.
See also the function `nreverse', which is used more often."
      (let ((result '()))
        (seq-map (lambda (elt) (push elt result))
                 sequence)
        (if (listp sequence)
            result
          (seq-into result (type-of sequence)))))))

(defun seq-into (sequence type)
  "Convert the sequence SEQUENCE into a sequence of type TYPE.
TYPE can be one of the following symbols: vector, string or list."
  (pcase type
    (`vector (seq--into-vector sequence))
    (`string (seq--into-string sequence))
    (`list (seq--into-list sequence))
    (_ (error "Not a sequence type name: %S" type))))

(defun seq-min (sequence)
  "Return the smallest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers."
  (apply #'min (seq-into sequence 'list)))

(defun seq-max (sequence)
    "Return the largest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers."
  (apply #'max (seq-into sequence 'list)))

(defun seq-random-elt (sequence)
  "Return a random element from SEQUENCE.
Signal an error if SEQUENCE is empty."
  (if (seq-empty-p sequence)
      (error "Sequence cannot be empty")
    (seq-elt sequence (random (seq-length sequence)))))

(defun seq--drop-list (list n)
  "Return a list from LIST without its first N elements.
This is an optimization for lists in `seq-drop'."
  (nthcdr n list))

(defun seq--take-list (list n)
  "Return a list from LIST made of its first N elements.
This is an optimization for lists in `seq-take'."
  (let ((result '()))
    (while (and list (> n 0))
      (setq n (1- n))
      (push (pop list) result))
    (nreverse result)))

(defun seq--drop-while-list (predicate list)
  "Return a list from the first element for which (PREDICATE element) is nil in LIST.
This is an optimization for lists in `seq-drop-while'."
  (while (and list (funcall predicate (car list)))
    (setq list (cdr list)))
  list)

(defun seq--take-while-list (predicate list)
  "Return the successive elements for which (PREDICATE element) is non-nil in LIST.
This is an optimization for lists in `seq-take-while'."
  (let ((result '()))
    (while (and list (funcall predicate (car list)))
      (push (pop list) result))
    (nreverse result)))

(defun seq--count-successive (predicate sequence)
  "Return the number of successive elements for which (PREDICATE element) is non-nil in SEQUENCE."
  (let ((n 0)
        (len (seq-length sequence)))
    (while (and (< n len)
                (funcall predicate (seq-elt sequence n)))
      (setq n (+ 1 n)))
    n))

;; Helper function for the Backward-compatible version of `seq-let'
;; for Emacs<25.1.
(defun seq--make-bindings (args sequence &optional bindings)
  "Return a list of bindings of the variables in ARGS to the elements of a sequence.
if BINDINGS is non-nil, append new bindings to it, and return
BINDINGS."
  (let ((index 0)
        (rest-marker nil))
    (seq-doseq (name args)
      (unless rest-marker
        (pcase name
          ((pred seqp)
           (setq bindings (seq--make-bindings (seq--elt-safe args index)
                                              `(seq--elt-safe ,sequence ,index)
                                              bindings)))
          (`&rest
           (progn (push `(,(seq--elt-safe args (1+ index))
                          (seq-drop ,sequence ,index))
                        bindings)
                  (setq rest-marker t)))
          (_
           (push `(,name (seq--elt-safe ,sequence ,index)) bindings))))
      (setq index (1+ index)))
    bindings))

(defun seq--elt-safe (sequence n)
  "Return element of SEQUENCE at the index N.
If no element is found, return nil."
  (when (or (listp sequence)
            (and (sequencep sequence)
                 (> (seq-length sequence) n)))
    (seq-elt sequence n)))

(defun seq--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in seq."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '("\\<seq-doseq\\>" "\\<seq-let\\>")))

(defalias 'seq-copy #'copy-sequence)
(defalias 'seq-elt #'elt)
(defalias 'seq-length #'length)
(defalias 'seq-do #'mapc)
(defalias 'seq-each #'seq-do)
(defalias 'seq-map #'mapcar)
(defalias 'seqp #'sequencep)

(defun seq--into-list (sequence)
  "Concatenate the elements of SEQUENCE into a list."
  (if (listp sequence)
      sequence
    (append sequence nil)))

(defun seq--into-vector (sequence)
  "Concatenate the elements of SEQUENCE into a vector."
  (if (vectorp sequence)
      sequence
    (vconcat sequence)))

(defun seq--into-string (sequence)
  "Concatenate the elements of SEQUENCE into a string."
  (if (stringp sequence)
      sequence
    (concat sequence)))

(unless (fboundp 'elisp--font-lock-flush-elisp-buffers)
  ;; In Emacs≥25, (via elisp--font-lock-flush-elisp-buffers and a few others)
  ;; we automatically highlight macros.
  (add-hook 'emacs-lisp-mode-hook #'seq--activate-font-lock-keywords))

(provide 'seq-24)
;;; seq-24.el ends here
