;;; taxy.el --- Programmable taxonomical grouping for arbitrary objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/taxy.el
;; Package-Requires: ((emacs "26.3"))
;; Version: 0.10.1
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
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

;; This library provides a programmable way to classify arbitrary
;; objects into a hierarchical taxonomy.  (That's a lot of fancy words
;; to say that this lets you put things in nested groups.)

;; Helpful features include:

;; + Dynamic taxonomies: Objects may be classified into hierarchies
;; automatically defined at runtime based on their attributes.

;; + Reusable taxonomies: Taxonomy definitions may be stored in
;; variables and reused in other taxonomies' descendant groups.

;; Basic usage:

;; 1.  Make a taxy with `make-taxy'.
;; 2.  Fill the taxy with items using `taxy-fill'.
;; 3.  For a simple display of a taxy's items, use `taxy-plain'.

;; For more details, please see the README.org file.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;;; Structs

(cl-defstruct taxy
  name description key items taxys
  (predicate #'identity) (then #'ignore)
  (make #'make-taxy)
  take)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun taxy-fill (items taxy)
  "Fill TAXY with ITEMS according to its definition."
  (cl-labels ((apply-item (item taxy)
                          (or (if (taxy-take taxy)
                                  (funcall (taxy-take taxy) item taxy)
                                (cl-loop for taxy in (taxy-taxys taxy)
                                         when (funcall (taxy-predicate taxy) item)
                                         do (progn
                                              (if (taxy-take taxy)
                                                  (funcall (taxy-take taxy) item taxy)
                                                (if (taxy-taxys taxy)
                                                    (or (apply-item item taxy)
                                                        (push item (taxy-items taxy)))
                                                  (push item (taxy-items taxy))))
                                              (setf item (funcall (taxy-then taxy) item)))
                                         unless item return t
                                         finally return nil))
                              ;; No sub-taxys took the item: add it to this taxy.
                              (when (funcall (taxy-predicate taxy) item)
                                (if (taxy-take taxy)
                                    (funcall (taxy-take taxy) item taxy)
                                  (push item (taxy-items taxy)))))))
    (dolist (item items taxy)
      (apply-item item taxy))))

(defun taxy-plain (taxy)
  "Return a list of the human-readable parts of TAXY."
  (delq nil
        (list (taxy-name taxy)
              (taxy-description taxy)
              (taxy-items taxy)
              (mapcar #'taxy-plain (taxy-taxys taxy)))))

(defun taxy-emptied (taxy)
  "Return a copy of TAXY without items.
Omits TAXY's items and those of its descendant taxys.  Useful
when reusing taxy definitions."
  (setf taxy (copy-taxy taxy)
        (taxy-items taxy) nil
        (taxy-taxys taxy) (mapcar #'taxy-emptied (taxy-taxys taxy)))
  taxy)

(defun taxy-flatten (taxy)
  "Return a list of items in TAXY and its sub-taxys."
  (append (taxy-items taxy)
          (cl-loop for taxy in (taxy-taxys taxy)
                   append (taxy-flatten taxy))))

(defun taxy-mapcar-items (fn taxy)
  "Return copy of TAXY, having replaced its items with the value of FN on each.
Replaces every item in TAXY and its descendants.  Useful to
replace items with a more useful form after classification."
  (declare (indent defun))
  ;; It might be preferable to destructively replace items rather
  ;; than consing new lists, but I haven't found a way that works
  ;; (even `cl-loop' with `in-ref' hasn't worked).
  (setf (taxy-items taxy) (mapcar fn (taxy-items taxy))
        (taxy-taxys taxy) (cl-loop for taxy in (taxy-taxys taxy)
                                   collect (taxy-mapcar-items fn taxy)))
  taxy)

(defalias 'taxy-mapcar #'taxy-mapcar-items)

(defun taxy-mapc-taxys (fn taxy)
  "Return TAXY having applied FN to it and its descendants.
Does not copy TAXY.  Destructively modifies TAXY, if FN does."
  (declare (indent defun))
  (funcall fn taxy)
  (cl-loop for sub-taxy in-ref (taxy-taxys taxy)
           do (setf sub-taxy (taxy-mapc-taxys fn sub-taxy)))
  taxy)

(defalias 'taxy-mapc* #'taxy-mapc-taxys)

(cl-defun taxy-take-keyed
    (key-fns item taxy
             &key (key-name-fn #'identity) (then #'ignore))
  "Take ITEM into TAXY, adding new taxys dynamically and recursively.
Places ITEM into a taxy in TAXY for the value returned by
KEY-FNS called with ITEM.  The new taxys are added to TAXY
recursively as necessary.  Each new taxy's name is that returned
by KEY-NAME-FN called with ITEM.

Each element of KEY-FNS may be a function or a list of functions.
A list of functions creates a \"chain\" of functions: when an
item is matched by the first function in a chain, it is placed
in that chain's taxonomy, and is not \"offered\" to functions
outside of that chain.

For example, if KEY-FNS were:

  '(((lambda (n) (< n 10)) oddp)
    ((lambda (n) (>= n 10)) evenp))

Then a list of numbers from 0-19 would be classified
like (listing numbers on a single line for the sake of example):

  - <10:
    - 0, 2, 4, 6, 8
    - oddp:
      - 1, 3, 5, 7, 9
  - >=10:
    - 11, 13, 15, 17, 19
    - evenp:
      - 10, 12, 14, 16, 18

So only numbers below 10 are tested against `oddp', and only
numbers greater-than-or-equal-to 10 are tested against
`evenp'.  (A contrived example, of course, since testing against
`evenp' or `oddp' is just the inverse.)"
  (declare (indent defun))
  (cl-macrolet ((offer-or-push
                 () `(if (cdr key-fns)
                         (taxy-take-keyed (cdr key-fns) item taxy
                           :key-name-fn key-name-fn :then then)
                       (push item (taxy-items taxy)))))
    (cl-typecase (car key-fns)
      (function
       ;; A single key function.
       (let ((key-fn (car key-fns)))
         (if-let ((key (funcall key-fn item)))
             ;; This key function returned non-nil for the item:
             ;; apply it to the appropriate sub-taxy.
             (let ((key-taxy
                    (or (cl-find-if (lambda (taxy-key)
                                      (equal key taxy-key))
                                    (taxy-taxys taxy)
                                    :key #'taxy-key)
                        ;; No existing, matching sub-taxy found: make
                        ;; a new one and add it to TAXY's sub-taxys.
                        (car
                         (push (funcall
                                ;; NOTE: Calling `make-taxy' directly might offer the
                                ;; compiler a chance to optimize compared to using `funcall',
                                ;; but allowing taxy structs to specify their own MAKE
                                ;; functions is very helpful when using specialized structs.
                                (taxy-make taxy)
                                :name (funcall key-name-fn key)
                                :key key
                                :predicate (lambda (item)
                                             (equal key (funcall key-fn item)))
                                :take (when (cdr key-fns)
                                        (lambda (item taxy)
                                          (taxy-take-keyed (cdr key-fns) item taxy
                                            :key-name-fn key-name-fn :then then)))
                                :then then)
                               (taxy-taxys taxy))))))
               (if (cdr key-fns)
                   ;; Other key-fns remain: offer item to them, allowing
                   ;; them to create more sub-taxys beneath this key-taxy.
                   (taxy-take-keyed (cdr key-fns) item key-taxy
                     :key-name-fn key-name-fn :then then)
                 ;; No more key-fns remain: add item to this taxy.
                 (push item (taxy-items key-taxy))))
           ;; No key value: offer to other KEY-FNS or push to this taxy.
           (offer-or-push))))
      (list
       ;; A "chain" of key functions.
       (or (when (funcall (caar key-fns) item)
             ;; The first function in this chain returns non-nil for
             ;; the item: apply the item to the chain.
             (taxy-take-keyed (car key-fns) item taxy
               :key-name-fn key-name-fn :then then))
           ;; This "chain" of key-fns didn't take the item: offer it to
           ;; other chains, or push to this taxy if they don't take it.
           (offer-or-push))))))

(defun taxy-size (taxy)
  "Return the number of items TAXY holds.
Includes items in TAXY's sub-taxys."
  (cl-loop for sub-taxy in (taxy-taxys taxy)
           sum (taxy-size sub-taxy) into total
           finally return (+ total (length (taxy-items taxy)))))

(defun taxy-sort-items (pred key taxy)
  "Sort TAXY's items by PRED and KEY.
Sorts items in TAXY and its sub-taxys.  KEY is passed to
`cl-sort', which see."
  (declare (indent defun))
  (taxy-mapc* (lambda (taxy)
                (setf (taxy-items taxy)
                      (cl-sort (taxy-items taxy)
                               pred :key key)))
    taxy))

(defalias 'taxy-sort #'taxy-sort-items)

(defun taxy-sort-taxys (pred key taxy)
  "Sort TAXY's sub-taxys by PRED and KEY.
KEY is passed to `cl-sort', which see."
  (declare (indent defun))
  (taxy-mapc* (lambda (taxy)
                (setf (taxy-taxys taxy)
                      (cl-sort (taxy-taxys taxy)
                               pred :key key)))
    taxy))

(defalias 'taxy-sort* #'taxy-sort-taxys)

;;;; Key functions

;; Utilities to define key and take functions in a standard way.

(defmacro taxy-define-key-definer (name variable prefix docstring)
  "Define a macro NAME that defines a key-function-defining macro.
The defined macro, having string DOCSTRING, associates the
defined key functions with their aliases in an alist stored in
symbol VARIABLE.  The defined key functions are named having
string PREFIX, which will have a hyphen appended to it.  The key
functions take one or more arguments, the first of which is the
item being tested, bound within the function to `item'."
  ;; Example docstring:

  ;;   "Define a `taxy-org-ql-view' key function by NAME having BODY taking ARGS.
  ;; Within BODY, `element' is bound to the `org-element' element
  ;; being tested.

  ;; Defines a function named `taxy-org-ql--predicate-NAME', and adds
  ;; an entry to `taxy-org-ql-view-keys' mapping NAME to the new
  ;; function symbol."
  (declare (indent defun))
  ;; I'm not sure why it's necessary to bind the variable in the first
  ;; level of the expansion here, but double-unquoting the variable in
  ;; the defined macro's form leaves the second comma in place, which
  ;; breaks the second expansion, and this works around that.
  `(let ((variable ',variable))
     (defvar ,variable nil
       ,(format "Alist mapping key aliases to key functions defined with `%s'."
                name))
     (defmacro ,name (name args &rest body)
       ,docstring
       (declare (indent defun)
                (debug (&define symbolp listp &rest def-form)))
       (let* ((fn-symbol (intern (format "%s-%s" ,prefix name)))
              (fn `(cl-function
                    (lambda (item ,@args)
                      ,@body))))
         `(progn
            (fset ',fn-symbol ,fn)
            (setf (map-elt ,variable ',name) ',fn-symbol))))))

(defun taxy-make-take-function (keys aliases)
  "Return a `taxy' \"take\" function for KEYS.
Each of KEYS should be a function alias defined in ALIASES, or a
list of such KEY-FNS (recursively, ad infinitum, approximately).
ALIASES should be an alist mapping aliases to functions (such as
defined with a definer defined by `taxy-define-key-definer')."
  (let ((macrolets (cl-loop for (name . fn) in aliases
                            collect `(,name ',fn))))
    (cl-labels ((expand-form
                 ;; Is using (cadr (macroexpand-all ...)) really better than `eval'?
                 (form) (cadr (macroexpand-all
                               `(cl-symbol-macrolet (,@macrolets)
                                  ,form))))
                (quote-fn
                 (fn) (pcase fn
                        ((pred symbolp) (expand-form fn))
                        (`(,(and (or 'and 'or 'not) boolean)
                           . ,(and args (map (:name name) (:keys keys))))
                         ;; Well, that pcase expression isn't confusing at all...  ;)
                         ;;  (cl-assert name t "Boolean key functions require a NAME")
                         ;;  (cl-assert keys t "Boolean key functions require KEYS")
                         `(lambda (buffer)
                            (when (cl-loop for fn in ',(mapcar #'quote-fn (or keys args))
                                           ,(pcase boolean
                                              ('and 'always)
                                              ('or 'thereis)
                                              ('not 'never))
                                           (funcall fn buffer))
                              (or ,name ""))))
                        (`(,(and (pred symbolp) fn)
                           . ,(and args (guard (pcase (car args)
                                                 ((or (pred keywordp)
                                                      (and (pred atom)
                                                           ;; SOMEDAY: Use (not symbolp) when depending on Emacs 28.1.
                                                           (pred (lambda (it) (not (symbolp it)))))
                                                      `(quote ,_))
                                                  t)))))
                         ;; Key with args: replace with a lambda that
                         ;; calls that key's function with given args.
                         `(lambda (element)
                            (,(expand-form fn) element ,@args)))
                        ((pred listp) (mapcar #'quote-fn fn)))))
      (setf keys (mapcar #'quote-fn keys))
      `(lambda (item taxy)
         (taxy-take-keyed ',keys item taxy)))))

;;;; Documentation group

;; Available in Emacs 28.  NOTE: In earlier Emacs versions,
;; byte-compiling this section will produce warnings due to the
;; shortdoc forms that appear to be function calls.

(with-eval-after-load 'shortdoc
  (declare-function shortdoc-add-function "shortdoc" (group section elem))
  (mapc (lambda (elem)
	  (shortdoc-add-function 'taxy nil elem))
        '((taxy-flatten
           :eval (taxy-flatten
                  (make-taxy
                   :items '(a b c)
                   :taxys (list (make-taxy
                                 :items '(d e f))))))
          (taxy-emptied
           :eval (taxy-emptied
                  (make-taxy
                   :items '(a b c)
                   :taxys (list (make-taxy
                                 :items '(d e f))))))
          (taxy-fill
           :eval (taxy-fill '(0 1 2 3)
                            (make-taxy
                             :name "Numbers"
                             :taxys (list (make-taxy
                                           :name "Odd"
                                           :predicate #'cl-oddp)
                                          (make-taxy
                                           :name "Even"
                                           :predicate #'cl-evenp)))))
          (taxy-make-take-function
           :eval (taxy-make-take-function
                  '(first-char second-char)
                  '((first-char (lambda (s) (substring s nil 1)))
                    (second-char (lambda (s) (substring s 1 2))))))
          (taxy-mapc-taxys
            :eval (taxy-mapc-taxys
                    (lambda (taxy)
                      (setf (taxy-name taxy) (upcase (taxy-name taxy))))
                    (make-taxy :name "a" :taxys (list (make-taxy :name "b")))))
          (taxy-mapcar-items
            :eval (taxy-mapcar-items #'upcase
                    (make-taxy :items (list "a" "b" "c")
                               :taxys (list (make-taxy :items (list "d" "e" "f"))))))
          (taxy-plain
           :eval (taxy-plain
                  (taxy-fill '(0 1 2 3)
                             (make-taxy
                              :name "Numbers"
                              :taxys (list (make-taxy
                                            :name "Odd"
                                            :predicate #'cl-oddp)
                                           (make-taxy
                                            :name "Even"
                                            :predicate #'cl-evenp))))))
          (taxy-size
           :eval (taxy-size
                  (make-taxy
                   :items '(a b c)
                   :taxys (list (make-taxy
                                 :items '(d e f))))))
          (taxy-sort-items
            :eval (taxy-sort-items #'string< #'identity
                    (make-taxy :items (list "c" "b" "a")
                               :taxys (list (make-taxy :items (list "f" "e" "d"))))))
          (taxy-sort-taxys
            :eval (taxy-sort-taxys #'string< #'taxy-name
                    (make-taxy :name "Taxy"
                               :taxys (list (make-taxy :name "Beta")
                                            (make-taxy :name "Alpha"))))))))

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
