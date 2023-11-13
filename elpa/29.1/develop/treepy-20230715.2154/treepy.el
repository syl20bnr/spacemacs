;;; treepy.el --- Generic tree traversal tools  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Daniel Barreto

;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Keywords: lisp, maint, tools
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/volrath/treepy.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generic tools for recursive and iterative tree traversal based on
;; clojure.walk and clojure.zip respectively.  Depends on `map', a map
;; manipulation library built in Emacs 25.1.  All functions are prefixed
;; with "treepy-".

;;; Code:

(require 'map)
(require 'cl-lib)

;;; Walk (recursive tree traversal)

(defun treepy-walk (inner outer form)
  "Using INNER and OUTER, traverse FORM, an arbitrary data structure.
INNER and OUTER are functions.  Apply INNER to each element of
FORM, building up a data structure of the same type, then apply
OUTER to the result.  Recognize cons, lists, alists, vectors and
hash tables."
  (cond
   ((and (listp form) (cdr form) (atom (cdr form))) (funcall outer (cons (funcall inner (car form))
                                                                         (funcall inner (cdr form)))))
   ((listp form) (funcall outer (mapcar inner form)))
   ((vectorp form) (funcall outer (apply #'vector (mapcar inner form))))
   ((hash-table-p form) (funcall outer (map-apply (lambda (k v) (funcall inner (cons k v))) form)))
   (t (funcall outer form))))

(defun treepy-postwalk (f form)
  "Perform a depth-first, post-order traversal of F applied to FORM.
Call F on each sub-form, use F's return value in place of the
original.  Recognize cons, lists, alists, vectors and
hash tables."
  (treepy-walk (apply-partially #'treepy-postwalk f) f form))

(defun treepy-prewalk (f form)
  "Perform a depth-first, pre-order traversal of F applied to FORM.
Like `treepy-postwalk'."
  (treepy-walk (apply-partially #'treepy-prewalk f) #'identity (funcall f form)))

(defun treepy-postwalk-demo (form)
  "Demonstrate the behavior of `treepy-postwalk' for FORM.
Return a list of each form as it is walked."
  (let ((walk nil))
    (treepy-postwalk (lambda (x) (push x walk) x)
                     form)
    (reverse walk)))

(defun treepy-prewalk-demo (form)
  "Demonstrate the behavior of `treepy-prewalk' for FORM.
Return a list of each form as it is walked."
  (let ((walk nil))
    (treepy-prewalk (lambda (x) (push x walk) x)
                    form)
    (reverse walk)))

(defun treepy-postwalk-replace (smap form &optional testfn)
  "Use SMAP to transform FORM by doing replacing operations.
Recursively replace in FORM keys in SMAP with their values.
Does replacement at the leaves of the tree first."
  ;; Also see comment in `map-contains-key's definition.
  (declare (advertised-calling-convention (smap key) "0.1.3"))
  (treepy-postwalk (lambda (x)
                     (if (with-suppressed-warnings ((callargs map-contains-key))
                           (map-contains-key smap x testfn))
                         (map-elt smap x)
                       x))
                   form))

(defun treepy-prewalk-replace (smap form &optional testfn)
  "Use SMAP to transform FORM by doing replacing operations.
Recursively replace in FORM keys in SMAP with their values.
Does replacement at the root of the tree first."
  ;; Also see comment in `map-contains-key's definition.
  (declare (advertised-calling-convention (smap key) "0.1.3"))
  (treepy-prewalk (lambda (x)
                    (if (with-suppressed-warnings ((callargs map-contains-key))
                          (map-contains-key smap x testfn))
                        (map-elt smap x)
                      x))
                  form))


;;; Zipper (iterative tree traversal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treepy--context (loc &optional key)
  "Return context for this LOC.
If KEY is given, only return this key's value in context."
  (let ((context (cdr (car loc))))
    (if (and context key)
        (map-elt context key)
      context)))

(defun treepy--context-assoc-1 (context k v)
  "Assoc in CONTEXT a key K with a value V."
  (if (map-contains-key context k)
      (mapcar (lambda (entry)
                (if (equal (car entry) k)
                    (cons k v)
                  entry))
              context)
    (cons (cons k v) context)))

(defun treepy--context-assoc (context &rest kvs)
  "Immutable map association in CONTEXT using KVS."
  (seq-reduce (lambda (context kv)
                (seq-let [k v] kv
                  (treepy--context-assoc-1 context k v)))
              (seq-partition kvs 2) context))

(defun treepy--meta (loc &optional key)
  "Return meta information for this LOC.
If KEY is given, only return this key's value in meta
information."
  (let ((meta (cdr loc)))
    (if key
        (map-elt meta key)
      meta)))

(defun treepy--with-meta (obj meta)
  "Bind OBJ with some META information."
  (cons obj meta))

(defun treepy--join-children (left-children right-children)
  "Return a joining of LEFT-CHILDREN and RIGHT-CHILDREN.
Reverses LEFT-CHILDREN so that they are correctly ordered as in
the tree."
  (append (reverse left-children) right-children))

(defmacro treepy--with-loc (loc vars &rest body)
  "Create a lexical context using LOC VARS.
Execute BODY in this context."
  (declare (indent defun))
  (let ((lex-ctx (mapcar (lambda (v)
                           (cl-case v
                             (node    `(node (treepy-node ,loc)))
                             (context `(context (treepy--context ,loc)))
                             (t       `(,v (treepy--context ,loc (quote ,(intern (concat ":" (symbol-name v)))))))))
                         vars)))
    `(let* (,@lex-ctx) ,@body)))

;;;; Construction

(defun treepy-zipper (branchp children make-node root)
  "Create a new zipper structure.

BRANCHP is a function that, given a node, returns t if it can
have children, even if it currently doesn't.

CHILDREN is a function that, given a branch node, returns a seq
of its children.

MAKE-NODE is a function that, given an existing node and a seq of
children, returns a new branch node with the supplied children.

ROOT is the root node."
  (treepy--with-meta
   (cons root nil)
   `((:branchp . ,branchp) (:children . ,children) (:make-node . ,make-node))))

(defun treepy-list-zip (root)
  "Return a zipper for nested lists, given a ROOT list."
  (let ((make-node (lambda (_ children) children)))
    (treepy-zipper #'listp #'identity make-node root)))

(defun treepy-vector-zip (root)
  "Return a zipper for nested vectors, given a ROOT vector."
  (let ((make-node (lambda (_ children) (apply #'vector children)))
        (children (lambda (cs) (seq-into cs 'list))))
    (treepy-zipper #'vectorp children make-node root)))

;;;; Context

(defun treepy-node (loc)
  "Return the node at LOC."
  (caar loc))

(defun treepy-branch-p (loc)
  "Return t if the node at LOC is a branch."
  (funcall (treepy--meta loc ':branchp) (treepy-node loc)))

(defun treepy-children (loc)
  "Return a children list of the node at LOC, which must be a branch."
  (if (treepy-branch-p loc)
      (funcall (treepy--meta loc ':children) (treepy-node loc))
    (error "Called children on a leaf node")))

(defun treepy-make-node (loc node children)
  "Return a new branch node.
Given an existing LOC, NODE and new CHILDREN, creates a new LOC
with them.  The LOC is only used to supply the constructor."
  (funcall (treepy--meta loc ':make-node) node children))

(defun treepy-path (loc)
  "Return a list of nodes leading to the given LOC."
  (reverse (treepy--context loc ':pnodes)))

(defun treepy-lefts (loc)
  "Return a list of the left siblings of this LOC."
  (reverse (treepy--context loc ':l)))

(defun treepy-rights (loc)
  "Return a list of the right siblings of this LOC."
  (treepy--context loc ':r))

;;;; Navigation

(defun treepy-down (loc)
  "Return the loc of the leftmost child of the node at this LOC.
nil if no children."
  (when (treepy-branch-p loc)
    (let ((children (treepy-children loc)))
      (treepy--with-loc loc (node context pnodes)
        (seq-let [c &rest cs] children
          (when children
            (treepy--with-meta
             `(,c . ((:l . ,nil)
                     (:pnodes . ,(if context (cons node pnodes) (list node)))
                     (:ppath . ,context)
                     (:r . ,cs)))
             (treepy--meta loc))))))))

(defun treepy-up (loc)
  "Return the loc of the parent of the node at this LOC.
nil if at the top."
  (treepy--with-loc loc (node pnodes ppath changed? l r)
    (when pnodes
      (let ((pnode (car pnodes)))
        (treepy--with-meta
         (if changed?
             (cons (treepy-make-node loc pnode (treepy--join-children l (cons node r)))
                   (and ppath (treepy--context-assoc ppath ':changed? t)))
           (cons pnode ppath))
         (treepy--meta loc))))))

(defun treepy-root (loc)
  "Zip from LOC all the way up and return the root node.
Reflect any alterations to the tree."
  (if (equal :end (treepy--context loc))
      (treepy-node loc)
    (let ((p loc))
      (while (setq p (treepy-up p))
        (setq loc p))
      (treepy-node loc))))

(defun treepy-right (loc)
  "Return the loc of the right sibling of the node at this LOC.
nil if there's no more right siblings."
  (treepy--with-loc loc (node context l r)
    (let ((r (if (listp r)
                 r
               ;; If `r' is not a list (or nil), then we're dealing with a non
               ;; nil cdr ending list.
               (cons r nil))))
      (seq-let [cr &rest rnext] r
        (when (and context r)
          (treepy--with-meta
           (cons cr
                 (treepy--context-assoc context
                                        ':l (cons node l)
                                        ':r rnext))
           (treepy--meta loc)))))))


(defun treepy-rightmost (loc)
  "Return the loc of the rightmost sibling of the node at this LOC.
If LOC is already the rightmost sibling, return self."
  (treepy--with-loc loc (node context l r)
    (if (and context r)
        (treepy--with-meta
         (cons (car (last r))
               (treepy--context-assoc context
                                      ':l (treepy--join-children l (cons node (butlast r)))
                                      ':r nil))
         (treepy--meta loc))
      loc)))

(defun treepy-left (loc)
  "Return the loc of the left sibling of the node at this LOC.
nil if no more left siblings."
  (treepy--with-loc loc (node context l r)
    (when (and context l)
      (seq-let [cl &rest lnext] l
        (treepy--with-meta
         (cons cl
               (treepy--context-assoc context
                                      ':l lnext
                                      ':r (cons node r)))
         (treepy--meta loc))))))

(defun treepy-leftmost (loc)
  "Return the loc of the leftmost sibling of the node at this LOC.
If LOC is already the leftmost sibling, return self."
  (treepy--with-loc loc (node context l r)
    (if (and context l)
        (treepy--with-meta
         (cons (car (last l))
               (treepy--context-assoc context
                                      ':l []
                                      ':r (treepy--join-children (butlast l) (cons node r))))
         (treepy--meta loc))
      loc)))

(defun treepy-leftmost-descendant (loc)
  "Return the leftmost descendant of the given LOC.
\(ie, down repeatedly)."
  (while (treepy-branch-p loc)
    (setq loc (treepy-down loc)))
  loc)

;;;; Modification

(defun treepy-insert-left (loc item)
  "Insert as the left sibling of this LOC'S node the ITEM.
Return same loc with siblings updated."
  (treepy--with-loc loc (node context l)
    (if (not context)
        (error "Insert at top")
      (treepy--with-meta
       (cons node
             (treepy--context-assoc context
                                    ':l (cons item l)
                                    ':changed? t))
       (treepy--meta loc)))))

(defun treepy-insert-right (loc item)
  "Insert as the right sibling of this LOC's node the ITEM.
Return same loc with siblings updated."
  (treepy--with-loc loc (node context r)
    (if (not context)
        (error "Insert at top")
      (treepy--with-meta
       (cons node
             (treepy--context-assoc context
                                    ':r (cons item r)
                                    ':changed? t))
       (treepy--meta loc)))))

(defun treepy-replace (loc node)
  "Replace the node in this LOC with the given NODE, without moving."
  (let ((context (treepy--context loc)))
    (treepy--with-meta
     (cons node
           (treepy--context-assoc context
                                  ':changed? t))
     (treepy--meta loc))))

(defun treepy-edit (loc f &rest args)
  "Replace the node at this LOC with the value of (F node ARGS)."
  (treepy-replace loc (apply f (treepy-node loc) args)))

(defun treepy-insert-child (loc item)
  "Insert as the leftmost child of this LOC's node the ITEM.
Return same loc with children updated."
  (treepy-replace loc (treepy-make-node loc (treepy-node loc) (cons item (treepy-children loc)))))

(defun treepy-append-child (loc item)
  "Insert as the rightmost child of this LOC'S node the ITEM.
Return same loc with children updated."
  (treepy-replace loc (treepy-make-node loc (treepy-node loc) (append (treepy-children loc) `(,item)))))  ;; TODO: check performance

(defun treepy-remove (loc)
  "Remove the node at LOC.
Return the loc that would have preceded it in a depth-first
walk."
  (treepy--with-loc loc (context pnodes ppath l r)
    (if (not context)
        (error "Remove at top")
      (if (> (length l) 0)
          (let ((nloc (treepy--with-meta (cons (car l)
                                               (treepy--context-assoc context
                                                                      ':l (cdr l)
                                                                      ':changed? t))
                                         (treepy--meta loc)))
                (child nil))
            (while (setq child (and (treepy-branch-p nloc) (treepy-children nloc)))
              (setq nloc (treepy-rightmost child)))
            nloc)
        (treepy--with-meta
         (cons (treepy-make-node loc (car pnodes) r)
               (and ppath (treepy--context-assoc context ':changed? t)))
         (treepy--meta loc))))))

;;;; Enumeration

(defun treepy--preorder-next (loc)
  "Move to the next LOC in the hierarchy, depth-first in preorder.
When reaching the end, returns a distinguished loc detectable via
`treepy-end-p'.  If already at the end, stays there."
  (if (equal :end (treepy--context loc))
      loc
    (let ((cloc loc))
      (or
       (and (treepy-branch-p cloc) (treepy-down cloc))
       (treepy-right cloc)
       (let ((p cloc)
             (pr nil))
         (while (and (treepy-up p) (not (setq pr (treepy-right (treepy-up p)))))
           (setq p (treepy-up p)))
         (or pr (cons (cons (treepy-node p) :end) nil)))))))

(defun treepy--postorder-next (loc)
  "Move to the next LOC in the hierarchy, depth-first in postorder.
When reaching the end, returns a distinguished loc detectable via
`treepy-end-p'.  If already at the end, stays there."
  (if (equal :end (treepy--context loc))
      loc
    (if (null (treepy-up loc))
        (cons (cons (treepy-node loc) :end) nil)
      (or (let ((rloc (treepy-right loc)))
            (and rloc (treepy-leftmost-descendant rloc)))
          (treepy-up loc)))))

(defun treepy-next (loc &optional order)
  "Move to the next LOC in the hierarchy, depth-first.
Use ORDER if given.  Possible values for ORDER are `:preorder' and
`:postorder', defaults to the former."
  (cl-case (or order ':preorder)
    (:preorder (treepy--preorder-next loc))
    (:postorder (treepy--postorder-next loc))
    (t (error "Unrecognized order"))))

(defun treepy--preorder-prev (loc)
  "Move to the previous LOC in the hierarchy, depth-first preorder.
If already at the root, returns nil."
  (let ((lloc (treepy-left loc))
        (child nil))
    (if lloc
        (progn
          (while (setq child (and (treepy-branch-p lloc) (treepy-children lloc)))
            (setq lloc (treepy-rightmost child)))
          lloc)
      (treepy-up loc))))

(defun treepy--postorder-prev (loc)
  "Move to the previous LOC in the hierarchy, depth-first postorder.
If already at the root, returns nil."
  (if (treepy-branch-p loc)
      (treepy-rightmost (treepy-down loc))
    (progn
      (while (not (treepy-left loc))
        (setq loc (treepy-up loc)))
      (treepy-left loc))))

(defun treepy-prev (loc &optional order)
  "Move to the previous LOC in the hierarchy, depth-first.
Use ORDER if given.  Possible values for ORDER are `:preorder' and `:postorder',
defaults to the former."
  (cl-case (or order ':preorder)
    (:preorder (treepy--preorder-prev loc))
    (:postorder (treepy--postorder-prev loc))
    (t (error "Unrecognized order"))))

(defun treepy-end-p (loc)
  "Return t if LOC represents the end of a depth-first walk."
  (equal :end (treepy--context loc)))

(provide 'treepy)

;;; treepy.el ends here
