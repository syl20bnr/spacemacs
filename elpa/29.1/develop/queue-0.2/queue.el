;;; queue.el --- Queue data structure  -*- lexical-binding: t; -*-

;; Copyright (C) 1991-1995, 2008-2009, 2012, 2017  Free Software Foundation, Inc

;; Author: Inge Wallin <inge@lysator.liu.se>
;;         Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: extensions, data structures, queue
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/predictive.git

;; This file is part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; These queues can be used both as a first-in last-out (FILO) and as a
;; first-in first-out (FIFO) stack, i.e. elements can be added to the front or
;; back of the queue, and can be removed from the front. (This type of data
;; structure is sometimes called an "output-restricted deque".)
;;
;; You create a queue using `make-queue', add an element to the end of the
;; queue using `queue-enqueue', and push an element onto the front of the
;; queue using `queue-prepend'. To remove the first element from a queue, use
;; `queue-dequeue'. A number of other queue convenience functions are also
;; provided, all starting with the prefix `queue-'.  Functions with prefix
;; `queue--' are for internal use only, and should never be used outside this
;; package.


;;; Code:

(eval-when-compile (require 'cl))

(defmacro queue--when-generators (then)
  "Evaluate THEN if `generator' library is available."
  (declare (debug t))
  (if (require 'generator nil 'noerror) then))


(defstruct (queue
            ;; A tagged list is the pre-defstruct representation.
            ;; (:type list)
	    :named
	    (:constructor nil)
	    (:constructor queue-create ())
	    (:copier nil))
  head tail)


;;;###autoload
(defalias 'make-queue 'queue-create
  "Create an empty queue data structure.")


(defun queue-enqueue (queue element)
  "Append an ELEMENT to the end of the QUEUE."
  (if (queue-head queue)
      (setcdr (queue-tail queue)
	      (setf (queue-tail queue) (cons element nil)))
    (setf (queue-head queue)
	  (setf (queue-tail queue) (cons element nil)))))

(defalias 'queue-append 'queue-enqueue)


(defun queue-prepend (queue element)
  "Prepend an ELEMENT to the front of the QUEUE."
  (if (queue-head queue)
      (push element (queue-head queue))
    (setf (queue-head queue)
	  (setf (queue-tail queue) (cons element nil)))))


(defun queue-dequeue (queue)
  "Remove the first element of QUEUE and return it.
Returns nil if the queue is empty."
  (unless (cdr (queue-head queue)) (setf (queue-tail queue) nil))
  (pop (queue-head queue)))


(defun queue-empty (queue)
  "Return t if QUEUE is empty, otherwise return nil."
  (null (queue-head queue)))


(defun queue-first (queue)
  "Return the first element of QUEUE or nil if it is empty,
without removing it from the QUEUE."
  (car (queue-head queue)))


(defun queue-nth (queue n)
  "Return the nth element of a queue, without removing it.
If the length of the queue is less than N, return nil. The first
element in the queue has index 0."
  (nth n (queue-head queue)))


(defun queue-last (queue)
  "Return the last element of QUEUE, without removing it.
Returns nil if the QUEUE is empty."
  (car (queue-tail queue)))


(defun queue-all (queue)
  "Return a list of all elements of QUEUE or nil if it is empty.
The oldest element in the queue is the first in the list."
  (queue-head queue))


(defun queue-copy (queue)
  "Return a copy of QUEUE.
The new queue contains the elements of QUEUE in the same
order. The elements themselves are *not* copied."
  (let ((q (queue-create))
	(list (queue-head queue)))
    (when (queue-head queue)
      (setf (queue-head q) (cons (car (queue-head queue)) nil)
	    (queue-tail q) (queue-head q))
      (while (setq list (cdr list))
	(setf (queue-tail q)
	      (setcdr (queue-tail q) (cons (car list) nil)))))
    q))


(defun queue-length (queue)
  "Return the number of elements in QUEUE."
  (length (queue-head queue)))


(defun queue-clear (queue)
  "Remove all elements from QUEUE."
  (setf (queue-head queue) nil
	(queue-tail queue) nil))


(queue--when-generators
 (iter-defun queue-iter (queue)
   "Return a queue iterator object.

Calling `iter-next' on this object will retrieve the next element
from the queue. The queue itself is not modified."
   (let ((list (queue-head queue)))
     (while list (iter-yield (pop list))))))

;;;; ChangeLog:

;; 2017-08-16  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Upgrade data structure packages to latest versions.
;; 
;; 2014-05-15  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	queue.el: fix buggy queue-first and queue-empty definitions.
;; 
;; 2012-04-30  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Minor fixes to commentaries, package headers, and whitespace
;; 
;; 	* queue.el: fix description of data structure in Commentary; add
;; 	Maintainer
;; 	 header.
;; 
;; 	* queue.el, heap.el, tNFA.el, trie.el, dict-tree.el: trivial whitespace
;; 	fixes.
;; 
;; 2012-04-29  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Add queue.el
;; 



(provide 'queue)


;;; queue.el ends here
