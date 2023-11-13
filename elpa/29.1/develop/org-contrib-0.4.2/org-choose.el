;;; org-choose.el --- decision management for org-mode

;; Copyright (C) 2009-2014, 2021 Tom Breton (Tehom)

;; This file is not part of GNU Emacs.

;; Author: Tom Breton (Tehom)
;; Keywords: outlines, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is code to support decision management.  It lets you treat a
;; group of sibling items in org-mode as alternatives in a decision.

;; There are no user commands in this file.  You use it by:
;;   * Loading it (manually or by M-x customize-apropos org-modules)

;;   * Setting up at least one set of TODO keywords with the
;;     interpretation "choose" by either:

;;     * Using the file directive #+CHOOSE_TODO:

;;       * For instance, "#+CHOOSE_TODO: NO(,-) MAYBE(,0) YES"

;;     * Or by M-x customize-apropos org-todo-keywords

;;   * Operating on single items with the TODO commands.

;;     * Use C-S-right to change the keyword set.  Use this to change to
;;       the "choose" keyword set that you just defined.

;;     * Use S-right to advance the TODO mark to the next setting.

;;       For "choose", that means you like this alternative more than
;;       before.  Other alternatives will be automatically demoted to
;;       keep your settings consistent.

;;     * Use S-left to demote TODO to the previous setting.

;;       For "choose", that means you don't like this alternative as much
;;       as before.  Other alternatives will be automatically promoted,
;;       if this item was all that was keeping them down.

;;     * All the other TODO commands are available and behave essentially
;;       the normal way.

;;; Requires

(require 'org)
					;(eval-when-compile
					;   (require 'cl))
(require 'cl)

;;; Body
;;; The variables

(defstruct (org-choose-mark-data. (:type list))
  "The format of an entry in org-choose-mark-data.
Indexes are 0-based or `nil'.
"
  keyword
  bot-lower-range
  top-upper-range
  range-length
  static-default
  all-keywords)

(defvar org-choose-mark-data
  ()
  "Alist of information for choose marks.

Each entry is an `org-choose-mark-data.'" )
(make-variable-buffer-local 'org-choose-mark-data)
;;;_ , For setup
;;;_  . org-choose-filter-one

(defun org-choose-filter-one (i)
  "Return a list of
 * a canonized version of the string
 * optionally one symbol"

  (if
      (not
       (string-match "(.*)" i))
      (list i i)
    (let*
	(
	 (end-text (match-beginning 0))
	 (vanilla-text (substring i 0 end-text))
	 ;;Get the parenthesized part.
	 (match (match-string 0 i))
	 ;;Remove the parentheses.
	 (args (substring match 1 -1))
	 ;;Split it
	 (arglist
	  (let
	      ((arglist-x (org-split-string args ",")))
	    ;;When string starts with "," `split-string' doesn't
	    ;;make a first arg, so in that case make one
	    ;;manually.
	    (if
		(string-match "^," args)
		(cons nil arglist-x)
	      arglist-x)))
	 (decision-arg (second arglist))
	 (type
	  (cond
	   ((string= decision-arg "0")
	    'default-mark)
	   ((string= decision-arg "+")
	    'top-upper-range)
	   ((string= decision-arg "-")
	    'bot-lower-range)
	   (t nil)))
	 (vanilla-arg (first arglist))
	 (vanilla-mark
	  (if vanilla-arg
	      (concat vanilla-text "("vanilla-arg")")
	    vanilla-text)))
      (if type
	  (list vanilla-text vanilla-mark type)
	(list vanilla-text vanilla-mark)))))

;;;_  . org-choose-setup-vars
(defun org-choose-setup-vars (bot-lower-range top-upper-range
					      static-default num-items all-mark-texts)
  "Add to org-choose-mark-data according to arguments"
  (let*
      ((tail
	;;If there's no bot-lower-range or no default, we don't
	;;have ranges.
	(cdr
	 (if (and static-default bot-lower-range)
	     (let*
		 ;;If there's no top-upper-range, use the last
		 ;;item.
		 ((top-upper-range
		   (or top-upper-range (1- num-items)))
		  (lower-range-length
		   (1+ (- static-default bot-lower-range)))
		  (upper-range-length
		   (- top-upper-range static-default))
		  (range-length
		   (min upper-range-length lower-range-length)))
	       (make-org-choose-mark-data.
		:keyword nil
		:bot-lower-range bot-lower-range
		:top-upper-range top-upper-range
		:range-length    range-length
		:static-default static-default
		:all-keywords all-mark-texts))
	   (make-org-choose-mark-data.
	    :keyword nil
	    :bot-lower-range nil
	    :top-upper-range nil
	    :range-length    nil
	    :static-default (or static-default 0)
	    :all-keywords all-mark-texts)))))
    (dolist (text all-mark-texts)
      (pushnew (cons text tail)
	       org-choose-mark-data
	       :test
               (lambda (a b)
                 (equal (car a) (car b)))))))

;;; org-choose-filter-tail
(defun org-choose-filter-tail (raw)
  "Return a translation of RAW to vanilla and set appropriate
buffer-local variables.

RAW is a list of strings representing the input text of a choose
interpretation."
  (let
      ((vanilla-list nil)
       (all-mark-texts nil)
       (index 0)
       bot-lower-range top-upper-range range-length static-default)
    (dolist (i raw)
      (destructuring-bind
	  (vanilla-text vanilla-mark &optional type)
	  (org-choose-filter-one i)
	(cond
	 ((eq type 'bot-lower-range)
	  (setq bot-lower-range index))
	 ((eq type 'top-upper-range)
	  (setq top-upper-range index))
	 ((eq type 'default-mark)
	  (setq static-default index)))
	(cl-incf index)
	(push vanilla-text all-mark-texts)
	(push vanilla-mark vanilla-list)))

    (org-choose-setup-vars bot-lower-range top-upper-range
			   static-default index (reverse all-mark-texts))
    (nreverse vanilla-list)))

;;; org-choose-setup-filter

(defun org-choose-setup-filter (raw)
  "A setup filter for choose interpretations."
  (when (eq (car raw) 'choose)
    (cons
     'choose
     (org-choose-filter-tail (cdr raw)))))

;;; org-choose-conform-after-promotion
(defun org-choose-conform-after-promotion (entry-pos keywords highest-ok-ix)
  "Conform the current item after another item was promoted"
  (unless
      ;;Skip the entry that triggered this by skipping any entry with
      ;;the same starting position.  plist uses the start of the
      ;;header line as the position, but map no longer does, so we
      ;;have to go back to the heading.
      (=
       (save-excursion
	 (org-back-to-heading)
	 (point))
       entry-pos)
    (let
	((ix
	  (org-choose-get-entry-index keywords)))
      ;;If the index of the entry exceeds the highest allowable
      ;;index, change it to that.
      (when (and ix
		 (> ix highest-ok-ix))
	(org-todo
	 (nth highest-ok-ix keywords))))))
;;;_  . org-choose-conform-after-demotion
(defun org-choose-conform-after-demotion (entry-pos keywords
						    raise-to-ix
						    old-highest-ok-ix)
  "Conform the current item after another item was demoted."
  (unless
      ;;Skip the entry that triggered this.
      (=
       (save-excursion
	 (org-back-to-heading)
	 (point))
       entry-pos)
    (let
	((ix
	  (org-choose-get-entry-index keywords)))
      ;;If the index of the entry was at or above the old allowable
      ;;position, change it to the new mirror position if there is
      ;;one.
      (when (and
	     ix
	     raise-to-ix
	     (>= ix old-highest-ok-ix))
	(org-todo
	 (nth raise-to-ix keywords))))))

;;; org-choose-keep-sensible (the org-trigger-hook function)
(defun org-choose-keep-sensible (change-plist)
  "Bring the other items back into a sensible state after an item's
setting was changed."
  (let*
      (  (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (entry-pos
	  (set-marker
	   (make-marker)
	   (plist-get change-plist :position)))
	 (kwd-data
	  (assoc to org-todo-kwd-alist)))
    (when
	(eq (nth 1 kwd-data) 'choose)
      (let*
	  (
	   (data
	    (assoc to org-choose-mark-data))
	   (keywords
	    (org-choose-mark-data.-all-keywords data))
	   (old-index
	    (org-choose-get-index-in-keywords
	     from
	     keywords))
	   (new-index
	    (org-choose-get-index-in-keywords
	     to
	     keywords))
	   (highest-ok-ix
	    (org-choose-highest-other-ok
	     new-index
	     data))
	   (funcdata
	    (cond
	     ;;The entry doesn't participate in conformance,
	     ;;so give `nil' which does nothing.
	     ((not highest-ok-ix) nil)
	     ;;The entry was created or promoted
	     ((or
	       (not old-index)
	       (> new-index old-index))
	      (list
	       #'org-choose-conform-after-promotion
	       entry-pos keywords
	       highest-ok-ix))
	     (t	;;Otherwise the entry was demoted.
	      (let
		  (
		   (raise-to-ix
		    (min
		     highest-ok-ix
		     (org-choose-mark-data.-static-default
		      data)))
		   (old-highest-ok-ix
		    (org-choose-highest-other-ok
		     old-index
		     data)))
		(list
		 #'org-choose-conform-after-demotion
		 entry-pos
		 keywords
		 raise-to-ix
		 old-highest-ok-ix))))))
	(if funcdata
	    ;;The funny-looking names are to make variable capture
	    ;;unlikely.  (Poor-man's lexical bindings).
	    (destructuring-bind (func-d473 . args-46k) funcdata
	      (let
		  ((map-over-entries
		    (org-choose-get-fn-map-group))
		   ;;We may call `org-todo', so let various hooks
		   ;;`nil' so we don't cause loops.
		   org-after-todo-state-change-hook
		   org-trigger-hook
		   org-blocker-hook
		   org-todo-get-default-hook
		   ;;Also let this alist `nil' so we don't log
		   ;;secondary transitions.
		   org-todo-log-states)
		;;Map over group
		(funcall map-over-entries
                         (lambda ()
                           (apply func-d473 args-46k))))))))
    ;;Remove the marker
    (set-marker entry-pos nil)))

;;; Getting the default mark
;;; org-choose-get-index-in-keywords
(defun org-choose-get-index-in-keywords (ix all-keywords)
  "Return the index of the current entry."
  (if ix
      (position ix all-keywords
		:test #'equal)))

;;; org-choose-get-entry-index
(defun org-choose-get-entry-index (all-keywords)
  "Return index of current entry."
  (let*
      ((state (org-entry-get (point) "TODO")))
    (org-choose-get-index-in-keywords state all-keywords)))

;;; org-choose-get-fn-map-group

(defun org-choose-get-fn-map-group ()
  "Return a function to map over the group"
  (lambda (fn)
    (require 'org-agenda) ;; `org-map-entries' seems to need it.
    (save-excursion
      (unless (org-up-heading-safe)
        (error "Choosing is only supported between siblings in a tree, not on top level"))
      (let
          ((level (org-reduced-level (org-outline-level))))
        (save-restriction
          (org-map-entries
           fn
           (format "LEVEL=%d" level)
           'tree))))))

;;; org-choose-get-highest-mark-index

(defun org-choose-get-highest-mark-index (keywords)
  "Get the index of the highest current mark in the group.
If there is none, return 0"
  (let*
      ;;Func maps over applicable entries.
      ((map-over-entries
	(org-choose-get-fn-map-group))
       (indexes-list
	(remove nil
		(funcall map-over-entries
                         (lambda ()
                           (org-choose-get-entry-index keywords))))))
    (if
	indexes-list
	(apply #'max indexes-list)
      0)))

;;; org-choose-highest-ok

(defun org-choose-highest-other-ok (ix data)
  "Return the highest index that any choose mark can sensibly have,
given that another mark has index IX.
DATA must be a `org-choose-mark-data.'."
  (let
      ((bot-lower-range
	(org-choose-mark-data.-bot-lower-range data))
       (top-upper-range
	(org-choose-mark-data.-top-upper-range data))
       (range-length
	(org-choose-mark-data.-range-length data)))
    (when (and ix bot-lower-range)
      (let*
	  ((delta
	    (- top-upper-range ix)))
	(unless
	    (< range-length delta)
	  (+ bot-lower-range delta))))))

;;; org-choose-get-default-mark-index

(defun org-choose-get-default-mark-index (data)
  "Return the index of the default mark in a choose interpretation.

DATA must be a `org-choose-mark-data.'."
  (or
   (let
       ((highest-mark-index
	 (org-choose-get-highest-mark-index
	  (org-choose-mark-data.-all-keywords data))))
     (org-choose-highest-other-ok
      highest-mark-index data))
   (org-choose-mark-data.-static-default data)))

;;; org-choose-get-mark-N
(defun org-choose-get-mark-N (n data)
  "Get the text of the nth mark in a choose interpretation."

  (let*
      ((l (org-choose-mark-data.-all-keywords data)))
    (nth n l)))

;;; org-choose-get-default-mark

(defun org-choose-get-default-mark (new-mark old-mark)
  "Get the default mark IFF in a choose interpretation.
NEW-MARK and OLD-MARK are the text of the new and old marks."
  (let*
      ((old-kwd-data
	(assoc old-mark org-todo-kwd-alist))
       (new-kwd-data
	(assoc new-mark org-todo-kwd-alist))
       (becomes-choose
	(and
	 (or
	  (not old-kwd-data)
	  (not
	   (eq (nth 1 old-kwd-data) 'choose)))
	 (eq (nth 1 new-kwd-data) 'choose))))
    (when
	becomes-choose
      (let
	  ((new-mark-data
	    (assoc new-mark org-choose-mark-data)))
	(if
	    new-mark
	    (org-choose-get-mark-N
	     (org-choose-get-default-mark-index
	      new-mark-data)
	     new-mark-data)
	  (error "Somehow got an unrecognizable mark"))))))

;;; Setting it all up

(eval-after-load 'org
  '(progn
     (add-to-list 'org-todo-setup-filter-hook
		  #'org-choose-setup-filter)
     (add-to-list 'org-todo-get-default-hook
		  #'org-choose-get-default-mark)
     (add-to-list 'org-trigger-hook
		  #'org-choose-keep-sensible)
     (add-to-list 'org-todo-interpretation-widgets
		  '(:tag "Choose   (to record decisions)" choose)
		  'append)))

(provide 'org-choose)

;;; org-choose.el ends here
