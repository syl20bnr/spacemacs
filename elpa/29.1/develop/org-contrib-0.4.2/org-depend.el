;;; org-depend.el --- TODO dependencies for Org-mode
;; Copyright (C) 2008-2021 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.08
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; WARNING: This file is just a PROOF OF CONCEPT, not a supported part
;;          of Org-mode.
;;
;; This is an example implementation of TODO dependencies in Org-mode.
;; It uses the new hooks in version 5.13 of Org-mode,
;; `org-trigger-hook' and `org-blocker-hook'.
;;
;; It implements the following:
;;
;; Triggering
;; ----------
;;
;; 1) If an entry contains a TRIGGER property that contains the string
;;    "chain-siblings(KEYWORD)", then switching that entry to DONE does
;;    do the following:
;;    - The sibling following this entry switched to todo-state KEYWORD.
;;    - The sibling also gets a TRIGGER property "chain-sibling(KEYWORD)",
;;      property, to make sure that, when *it* is DONE, the chain will
;;      continue.
;;
;; 2) If an entry contains a TRIGGER property that contains the string
;;    "chain-siblings-scheduled", then switching that entry to DONE does
;;    the following actions, similarly to "chain-siblings(KEYWORD)":
;;    - The sibling receives the same scheduled time as the entry
;;      marked as DONE (or, in the case, in which there is no scheduled
;;      time, the sibling does not get any either).
;;    - The sibling also gets the same TRIGGER property
;;      "chain-siblings-scheduled", so the chain can continue.
;;
;; 3) If the TRIGGER property contains the string
;;    "chain-find-next(KEYWORD[,OPTIONS])", then switching that entry
;;    to DONE do the following:
;;    - All siblings are of the entry are collected into a temporary
;;      list and then filtered and sorted according to OPTIONS
;;    - The first sibling on the list is changed into KEYWORD state
;;    - The sibling also gets the same TRIGGER property
;;      "chain-find-next", so the chain can continue.
;;
;;    OPTIONS should be a comma separated string without spaces, and
;;    can contain following options:
;;
;;    - from-top      the candidate list is all of the siblings in
;;                    the current subtree
;;
;;    - from-bottom   candidate list are all siblings from bottom up
;;
;;    - from-current  candidate list are all siblings from current item
;;                    until end of subtree, then wrapped around from
;;                    first sibling
;;
;;    - no-wrap       candidate list are siblings from current one down
;;
;;    - todo-only     Only consider siblings that have a todo keyword
;;    -
;;    - todo-and-done-only
;;                    Same as above but also include done items.
;;
;;    - priority-up   sort by highest priority
;;    - priority-down sort by lowest priority
;;    - effort-up     sort by highest effort
;;    - effort-down   sort by lowest effort
;;
;;    Default OPTIONS are from-top
;;
;;
;; 4) If the TRIGGER property contains any other words like
;;    XYZ(KEYWORD), these are treated as entry id's with keywords.  That
;;    means Org-mode will search for an entry with the ID property XYZ
;;    and switch that entry to KEYWORD as well.
;;
;; Blocking
;; --------
;;
;; 1) If an entry contains a BLOCKER property that contains the word
;;    "previous-sibling", the sibling above the current entry is
;;    checked when you try to mark it DONE.  If it is still in a TODO
;;    state, the current state change is blocked.
;;
;; 2) If the BLOCKER property contains any other words, these are
;;    treated as entry id's.  That means Org-mode will search for an
;;    entry with the ID property exactly equal to this word.  If any
;;    of these entries is not yet marked DONE, the current state change
;;    will be blocked.
;;
;; 3) Whenever a state change is blocked, an org-mark is pushed, so that
;;    you can find the offending entry with `C-c &'.
;;
;;; Example:
;;
;; When trying this example, make sure that the settings for TODO keywords
;; have been activated, i.e. include the following line and press C-c C-c
;; on the line before working with the example:
;;
;; #+TYP_TODO: TODO NEXT | DONE
;;
;; * TODO Win a million in Las Vegas
;;   The "third" TODO (see above) cannot become a TODO without this money.
;;
;;   :PROPERTIES:
;;     :ID: I-cannot-do-it-without-money
;;   :END:
;;
;; * Do this by doing a chain of TODO's
;; ** NEXT This is the first in this chain
;;    :PROPERTIES:
;;      :TRIGGER: chain-siblings(NEXT)
;;    :END:
;;
;; ** This is the second in this chain
;;
;; ** This is the third in this chain
;;    :PROPERTIES:
;;      :BLOCKER: I-cannot-do-it-without-money
;;    :END:
;;
;; ** This is the forth in this chain
;;    When this is DONE, we will also trigger entry XYZ-is-my-id
;;   :PROPERTIES:
;;     :TRIGGER: XYZ-is-my-id(TODO)
;;   :END:
;;
;; ** This is the fifth in this chain
;;
;; * Start writing report
;;   :PROPERTIES:
;;     :ID: XYZ-is-my-id
;;   :END:
;;
;;

(require 'org)
(eval-when-compile
  (require 'cl))

(defcustom org-depend-tag-blocked t
  "Whether to indicate blocked TODO items by a special tag."
  :group 'org
  :type 'boolean)

(defcustom org-depend-find-next-options
  "from-current,todo-only,priority-up"
  "Default options for chain-find-next trigger"
  :group 'org
  :type 'string)

(defmacro org-depend-act-on-sibling (trigger-val &rest rest)
  "Perform a set of actions on the next sibling, if it exists,
copying the sibling spec TRIGGER-VAL to the next sibling."
  `(catch 'exit
     (save-excursion
       (goto-char pos)
       ;; find the sibling, exit if no more siblings
       (condition-case nil
           (outline-forward-same-level 1)
         (error (throw 'exit t)))
       ;; mark the sibling TODO
       ,@rest
       ;; make sure the sibling will continue the chain
       (org-entry-add-to-multivalued-property
        nil "TRIGGER" ,trigger-val))))

(defvar org-depend-doing-chain-find-next nil)

(defun org-depend-trigger-todo (change-plist)
  "Trigger new TODO entries after the current is switched to DONE.
This does two different kinds of triggers:

- If the current entry contains a TRIGGER property that contains
  \"chain-siblings(KEYWORD)\", it goes to the next sibling, marks it
  KEYWORD and also installs the \"chain-sibling\" trigger to continue
  the chain.
- If the current entry contains a TRIGGER property that contains
  \"chain-siblings-scheduled\", we go to the next sibling and copy
  the scheduled time from the current task, also installing the property
  in the sibling.
- Any other word (space-separated) like XYZ(KEYWORD) in the TRIGGER
  property is seen as an entry id.  Org-mode finds the entry with the
  corresponding ID property and switches it to the state TODO as well."

  ;; Refresh the effort text properties
  (org-refresh-properties org-effort-property 'org-effort)
  ;; Get information from the plist
  (let* ((type (plist-get change-plist :type))
	 (pos (plist-get change-plist :position))
	 (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (org-log-done nil) ; IMPORTANT!: no logging during automatic trigger!
	 trigger triggers tr p1 p2 kwd id)
    (catch 'return
      (unless (eq type 'todo-state-change)
	;; We are only handling todo-state-change....
	(throw 'return t))
      (unless (and (member from org-not-done-keywords)
		   (member to org-done-keywords))
	;; This is not a change from TODO to DONE, ignore it
	(throw 'return t))

      ;; OK, we just switched from a TODO state to a DONE state
      ;; Lets see if this entry has a TRIGGER property.
      ;; If yes, split it up on whitespace.
      (setq trigger (org-entry-get pos "TRIGGER")
	    triggers (and trigger (split-string trigger)))

      ;; Go through all the triggers
      (while (setq tr (pop triggers))
	(cond
	 ((and (not org-depend-doing-chain-find-next)
	       (string-match "\\`chain-find-next(\\b\\(.+?\\)\\b\\(.*\\))\\'" tr))
	  ;; smarter sibling selection
	  (let* ((org-depend-doing-chain-find-next t)
		 (kwd (match-string 1 tr))
		 (options (match-string 2 tr))
		 (options (if (or (null options)
				  (equal options ""))
			      org-depend-find-next-options
			    options))
		 (todo-only (string-match "todo-only" options))
		 (todo-and-done-only (string-match "todo-and-done-only"
						   options))
		 (from-top (string-match "from-top" options))
		 (from-bottom (string-match "from-bottom" options))
		 (from-current (string-match "from-current" options))
		 (no-wrap (string-match "no-wrap" options))
		 (priority-up (string-match "priority-up" options))
		 (priority-down (string-match "priority-down" options))
		 (effort-up (string-match "effort-up" options))
		 (effort-down (string-match "effort-down" options)))
	    (save-excursion
	      (org-back-to-heading t)
	      (let ((this-item (point)))
		;; go up to the parent headline, then advance to next child
		(org-up-heading-safe)
		(let ((end (save-excursion (org-end-of-subtree t)
					   (point)))
		      (done nil)
		      (items '()))
		  (outline-next-heading)
		  (while (not done)
		    (if (not (looking-at org-complex-heading-regexp))
			(setq done t)
		      (let ((todo-kwd (match-string 2))
			    (tags (match-string 5))
			    (priority (org-get-priority (or (match-string 3) "")))
			    (effort (when (or effort-up effort-down)
				      (let ((effort (get-text-property (point) 'org-effort)))
					(when effort
					  (org-duration-to-minutes effort))))))
			(push (list (point) todo-kwd priority tags effort)
			      items))
		      (unless (org-goto-sibling)
			(setq done t))))
		  ;; massage the list according to options
		  (setq items
			(cond (from-top (nreverse items))
			      (from-bottom items)
			      ((or from-current no-wrap)
			       (let* ((items (nreverse items))
				      (pos (cl-position this-item items :key #'cl-first))
				      (items-before (cl-subseq items 0 pos))
				      (items-after (cl-subseq items pos)))
				 (if no-wrap items-after
				   (append items-after items-before))))
			      (t (nreverse items))))
		  (setq items (cl-remove-if
			       (lambda (item)
				 (or (equal (first item) this-item)
				     (and (not todo-and-done-only)
					  (member (second item) org-done-keywords))
				     (and (or todo-only
					      todo-and-done-only)
					  (null (second item)))))
			       items))
		  (setq items
			(sort
			 items
			 (lambda (item1 item2)
			   (let* ((p1 (third item1))
				  (p2 (third item2))
				  (e1 (fifth item1))
				  (e2 (fifth item2))
				  (p1-lt (< p1 p2))
				  (p1-gt (> p1 p2))
				  (e1-lt (and e1 (or (not e2) (< e1 e2))))
				  (e2-gt (and e2 (or (not e1) (> e1 e2)))))
			     (cond (priority-up
				    (or p1-gt
					(and (equal p1 p2)
					     (or (and effort-up e1-lt)
						 (and effort-down e2-gt)))))
				   (priority-down
				    (or p1-lt
					(and (equal p1 p2)
					     (or (and effort-up e1-lt)
						 (and effort-down e2-gt)))))
				   (effort-up
				    (or e2-gt (and (equal e1 e2) p1-gt)))
				   (effort-down
				    (or e1-lt (and (equal e1 e2) p1-gt))))))))
		  (when items
		    (goto-char (first (first items)))
		    (org-entry-add-to-multivalued-property nil "TRIGGER" tr)
		    (org-todo kwd)))))))
	 ((string-match "\\`chain-siblings(\\(.*?\\))\\'" tr)
	  ;; This is a TODO chain of siblings
	  (setq kwd (match-string 1 tr))
          (org-depend-act-on-sibling (format "chain-siblings(%s)" kwd)
                                     (org-todo kwd)))
	 ((string-match "\\`\\(\\S-+\\)(\\(.*?\\))\\'" tr)
	  ;; This seems to be ENTRY_ID(KEYWORD)
	  (setq id (match-string 1 tr)
		kwd (match-string 2 tr)
		p1 (org-find-entry-with-id id))
	  ;; First check current buffer, then all files.
	  (if p1
	      ;; There is an entry with this ID, mark it TODO.
	      (save-excursion
		(goto-char p1)
		(org-todo kwd))
	    (when (setq p2 (org-id-find id))
	      (save-excursion
		(with-current-buffer (find-file-noselect (car p2))
		  (goto-char (cdr p2))
		  (org-todo kwd))))))
         ((string-match "\\`chain-siblings-scheduled\\'" tr)
          (let ((time (org-get-scheduled-time pos)))
            (when time
              (org-depend-act-on-sibling
               "chain-siblings-scheduled"
               (org-schedule nil time))))))))))

(defun org-depend-block-todo (change-plist)
  "Block turning an entry into a TODO.
This checks for a BLOCKER property in an entry and checks
all the entries listed there.  If any of them is not done,
block changing the current entry into a TODO entry.  If the property contains
the word \"previous-sibling\", the sibling above the current entry is checked.
Any other words are treated as entry id's. If an entry exists with the
this ID property, that entry is also checked."
  ;; Get information from the plist
  (let* ((type (plist-get change-plist :type))
	 (pos (plist-get change-plist :position))
	 (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (org-log-done nil) ; IMPORTANT!: no logging during automatic trigger
	 blocker blockers bl p1 p2
	 (proceed-p
	  (catch 'return
            ;; If this is not a todo state change, or if this entry is
            ;; DONE, do not block
            (when (or (not (eq type 'todo-state-change))
                      (member from (cons 'done org-done-keywords))
                      (member to (cons 'todo org-not-done-keywords))
                      (not to))
              (throw 'return t))

	    ;; OK, the plan is to switch from nothing to TODO
	    ;; Lets see if we will allow it.  Find the BLOCKER property
	    ;; and split it on whitespace.
	    (setq blocker (org-entry-get pos "BLOCKER")
		  blockers (and blocker (split-string blocker)))

	    ;; go through all the blockers
	    (while (setq bl (pop blockers))
	      (cond
	       ((equal bl "previous-sibling")
		;; the sibling is required to be DONE.
		(catch 'ignore
		  (save-excursion
		    (goto-char pos)
		    ;; find the older sibling, exit if no more siblings
		    (unless (org-get-last-sibling)
		      (throw 'ignore t))
		    ;; Check if this entry is not yet done and block
		    (unless (org-entry-is-done-p)
		      ;; return nil, to indicate that we block the change!
		      (org-mark-ring-push)
		      (throw 'return nil)))))
	       ((setq p1 (org-find-entry-with-id bl))
		;; there is an entry with this ID, check it out
		(save-excursion
		  (goto-char p1)
		  (unless (org-entry-is-done-p)
		    ;; return nil, to indicate that we block the change!
		    (org-mark-ring-push)
		    (throw 'return nil))))
	       ((setq p2 (org-id-find bl))
		(save-excursion
		  (with-current-buffer (find-file-noselect (car p2))
		    (goto-char (cdr p2))
		    (unless (org-entry-is-done-p)
		      (org-mark-ring-push)
		      (throw 'return nil)))))))
	    ;; Return t to indicate that we are not blocking.
	    t)))
    (when org-depend-tag-blocked
      (org-toggle-tag "blocked" (if proceed-p 'off 'on)))

    proceed-p))

(add-hook 'org-trigger-hook 'org-depend-trigger-todo)
(add-hook 'org-blocker-hook 'org-depend-block-todo)

(provide 'org-depend)

;;; org-depend.el ends here
