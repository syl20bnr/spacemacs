;;; org-interactive-query.el --- Interactive modification of agenda query
;;
;; Copyright 2007-2021 Free Software Foundation, Inc.
;;
;; Author: Christopher League <league at contrapunctus dot net>
;; Version: 1.0
;; Keywords: org, wp
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file is DEPRECATED. The functionality here has been mostly subsumed by
;; features added to Org agenda, especially commands that begin with
;; org-agenda-filter*.

;; This library implements interactive modification of a tags/todo query
;; in the org-agenda.  It adds 4 keys to the agenda
;;
;; /   add a keyword as a positive selection criterion
;; \   add a keyword as a newgative selection criterion
;; =   clear a keyword from the selection string
;; ;

(require 'org)

(org-defkey org-agenda-mode-map "=" 'org-agenda-query-clear-cmd)
(org-defkey org-agenda-mode-map "/" 'org-agenda-query-and-cmd)
(org-defkey org-agenda-mode-map ";" 'org-agenda-query-or-cmd)
(org-defkey org-agenda-mode-map "\\" 'org-agenda-query-not-cmd)

;;; Agenda interactive query manipulation

(defcustom org-agenda-query-selection-single-key t
  "Non-nil means query manipulation exits after first change.
When nil, you have to press RET to exit it.
During query selection, you can toggle this flag with `C-c'.
This variable can also have the value `expert'.  In this case, the window
displaying the tags menu is not even shown, until you press C-c again."
  :group 'org-agenda
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert)))

(defun org-agenda-query-selection (current op table &optional todo-table)
  "Fast query manipulation with single keys.
CURRENT is the current query string, OP is the initial
operator (one of \"+|-=\"), TABLE is an alist of tags and
corresponding keys, possibly with grouping information.
TODO-TABLE is a similar table with TODO keywords, should these
have keys assigned to them.  If the keys are nil, a-z are
automatically assigned.  Returns the new query string, or nil to
not change the current one."
  (let* ((fulltable (append table todo-table))
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (expert (eq org-agenda-query-selection-single-key 'expert))
	 (exit-after-next org-agenda-query-selection-single-key)
	 (done-keywords org-done-keywords)
         tbl char cnt e groups ingroup
	 tg c2 c c1 ntable rtn)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org tags*"))
	(delete-other-windows)
	(split-window-vertically)
	(org-switch-to-buffer-other-window (get-buffer-create " *Org tags*")))
      (erase-buffer)
      (setq-local org-done-keywords done-keywords)
      (insert "Query:    " current "\n")
      (org-agenda-query-op-line op)
      (insert "\n\n")
      (org-fast-tag-show-exit exit-after-next)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
	(cond
	 ((equal e '(:startgroup))
	  (push '() groups) (setq ingroup t)
	  (when (not (= cnt 0))
	    (setq cnt 0)
	    (insert "\n"))
	  (insert "{ "))
	 ((equal e '(:endgroup))
	  (setq ingroup nil cnt 0)
	  (insert "}\n"))
	 (t
	  (setq tg (car e) c2 nil)
	  (if (cdr e)
	      (setq c (cdr e))
	    ;; automatically assign a character.
	    (setq c1 (string-to-char
		      (downcase (substring
				 tg (if (= (string-to-char tg) ?@) 1 0)))))
	    (if (or (rassoc c1 ntable) (rassoc c1 table))
		(while (or (rassoc char ntable) (rassoc char table))
		  (setq char (1+ char)))
	      (setq c2 c1))
	    (setq c (or c2 char)))
	  (if ingroup (push tg (car groups)))
	  (setq tg (org-add-props tg nil 'face
				  (cond
				   ((not (assoc tg table))
				    (org-get-todo-face tg))
				   (t nil))))
	  (if (and (= cnt 0) (not ingroup)) (insert "  "))
	  (insert "[" c "] " tg (make-string
				 (- fwidth 4 (length tg)) ?\ ))
	  (push (cons tg c) ntable)
	  (when (= (setq cnt (1+ cnt)) ncol)
	    (insert "\n")
	    (if ingroup (insert "  "))
	    (setq cnt 0)))))
      (setq ntable (nreverse ntable))
      (insert "\n")
      (goto-char (point-min))
      (if (and (not expert) (fboundp 'fit-window-to-buffer))
	  (fit-window-to-buffer))
      (setq rtn
	    (catch 'exit
	      (while t
		(message "[a-z..]:Toggle [SPC]:clear [RET]:accept [TAB]:free%s%s"
			 (if groups " [!] no groups" " [!]groups")
			 (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		(cond
		 ((= c ?\r) (throw 'exit t))
		 ((= c ?!)
		  (setq groups (not groups))
		  (goto-char (point-min))
		  (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		 ((= c ?\C-c)
		  (if (not expert)
		      (org-fast-tag-show-exit
		       (setq exit-after-next (not exit-after-next)))
		    (setq expert nil)
		    (delete-other-windows)
		    (split-window-vertically)
		    (org-switch-to-buffer-other-window " *Org tags*")
		    (and (fboundp 'fit-window-to-buffer)
			 (fit-window-to-buffer))))
		 ((or (= c ?\C-g)
		      (and (= c ?q) (not (rassoc c ntable))))
		  (setq quit-flag t))
		 ((= c ?\ )
		  (setq current "")
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\[)             ; clear left
                  (org-agenda-query-decompose current)
                  (setq current (concat "/" (match-string 2 current)))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\])             ; clear right
                  (org-agenda-query-decompose current)
                  (setq current (match-string 1 current))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\t)
		  (condition-case nil
		      (setq current (read-string "Query: " current))
		    (quit))
		  (if exit-after-next (setq exit-after-next 'now)))
                 ;; operators
                 ((or (= c ?/) (= c ?+)) (setq op "+"))
                 ((or (= c ?\;) (= c ?|)) (setq op "|"))
                 ((or (= c ?\\) (= c ?-)) (setq op "-"))
                 ((= c ?=) (setq op "="))
                 ;; todos
                 ((setq e (rassoc c todo-table) tg (car e))
                  (setq current (org-agenda-query-manip
                                 current op groups 'todo tg))
                  (if exit-after-next (setq exit-after-next 'now)))
                 ;; tags
                 ((setq e (rassoc c ntable) tg (car e))
                  (setq current (org-agenda-query-manip
                                 current op groups 'tag tg))
                  (if exit-after-next (setq exit-after-next 'now))))
		(if (eq exit-after-next 'now) (throw 'exit t))
		(goto-char (point-min))
		(beginning-of-line 1)
		(delete-region (point) (point-at-eol))
                (insert "Query:    " current)
                (beginning-of-line 2)
                (delete-region (point) (point-at-eol))
                (org-agenda-query-op-line op)
		(goto-char (point-min)))))
      (if rtn current nil))))

(defun org-agenda-query-op-line (op)
  (insert "Operator: "
          (org-agenda-query-op-entry (equal op "+") "/+" "and")
          (org-agenda-query-op-entry (equal op "|") ";|" "or")
          (org-agenda-query-op-entry (equal op "-") "\\-" "not")
          (org-agenda-query-op-entry (equal op "=") "=" "clear")))

(defun org-agenda-query-op-entry (matchp chars str)
  (if matchp
      (org-add-props (format "[%s %s]  " chars (upcase str))
          nil 'face 'org-todo)
    (format "[%s]%s   " chars str)))

(defun org-agenda-query-decompose (current)
  (string-match "\\([^/]*\\)/?\\(.*\\)" current))

(defun org-agenda-query-clear (current prefix tag)
  (if (string-match (concat prefix "\\b" (regexp-quote tag) "\\b") current)
      (replace-match "" t t current)
    current))

(defun org-agenda-query-manip (current op groups kind tag)
  "Apply an operator to a query string and a tag.
CURRENT is the current query string, OP is the operator, GROUPS is a
list of lists of tags that are mutually exclusive.  KIND is 'tag for a
regular tag, or 'todo for a TODO keyword, and TAG is the tag or
keyword string."
  ;; If this tag is already in query string, remove it.
  (setq current (org-agenda-query-clear current "[-\\+&|]?" tag))
  (if (equal op "=") current
    ;; When using AND, also remove mutually exclusive tags.
    (if (equal op "+")
        (loop for g in groups do
              (if (member tag g)
                  (mapc (lambda (x)
                          (setq current
                                (org-agenda-query-clear current "\\+" x)))
                        g))))
    ;; Decompose current query into q1 (tags) and q2 (TODOs).
    (org-agenda-query-decompose current)
    (let* ((q1 (match-string 1 current))
           (q2 (match-string 2 current)))
      (cond
       ((eq kind 'tag)
        (concat q1 op tag "/" q2))
       ;; It's a TODO; when using AND, drop all other TODOs.
       ((equal op "+")
        (concat q1 "/+" tag))
       (t
        (concat q1 "/" q2 op tag))))))

(defun org-agenda-query-global-todo-keys (&optional files)
  "Return alist of all TODO keywords and their fast keys, in all FILES."
  (let (alist)
    (unless (and files (car files))
      (setq files (org-agenda-files)))
    (save-excursion
      (loop for f in files do
            (set-buffer (find-file-noselect f))
            (loop for k in org-todo-key-alist do
                  (setq alist (org-agenda-query-merge-todo-key
                               alist k)))))
    alist))

(defun org-agenda-query-merge-todo-key (alist entry)
  (let (e)
    (cond
     ;; if this is not a keyword (:startgroup, etc), ignore it
     ((not (stringp (car entry))))
     ;; if keyword already exists, replace char if it's null
     ((setq e (assoc (car entry) alist))
      (when (null (cdr e)) (setcdr e (cdr entry))))
     ;; if char already exists, prepend keyword but drop char
     ((rassoc (cdr entry) alist)
      (message "TRACE POSITION 2")
      (setq alist (cons (cons (car entry) nil) alist)))
     ;; else, prepend COPY of entry
     (t
      (setq alist (cons (cons (car entry) (cdr entry)) alist)))))
  alist)

(defun org-agenda-query-generic-cmd (op)
  "Activate query manipulation with OP as initial operator."
  (let ((q (org-agenda-query-selection org-agenda-query-string op
                                       org-tag-alist
                                       (org-agenda-query-global-todo-keys))))
    (when q
      (setq org-agenda-query-string q)
      (org-agenda-redo))))

(defun org-agenda-query-clear-cmd ()
  "Activate query manipulation, to clear a tag from the string."
  (interactive)
  (org-agenda-query-generic-cmd "="))

(defun org-agenda-query-and-cmd ()
  "Activate query manipulation, initially using the AND (+) operator."
  (interactive)
  (org-agenda-query-generic-cmd "+"))

(defun org-agenda-query-or-cmd ()
  "Activate query manipulation, initially using the OR (|) operator."
  (interactive)
  (org-agenda-query-generic-cmd "|"))

(defun org-agenda-query-not-cmd ()
  "Activate query manipulation, initially using the NOT (-) operator."
  (interactive)
  (org-agenda-query-generic-cmd "-"))

(provide 'org-interactive-query)
