;;; org-expiry.el --- expiry mechanism for Org entries
;;
;; Copyright 2007-2021 Free Software Foundation, Inc.
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Version: 0.2
;; Keywords: org, expiry
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

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
;; This gives you a chance to get rid of old entries in your Org files
;; by expiring them.
;;
;; By default, entries that have no EXPIRY property are considered to be
;; new (i.e. 0 day old) and only entries older than one year go to the
;; expiry process, which consist in adding the ARCHIVE tag.  None of
;; your tasks will be deleted with the default settings.
;;
;; When does an entry expires?
;;
;; Consider this entry:
;;
;; * Stop watching TV
;;   :PROPERTIES:
;;   :CREATED:  <2008-01-07 lun 08:01>
;;   :EXPIRY:   <2008-01-09 08:01>
;;   :END:
;;
;; This entry will expire on the 9th, january 2008.

;; * Stop watching TV
;;   :PROPERTIES:
;;   :CREATED:  <2008-01-07 lun 08:01>
;;   :EXPIRY:   +1w
;;   :END:
;;
;; This entry will expire on the 14th, january 2008, one week after its
;; creation date.
;;
;; What happen when an entry is expired?  Nothing until you explicitly
;; M-x org-expiry-process-entries When doing this, org-expiry will check
;; for expired entries and request permission to process them.
;;
;; Processing an expired entries means calling the function associated
;; with `org-expiry-handler-function'; the default is to add the tag
;; :ARCHIVE:, but you can also add a EXPIRED keyword or even archive
;; the subtree.
;;
;; Is this useful?  Well, when you're in a brainstorming session, it
;; might be useful to know about the creation date of an entry, and be
;; able to archive those entries that are more than xxx days/weeks old.
;;
;; When you're in such a session, you can insinuate org-expiry like
;; this: M-x org-expiry-insinuate
;;
;; Then, each time you're pressing M-RET to insert an item, the CREATION
;; property will be automatically added.  Same when you're scheduling or
;; deadlining items.  You can deinsinuate: M-x org-expiry-deinsinuate

;;; Code:

;;; User variables:

(defgroup org-expiry nil
  "Org expiry process."
  :tag "Org Expiry"
  :group 'org)

(defcustom org-expiry-inactive-timestamps nil
  "Insert inactive timestamps for created/expired properties."
  :type 'boolean
  :group 'org-expiry)

(defcustom org-expiry-created-property-name "CREATED"
  "The name of the property for setting the creation date."
  :type 'string
  :group 'org-expiry)

(defcustom org-expiry-expiry-property-name "EXPIRY"
  "The name of the property for setting the expiry date/delay."
  :type 'string
  :group 'org-expiry)

(defcustom org-expiry-keyword "EXPIRED"
  "The default keyword for `org-expiry-add-keyword'."
  :type 'string
  :group 'org-expiry)

(defcustom org-expiry-wait "+1y"
  "Time span between the creation date and the expiry.
The default value for this variable (\"+1y\") means that entries
will expire if there are at least one year old.

If the expiry delay cannot be retrieved from the entry or the
subtree above, the expiry process compares the expiry delay with
`org-expiry-wait'.  This can be either an ISO date or a relative
time specification.  See `org-read-date' for details."
  :type 'string
  :group 'org-expiry)

(defcustom org-expiry-created-date "+0d"
  "The default creation date.
The default value of this variable (\"+0d\") means that entries
without a creation date will be handled as if they were created
today.

If the creation date cannot be retrieved from the entry or the
subtree above, the expiry process will compare the expiry delay
with this date.  This can be either an ISO date or a relative
time specification.  See `org-read-date' for details on relative
time specifications."
  :type 'string
  :group 'org-expiry)

(defcustom org-expiry-handler-function 'org-toggle-archive-tag
  "Function to process expired entries.
Possible candidates for this function are:

`org-toggle-archive-tag'
`org-expiry-add-keyword'
`org-expiry-archive-subtree'"
  :type 'function
  :group 'org-expiry)

(defcustom org-expiry-confirm-flag t
  "Non-nil means confirm expiration process."
  :type '(choice
	  (const :tag "Always require confirmation" t)
	  (const :tag "Do not require confirmation" nil)
	  (const :tag "Require confirmation in interactive expiry process"
		 interactive))
  :group 'org-expiry)

(defcustom org-expiry-advised-functions
  '(org-scheduled org-deadline org-time-stamp)
  "A list of advised functions.
`org-expiry-insinuate' will activate the expiry advice for these
functions.  `org-expiry-deinsinuate' will deactivate them."
  :type 'boolean
  :group 'list)

;;; Advices and insinuation:

(defadvice org-schedule (after org-schedule-update-created)
  "Update the creation-date property when calling `org-schedule'."
  (org-expiry-insert-created))

(defadvice org-deadline (after org-deadline-update-created)
  "Update the creation-date property when calling `org-deadline'."
  (org-expiry-insert-created))

(defadvice org-time-stamp (after org-time-stamp-update-created)
  "Update the creation-date property when calling `org-time-stamp'."
  (org-expiry-insert-created))

(defun org-expiry-insinuate (&optional arg)
  "Add hooks and activate advices for org-expiry.
If ARG, also add a hook to `before-save-hook' in `org-mode' and
restart `org-mode' if necessary."
  (interactive "P")
  (ad-activate 'org-schedule)
  (ad-activate 'org-time-stamp)
  (ad-activate 'org-deadline)
  (add-hook 'org-insert-heading-hook 'org-expiry-insert-created)
  (add-hook 'org-after-todo-state-change-hook 'org-expiry-insert-created)
  (add-hook 'org-after-tags-change-hook 'org-expiry-insert-created)
  (when arg
    (add-hook 'org-mode-hook
	      (lambda() (add-hook 'before-save-hook
				  'org-expiry-process-entries t t)))
    ;; need this to refresh org-mode hooks
    (when (eq major-mode 'org-mode)
      (org-mode)
      (if (called-interactively-p 'any)
	  (message "Org-expiry insinuated, `org-mode' restarted.")))))

(defun org-expiry-deinsinuate (&optional arg)
  "Remove hooks and deactivate advices for org-expiry.
If ARG, also remove org-expiry hook in Org's `before-save-hook'
and restart `org-mode' if necessary."
  (interactive "P")
  (ad-deactivate 'org-schedule)
  (ad-deactivate 'org-time-stamp)
  (ad-deactivate 'org-deadline)
  (remove-hook 'org-insert-heading-hook 'org-expiry-insert-created)
  (remove-hook 'org-after-todo-state-change-hook 'org-expiry-insert-created)
  (remove-hook 'org-after-tags-change-hook 'org-expiry-insert-created)
  (remove-hook 'org-mode-hook
	       (lambda() (add-hook 'before-save-hook
				   'org-expiry-process-entries t t)))
  (when arg
    ;; need this to refresh org-mode hooks
    (when (eq major-mode 'org-mode)
      (org-mode)
      (if (called-interactively-p 'any)
	  (message "Org-expiry de-insinuated, `org-mode' restarted.")))))

;;; org-expiry-expired-p:

(defun org-expiry-expired-p ()
  "Check if the entry at point is expired.
Return nil if the entry is not expired.  Otherwise return the
amount of time between today and the expiry date.

If there is no creation date, use `org-expiry-created-date'.
If there is no expiry date, use `org-expiry-wait'."
  (let* ((ex-prop org-expiry-expiry-property-name)
	 (cr-prop org-expiry-created-property-name)
	 (ct (current-time))
	 (cr (org-read-date nil t (or (org-entry-get (point) cr-prop t)
				      org-expiry-created-date)))
	 (ex-field (or (org-entry-get (point) ex-prop t) org-expiry-wait))
	 (ex (if (string-match "^[ \t]?[+-]" ex-field)
		 (time-add cr (time-subtract (org-read-date nil t ex-field) ct))
	       (org-read-date nil t ex-field))))
    (if (time-less-p ex ct)
	(time-subtract ct ex))))

;;; Expire an entry or a region/buffer:

(defun org-expiry-process-entry (&optional force)
  "Call `org-expiry-handler-function' on entry.
If FORCE is non-nil, don't require confirmation from the user.
Otherwise rely on `org-expiry-confirm-flag' to decide."
  (interactive "P")
  (save-excursion
    (when (called-interactively-p) (org-reveal))
    (when (org-expiry-expired-p)
      (org-back-to-heading)
      (looking-at org-complex-heading-regexp)
      (let* ((ov (make-overlay (point) (match-end 0)))
	     (e (org-expiry-expired-p))
	     (d (time-to-number-of-days e)))
	(overlay-put ov 'face 'secondary-selection)
	(if (or force
		(null org-expiry-confirm-flag)
		(and (eq org-expiry-confirm-flag 'interactive)
		     (not (interactive)))
		(and org-expiry-confirm-flag
		     (y-or-n-p (format "Entry expired by %d days.  Process? " d))))
	  (funcall org-expiry-handler-function))
	(delete-overlay ov)))))

(defun org-expiry-process-entries (beg end)
  "Process all expired entries between BEG and END.
The expiry process will run the function defined by
`org-expiry-handler-functions'."
  (interactive "r")
  (save-excursion
    (let ((beg (if (org-region-active-p)
		   (region-beginning) (point-min)))
	  (end (if (org-region-active-p)
		   (region-end) (point-max))))
      (goto-char beg)
      (let ((expired 0) (processed 0))
	(while (and (outline-next-heading) (< (point) end))
	  (when (org-expiry-expired-p)
	    (setq expired (1+ expired))
	    (if (if (called-interactively-p 'any)
		    (call-interactively 'org-expiry-process-entry)
		  (org-expiry-process-entry))
		(setq processed (1+ processed)))))
	(if (equal expired 0)
	    (message "No expired entry")
	  (message "Processed %d on %d expired entries"
		   processed expired))))))

;;; Insert created/expiry property:
(defun org-expiry-format-timestamp (timestr inactive)
  "Properly format TIMESTR into an org (in)active timestamp"
  (format (if inactive "[%s]" "<%s>") timestr))

(defun org-expiry-insert-created (&optional arg)
  "Insert or update a property with the creation date.
If ARG, always update it.  With one `C-u' prefix, silently update
to today's date.  With two `C-u' prefixes, prompt the user for to
update the date."
  (interactive "P")
  (let* ((d (org-entry-get (point) org-expiry-created-property-name))
	 d-time d-hour timestr)
    (when (or (null d) arg)
      ;; update if no date or non-nil prefix argument
      ;; FIXME Use `org-time-string-to-time'
      (setq d-time (if d (org-time-string-to-time d)
		     (current-time)))
      (setq d-hour (format-time-string "%H:%M" d-time))
      (setq timestr
	    ;; two C-u prefixes will call org-read-date
            (org-expiry-format-timestamp
             (if (equal arg '(16))
                 (org-read-date nil nil nil nil d-time d-hour)
               (format-time-string
                (replace-regexp-in-string "\\(^<\\|>$\\)" ""
                                          (cdr org-time-stamp-formats))))
             org-expiry-inactive-timestamps))
      (save-excursion
	(org-entry-put
	 (point) org-expiry-created-property-name timestr)))))

(defun org-expiry-insert-expiry (&optional today)
  "Insert a property with the expiry date.
With one `C-u' prefix, don't prompt interactively for the date
and insert today's date."
  (interactive "P")
  (let* ((d (org-entry-get (point) org-expiry-expiry-property-name))
	 d-time d-hour)
    (setq d-time (if d (org-time-string-to-time d)
		   (current-time)))
    (setq d-hour (format-time-string "%H:%M" d-time))
    (setq timestr (org-expiry-format-timestamp
                   (if today
                       (format-time-string
                        (replace-regexp-in-string "\\(^<\\|>$\\)" ""
                                                  (cdr org-time-stamp-formats)))
                     (org-read-date nil nil nil nil d-time d-hour))
                   org-expiry-inactive-timestamps))
    ;; maybe transform to inactive timestamp
    (if org-expiry-inactive-timestamps
	(setq timestr (concat "[" (substring timestr 1 -1) "]")))

    (save-excursion
      (org-entry-put
       (point) org-expiry-expiry-property-name timestr))))

;;; Functions to process expired entries:

(defun org-expiry-archive-subtree ()
  "Archive the entry at point if it is expired."
  (interactive)
  (save-excursion
    (if (org-expiry-expired-p)
	(org-archive-subtree)
      (if (called-interactively-p 'any)
	  (message "Entry at point is not expired.")))))

(defun org-expiry-add-keyword (&optional keyword)
  "Add KEYWORD to the entry at point if it is expired."
  (interactive "sKeyword: ")
  (if (or (member keyword org-todo-keywords-1)
	  (setq keyword org-expiry-keyword))
      (save-excursion
	(if (org-expiry-expired-p)
	    (org-todo keyword)
	  (if (called-interactively-p 'any)
	      (message "Entry at point is not expired."))))
    (error "\"%s\" is not a to-do keyword in this buffer" keyword)))

;; FIXME what about using org-refile ?

(provide 'org-expiry)

;;; org-expiry.el ends here
