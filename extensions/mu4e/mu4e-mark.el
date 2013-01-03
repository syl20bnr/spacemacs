;;; mu4e-mark.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file are function related to marking messages; they assume we are
;; currently in the headers buffer.

;; Code:
(require 'mu4e-proc)
(require 'mu4e-utils)

(defcustom mu4e-headers-leave-behavior 'ask
  "What to do when user leaves the headers view (e.g. quits,
  refreshes or does a new search). Value is one of the following
  symbols:
- ask (ask the user whether to ignore the marks)
- apply (automatically apply the marks before doing anything else)
- ignore (automatically ignore the marks without asking)."
  :type 'symbol
  :group 'mu4e-headers)

(defvar mu4e-headers-show-target t
  "Whether to show targets (such as '-> delete', '-> /archive')
when marking message. Normally, this is useful information for the
user, however, when you often mark large numbers (thousands) of
message, showing the target makes this quite a bit slower (showing
the target uses an emacs feature called 'overlays', which aren't
particularly fast).")

;;; insert stuff;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e~mark-map nil
  "Map (hash) of docid->markinfo; when a message is marked, the
information is added here.

markinfo is a cons cell consisting of the following:
\(mark . target)
where
   MARK is the type of mark (move, trash, delete)
   TARGET (optional) is the target directory (for 'move')")

;; the mark-map is specific for the current header buffer
;; currently, there can't be more than one, but we never know what will
;; happen in the future

;; the fringe is the space on the left of headers, where we put marks below some
;; handy definitions; only `mu4e-mark-fringe-len' should be change (if ever),
;; the others follow from that.
(defconst mu4e~mark-fringe-len 2
  "Width of the fringe for marks on the left.")
(defconst mu4e~mark-fringe (make-string mu4e~mark-fringe-len ?\s)
  "The space on the left of message headers to put marks.")
(defconst mu4e~mark-fringe-format (format "%%-%ds" mu4e~mark-fringe-len)
  "Format string to set a mark and leave remaining space.")

(defun mu4e~mark-initialize ()
  "Initialize the marks subsystem."
  (make-local-variable 'mu4e~mark-map)
  (setq mu4e~mark-map (make-hash-table :size 16)))

(defun mu4e~mark-clear ()
  "Clear the marks subsystem."
  (clrhash mu4e~mark-map))

(defun mu4e-mark-at-point (mark &optional target)
  "Mark (or unmark) message at point. MARK specifies the
  mark-type. For `move'-marks there is also the TARGET argument,
  which specifies to which maildir the message is to be moved. The
  functipn works in both headers buffers and message buffers.

The following marks are available, and the corresponding props:

   MARK       TARGET    description
   ----------------------------------------------------------
   `move'     y         move the message to some folder
   `trash'    n         move the message to `mu4e-trash-folder'
   `delete'   n         remove the message
   `read'     n         mark the message as read
   `unread'   n         mark the message as unread
   `flag'     n         mark this message for flagging
   `unflag'   n         mark this message for unflagging
   `deferred' n         mark this message for *something* (decided later)
   `unmark'   n         unmark this message"
  (interactive)
  (let* ((docid (mu4e~headers-docid-at-point))
	  ;; get a cell with the mark char and the 'target' 'move' already has a
	  ;; target (the target folder) the other ones get a pseudo "target", as
	  ;; info for the user.
	  (markcell
	    (case mark
	      (move      `("m" . ,target))
	      (trash     '("d" . "trash"))
	      (delete    '("D" . "delete"))
	      (unread    '("o" . "unread"))
	      (read      '("r" . "read"))
	      (flag      '("+" . "flag"))
	      (unflag    '("-" . "unflag"))
	      (deferred  '("*" . "deferred"))
	      (unmark    '(" " . nil))
	      (otherwise (error "Invalid mark %S" mark))))
	  (markkar (car markcell))
	  (target (cdr markcell)))
    (unless docid (error "No message on this line"))
    (save-excursion
      (when (mu4e~headers-mark docid markkar)
	;; update the hash -- remove everything current, and if add the new stuff,
	;; unless we're unmarking
	(remhash docid mu4e~mark-map)
	;; remove possible overlays
	(remove-overlays (line-beginning-position) (line-end-position))
	;; now, let's set a mark (unless we were unmarking)
	(unless (eql mark 'unmark)
	  (puthash docid (cons mark target) mu4e~mark-map)
	  ;; when we have a target (ie., when moving), show the target folder in
	  ;; an overlay
	  (when (and target mu4e-headers-show-target)
	    (let* ((targetstr (propertize (concat "-> " target " ")
				'face 'mu4e-system-face))
		    ;; mu4e~headers-goto-docid docid t \will take us just after the
		    ;; docid cookie and then we skip the mu4e~mark-fringe
		    (start (+ (length mu4e~mark-fringe)
			     (mu4e~headers-goto-docid docid t)))
		    (overlay (make-overlay start (+ start (length targetstr)))))
	      (overlay-put overlay 'display targetstr)
	      docid)))))))


(defun mu4e-mark-set (mark &optional target)
  "Mark the header at point, or, if region is active, mark all
headers in the region."
  (interactive)
  (if (use-region-p)
    ;; mark all messages in the region.
    (save-excursion
      (let ((b (region-beginning)) (e (region-end)))
	(goto-char b)
	(while (<= (line-beginning-position) e)
	  (mu4e-mark-at-point mark target)
	  (forward-line 1))))
    ;; just a single message
    (mu4e-mark-at-point mark target)))

(defun mu4e-mark-restore (docid)
  "Restore the visual mark for the message with DOCID."
  (let ((markcell (gethash docid mu4e~mark-map)))
    (when markcell
      (save-excursion
	(when (mu4e~headers-goto-docid docid)
	  (mu4e-mark-at-point (car markcell) (cdr markcell)))))))

(defun mu4e-mark-for-move-set (&optional target)
  "Mark message at point or, if region is active, all messages in
the region, for moving to maildir TARGET. If target is not
provided, function asks for it."
  (interactive)
  (unless (mu4e~headers-docid-at-point)
    (error "No message at point."))
  (let* ((target (or target (mu4e-ask-maildir "Move message to: ")))
	  (target (if (string= (substring target 0 1) "/")
		    target
		    (concat "/" target)))
	  (fulltarget (concat mu4e-maildir target)))
    (when (or (file-directory-p fulltarget)
	    (and (yes-or-no-p
		   (format "%s does not exist. Create now?" fulltarget))
	      (mu4e~proc-mkdir fulltarget)))
      (mu4e-mark-set 'move target))))


(defun mu4e~mark-get-markpair (prompt &optional allow-deferred)
  "Ask user for a mark; return (MARK . TARGET). If ALLOW-DEFERRED
is non-nil, allow the 'deferred' pseudo mark as well."
  (let* ((marks '(("move"	. move)
		   ("dtrash"	. trash)
		   ("Delete"	. delete)
		   ("ounread"	. unread)
		   ("read"	. read)
		   ("+flag"	. flag)
		   ("-unflag"	. unflag)
		   ("unmark"	. unmark)))
	  (marks
	    (if allow-deferred
	      (append marks (list '("*deferred" . deferred)))
	      marks))
	  (mark (mu4e-read-option prompt marks))
	  (target
	    (when (eq mark 'move)
	      (mu4e-ask-maildir-check-exists "Move message to: "))))
    (cons mark target)))


(defun mu4e-mark-resolve-deferred-marks ()
  "Check if there are any deferred marks. If there are such marks,
replace them with a _real_ mark (ask the user which one)."
  (interactive)
  (let ((markpair))
    (maphash
      (lambda (docid val)
	(let ((mark (car val)) (target (cdr val)))
	  (when (eql mark 'deferred)
	    (unless markpair
	      (setq markpair
		(mu4e~mark-get-markpair "Set deferred mark to: " nil)))
	    (save-excursion
	      (when (mu4e~headers-goto-docid docid)
		(mu4e-mark-set (car markpair) (cdr markpair)))))))
      mu4e~mark-map)))

(defun mu4e-mark-execute-all (&optional no-confirmation)
  "Execute the actions for all marked messages in this
buffer. After the actions have been executed succesfully, the
affected messages are *hidden* from the current header list. Since
the headers are the result of a search, we cannot be certain that
the messages no longer matches the current one - to get that
certainty, we need to rerun the search, but we don't want to do
that automatically, as it may be too slow and/or break the users
flow. Therefore, we hide the message, which in practice seems to
work well.

If NO-CONFIRMATION is non-nil, don't ask user for confirmation."
  (interactive)
  (let ((marknum (hash-table-count mu4e~mark-map)))
    (if (zerop marknum)
      (message "Nothing is marked")
      (mu4e-mark-resolve-deferred-marks)
      (when (or no-confirmation
	      (y-or-n-p
		(format "Are you sure you want to execute %d mark%s?"
		  marknum (if (> marknum 1) "s" ""))))
	(maphash
	  (lambda (docid val)
	    (let ((mark (car val)) (target (cdr val)))
	      ;; note: whenever you do something with the message,
	      ;; it looses its N (new) flag
	      (case mark
		(move   (mu4e~proc-move docid target "-N"))
		(read   (mu4e~proc-move docid nil "+S-u-N"))
		(unread (mu4e~proc-move docid nil "-S+u-N"))
		(flag   (mu4e~proc-move docid nil "+F-u-N"))
		(unflag (mu4e~proc-move docid nil "-F-N"))
		(trash
		  (unless mu4e-trash-folder
		    (error "`mu4e-trash-folder' not set"))
		  (mu4e~proc-move docid mu4e-trash-folder "+T-N"))
		(delete (mu4e~proc-remove docid))
		(otherwise (error "Unrecognized mark %S" mark)))))
	  mu4e~mark-map))
      (mu4e-mark-unmark-all)
      (message nil))))

(defun mu4e-mark-unmark-all ()
  "Unmark all marked messages."
  (interactive)
  (when (or (null mu4e~mark-map) (zerop (hash-table-count mu4e~mark-map)))
    (error "Nothing is marked"))
  (maphash
    (lambda (docid val)
      (save-excursion
	(when (mu4e~headers-goto-docid docid)
	  (mu4e-mark-set 'unmark))))
    mu4e~mark-map)
  ;; in any case, clear the marks map
  (mu4e~mark-clear))


(defun mu4e-mark-docid-marked-p (docid)
  "Is the given docid marked?"
  (when (gethash docid mu4e~mark-map) t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e-mark-handle-when-leaving ()
  "If there are any marks in the current buffer, handle those
according to the value of `mu4e-headers-leave-behavior'. This
function is to be called before any further action (like searching,
quiting the buffer) is taken; returning t means 'take the following
action', return nil means 'don't do anything'"
  (let ((marknum (if mu4e~mark-map (hash-table-count mu4e~mark-map) 0))
	 (what mu4e-headers-leave-behavior))
    (unless (zerop marknum) ;; nothing to do
      (unless (or (eq what 'ignore) (eq what 'apply))
	;; if `mu4e-headers-leave-behavior' is not apply or ignore, ask the user
	(setq what
	  (let ((what (mu4e-read-option
			  "There are existing marks; should we: "
			  '( ("apply marks"   . apply)
			     ("ignore marks?" . ignore)))))
	    ;; we determined what to do... now do it
	    (when (eq what 'apply)
	      (mu4e-mark-execute-all t))))))))

(provide 'mu4e-mark)
;; End of mu4e-mark.el
