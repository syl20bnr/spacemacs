;; mu4e-draft.el -- part of mu4e, the mu mail user agent for emacs
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

;; In this file, various functions to create draft messages

;; Code

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'mu4e-vars)
(require 'mu4e-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e~draft-user-agent-construct ()
  "Return the User-Agent string for mu4e.
This is either the value of `mu4e-user-agent', or, if not set, a
string based on the versions of mu4e and emacs."
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version))


(defun mu4e~draft-cite-original (msg)
  "Return a cited version of the original message MSG as a plist.
This function use gnus' `message-cite-function', and as such all
its settings apply."
  (with-temp-buffer
    (when (fboundp 'mu4e-view-message-text) ;; keep bytecompiler happy
      (insert (mu4e-view-message-text msg))
      ;; this seems to be needed, otherwise existing signatures
      ;; won't be stripped
      (message-yank-original)
      (goto-char (point-min))
      (push-mark (point-max))
      (funcall message-cite-function)
      (pop-mark)
      (buffer-string))))

(defun mu4e~draft-header (hdr val)
  "Return a header line of the form \"HDR: VAL\".
If VAL is nil, return nil."
  (when val (format "%s: %s\n" hdr val)))

(defun mu4e~draft-references-construct (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of thedmesg
q
existing References + In-Reply-To (which may be empty, an note
that :references includes the old in-reply-to as well) and the
message-id. If the message-id is empty, returns the old
References. If both are empty, return nil."
  (let* ( ;; these are the ones from the message being replied to / forwarded
	  (refs (mu4e-message-field msg :references))
	  (msgid (mu4e-message-field msg :message-id))
	  ;; now, append in
	  (refs (if (and msgid (not (string= msgid "")))
		  (append refs (list msgid)) refs))
	  ;; no doubles
	  (refs (delete-duplicates refs :test #'equal)))
    (mapconcat (lambda (id) (format "<%s>" id)) refs " ")))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine the recipient fields for new messages

(defun mu4e~draft-recipients-list-to-string (lst)
  "Convert a lst LST of address cells into a string with a list of
e-mail addresses. If LST is nil, returns nil."
  (when lst
    (mapconcat
      (lambda (addrcell)
	(let ((name (car addrcell))
	       (email (cdr addrcell)))
	  (if name
	    (format "\"%s\" <%s>" name email)
	    (format "%s" email))))
      lst ", ")))

(defun mu4e~draft-address-cell-equal (cell1 cell2)
  "Return t if CELL1 and CELL2 have the same e-mail address.
The comparison is done case-insensitively. If the cells done
match return nil. CELL1 and CELL2 are cons cells of the
form (NAME . EMAIL)."
  (string=
    (downcase (or (cdr cell1) ""))
    (downcase (or (cdr cell2) ""))))


(defun mu4e~draft-create-to-lst (origmsg)
  "Create a list of address for the To: in a new message, based on
the original message ORIGMSG. If the Reply-To address is set, use
that, otherwise use the From address. Note, whatever was in the To:
field before, goes to the Cc:-list (if we're doing a reply-to-all)."
  (let ((reply-to
	   (or (plist-get origmsg :reply-to) (plist-get origmsg :from))))
    (delete-duplicates reply-to :test #'mu4e~draft-address-cell-equal)))


(defun mu4e~draft-create-cc-lst (origmsg reply-all)
  "Create a list of address for the Cc: in a new message, based on
the original message ORIGMSG, and whether it's a reply-all."
  (when reply-all
    (let* ((cc-lst ;; get the cc-field from the original, remove dups
	     (delete-duplicates
	       (append
		 (plist-get origmsg :to)
		 (plist-get origmsg :cc))
	       :test #'mu4e~draft-address-cell-equal))
	    ;; now we have the basic list, but we must remove
	    ;; addresses also in the to list
	    (cc-lst
	      (delete-if
		(lambda (cc-cell)
		  (find-if
		    (lambda (to-cell)
		      (mu4e~draft-address-cell-equal cc-cell to-cell))
		    (mu4e~draft-create-to-lst origmsg)))
		cc-lst))
	    ;; finally, we need to remove ourselves from the cc-list
	    ;; unless mu4e-compose-keep-self-cc is non-nil
	    (cc-lst
	      (if (or mu4e-compose-keep-self-cc (null user-mail-address))
		cc-lst
		(delete-if
		  (lambda (cc-cell)
		    (member-if
		      (lambda (addr)
			(string= (downcase addr) (downcase (cdr cc-cell))))
		      mu4e-user-mail-address-list))
		  cc-lst))))
      cc-lst)))

(defun mu4e~draft-recipients-construct (field origmsg &optional reply-all)
  "Create value (a string) for the recipient field FIELD (a
symbol, :to or :cc), based on the original message ORIGMSG,
and (optionally) REPLY-ALL which indicates this is a reply-to-all
message. Return nil if there are no recipients for the particular field."
  (mu4e~draft-recipients-list-to-string
    (case field
      (:to
	(mu4e~draft-create-to-lst origmsg))
      (:cc
	(mu4e~draft-create-cc-lst origmsg reply-all))
      (otherwise
	(mu4e-error "Unsupported field")))))


(defun mu4e~draft-from-construct ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~draft-insert-mail-header-separator ()
  "Insert `mail-header-separator' in the first empty line of the message.
`message-mode' needs this line to know where the headers end and
the body starts. Note, in `mu4e-compose-mode', we use
`before-save-hook' and `after-save-hook' to ensure that this
separator is never written to the message file. Also see
`mu4e-remove-mail-header-separator'."
  ;; we set this here explicitly, since (as it has happened) a wrong
  ;; value for this (such as "") breaks address completion and other things
  (set (make-local-variable 'mail-header-separator)
    (purecopy "--text follows this line--"))
  (put 'mail-header-separator 'permanent-local t)
  (save-excursion
    (let ((sepa (propertize mail-header-separator
		  'intangible t
		  'read-only "Can't touch this"
		  'rear-nonsticky t
		  'font-lock-face 'mu4e-system-face)))
      (goto-char (point-min))
      ;; search for the first empty line
      (if (search-forward-regexp "^$" nil t)
	(replace-match (concat sepa))
	(progn 	;; no empty line? then prepend one
	  (goto-char (point-max))
	  (insert "\n" sepa))))))

(defun mu4e~draft-remove-mail-header-separator ()
  "Remove `mail-header-separator; we do this before saving a
file (and restore it afterwards), to ensure that the separator
never hits the disk. Also see `mu4e~draft-insert-mail-header-separator."
  (save-excursion
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (when (search-forward-regexp (concat "^" mail-header-separator))
      (let ((inhibit-read-only t))
	(replace-match "")))))

(defun mu4e~draft-user-wants-reply-all (origmsg)
  "Ask user whether she wants to reply to *all* recipients.
If there is just one recipient of ORIGMSG do nothing."
  (let* ((recipnum
	   (+ (length (mu4e~draft-create-to-lst origmsg))
	     (length (mu4e~draft-create-cc-lst origmsg t))))
	  (response
	    (if (= recipnum 1)
	      'all ;; with one recipient, we can reply to 'all'....
	      (mu4e-read-option
		"Reply to "
		`( (,(format "all %d recipients" recipnum) . all)
		   ("sender only" . sender-only))))))
    (eq response 'all)))

(defun mu4e~draft-message-filename-construct (&optional flagstr)
  "Construct a randomized name for a message file with flags FLAGSTR.
It looks something like
  <time>-<random>.<hostname>:2,
You can append flags."
  (let* ((hostname
	   (downcase
	     (save-match-data
	       (substring system-name
		 (string-match "^[^.]+" system-name) (match-end 0))))))
    (format "%s-%x%x.%s:2,%s"
      (format-time-string "%Y%m%d" (current-time))
      (emacs-pid) (random t) hostname (or flagstr ""))))

(defun mu4e~draft-common-construct ()
  "Construct the common headers for each message."
  (mu4e~draft-header "User-agent" (mu4e~draft-user-agent-construct)))

(defconst mu4e~draft-reply-prefix "Re: "
  "String to prefix replies with.")

(defun mu4e~draft-reply-construct (origmsg)
  "Create a draft message as a reply to original message ORIGMSG."
  (let* ((recipnum
	   (+ (length (mu4e~draft-create-to-lst origmsg))
	     (length (mu4e~draft-create-cc-lst origmsg t))))
	  (reply-all (mu4e~draft-user-wants-reply-all origmsg))
	  (old-msgid (plist-get origmsg :message-id))
	  (subject
	    (concat mu4e~draft-reply-prefix
	      (message-strip-subject-re (or (plist-get origmsg :subject) "")))))
     (concat
      (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
      (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
      (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
      (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg
				  reply-all))
      (mu4e~draft-header "Subject" subject)
      (mu4e~draft-header "References"
	(mu4e~draft-references-construct origmsg))
      (mu4e~draft-common-construct)
      (when old-msgid
	(mu4e~draft-header "In-reply-to" (format "<%s>" old-msgid)))
      "\n\n"
      (mu4e~draft-cite-original origmsg))))

(defconst mu4e~draft-forward-prefix "Fwd: "
  "String to prefix replies with.")

(defun mu4e~draft-forward-construct (origmsg)
  "Create a draft forward message for original message ORIGMSG."
  (let ((subject
	  (or (plist-get origmsg :subject) "")))
    (concat
      (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
      (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
      (mu4e~draft-header "To" "")
      (mu4e~draft-common-construct)
      (mu4e~draft-header "References"
	(mu4e~draft-references-construct origmsg))
      (mu4e~draft-header "Subject"
	(concat
	  ;; if there's no Fwd: yet, prepend it
	  (if (string-match "^Fwd:" subject)
	    ""
	    mu4e~draft-forward-prefix)
	  subject))
      "\n\n"
      (mu4e~draft-cite-original origmsg))))

(defun mu4e~draft-newmsg-construct ()
  "Create a new message."
  (concat
    (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
    (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
    (mu4e~draft-header "To" "")
    (mu4e~draft-header "Subject" "")
    (mu4e~draft-common-construct)))

(defvar mu4e~draft-drafts-folder nil
  "The drafts-folder for this compose buffer, based on
mu4e-drafts-folder', which will be evaluated once.")

(defun mu4e-draft-open (compose-type &optional msg)
  "Open a draft file for a new message (when COMPOSE-TYPE is reply, forward or new),
or open an existing draft (when COMPOSE-TYPE is edit).

The name of the draft folder is constructed from the concatenation
of `mu4e-maildir' and `mu4e-drafts-folder' (the latter will be
evaluated). The message file name is a unique name determined by
`mu4e-send-draft-file-name'. The initial contents will be created
from either `mu4e~draft-reply-construct', or
`mu4e~draft-forward-construct' or
`mu4e~draft-newmsg-construct'."
  ;; evaluate mu4e-drafts-folder once, here, and use that value throughout.
  (set (make-local-variable 'mu4e~draft-drafts-folder)
    (mu4e-get-drafts-folder msg))
  (put 'mu4e~draft-drafts-folder 'permanent-local t)
  (unless mu4e-maildir (mu4e-error "mu4e-maildir not set"))
  (if (eq compose-type 'edit)
    ;; case-1: re-editing a draft messages. in this case, we do know the full
    ;; path, but we cannot really now 'drafts folder'
    (find-file (mu4e-message-field msg :path))
    ;; case-2: creating a new message; in this case, we can determing
    ;; mu4e-get-drafts-folder
    (let* ((draftsfolder (mu4e-get-drafts-folder msg))
	    (draftpath
	      (format "%s/%s/cur/%s"
		mu4e-maildir
		draftsfolder
		(mu4e~draft-message-filename-construct "DS"))))
      (find-file draftpath)
      (insert
	(case compose-type
	  (reply   (mu4e~draft-reply-construct msg))
	  (forward (mu4e~draft-forward-construct msg))
	  (new     (mu4e~draft-newmsg-construct))
	  (t (mu4e-error "unsupported compose-type %S" compose-type))))
      ;; save the drafts folder 'permanently' for this buffer
      (set (make-local-variable 'mu4e~draft-drafts-folder) draftsfolder)
      (put 'mu4e~draft-drafts-folder 'permanent-local t))))


;; (defun mu4e-draft-setup-fcc ()
;;   "Setup Fcc, based on `mu4e-sent-messages-behavior'. If needed,
;; set the Fcc header, and register the handler function."
;;   (let* ((mdir
;; 	   (case mu4e-sent-messages-behavior
;; 	     (delete nil)
;; 	     (trash (mu4e-get-trash-folder mu4e-compose-parent-message))
;; 	     (sent  (mu4e-get-sent-folder mu4e-compose-parent-message))
;; 	     (otherwise
;; 	       (mu4e-error "unsupported value '%S' `mu4e-sent-messages-behavior'."
;; 		 mu4e-sent-messages-behavior))))
;; 	  (fccfile (and mdir
;; 		     (concat mu4e-maildir mdir "/cur/"
;; 		       (mu4e~compose-message-filename-construct "S")))))
;;     ;; if there's an fcc header, add it to the file
;;      (when fccfile
;;        (message-add-header (concat "Fcc: " fccfile "\n"))
;;        ;; sadly, we cannot define as 'buffer-local'...  this will screw up gnus
;;        ;; etc. if you run it after mu4e so, (hack hack) we reset it to the old
;;        ;; handler after we've done our thing.
;;       (setq message-fcc-handler-function
;; 	(lexical-let ((maildir mdir) (old-handler message-fcc-handler-function))
;; 	  (lambda (file)
;; 	    (setq message-fcc-handler-function old-handler) ;; reset the fcc handler
;; 	    (write-file file)		       ;; writing maildirs files is easy
;; 	    (mu4e~proc-add file maildir))))))) ;; update the database

(provide 'mu4e-draft)
