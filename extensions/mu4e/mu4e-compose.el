;; mu4e-compose.el -- part of mu4e, the mu mail user agent for emacs
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

;; In this file, various functions to compose/send messages, piggybacking on
;; gnus' message mode

;;; Code:
;; we use some stuff from gnus..

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'message)
(require 'mail-parse)
(require 'smtpmail)
(require 'rfc2368)

(require 'mu4e-utils)
(require 'mu4e-vars)
(require 'mu4e-proc)
(require 'mu4e-actions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing / Sending messages
(defgroup mu4e-compose nil
  "Customizations for composing/sending messages."
  :group 'mu4e)

(defcustom mu4e-reply-to-address nil
  "The Reply-To address (if this, for some reason, is not equal to
the From: address.)"
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-sent-messages-behavior 'sent
  "Determines what mu4e does with sent messages - this is a symbol
which can be either:
'sent   --> move the sent message to the Sent-folder (`mu4e-sent-folder')
'trash  --> move the sent message to the Trash-folder (`mu4e-trash-folder')
'delete --> delete the sent message.
Note, when using GMail/IMAP, you should set this to either 'trash
or 'delete, since GMail already takes care of keeping copies in the
sent folder."
  :type 'symbol
  :safe 'symbolp
  :group 'mu4e-compose)


(defcustom mu4e-compose-keep-self-cc nil
  "Non-nil means your e-mail address is kept on the CC list when
replying to messages."
  :type 'boolean
  :group 'mu4e-compose)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e-compose-attach-captured-message()
  "Insert the last captured message file as an attachment."
    (interactive)
  (unless mu4e-captured-message
    (error "No message has been captured"))
  (let ((path (plist-get mu4e-captured-message :path)))
    (unless (file-exists-p path)
      (error "Captured message file not found"))
    (mml-attach-file
      path
      "application/octet-stream"
      (or (plist-get mu4e-captured-message :subject) "No subject")
      "attachment"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e~compose-user-agent-construct ()
  "Return the User-Agent string for mu4e. This is either the value
of `mu4e-user-agent', or, if not set, a string based on the versions
of mu4e and emacs."
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)) 


(defun mu4e~compose-cite-original (msg)
  "Return a cited version of the original message MSG (ie., the
plist). This function use gnus' `message-cite-function', and as
such all its settings apply."
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

(defun mu4e~compose-header (hdr val)
  "Return a header line of the form HDR: VAL\n. If VAL is nil,
return nil."
  (when val (format "%s: %s\n" hdr val)))


(defun mu4e~compose-references-construct (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of the
existing References (which may be empty) and the message-id. If the
message-id is empty, returns the old References. If both are empty,
return nil."
  (let ((refs (plist-get msg :references))
	 (old-msgid (plist-get msg :message-id)))
    (when old-msgid
      (setq refs (append refs (list old-msgid)))
      (mapconcat
	(lambda (msgid) (format "<%s>" msgid))
	refs ","))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine the recipient fields for new messages

(defun mu4e~compose-recipients-list-to-string (lst)
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

(defun mu4e~compose-address-cell-equal (cell1 cell2)
  "Return t if cell1 and cell2 have the same e-mail
  address (case-insensitively), nil otherwise. cell1 and cell2 are
  cons cells (NAME . EMAIL)."
  (string=
    (downcase (or (cdr cell1) ""))
    (downcase (or (cdr cell2) ""))))


(defun mu4e~compose-create-to-lst (origmsg)
  "Create a list of address for the To: in a new message, based on
the original message ORIGMSG. If the Reply-To address is set, use
that, otherwise use the From address. Note, whatever was in the To:
field before, goes to the Cc:-list (if we're doing a reply-to-all)."
  (let ((reply-to
	   (or (plist-get origmsg :reply-to) (plist-get origmsg :from))))
    (delete-duplicates reply-to  :test #'mu4e~compose-address-cell-equal)))


(defun mu4e~compose-create-cc-lst (origmsg reply-all)
  "Create a list of address for the Cc: in a new message, based on
the original message ORIGMSG, and whether it's a reply-all."
  (when reply-all
    (let* ((cc-lst ;; get the cc-field from the original, remove dups
	     (delete-duplicates
	       (append
		 (plist-get origmsg :to)
		 (plist-get origmsg :cc))
	       :test #'mu4e~compose-address-cell-equal))
	    ;; now we have the basic list, but we must remove
	    ;; addresses also in the to list
	    (cc-lst
	      (delete-if
		(lambda (cc-cell)
		  (find-if
		    (lambda (to-cell)
		      (mu4e~compose-address-cell-equal cc-cell to-cell))
		    (mu4e~compose-create-to-lst origmsg)))
		cc-lst))
	    ;; finally, we need to remove ourselves from the cc-list
	    ;; unless mu4e-compose-keep-self-cc is non-nil
	    (cc-lst
	      (if (or mu4e-compose-keep-self-cc (null user-mail-address))
		cc-lst
		(delete-if
		  (lambda (cc-cell)
		    (mu4e~compose-address-cell-equal cc-cell
		      (cons nil user-mail-address)))
		  cc-lst))))
      cc-lst)))

(defun mu4e~compose-recipients-construct (field origmsg &optional reply-all)
  "Create value (a string) for the recipient field FIELD (a
symbol, :to or :cc), based on the original message ORIGMSG,
and (optionally) REPLY-ALL which indicates this is a reply-to-all
message. Return nil if there are no recipients for the particular field."
  (mu4e~compose-recipients-list-to-string
    (case field
      (:to
	(mu4e~compose-create-to-lst origmsg))
      (:cc
	(mu4e~compose-create-cc-lst origmsg reply-all))
      (otherwise
	(error "Unsupported field")))))


(defun mu4e~compose-from-construct ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~compose-insert-mail-header-separator ()
  "Insert `mail-header-separator' in the first empty line of the
message. message-mode needs this line to know where the headers end
and the body starts. Note, in `mu4e-compose-mode, we use
`before-save-hook' and `after-save-hook' to ensure that this
separator is never written to file. Also see
`mu4e-remove-mail-header-separator'."
  (save-excursion
    (let ((sepa (propertize mail-header-separator
		  'intangible t 'read-only t
		  'font-lock-face 'mu4e-system-face)))
      (goto-char (point-min))
      ;; search for the first empty line
      (if (search-forward-regexp "^$" nil t)
	(replace-match sepa)
	(progn 	;; no empty line? then prepend one
	  (goto-char (point-max))
	  (insert (concat "\n" sepa)))))))

(defun mu4e~compose-remove-mail-header-separator ()
  "Remove `mail-header-separator; we do this before saving a
file (and restore it afterwardds), to ensure that the separator
never hits the disk. Also see `mu4e~compose-insert-mail-header-separator."
  (save-excursion
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (when (search-forward-regexp (concat "^" mail-header-separator))
      (let ((inhibit-read-only t))
	(replace-match "")))))

(defun mu4e~compose-user-wants-reply-all (origmsg)
  "Ask user whether she wants to reply to *all* recipients if there
are more than 1 (based on ORIGMSG)."
  (let* ((recipnum
	   (+ (length (mu4e~compose-create-to-lst origmsg))
	     (length (mu4e~compose-create-cc-lst origmsg t))))
	  (response
	    (if (= recipnum 1)
	      'all ;; with one recipient, we can reply to 'all'....
	      (mu4e-read-option
		"Reply to "
		`( (,(format "all %d recipients" recipnum) . all)
		   ("sender only" . sender-only))))))
    (eq response 'all)))

(defun mu4e~compose-message-filename-construct (&optional flagstr)
  "Construct a randomized name for a message file with flags FLAGSTR; it looks
something like
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

(defun mu4e~compose-common-construct ()
  "Construct the common headers for each message."
  (mu4e~compose-header "User-agent" (mu4e~compose-user-agent-construct)))

(defconst mu4e~compose-reply-prefix "Re: "
  "String to prefix replies with.")

(defun mu4e~compose-reply-construct (origmsg)
  "Create a draft message as a reply to original message ORIGMSG."
  (let* ((recipnum
	   (+ (length (mu4e~compose-create-to-lst origmsg))
	     (length (mu4e~compose-create-cc-lst origmsg t))))
	  (reply-all (mu4e~compose-user-wants-reply-all origmsg))
	  (old-msgid (plist-get origmsg :message-id))
	  (subject
	    (concat mu4e~compose-reply-prefix
	      (message-strip-subject-re (or (plist-get origmsg :subject) "")))))
    (message "S:%s" subject)
    (concat
      (mu4e~compose-header "From" (or (mu4e~compose-from-construct) ""))
      (mu4e~compose-header "Reply-To" mu4e-reply-to-address)
      (mu4e~compose-header "To" (mu4e~compose-recipients-construct :to origmsg))
      (mu4e~compose-header "Cc" (mu4e~compose-recipients-construct :cc origmsg
				  reply-all))
      (mu4e~compose-header "Subject" subject)
      (mu4e~compose-header "References"
	(mu4e~compose-references-construct origmsg))
      (mu4e~compose-common-construct)
      (when old-msgid
	(mu4e~compose-header "In-reply-to" (format "<%s>" old-msgid)))
      "\n\n"
      (mu4e~compose-cite-original origmsg))))

(defconst mu4e~compose-forward-prefix "Fwd: "
  "String to prefix replies with.")

(defun mu4e~compose-forward-construct (origmsg)
  "Create a draft forward message for original message ORIGMSG."
  (let ((subject
	  (or (plist-get origmsg :subject) "")))
    (concat
      (mu4e~compose-header "From" (or (mu4e~compose-from-construct) ""))
      (mu4e~compose-header "Reply-To" mu4e-reply-to-address)
      (mu4e~compose-header "To" "")
      (mu4e~compose-common-construct)
      (mu4e~compose-header "References"
	(mu4e~compose-references-construct origmsg))
      (mu4e~compose-header "Subject"
	(concat
	  ;; if there's no Fwd: yet, prepend it
	  (if (string-match "^Fwd:" subject)
	    ""
	    mu4e~compose-forward-prefix)
	  subject))
      "\n\n"
      (mu4e~compose-cite-original origmsg))))


(defun mu4e~compose-newmsg-construct ()
  "Create a new message."
  (concat
    (mu4e~compose-header "From" (or (mu4e~compose-from-construct) ""))
    (mu4e~compose-header "Reply-To" mu4e-reply-to-address)
    (mu4e~compose-header "To" "")
    (mu4e~compose-header "Subject" "")
    (mu4e~compose-common-construct)))


(defun mu4e~compose-open-new-draft-file (compose-type &optional msg)
  "Open a draft file for a new message, creating it if it does not
already exist, and optionally fill it with STR. Function also adds
the new message to the database. When the draft message is added to
the database, `mu4e-path-docid-map' will be updated, so that we can
use the new docid. Returns the full path to the new message."
  (let* ((draft
	   (concat mu4e-maildir mu4e-drafts-folder "/cur/"
	     (mu4e~compose-message-filename-construct "DS")))
	  (str (case compose-type
		 (reply   (mu4e~compose-reply-construct msg))
		 (forward (mu4e~compose-forward-construct msg))
		 (new     (mu4e~compose-newmsg-construct))
		 (t (error "unsupported compose-type %S" compose-type)))))
    (when str
      (with-current-buffer (find-file-noselect draft)
	(insert str)))
    draft))  ;; return the draft buffer file


;; 'fcc' refers to saving a copy of a sent message to a certain folder. that's
;; what these 'Sent mail' folders are for!
;;
;; We let message mode take care of this by adding a field
;;   Fcc: <full-path-to-message-in-target-folder>
;; in the "message-send-hook" (ie., just before sending).
;; message mode will then take care of the saving when the message is actually
;; sent.
;;
;; note, where and if you make this copy depends on the value of
;; `mu4e-sent-messages-behavior'.

(defun mu4e~setup-fcc-maybe ()
  "Maybe setup Fcc, based on `mu4e-sent-messages-behavior'. If
needed, set the Fcc header, and register the handler function."
  (let* ((mdir
	   (case mu4e-sent-messages-behavior
	     (delete nil)
	     (trash mu4e-trash-folder)
	     (sent mu4e-sent-folder)
	     (otherwise
	       (error "unsupported value '%S' `mu4e-sent-messages-behavior'."
		 mu4e-sent-messages-behavior))))
	  (fccfile (and mdir
		     (concat mu4e-maildir mdir "/cur/"
		       (mu4e~compose-message-filename-construct "S")))))
    ;; if there's an fcc header, add it to the file
    (when fccfile
      (message-add-header (concat "Fcc: " fccfile "\n"))
      ;; sadly, we cannot define as 'buffer-local'...  this will screw up gnus
      ;; etc. if you run it after mu4e so, (hack hack) we reset it to the old
      ;; hander after we've done our thing.
      (setq message-fcc-handler-function
	(lexical-let ((maildir mdir) (old-handler message-fcc-handler-function))
	  (lambda (file)
	    (setq message-fcc-handler-function old-handler) ;; reset the fcc handler
	    (write-file file) ;; writing maildirs files is easy
	    (mu4e~proc-add file maildir))))))) ;; update the database


(defun mu4e~compose-register-message-save-hooks ()
  "Just before saving, we remove the mail-header-separator; just
  after saving we restore it; thus, the separator should never
  appear on disk."
  (add-hook 'before-save-hook
    'mu4e~compose-remove-mail-header-separator nil t)
  (add-hook 'after-save-hook
    (lambda ()
      (mu4e~compose-set-friendly-buffer-name)
      (mu4e~compose-insert-mail-header-separator)
      (set-buffer-modified-p nil)
      ;; update the file on disk -- ie., without the separator
      (mu4e~proc-add (buffer-file-name) mu4e-drafts-folder)) nil t))

(defconst mu4e~compose-hidden-headers
  `("^References:" "^Face:" "^X-Face:"
     "^X-Draft-From:" "^User-agent:")
  "Hidden headers when composing.")

(defconst mu4e~compose-address-fields-regexp
  "^\\(To\\|B?Cc\\|Reply-To\\|From\\):")

(defun mu4e~compose-find-completion-style (some-style)
  "Find completion style SOME-STYLE in completion-styles-alist, or return nil."
  (find-if (lambda (style) (eq some-style (car style))) completion-styles-alist))

(defconst mu4e~completion-cycle-treshold 5
  "mu4e value for `completion-cycle-treshold'.")

(defun mu4e~compose-setup-completion ()
  "Set up autocompletion of addresses."
  (let ((compstyle
	  (or (mu4e~compose-find-completion-style 'substring)
	    (mu4e~compose-find-completion-style 'partial-completion))))
    ;; emacs-24+ has 'substring, otherwise we try partial-completion, otherwise
    ;; we leave it at the default
    (when compstyle
      (make-local-variable 'completion-styles)
      (add-to-list 'completion-styles (car compstyle)))
    (when (boundp 'completion-cycle-threshold)
      (make-local-variable 'completion-cycle-threshold)
      (setq completion-cycle-threshold mu4e~completion-cycle-treshold))
    (add-to-list 'completion-at-point-functions 'mu4e~compose-complete-contact)
    ;; needed for emacs 23...
    (when (= emacs-major-version 23)
      (make-local-variable 'message-completion-alist)
      (setq message-completion-alist
	(cons
	  (cons mu4e~compose-address-fields-regexp 'completion-at-point)
	  message-completion-alist)))))
  
(define-derived-mode mu4e-compose-mode message-mode "mu4e:compose"
  "Major mode for the mu4e message composition, derived from `message-mode'.
\\{message-mode-map}."
  (let ((message-hidden-headers mu4e~compose-hidden-headers))
    (use-local-map mu4e-compose-mode-map)
    (make-local-variable 'message-default-charset)
    ;; if the default charset is not set, use UTF-8
    (unless message-default-charset
      (setq message-default-charset 'utf-8))
    ;; make sure mu4e is started in the background (ie. we don't want to error
    ;; out when sending the message; better to do it now if there's a problem)
    (mu4e~start) ;; start mu4e in background, if needed
    (mu4e~compose-register-message-save-hooks)
    ;; set the default directory to the user's home dir; this is probably more
    ;; useful e.g. when finding an attachment file the directory the current
    ;; mail files lives in...
    (setq default-directory (expand-file-name "~/"))
     
    ;; offer completion for e-mail addresses
    (when (and mu4e-compose-complete-addresses
	    (boundp 'completion-at-point-functions))
      (mu4e~compose-setup-completion))
      
    ;; setup the fcc-stuff, if needed
    (add-hook 'message-send-hook
      (lambda ()
	;; for safety, always save the draft before sending
	(set-buffer-modified-p t)
	(save-buffer)
	(mu4e~setup-fcc-maybe)) nil t)
    ;; when the message has been sent.
    (add-hook 'message-sent-hook
      (lambda ()
	(setq mu4e-sent-func 'mu4e-sent-handler)
	(mu4e~proc-sent (buffer-file-name) mu4e-drafts-folder)) nil)))

(defconst mu4e~compose-buffer-max-name-length 30
  "Maximum length of the mu4e-send-buffer-name.")

(defun mu4e~compose-set-friendly-buffer-name (&optional compose-type)
  "Set some user-friendly buffer name based on the compose type."
  (let* ((subj (message-field-value "subject"))
	  (subj (unless (and subj (string-match "^[:blank:]*$" subj)) subj))
	  (str (or subj
		 (case compose-type
		   (reply       "*reply*")
		   (forward     "*forward*")
		   (otherwise   "*draft*")))))
    (rename-buffer (generate-new-buffer-name
		     (truncate-string-to-width str
		       mu4e~compose-buffer-max-name-length
		       nil nil t)))))

(defconst mu4e~compose-hidden-headers
  '("^References:" "^Face:" "^X-Face:" "^X-Draft-From:"
     "^User-Agent:" "^In-Reply-To:")
  "List of regexps with message headers that are to be hidden.")

(defun mu4e~compose-handler (compose-type &optional original-msg includes)
  "Create a new draft message, or open an existing one.

COMPOSE-TYPE determines the kind of message to compose and is a
symbol, either `reply', `forward', `edit', `new'. `edit' is for
editing existing messages.

When COMPOSE-TYPE is `reply' or `forward', MSG should be a message
plist.  If COMPOSE-TYPE is `new', ORIGINAL-MSG should be nil.

Optionally (when forwarding, replying) ORIGINAL-MSG is the original
message we will forward / reply to.

Optionally (when forwarding) INCLUDES contains a list of
   (:file-name <filename> :mime-type <mime-type> :disposition <disposition>)
for the attachements to include; file-name refers to
a file which our backend has conveniently saved for us (as a
tempfile).

The name of the draft folder is constructed from the concatenation
 of `mu4e-maildir' and `mu4e-drafts-folder' (therefore, these must be
 set).

The message file name is a unique name determined by
`mu4e-send-draft-file-name'.

The initial STR would be created from either
`mu4e~compose-reply-construct', ar`mu4e~compose-forward-construct'
or `mu4e~compose-newmsg-construct'. The editing buffer is using
Gnus' `message-mode'."
  (unless mu4e-maildir       (error "mu4e-maildir not set"))
  (unless mu4e-drafts-folder (error "mu4e-drafts-folder not set"))
  (let ((inhibit-read-only t)
	 (draft
	   (if (member compose-type '(reply forward new))
	     (mu4e~compose-open-new-draft-file compose-type original-msg)
	     (if (eq compose-type 'edit)
	       (plist-get original-msg :path)
	       (error "unsupported compose-type %S" compose-type)))))
    (find-file draft)
    ;; insert mail-header-separator, which is needed by message mode to separate
    ;; headers and body. will be removed before saving to disk
    (mu4e~compose-insert-mail-header-separator)
    (insert "\n") ;; insert a newline after header separator

    ;; include files -- e.g. when forwarding a message with attachments,
    ;; we take those from the original.
    (save-excursion
      (goto-char (point-max)) ;; put attachments at the end
      (dolist (att includes)
	(mml-attach-file
	  (plist-get att :file-name) (plist-get att :mime-type))))

    ;; include the message header (if it's set); but not when editing an
    ;; existing message.
    (unless (eq compose-type 'edit)
      (message-insert-signature))

    ;; hide some headers
    (let ((message-hidden-headers mu4e~compose-hidden-headers))
      (message-hide-headers))

    ;; set compose mode -- so now hooks can run
    (mu4e-compose-mode)

    ;; buffer is not user-modified yet
    (mu4e~compose-set-friendly-buffer-name compose-type)
    (set-buffer-modified-p nil)

    ;; now jump to some use positions, and start writing that mail!
    (if (member compose-type '(new forward))
      (message-goto-to)
      (message-goto-body))))

(defun mu4e-sent-handler (docid path)
  "Handler function, called with DOCID and PATH for the just-sent
message. For Forwarded ('Passed') and Replied messages, try to set
the appropriate flag at the message forwarded or replied-to."
  (mu4e~compose-set-parent-flag path)
  (when (file-exists-p path) ;; maybe the draft was not saved at all
    (mu4e~proc-remove docid))
  ;; kill any remaining buffers for the draft file, or they will hang around...
  ;; this seems a bit hamfisted...
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
	    (string= (buffer-file-name buf) path))
      (kill-buffer buf)))
  (mu4e-message "Message sent"))

(defun mu4e~compose-set-parent-flag (path)
  "Set the 'replied' \"R\" flag on messages we replied to, and the
'passed' \"F\" flag on message we have forwarded.

If a message has an 'in-reply-to' header, it is considered a reply
to the message with the corresponding message id. If it does not
have an 'in-reply-to' header, but does have a 'references' header,
it is considered to be a forward message for the message
corresponding with the /last/ message-id in the references header.

Now, if the message has been determined to be either a forwarded
message or a reply, we instruct the server to update that message
with resp. the 'P' (passed) flag for a forwarded message, or the
'R' flag for a replied message.

Function assumes that it's executed in the context of the message
buffer."
  (let ((buf (find-file-noselect path)))
    (when buf
      (with-current-buffer buf
	(let ((in-reply-to (message-fetch-field "in-reply-to"))
	       (forwarded-from)
	       (references (message-fetch-field "references")))
	  (unless in-reply-to
	    (when references
	      (with-temp-buffer ;; inspired by `message-shorten-references'.
		(insert references)
		(goto-char (point-min))
		(let ((refs))
		  (while (re-search-forward "<[^ <]+@[^ <]+>" nil t)
		    (push (match-string 0) refs))
		  ;; the last will be the first
		  (setq forwarded-from (first refs))))))
	  ;; remove the <>
	  (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
	    (mu4e~proc-move (match-string 1 in-reply-to) nil "+R"))
	  (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
	    (mu4e~proc-move (match-string 1 forwarded-from) nil "+P")))))))



(defun mu4e-compose (compose-type)
  "Start composing a message of COMPOSE-TYPE, where COMPOSE-TYPE is
a symbol, one of `reply', `forward', `edit', `new'. All but `new'
take the message at point as input. Symbol `edit' is only allowed
for draft messages."
  (unless (member compose-type '(reply forward edit new))
    (error "Invalid compose type '%S'" compose-type))
  ;; 'new is special, since it takes no existing message as arg therefore,
  ;; we don't need to call thec backend, and call the handler *directly*
  (if (eq compose-type 'new)
    (mu4e~compose-handler 'new)
    ;; otherwise, we need the doc-id
    (let ((docid (mu4e-field-at-point :docid)))
      ;; note, the first two chars of the line (the mark margin) does *not*
      ;; have the 'draft property; thus, we check one char before the end of
      ;; the current line instead
      (unless (or (not (eq compose-type 'edit))
		(member 'draft (mu4e-field-at-point :flags)))
	  (error "Editing is only allowed for draft messages"))
      ;; if there's a visible view window, select that before starting
      ;; composing a new message, so that one will be replaced by the
      ;; compose window. The 10-or-so line headers buffer is not a good way
      ;; to write it...
	(let ((viewwin (get-buffer-window mu4e~view-buffer)))
	  (when (window-live-p viewwin)
	    (select-window viewwin)))
	;; talk to the backend
      (mu4e~proc-compose compose-type docid))))

(defun mu4e-compose-reply ()
  "Compose a reply for the message at point in the headers buffer."
  (interactive)
  (mu4e-compose 'reply))

(defun mu4e-compose-forward ()
  "Forward the message at point in the headers buffer."
  (interactive)
  (mu4e-compose 'forward))

(defun mu4e-compose-edit ()
  "Edit the draft message at point in the headers buffer. This is
only possible if the message at point is, in fact, a draft
message."
  (interactive)
  (mu4e-compose 'edit))

(defun mu4e-compose-new ()
  "Start writing a new message."
  (interactive)
  (mu4e-compose 'new))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; address completion; inspired by org-contacts.el
(defun mu4e~compose-complete-contact (&optional start)
  "Complete the text at START with a contact (ie. either 'name
<email>' or 'email')."
  (interactive)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
	 (eoh ;; end-of-headers
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward-regexp mail-header-separator nil t))))
	 ;; try to complete only when we're in the headers area,
	 ;; looking  at an address field.
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
	      (start
		(or start
		  (save-excursion
		    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		    (goto-char (match-end 0))
		    (point))))
	      (orig (buffer-substring-no-properties start end))
	      (completion-ignore-case t))
	  (list start end mu4e~contacts-for-completion)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e-compose-func and mu4e-send-func are wrappers so we can set ourselves
;; as default emacs mailer (define-mail-user-agent etc.)

(defun mu4e~compose-mail (&optional to subject other-headers continue
			   switch-function yank-action send-actions return-action)
  "mu4e's implementation of `compose-mail'."
  ;; create a new draft message 'resetting' (as below) is not actually needed in
  ;; this case, but let's prepare for the re-edit case as well
  (mu4e~compose-handler 'new)

  (when (message-goto-to) ;; reset to-address, if needed
    (message-delete-line))
  (message-add-header (concat "To: " to "\n"))

  (when (message-goto-subject) ;; reset subject, if needed
    (message-delete-line))
  (message-add-header (concat "Subject: " subject "\n"))

  ;; add any other headers specified
  (when other-headers
    (message-add-header other-headers))

  ;; yank message
  (if (bufferp yank-action)
    (list 'insert-buffer yank-action)
    yank-action)

  ;; try to put the user at some reasonable spot...
  (if (not to)
    (message-goto-to)
    (if (not subject)
      (message-goto-subject)
      (message-goto-body))))

;; happily, we can re-use most things from message mode
(define-mail-user-agent 'mu4e-user-agent
  'mu4e~compose-mail
  'message-send-and-exit
  'message-kill-buffer
  'message-send-hook)

(defun mu4e~compose-browse-url-mail (url &optional ignored)
  "Adapter for `browse-url-mailto-function."
  (let* ((headers (rfc2368-parse-mailto-url url))
	  (to (cdr (assoc "To" headers)))
	  (subject (cdr (assoc "Subject" headers)))
	  (body (cdr (assoc "Body" headers))))
    (mu4e~compose-mail to subject)
    (if body
      (progn
	(message-goto-body)
	(insert body)
	(if (not to)
	  (message-goto-to)
	  (if (not subject)
	    (message-goto-subject)
	    (message-goto-body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-compose)
