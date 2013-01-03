;;; mu4e-utils.el -- part of mu4e, the mu mail user agent
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

;; Utility functions used in the mu4e

;;; Code:
(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'html2text)
(require 'mu4e-vars)
(require 'mu4e-about)
(require 'doc-view)
(require 'org) ;; for org-parse-time-string

(defcustom mu4e-html2text-command nil
  "Shell command that converts HTML from stdin into plain text on
stdout. If this is not defined, the emacs `html2text' tool will be
used when faced with html-only message. If you use htmltext, it's
recommended you use \"html2text -utf8 -width 72\"."
  :type 'string
  :group 'mu4e-view
  :safe 'stringp)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the HTML-version of the
e-mail message (if there is any."
  :type 'boolean
  :group 'mu4e-view)


(defun mu4e-create-maildir-maybe (dir)
  "Offer to create DIR if it does not exist yet. Return t if the
dir already existed, or has been created, nil otherwise."
  (if (and (file-exists-p dir) (not (file-directory-p dir)))
    (error "%s exists, but is not a directory." dir))
  (cond
    ((file-directory-p dir) t)
    ((yes-or-no-p (mu4e-format "%s does not exist yes. Create now?" dir))
      (mu4e~proc-mkdir dir))
    (t nil)))

(defun mu4e-format (frm &rest args)
  "Create [mu4e]-prefixed string based on format FRM and ARGS."
  (concat "[" mu4e-logo "] "  (apply 'format frm args)))

(defun mu4e-message (frm &rest args)
  "Like `message', but prefixed with mu4e. If we're waiting for
user-input, don't show anyhting."
  (unless (waiting-for-user-input-p)
    (message "%s" (apply 'mu4e-format frm args))))

(defun mu4e~read-char-choice (prompt choices)
  "Compatiblity wrapper for `read-char-choice', which is emacs-24
only."
  (let ((choice) (ok) (inhibit-quit nil))
    (while (not ok)
      (message nil);; this seems needed...
      (setq choice (read-char-exclusive prompt))
      (setq ok (member choice choices)))
    choice))

(defun mu4e-read-option (prompt options)
  "Ask user for an option from a list on the input area. PROMPT
describes a multiple-choice question to the user, OPTIONS describe
the options, and is a list of cells describing particular
options. Cells have the following structure:

   (OPTIONSTRING . RESULT)

 where OPTIONSTRING is a non-empty string describing the
 option. The first character of OPTIONSTRING is used as the
 shortcut, and obviously all shortcuts must be different, so you
 can prefix the string with an uniquifying character.

The options are provided as a list for the user to choose from;
user can then choose by typing CHAR.  Example:
  (mu4e-read-option \"Choose an animal: \"
              '((\"Monkey\" . monkey) (\"Gnu\" . gnu) (\"xMoose\" . moose)))

User now will be presented with a list: \"Choose an animal:
   [M]onkey, [G]nu, [x]Moose\".

Function will return the cdr of the list element."
  (let* ((prompt (mu4e-format "%s" prompt))
	  (chosen)
	  (optionsstr
	    (mapconcat
	      (lambda (option)
		;; try to detect old-style options, and warn
		(when (characterp (car-safe (cdr-safe option)))
		  (error (concat "Please use the new format for options/actions; "
			   "see the manual")))
		(let* ((kar (substring (car option) 0 1))
			(val (cdr option)))
		  (concat
		    "[" (propertize kar 'face 'mu4e-highlight-face) "]"
		    (substring (car option) 1))))
	      options ", "))
	  (response
	    (mu4e~read-char-choice
	      (concat prompt optionsstr
		" [" (propertize "C-g" 'face 'mu4e-highlight-face) " to quit]")
	      ;; the allowable chars
	      (map 'list (lambda(elm) (string-to-char (car elm))) options)))
	  (chosen
	    (find-if
	      (lambda (option) (eq response (string-to-char (car option))))
	      options)))
    (unless chosen (error "%S not found" response))
    (cdr chosen)))


(defun mu4e~get-maildirs-1 (path &optional mdir)
  "Get maildirs under path, recursively, as a list of relative
paths."
  ;; first, we get a list of all directory paths under PATH that have a
  ;; directory 'cur' as leaf; then we we remove from that list all of those that
  ;; don't have tmp, new sister dirs. And there we're done!
  ;; 1. get all proper subdirs of the current dir
  (let* ((subdirs
	   (remove-if
	     (lambda (de)
	       (or (not (file-directory-p (concat path mdir "/" de)))
		 (string-match "\\.\\{1,2\\}$" de)))
	     (directory-files (concat path mdir))))
	  ;; 2. get the list of dirs with a /cur leaf dir
	  (maildirs '()))
    (dolist (dir subdirs)
      (when (string= dir "cur")
	;; be pedantic, and insist on there being a new/tmp as well
	(when (and (file-directory-p (concat path mdir "/new" ))
		(file-directory-p (concat path mdir "/tmp")))
	  (add-to-list 'maildirs (if mdir mdir "/") t)))
      (setq maildirs (append maildirs
		       (mu4e~get-maildirs-1 path (concat mdir "/" dir)))))
    maildirs))

(defvar mu4e~maildir-list nil "Cached list of maildirs.")

(defun mu4e-get-maildirs (path)
  "Get maildirs under path, recursively, as a list of relative
paths (ie., /archive, /sent etc.). Most of the work is done in
`mu4e-get-maildirs-1'. Note, these results are /cached/, so the
list of maildirs will not change until you restart mu4e."
  (unless mu4e~maildir-list
    (setq mu4e~maildir-list
      (sort (mu4e~get-maildirs-1 path)
	(lambda (m1 m2)
	  (when (string= m1 "/")
	    -1 ;; '/' comes first
	    (compare-strings m1 0 nil m2 0 nil t))))))
  mu4e~maildir-list)


(defun mu4e-ask-maildir (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts is not defined, let user choose from all
maildirs under `mu4e-maildir."
  (unless mu4e-maildir (error "`mu4e-maildir' is not defined"))
  (let ((prompt (mu4e-format "%s" prompt)))
    (if (not mu4e-maildir-shortcuts)
      (ido-completing-read prompt
	(mu4e-get-maildirs mu4e-maildir))
      (let* ((mlist (append mu4e-maildir-shortcuts '(("ther" . ?o))))
	      (fnames
		(mapconcat
		  (lambda (item)
		    (concat
		      "["
		      (propertize (make-string 1 (cdr item))
			'face 'mu4e-highlight-face)
		      "]"
		      (car item)))
		  mlist ", "))
	      (kar (read-char (concat prompt fnames))))
	(if (= kar ?o) ;; user chose 'other'?
	  (ido-completing-read prompt (mu4e-get-maildirs mu4e-maildir))
	  (or (car-safe
		(find-if (lambda (item) (= kar (cdr item))) mu4e-maildir-shortcuts))
	    (error "Invalid shortcut '%c'" kar)))))))


(defun mu4e-ask-maildir-check-exists (prompt)
  "Like `mu4e-ask-maildir', but check for existence of the maildir,
and offer to create it if it does not exist yet."
  (let* ((mdir (mu4e-ask-maildir prompt))
	  (fullpath (concat mu4e-maildir mdir)))
    (unless (file-directory-p fullpath)
      (and (yes-or-no-p
	     (mu4e-format "%s does not exist. Create now?" fullpath))
	      (mu4e~proc-mkdir fullpath)))
    mdir))


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
		   (mu4e-format "%s does not exist. Create now?" fulltarget))
	      (mu4e~proc-mkdir fulltarget)))
      (mu4e-mark-set 'move target))))


(defun mu4e-ask-bookmark (prompt &optional kar)
  "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query."
  (unless mu4e-bookmarks (error "`mu4e-bookmarks' is not defined"))
  (let* ((prompt (mu4e-format "%s" prompt))
	  (bmarks
	   (mapconcat
	     (lambda (bm)
	       (let ((query (nth 0 bm)) (title (nth 1 bm)) (key (nth 2 bm)))
		 (concat
		   "[" (propertize (make-string 1 key)
			 'face 'mu4e-highlight-face)
		   "]"
		   title))) mu4e-bookmarks ", "))
	  (kar (read-char (concat prompt bmarks))))
    (mu4e-get-bookmark-query kar)))

(defun mu4e-get-bookmark-query (kar)
  "Get the corresponding bookmarked query for shortcut character
KAR, or raise an error if none is found."
 (let ((chosen-bm
	 (find-if
	   (lambda (bm)
	     (= kar (nth 2 bm)))
	   mu4e-bookmarks)))
   (if chosen-bm
     (nth 0 chosen-bm)
     (error "Invalid shortcut '%c'" kar))))


;;; converting flags->string and vice-versa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~flags-to-string-raw (flags)
  "Convert a list of flags into a string as seen in Maildir
message files; flags are symbols draft, flagged, new, passed,
replied, seen, trashed and the string is the concatenation of the
uppercased first letters of these flags, as per [1]. Other flags
than the ones listed here are ignored.
Also see `mu4e-flags-to-string'.
\[1\]: http://cr.yp.to/proto/maildir.html"
  (when flags
    (let ((kar (case (car flags)
		 ('draft     ?D)
		 ('flagged   ?F)
		 ('new       ?N)
		 ('passed    ?P)
		 ('replied   ?R)
		 ('seen      ?S)
		 ('trashed   ?T)
		 ('attach    ?a)
		 ('encrypted ?x)
		 ('signed    ?s)
		 ('unread    ?u))))
      (concat (and kar (string kar))
	(mu4e~flags-to-string-raw (cdr flags))))))

(defun mu4e-flags-to-string (flags)
  "Remove duplicates and sort the output of `mu4e~flags-to-string-raw'."
  (concat
    (sort (remove-duplicates
	    (append (mu4e~flags-to-string-raw flags) nil)) '>)))


(defun mu4e~string-to-flags-1 (str)
  "Convert a string with message flags as seen in Maildir
messages into a list of flags in; flags are symbols draft,
flagged, new, passed, replied, seen, trashed and the string is
the concatenation of the uppercased first letters of these flags,
as per [1]. Other letters than the ones listed here are ignored.
Also see `mu4e-flags-to-string'.
\[1\]: http://cr.yp.to/proto/maildir.html."
  (when (/= 0 (length str))
    (let ((flag
	    (case (string-to-char str)
	      (?D   'draft)
	      (?F   'flagged)
	      (?P   'passed)
	      (?R   'replied)
	      (?S   'seen)
	      (?T   'trashed))))
      (append (when flag (list flag))
	(mu4e~string-to-flags-1 (substring str 1))))))

(defun mu4e-string-to-flags (str)
" Convert a string with message flags as seen in Maildir messages
into a list of flags in; flags are symbols draft, flagged, new,
passed, replied, seen, trashed and the string is the concatenation
of the uppercased first letters of these flags, as per [1]. Other
letters than the ones listed here are ignored.  Also see
`mu4e-flags-to-string'.  \[1\]:
http://cr.yp.to/proto/maildir.html "
  ;;  "Remove duplicates from the output of `mu4e~string-to-flags-1'"
  (remove-duplicates (mu4e~string-to-flags-1 str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e-display-size (size)
  "Get a string representation of SIZE (in bytes)."
  (cond
    ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
    ((and (>= size 1000) (< size 1000000))
      (format "%2.1fK" (/ size 1000.0)))
    ((< size 1000) (format "%d" size))
    (t (propertize "?" 'face 'mu4e-system-face))))



(defun mu4e-body-text (msg)
  "Get the body in text form for this message, which is either :body-txt,
or if not available, :body-html converted to text. By default, it
uses the emacs built-in `html2text'. Alternatively, if
`mu4e-html2text-command' is non-nil, it will use that. Normally,
function prefers the text part, but this can be changed by setting
`mu4e-view-prefer-html'."
  (let* ((txt (plist-get msg :body-txt))
	  (html (plist-get msg :body-html))
	  (body
	    (cond
	      ;; does it look like some text? ie., 20x the length of the text
	      ;; should be longer than the html, an heuristic to guard against
	      ;; 'This messages requires html' text bodies.
	      ((and (> (* 20 (length txt)) (length html))
		 ;; use html if it's prefered, unless there is no html
		 (or (not mu4e-view-prefer-html) (not html)))
		txt)
	      ;; otherwise, it there some html?
	      (html
		(with-temp-buffer
		  (insert html)
		  ;; if defined, use the external tool
		  (if mu4e-html2text-command
		    (shell-command-on-region (point-min) (point-max)
		      mu4e-html2text-command nil t)
		    ;; otherwise...
		    (html2text))
		  (buffer-string)))
	      (t ;; otherwise, an empty body
		""))))
    ;; and finally, remove some crap from the remaining string; it seems
    ;; esp. outlook lies about its encoding (ie., it says 'iso-8859-1' but
    ;; really it's 'windows-1252'), thus giving us these funky chars. here, we
    ;; either remove them, or replace with 'what-was-meant' (heuristically)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward "[ ]" nil t)
	(replace-match
	  (cond
	    ((string= (match-string 0) "") "'")
	    (t		                       ""))))
      (buffer-string))))



(defun mu4e-display-manual ()
  "Display the mu4e manual page for the current mode, or go to the
top level if there is none."
  (interactive)
  (info (case major-mode
	  ('mu4e-main-mode "(mu4e)Main view")
	  ('mu4e-headers-mode "(mu4e)Headers view")
	  ('mu4e-view-mode "(mu4e)Message view")
	  (t               "mu4e"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e-msg-field (msg field)
  "Retrieve FIELD from message plist MSG. FIELD is one
of :from, :to, :cc, :bcc, :subject, :data, :message-id, :path, :maildir,
:priority, :attachments, :references, :in-reply-to, :body-txt, :body-html

A message plist looks something like:
\(:docid 32461
 :from ((\"Nikola Tesla\" . \"niko@example.com\"))
 :to ((\"Thomas Edison\" . \"tom@example.com\"))
 :cc ((\"Rupert The Monkey\" . \"rupert@example.com\"))
 :subject \"RE: what about the 50K?\"
 :date (20369 17624 0)
 :size 4337
 :message-id \"6BDC23465F79238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com\"
 :path  \"/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S\"
 :maildir \"/INBOX\"
 :priority normal
 :flags (seen)
 :attachments
     ((:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)
      (:index 3 :name \"book.pdf\" :mime-type \"application/pdf\" :size 192220))
 :references  (\"6BDC23465F79238C8384574032D81EE81AF0114E4E74@123213.mail.example.com\"
 \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\")
 :in-reply-to \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\"
 :body-txt \"Hi Tom, ...\"
\)).
Some  notes on the format:
- The address fields are lists of pairs (NAME . EMAIL), where NAME can be nil.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments. :body-txt or :body-html fields. Message in the
  Message view use the actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (plist-get msg field))

(defun mu4e-message-at-point (&optional raise-err)
  "Get the message s-expression for the message at point in either
the headers buffer or the view buffer, or nil if there is no such
message. If optional RAISE-ERR is non-nil, raise an error when
there is no message at point."
  (let ((msg
	 (cond
	   ((eq major-mode 'mu4e-headers-mode)
	     (get-text-property (point) 'msg))
	   ((eq major-mode 'mu4e-view-mode)
	     mu4e~view-msg))))
    (if (and (null msg) raise-err)
      (error "No message at point")
      msg)))

(defun mu4e-field-at-point (field)
  "Get FIELD (a symbol, see `mu4e-header-names') for the message at
point in eiter the headers buffer or the view buffer."
  (plist-get (mu4e-message-at-point t) field))

(defun mu4e-last-query ()
  "Get the most recent query or nil if there is none."
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer mu4e~headers-buffer
      mu4e~headers-last-query)))

(defun mu4e-select-other-view ()
  "When the headers view is selected, select the message view (if
that has a live window), and vice versa."
  (interactive)
  (let* ((other-buf
	   (cond
	     ((eq major-mode 'mu4e-headers-mode)
	       mu4e~view-buffer)
	     ((eq major-mode 'mu4e-view-mode)
	       mu4e~headers-buffer)))
	  (other-win (and other-buf (get-buffer-window other-buf))))
    (if (window-live-p other-win)
      (select-window other-win)
      (mu4e-message "No window to switch to"))))


(defconst mu4e-output-buffer-name "*mu4e-output*"
  "*internal* Name of the mu4e output buffer.")

(defun mu4e-process-file-through-pipe (path pipecmd)
  "Process file at PATH through a pipe with PIPECMD."
  (let ((buf (get-buffer-create mu4e-output-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(call-process-shell-command pipecmd path t t)
	(view-mode)))
    (switch-to-buffer buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some handler functions for server messages
;;
(defun mu4e-info-handler (info)
  "Handler function for (:info ...) sexps received from the server
process."
  (let ((type (plist-get info :info)))
    (cond
      ((eq type 'add) t) ;; do nothing
      ((eq type 'index)
	(if (eq (plist-get info :status) 'running)
	  (mu4e-message "Indexing... processed %d, updated %d"
	    (plist-get info :processed) (plist-get info :updated))
	  (mu4e-message
	    "Indexing completed; processed %d, updated %d, cleaned-up %d"
	    (plist-get info :processed) (plist-get info :updated)
	    (plist-get info :cleaned-up))))
      ((plist-get info :message)
	(mu4e-message "%s" (plist-get info :message))))))


(defun mu4e-error-handler (errcode errmsg)
  "Handler function for showing an error."
  (case errcode
    (4 (mu4e-message "No matches for this search query."))
    (t (mu4e-message "Error %d: %s" errcode errmsg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst mu4e~update-buffer-name "*mu4e-update*"
  "Name of the buffer for message retrieval/database updating.")

(defconst mu4e~update-buffer-height 8
  "Height of the mu4e message retrieval/update buffer.")

(defun mu4e-update-mail-show-window ()
  "Try to retrieve mail (using the user-provided shell command),
and update the database afterwards, and show the progress in a
split-window."
  (interactive)
  (unless mu4e-get-mail-command
    (error "`mu4e-get-mail-command' is not defined"))
  ;; delete any old update buffer
  (when (buffer-live-p mu4e~update-buffer-name)
    (with-current-buffer mu4e~update-buffer-name
      (kill-buffer-and-window)))
  ;; create a new one
  (let ((buf (get-buffer-create mu4e~update-buffer-name))
	 (win (split-window (selected-window)
	     (- (window-height (selected-window)) 8))))
    (with-selected-window win
      (switch-to-buffer buf)
      (set-window-dedicated-p win t)
      (erase-buffer)
      (insert "\n") ;; FIXME -- needed so output starts
      (mu4e-update-mail buf))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start and stopping
(defun mu4e~fill-contacts (contacts)
  "We receive a list of contacts, which each contact of the form
     (:name NAME :mail EMAIL)
and fill the list `mu4e~contacts-for-completion' with it, with
each element looking like
    name <email>
This is used by the completion function in mu4e-compose."
  (let ((lst))
    (dolist (contact contacts)
      (let ((name (plist-get contact :name))
	     (mail (plist-get contact :mail)))
	(when mail
	  (unless ;; ignore some address ('noreply' etc.)
	    (and mu4e-compose-complete-ignore-address-regexp
	      (string-match mu4e-compose-complete-ignore-address-regexp mail))
	  (add-to-list 'lst
	    (if name (format "%s <%s>" name mail) mail))))))
    (setq mu4e~contacts-for-completion lst)
    (mu4e-message "Contacts received: %d"
      (length mu4e~contacts-for-completion))))


(defun mu4e~check-requirements ()
  "Check for the settings required for running mu4e."
  (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
    (error "Please set `mu4e-mu-binary' to the full path to the mu
    binary."))
  (unless mu4e-maildir
    (error "Please set `mu4e-maildir' to the full path to your
    Maildir directory."))
  ;; expand mu4e-maildir, mu4e-attachment-dir
  (setq
    mu4e-maildir (expand-file-name mu4e-maildir)
    mu4e-attachment-dir (expand-file-name mu4e-attachment-dir))
  (unless (mu4e-create-maildir-maybe mu4e-maildir)
    (error "%s is not a valid maildir directory" mu4e-maildir))
  (dolist (var '( mu4e-sent-folder
		  mu4e-drafts-folder
		  mu4e-trash-folder))
    (unless (and (boundp var) (symbol-value var))
      (error "Please set %S" var))
    (let* ((dir (symbol-value var)) (path (concat mu4e-maildir dir)))
      (unless (string= (substring dir 0 1) "/")
	(error "%S must start with a '/'" dir))
      (unless (mu4e-create-maildir-maybe path)
	(error "%s (%S) does not exist" path var)))))


(defun mu4e~start (&optional func)
  "If mu4e is already running, execute function FUNC (if non-nil). Otherwise,
check various requirements, then start mu4e. When succesful, call
FUNC (if non-nil) afterwards."
  ;; if we're already running, simply go to the main view
  (if (mu4e~proc-is-running)   ;; already running?
    (when func
      (funcall func))) ;; yup!
    (progn ;; nope: check whether all is okay;
      (mu4e~check-requirements)
      ;; explicit version checks are a bit questionable,
      ;; better to check for specific features
      (unless (>= emacs-major-version 23)
	(error "Emacs >= 23.x is required for mu4e"))

      ;; set up the 'pong' handler func
      (lexical-let ((func func))
	(setq mu4e-pong-func
	  (lambda (version doccount)
	    (unless (string= version mu4e-mu-version)
	      (error "mu server has version %s, but we need %s"
		version mu4e-mu-version))
	    (when func (funcall func))
	    (when (and mu4e-update-interval (null mu4e-update-timer))
	      (setq mu4e-update-timer
		(run-at-time
		  0 mu4e-update-interval 'mu4e-update-mail)))
	    (mu4e-message "Started mu4e with %d message%s in store"
	      doccount (if (= doccount 1) "" "s")))))
      ;; send the ping
      (mu4e~proc-ping)
      ;; get the address list
      (when mu4e-compose-complete-addresses
	(setq mu4e-contacts-func 'mu4e~fill-contacts)
	(mu4e~proc-contacts
	  mu4e-compose-complete-only-personal
	  (when mu4e-compose-complete-only-after
	    (float-time
	      (apply 'encode-time
		(org-parse-time-string mu4e-compose-complete-only-after))))))))

(defun mu4e~stop ()
  "Stop the mu4e session."
  (when mu4e-update-timer
    (cancel-timer mu4e-update-timer)
    (setq
      mu4e-update-timer nil
      mu4e~maildir-list nil))
  (mu4e~proc-kill)
  ;; kill all main/view/headers buffer
  (mapcar
    (lambda (buf)
      (with-current-buffer buf
	(when (member major-mode '(mu4e-headers-mode mu4e-view-mode mu4e-main-mode))
	  (kill-buffer))))
    (buffer-list)))


(defvar mu4e-update-timer nil
  "*internal* The mu4e update timer.")

(defconst mu4e-update-mail-name "*mu4e-update-mail*"
  "*internal* Name of the process to update mail")

(defun mu4e-update-mail (&optional buf)
  "Update mail (retrieve using `mu4e-get-mail-command' and update
the database afterwards), with output going to BUF if not nil, or
discarded if nil. After retrieving mail, update the database. Note,
function is asynchronous, returns (almost) immediately, and all the
processing takes part in the background, unless buf is non-nil."
  (unless mu4e-get-mail-command
    (error "`mu4e-get-mail-command' is not defined"))
  (let* ((process-connection-type t)
	  (proc (start-process-shell-command
		 mu4e-update-mail-name buf mu4e-get-mail-command)))
    (mu4e-message "Retrieving mail...")
    (set-process-sentinel proc
      (lambda (proc msg)
	(let* ((status (process-status proc))
		(code (process-exit-status proc))
		;; sadly, fetchmail returns '1' when there is no mail; this is
		;; not really an error of course, but it's hard to distinguish
		;; from a genuine error
		(maybe-error (or (not (eq status 'exit)) (/= code 0)))
		(buf (process-buffer proc)))
	  (message nil)
	  ;; there may be an error, give the user up to 5 seconds to check
	  (when maybe-error
	    (sit-for 5))
	  (mu4e~proc-index mu4e-maildir mu4e-my-email-addresses)
	  (when (buffer-live-p buf)
	    (kill-buffer buf)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging / debugging
(defvar mu4e~log-max-lines 1200
  "*internal* Last <n> number of lines to keep around in the buffer.")
(defconst mu4e~log-buffer-name "*mu4e-log*"
  "*internal* Name of the logging buffer.")

(defun mu4e-log (type frm &rest args)
  "Write a message of TYPE with format-string FRM and ARGS in
*mu4e-log* buffer, if the variable mu4e-debug is non-nil. Type is
either 'to-server, 'from-server or 'misc. This function is meant for debugging."
  (when mu4e-debug
    (with-current-buffer (get-buffer-create mu4e~log-buffer-name)
      (view-mode)
      (setq buffer-undo-list t)
      (let* ((inhibit-read-only t)
	      (tstamp (propertize (format-time-string "%Y-%m-%d %T" (current-time))
			'face 'font-lock-string-face))
	      (msg-face
		(case type
		  (from-server 'font-lock-type-face)
		  (to-server   'font-lock-function-name-face)
		  (misc        'font-lock-variable-name-face)
		  (otherwise   (error "Unsupported log type"))))
	      (msg (propertize (apply 'format frm args) 'face msg-face)))
	(goto-char (point-max))
	(insert tstamp
	  (case type
	    (from-server " <- ")
	    (to-server   " -> " )
	    (otherwise   " "))
	  msg "\n")

	;; if `mu4e-log-max-lines is specified and exceeded, clearest the oldest
	;; lines
	(when (numberp mu4e~log-max-lines)
	  (let ((lines (count-lines (point-min) (point-max))))
	    (when (> lines mu4e~log-max-lines)
	      (goto-char (point-max))
	      (forward-line (- mu4e~log-max-lines lines))
	      (beginning-of-line)
	      (delete-region (point-min) (point)))))))))



(defun mu4e-toggle-logging ()
  "Toggle between enabling/disabling debug-mode (in debug-mode,
mu4e logs some of its internal workings to a log-buffer. See
`mu4e-visit-log'."
  (interactive)
  (mu4e-log 'misc "logging disabled")
  (setq mu4e-debug (not mu4e-debug))
  (mu4e-message "debug logging has been %s"
    (if mu4e-debug "enabled" "disabled"))
  (mu4e-log 'misc "logging enabled"))



(defun mu4e-show-log ()
  "Visit the mu4e debug log."
  (interactive)
  (let ((buf (get-buffer mu4e~log-buffer-name)))
    (unless (buffer-live-p buf)
      (error "No debug log available"))
    (switch-to-buffer buf)))


(defun mu4e-split-ranges-to-numbers (str n)
  "Convert STR containing attachment numbers into a list of numbers.
STR is a string; N is the highest possible number in the list.
This includes expanding e.g. 3-5 into 3,4,5.  If the letter
\"a\" ('all')) is given, that is expanded to a list with numbers [1..n]."
  (let ((str-split (split-string str))
	 beg end list)
    (dolist (elem str-split list)
      ;; special number "a" converts into all attachments 1-N.
      (when (equal elem "a")
	(setq elem (concat "1-" (int-to-string n))))
      (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" elem)
	;; we have found a range A-B, which needs converting
	;; into the numbers A, A+1, A+2, ... B.
	(progn
	  (setq beg (string-to-number (match-string 1 elem))
	    end (string-to-number (match-string 2 elem)))
	  (while (<= beg end)
	    (add-to-list 'list beg 'append)
	    (setq beg (1+ beg))))
	;; else just a number
	(add-to-list 'list (string-to-number elem) 'append)))
    ;; Check that all numbers are valid.
    (mapc
      #'(lambda (x)
	  (cond
	    ((> x n)
	      (error "Attachment %d bigger than maximum (%d)" x n))
	    ((< x 1)
	      (error "Attachment number must be greater than 0 (%d)" x))))
      list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mu4e-imagemagick-identify "identify"
  "Name/path of the Imagemagick 'identify' program.")

(defun mu4e-display-image (imgpath &optional maxwidth)
  "Display image IMG at point; optionally specify
MAXWIDTH. Function tries to use imagemagick if available (ie.,
emacs was compiled with inmagemagick support); otherwise MAXWIDTH
is ignored."
  (let* ((have-im (and (fboundp 'imagemagick-types)
		    (imagemagick-types))) ;; hmm, should check for specific type
	  (identify (and have-im maxwidth
		      (executable-find mu4e-imagemagick-identify)))
	  (props (and identify (shell-command-to-string
				 (format "%s -format '%%w' %s"
				   identify (shell-quote-argument imgpath)))))
	  (width (and props (string-to-number props)))
	  (img (if have-im
		 (if (> (or width 0) (or maxwidth 0))
		   (create-image imgpath 'imagemagick nil :width maxwidth)
		   (create-image imgpath 'imagemagick))
		 (create-image imgpath))))
    ;;(message "DISPLAY: %S %S" imgpath img)
    (when img
      (newline)
      (insert-image img imgpath nil t))))



(defun mu4e-hide-other-mu4e-buffers ()
  "Bury mu4e-buffers (main, headers, view) (and delete all windows
displaying it). Do _not_ bury the current buffer, though."
  (interactive)
  (let ((curbuf (current-buffer)))
    ;; note: 'walk-windows' does not seem to work correctly when modifying
    ;; windows; therefore, the doloops here
    (dolist (frame (frame-list))
      (dolist (win (window-list frame nil))
	(with-current-buffer (window-buffer win)
	  (unless (eq curbuf (current-buffer))
	    (when (member major-mode '(mu4e-headers-mode mu4e-view-mode))
	      (unless (one-window-p t)
		(delete-window win))))))) nil t))


(defun mu4e-get-time-date (prompt)
  "Determine the emacs time value for the time/date entered by user
  after PROMPT. Formats are all that are accepted by
  `parse-time-string'."
  (let ((timestr (read-string (mu4e-format "%s" prompt))))
    (apply 'encode-time (org-parse-time-string timestr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mu4e~main-about-buffer-name "*mu4e-about*"
  "Name for the mu4e-about buffer.")

(defun mu4e-about ()
  "Show a buffer with the mu4e-about text."
  (interactive)
  (with-current-buffer
    (get-buffer-create mu4e~main-about-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert mu4e-about)
      (org-mode)))
  (switch-to-buffer mu4e~main-about-buffer-name)
  (setq buffer-read-only t)
  (local-set-key "q" 'bury-buffer)
  (goto-char (point-min)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mu4e-utils)
;;; End of mu4e-utils.el
