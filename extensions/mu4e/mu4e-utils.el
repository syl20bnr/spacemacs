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

(eval-when-compile (require 'org nil 'noerror))

(require 'mu4e-vars)
(require 'mu4e-about)
(require 'mu4e-lists)
(require 'doc-view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following is taken from org.el; we copy it here since we don't want to
;; depend on org-mode directly (it causes byte-compilation errors) TODO: a
;; cleaner solution....
(defconst mu4e~ts-regexp0
  (concat
    "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
    "\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)")
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be
used on a string that terminates immediately after the date.")

(defun mu4e-parse-time-string (s &optional nodefault)
  "Parse the standard Org-mode time string.
This should be a lot faster than the normal `parse-time-string'.
If time is not given, defaults to 0:00.  However, with optional
NODEFAULT, hour and minute fields will be nil if not given."
  (if (string-match mu4e~ts-regexp0 s)
      (list 0
	    (if (or (match-beginning 8) (not nodefault))
		(string-to-number (or (match-string 8 s) "0")))
	    (if (or (match-beginning 7) (not nodefault))
		(string-to-number (or (match-string 7 s) "0")))
	    (string-to-number (match-string 4 s))
	    (string-to-number (match-string 3 s))
	    (string-to-number (match-string 2 s))
	    nil nil nil)
    (mu4e-error "Not a standard mu4e time string: %s" s)))


(defun mu4e-user-mail-address-p (addr)
  "If ADDR is one of user's e-mail addresses return t, nil otherwise.
User's addresses are set in `mu4e-user-mail-address-list')."
  (when (and addr mu4e-user-mail-address-list
	  (find addr mu4e-user-mail-address-list :test 'string=))
    t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the standard folders can be functions too
(defun mu4e~get-folder (foldervar msg)
  "Get message folder FOLDER.
If FOLDER is a string, return it, if it is a function, evaluate
this function with MSG as parameter (which may be `nil'), and
return the result."
  (unless (member foldervar '(mu4e-sent-folder mu4e-drafts-folder
			       mu4e-trash-folder mu4e-refile-folder))
    (mu4e-error "Folder must be either mu4e-sent-folder,
    mu4e-drafts-folder or mu4e-trash-folder (not %S)" foldervar))
  (let* ((folder (symbol-value foldervar))
	  (val
	    (cond
	      ((stringp   folder) folder)
	      ((functionp folder) (funcall folder msg))
	      (t (mu4e-error "unsupported type for %S" folder)))))
    (or val (mu4e-error "%S evaluates to nil" foldervar))))

(defun mu4e-get-drafts-folder (&optional msg)
  "Get the sent folder. See `mu4e-drafts-folder'."
  (mu4e~get-folder 'mu4e-drafts-folder msg))

(defun mu4e-get-refile-folder (&optional msg)
  "Get the folder for refiling. See `mu4e-refile-folder'."
  (mu4e~get-folder 'mu4e-refile-folder msg))

(defun mu4e-get-sent-folder (&optional msg)
  "Get the sent folder. See `mu4e-sent-folder'."
  (mu4e~get-folder 'mu4e-sent-folder msg))

(defun mu4e-get-trash-folder (&optional msg)
  "Get the sent folder. See `mu4e-trash-folder'."
  (mu4e~get-folder 'mu4e-trash-folder msg))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e-attachment-dir is either a string or a function that takes a filename
;; and the mime-type as argument, either (or both) which can be nil
(defun mu4e~get-attachment-dir (&optional fname mimetype)
  "Get the directory for saving attachments from
`mu4e-attachment-dir' (which can be either a string or a function,
see its docstring)."
  (let
    ((dir
       (cond
	 ((stringp mu4e-attachment-dir)
	   mu4e-attachment-dir)
	 ((functionp mu4e-attachment-dir)
	   (funcall mu4e-attachment-dir fname mimetype))
	 (t
	   (mu4e-error "unsupported type for mu4e-attachment-dir" )))))
    (if dir
      (expand-file-name dir)
      (mu4e-error (mu4e-error "mu4e-attachment-dir evaluates to nil")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e-create-maildir-maybe (dir)
  "Offer to create maildir DIR if it does not exist yet.
Return t if the dir already existed, or has been created, nil
otherwise. DIR has to be an absolute path."
  (if (and (file-exists-p dir) (not (file-directory-p dir)))
    (mu4e-error "%s exists, but is not a directory." dir))
  (cond
    ((file-directory-p dir) t)
    ((yes-or-no-p (mu4e-format "%s does not exist yes. Create now?" dir))
      (mu4e~proc-mkdir dir))
    (t nil)))

(defun mu4e-format (frm &rest args)
  "Create [mu4e]-prefixed string based on format FRM and ARGS."
  (concat
    "[" (propertize "mu4e" 'face 'mu4e-title-face) "] "
    (apply 'format frm args)))

(defun mu4e-message (frm &rest args)
  "Like `message', but prefixed with mu4e.
If we're waiting for user-input, don't show anyhting."
  (unless (active-minibuffer-window)
    (message "%s" (apply 'mu4e-format frm args))
    nil))

(defun mu4e-error (frm &rest args)
  "Create [mu4e]-prefixed error based on format FRM and ARGS.
Does a local-exit and does not return, and raises a
debuggable (backtrace) error."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (error "%s" (apply 'mu4e-format frm args)))

;; the user-error function is only available in emacs-trunk
(unless (fboundp 'user-error)
  (defalias 'user-error 'error))

(defun mu4e-warn (frm &rest args)
  "Create [mu4e]-prefixed warning based on format FRM and ARGS.
Does a local-exit and does not return. In emacs versions below
24.2, the functions is the same as `mu4e-error'."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (user-error "%s" (apply 'mu4e-format frm args)))

(defun mu4e~read-char-choice (prompt choices)
  "Compatiblity wrapper for `read-char-choice'.
That function is available which emacs-24 only."
  (let ((choice) (ok) (inhibit-quit nil))
    (while (not ok)
      (message nil);; this seems needed...
      (setq choice (read-char-exclusive prompt))
      (setq ok (member choice choices)))
    choice))

(defun mu4e-read-option (prompt options)
  "Ask user for an option from a list on the input area.
PROMPT describes a multiple-choice question to the user.
OPTIONS describe the options, and is a list of cells describing
particular options. Cells have the following structure:

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
		  (mu4e-error (concat "Please use the new format for options/actions; "
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
		" [" (propertize "C-g" 'face 'mu4e-highlight-face) " to cancel]")
	      ;; the allowable chars
	      (map 'list (lambda(elm) (string-to-char (car elm))) options)))
	  (chosen
	    (find-if
	      (lambda (option) (eq response (string-to-char (car option))))
	      options)))
    (if chosen
      (cdr chosen)
      (mu4e-warn "Unknown shortcut '%c'" response))))


(defun mu4e~get-maildirs-1 (path mdir)
  "Get maildirs under path, recursively, as a list of relative paths."
  (let ((dirs)
	 (dentries
	   (ignore-errors
	     (directory-files-and-attributes
	       (concat path mdir) nil
	       "^[^.]\\|\\.[^.][^.]" t))))
    (dolist (dentry dentries)
      (when (and (booleanp (cadr dentry)) (cadr dentry))
	(if (file-accessible-directory-p
	      (concat mu4e-maildir "/" mdir "/" (car dentry) "/cur"))
	  (setq dirs (cons (concat mdir (car dentry)) dirs))
	  (setq dirs (append dirs (mu4e~get-maildirs-1 path
				    (concat mdir (car dentry) "/")))))))
    dirs))

(defvar mu4e~maildir-list nil "Cached list of maildirs.")

(defun mu4e-get-maildirs ()
  "Get maildirs under `mu4e-maildir', recursively, as a list of
relative paths (ie., /archive, /sent etc.). Most of the work is
done in `mu4e-get-maildirs-1'. Note, these results are /cached/, so
the list of maildirs will not change until you restart mu4e."
  (unless mu4e-maildir (mu4e-error "`mu4e-maildir' is not defined"))
  (unless mu4e~maildir-list
    (setq mu4e~maildir-list
      (sort
	(append
	  (when (file-accessible-directory-p (concat mu4e-maildir "/cur")) '("/"))
	  (mu4e~get-maildirs-1 mu4e-maildir "/"))
	(lambda (s1 s2) (string< (downcase s1) (downcase s2))))))
  mu4e~maildir-list)

(defun mu4e-ask-maildir (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts' is not defined, let user choose from all
maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (if (not mu4e-maildir-shortcuts)
      (ido-completing-read prompt (mu4e-get-maildirs))
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
	(if (member kar '(?/ ?o)) ;; user chose 'other'?
	  (ido-completing-read prompt (mu4e-get-maildirs) nil nil "/")
	  (or (car-safe
		(find-if (lambda (item) (= kar (cdr item))) mu4e-maildir-shortcuts))
	    (mu4e-warn "Unknown shortcut '%c'" kar)))))))


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


(defun mu4e-ask-bookmark (prompt &optional kar)
  "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query."
  (unless mu4e-bookmarks (mu4e-error "`mu4e-bookmarks' is not defined"))
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
     (mu4e-warn "Unknown shortcut '%c'" kar))))


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
  "Convert a string with message flags as seen in Maildir messages
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


(defun mu4e-display-manual ()
  "Display the mu4e manual page for the current mode.
Or go to the top level if there is none."
  (interactive)
  (info (case major-mode
	  ('mu4e-main-mode "(mu4e)Main view")
	  ('mu4e-headers-mode "(mu4e)Headers view")
	  ('mu4e-view-mode "(mu4e)Message view")
	  (t               "mu4e"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e-last-query ()
  "Get the most recent query or nil if there is none."
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer  mu4e~headers-buffer
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
(defvar mu4e~lists-hash nil
  "Hashtable of mailing-list-id => shortname, based on
  `mu4e~mailing-lists' and `mu4e-user-mailing-lists'.")

(defun mu4e-get-mailing-list-shortname (list-id)
  "Get the shortname for a mailing-list with list-id LIST-ID. based on `mu4e~mailing-lists'
  and `mu4e-user-mailing-lists'."
  (unless mu4e~lists-hash
    (setq mu4e~lists-hash (make-hash-table :test 'equal))
    (dolist (cell mu4e~mailing-lists) (puthash (car cell) (cdr cell) mu4e~lists-hash))
    (dolist (cell mu4e-user-mailing-lists) (puthash (car cell) (cdr cell) mu4e~lists-hash)))
  (or
    (gethash list-id mu4e~lists-hash)
    ;; if it's not in the db, take the part until the first dot if there is one;
    ;; otherwise just return the whole thing
    (if (string-match "\\([^.]*\\)\\." list-id)
      (match-string 1 list-id)
      list-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mu4e-index-updated-hook nil
  "Hook run when the indexing process had one or more updated messages.
This can be used as a simple way to invoke some action when new
messages appear, but note that an update in the index does not
necessarily mean a new message.")

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
	  (progn
	    (mu4e-message
	      "Indexing completed; processed %d, updated %d, cleaned-up %d"
	      (plist-get info :processed) (plist-get info :updated)
	      (plist-get info :cleaned-up))
	    (unless (zerop (plist-get info :updated))
	      (run-hooks 'mu4e-index-updated-hook)))))
      ((plist-get info :message)
	(mu4e-message "%s" (plist-get info :message))))))

(defun mu4e-error-handler (errcode errmsg)
  "Handler function for showing an error."
  ;; don't use mu4e-error here; it's running in the process filter ctx
  (case errcode
    (4 (user-error "No matches for this search query."))
    (t (error "Error %d: %s" errcode errmsg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
	    (if name (format "\"%s\" <%s>" name mail) mail))))))
    (setq mu4e~contacts-for-completion lst)
    (mu4e-message "Contacts received: %d"
      (length mu4e~contacts-for-completion))))


(defun mu4e~check-requirements ()
  "Check for the settings required for running mu4e."
  (unless (>= emacs-major-version 23)
    (mu4e-error "Emacs >= 23.x is required for mu4e"))
  (when mu4e~server-props
    (let ((version (plist-get mu4e~server-props :version)))
      (unless (string= version mu4e-mu-version)
	(mu4e-error "mu server has version %s, but we need %s"
	  version mu4e-mu-version))))
  (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
    (mu4e-error "Please set `mu4e-mu-binary' to the full path to the mu
    binary."))
  (unless mu4e-maildir
    (mu4e-error "Please set `mu4e-maildir' to the full path to your
    Maildir directory."))
  ;; expand mu4e-maildir, mu4e-attachment-dir
  (setq mu4e-maildir (expand-file-name mu4e-maildir))
  (unless (mu4e-create-maildir-maybe mu4e-maildir)
    (mu4e-error "%s is not a valid maildir directory" mu4e-maildir))
  (dolist (var '(mu4e-sent-folder mu4e-drafts-folder
		  mu4e-trash-folder))
    (unless (and (boundp var) (symbol-value var))
      (mu4e-error "Please set %S" var))
    (unless (functionp (symbol-value var)) ;; functions are okay, too
      (let* ((dir (symbol-value var))
	      (path (concat mu4e-maildir dir)))
	(unless (string= (substring dir 0 1) "/")
	  (mu4e-error "%S must start with a '/'" dir))
	(unless (mu4e-create-maildir-maybe path)
	  (mu4e-error "%s (%S) does not exist" path var))))))


(defun mu4e-running-p ()
  "Whether mu4e is running.
Checks whether the server process is live."
  (mu4e~proc-running-p))

(defun mu4e~start (&optional func)
  "If mu4e is already running, execute function FUNC (if non-nil).
Otherwise, check various requirements, then start mu4e. When
successful, call FUNC (if non-nil) afterwards."
  ;; if we're already running, simply go to the main view
  (if (mu4e-running-p)   ;; already running?
    (when func                 ;; yes! run func if defined
      (funcall func))
    (progn
      ;; no! do some checks, set up pong handler and ping the server
      (lexical-let ((func func))
	(mu4e~check-requirements)
	;; set up the 'pong' handler func
	(setq mu4e-pong-func
	  (lambda (props)
	    (setq mu4e~server-props props) ;; save the props we got from the server
	    (let ((version (plist-get props :version))
		   (doccount (plist-get props :doccount)))
	      (mu4e~check-requirements)
	      (when func (funcall func))
	      (when (and mu4e-update-interval (null mu4e~update-timer))
		(setq mu4e~update-timer
		  (run-at-time
		    0 mu4e-update-interval
		    (lambda () (mu4e-update-mail-and-index t)))))
	      (mu4e-message "Started mu4e with %d message%s in store"
		doccount (if (= doccount 1) "" "s"))))))

      (mu4e~proc-ping)

      ;; get the address list if it's not already set.
      (when (and mu4e-compose-complete-addresses
	      (not mu4e~contacts-for-completion))
	(setq mu4e-contacts-func 'mu4e~fill-contacts)
	(mu4e~proc-contacts
	  mu4e-compose-complete-only-personal
	  (when mu4e-compose-complete-only-after
	    (float-time
	      (apply 'encode-time
		(mu4e-parse-time-string mu4e-compose-complete-only-after)))))))))

(defun mu4e~stop ()
  "Stop the mu4e session."
  (when mu4e~update-timer
    (cancel-timer mu4e~update-timer)
    (setq
      mu4e~update-timer nil
      mu4e~maildir-list nil
      mu4e~contacts-for-completion nil))
  (mu4e~proc-kill)
  ;; kill all main/view/headers buffer
  (mapcar
    (lambda (buf)
      (with-current-buffer buf
	(when (member major-mode
		'(mu4e-headers-mode mu4e-view-mode mu4e-main-mode))
	  (kill-buffer))))
    (buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getting mail / updating the index
;;
;;
(defvar mu4e~update-timer nil
  "The mu4e update timer.")
(defconst mu4e~update-name "*mu4e-update*"
  "Name of the process and buffer to update mail.")
(defconst mu4e~update-buffer-height 8
  "Height of the mu4e message retrieval/update buffer.")

(defvar mu4e~get-mail-ask-password "mu4e get-mail: Enter password: "
  "Query string for `mu4e-get-mail-command' password.")
(defvar mu4e~get-mail-password-regexp "^Remote: Enter password: $"
  "Regexp to match a password query in the `mu4e-get-mail-command' output.")

(defun mu4e~get-mail-process-filter (proc msg)
  "Filter the output of `mu4e-get-mail-command'.
Currently the filter only checks if the command asks for a password
by matching the output against `mu4e~get-mail-password-regexp'.
The messages are inserted into the process buffer."
  (save-current-buffer
    (when (process-buffer proc)
      (set-buffer (process-buffer proc)))
    (let ((inhibit-read-only t))
      ;; Check whether process asks for a password and query user
      (when (string-match mu4e~get-mail-password-regexp msg)
        (if (process-get proc 'x-interactive)
            (process-send-string proc
	      (concat (read-passwd mu4e~get-mail-ask-password) "\n"))
	  ;; TODO kill process?
          (mu4e-error "Unrecognized password request")))
      (when (process-buffer proc)
        (insert msg)))))

(defun  mu4e-update-index ()
  "Update the mu4e index."
  (interactive)
  (unless mu4e-maildir
    (mu4e-error "`mu4e-maildir' is not defined"))
  (mu4e~proc-index mu4e-maildir mu4e-user-mail-address-list))

;; complicated function, as it:
;;   - needs to check for errors
;;   - (optionally) pop-up a window
;;   - (optionally) check password requests
(defun mu4e-update-mail-and-index (run-in-background)
  "Get a new mail by running `mu4e-get-mail-command'. If
run-in-background is non-nil (or functional called with
prefix-argument), run in the background; otherwise, pop up a
window."
  (interactive "P")
  (unless mu4e-get-mail-command
    (mu4e-error "`mu4e-get-mail-command' is not defined"))
  (let* ((buf (unless run-in-background
		(get-buffer-create mu4e~update-name)))
	  (win (and buf (split-window (selected-window)
			  (- (window-height (selected-window)) 8))))
	  (process-connection-type t)
	  (proc (start-process-shell-command
		  mu4e~update-name buf mu4e-get-mail-command)))
    (mu4e-message "Retrieving mail...")
    (when (window-live-p win)
      (with-selected-window win
	(switch-to-buffer buf)
	(set-window-dedicated-p win t)
	(erase-buffer)
	(insert "\n"))) ;; FIXME -- needed so output starts
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
	  (when maybe-error (sit-for 5))
	  (mu4e-update-index)
	  (when (buffer-live-p buf) (kill-buffer buf)))))
    ;; if we're running in the foreground, handle password requests
    (unless run-in-background
      (process-put proc 'x-interactive (not run-in-background))
      (set-process-filter proc 'mu4e~get-mail-process-filter))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
		  (error       'font-lock-warning-face)
		  (otherwise   (mu4e-error "Unsupported log type"))))
	      (msg (propertize (apply 'format frm args) 'face msg-face)))
	(goto-char (point-max))
	(insert tstamp
	  (case type
	    (from-server " <- ")
	    (to-server   " -> ")
	    (error       " !! ")
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
      (mu4e-warn "No debug log available"))
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
	      (mu4e-warn "Attachment %d bigger than maximum (%d)" x n))
	    ((< x 1)
	      (mu4e-warn "Attachment number must be greater than 0 (%d)" x))))
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
    (apply 'encode-time (mu4e-parse-time-string timestr))))
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
      (org-mode)
      (show-all)))
  (switch-to-buffer mu4e~main-about-buffer-name)
  (setq buffer-read-only t)
  (local-set-key "q" 'bury-buffer)
  (goto-char (point-min)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-refresh-message (path maildir)
  "Re-parse message at PATH and MAILDIR; if this works, we will
receive (:info add :path <path> :docid <docid>) as well as (:update
<msg-sexp>)."
  (mu4e~proc-add path maildir))

(provide 'mu4e-utils)
;;; End of mu4e-utils.el
