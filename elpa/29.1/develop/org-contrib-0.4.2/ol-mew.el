;;; ol-mew.el --- Links to Mew messages

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Tokuya Kameshima <kames at fa2 dot so-net dot ne dot jp>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Mew messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.
;;
;; Here is an example of workflow:

;; In your ~/.mew.el configuration file:
;;
;; (define-key mew-summary-mode-map "'" 'org-mew-search)
;; (eval-after-load "mew-summary"
;;   '(define-key mew-summary-mode-map "\C-o" 'org-mew-capture))

;; 1. In the Mew's inbox folder, take a glance at new messages to find
;;    a message that requires any action.

;; 2. If the message is a reply from somebody and associated with the
;;    existing orgmode entry, type M-x `org-mew-search' RET (or press
;;    the ' key simply) to find the entry.  If you can find the entry
;;    successfully and think you should start the task right now,
;;    start the task by M-x `org-agenda-clock-in' RET.

;; 3. If the message is a new message, type M-x `org-mew-capture' RET,
;;    enter the refile folder, and the buffer to capture the message
;;    is shown up (without selecting the template by hand).  Then you
;;    can fill the template and type C-c C-c to complete the capture.
;;    Note that you can configure `org-capture-templates' so that the
;;    captured entry has a link to the message.

;;; Code:

(require 'org)
(require 'ol)

(defgroup org-mew nil
  "Options concerning the Mew link."
  :tag "Org Startup"
  :group 'org-link)

(defcustom org-mew-link-to-refile-destination t
  "Create a link to the refile destination if the message is marked as refile."
  :group 'org-mew
  :type 'boolean)

(defcustom org-mew-inbox-folder nil
  "The folder where new messages are incorporated.
If `org-mew-inbox-folder' is non-nil, `org-mew-open' locates the message
in this inbox folder as well as the folder specified by the link."
  :group 'org-mew
  :type 'string)

(defcustom org-mew-use-id-db t
  "Use ID database to locate the message if id.db is created."
  :group 'org-mew
  :type 'boolean)

(defcustom org-mew-subject-alist
  (list (cons (concat "^\\(?:\\(?:re\\|fwd?\\): *\\)*"
		      "\\(?:[[(][a-z0-9._-]+[:,]? [0-9]+[])]\\)? *"
		      "\\(?:\\(?:re\\|fwd?\\): *\\)*"
		      "\\(.*\\)[ \t]*")
	      1))
  "Alist of subject regular expression and matched group number for search."
  :group 'org-mew
  :type '(repeat (cons (regexp) (integer))))

(defcustom org-mew-capture-inbox-folders nil
  "List of inbox folders whose messages need refile marked before capture.
`org-mew-capture' will ask you to put the refile mark on the
message if the message's folder is any of these folders and the
message is not marked.  Nil means `org-mew-capture' never ask you
destination folders before capture."
  :group 'org-mew
  :type '(repeat string))

(defcustom org-mew-capture-guess-alist nil
  "Alist of the regular expression of the folder name and the capture
template selection keys.

For example,
    '((\"^%emacs-orgmode$\" . \"o\")
      (\"\" . \"t\"))
the messages in \"%emacs-orgmode\" folder will be captured with
the capture template associated with \"o\" key, and any other
messages will be captured with the capture template associated
with \"t\" key."
  :group 'org-mew
  :type '(repeat (cons regexp string)))

;; Declare external functions and variables
(declare-function mew-cache-hit "ext:mew-cache" (fld msg &optional must-hit))
(declare-function mew-case-folder "ext:mew-func" (case folder))
(declare-function mew-folder-path-to-folder
		  "ext:mew-func" (path &optional has-proto))
(declare-function mew-idstr-to-id-list "ext:mew-header" (idstr &optional rev))
(declare-function mew-folder-remotep "ext:mew-func" (folder))
(declare-function mew-folder-virtualp "ext:mew-func" (folder))
(declare-function mew-header-get-value "ext:mew-header"
		  (field &optional as-list))
(declare-function mew-init "ext:mew" ())
(declare-function mew-refile-get "ext:mew-refile" (msg))
(declare-function mew-sinfo-get-case "ext:mew-summary" ())
(declare-function mew-summary-diag-global "ext:mew-thread" (id opt who))
(declare-function mew-summary-display "ext:mew-summary2" (&optional redisplay))
(declare-function mew-summary-folder-name "ext:mew-syntax" (&optional ext))
(declare-function mew-summary-get-mark "ext:mew-mark" ())
(declare-function mew-summary-message-number2 "ext:mew-syntax" ())
(declare-function mew-summary-pick-with-mewl "ext:mew-pick"
		  (pattern folder src-msgs))
(declare-function mew-summary-refile "ext:mew-refile" (&optional report))
(declare-function mew-summary-search-msg "ext:mew-const" (msg))
(declare-function mew-summary-set-message-buffer "ext:mew-summary3" (fld msg))
(declare-function mew-summary-visit-folder "ext:mew-summary4"
		  (folder &optional goend no-ls))
(declare-function mew-window-push "ext:mew" ())
(declare-function mew-expand-folder "ext:mew-func" (folder))
(declare-function mew-case:folder-folder "ext:mew-func" (case:folder))
(declare-function mew "ext:mew" (&optional arg))
(declare-function mew-message-goto-summary "ext:mew-message" ())
(declare-function mew-summary-mode "ext:mew-summary" ())

(defvar mew-init-p)
(defvar mew-mark-afterstep-spec)
(defvar mew-summary-goto-line-then-display)

;; Install the link type
(org-link-set-parameters "mew" :follow #'org-mew-open :store #'org-mew-store-link)

;; Implementation
(defun org-mew-store-link ()
  "Store a link to a Mew folder or message."
  (save-window-excursion
    (if (eq major-mode 'mew-message-mode)
	(mew-message-goto-summary))
    (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
      (let ((msgnum (mew-summary-message-number2))
	    (folder-name (org-mew-folder-name)))
	(if (fboundp 'mew-summary-set-message-buffer)
	    (mew-summary-set-message-buffer folder-name msgnum)
	  (set-buffer (mew-cache-hit folder-name msgnum t)))
	(let* ((message-id (mew-header-get-value "Message-Id:"))
	       (from (mew-header-get-value "From:"))
	       (to (mew-header-get-value "To:"))
	       (date (mew-header-get-value "Date:"))
	       (subject (mew-header-get-value "Subject:"))
	       desc link)
	  (org-store-link-props :type "mew" :from from :to to :date date
				:subject subject :message-id message-id)
	  (setq message-id (org-unbracket-string "<" ">" message-id))
	  (setq desc (org-email-link-description))
	  (setq link (concat "mew:" folder-name "#" message-id))
	  (org-add-link-props :link link :description desc)
	  link)))))

(defun org-mew-folder-name ()
  "Return the folder name of the current message."
  (save-window-excursion
    (if (eq major-mode 'mew-message-mode)
	(mew-message-goto-summary))
    (let* ((msgnum (mew-summary-message-number2))
	   (mark-info (mew-summary-get-mark)))
      (if (and org-mew-link-to-refile-destination
	       (eq mark-info ?o))	; marked as refile
	  (mew-case-folder (mew-sinfo-get-case)
			   (nth 1 (mew-refile-get msgnum)))
	(let ((folder-or-path (mew-summary-folder-name)))
	  (mew-folder-path-to-folder folder-or-path t))))))

(defun org-mew-open (path _)
  "Follow the Mew message link specified by PATH."
  (let (folder message-id)
    (cond ((string-match "\\`\\(+.*\\)+\\+\\([0-9]+\\)\\'" path) ; for Bastien's
	   (setq folder (match-string 1 path))
	   (setq message-id (match-string 2 path)))
	  ((string-match "\\`\\(\\(%#\\)?[^#]+\\)\\(#\\(.*\\)\\)?" path)
	   (setq folder (match-string 1 path))
	   (setq message-id (match-string 4 path)))
	  ((and org-mew-use-id-db (string-match "\\`#\\(.+\\)" path))
	   (setq folder nil)
	   (setq message-id (match-string 1 path)))
	  (t (error "Error in Mew link")))
    (require 'mew)
    (mew-window-push)
    (unless mew-init-p (mew-init))
    (if (null folder)
	(progn
	  (mew t)
	  (org-mew-open-by-message-id message-id))
      (or (org-mew-follow-link folder message-id)
	  (and org-mew-inbox-folder (not (string= org-mew-inbox-folder folder))
	       (org-mew-follow-link org-mew-inbox-folder message-id))
	  (and org-mew-use-id-db
	       (org-mew-open-by-message-id message-id))
	  (error "Message not found")))))

(defun org-mew-follow-link (folder message-id)
  (unless (org-mew-folder-exists-p folder)
    (error "No such folder or wrong folder %s" folder))
  (mew-summary-visit-folder folder)
  (when message-id
    (let ((msgnum (org-mew-get-msgnum folder message-id)))
      (when (mew-summary-search-msg msgnum)
	(if mew-summary-goto-line-then-display
	    (mew-summary-display))
	t))))

(defun org-mew-folder-exists-p (folder)
  (let ((dir (mew-expand-folder folder)))
    (cond
     ((mew-folder-virtualp folder) (get-buffer folder))
     ((null dir) nil)
     ((mew-folder-remotep (mew-case:folder-folder folder)) t)
     (t (file-directory-p dir)))))

(defun org-mew-get-msgnum (folder message-id)
  (if (string-match "\\`[0-9]+\\'" message-id)
      message-id
    (let* ((pattern (concat "message-id=" message-id))
	   (msgs (mew-summary-pick-with-mewl pattern folder nil)))
      (car msgs))))

(defun org-mew-open-by-message-id (message-id)
  "Open message using ID database."
  (let ((result (mew-summary-diag-global (format "<%s>" message-id)
					 "-p" "Message")))
    (unless (eq result t)
      (error "Message not found"))))

;; In ~/.mew.el, add the following line:
;;   (define-key mew-summary-mode-map "'" 'org-mew-search)
(defun org-mew-search (&optional arg)
  "Show all entries related to the message using `org-search-view'.

It shows entries which contains the message ID, the reference
IDs, or the subject of the message.

With C-u prefix, search for the entries that contains the message
ID or any of the reference IDs.  With C-u C-u prefix, search for
the message ID or the last reference ID.

The search phase for the subject is extracted with
`org-mew-subject-alist', which defines the regular expression of
the subject and the group number to extract.  You can get rid of
\"Re:\" and some other prefix from the subject text."
  (interactive "P")
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
    (let ((last-reference-only (equal arg '(16)))
	  (by-subject (null arg))
	  (msgnum (mew-summary-message-number2))
	  (folder-name (mew-summary-folder-name))
	  subject message-id references id-list)
      (save-window-excursion
	(if (fboundp 'mew-summary-set-message-buffer)
	    (mew-summary-set-message-buffer folder-name msgnum)
	  (set-buffer (mew-cache-hit folder-name msgnum t)))
	(setq subject (mew-header-get-value "Subject:"))
	(setq message-id (mew-header-get-value "Message-Id:"))
	(setq references (mew-header-get-value "References:")))
      (setq id-list (mapcar (lambda (id) (org-unbracket-string "<" ">" id))
			    (mew-idstr-to-id-list references)))
      (if last-reference-only
	  (setq id-list (last id-list))
	(if message-id
	    (setq id-list (cons (org-unbracket-string "<" ">" message-id)
				id-list))))
      (when (and by-subject (stringp subject))
	(catch 'matched
	  (mapc (lambda (elem)
		  (let ((regexp (car elem))
			(num (cdr elem)))
		    (when (string-match regexp subject)
		      (setq subject (match-string num subject))
		      (throw 'matched t))))
		org-mew-subject-alist))
	(setq id-list (cons subject id-list)))
      (cond ((null id-list)
	     (error "No message ID to search"))
	    ((equal (length id-list) 1)
	     (org-search-view nil (car id-list)))
	    (t
	     (org-search-view nil (format "{\\(%s\\)}"
					  (mapconcat 'regexp-quote
						     id-list "\\|"))))))
    (delete-other-windows)))

(defun org-mew-capture (arg)
  "Guess the capture template from the folder name and invoke `org-capture'.

This selects a capture template in `org-capture-templates' by
searching for capture template selection keys defined in
`org-mew-capture-guess-alist' which are associated with the
regular expression that matches the message's folder name, and
then invokes `org-capture'.

If the message's folder is a inbox folder, you are prompted to
put the refile mark on the message and the capture template is
guessed from the refile destination folder.  You can customize
the inbox folders by `org-mew-capture-inbox-folders'.

If ARG is non-nil, this does not guess the capture template but
asks you to select the capture template."
  (interactive "P")
  (or (not (member (org-mew-folder-name)
		   org-mew-capture-inbox-folders))
      (eq (mew-summary-get-mark) ?o)
      (save-window-excursion
	(if (eq major-mode 'mew-message-mode)
	    (mew-message-goto-summary))
	(let ((mew-mark-afterstep-spec '((?o 0 0 0 0 0 0 0))))
	  (mew-summary-refile)))
      (error "No refile folder selected"))
  (let* ((org-mew-link-to-refile-destination t)
	 (folder-name (org-mew-folder-name))
	 (keys (if arg
		   nil
		 (org-mew-capture-guess-selection-keys folder-name))))
    (org-capture nil keys)))

(defun org-mew-capture-guess-selection-keys (folder-name)
  (catch 'found
    (let ((alist org-mew-capture-guess-alist))
      (while alist
	(let ((elem (car alist)))
	  (if (string-match (car elem) folder-name)
	      (throw 'found (cdr elem))))
	(setq alist (cdr alist))))))

(provide 'ol-mew)

;;; ol-mew.el ends here
