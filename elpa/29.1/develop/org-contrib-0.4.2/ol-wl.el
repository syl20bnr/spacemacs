;;; ol-wl.el --- Links to Wanderlust messages

;; Copyright (C) 2004-2021 Free Software Foundation, Inc.

;; Author: Tokuya Kameshima <kames at fa2 dot so-net dot ne dot jp>
;;         David Maus <dmaus at ictsoc dot de>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;;
;; This file is not part of GNU Emacs.
;;
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

;; This file implements links to Wanderlust messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'ol)
(require 'org)

(defgroup org-wl nil
  "Options concerning the Wanderlust link."
  :tag "Org Startup"
  :group 'org-link)

(defcustom org-wl-link-to-refile-destination t
  "Create a link to the refile destination if the message is marked as refile."
  :group 'org-wl
  :type 'boolean)

(defcustom org-wl-link-remove-filter nil
  "Remove filter condition if message is filter folder."
  :group 'org-wl
  :type 'boolean)

(defcustom org-wl-shimbun-prefer-web-links nil
  "If non-nil create web links for shimbun messages."
  :group 'org-wl
  :type 'boolean)

(defcustom org-wl-nntp-prefer-web-links nil
  "If non-nil create web links for nntp messages.
When folder name contains string \"gmane\" link to gmane,
googlegroups otherwise."
  :type 'boolean
  :group 'org-wl)

(defcustom org-wl-disable-folder-check t
  "Disable check for new messages when open a link."
  :type 'boolean
  :group 'org-wl)

(defcustom org-wl-namazu-default-index nil
  "Default namazu search index."
  :type '(choice (const nil) (directory))
  :group 'org-wl)

;; Declare external functions and variables
(declare-function elmo-folder-exists-p "ext:elmo" (folder) t)
(declare-function elmo-message-entity-field "ext:elmo-msgdb"
		  (entity field &optional type))
(declare-function elmo-message-field "ext:elmo"
		  (folder number field &optional type) t)
(declare-function elmo-msgdb-overview-get-entity "ext:elmo" (id msgdb) t)
;; Backward compatibility to old version of wl
(declare-function wl "ext:wl" () t)
(declare-function wl-summary-buffer-msgdb "ext:wl-folder" () t)
(declare-function wl-summary-jump-to-msg-by-message-id "ext:wl-summary"
		  (&optional id))
(declare-function wl-summary-jump-to-msg "ext:wl-summary"
		  (&optional number beg end))
(declare-function wl-summary-line-from "ext:wl-summary" ())
(declare-function wl-summary-line-subject "ext:wl-summary" ())
(declare-function wl-summary-message-number "ext:wl-summary" ())
(declare-function wl-summary-redisplay "ext:wl-summary" (&optional arg))
(declare-function wl-summary-registered-temp-mark "ext:wl-action" (number))
(declare-function wl-folder-goto-folder-subr "ext:wl-folder"
		  (&optional folder sticky))
(declare-function wl-folder-get-petname "ext:wl-folder" (name))
(declare-function wl-folder-get-entity-from-buffer "ext:wl-folder"
		  (&optional getid))
(declare-function wl-folder-buffer-group-p "ext:wl-folder")
(defvar wl-init)
(defvar wl-summary-buffer-elmo-folder)
(defvar wl-summary-buffer-folder-name)
(defvar wl-folder-group-regexp)
(defvar wl-auto-check-folder-name)
(defvar elmo-nntp-default-server)

(defconst org-wl-folder-types
  '(("%" . imap) ("-" . nntp) ("+" . mh) ("." . maildir)
    ("=" . spool) ("$" . archive) ("&" . pop) ("@" . shimbun)
    ("rss" . rss) ("[" . search) ("*" . multi) ("/" . filter)
    ("|" . pipe) ("'" . internal) )
  "List of folder indicators.  See Wanderlust manual, section 3.")

;; Install the link type
(org-link-set-parameters "wl" :follow #'org-wl-open :store #'org-wl-store-link)

;; Implementation

(defun org-wl-folder-type (folder)
  "Return symbol that indicates the type of FOLDER.
FOLDER is the wanderlust folder name.  The first character of the
folder name determines the folder type."
  (let* ((indicator (substring folder 0 1))
	 (type (cdr (assoc indicator org-wl-folder-types))))
    ;; maybe access or file folder
    (when (not type)
      (setq type
	    (cond
	     ((and (>= (length folder) 5)
		   (string= (substring folder 0 5) "file:"))
	      'file)
	     ((and (>= (length folder) 7)
		   (string= (substring folder 0 7) "access:"))
	      'access)
	     (t
	      nil))))
    type))

(defun org-wl-message-field (field entity)
  "Return content of FIELD in ENTITY.
FIELD is a symbol of a rfc822 message header field.
ENTITY is a message entity."
  (let ((content (elmo-message-entity-field entity field 'string)))
    (if (listp content) (car content) content)))

(defun org-wl-store-link ()
  "Store a link to a WL message or folder."
  (unless (eobp)
    (cond
     ((memq major-mode '(wl-summary-mode mime-view-mode))
      (org-wl-store-link-message))
     ((eq major-mode 'wl-folder-mode)
      (org-wl-store-link-folder))
     (t
      nil))))

(defun org-wl-store-link-folder ()
  "Store a link to a WL folder."
  (let* ((folder (wl-folder-get-entity-from-buffer))
	 (petname (wl-folder-get-petname folder))
	 (link (concat "wl:" folder)))
    (save-excursion
      (beginning-of-line)
      (unless (and (wl-folder-buffer-group-p)
		   (looking-at wl-folder-group-regexp))
	(org-store-link-props :type "wl" :description petname
			      :link link)
	link))))

(defun org-wl-store-link-message ()
  "Store a link to a WL message."
  (save-excursion
    (let ((buf (if (eq major-mode 'wl-summary-mode)
		   (current-buffer)
		 (and (boundp 'wl-message-buffer-cur-summary-buffer)
		      wl-message-buffer-cur-summary-buffer))))
      (when buf
	(with-current-buffer buf
	  (let* ((msgnum (wl-summary-message-number))
		 (mark-info (wl-summary-registered-temp-mark msgnum))
		 (folder-name
		  (if (and org-wl-link-to-refile-destination
			   mark-info
			   (equal (nth 1 mark-info) "o")) ; marked as refile
		      (nth 2 mark-info)
		    wl-summary-buffer-folder-name))
		 (folder-type (org-wl-folder-type folder-name))
		 (wl-message-entity
		  (if (fboundp 'elmo-message-entity)
		      (elmo-message-entity
		       wl-summary-buffer-elmo-folder msgnum)
		    (elmo-msgdb-overview-get-entity
		     msgnum (wl-summary-buffer-msgdb))))
		 (message-id
		  (org-wl-message-field 'message-id wl-message-entity))
		 (message-id-no-brackets
		  (org-unbracket-string "<" ">" message-id))
		 (from (org-wl-message-field 'from wl-message-entity))
		 (to (org-wl-message-field 'to wl-message-entity))
		 (xref (org-wl-message-field 'xref wl-message-entity))
		 (subject (org-wl-message-field 'subject wl-message-entity))
		 (date (org-wl-message-field 'date wl-message-entity))
		 desc link)

	    ;; remove text properties of subject string to avoid possible bug
	    ;; when formatting the subject
	    ;; (Emacs bug #5306, fixed)
	    (set-text-properties 0 (length subject) nil subject)

	    ;; maybe remove filter condition
	    (when (and (eq folder-type 'filter) org-wl-link-remove-filter)
	      (while (eq (org-wl-folder-type folder-name) 'filter)
		(setq folder-name
		      (replace-regexp-in-string "^/[^/]+/" "" folder-name))))

	    ;; maybe create http link
	    (cond
	     ((and (eq folder-type 'shimbun)
		   org-wl-shimbun-prefer-web-links xref)
	      (org-store-link-props :type "http" :link xref :description subject
				    :from from :to to :message-id message-id
				    :message-id-no-brackets message-id-no-brackets
				    :subject subject))
	     ((and (eq folder-type 'nntp) org-wl-nntp-prefer-web-links)
	      (setq link
		    (format
		     (if (string-match-p "gmane\\." folder-name)
			 "http://mid.gmane.org/%s"
                       "https://groups.google.com/groups/search?as_umsgid=%s")
		     (url-encode-url message-id)))
	      (org-store-link-props :type "http" :link link :description subject
				    :from from :to to :message-id message-id
				    :message-id-no-brackets message-id-no-brackets
				    :subject subject))
	     (t
	      (org-store-link-props :type "wl" :from from :to to
				    :subject subject :message-id message-id
				    :message-id-no-brackets message-id-no-brackets)
	      (setq desc (org-email-link-description))
	      (setq link (concat "wl:" folder-name "#" message-id-no-brackets))
	      (org-add-link-props :link link :description desc)))
	    (org-add-link-props :date date)
	    (or link xref)))))))

(defun org-wl-open-nntp (path)
  "Follow the nntp: link specified by PATH."
  (let* ((spec (split-string path "/"))
	 (server (split-string (nth 2 spec) "@"))
	 (group (nth 3 spec))
	 (article (nth 4 spec)))
    (org-wl-open
     (concat "-" group ":" (if (cdr server)
			       (car (split-string (car server) ":"))
			     "")
	     (if (string= elmo-nntp-default-server (nth 2 spec))
		 ""
	       (concat "@" (or (cdr server) (car server))))
	     (if article (concat "#" article) "")))))

(defun org-wl-open (path &rest _)
  "Follow the WL message link specified by PATH.
When called with one prefix, open message in namazu search folder
with `org-wl-namazu-default-index' as search index.  When called
with two prefixes or `org-wl-namazu-default-index' is nil, ask
for namazu index."
  (require 'wl)
  (let ((wl-auto-check-folder-name
	 (if org-wl-disable-folder-check
	     'none
	   wl-auto-check-folder-name)))
    (unless wl-init (wl))
    ;; XXX: The imap-uw's MH folder names start with "%#".
    (if (not (string-match "\\`\\(\\(?:%#\\)?[^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in Wanderlust link"))
    (let ((folder (match-string 1 path))
	  (article (match-string 3 path)))
      ;; maybe open message in namazu search folder
      (when current-prefix-arg
	(setq folder (concat "[" article "]"
			     (if (and (equal current-prefix-arg '(4))
				      org-wl-namazu-default-index)
				 org-wl-namazu-default-index
			       (read-directory-name "Namazu index: ")))))
      (if (not (elmo-folder-exists-p (with-no-warnings
				       (wl-folder-get-elmo-folder folder))))
	  (error "No such folder: %s" folder))
      (let ((old-buf (current-buffer))
	    (old-point (point-marker)))
	(wl-folder-goto-folder-subr folder)
	(with-current-buffer old-buf
	  ;; XXX: `wl-folder-goto-folder-subr' moves point to the
	  ;; beginning of the current line.  So, restore the point
	  ;; in the old buffer.
	  (goto-char old-point))
	(when article
	  (if (string-match-p "@" article)
	      (wl-summary-jump-to-msg-by-message-id (org-link-add-angle-brackets
						     article))
	    (or (wl-summary-jump-to-msg (string-to-number article))
		(error "No such message: %s" article)))
	  (wl-summary-redisplay))))))

(provide 'ol-wl)

;;; ol-wl.el ends here
