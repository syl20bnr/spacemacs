;;; ol-vm.el --- Links to VM messages

;; Copyright (C) 2004-2021 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;;
;; Support for IMAP folders added
;; by Konrad Hinsen <konrad dot hinsen at fastmail dot net>
;; Requires VM 8.2.0a or later.
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
;; This file implements links to VM messages and folders from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'ol)
(require 'org)

;; Declare external functions and variables
(declare-function vm-preview-current-message "ext:vm-page" ())
(declare-function vm-follow-summary-cursor "ext:vm-motion" ())
(declare-function vm-get-header-contents "ext:vm-summary"
		  (message header-name-regexp &optional clump-sep))
(declare-function vm-isearch-narrow "ext:vm-search" ())
(declare-function vm-isearch-update "ext:vm-search" ())
(declare-function vm-select-folder-buffer "ext:vm-macro" ())
(declare-function vm-su-message-id "ext:vm-summary" (m))
(declare-function vm-su-subject "ext:vm-summary" (m))
(declare-function vm-summarize "ext:vm-summary" (&optional display raise))
(declare-function vm-imap-folder-p "ext:vm-save" ())
(declare-function vm-imap-find-spec-for-buffer "ext:vm-imap" (buffer))
(declare-function vm-imap-folder-for-spec "ext:vm-imap" (spec))
(declare-function vm-imap-parse-spec-to-list "ext:vm-imap" (spec))
(declare-function vm-imap-spec-for-account "ext:vm-imap" (account))
(defvar vm-message-pointer)
(defvar vm-folder-directory)

;; Install the link type
(org-link-set-parameters "vm" :follow #'org-vm-open :store #'org-vm-store-link)
(org-link-set-parameters "vm-imap" :follow #'org-vm-imap-open)

;; Implementation
(defun org-vm-store-link ()
  "Store a link to a VM folder or message."
  (when (and (or (eq major-mode 'vm-summary-mode)
		 (eq major-mode 'vm-presentation-mode))
	     (save-window-excursion
	       (vm-select-folder-buffer) buffer-file-name))
    (and (eq major-mode 'vm-presentation-mode) (vm-summarize))
    (vm-follow-summary-cursor)
    (save-excursion
      (vm-select-folder-buffer)
      (let* ((message (car vm-message-pointer))
  	     (subject (vm-su-subject message))
	     (to (vm-get-header-contents message "To"))
	     (from (vm-get-header-contents message "From"))
             (message-id (vm-su-message-id message))
             (link-type (if (vm-imap-folder-p) "vm-imap" "vm"))
	     (date (vm-get-header-contents message "Date"))
	     folder desc link)
        (if (vm-imap-folder-p)
	    (let ((spec (vm-imap-find-spec-for-buffer (current-buffer))))
	      (setq folder (vm-imap-folder-for-spec spec)))
          (progn
            (setq folder (abbreviate-file-name buffer-file-name))
            (if (and vm-folder-directory
                     (string-match (concat "^" (regexp-quote vm-folder-directory))
                                   folder))
                (setq folder (replace-match "" t t folder)))))
        (setq message-id (org-unbracket-string "<" ">" message-id))
	(org-store-link-props :type link-type :from from :to to :subject subject
			      :message-id message-id :date date)
	(setq desc (org-email-link-description))
	(setq link (concat (concat link-type ":") folder "#" message-id))
	(org-add-link-props :link link :description desc)
	link))))

(defun org-vm-open (path _)
  "Follow a VM message link specified by PATH."
  (let (folder article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in VM link"))
    (setq folder (match-string 1 path)
	  article (match-string 3 path))
    ;; The prefix argument will be interpreted as read-only
    (org-vm-follow-link folder article current-prefix-arg)))

(defun org-vm-follow-link (&optional folder article readonly)
  "Follow a VM link to FOLDER and ARTICLE."
  (require 'vm)
  (setq article (org-link-add-angle-brackets article))
  (if (string-match "^//\\([a-zA-Z]+@\\)?\\([^:]+\\):\\(.*\\)" folder)
      ;; ange-ftp or efs or tramp access
      (let ((user (or (match-string 1 folder) (user-login-name)))
	    (host (match-string 2 folder))
	    (file (match-string 3 folder)))
	(cond
	 ((featurep 'tramp)
	  ;; use tramp to access the file
	  (setq folder (format "/%s@%s:%s" user host file)))
	 (t
	  ;; use ange-ftp or efs
	  (require 'ange-ftp)
	  (setq folder (format "/%s@%s:%s" user host file))))))
  (when folder
    (funcall (cdr (assq 'vm org-link-frame-setup)) folder readonly)
    (when article
      (org-vm-select-message (org-link-add-angle-brackets article)))))

(defun org-vm-imap-open (path _)
  "Follow a VM link to an IMAP folder."
  (require 'vm-imap)
  (when (string-match "\\([^:]+\\):\\([^#]+\\)#?\\(.+\\)?" path)
    (let* ((account-name (match-string 1 path))
           (mailbox-name (match-string 2 path))
           (message-id  (match-string 3 path))
           (account-spec (vm-imap-parse-spec-to-list
                          (vm-imap-spec-for-account account-name)))
           (mailbox-spec (mapconcat 'identity
                                    (append (butlast account-spec 4)
                                            (cons mailbox-name
                                                  (last account-spec 3)))
                                    ":")))
      (funcall (cdr (assq 'vm-imap org-link-frame-setup))
               mailbox-spec)
      (when message-id
        (org-vm-select-message (org-link-add-angle-brackets message-id))))))

(defun org-vm-select-message (message-id)
  "Go to the message with message-id in the current folder."
  (require 'vm-search)
  (sit-for 0.1)
  (vm-select-folder-buffer)
  (widen)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (if (not (re-search-forward
              (concat "^" "message-id:\\s-*" (regexp-quote message-id))))
        (error "Could not find the specified message in this folder"))
    (vm-isearch-update)
    (vm-isearch-narrow)
    (vm-preview-current-message)
    (vm-summarize)))

(provide 'ol-vm)

;;; ol-vm.el ends here
