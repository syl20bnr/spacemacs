;;; org-mu4e -- Support for links to mu4e messages/queries from within org-mode,
;;; and for writing message in org-mode, sending them as rich-text
;;
;; Copyright (C) 2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail
;; Version: 0.0

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

;;; Code:

(require 'org)
(eval-when-compile (require 'org-exp))
(eval-when-compile (require 'cl))
(eval-when-compile (require 'mu4e))

(defun org-mu4e-store-link ()
  "Store a link to a mu4e query or message."
  (cond
    ;; storing links to queries
    ((eq major-mode 'mu4e-headers-mode)
      (let* ((query (mu4e-last-query))
	      desc link)
	(org-store-link-props :type "mu4e" :query query)
	(setq
	  desc (org-make-link "mu4e:query:" query)
	  link desc)
	(org-add-link-props :link link :description desc)
	link))

      ;; storing links to messages
    ((eq major-mode 'mu4e-view-mode)
      (let* ((msg  (mu4e-message-at-point))
	      (msgid   (or (plist-get msg :message-id) "<none>"))
	      (subject (or (plist-get msg :subject) "No subject"))
	      link)
	(org-store-link-props :type "mu4e" :link link
	  :message-id msgid :subject subject)
	(setq link (org-make-link "mu4e:msgid:" msgid))
	(org-add-link-props :link link :description subject)
	link))))

(org-add-link-type "mu4e" 'org-mu4e-open)
(add-hook 'org-store-link-functions 'org-mu4e-store-link)

(defun org-mu4e-open (path)
  "Open the mu4e message (for paths starting with 'msgid:') or run
the query (for paths starting with 'query:')."
  (require 'mu4e)
  (cond
    ((string-match "^msgid:\\(.+\\)" path)
      (mu4e-view-message-with-msgid (match-string 1 path)))
    ((string-match "^query:\\(.+\\)" path)
      (mu4e-headers-search (match-string 1 path) current-prefix-arg))
    (t (message "mu4e: unrecognized link type '%s'" path))))






;;; editing with org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; below, some functions for the org->html conversion
;; based on / inspired by Eric Schulte's org-mime.el
;; Homepage: http://orgmode.org/worg/org-contrib/org-mime.php
(defun org~mu4e-mime-file (ext path id)
  "Create a file for an attachment."
  (format (concat "<#part type=\"%s\" filename=\"%s\" "
	    "disposition=inline id=\"<%s>\">\n<#/part>\n")
    ext path id))

(defun org~mu4e-mime-multipart (plain html &optional images)
  "Create a multipart/alternative with text/plain and text/html alternatives.
If the html portion of the message includes images, wrap the html
and images in a multipart/related part."
  (concat "<#multipart type=alternative><#part type=text/plain>"
    plain
    (when images "<#multipart type=related>")
    "<#part type=text/html>"
    html
    images
    (when images "<#/multipart>\n")
    "<#/multipart>\n"))

(defun org~mu4e-mime-replace-images (str current-file)
  "Replace images in html files with cid links."
  (let (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"cid:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (expand-file-name
                       url (file-name-directory current-file)))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
           (add-to-list 'html-images
                        (org~mu4e-mime-file
			  (concat "image/" ext) path id))
           id)))
      str)
     html-images)))

(defun org~mu4e-mime-convert-to-html ()
  "Convert the current body to html."
  (let* ((begin
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward mail-header-separator)))
	  (end (point-max))
	  (raw-body (buffer-substring begin end))
	  (tmp-file (make-temp-name (expand-file-name "mail"
					 temporary-file-directory)))
	  (body (org-export-string raw-body 'org (file-name-directory tmp-file)))
	  ;; because we probably don't want to skip part of our mail
	  (org-export-skip-text-before-1st-heading nil)
         ;; because we probably don't want to export a huge style file
	  (org-export-htmlize-output-type 'inline-css)
	  ;; makes the replies with ">"s look nicer
	  (org-export-preserve-breaks t)
	  ;; dvipng for inline latex because MathJax doesn't work in mail
	  (org-export-with-LaTeX-fragments 'dvipng)
	  ;; to hold attachments for inline html images
	  (html-and-images
	    (org~mu4e-mime-replace-images
	      (org-export-string raw-body 'html (file-name-directory tmp-file))
	      tmp-file))
	  (html-images (cdr html-and-images))
	  (html (car html-and-images)))
    (delete-region begin end)
    (save-excursion
      (goto-char begin)
      (newline)
      (insert (org~mu4e-mime-multipart
		body html (mapconcat 'identity html-images "\n"))))))

;; next some functions to make the org/mu4e-compose-mode switch as smooth as
;; possible.
(defun org~mu4e-mime-decorate-headers ()
  "Make the headers visually distinctive (org-mode)."
  (save-excursion
    (goto-char (point-min))
    (let* ((eoh (when (search-forward mail-header-separator)
		  (match-end 0)))
	    (olay (make-overlay (point-min) eoh)))
      (when olay
	(overlay-put olay 'face 'font-lock-comment-face)))))

(defun org~mu4e-mime-undecorate-headers ()
  "Don't make the headers visually distinctive (well,
mu4e-compose-mode will take care of that)."
  (save-excursion
    (goto-char (point-min))
    (let* ((eoh (when (search-forward mail-header-separator)
		  (match-end 0))))
      (remove-overlays (point-min) eoh))))

(defvar org-mu4e-convert-to-html nil
  "Wether to an org-mode => html conversion when sending messages.")

(defun org~mu4e-mime-convert-to-html-maybe ()
  "Convert to html if `org-mu4e-convert-to-html' is non-nil. This
function is called when sending a message (from
`message-send-hook') and, if non-nil, will send the message as the
rich-text version of the what is assumed to be an org-mode body."
  (when org-mu4e-convert-to-html
    (message "Converting to html")
    (org~mu4e-mime-convert-to-html)))

(defun org~mu4e-execute-key-sequence-in-compose-mode (keyseq)
  "Execute keysequence KEYSEQ by (temporarily) switching to compose
mode."
  (mu4e-compose-mode)
  (add-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t t)
  (let ((func (lookup-key (current-local-map) keyseq)))
    (unless (functionp func)
      (error "Invalid key binding"))
    (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe t t)
    (funcall func)))

(defun org~mu4e-mime-switch-headers-or-body ()
  "Switch the buffer to either mu4e-compose-mode (when in headers)
or org-mode (when in the body),"
  (interactive)
  (let* ((sepapoint
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward-regexp mail-header-separator nil t))))
    ;; only do stuff when the sepapoint exist; note that after sending the
    ;; message, this function maybe called on a message with the sepapoint
    ;; stripped. This is why we don't use `message-point-in-header'.
    (when sepapoint
      (cond
	;; we're in the body, but in mu4e-compose-mode?
	;; if so, switch to org-mode
	((and (> (point) sepapoint) (eq major-mode 'mu4e-compose-mode))
	  (org-mode)
	  (add-hook 'before-save-hook
	    (lambda ()
	      (error "Switch to mu4e-compose-mode (M-m) before saving.")) nil t)
	  (org~mu4e-mime-decorate-headers)
	  (local-set-key (kbd "M-m")
	    (lambda (key)
	      (interactive "kEnter mu4e-compose-mode key sequence: ")
	      (org~mu4e-execute-key-sequence-in-compose-mode key))))
	;; we're in the headers, but in org-mode?
	;; if so, switch to mu4e-compose-mode
	((and (<= (point) sepapoint) (eq major-mode 'org-mode))
      	  (org~mu4e-mime-undecorate-headers)
	  (mu4e-compose-mode)
	  (add-hook 'message-send-hook
	    'org~mu4e-mime-convert-to-html-maybe nil t)))
      ;; and add the hook
      (add-hook 'post-command-hook
	'org~mu4e-mime-switch-headers-or-body t t))))


(defun org-mu4e-compose-org-mode ()
  "Pseudo-Minor mode for mu4e-compose-mode, to edit the message
    body using org-mode."
  (interactive)
  (unless (member major-mode '(org-mode mu4e-compose-mode))
    (error "Need org-mode or mu4e-compose-mode"))
  (unless (executable-find "dvipng")
    (error "Required program dvipng not found"))
  ;; we can check if we're already in mu4e-compose-mode by checking
  ;; if the post-command-hook is set; hackish...
  (if (not (member 'org~mu4e-mime-switch-headers-or-body post-command-hook))
    (progn
      (org~mu4e-mime-switch-headers-or-body)
      (message
	(concat
	  "org-mu4e-compose-org-mode enabled; "
	  "press M-m before issuing message-mode commands")))
    (progn ;; otherwise, remove crap
      (remove-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t)
      (org~mu4e-mime-undecorate-headers) ;; shut off org-mode stuff
      (mu4e-compose-mode)
      (message "org-mu4e-compose-org-mode disabled"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-mu4e)
;;; org-mu4e.el ends here
