;;; mu4e-message.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2012 Dirk-Jan C. Binnema

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

;; Functions to get data from mu4e-message plist structure

;;; Code:
(eval-when-compile (byte-compile-disable-warning 'cl-functions))

(require 'mu4e-vars)
(require 'mu4e-utils)

(require 'cl)
(require 'html2text)


(defcustom mu4e-html2text-command nil
  "Shell command that converts from html to plain text.
The command has to read html from stdin and output plain text on
stdout. If this is not defined, the emacs `html2text' tool will
be used when faced with html-only message. If you use htmltext,
it's recommended you use \"html2text -utf8 -width 72\"."
  :type 'string
  :group 'mu4e-view
  :safe 'stringp)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the html-version.
If the e-mail message has no html-version the plain-text version
is always used."
  :type 'boolean
  :group 'mu4e-view)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst mu4e-message-field-raw (msg field)
  "Retrieve FIELD from message plist MSG.
FIELD is one of :from, :to, :cc, :bcc, :subject, :data,
:message-id, :path, :maildir, :priority, :attachments,
:references, :in-reply-to, :body-txt, :body-html

Returns `nil' if the field does not exist.

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
Some notes on the format:
- The address fields are lists of pairs (NAME . EMAIL), where NAME can be nil.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments, :body-txt or :body-html fields. Message in the
  Message view use the actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (if msg
    (plist-get msg field)
    (mu4e-error "message must be non-nil")))

(defsubst mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG.
Like `mu4e-message-field-nil', but will sanitize `nil' values:
- all string field except body-txt/body-html: nil -> \"\"
- numeric fields + dates                    : nil -> 0
- all others                                : return the value
Thus, function will return nil for empty lists, non-existing body-txt or body-html."
  (let ((val (mu4e-message-field-raw msg field)))
    (cond
      (val
	val)   ;; non-nil -> just return it
      ((member field '(:subject :message-id :path :maildir :in-reply-to))
	"")    ;; string fields except body-txt, body-html: nil -> ""
      ((member field '(:body-html :body-txt))
	val)
      ((member field '(:docid :size))
	0)     ;; numeric type: nil -> 0
      (t 
	val)))) ;; otherwise, just return nil

(defsubst mu4e-message-has-field (msg field)
  "Return t if MSG contains FIELD, nil otherwise."
  (plist-member msg field))

(defsubst mu4e-message-at-point (&optional noerror)
  "Get the message s-expression for the message at point in either
the headers buffer or the view buffer, or nil if there is no such
message. If optional NOERROR is non-nil, do not raise an error when
there is no message at point."
  (let ((msg (or (get-text-property (point) 'msg) mu4e~view-msg))) 
    (if msg
      msg
      (unless noerror (mu4e-warn "No message at point")))))

(defsubst mu4e-message-field-at-point (field)
  "Get the field FIELD from the message at point.
This is equivalent to:
  (mu4e-message-field (mu4e-message-at-point) FIELD)."
  (mu4e-message-field (mu4e-message-at-point) field))

(defun mu4e-message-body-text (msg)
  "Get the body in text form for this message.
This is either :body-txt, or if not available, :body-html
converted to text. By default, it uses the emacs built-in
`html2text'. Alternatively, if `mu4e-html2text-command' is
non-nil, it will use that. Normally, function prefers the text
part, but this can be changed by setting
`mu4e-view-prefer-html'."
  (let* ((txt (mu4e-message-field msg :body-txt))
	  (html (mu4e-message-field msg :body-html))
	  (body
	    (cond
	      ;; does it look like some text? ie., 10x the length of the text
	      ;; should be longer than the html, an heuristic to guard against
	      ;; 'This messages requires html' text bodies.
	      ((and (> (* 10 (length txt)) (length html))
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
      (while (re-search-forward "[ ’]" nil t)
	(replace-match
	  (cond
	    ((string= (match-string 0) "’") "'")
	    (t		                       ""))))
      (buffer-string))))

(defun mu4e-message-contact-field-matches (msg cfield rx)
  "Checks whether any of the of the contacts in field
CFIELD (either :to, :from, :cc or :bcc) of msg MSG matches (with
their name or e-mail address) regular expressions RX. If there is a
match, return non-nil; otherwise return nil. RX can also be a list
of regular expressions, in which case any of those are tried for a
match."
  (unless (member cfield '(:to :from :bcc :cc))
    (mu4e-error "Not a contacts field (%S)" cfield))
  (if (listp rx)
    ;; if rx is a list, try each one of them for a match
    (find-if
      (lambda (a-rx) (mu4e-message-contact-field-matches msg cfield a-rx))
      rx)
    ;; not a list, check the rx
    (find-if
      (lambda (ct)
	(let ((name (car ct)) (email (cdr ct)))
	  (or
	    (and name  (string-match rx name))
	    (and email (string-match rx email)))))
      (mu4e-message-field msg cfield)))) 


(defsubst mu4e-message-part-field  (msgpart field)
  "Get some field in a message part; a part would look something like:
  (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get msgpart field))



;; backward compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'mu4e-msg-field 'mu4e-message-field)
(defalias 'mu4e-body-text 'mu4e-message-body-text) ;; backward compatibility

(defun mu4e-field-at-point (field)
  "Get FIELD (a symbol, see `mu4e-header-info') for the message at
point in eiter the headers buffer or the view buffer."
  (plist-get (mu4e-message-at-point) field))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mu4e-message)
