;;; mu4e-vars.el -- part of mu4e, the mu mail user agent
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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
(require 'mu4e-meta)

(defgroup mu4e nil
  "mu4e - mu for emacs"
  :group 'local)

(defcustom mu4e-mu-home nil
  "Location of the mu homedir, or nil for the default."
  :type 'directory
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-mu-binary (executable-find "mu")
  "Name of the mu-binary to use; if it cannot be found in your
PATH, you can specify the full path."
  :type 'file
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-maildir (expand-file-name "~/Maildir")
  "Your Maildir directory; by default, mu4e assumes
~/Maildir."
  :type 'directory
  :safe 'stringp
  :group 'mu4e)

(defcustom mu4e-get-mail-command nil
  "Shell command to run to retrieve new mail; e.g. 'offlineimap' or
'fetchmail'. Note, when there is no mail, fetchmail will return 1
as it error code, which mu4e interprets as an error."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-update-interval nil
  "Number of seconds between automatic calls to retrieve mail and
update the database. If nil, don't update automatically. Note,
changes in `mu4e-update-interval' only take effect after restarting
mu4e."
  :type 'integer
  :group 'mu4e
  :safe 'integerp)

(defcustom mu4e-attachment-dir (expand-file-name "~/")
  "Default directory for saving attachments."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-user-mail-address-regexp "$^"
  "Regular expression matching the user's mail address(es). This is
used to distinguish ourselves from others, e.g. when replying and
in :from-or-to headers. By default, match nothing."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-my-email-addresses `(,user-mail-address)
  "List of e-mail addresses to consider 'my email addresses',
ie. addresses whose presence in an email imply that it is a
personal message. This is used when indexing messages."
  :type '(string)
  :group 'mu4e)

(defvar mu4e-date-format-long "%c"
  "Date format to use in the message view, in the format of
  `format-time-string'.")

(defvar mu4e-search-results-limit 500
  "Maximum number of search results (or -1 for unlimited). Since
limiting search results speeds up searches significantly, it's
useful to limit this. Note, to ignore the limit, use a prefix
argument (C-u) before invoking the search.")

(defvar mu4e-debug nil
  "When set to non-nil, log debug information to the *mu4e-log* buffer.")

(defvar mu4e-bookmarks
  '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
     ("date:today..now"                  "Today's messages"     ?t)
     ("date:7d..now"                     "Last 7 days"          ?w)
     ("mime:image/*"                     "Messages with images" ?p))
  "A list of pre-defined queries; these will show up in the main
screen. Each of the list elements is a three-element list of the
form (QUERY DESCRIPTION KEY), where QUERY is a string with a mu
query, DESCRIPTION is a short description of the query (this will
show up in the UI), and KEY is a shortcut key for the query.")

(defvar mu4e-split-view 'horizontal
  "How to show messages / headers; a symbol which is either: * a
symbol 'horizontal: split horizontally (headers on top) * a symbol
'vertical: split vertically (headers on the left).  * anything
else: don't split (show either headers or messages, not both) Also
see `mu4e-headers-visible-lines' and
`mu4e-headers-visible-columns'.")

;; completion; we put them here rather than in mu4e-compose, as mu4e-utils needs
;; the variables.

(defgroup mu4e-compose nil
  "Message-composition related settings."
  :group 'mu4e)

;; address completion
(defcustom mu4e-compose-complete-addresses t
  "Whether to do auto-completion of e-mail addresses."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-personal nil
  "Whether to consider only 'personal' e-mail addresses,
i.e. addresses from messages where user was explicitly in one of
the address fields (this excludes mailing list messages)."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-after "2010-01-01"
  "Consider only contacts last seen after this date. Date must be a
  string, in a format parseable by `org-parse-time-string'. This
  excludes really old contacts. Set to nil to not have any
  time-based restriction."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-ignore-address-regexp "noreply"
  "Ignore any e-mail addresses for completion if they match this
regexp."
  :type 'string
  :group 'mu4e-compose)



;; Folders
(defgroup mu4e-folders nil
  "Special folders."
  :group 'mu4e)

(defcustom mu4e-sent-folder "/sent"
  "Your folder for sent messages, relative to `mu4e-maildir',
  e.g. \"/Sent Items\"."
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)

(defcustom mu4e-drafts-folder "/drafts"
  "Your folder for draft messages, relative to `mu4e-maildir',
  e.g. \"/drafts\""
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)

(defcustom mu4e-trash-folder "/trash"
  "Your folder for trashed messages, relative to `mu4e-maildir',
  e.g. \"/trash\"."
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)


(defcustom mu4e-maildir-shortcuts nil
  "A list of maildir shortcuts to enable quickly going to the
 particular for, or quickly moving messages towards them (i.e.,
 archiving or refiling). The list contains elements of the form
 (maildir . shortcut), where MAILDIR is a maildir (such as
\"/archive/\"), and shortcut a single shortcut character. With
this, in the header buffer and view buffer you can execute
`mu4e-mark-for-move-quick' (or 'm', by default) or
`mu4e-jump-to-maildir' (or 'j', by default), followed by the
designated shortcut character for the maildir.")



;; Faces
(defgroup mu4e-faces nil
  "Type faces (fonts) used in mu4e."
  :group 'mu4e
  :group 'faces)


(defface mu4e-unread-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for an unread message header."
  :group 'mu4e-faces)

(defface mu4e-moved-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for a message header that has been moved to some
folder (it's still visible in the search results, since we cannot
be sure it no longer matches)."
  :group 'mu4e-faces)

(defface mu4e-trashed-face
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for an message header in the trash folder."
  :group 'mu4e-faces)

(defface mu4e-draft-face
  '((t :inherit font-lock-string-face))
  "Face for a draft message header (i.e., a message with the draft
flag set)."
  :group 'mu4e-faces)

(defface mu4e-flagged-face
  '((t :inherit font-lock-builtin-face :bold t))
  "Face for a flagged message header."
  :group 'mu4e-faces)

(defface mu4e-header-face
  '((t :inherit default))
  "Face for a header without any special flags."
  :group 'mu4e-faces)

(defface mu4e-header-title-face
  '((t :inherit font-lock-type-face))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-header-highlight-face
  '((t :inherit default :weight bold :underline t))
  "Face for the header at point."
  :group 'mu4e-faces)

(defface mu4e-header-marks-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the mark in the headers list."
  :group 'mu4e-faces)

(defface mu4e-view-header-key-face
  '((t :inherit font-lock-builtin-face :bold t))
  "Face for a header key (such as \"Foo\" in \"Subject:\ Foo\") in
  the message view."
  :group 'mu4e-faces)

(defface mu4e-view-header-value-face
  '((t :inherit font-lock-doc-face))
  "Face for a header value (such as \"Re: Hello!\") in the message
  view."
  :group 'mu4e-faces)

(defface mu4e-view-link-face
  '((t :inherit font-lock-type-face :underline t))
  "Face for showing URLs and attachments in the message view."
  :group 'mu4e-faces)

(defface mu4e-highlight-face
  '((t :inherit font-lock-pseudo-keyword-face :bold t))
  "Face for highlighting things."
  :group 'mu4e-faces)

(defface mu4e-title-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-footer-face
  '((t :inherit font-lock-comment-face))
  "Face for message footers (signatures)."
  :group 'mu4e-faces)

(defface mu4e-view-url-number-face
  '((t :inherit font-lock-reference-face :bold t))
  "Face for the number tags for URLs."
  :group 'mu4e-faces)

(defface mu4e-view-attach-number-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for the number tags for attachments."
  :group 'mu4e-faces)

(defface mu4e-cited-1-face
  '((t :inherit font-lock-builtin-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'mu4e-faces)

(defface mu4e-cited-2-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'mu4e-faces)

(defface mu4e-cited-3-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'mu4e-faces)

(defface mu4e-cited-4-face
  '((t :inherit font-lock-pseudo-keyword-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'mu4e-faces)

(defface mu4e-cited-5-face
  '((t :inherit font-lock-comment-face :bold nil :italic t))
  "Face for cited message parts (level 5)."
  :group 'mu4e-faces)

(defface mu4e-cited-6-face
  '((t :inherit font-lock-comment-delimiter-face :bold nil :italic t))
  "Face for cited message parts (level 6)."
  :group 'mu4e-faces)

(defface mu4e-cited-7-face
  '((t :inherit font-lock-preprocessor-face :bold nil :italic t))
  "Face for cited message parts (level 7)."
  :group 'mu4e-faces)




(defface mu4e-system-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for system message (such as the footers for message
headers)."
  :group 'mu4e-faces)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal variables / constants

(defconst mu4e-header-names
  '( (:attachments   .  "Attach")
     (:bcc           .  "Bcc")
     (:cc            .  "Cc")
     (:date          .  "Date")
     (:flags         .  "Flags")
     (:from          .  "From")
     (:from-or-to    .  "From/To")
     (:maildir       .  "Maildir")
     (:path          .  "Path")
     (:subject       .  "Subject")
     (:to            .  "To"))
"An alist of all possible header fields; this is used in the UI (the
column headers in the header list, and the fields the message
view). Most fields should be self-explanatory. A special one is
`:from-or-to', which is equal to `:from' unless `:from' matches
`mu4e-user-mail-address-regexp', in which case it will be equal to
`:to'.")


(defconst mu4e-logo
  (propertize "mu4e" 'face 'mu4e-title-face)
  "A propertized string for the mu4e 'logo'.")

(defconst mu4e-prefix
  (concat "[" mu4e-logo "]")
  "Prefix for mu4e minibuffer input.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal variables / constants

(defconst mu4e-header-names
  '( (:attachments   .  "Attach")
     (:bcc           .  "Bcc")
     (:cc            .  "Cc")
     (:date          .  "Date")
     (:flags         .  "Flags")
     (:from          .  "From")
     (:from-or-to    .  "From/To")
     (:maildir       .  "Maildir")
     (:path          .  "Path")
     (:subject       .  "Subject")
     (:to            .  "To"))
"An alist of all possible header fields; this is used in the UI (the
column headers in the header list, and the fields the message
view). Most fields should be self-explanatory. A special one is
`:from-or-to', which is equal to `:from' unless `:from' matches
`mu4e-user-mail-address-regexp', in which case it will be equal to
`:to'.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time vars used in multiple places

;; headers
(defconst mu4e~headers-buffer-name "*mu4e-headers*"
  "Name of the buffer for message headers.")
(defvar mu4e~headers-buffer nil "Buffer for message headers")
; view
(defconst mu4e~view-buffer-name "*mu4e-view*"
  "Name for the message view buffer")
(defvar mu4e~view-buffer nil "The view buffer.")

(defvar mu4e~view-msg nil "The message being viewed in view mode.")

(defvar mu4e~contacts-for-completion nil
  "List of contacts (ie. 'name <e-mail>'),
used by the completion functions in mu4e-compose, and filled when
mu4e starts.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; our handlers funcs
;; these handler funcs define what happens when we receive a certain message
;; from the server
(defun mu4e~default-handler (&rest args)
  "*internal* Dummy handler function."
  (error "Not handled: %S" args))

(defvar mu4e-error-func 'mu4e~default-handler
  "A function called for each error returned from the server
process; the function is passed an error plist as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-update-func 'mu4e~default-handler
  "A function called for each :update sexp returned from the server
process; the function is passed a msg sexp as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-remove-func  'mu4e~default-handler
  "A function called for each :remove sexp returned from the server
process, when some message has been deleted. The function is passed
the docid of the removed message.")

(defvar mu4e-sent-func  'mu4e~default-handler
  "A function called for each :sent sexp returned from the server
process, when some message has been sent. The function is passed
the docid and the draft-path of the sent message.")

(defvar mu4e-view-func  'mu4e~default-handler
  "A function called for each single message sexp returned from the
server process. The function is passed a message sexp as
argument. See `mu4e~proc-filter' for the format.")

(defvar mu4e-header-func  'mu4e~default-handler
  "A function called for each message returned from the server
process; the function is passed a msg plist as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-found-func  'mu4e~default-handler
  "A function called for when we received a :found sexp after the
headers have returns, to report on the number of matches. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-erase-func 'mu4e~default-handler
  "A function called for when we received an :erase sexp after the
headers have returns, to clear the current headers buffer. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-compose-func  'mu4e~default-handler
  "A function called for each message returned from the server
process that is used as basis for composing a new message (ie.,
either a reply or a forward); the function is passed msg and a
symbol (either reply or forward). See `mu4e~proc-filter' for the
format of <msg-plist>.")

(defvar mu4e-info-func  'mu4e~default-handler
  "A function called for each (:info type ....) sexp received from
the server process.")

(defvar mu4e-pong-func 'mu4e~default-handler
  "A function called for each (:pong type ....) sexp received from
the server process.")

(defvar mu4e-contacts-func 'mu4e~default-handler
  "A function called for each (:contacts (<list-of-contacts>) sexp
received from the server process.")

(defvar mu4e-temp-func 'mu4e~default-handler
  "A function called for each (:temp <file> <cookie>) sexp received
from the server process.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-vars)
;;; End of mu4e-vars.el
