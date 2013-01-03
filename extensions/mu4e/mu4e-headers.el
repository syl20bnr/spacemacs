;;; mu4e-headers.el -- part of mu4e, the mu mail user agent
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

;; In this file are function related mu4e-headers-mode, to creating the list of
;; one-line descriptions of emails, aka 'headers' (not to be confused with
;; headers like 'To:' or 'Subject:')

;; Code:
(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'fringe)
(require 'hl-line)

(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-proc)
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-compose)
(require 'mu4e-actions)

;; the headers view
(defgroup mu4e-headers nil
  "Settings for the headers view."
  :group 'mu4e)

(defcustom mu4e-headers-fields
  '( (:date          .  25)
     (:flags         .   6)
     (:from          .  22)
     (:subject       .  nil))
  "A list of header fields to show in the headers buffer, and their
respective widths in characters. A width of `nil' means
'unrestricted', and this is best reserved fo the rightmost (last)
field. For the complete list of available headers, see
`mu4e-header-names'"
  :type (list 'symbol)
  :group 'mu4e-headers)

(defcustom mu4e-headers-date-format "%x %X"
  "Date format to use in the headers view, in the format of
  `format-time-string'."
  :type  'string
  :group 'mu4e-headers)

(defcustom mu4e-headers-visible-lines 10
  "Number of lines to display in the header view when using the
horizontal split-view. This includes the header-line at the top,
and the mode-line."
  :type 'integer
  :group 'mu4e-headers)


(defcustom mu4e-headers-visible-columns 30
  "Number of columns to display for the header view when using the
vertical split-view."
  :type 'integer
  :group 'mu4e-headers)

(defvar mu4e-headers-actions
  '( ("capture message" . mu4e-action-capture-message))
  "List of actions to perform on messages in the headers list. The actions
are of the form:
   (NAME SHORTCUT FUNC) where:
* NAME is the name of the action (e.g. \"Count lines\")
* SHORTCUT is a one-character shortcut to call this action
* FUNC is a function which receives a message plist as an argument.")

(defvar mu4e-headers-custom-markers
  '(("Older than"
      (lambda (msg date) (time-less-p (mu4e-msg-field msg :date) date))
      (lambda () (mu4e-get-time-date "Match messages before: ")))
     ("Newer than"
       (lambda (msg date) (time-less-p date (mu4e-msg-field msg :date)))
       (lambda () (mu4e-get-time-date "Match messages after: ")))
     ("Bigger than"
       (lambda (msg bytes) (> (mu4e-msg-field msg :size) (* 1024 bytes)))
       (lambda () (read-number "Match messages bigger than (Kbytes): "))))
  "List of custom markers -- functions to mark message that match
some custom function. Each of the list members has the following format:
  (NAME PREDICATE-FUNC PARAM-FUNC)
* NAME is the name of the predicate function, and the first character
is the shortcut (so keep those unique).
* PREDICATE-FUNC is a function that takes two parameters, MSG and (optionally) PARAM,
and should return non-nil when there's a match.
* PARAM-FUNC is function that is evaluated once, and its value is then passed to
PREDICATE-FUNC as PARAM. This is useful for getting user-input.")

(defvar mu4e-headers-sortfield 'date
  "Field to sort the headers by. Field must be a symbol, one of:
     date, subject, size, prio, from, to.")

(defvar mu4e-headers-sort-revert t
  "Whether to revert the sort-order, i.e. Z->A instead of A-Z. When
  sorting by date, it's useful to go from biggest to smallest, so
  newest messages come first.")

(defvar mu4e-headers-show-threads t
  "Whether to show threads in the headers list.")

(defvar mu4e-headers-full-search nil
  "Whether to show all results (or just up to
  `mu4e-search-results-limit')")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; docid cookies
(defconst mu4e~headers-docid-pre "\376"
  "Each header starts (invisibly) with the `mu4e~headers-docid-pre',
  followed by the docid, followed by `mu4e~headers-docid-post'.")
(defconst mu4e~headers-docid-post "\377"
  "Each header starts (invisibly) with the `mu4e~headers-docid-pre',
  followed by the docid, followed by `mu4e~headers-docid-post'.")

(defvar mu4e~headers-view-win nil
  "The view window connected to this headers view.")
 
(defvar mu4e~headers-sortfield-choices
  '( ("date"	. date)
     ("from"	. from)
     ("prio"	. prio)
     ("zsize"	. size)
     ("subject"	. subject)
     ("to"	. to))
  "List of cells describing the various sort-options (in the format
  needed for `mu4e-read-option'.")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e~headers-clear ()
  "Clear the header buffer and related data structures."
  (when (buffer-live-p mu4e~headers-buffer)
    (let ((inhibit-read-only t))
      (with-current-buffer mu4e~headers-buffer
	(erase-buffer)
(mu4e~mark-clear)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler functions
;;
;; next are a bunch of handler functions; those will be called from mu4e~proc in
;; response to output from the server process


(defun mu4e~headers-view-handler (msg)
  "Handler function for displaying a message."
  (mu4e-view msg mu4e~headers-buffer))

(defun mu4e~headers-update-handler (msg is-move)
  "Update handler, will be called when a message has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer mu4e~headers-buffer
      (let* ((docid (plist-get msg :docid))
 	      (point (mu4e~headers-docid-pos docid)))
	(when point ;; is the message present in this list?

	  ;; if it's marked, unmark it now
	  (when (mu4e-mark-docid-marked-p docid)
	    (mu4e-mark-set 'unmark))

 	  ;; re-use the thread info from the old one; this is needed because
 	  ;; *update* messages don't have thread info by themselves (unlike
 	  ;; search results)
	  ;; since we still have the search results, re-use
 	  ;; those
 	  (plist-put msg :thread
 	    (mu4e~headers-field-for-docid docid :thread))

	  ;; first, remove the old one (otherwise, we'd have two headers with
	  ;; the same docid...
	  (mu4e~headers-remove-handler docid)

	  ;; if we we're actually viewing this message (in mu4e-view mode), we
	  ;; update it; that way, the flags can be updated, as well as the path
	  ;; (which is useful for viewing the raw message)
	  (let ((viewbuf (get-buffer mu4e~view-buffer-name)))
	    (when (and viewbuf (buffer-live-p viewbuf))
	      (with-current-buffer viewbuf
		(when (eq docid (plist-get mu4e~view-msg :docid))
		  (mu4e-view msg mu4e~headers-buffer)))))

	  ;; now, if this update was about *moving* a message, we don't show it
	  ;; anymore (of course, we cannot be sure if the message really no
	  ;; longer matches the query, but this seem a good heuristic.
	  ;; if it was only a flag-change, show the message with its updated flags.
	  (unless is-move
	    (mu4e~headers-header-handler msg point))

	  ;; attempt to highlight the corresponding line and make it visible
	  (mu4e~headers-highlight docid))))))


(defun mu4e~headers-remove-handler (docid)
  "Remove handler, will be called when a message with DOCID has
been removed from the database. This function will hide the removed
message from the current list of headers. If the message is not
present, don't do anything."
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer mu4e~headers-buffer
      (mu4e~headers-remove-header docid t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e~headers-contact-str (contacts)
  "Turn the list of contacts CONTACTS (with elements (NAME . EMAIL)
into a string."
  (mapconcat
    (lambda (ct)
      (let ((name (car ct)) (email (cdr ct)))
	(or name email "?"))) contacts ", "))

(defun mu4e~headers-thread-prefix (thread)
  "Calculate the thread prefix based on thread info THREAD."
  (if thread
    (let ( (level        (plist-get thread :level))
	   (first-child  (plist-get thread :first-child))
	   (has-child    (plist-get thread :has-child))
	   (duplicate    (plist-get thread :duplicate))
	   (empty-parent (plist-get thread :empty-parent)))
      (concat
	(make-string (* (if empty-parent 0 2) level) ?\s)
	(cond
	  (has-child    "+ ")
	  (empty-parent "- ")
	  (first-child  "\\ ")
	  (duplicate    "= ")
	  (t            "| "))))))

(defconst mu4e-headers-from-or-to-prefix '("" . "To ")
  "Prefix for the :from-or-to field when it is showing,
  respectively, From: or To:. It's a cons cell with the car element
  being the From: prefix, the cdr element the To: prefix.")

(defun mu4e~headers-header-handler (msg &optional point)
  "Create a one line description of MSG in this buffer, at POINT,
if provided, or at the end of the buffer otherwise."
  (when (buffer-live-p mu4e~headers-buffer)
  (let* ((docid (plist-get msg :docid))
	  (line
	   (mapconcat
	     (lambda (f-w)
	       (let* ((field (car f-w)) (width (cdr f-w))
		       (val (plist-get msg field))
		       (str
			 (case field
			   (:subject
			     (concat ;; prefix subject with a thread indicator
			       (mu4e~headers-thread-prefix (plist-get msg :thread))
			       ;;  "["(plist-get (plist-get msg :thread) :path) "] "
			       val))
			   ((:maildir :path) val)
			   ((:to :from :cc :bcc) (mu4e~headers-contact-str val))
			   ;; if we (ie. `user-mail-address' is the 'From', show
			   ;; 'To', otherwise show From
			   (:from-or-to
			     (let* ((from-lst (plist-get msg :from))
				     (from (and from-lst (cdar from-lst))))
			       (if (and from
				     (string-match mu4e-user-mail-address-regexp
				       from))
				 (concat (or (cdr-safe
					       mu4e-headers-from-or-to-prefix))
				   (mu4e~headers-contact-str (plist-get msg :to)))
				 (concat  (or (car-safe
						mu4e-headers-from-or-to-prefix))
				   (mu4e~headers-contact-str from-lst)))))
			   (:date (format-time-string mu4e-headers-date-format val))
			   (:flags (mu4e-flags-to-string val))
			   (:size (mu4e-display-size val))
			   (t (error "Unsupported header field (%S)" field)))))
		 (when str
		   (if (not width)
		     str
		     (truncate-string-to-width str width 0 ?\s t)))))
	     mu4e-headers-fields " "))
	  (flags (plist-get msg :flags))
	  (line	(cond
		  ((member 'draft flags)
		    (propertize line 'face 'mu4e-draft-face))
		  ((member 'trashed flags)
		    (propertize line 'face 'mu4e-trashed-face))
		  ((member 'unread flags)
		    (propertize line 'face 'mu4e-unread-face))
		  ((member 'flagged flags)
		    (propertize line 'face 'mu4e-flagged-face))
		  (t ;; else
		    (propertize line 'face 'mu4e-header-face)))))

    ;; now, append the header line
    (mu4e~headers-add-header line docid point msg))))

(defun mu4e~headers-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (when (buffer-live-p mu4e~headers-buffer)
  (with-current-buffer mu4e~headers-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t)
	     (str (if (= 0 count)
		    "No matching messages found"
		    "End of search results")))
	(insert (propertize str 'face 'mu4e-system-face 'intangible t))
	(unless (= 0 count)
	  (mu4e-message "Found %d matching message%s"
	    count (if (= 1 count) "" "s"))
	  ;; highlight the first message
	  (mu4e~headers-highlight (mu4e~headers-docid-at-point (point-min)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro mu4e~headers-defun-mark-func (mark)
  "Define a function mu4e~headers-mark-MARK."
  (let ((funcname (intern (concat "mu4e~headers-mark-" (symbol-name mark))))
	 (docstring (concat "Mark header at point with " (symbol-name mark) ".")))
     `(defun ,funcname () ,docstring
	(interactive)
	(mu4e-headers-mark-and-next (quote ,mark)))))

;; define our mark functions; there must be some way to do this in a loop but
;; since `mu4e~headers-defun-mark-func' is a macro, the argument must be a
;; literal value.
(mu4e~headers-defun-mark-func trash)
(mu4e~headers-defun-mark-func delete)
(mu4e~headers-defun-mark-func read)
(mu4e~headers-defun-mark-func unread)
(mu4e~headers-defun-mark-func flag)
(mu4e~headers-defun-mark-func unflag)
(mu4e~headers-defun-mark-func deferred)
(mu4e~headers-defun-mark-func unmark)


;;; headers-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-headers-mode-map nil
  "Keymap for *mu4e-headers* buffers.")
(unless mu4e-headers-mode-map
  (setq mu4e-headers-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "s" 'mu4e-headers-search)
      (define-key map "S" 'mu4e-headers-search-edit)
      (define-key map "/" 'mu4e-headers-search-narrow)

      (define-key map "j" 'mu4e~headers-jump-to-maildir)

      (define-key map (kbd "<M-left>")  'mu4e-headers-query-prev)
      (define-key map (kbd "<M-right>") 'mu4e-headers-query-next)

      (define-key map "b" 'mu4e-headers-search-bookmark)
      (define-key map "B" 'mu4e-headers-search-bookmark-edit)

      (define-key map "O" 'mu4e-headers-change-sorting)
      (define-key map "P" 'mu4e-headers-toggle-threading)
      (define-key map "Q" 'mu4e-headers-toggle-full-search)

      (define-key map "q" 'mu4e~headers-quit-buffer)
      (define-key map "z" 'mu4e~headers-quit-buffer)

      (define-key map "r" 'mu4e-headers-rerun-search)
      (define-key map "g" 'mu4e-headers-rerun-search) ;; for compatibility

      (define-key map "%" 'mu4e-headers-mark-pattern)
      (define-key map "t" 'mu4e-headers-mark-subthread)
      (define-key map "T" 'mu4e-headers-mark-thread)


      ;; navigation between messages
      (define-key map "p" 'mu4e-headers-prev)
      (define-key map "n" 'mu4e-headers-next)
      (define-key map (kbd "<M-up>") 'mu4e-headers-prev)
      (define-key map (kbd "<M-down>") 'mu4e-headers-next)

      ;; change the number of headers
      (define-key map (kbd "C-+") 'mu4e-headers-split-view-resize)
      (define-key map (kbd "C--")
	(lambda () (interactive) (mu4e-headers-split-view-resize -1)))
      (define-key map (kbd "<C-kp-add>") 'mu4e-headers-split-view-resize)
      (define-key map (kbd "<C-kp-subtract>")
	(lambda () (interactive) (mu4e-headers-split-view-resize -1)))


      ;; switching to view mode (if it's visible)
      (define-key map "y" 'mu4e-select-other-view)

      ;; marking/unmarking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define-key map (kbd "<backspace>") 'mu4e~headers-mark-trash)
      (define-key map (kbd "d") 'mu4e~headers-mark-trash)

      (define-key map (kbd "<delete>") 'mu4e~headers-mark-delete)
      (define-key map (kbd "<deletechar>") 'mu4e~headers-mark-delete)
      (define-key map (kbd "D") 'mu4e~headers-mark-delete)

      (define-key map (kbd "o") 'mu4e~headers-mark-unread)
      (define-key map (kbd "r") 'mu4e~headers-mark-read)
      (define-key map (kbd "u") 'mu4e~headers-mark-unmark)
      (define-key map (kbd "+") 'mu4e~headers-mark-flag)
      (define-key map (kbd "-") 'mu4e~headers-mark-unflag)
      (define-key map (kbd "&") 'mu4e-headers-mark-custom)
      (define-key map "m" 'mu4e-headers-mark-for-move-and-next)

      (define-key map (kbd "*")  'mu4e~headers-mark-deferred)
      (define-key map (kbd "<kp-multiply>")  'mu4e~headers-mark-deferred)
      (define-key map (kbd "#")   'mu4e-mark-resolve-deferred-marks)

      (define-key map "U" 'mu4e-mark-unmark-all)
      (define-key map "x" 'mu4e-mark-execute-all)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-key map "a" 'mu4e-headers-action)

      ;; message composition
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-compose-edit)

      (define-key map (kbd "RET") 'mu4e-headers-view-message)
      (define-key map [mouse-2]   'mu4e-headers-view-message)

      (define-key map "$" 'mu4e-show-log)
      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Headers")))
	(define-key map [menu-bar headers] (cons "Headers" menumap))

	(define-key menumap [mu4e~headers-quit-buffer]
	  '("Quit view" . mu4e~headers-quit-buffer))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))

	(define-key menumap [execute-marks]  '("Execute marks"
						. mu4e-mark-execute-all))
	(define-key menumap [unmark-all]  '("Unmark all" . mu4e-mark-unmark-all))
	(define-key menumap [unmark]      '("Unmark" . mu4e~headers-mark-unmark))

	(define-key menumap [mark-pattern]  '("Mark pattern" .
					       mu4e-headers-mark-pattern))
	(define-key menumap [mark-as-read]  '("Mark as read" .
					       mu4e~headers-mark-read))
	(define-key menumap [mark-as-unread]
	  '("Mark as unread" .  mu4e~headers-mark-unread))

	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mu4e~headers-mark-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mu4e~headers-mark-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mu4e-headers-mark-for-move-and-next))
	(define-key menumap [sepa1] '("--"))

	(define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
	(define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
	(define-key menumap [sepa2] '("--"))

	(define-key menumap [query-next]  '("Next query" . mu4e-headers-query-next))
	(define-key menumap [query-prev]  '("Previous query" .
					     mu4e-headers-query-prev))
	(define-key menumap [narrow-search] '("Narrow search" .
					       mu4e-headers-search-narrow))
	(define-key menumap [bookmark]  '("Search bookmark" .
					   mu4e-headers-search-bookmark))
	(define-key menumap [jump]  '("Jump to maildir" .
				       mu4e~headers-jump-to-maildir))
	(define-key menumap [refresh]  '("Refresh" . mu4e-headers-rerun-search))
	(define-key menumap [search]  '("Search" . mu4e-headers-search))


	(define-key menumap [sepa3] '("--"))

	(define-key menumap [view]  '("View" . mu4e-headers-view-message))
	(define-key menumap [next]  '("Next" . mu4e-headers-next))
	(define-key menumap [previous]  '("Previous" . mu4e-headers-prev))
	(define-key menumap [sepa4] '("--")))
      map)))

(fset 'mu4e-headers-mode-map mu4e-headers-mode-map)


(define-derived-mode mu4e-headers-mode special-mode
    "mu4e:headers"
  "Major mode for displaying mu4e search results.
\\{mu4e-headers-mode-map}."
  (use-local-map mu4e-headers-mode-map)

  (make-local-variable 'mu4e~headers-proc)
  (make-local-variable 'mu4e~highlighted-docid)
  (make-local-variable 'global-mode-string)
  (make-local-variable 'hl-line-face)

  (setq
    truncate-lines t
    buffer-undo-list t ;; don't record undo information
    overwrite-mode 'overwrite-mode-binary
    hl-line-face 'mu4e-header-highlight-face)
  (mu4e~mark-initialize) ;; initialize the marking subsystem
  (hl-line-mode 1)
  
  (setq header-line-format
    (cons
      (make-string
	(+ mu4e~mark-fringe-len (floor (fringe-columns 'left t))) ?\s)
      (map 'list
	(lambda (item)
	  (let ((field (cdr (assoc (car item) mu4e-header-names)))
		 (width (cdr item)))
	    (concat
	      (propertize
		(if width
		  (truncate-string-to-width field width 0 ?\s t)
		  field)
		'face 'mu4e-header-title-face) " ")))
	mu4e-headers-fields))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; higlighting
(defvar mu4e~highlighted-docid nil
  "*internal* The highlighted docid")

(defun mu4e~headers-highlight (docid)
  "Highlight the header with DOCID, or do nothing if it's not
found. Also, unhighlight any previously highlighted headers."
  (with-current-buffer mu4e~headers-buffer
    (save-excursion
      ;; first, unhighlight the previously highlighted docid, if any
      (when (and mu4e~highlighted-docid
	      (mu4e~headers-goto-docid mu4e~highlighted-docid))
	(hl-line-unhighlight))
      ;; now, highlight the new one
      (when (mu4e~headers-goto-docid docid)
	(hl-line-highlight)))
    (setq mu4e~highlighted-docid docid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~headers-select-window ()
  "When there is a visible window for the headers buffer, make sure
to select it. This is needed when adding new headers, otherwise
adding a lot of new headers looks really choppy."
  (let ((win (get-buffer-window mu4e~headers-buffer)))
    (when win (select-window win))))

;;;; headers in the buffer are prefixed by an invisible string with the docid
;;;; followed by an EOT ('end-of-transmission', \004, ^D) non-printable ascii
;;;; character. this string also has a text-property with the docid. the former
;;;; is used for quickly finding a certain header, the latter for retrieving the
;;;; docid at point without string matching etc.

(defun mu4e~headers-docid-cookie (docid)
  "Create an invisible string containing DOCID; this is to be used
at the beginning of lines to identify headers."
  (propertize (format "%s%d%s"
		mu4e~headers-docid-pre docid mu4e~headers-docid-post)
    'docid docid 'invisible t))


(defun mu4e~headers-docid-at-point (&optional point)
  "Get the docid for the header at POINT, or at current (point) if
nil. Returns the docid, or nil if there is none."
    (save-excursion
      (when point
	(goto-char point))
      (get-text-property (line-beginning-position) 'docid)))

(defun mu4e~headers-goto-docid (docid &optional to-mark)
  "Go to the beginning of the line with the header with docid
DOCID, or nil if it cannot be found. If the optional TO-MARK is
non-nil, go to the point directly *after* the docid-cookie instead
of the beginning of the line."
  (let ((oldpoint (point)) (newpoint))
    (goto-char (point-min))
    (setq newpoint
      (search-forward (mu4e~headers-docid-cookie docid) nil t))
    (unless to-mark
      (if (null newpoint)
	(goto-char oldpoint) ;; not found; restore old pos
	(progn
	  (beginning-of-line) ;; found, move to beginning of line
	  (setq newpoint (point)))))
    newpoint)) ;; return the point, or nil if not found

(defun mu4e~headers-docid-pos (docid)
  "Return the pos of the beginning of the line with the header with
docid DOCID, or nil if it cannot be found."
  (let ((pos))
    (save-excursion
      (setq pos (mu4e~headers-goto-docid docid)))
    pos))

(defun mu4e~headers-field-for-docid (docid field)
  "Get FIELD (a symbol, see `mu4e-headers-names') for the message
with DOCID which must be present in the headers buffer."
  (save-excursion
    (when (mu4e~headers-goto-docid docid)
      (mu4e-field-at-point field))))

;;;; markers mark headers for
(defun mu4e~headers-mark (docid mark)
  "(Visually) mark the header for DOCID with character MARK."
  (with-current-buffer mu4e~headers-buffer
    (let ((inhibit-read-only t) (oldpoint (point)))
      (unless (mu4e~headers-goto-docid docid)
	(error "Cannot find message with docid %S" docid))
      ;; now, we're at the beginning of the header, looking at
      ;; <docid>\004
      ;; (which is invisible). jump past that…
      (unless (re-search-forward mu4e~headers-docid-post nil t)
	(error "Cannot find the `mu4e~headers-docid-post' separator"))

      ;; clear old marks, and add the new ones.
      (let ((msg (get-text-property (point) 'msg)))
	(delete-char mu4e~mark-fringe-len)
	(insert (propertize
		  (format mu4e~mark-fringe-format mark)
		  'face 'mu4e-header-marks-face
		  'docid docid
		  'msg msg)))
      (goto-char oldpoint))))


(defun mu4e~headers-add-header (str docid point &optional msg)
  "Add header STR with DOCID to the buffer at POINT if non-nil, or
at (point-max) otherwise. If MSG is not nil, add it as the text-property `msg'."
  (unless docid (error "Invalid message"))
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer mu4e~headers-buffer
      (let ((inhibit-read-only t)
	     (is-first-header (= (point-min) (point-max))))
	(save-excursion
	  (goto-char (if point point (point-max)))
	  (insert
	    (propertize
	      (concat
		(mu4e~headers-docid-cookie docid)
		mu4e~mark-fringe
		str "\n")
	      'docid docid 'msg msg)))))))

(defun mu4e~headers-remove-header (docid &optional ignore-missing)
  "Remove header with DOCID at POINT; when IGNORE-MISSING is
non-nill, don't raise an error when the docid is not found."
  (with-current-buffer mu4e~headers-buffer
    (if (mu4e~headers-goto-docid docid)
      (let ((inhibit-read-only t))
	(delete-region (line-beginning-position) (line-beginning-position 2)))
      (unless ignore-missing
	(error "Cannot find message with docid %S" docid)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~headers-update-global-mode-string ()
  "Determine the mode string for the headers buffers (based on the
last query, sorting settings."
  (let* ((cell (find-if
		(lambda (cell)
		  (eq (cdr cell) mu4e-headers-sortfield))
		mu4e~headers-sortfield-choices))
	  (optchar (substring (car cell) 0 1)))
    (setq global-mode-string
      (concat
	(propertize mu4e~headers-last-query 'face 'mu4e-title-face)
	"("
	optchar
	(if mu4e-headers-sort-revert "d" "a")
	(when mu4e-headers-show-threads "T")
	(when mu4e-headers-full-search "F")
	")"))))

(defun mu4e~headers-search-execute (expr ignore-history)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results. If IGNORE-HISTORY is true, do *not* update
the query history stack."
  ;; note: we don't want to update the history if this query comes from
  ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
  (mu4e-hide-other-mu4e-buffers)
  (let ((buf (get-buffer-create mu4e~headers-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (mu4e-headers-mode)
      (unless ignore-history
	;; save the old present query to the history list
	(when mu4e~headers-last-query
	  (mu4e~headers-push-query mu4e~headers-last-query 'past)))
      (setq
	mu4e~headers-buffer buf
	mode-name "mu4e-headers"
	mu4e~headers-last-query expr)
      (mu4e~headers-update-global-mode-string))
    (switch-to-buffer buf)
    (mu4e~proc-find
      (replace-regexp-in-string "\"" "\\\\\"" expr) ;; escape "\"
      mu4e-headers-show-threads
      mu4e-headers-sortfield
      mu4e-headers-sort-revert
      (unless mu4e-headers-full-search mu4e-search-results-limit))))

(defun mu4e~headers-redraw-get-view-window ()
  "Close all windows, redraw the headers buffer based on the value
of `mu4e-split-view', and return a window for the message view."
  (mu4e-hide-other-mu4e-buffers)
  (unless (buffer-live-p mu4e~headers-buffer)
    (error "No headers buffer available"))
  (switch-to-buffer mu4e~headers-buffer)
  ;; kill the existing view win
  (when (buffer-live-p mu4e~view-buffer)
    (kill-buffer mu4e~view-buffer))
  ;; get a new view window
  (setq mu4e~headers-view-win
    (cond
      ((eq mu4e-split-view 'horizontal) ;; split horizontally
	(split-window-vertically mu4e-headers-visible-lines))
      ((eq mu4e-split-view 'vertical) ;; split vertically
	(split-window-horizontally mu4e-headers-visible-columns))
      (t ;; no splitting; just use the currently selected one
	(selected-window))))
  mu4e~headers-view-win)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search-based marking

(defun mu4e-headers-for-each (func)
  "Call FUNC for each header, moving point to the header. FUNC
takes one argument msg, the msg s-expression for the corresponding
header."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward mu4e~headers-docid-pre nil t)
      ;; not really sure why we need to jump to bol; we we need
      ;; to, otherwise we miss lines sometimes...
      (let ((msg (get-text-property (line-beginning-position) 'msg)))
	(when msg
	  (funcall func msg))))))

 (defvar mu4e~headers-regexp-hist nil
  "History list of regexps used.")

 (defun mu4e~headers-mark-for-each-if (markpair mark-pred &optional param)
  "Mark all headers for with predicate function MARK-PRED return
non-nil with MARKPAIR. MARK-PRED is function that takes two
arguments, MSG (the message at point) and PARAM (a user-specified
parameter). MARKPAIR is a cell (MARK . TARGET)."
  (mu4e-headers-for-each
    (lambda (msg)
      (when (funcall mark-pred msg param)
	(mu4e-mark-at-point (car markpair) (cdr markpair))))))

(defun mu4e-headers-mark-pattern ()
  "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (let ((markpair (mu4e~mark-get-markpair "Mark matched messages with: " t))
	 (field (mu4e-read-option "Field to match: "
		  '( ("subject" . :subject)
		     ("from"    . :from)
		     ("to"      . :to))))
	  (pattern (read-string
		     (mu4e-format "Regexp:")
		     nil 'mu4e~headers-regexp-hist)))
    (mu4e~headers-mark-for-each-if
      markpair
      (lambda (msg param)
	(let* ((do-mark) (value (mu4e-msg-field msg field)))
	  (setq do-mark
	    (if (member field '(:to :from :cc :bcc :reply-to))
	      (find-if (lambda (contact)
			 (let ((name (car contact)) (email (cdr contact)))
			   (or (and name (string-match pattern name))
			     (and email (string-match pattern email))))) value)
	      (string-match pattern (or value "")))))))))

(defun mu4e-headers-mark-custom ()
  "Mark messages based on a user-provided predicate function."
  (interactive)
  (let* ((pred (mu4e-read-option "Match function: "
		 mu4e-headers-custom-markers))
	  (param (when (cdr pred) (eval (cdr pred))))
	  (markpair (mu4e~mark-get-markpair "Mark matched messages with: " t)))
    (mu4e~headers-mark-for-each-if markpair (car pred) param)))

(defun mu4e~headers-get-thread-info (msg what)
  "Get WHAT (a symbol, either path or thread-id) for MSG."
  (let* ((thread (or (plist-get msg :thread)  (error "No thread info found")))
	  (path  (or (plist-get thread :path)  (error "No threadpath found"))))
    (case what
      (path path)
      (thread-id
	(save-match-data
	  ;; the thread id is the first segment of the thread path
	  (when (string-match "^\\([[:xdigit:]]+\\):?" path)
	    (match-string 1 path))))
      (otherwise (error "Not supported")))))


(defun mu4e-headers-mark-thread (&optional subthread)
  "Mark the thread at point, if SUBTHREAD is non-nil, marking is
limited to the message at point and its descendants."
  ;; the tread id is shared by all messages in a thread
  (interactive "P")
  (let* ((thread-id (mu4e~headers-get-thread-info
		      (mu4e-message-at-point t) 'thread-id))
	  (path     (mu4e~headers-get-thread-info
		      (mu4e-message-at-point t) 'path))
	  (markpair
	    (mu4e~mark-get-markpair
	      (if subthread "Mark subthread with: " "Mark whole thread with: ")
	      t))
	  (last-marked-point))
    (mu4e-headers-for-each
      (lambda (msg)
 	(let ((my-thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
	  (if subthread
	    ;; subthread matching; msg's thread path should have path as its
	    ;; prefix
	    (when (string-match (concat "^" path)
		    (mu4e~headers-get-thread-info msg 'path))
	      (mu4e-mark-at-point (car markpair) (cdr markpair))
	      (setq last-marked-point (point)))
	    ;; nope; not looking for the subthread; looking for the whole thread
	    (when (string= thread-id
		    (mu4e~headers-get-thread-info msg 'thread-id))
	      (mu4e-mark-at-point (car markpair) (cdr markpair))
	      (setq last-marked-point (point)))))))
    (when last-marked-point
      (goto-char last-marked-point)
      (mu4e-headers-next))))

(defun mu4e-headers-mark-subthread ()
  "Like `mu4e-mark-thread', but only for a sub-thread."
  (interactive)
  (mu4e-headers-mark-thread t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; the query past / present / future ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e~headers-query-past nil
  "Stack of queries before the present one.")
(defvar mu4e~headers-query-future nil
  "Stack of queries after the present one.")
(defvar mu4e~headers-last-query nil
  "The present (most recent) query.")
(defvar mu4e~headers-query-stack-size 20
  "Maximum size for the query stacks.")

(defun mu4e~headers-push-query (query where)
  "Push QUERY to one of the query stacks; WHERE is a symbol telling
us where to push; it's a symbol, either 'future or
'past. Functional also removes duplicats, limits the stack size."
  (let ((stack
	  (case where
	    (past   mu4e~headers-query-past)
	    (future mu4e~headers-query-future))))
     ;; only add if not the same item
    (unless (and stack (string= (car stack) query))
      (push query stack)
      ;; limit the stack to `mu4e~headers-query-stack-size' elements
      (when (> (length stack) mu4e~headers-query-stack-size)
	(setq stack (subseq stack 0 mu4e~headers-query-stack-size)))
      ;; remove all duplicates of the new element
      (remove-if (lambda (elm) (string= elm (car stack))) (cdr stack))
      ;; update the stacks
      (case where
	(past   (setq mu4e~headers-query-past   stack))
	(future (setq mu4e~headers-query-future stack))))))

(defun mu4e~headers-pop-query (whence)
    "Pop a query from the stack. WHENCE is a symbol telling us where
to get it from; it's a symbol, either 'future or 'past."
  (case whence
    (past
      (unless mu4e~headers-query-past
	(error "No more previous queries"))
      (pop mu4e~headers-query-past))
    (future
      (unless mu4e~headers-query-future
	(error "No more next queries"))
      (pop mu4e~headers-query-future))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e~headers-search-hist nil
  "History list of searches.")

(defun mu4e-headers-search (&optional expr prompt edit ignore-history)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results. This is an interactive function which ask
user for EXPR. PROMPT, if non-nil, is the prompt used by this
function (default is \"Search for:\"). If EDIT is non-nil, instead
of executing the query for EXPR, let the user edit the query before
executing it. If IGNORE-HISTORY is true, do *not* update the query
history stack."
  ;; note: we don't want to update the history if this query comes from
  ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'."
  (interactive)
  (let* ((prompt (mu4e-format (or prompt "Search for: ")))
	  (expr
	    (if edit
	      (read-string prompt expr)
	      (or expr
		(read-string prompt nil 'mu4e~headers-search-hist)))))
    (mu4e-mark-handle-when-leaving)
    (mu4e~headers-search-execute expr
      ignore-history)))


(defun mu4e-headers-search-edit ()
  "Edit the last search expression."
  (interactive)
  (mu4e-headers-search mu4e~headers-last-query nil t))

(defun mu4e-headers-search-bookmark (&optional expr edit)
  "Search using some bookmarked query EXPR.
If EDIT is non-nil, let the user edit the bookmark before starting
the search."
  (interactive)
  (let ((expr
	  (or expr
	    (mu4e-ask-bookmark (if edit "Select bookmark: " "Bookmark: ")))))
    (mu4e-headers-search expr (when edit "Edit bookmark: ") edit)))

(defun mu4e-headers-search-bookmark-edit ()
  "Edit an existing bookmark before executing it."
  (interactive)
  (mu4e-headers-search-bookmark nil t))


(defun mu4e-headers-search-narrow (filter )
  "Narrow the last search by appending search expression FILTER to
the last search expression."
  (interactive
    (let ((filter
  	    (read-string (mu4e-format "Narrow down to: ")
  	      nil 'mu4e~headers-search-hist nil t)))
      (list filter)))
  (unless mu4e~headers-last-query
    (error "There's nothing to filter"))
  (mu4e-headers-search
    (format "(%s) AND %s" mu4e~headers-last-query filter)))


(defun mu4e-headers-change-sorting (&optional dont-refresh)
  "Interactively change the sorting/threading parameters. With prefix-argument,
do _not_ refresh the last search with the new parameters."
  (interactive "P")
  (let* ((sortfield
	   (mu4e-read-option "Sortfield: " mu4e~headers-sortfield-choices))
	  (revert
	    (mu4e-read-option "Direction: "
	      '(("ascending" . nil) ("descending" . t)))))
    (setq
      mu4e-headers-sortfield sortfield
      mu4e-headers-sort-revert revert) ;; "descending" means "revert"
    (mu4e-message "Sorting by %s (%s)%s"
      (symbol-name sortfield)
      (if revert "descending" "ascending")
      (if dont-refresh
	" (press 'g' to refresh)" ""))
    (unless dont-refresh
      (mu4e-headers-rerun-search))))

(defun mu4e-headers-toggle-threading (&optional dont-refresh)
  "Toggle threading on/off for the search results. With prefix-argument,
do _not_ refresh the last search with the new setting for
threading."
  (interactive "P")
  (setq mu4e-headers-show-threads (not mu4e-headers-show-threads))
  (mu4e-message "Threading turned %s%s"
    (if mu4e-headers-show-threads "on" "off")
    (if dont-refresh
	" (press 'g' to refresh)" ""))
  (unless dont-refresh
    (mu4e-headers-rerun-search)))

(defun mu4e-headers-toggle-full-search (&optional dont-refresh)
  "Toggle full-search on/off for the search results. With prefix-argument,
do _not_ refresh the last search with the new setting for
threading."
  (interactive "P")
  (setq mu4e-headers-full-search (not mu4e-headers-full-search))
  (mu4e-message "Full search turned %s%s"
    (if mu4e-headers-full-search "on" "off")
    (if dont-refresh
	" (press 'g' to refresh)" ""))
  (unless dont-refresh
    (mu4e-headers-rerun-search)))

(defvar mu4e~headers-loading-buf nil
  "A buffer for loading a message view.")

(defun mu4e~headers-get-loading-buf ()
  "Get a buffer to give feedback while loading a message view."
  (unless (buffer-live-p mu4e~headers-loading-buf)
    (setq mu4e~headers-loading-buf
      (get-buffer-create " *mu4e-loading*"))
    (with-current-buffer mu4e~headers-loading-buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (propertize "Waiting for message..."
		  'face 'mu4e-system-face 'intangible t)))
      (setq buffer-read-only t)))
  mu4e~headers-loading-buf)

(defun mu4e-headers-view-message ()
  "View message at point. If there's an existing window for the
view, re-use that one. If not, create a new one, depending on the
value of `mu4e-split-view': if it's a symbol `horizontal' or
`vertical', split the window accordingly; if it is nil, replace the
current window. "
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((docid (or (mu4e~headers-docid-at-point)
		  (error "No message at point")))
	  (viewwin (mu4e~headers-redraw-get-view-window)))
    (unless (window-live-p viewwin)
      (error "Cannot get a message view"))
    (select-window viewwin)
    (switch-to-buffer (mu4e~headers-get-loading-buf))
    (mu4e~proc-view docid mu4e-view-show-images))) 

(defun mu4e-headers-rerun-search ()
  "Rerun the search for the last search expression."
  (interactive)
  (mu4e-headers-search mu4e~headers-last-query))

(defun mu4e~headers-query-navigate (whence)
  "Execute the previous query from the query stacks. WHENCE
determines where the query is taken from and is a symbol, either
`future' or `past'."
  (let ((query (mu4e~headers-pop-query whence))
	 (where (if (eq whence 'future) 'past 'future)))
    (mu4e~headers-push-query mu4e~headers-last-query where)
    (mu4e-headers-search query nil nil t)))

(defun mu4e-headers-query-next ()
  "Execute the previous query from the query stacks."
  (interactive)
  (mu4e~headers-query-navigate 'future))

(defun mu4e-headers-query-prev ()
  "Execute the previous query from the query stacks."
  (interactive)
  (mu4e~headers-query-navigate 'past))

;; forget the past so we don't repeat it :/
(defun mu4e-headers-forget-queries ()
  "Forget all the complete query history."
  (interactive)
  (setq
    ;; note: don't forget the present one
    mu4e~headers-query-past nil
    mu4e~headers-query-future nil)
  (mu4e-message "Query history cleared"))

(defun mu4e~headers-move (lines)
  "Move point LINES lines forward (if LINES is positive) or
backward (if LINES is negative). If this succeeds, return the new
docid. Otherwise, return nil."
  (unless (eq major-mode 'mu4e-headers-mode)
    (error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let ((succeeded (zerop (forward-line lines)))
	 (docid (mu4e~headers-docid-at-point)))
    ;; move point, even if this function is called when this window is not
    ;; visible
    (when docid
      ;; update all windows showing the headers buffer
      (walk-windows
	(lambda (win)
	  (when (eq (window-buffer win) mu4e~headers-buffer)
	    (set-window-point win (point))))
	nil t)
       ;;(set-window-point (get-buffer-window mu4e~headers-buffer t) (point))
      ;; attempt to highlight the new line, display the message
      (mu4e~headers-highlight docid)
      ;; update message view if it was already showing
      (when (window-live-p mu4e~headers-view-win)
	(mu4e-headers-view-message))
      docid)))

(defun mu4e-headers-next (&optional n)
  "Move point to the next message header. If this succeeds, return
the new docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (mu4e~headers-move (or n 1)))

(defun mu4e-headers-prev (&optional n)
  "Move point to the previous message header. If this succeeds, return
the new docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (mu4e~headers-move (- (or n 1))))

(defun mu4e~headers-jump-to-maildir (maildir)
  "Show the messages in maildir (user is prompted to ask what
maildir)."
  (interactive
    (let ((maildir (mu4e-ask-maildir "Jump to maildir: ")))
      (list maildir)))
  (when maildir
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search (concat "\"maildir:" maildir "\""))))

(defun mu4e-headers-split-view-resize (n)
  "In horizontal split-view, increase the number of lines shown by
N; in vertical split-view, increase the number of columns shown by
N. Otherwise, don't do anything."
  (interactive "P")
   (when (buffer-live-p mu4e~view-buffer)
    (case mu4e-split-view
	(horizontal
	  (let ((newval (+ (or n 1) mu4e-headers-visible-lines)))
	    (unless (> newval 0)
	      (error "Cannot make the number of visible lines any smaller"))
	    (setq mu4e-headers-visible-lines newval)))
	(vertical
	  (let ((newval (+ (or n 1) mu4e-headers-visible-columns)))
	    (unless (> newval 0)
	      (error "Cannot make the number of visible columns any smaller"))
	    (setq mu4e-headers-visible-columns newval))))
      (let ((viewwin (mu4e~headers-redraw-get-view-window)))
	(when (window-live-p viewwin)
	  (select-window viewwin)
	  (switch-to-buffer mu4e~view-buffer)))))

(defun mu4e-headers-action ()
  "Ask user what to do with message-at-point, then do it. The
actions are specified in `mu4e-headers-actions'."
  (interactive)
  (let ((msg (mu4e-message-at-point t))
	 (actionfunc (mu4e-read-option "Action: " mu4e-headers-actions)))
    (funcall actionfunc msg)))

(defun mu4e-headers-mark-and-next (mark)
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
  (interactive)
  (mu4e-mark-set mark)
  (mu4e-headers-next))

(defun mu4e-headers-mark-for-move-and-next ()
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
  (interactive)
  (mu4e-mark-for-move-set)
  (mu4e-headers-next))

(defun mu4e~headers-quit-buffer ()
  "Quit the mu4e-headers buffer. This is a rather complex function,
to ensure we don't disturb other windows."
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (error "Must be in mu4e-headers-mode (%S)" major-mode))
  (mu4e-mark-handle-when-leaving)
  (let ((curbuf (current-buffer)) (curwin (selected-window))
	 (headers-visible))
    (walk-windows
      (lambda (win)
	;; if we the view window connected to this one, kill it
	(when (eq mu4e~headers-view-win win)
	  (delete-window win)
	  (setq mu4e~headers-view-win nil))
	;; and kill any _other_ (non-selected) window that shows the current
	;; buffer
	(when (and
		(eq curbuf (window-buffer win)) ;; does win show curbuf?
		(not (eq curwin win))	        ;; it's not the curwin?
		(not (one-window-p)))           ;; and not the last one?
	  (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone. kill ourselves, and return
    ;; to the main view
    (kill-buffer)
    (mu4e~main-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-headers)
