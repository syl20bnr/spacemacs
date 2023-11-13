;;; org-mairix.el - Support for hooking mairix search into Org for different MUAs
;;
;; Copyright (C) 2007-2014, 2021 Georg C. F. Greve
;; mutt support by Adam Spiers <orgmode at adamspiers dot org>
;;
;; Author: Georg C. F. Greve <greve at fsfeurope dot org>
;; Keywords: outlines, hypermedia, calendar, wp, email, mairix
;; Purpose: Integrate mairix email searching into Org mode
;; See https://orgmode.org and http://www.rpcurnow.force9.co.uk/mairix/
;; Version: 0.5
;;
;; This file is not part of GNU Emacs.
;;
;; This file is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USAGE NOTE
;;
;; You will need to configure mairix first, which involves setting up your
;; .mairixrc in your home directory. Once it is working, you should set up
;; your way to display results in your favorite way -- usually a MUA.
;; Currently gnus and mutt are supported.
;;
;; After both steps are done, all you should need to hook mairix, org
;; and your MUA together is to do (require 'org-mairix) in your
;; startup file. Everything can then be configured normally through
;; Emacs customisation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;;; The custom variables

(defgroup org-mairix nil
  "Mairix support/integration in org."
  :tag "Org Mairix"
  :group 'org)

(defcustom org-mairix-threaded-links t
  "Should new links be created as threaded links?
If t, links will be stored as threaded searches.
If nil, links will be stored as non-threaded searches."
  :group 'org-mairix
  :type 'boolean)

(defcustom org-mairix-augmented-links nil
  "Should new links be created as augmenting searches?
If t, links will be stored as augmenting searches.
If nil, links will be stored as normal searches.

Attention: When activating this option, you will need
to remove old articles from your mairix results group
in some other way, mairix will not do it for you."
  :group 'org-mairix
  :type 'boolean)

(defcustom org-mairix-display-hook 'org-mairix-gnus-display-results
  "Hook to call to display the results of a successful mairix search.
Defaults to Gnus, feel free to add your own MUAs or methods."
  :group 'org-mairix
  :type 'hook)

(defcustom org-mairix-open-command "mairix %args% '%search%'"
  "The mairix command-line to use. If your paths are set up
correctly, you should not need to change this.

'%search%' will get substituted with the search expression, and
'%args%' with any additional arguments."
  :group 'org-mairix
  :type 'string)

;;; The hooks to integrate mairix into org

(org-link-set-parameters "mairix"
			 :follow #'org-mairix-open
			 :store #'org-mairix-store-gnus-link)

;;; Generic org-mairix functions

(defun org-mairix-construct-link (message-id)
  "Construct a mairix: hyperlink based on message-id."
  (concat "mairix:"
          (if org-mairix-threaded-links "t:")
          (if org-mairix-augmented-links "a:")
          "@@"
          (org-unbracket-string "<" ">" message-id)))

(defun org-store-mairix-link-props (&rest plist)
  "Take a property list describing a mail, and add mairix link
and description properties so that org can build a mairix link to
it."
  ;; We have to call `org-store-link-props' twice:
  ;;
  ;;   - It extracts 'fromname'/'fromaddress' from 'from' property,
  ;;     and stores the updated plist to `org-store-link-plist'.
  ;;
  ;;   - `org-email-link-description' uses these new properties to
  ;;     build a description from the previously stored plist.  I
  ;;     wrote a tiny patch to `org-email-link-description' so it
  ;;     could take a non-stored plist as an optional 2nd argument,
  ;;     but the plist provided still needs 'fromname'/'fromaddress'.
  ;;
  ;;   - Ideally we would decouple the storing bit of
  ;;     `org-store-link-props' from the extraction bit, but lots of
  ;;     stuff in `org-store-link' which calls it would need to be
  ;;     changed.  Maybe just factor out the extraction so it can be
  ;;     reused separately?
  (let ((mid (plist-get plist :message-id)))
    (apply 'org-store-link-props
           (append plist
                   (list :type "mairix"
                         :link (org-mairix-construct-link mid))))
    (apply 'org-store-link-props
           (append org-store-link-plist
                   (list :description (org-email-link-description))))))

(defun org-mairix-message-send-and-exit-with-link ()
  "Function that can be assigned as an alternative sending function,
it sends the message and then stores a mairix link to it before burying
the buffer just like 'message-send-and-exit' does."
  (interactive)
  (message-send)
  (let* ((message-id (message-fetch-field "Message-Id"))
         (subject (message-fetch-field "Subject"))
         (link (org-mairix-construct-link message-id))
         (desc (concat "Email: '" subject "'")))
    (setq org-stored-links
          (cons (list link desc) org-stored-links)))
  (message-bury (current-buffer)))

(defun org-mairix-open (search _)
  "Function to open mairix link.

We first need to split it into its individual parts, and then
extract the message-id to be passed on to the display function
before call mairix, evaluate the number of matches returned, and
make sure to only call display of mairix succeeded in matching."
  (let* ((args ""))
    (if (equal (substring search 0 2) "t:" )
        (progn (setq search (substring search 2 nil))
               (setq args (concat args " --threads"))))
    (if (equal (substring search 0 2) "a:")
        (progn (setq search (substring search 2 nil))
               (setq args (concat args " --augment"))))
    (let ((cmdline (org-mairix-command-substitution
                    org-mairix-open-command search args)))
      (print cmdline)
      (setq retval (shell-command-to-string cmdline))
      (string-match "\[0-9\]+" retval)
      (setq matches (string-to-number (match-string 0 retval)))
      (if (eq matches 0) (message "Link failed: no matches, sorry")
        (message "Link returned %d matches" matches)
        (run-hook-with-args 'org-mairix-display-hook search args)))))

(defun org-mairix-command-substitution (cmd search args)
  "Substitute '%search%' and '%args% in mairix search command."
  (while (string-match "%search%" cmd)
    (setq cmd (replace-match search 'fixedcase 'literal cmd)))
  (while (string-match "%args%" cmd)
    (setq cmd (replace-match args 'fixedcase 'literal cmd)))
  cmd)

;;; Functions necessary for integration of external MUAs.

;; Of course we cannot call `org-store-link' from within an external
;; MUA, so we need some other way of storing a link for later
;; retrieval by org-mode and/or remember-mode.  To do this we use a
;; temporary file as a kind of dedicated clipboard.

(defcustom org-mairix-link-clipboard "~/.org-mairix-link"
  "Pseudo-clipboard file where mairix URLs get copied to by external
applications in order to mimic `org-store-link'.  Used by
`org-mairix-insert-link'."
  :group 'org-mairix
  :type 'string)

;; When we resolve some of the issues with `org-store-link' detailed
;; at <https://orgmode.org/list/20071105181739.GB13544@atlantic.linksys.moosehall
;; we might not need org-mairix-insert-link.

(defun org-mairix-insert-link ()
  "Insert link from file defined by `org-mairix-link-clipboard'."
  (interactive)
  (let ((bytes (cadr (insert-file-contents
                      (expand-file-name org-mairix-link-clipboard)))))
    (forward-char bytes)
    (save-excursion
      (forward-char -1)
      (if (looking-at "\n")
          (delete-char 1)))))

;;; Functions necessary for mutt integration

(defgroup org-mairix-mutt nil
  "Use mutt for mairix support in org."
  :tag "Org Mairix Mutt"
  :group 'org-mairix)

(defcustom org-mairix-mutt-display-command
  "xterm -title 'mairix search: %search%' -e 'unset COLUMNS; mutt -f
~/mail/mairix -e \"push <display-message>\"' &"
  "Command to execute to display mairix search results via mutt within
an xterm.

'%search%' will get substituted with the search expression, and
'%args%' with any additional arguments used in the search."
  :group 'org-mairix-mutt
  :type 'string)

(defun org-mairix-mutt-display-results (search args)
  "Display results of mairix search in mutt, using the command line
defined in `org-mairix-mutt-display-command'."
  ;; By default, async `shell-command' invocations display the temp
  ;; buffer, which is annoying here.  We choose a deterministic
  ;; buffer name so we can hide it again immediately.
  ;; Note: `call-process' is synchronous so not useful here.
  (let ((cmd (org-mairix-command-substitution
              org-mairix-mutt-display-command search args))
        (tmpbufname (generate-new-buffer-name " *mairix-view*")))
    (shell-command cmd tmpbufname)
    (delete-windows-on (get-buffer tmpbufname))))

;;; Functions necessary for gnus integration

(defgroup org-mairix-gnus nil
  "Use gnus for mairix support in org."
  :tag "Org Mairix Gnus"
  :group 'org-mairix)

(defcustom org-mairix-gnus-results-group "nnmaildir:mairix"
  "The group that is configured to hold the mairix search results,
which needs to be setup independently of the org-mairix integration,
along with general mairix configuration."
  :group 'org-mairix-gnus
  :type 'string)

(defcustom org-mairix-gnus-select-display-group-function
'org-mairix-gnus-select-display-group-function-gg
  "Hook to call to select the group that contains the matching articles.
We should not need this, it is owed to a problem of gnus that people were
not yet able to figure out, see
 http://article.gmane.org/gmane.emacs.gnus.general/65248
 http://article.gmane.org/gmane.emacs.gnus.general/65265
 http://article.gmane.org/gmane.emacs.gnus.user/9596
for reference.

It seems gnus needs a 'forget/ignore everything you think you
know about that group' function. Volunteers?"
  :group 'org-mairix-gnus
  :type 'hook)

(defun org-mairix-store-gnus-link ()
  "Store a link to the current gnus message as a Mairix search for its
Message ID."

  ;; gnus integration
  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
    (let* ((article (gnus-summary-article-number))
           (header (gnus-summary-article-header article))
           (from (mail-header-from header))
           (message-id (mail-header-id header))
           (subject (gnus-summary-subject-string)))
      (org-store-mairix-link-props :from from
                                   :subject subject
                                   :message-id message-id))))

(defun org-mairix-gnus-display-results (search args)
  "Display results of mairix search in Gnus.

Note: This does not work as cleanly as I would like it to. The
problem being that Gnus should simply reread the group cleanly,
without remembering anything. At the moment it seems to be unable
to do that -- so you're likely to see zombies floating around.

If you can improve this, please do!"
  (if (not (equal (substring search 0 2) "m:" ))
      (error "org-mairix-gnus-display-results: display of search other than
message-id not implemented yet"))
  (setq message-id (substring search 2 nil))
  (require 'gnus)
  (require 'gnus-sum)
  ;; FIXME: (bzg/gg) We might need to make sure gnus is running here,
  ;;        and to start it in case it isn't running already. Does
  ;;        anyone know a function to do that? It seems main org mode
  ;;        does not do this, either.
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))

  ;; FIXME: This is horribly broken. Please see
  ;;  http://article.gmane.org/gmane.emacs.gnus.general/65248
  ;;  http://article.gmane.org/gmane.emacs.gnus.general/65265
  ;;  http://article.gmane.org/gmane.emacs.gnus.user/9596
  ;; for reference.
  ;;
  ;; It seems gnus needs a "forget/ignore everything you think you
  ;; know about that group" function. Volunteers?
  ;;
  ;; For now different methods seem to work differently well for
  ;; different people. So we're playing hook-selection here to make
  ;; it easy to play around until we found a proper solution.
  (run-hook-with-args 'org-mairix-gnus-select-display-group-function)
  (gnus-summary-select-article
   nil t t (car (gnus-find-matching-articles "message-id" message-id))))

(defun org-mairix-gnus-select-display-group-function-gg ()
  "Georg's hack to select a group that gnus (falsely) believes to be
empty to then call rebuilding of the summary. It leaves zombies of
old searches around, though."
  (gnus-group-quick-select-group 0 org-mairix-gnus-results-group)
  (gnus-group-clear-data)
  (gnus-summary-reselect-current-group t t))

(defun org-mairix-gnus-select-display-group-function-bzg ()
  "This is the classic way the org mode is using, and it seems to be
using better for Bastien, so it may work for you."
  (gnus-group-clear-data org-mairix-gnus-results-group)
  (gnus-group-read-group t nil org-mairix-gnus-results-group))

(provide 'org-mairix)

;;; org-mairix.el ends here
