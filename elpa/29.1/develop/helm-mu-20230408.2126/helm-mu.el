;;; helm-mu.el --- Helm search for e-mails and contacts in mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2013 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/emacs-helm/helm-mu
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helm sources for searching emails and contacts using mu and
;; mu4e. Mu is an indexer for maildirs and mu4e is a mutt-like MUA for
;; Emacs build on top of mu. Mu is highly efficient making it possible
;; to get instant results even for huge maildirs. It also provides
;; search operators similar to Google mail, e.g:
;;
;;     from:Adam to:Eve flag:attach vacation photos
;;
;; See the Github page for details and install instructions:
;;
;;  https://github.com/emacs-helm/helm-mu
;;
;; News:
;; - 2022-12-17: To override mu4e’s default search function, you now
;;   have to use: (define-key mu4e-search-minor-mode-map "s" 'helm-mu)
;;
;; - 2016-05-31: Added two new actions in helm-mu-contacts: 1. Insert
;;   selected contacts at point. 2) Copy selected contacts to
;;   clipboard.  Contacts are inserted/copied in a format that is
;;   suitable for address fields, i.e. with quote names and email
;;   addresses.

;;; Install:

;; Helm-mu requires a fully configured mu4e setup and the latest
;; version of mu (version from Sept 27 2013 or later).  Also make sure
;; that helm is configured.  See
;; https://github.com/emacs-helm/helm#install-from-emacs-packaging-system
;; for details.
;;
;; Copy helm-mu.el to a directory in your load-path or install helm-mu
;; from MELPA (preferred).  Then add the following to your init file:
;;
;;     (require 'helm-mu)
;;
;; Alternatively, you can use the autoload facility:
;;
;;     (autoload 'helm-mu "helm-mu" "" t)
;;     (autoload 'helm-mu-contacts "helm-mu" "" t)
;;
;; To run mu, helm-mu uses the function `start-process-shell-command'.
;; It assumes that the shell called by that function is compatible
;; with the Bourne shell (e.g., bash).  If your shell is incompatible,
;; the mu command may not work.
;;
;; GNU sed is used to do some filtering of the results returned by
;; mu.  GNU sed is standard on Linux but OSX users may have to install
;; it since the pre-installed BSD sed has different command line
;; options.
;;
;; Some things that can be configured:
;;
;; - `helm-mu-default-search-string'
;; - `helm-mu-skip-duplicates'
;; - `helm-mu-contacts-name-colwidth'
;; - `helm-mu-contacts-name-replace'
;; - `helm-mu-contacts-after'
;; - `helm-mu-contacts-personal'
;; - `helm-mu-gnu-sed-program'
;; - `helm-mu-append-implicit-wildcard'
;;
;; Consult the documentation in Emacs or the source code below for
;; explanations of these variables.

;;; Usage:

;; To search for emails use `helm-mu'.  When you would like to read an
;; email without finishing the helm session, you can select the email
;; and press Ctrl-z.  This will split the screen horizontally and show
;; the email in the new window while keeping the search results in the
;; other.  Alternatively, you can open the email using the enter key
;; and return to the helm session using the command `helm-resume'.
;;
;; To search for contacts use `helm-mu-contacts'.  Note that search
;; terms are interpreted differently by `helm-mu-contacts' than by
;; `helm-mu'.  `helm-mu' assumes that the search terms are complete
;; words, i.e., that they are surrounded by white spaces or
;; punctuation.  So if you search for "jo" it will only return emails
;; in which "jo" occurs as a word.  In contrast to that,
;; `helm-mu-contacts' will return all contacts in which "jo" occurs as
;; a substring.  `helm-mu-contacts' uses the grep tool for
;; searching.  That means that any regular expression supported by
;; grep can be used when searching for contacts.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'mu4e)

(defgroup helm-mu nil
  "Helm completion for mu."
  :group 'mu4e)

(defcustom helm-mu-default-search-string ""
  "A default search string for new searches. By default mu
searches all maildirs.  That includes mails from trash and drafts
folders.  The default search string can be used to restrict the
search to only emails in a specific maildir.  For instance, in
order to restrict the search to the inbox, the following can be
used: maildir:/INBOX"
  :group 'helm-mu
  :type  'string)

(defcustom helm-mu-always-use-default-search-string nil
  "When non nil always use default-search-string.

By default, starting a search from mu4e-headers-mode will not use
the default search string, and will instead prefill the search
with the current query, this option set to non nil ensure to always
search with default-search-string."
  :group 'helm-mu
  :type 'boolean)

(defcustom helm-mu-skip-duplicates mu4e-headers-skip-duplicates
  "With this option set to non-nil, show only one of duplicate
messages. This is useful when you have multiple copies of the same
message, which is a common occurence for example when using Gmail
and offlineimap."
  :group 'helm-mu
  :type 'boolean)

(defcustom helm-mu-contacts-name-colwidth 22
  "The width of the column showing names when searching contacts."
  :group 'helm-mu
  :type  'integer)

(defcustom helm-mu-contacts-name-replace '("[\"']" "")
  "This can be used for basic transformations of the names.  The
default value removes quotation marks."
  :group 'helm-mu
  :type  '(list string string))

(defcustom helm-mu-contacts-after "01-Jan-1970 00:00:01"
  "Only show contacts from mails received after that time.
Should be of the form the function `date-to-time' can parse."
  :group 'helm-mu
  :type  'string)

(defcustom helm-mu-contacts-personal nil
  "If non-nil, only show addresses seen in messages where one of
'my' e-mail addresses was seen in one of the address fields; this
is to exclude addresses only seen in mailing-list messages. See
the --my-address parameter in mu index."
  :group 'helm-mu
  :type  'boolean)

(defcustom helm-mu-gnu-sed-program "sed"
  "Program name of GNU sed.  For Mac OS X user, you might need to
set it to \"gsed\" if your GNU sed is installed via MacPorts or
Homebrew without some specific installation options."
  :group 'helm-mu
  :type 'string)

(defcustom helm-mu-append-implicit-wildcard nil
  "Should a wildcard be appended implicitly to the search string.
If non-nil a wildcard is appended to the user's search query
before passing it to mu, this allows getting results even for
partially entered queries.  See `helm-mu-get-search-pattern'"
  :group 'helm-mu
  :type 'boolean)

(defcustom helm-mu-command-arguments ""
  "Additional arguments passed to `mu' when retrieving contact or mail
informations. This could be useful, for example, if you use another folder
than '~/.mu' to store your data"
  :group 'helm-mu
  :type 'string)

;;;###autoload
(progn
  (require 'helm-easymenu)
  (easy-menu-add-item nil '("Tools" "Helm" "Tools") ["Mu" helm-mu t])
  (easy-menu-add-item nil '("Tools" "Helm" "Tools") ["Mu contacts" helm-mu-contacts t]))


(defface helm-mu-contacts-name-face
  '((((background dark)) :foreground "white")
    (t :foreground "black"))
  "Face for names in contacts list."
  :group 'helm-mu-faces)

(defface helm-mu-contacts-address-face
  '((((background dark)) :foreground "gray")
    (t :foreground "dim gray"))
  "Face for email addresses in contacts list."
  :group 'helm-mu-faces)


(defvar helm-mu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-c") 'helm-mu-open-headers-view)
    (define-key map (kbd "S-<return>") 'helm-mu-open-headers-view)
    map)
  "Keymap used in helm-mu.")


(defvar helm-source-mu
  (helm-build-async-source "Search email with mu"
    :candidates-process #'helm-mu-init
    :candidate-transformer
    '(helm-mu-candidate-parser helm-mu-candidates-formatter)
    :multimatch nil
    :nohighlight t
    :keymap 'helm-mu-map
    :requires-pattern 3
    :persistent-action #'helm-mu-persistent-action
    :action '(("Display message in mu4e" . helm-mu-display-email))))

(defvar helm-source-mu-contacts
  (helm-build-in-buffer-source "Search contacts with mu"
    :data #'helm-mu-contacts-init
    :filtered-candidate-transformer #'helm-mu-contacts-transformer
    :action '(("Compose email to contact(s)"      .
               helm-mu-compose-mail)
              ("Search emails from/to contact(s)" .
               helm-mu-action-get-contact-emails)
              ("Insert contact(s) at point"       .
               helm-mu-action-insert-contacts)
              ("Copy contact(s) to clipboard"     .
               helm-mu-action-copy-contacts-to-clipboard))))



(defun helm-mu-get-search-pattern ()
  "Get the pattern that should be sent to mu.
If `helm-mu-append-implicit-wildcard' is non-nil, this creates a search pattern
by appending a `*' to the pattern input by the user"
  ;; mu4e will error if the pattern contains newlines: replace them
  ;; with spaces and delete the last one.  to make sure '*' applies on
  ;; the last word.
  (let* ((current-or-last-pattern
         (if (not (string= helm-pattern ""))
             helm-pattern helm-last-query))
         (pattern
          (helm-aand current-or-last-pattern
                     (replace-regexp-in-string "\n" " " it)
                     (replace-regexp-in-string "\n$" "" it))))
    (if (and helm-mu-append-implicit-wildcard
             ;; Do not append a wildcard if flag is being searched
             ;; for, wildcards do not work with flag
             (not  (string-match-p "flag:[[:alnum:]]+$" pattern))
             (not (string-match-p "[ \t]$" pattern)))
        (concat pattern "*")
      pattern)))

(defun helm-mu-init ()
  "Initialize async mu process for `helm-source-mu'."
  (let ((process-connection-type nil)
        (maxnum (helm-candidate-number-limit helm-source-mu))
        (mucmd (concat mu4e-mu-binary
                       " find "
                       helm-mu-command-arguments
                       " -f $'i\td\tf\tt\ts' --sortfield=date --maxnum=%d --reverse --format=sexp %s 2>/dev/null "))
        (sedcmd (concat helm-mu-gnu-sed-program
                        " -e ':a;N;$!ba;s/\\n\\(\\t\\|\\()\\)\\)/ \\2/g'"))
        (pattern (helm-mu-get-search-pattern)))
    (prog1
      (start-process-shell-command "helm-mu" helm-buffer
        (concat (format mucmd maxnum (if helm-mu-skip-duplicates "--skip-dups" ""))
                (mapconcat 'shell-quote-argument
                           (split-string pattern " ")
                           " ")
                 " | " sedcmd))
      (set-process-sentinel
        (get-buffer-process helm-buffer)
        (lambda (_process event)
          (if (string= event "finished\n")
              (with-helm-window
                (setq mode-line-format
                      '(" " mode-line-buffer-identification " "
                        (line-number-mode "%l") " "
                        (:eval (propertize
                                (format "[Mu Process Finish- (%s results)]"
                                        (max (1- (count-lines
                                                  (point-min) (point-max)))
                                             0))
                                'face 'helm-grep-finish))))
                (force-mode-line-update))
            (helm-log "helm-mu-init" "Error: Mu %s"
                      (replace-regexp-in-string "\n" "" event))))))))

(defun helm-mu-contacts-init ()
  "Retrieves contacts from mu."
  (let ((cmd (concat
              mu4e-mu-binary
              " cfind --format=mutt-ab "
              helm-mu-command-arguments
              (if helm-mu-contacts-personal " --personal" "")
              (format
                " --after=%d"
                (truncate (float-time (date-to-time helm-mu-contacts-after)))))))
    (cdr (split-string (shell-command-to-string cmd) "\n"))))


(defun helm-mu-candidate-parser (candidates)
  "Parses the sexps obtained from mu find."
  (cl-loop for i in candidates
        if (string= i "mu: no matches for search expression")
          collect i
        else
          collect (car (read-from-string i))))

;; The following function recyles code from
;; mu4e~headers-header-handler in order to achieve the same formatting
;; as used in mu4e-headers-view.
(defun helm-mu-candidate-formatter (candidate)
  "Formats a candidate to look like entries in mu4e headers view."
  (let ((line " "))
    (dolist (f-w mu4e-headers-fields)
      (let ((field (car f-w))
            (width (cdr f-w))
            (val (mu4e-message-field candidate (car f-w))) (str))
        (setq str
          (cl-case field
            (:subject
              (concat
                (mu4e~headers-thread-prefix (mu4e-message-field candidate :thread))
                val))
            (:thread-subject (mu4e~headers-thread-subject candidate))
            ((:maildir :path) val)
            ((:to :from :cc :bcc) (mu4e~headers-contact-str val))
            (:from-or-to (mu4e~headers-from-or-to candidate))
            (:date (format-time-string mu4e-headers-date-format val))
            (:mailing-list (mu4e~headers-mailing-list val))
            (:human-date (mu4e~headers-human-date candidate))
            (:flags (propertize (mu4e~headers-flags-str val)
                      'help-echo (format "%S" val)))
            (:tags (propertize (mapconcat 'identity val ", ")))
            (:size (mu4e-display-size val))
            (t (mu4e~headers-custom-field-value candidate field))))
        (when str
          (setq line (concat line
              (if (not width) str
                (truncate-string-to-width str width 0 ?\s t))
              " ")))))
    (propertize line 'face
      (let ((flags (mu4e-message-field candidate :flags)))
        (cond
          ((memq 'trashed flags) 'mu4e-trashed-face)
          ((memq 'draft flags)   'mu4e-draft-face)
          ((or (memq 'unread flags)
               (memq 'new flags))
           'mu4e-unread-face)
          ((memq 'flagged flags) 'mu4e-flagged-face)
          ((memq 'replied flags) 'mu4e-replied-face)
          ((memq 'passed flags)  'mu4e-forwarded-face)
          (t                     'mu4e-header-face))))))

;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun helm-mu-window-width ()
  (if (and (not (featurep 'xemacs))
           (display-graphic-p)
           overflow-newline-into-fringe
           (/= (frame-parameter nil 'left-fringe) 0)
           (/= (frame-parameter nil 'right-fringe) 0))
      (window-body-width)
    (1- (window-body-width))))

(defun helm-mu-candidates-formatter (candidates)
  "Formats the candidates to look like the entries in mu4e headers view."
  (if (equal candidates '("mu: no matches for search expression"))
      (list (propertize (car candidates) 'face 'mu4e-system-face))
    (cl-loop for i in candidates
          for width = (save-excursion
                        (with-helm-window (helm-mu-window-width)))
          for line = (helm-mu-candidate-formatter i)
          collect (cons (truncate-string-to-width line width) i))))

(defun helm-mu-contacts-transformer (candidates _source)
  "Formats the contacts to display in two columns, name and
address.  The name column has a predefined width."
  (let* ((candidates (helm-remove-if-not-match "@" candidates))
         (candidates (helm-remove-if-match
                      "\\`\\(reply.*reply\\.github\\.com\\)\\|\\(noreply\\)"
                      candidates)))
    (cl-loop for i in candidates
             for contact = (split-string i "\t")
             for name = (replace-regexp-in-string
                         (car helm-mu-contacts-name-replace)
                         (cadr helm-mu-contacts-name-replace)
                         (cadr contact))
             for address = (car contact)
             for width = (save-excursion
                           (with-helm-window (helm-mu-window-width)))
             collect
             (cons (concat
                    (propertize
                     (truncate-string-to-width
                      name helm-mu-contacts-name-colwidth 0 ?\s)
                     'face 'helm-mu-contacts-name-face)
                    " "
                    (propertize
                     (truncate-string-to-width
                      address (- width helm-mu-contacts-name-colwidth 1)
                      0 ?\s)
                     'face 'helm-mu-contacts-address-face))
                   i))))

(defun helm-mu-format-contact (candidate)
  "Convert a CANDIDATE into a format suitable for mailing."
  (let* ((cand (split-string candidate "\t"))
         (name (cadr cand))
         (address (car cand)))
    (if (string-match "," name)
        (format "\"%s\" <%s>" name address)
      (format "%s <%s>" name address))))


(defun helm-mu-open-headers-view ()
  "Open current helm search in mu4e-headers-view."
  (interactive)
  (helm-run-after-quit 'mu4e-headers-search (helm-mu-get-search-pattern)))

(defun helm-mu-display-email (candidate)
  "Open an email using mu4e and display result for the current
search in mu4e-headers view."
  (mu4e-headers-search (helm-mu-get-search-pattern)
                       nil nil nil (plist-get candidate :message-id) t))

(defun helm-mu-compose-mail (_candidate)
  "Compose a new email directed to the selected contacts."
  (mu4e~compose-mail (mapconcat 'helm-mu-format-contact
                                (helm-marked-candidates) ", "))
  (mu4e-compose-mode))

(defun helm-mu-action-get-contact-emails (_candidate)
  "Get the emails from/to (marked) contact"
  ;; Extract email from marked candidates
  (let* ((emails (mapcar #'cl-first
                         (mapcar #'split-string
                                 (helm-marked-candidates))))
         ;; Compose the search query for helm-mu and let bind it to
         ;; `helm-mu-default-search-string'. The query is grouped so
         ;; that any further filter supplied by user are applied for
         ;; messages matching all contacts not just the last contact
         (helm-mu-default-search-string
          (concat "("
                  ;; Not using `string-join' here
                  ;; since it is not available on
                  ;; pre 24.4 emacs
                  (mapconcat 'identity
                             (mapcar (lambda (email)
                                       (format "contact:%s" email))
                                     emails)
                             " OR ")
                  ")")))
    (helm-mu)))

(defun helm-mu-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun helm-mu-compile-address-list (candidates)
  "Compile a string containing a list of contacts suitable for
use in the address field of an email."
  (let* ((candidates (mapcar (lambda (s) (split-string s "[ \t]"))
                             candidates)))
    (mapconcat #'identity
               (cl-loop
                for cand in candidates
                for address = (helm-mu-chomp (car cand))
                for name1   = (helm-mu-chomp
                               (mapconcat #'identity (cdr cand) " "))
                for name    = (if (string-match "," name1)
                                  (format "\"%s\"" name1)
                                name1)
                if (string= name "") collect address
                else collect (format "%s <%s>" name address))
               ", ")))

(defun helm-mu-action-insert-contacts (_candidate)
  "Insert list of contacts at point.  The contacts are formatted
in a format suitable for use in the address field of an email."
  (let* ((candidates (helm-marked-candidates))
         (addresses (helm-mu-compile-address-list candidates)))
    (with-helm-current-buffer
      (insert addresses))))

(defun helm-mu-action-copy-contacts-to-clipboard (_candidate)
  "Copy a list of contacts to the clipboard.  The contacts are
formatted in a format suitable for use in the address field of an
email."
  (let* ((candidates (helm-marked-candidates))
         (addresses (helm-mu-compile-address-list candidates)))
    (kill-new addresses)))

(defun helm-mu-persistent-action (candidate)
  (save-selected-window
    (helm-mu-display-email candidate))
  ;; Redisplay.
  (sit-for 0.1))


;;;###autoload
(defun helm-mu ()
  "Search for emails.  If started in mu4e-headers-view, the
current query will be used to initialize the search.  Otherwise
`helm-mu-default-search-string' will be used."
  (interactive)
  (let* ((query (if (and (eq major-mode 'mu4e-headers-mode)
                         (not helm-mu-always-use-default-search-string))
                    (mu4e-last-query)
                  helm-mu-default-search-string))
         ;; Do not append space it there is already trailing space or
         ;; query is empty
         (input (if (not (or (string-match-p " $" query)
                             (string= "" query)))
                    (concat query " ")
                  query)))

    ;; If there is an existing helm action buffer kill it, otherwise
    ;; it interferes with the action for this source. This will happen
    ;; if helm-mu is called as an action from some other source
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))

    (helm :sources 'helm-source-mu
          :buffer "*helm mu*"
          :full-frame t
          :input input
          :candidate-number-limit 500)))

;;;###autoload
(defun helm-mu-contacts ()
  "Search for contacts."
  (interactive)
  (helm :sources 'helm-source-mu-contacts
        :buffer "*helm mu contacts*"))

(provide 'helm-mu)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mu.el ends here
