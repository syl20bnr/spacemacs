;;; ement.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/ement.el
;; Version: 0.13
;; Package-Requires: ((emacs "27.1") (map "2.1") (persist "0.5") (plz "0.6") (taxy "0.10") (taxy-magit-section "0.12.1") (svg-lib "0.2.5") (transient "0.3.7"))
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Another Matrix client!  This one is written from scratch and is
;; intended to be more "Emacsy," more suitable for MELPA, etc.  Also
;; it has a shorter, perhaps catchier name, that is a mildly clever
;; play on the name of the official Matrix client and the Emacs Lisp
;; filename extension (oops, I explained the joke), which makes for
;; much shorter symbol names.

;; This file implements the core client library.  Functions that may be called in multiple
;; files belong in `ement-lib'.

;;; Code:

;;;; Debugging

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `ement-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (require 'warnings)
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)
(require 'dns)
(require 'files)
(require 'map)

;; This package.
(require 'ement-lib)
(require 'ement-room)
(require 'ement-notifications)
(require 'ement-notify)

;;;; Variables

(defvar ement-sessions nil
  "Alist of active `ement-session' sessions, keyed by MXID.")

(defvar ement-syncs nil
  "Alist of outstanding sync processes for each session.")

(defvar ement-users (make-hash-table :test #'equal)
  ;; NOTE: When changing the ement-user struct, it's necessary to
  ;; reset this table to clear old-type structs.
  "Hash table storing user structs keyed on user ID.")

(defvar ement-progress-reporter nil
  "Used to report progress while processing sync events.")

(defvar ement-progress-value nil
  "Used to report progress while processing sync events.")

(defvar ement-sync-callback-hook
  '(ement--update-room-buffers ement--auto-sync ement-tabulated-room-list-auto-update
                               ement-room-list-auto-update)
  "Hook run after `ement--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

(defvar ement-event-hook
  '(ement-notify ement--process-event ement--put-event)
  "Hook called for events.
Each function is called with three arguments: the event, the
room, and the session.  This hook isn't intended to be modified
by users; ones who do so should know what they're doing.")

(defvar ement-default-sync-filter
  '((room (state (lazy_load_members . t))
          (timeline (lazy_load_members . t))))
  "Default filter for sync requests.")

(defvar ement-images-queue (make-plz-queue :limit 5)
  "`plz' HTTP request queue for image requests.")

(defvar ement-read-receipt-idle-timer nil
  "Idle timer used to update read receipts.")

(defvar ement-connect-user-id-history nil
  "History list of user IDs entered into `ement-connect'.")

;; From other files.
(defvar ement-room-avatar-max-width)
(defvar ement-room-avatar-max-height)

;;;; Customization

(defgroup ement nil
  "Options for Ement, the Matrix client."
  :group 'comm)

(defcustom ement-save-sessions nil
  "Save session to disk.
Writes the session file when Emacs is killed."
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (if value
             (add-hook 'kill-emacs-hook #'ement--kill-emacs-hook)
           (remove-hook 'kill-emacs-hook #'ement--kill-emacs-hook))))

(defcustom ement-sessions-file "~/.cache/ement.el"
  ;; FIXME: Expand correct XDG cache directory (new in Emacs 27).
  "Save username and access token to this file."
  :type 'file)

(defcustom ement-auto-sync t
  "Automatically sync again after syncing."
  :type 'boolean)

(defcustom ement-after-initial-sync-hook
  '(ement-room-list--after-initial-sync ement-view-initial-rooms ement--link-children ement--run-idle-timer)
  "Hook run after initial sync.
Run with one argument, the session synced."
  :type 'hook)

(defcustom ement-initial-sync-timeout 40
  "Timeout in seconds for initial sync requests.
For accounts in many rooms, the Matrix server may take some time
to prepare the initial sync response, and increasing this timeout
might be necessary."
  :type 'integer)

(defcustom ement-auto-view-rooms nil
  "Rooms to view after initial sync.
Alist mapping user IDs to a list of room aliases/IDs to open buffers for."
  :type '(alist :key-type (string :tag "Local user ID")
                :value-type (repeat (string :tag "Room alias/ID"))))

(defcustom ement-disconnect-hook '(ement-kill-buffers ement--stop-idle-timer)
  ;; FIXME: Put private functions in a private hook.
  "Functions called when disconnecting.
That is, when calling command `ement-disconnect'.  Functions are
called with no arguments."
  :type 'hook)

(defcustom ement-view-room-display-buffer-action '(display-buffer-same-window)
  "Display buffer action to use when opening room buffers.
See function `display-buffer' and info node `(elisp) Buffer
Display Action Functions'."
  :type 'function)

(defcustom ement-auto-view-room-display-buffer-action '(display-buffer-no-window)
  "Display buffer action to use when automatically opening room buffers.
That is, rooms listed in `ement-auto-view-rooms', which see.  See
function `display-buffer' and info node `(elisp) Buffer Display
Action Functions'."
  :type 'function)

(defcustom ement-interrupted-sync-hook '(ement-interrupted-sync-warning)
  "Functions to call when syncing of a session is interrupted.
Only called when `ement-auto-sync' is non-nil.  Functions are
called with one argument, the session whose sync was interrupted.

This hook allows the user to customize how sync interruptions are
handled (e.g. how to be notified)."
  :type 'hook
  :options '(ement-interrupted-sync-message ement-interrupted-sync-warning))

(defcustom ement-sso-server-port 4567
  "TCP port used for local HTTP server for SSO logins.
It shouldn't usually be necessary to change this."
  :type 'integer)

;;;; Commands

;;;###autoload
(cl-defun ement-connect (&key user-id password uri-prefix session)
  "Connect to Matrix with USER-ID and PASSWORD, or using SESSION.
Interactively, with prefix, ignore a saved session and log in
again; otherwise, use a saved session if `ement-save-sessions' is
enabled and a saved session is available, or prompt to log in if
not enabled or available.

If USERID or PASSWORD are not specified, the user will be
prompted for them.

If URI-PREFIX is specified, it should be the prefix of the
server's API URI, including protocol, hostname, and optionally
the port, e.g.

  \"https://matrix-client.matrix.org\"
  \"http://localhost:8080\""
  (interactive (if current-prefix-arg
                   ;; Force new session.
                   (list :user-id (read-string "User ID: " nil 'ement-connect-user-id-history))
                 ;; Use known session.
                 (unless ement-sessions
                   ;; Read sessions from disk.
                   (condition-case err
                       (setf ement-sessions (ement--read-sessions))
                     (error (display-warning 'ement (format "Unable to read session data from disk (%s).  Prompting to log in again."
                                                            (error-message-string err))))))
                 (cl-case (length ement-sessions)
                   (0 (list :user-id (read-string "User ID: " nil 'ement-connect-user-id-history)))
                   (1 (list :session (cdar ement-sessions)))
                   (otherwise (list :session (ement-complete-session))))))
  (let (sso-server-process)
    (cl-labels ((new-session ()
                  (unless (string-match (rx bos "@" (group (1+ (not (any ":")))) ; Username
                                            ":" (group (optional (1+ (not (any blank)))))) ; Server name
                                        user-id)
                    (user-error "Invalid user ID format: use @USERNAME:SERVER"))
                  (let* ((username (match-string 1 user-id))
                         (server-name (match-string 2 user-id))
                         (uri-prefix (or uri-prefix (ement--hostname-uri server-name)))
                         (user (make-ement-user :id user-id :username username))
                         (server (make-ement-server :name server-name :uri-prefix uri-prefix))
                         (transaction-id (ement--initial-transaction-id))
                         (initial-device-display-name (format "Ement.el: %s@%s"
                                                              ;; Just to be extra careful:
                                                              (or user-login-name "[unknown user-login-name]")
                                                              (or (system-name) "[unknown system-name]")))
                         (device-id (secure-hash 'sha256 initial-device-display-name)))
                    (make-ement-session :user user :server server :transaction-id transaction-id
                                        :device-id device-id :initial-device-display-name initial-device-display-name
                                        :events (make-hash-table :test #'equal))))
                (password-login ()
                  (pcase-let* (((cl-struct ement-session user device-id initial-device-display-name) session)
                               ((cl-struct ement-user id) user)
                               (data (ement-alist "type" "m.login.password"
                                                  "identifier"
                                                  (ement-alist "type" "m.id.user"
                                                               "user" id)
                                                  "password" (or password
                                                                 (read-passwd (format "Password for %s: " id)))
                                                  "device_id" device-id
                                                  "initial_device_display_name" initial-device-display-name)))
                    ;; TODO: Clear password in callback (if we decide to hold on to it for retrying login timeouts).
                    (ement-api session "login" :method 'post :data (json-encode data)
                      :then (apply-partially #'ement--login-callback session))
                    (ement-message "Logging in with password...")))
                (sso-filter (process string)
                  ;; NOTE: This is technically wrong, because it's not guaranteed that the
                  ;; string will be a complete request--it could just be a chunk.  But in
                  ;; practice, if this works, it's much simpler than setting up process log
                  ;; functions and per-client buffers for this throwaway, pretend HTTP server.
                  (when (string-match (rx "GET /?loginToken=" (group (0+ nonl)) " " (0+ nonl)) string)
                    (unwind-protect
                        (pcase-let* ((token (match-string 1 string))
                                     ((cl-struct ement-session user device-id initial-device-display-name)
                                      session)
                                     ((cl-struct ement-user id) user)
                                     (data (ement-alist
                                            "type" "m.login.token"
                                            "identifier" (ement-alist "type" "m.id.user"
                                                                      "user" id)
                                            "token" token
                                            "device_id" device-id
                                            "initial_device_display_name" initial-device-display-name)))
                          (ement-api session "login" :method 'post
                            :data (json-encode data)
                            :then (apply-partially #'ement--login-callback session))
                          (process-send-string process "HTTP/1.0 202 Accepted
Content-Type: text/plain; charset=utf-8

Ement: SSO login accepted; session token received.  Connecting to Matrix server.  (You may close this page.)")
                          (process-send-eof process))
                      (delete-process sso-server-process)
                      (delete-process process))))
                (sso-login ()
                  (setf sso-server-process
                        (make-network-process
                         :name "ement-sso" :family 'ipv4 :host 'local :service ement-sso-server-port
                         :filter #'sso-filter :server t :noquery t))
                  ;; Kill server after 2 minutes in case of problems.
                  (run-at-time 120 nil (lambda ()
                                         (when (process-live-p sso-server-process)
                                           (delete-process sso-server-process))))
                  (let ((url (concat (ement-server-uri-prefix (ement-session-server session))
                                     "/_matrix/client/r0/login/sso/redirect?redirectUrl=http://localhost:"
                                     (number-to-string ement-sso-server-port))))
                    (funcall browse-url-secondary-browser-function url)
                    (message "Browsing to single sign-on page <%s>..." url)))
                (flows-callback (data)
                  (let ((flows (cl-loop for flow across (map-elt data 'flows)
                                        for type = (map-elt flow 'type)
                                        when (member type '("m.login.password" "m.login.sso"))
                                        collect type)))
                    (pcase (length flows)
                      (0 (error "Ement: No supported login flows:  Server:%S  Supported flows:%S"
                                (ement-server-uri-prefix (ement-session-server session))
                                (map-elt data 'flows)))
                      (1 (pcase (car flows)
                           ("m.login.password" (password-login))
                           ("m.login.sso" (sso-login))
                           (_ (error "Ement: Unsupported login flow: %s  Server:%S  Supported flows:%S"
                                     (car flows) (ement-server-uri-prefix (ement-session-server session))
                                     (map-elt data 'flows)))))
                      (_ (pcase (completing-read "Select authentication method: "
                                                 (cl-loop for flow in flows
                                                          collect (string-trim-left flow (rx "m.login."))))
                           ("password" (password-login))
                           ("sso" (sso-login))
                           (else (error "Ement: Unsupported login flow:%S  Server:%S  Supported flows:%S"
                                        else (ement-server-uri-prefix (ement-session-server session))
                                        (map-elt data 'flows)))))))))
      (if session
          ;; Start syncing given session.
          (let ((user-id (ement-user-id (ement-session-user session))))
            ;; HACK: If session is already in ement-sessions, this replaces it.  I think that's okay...
            (setf (alist-get user-id ement-sessions nil nil #'equal) session)
            (ement--sync session :timeout ement-initial-sync-timeout))
        ;; Start password login flow.  Prompt for user ID and password
        ;; if not given (i.e. if not called interactively.)
        (unless user-id
          (setf user-id (read-string "User ID: " nil 'ement-connect-user-id-history)))
        (setf session (new-session))
        (when (ement-api session "login" :then #'flows-callback)
          (message "Ement: Checking server's login flows..."))))))

(defun ement-disconnect (sessions)
  "Disconnect from SESSIONS.
Interactively, with prefix, disconnect from all sessions.  If
`ement-auto-sync' is enabled, stop syncing, and clear the session
data.  When enabled, write the session to disk.  Any existing
room buffers are left alive and can be read, but other commands
in them won't work."
  (interactive (list (if current-prefix-arg
                         (mapcar #'cdr ement-sessions)
                       (list (ement-complete-session)))))
  (when ement-save-sessions
    ;; Write sessions before we remove them from the variable.
    (ement--write-sessions ement-sessions))
  (dolist (session sessions)
    (let ((user-id (ement-user-id (ement-session-user session))))
      (when-let ((process (map-elt ement-syncs session)))
        (ignore-errors
          (delete-process process)))
      ;; NOTE: I'd like to use `map-elt' here, but not until
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47368> is fixed, I guess.
      (setf (alist-get session ement-syncs nil nil #'equal) nil
            (alist-get user-id ement-sessions nil 'remove #'equal) nil)))
  (unless ement-sessions
    ;; HACK: If no sessions remain, clear the users table.  It might be best
    ;; to store a per-session users table, but this is probably good enough.
    (clrhash ement-users))
  (run-hooks 'ement-disconnect-hook)
  (message "Ement: Disconnected (%s)"
           (string-join (cl-loop for session in sessions
                                 collect (ement-user-id (ement-session-user session)))
                        ", ")))

(defun ement-kill-buffers ()
  "Kill all Ement buffers.
Useful in, e.g. `ement-disconnect-hook', which see."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "ement-" (symbol-name (buffer-local-value 'major-mode buffer)))
      (kill-buffer buffer))))

(defun ement--login-callback (session data)
  "Record DATA from logging in to SESSION and do initial sync."
  (pcase-let* (((cl-struct ement-session (user (cl-struct ement-user (id user-id)))) session)
               ((map ('access_token token) ('device_id device-id)) data))
    (setf (ement-session-token session) token
          (ement-session-device-id session) device-id
          (alist-get user-id ement-sessions nil nil #'equal) session)
    (ement--sync session :timeout ement-initial-sync-timeout)))

;;;; Functions

(defun ement-interrupted-sync-warning (session)
  "Display a warning that syncing of SESSION was interrupted."
  (display-warning
   'ement
   (format
    (substitute-command-keys
     "\\<ement-room-mode-map>Syncing of session <%s> was interrupted.  Use command `ement-room-sync' in a room buffer to retry.")
    (ement-user-id (ement-session-user session)))
   :error))

(defun ement-interrupted-sync-message (session)
  "Display a message that syncing of SESSION was interrupted."
  (message
   (substitute-command-keys
    "\\<ement-room-mode-map>Syncing of session <%s> was interrupted.  Use command `ement-room-sync' in a room buffer to retry.")
   (ement-user-id (ement-session-user session))))

(defun ement--run-idle-timer (&rest _ignore)
  "Run idle timer that updates read receipts.
To be called from `ement-after-initial-sync-hook'.  Timer is
stored in `ement-read-receipt-idle-timer'."
  (unless (timerp ement-read-receipt-idle-timer)
    (setf ement-read-receipt-idle-timer (run-with-idle-timer 3 t #'ement-room-read-receipt-idle-timer))))

(defun ement--stop-idle-timer (&rest _ignore)
  "Stop idle timer stored in `ement-read-receipt-idle-timer'.
To be called from `ement-disconnect-hook'."
  (unless ement-sessions
    (when (timerp ement-read-receipt-idle-timer)
      (cancel-timer ement-read-receipt-idle-timer)
      (setf ement-read-receipt-idle-timer nil))))

(defun ement-view-initial-rooms (session)
  "View rooms for SESSION configured in `ement-auto-view-rooms'."
  (when-let (rooms (alist-get (ement-user-id (ement-session-user session))
			      ement-auto-view-rooms nil nil #'equal))
    (dolist (alias/id rooms)
      (when-let (room (cl-find-if (lambda (room)
				    (or (equal alias/id (ement-room-canonical-alias room))
					(equal alias/id (ement-room-id room))))
				  (ement-session-rooms session)))
        (let ((ement-view-room-display-buffer-action ement-auto-view-room-display-buffer-action))
          (ement-view-room room session))))))

(defun ement--initial-transaction-id ()
  "Return an initial transaction ID for a new session."
  ;; We generate a somewhat-random initial transaction ID to avoid potential conflicts in
  ;; case, e.g. using Pantalaimon causes a transaction ID conflict.  See
  ;; <https://github.com/alphapapa/ement.el/issues/36>.
  (cl-parse-integer
   (secure-hash 'sha256 (prin1-to-string (list (current-time) (system-name))))
   :end 8 :radix 16))

(defsubst ement--sync-messages-p (session)
  "Return non-nil if sync-related messages should be shown for SESSION."
  ;; For now, this seems like the best way.
  (or (not (ement-session-has-synced-p session))
      (not ement-auto-sync)))

(defun ement--hostname-uri (hostname)
  "Return the \".well-known\" URI for server HOSTNAME.
If no URI is found, prompt the user for the hostname."
  ;; FIXME: When fail-prompting, a URI should be returned, not just a hostname.
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id178> ("4.1   Well-known URI")
  (cl-labels ((fail-prompt ()
                (let ((input (read-string "Auto-discovery of server's well-known URI failed.  Input server hostname, or leave blank to use server name: ")))
                  (pcase input
                    ("" hostname)
                    (_ input))))
              (parse (string)
                (if-let* ((object (ignore-errors (json-read-from-string string)))
                          (url (map-nested-elt object '(m.homeserver base_url)))
                          ((string-match-p
                            (rx bos "http" (optional "s") "://" (1+ nonl))
                            url)))
                    url
                  ;; Parsing error: FAIL_PROMPT.
                  (fail-prompt))))
    (condition-case err
        (let ((response (plz 'get (concat "https://" hostname "/.well-known/matrix/client")
                          :as 'response :then 'sync)))
          (if (plz-response-p response)
              (pcase (plz-response-status response)
                (200 (parse (plz-response-body response)))
                (404 (fail-prompt))
                (_ (warn "Ement: `plz' request for .well-known URI returned unexpected code: %s"
                         (plz-response-status response))
                   (fail-prompt)))
            (warn "Ement: `plz' request for .well-known URI did not return a `plz' response")
            (fail-prompt)))
      (error (warn "Ement: `plz' request for .well-known URI signaled an error: %S" err)
             (fail-prompt)))))

(cl-defun ement--sync (session &key force quiet
                               (timeout 40) ;; Give the server an extra 10 seconds.
                               (filter ement-default-sync-filter))
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used.  If FORCE, first
delete any outstanding sync processes.  If QUIET, don't show a
message about syncing this time.  Cancel request after TIMEOUT
seconds.

FILTER may be an alist representing a raw event filter (i.e. not
a filter ID).  When unspecified, the value of
`ement-default-sync-filter' is used.  The filter is encoded with
`json-encode'.  To use no filter, specify FILTER as nil."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Use a filter ID for default filter.
  ;; TODO: Optionally, automatically sync again when HTTP request fails.
  ;; TODO: Ensure that the process in (map-elt ement-syncs session) is live.
  (when (map-elt ement-syncs session)
    (if force
        (condition-case err
            (delete-process (map-elt ement-syncs session))
          ;; Ensure the only error is the expected one from deleting the process.
          (ement-api-error (cl-assert (equal "curl process killed" (plz-error-message (cl-third err))))
                           (message "Ement: Forcing new sync")))
      (user-error "Ement: Already syncing this session")))
  (pcase-let* (((cl-struct ement-session next-batch) session)
               (params (remove
                        nil (list (list "full_state" (if next-batch "false" "true"))
                                  (when filter
                                    ;; TODO: Document filter arg.
                                    (list "filter" (json-encode filter)))
                                  (when next-batch
                                    (list "since" next-batch))
                                  (when next-batch
                                    (list "timeout" "30000")))))
               (sync-start-time (time-to-seconds))
               ;; FIXME: Auto-sync again in error handler.
               (process (ement-api session "sync" :params params
                          :timeout timeout
                          :then (apply-partially #'ement--sync-callback session)
                          :else (lambda (plz-error)
                                  (setf (map-elt ement-syncs session) nil)
                                  ;; TODO: plz probably needs nicer error handling.
                                  ;; Ideally we would use `condition-case', but since the
                                  ;; error is signaled in `plz--sentinel'...
                                  (pcase-let (((cl-struct plz-error curl-error response) plz-error)
                                              (reason))
                                    (cond ((when response
                                             (pcase (plz-response-status response)
                                               ((or 429 502) (setf reason "failed")))))
                                          ((pcase curl-error
                                             (`(28 . ,_) (setf reason "timed out")))))
                                    (if reason
                                        (if (not ement-auto-sync)
                                            (run-hook-with-args 'ement-interrupted-sync-hook session)
                                          (message "Ement: Sync %s (%s).  Syncing again..."
                                                   reason (ement-user-id (ement-session-user session)))
                                          ;; Set QUIET to allow the just-printed message to remain visible.
                                          (ement--sync session :timeout timeout :quiet t))
                                      ;; Unrecognized errors:
                                      (pcase curl-error
                                        (`(,code . ,message)
                                         (signal 'ement-api-error (list (format "Ement: Network error: %s: %s" code message)
                                                                        plz-error)))
                                        (_ (signal 'ement-api-error (list "Ement: Unrecognized network error" plz-error)))))))
                          :json-read-fn (lambda ()
                                          "Print a message, then call `ement--json-parse-buffer'."
                                          (when (ement--sync-messages-p session)
                                            (message "Ement: Response arrived after %.2f seconds.  Reading %s JSON response..."
                                                     (- (time-to-seconds) sync-start-time)
                                                     (file-size-human-readable (buffer-size))))
                                          (let ((start-time (time-to-seconds)))
                                            (prog1 (ement--json-parse-buffer)
                                              (when (ement--sync-messages-p session)
                                                (message "Ement: Reading JSON took %.2f seconds"
                                                         (- (time-to-seconds) start-time)))))))))
    (when process
      (setf (map-elt ement-syncs session) process)
      (when (and (not quiet) (ement--sync-messages-p session))
        (ement-message "Sync request sent.  Waiting for response...")))))

(defun ement--sync-callback (session data)
  "Process sync DATA for SESSION.
Runs `ement-sync-callback-hook' with SESSION."
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt ement-syncs session) nil)
  (pcase-let* (((map rooms ('next_batch next-batch) ('account_data (map ('events account-data-events))))
                data)
               ((map ('join joined-rooms) ('invite invited-rooms) ('leave left-rooms)) rooms)
               (num-events (+
                            ;; HACK: In `ement--push-joined-room-events', we do something
                            ;; with each event 3 times, so we multiply this by 3.
                            ;; FIXME: That calculation doesn't seem to be quite right, because
                            ;; the progress reporter never seems to hit 100% before it's done.
                            (* 3 (cl-loop for (_id . room) in joined-rooms
                                          sum (length (map-nested-elt room '(state events)))
                                          sum (length (map-nested-elt room '(timeline events)))))
                            (cl-loop for (_id . room) in invited-rooms
                                     sum (length (map-nested-elt room '(invite_state events)))))))
    ;; Append account data events.
    ;; TODO: Since only one event of each type is allowed in account data (the spec
    ;; doesn't seem to make this clear, but see
    ;; <https://github.com/matrix-org/matrix-js-sdk/blob/d0b964837f2820940bd93e718a2450b5f528bffc/src/store/memory.ts#L292>),
    ;; we should store account-data events in a hash table or alist rather than just a
    ;; list of events.
    (cl-callf2 append (cl-coerce account-data-events 'list) (ement-session-account-data session))
    ;; Process invited and joined rooms.
    (ement-with-progress-reporter (:when (ement--sync-messages-p session)
                                         :reporter ("Ement: Reading events..." 0 num-events))
      ;; Left rooms.
      (mapc (apply-partially #'ement--push-left-room-events session) left-rooms)
      ;; Invited rooms.
      (mapc (apply-partially #'ement--push-invite-room-events session) invited-rooms)
      ;; Joined rooms.
      (mapc (apply-partially #'ement--push-joined-room-events session) joined-rooms))
    ;; TODO: Process "left" rooms (remove room structs, etc).
    ;; NOTE: We update the next-batch token before updating any room buffers.  This means
    ;; that any errors in updating room buffers (like for unexpected event formats that
    ;; expose a bug) could cause events to not appear in the buffer, but the user could
    ;; still dismiss the error and start syncing again, and the client could remain
    ;; usable.  Updating the token after doing everything would be preferable in some
    ;; ways, but it would mean that an event that exposes a bug would be processed again
    ;; on every sync, causing the same error each time.  It would seem preferable to
    ;; maintain at least some usability rather than to keep repeating a broken behavior.
    (setf (ement-session-next-batch session) next-batch)
    ;; Run hooks which update buffers, etc.
    (run-hook-with-args 'ement-sync-callback-hook session)
    ;; Show sync message if appropriate, and run after-initial-sync-hook.
    (when (ement--sync-messages-p session)
      (message (concat "Ement: Sync done."
                       (unless (ement-session-has-synced-p session)
                         (run-hook-with-args 'ement-after-initial-sync-hook session)
                         ;; Show tip after initial sync.
                         (setf (ement-session-has-synced-p session) t)
                         "  Use commands `ement-list-rooms' or `ement-view-room' to view a room."))))))

(defun ement--push-invite-room-events (session invited-room)
  "Push events for INVITED-ROOM into that room in SESSION."
  ;; TODO: Make ement-session-rooms a hash-table.
  (ement--push-joined-room-events session invited-room 'invite))

(defun ement--auto-sync (session)
  "If `ement-auto-sync' is non-nil, sync SESSION again."
  (when ement-auto-sync
    (ement--sync session)))

(defun ement--update-room-buffers (session)
  "Insert new events into SESSION's rooms which have buffers.
To be called in `ement-sync-callback-hook'."
  ;; TODO: Move this to ement-room.el, probably.
  ;; For now, we primitively iterate over the buffer list to find ones
  ;; whose mode is `ement-room-mode'.
  (let* ((buffers (cl-loop for room in (ement-session-rooms session)
                           for buffer = (map-elt (ement-room-local room) 'buffer)
                           when (buffer-live-p buffer)
                           collect buffer)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (save-window-excursion
          ;; NOTE: When the buffer has a window, it must be the selected one
          ;; while calling event-insertion functions.  I don't know if this is
          ;; due to a bug in EWOC or if I just misunderstand something, but
          ;; without doing this, events may be inserted at the wrong place.
          (when-let ((buffer-window (get-buffer-window buffer)))
            (select-window buffer-window))
          (cl-assert ement-room)
          (when (ement-room-ephemeral ement-room)
            ;; Ephemeral events.
            (ement-room--process-events (ement-room-ephemeral ement-room))
            (setf (ement-room-ephemeral ement-room) nil))
          (when-let ((new-events (alist-get 'new-events (ement-room-local ement-room))))
            ;; HACK: Process these events in reverse order, so that later events (like reactions)
            ;; which refer to earlier events can find them.  (Not sure if still necessary.)
            (ement-room--process-events (reverse new-events))
            (setf (alist-get 'new-events (ement-room-local ement-room)) nil))
          (when-let ((new-events (alist-get 'new-account-data-events (ement-room-local ement-room))))
            ;; Account data events.  Do this last so, e.g. read markers can refer to message events we've seen.
            (ement-room--process-events new-events)
            (setf (alist-get 'new-account-data-events (ement-room-local ement-room)) nil)))))))

(cl-defun ement--push-joined-room-events (session joined-room &optional (status 'join))
  "Push events for JOINED-ROOM into that room in SESSION.
Also used for left rooms, in which case STATUS should be set to
`leave'."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id)) ; Really important that the ID is a STRING!
               ;; TODO: Make ement-session-rooms a hash-table.
               (room (or (cl-find-if (lambda (room)
                                       (equal id (ement-room-id room)))
                                     (ement-session-rooms session))
                         (car (push (make-ement-room :id id) (ement-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('invite_state (map ('events invite-state-events)))
                     ('account_data (map ('events account-data-events)))
                     ('unread_notifications unread-notifications))
                event-types)
               (latest-timestamp))
    (setf (ement-room-status room) status
          (ement-room-unread-notifications room) unread-notifications)
    ;; NOTE: The idea is that, assuming that events in the sync reponse are in
    ;; chronological order, we push them to the lists in the room slots in that order,
    ;; leaving the head of each list as the most recent event of that type.  That means
    ;; that, e.g. the room state events may be searched in order to find, e.g. the most
    ;; recent room name event.  However, chronological order is not guaranteed, e.g. after
    ;; loading older messages (the "retro" function; this behavior is in development).

    ;; MAYBE: Use queue.el to store the events in a DLL, so they could
    ;; be accessed from either end.  Could be useful.

    ;; Push the StrippedState events to the room's invite-state.  (These events have no
    ;; timestamp data.)  We also run the event hook, because for invited rooms, the
    ;; invite-state events include room name, topic, etc.
    (cl-loop for event across-ref invite-state-events do
             (setf event (ement--make-event event))
             (push event (ement-room-invite-state room))
             (run-hook-with-args 'ement-event-hook event room session))

    ;; Save room summary.
    (dolist (parameter '(m.heroes m.joined_member_count m.invited_member_count))
      (when (alist-get parameter summary)
        ;; These fields are only included when they change.
        (setf (alist-get parameter (ement-room-summary room)) (alist-get parameter summary))))

    ;; Update account data.  According to the spec, only one of each event type is
    ;; supposed to be present in a room's account data, so we store them as an alist keyed
    ;; on their type.  (NOTE: We don't currently make them into event structs, but maybe
    ;; we should in the future.)
    (cl-loop for event across account-data-events
             for type = (alist-get 'type event)
             do (setf (alist-get type (ement-room-account-data room) nil nil #'equal) event))
    ;; But we also need to track just the new events so we can process those in a room
    ;; buffer (and for some reason, we do make them into structs here, but I don't
    ;; remember why).  FIXME: Unify this.
    (cl-callf2 append (mapcar #'ement--make-event account-data-events)
               (alist-get 'new-account-data-events (ement-room-local room)))

    ;; Save state and timeline events.
    (cl-macrolet ((push-events (type accessor)
                    ;; Push new events of TYPE to room's slot of ACCESSOR, and return the latest timestamp pushed.
                    `(let ((ts 0))
                       ;; NOTE: We replace each event in the vector with the
                       ;; struct, which is used when calling hooks later.
                       (cl-loop for event across-ref (alist-get 'events ,type)
                                do (setf event (ement--make-event event))
                                do (push event (,accessor room))
                                (when (ement--sync-messages-p session)
                                  (ement-progress-update))
                                (when (> (ement-event-origin-server-ts event) ts)
                                  (setf ts (ement-event-origin-server-ts event))))
                       ;; One would think that one should use `maximizing' here, but, completely
                       ;; inexplicably, it sometimes returns nil, even when every single value it's comparing
                       ;; is a number.  It's absolutely bizarre, but I have to do the equivalent manually.
                       ts)))
      ;; FIXME: This is a bit convoluted and hacky now.  Refactor it.
      (setf latest-timestamp
            (max (push-events state ement-room-state)
                 (push-events timeline ement-room-timeline)))
      ;; NOTE: We also append the new events to the new-events list in the room's local
      ;; slot, which is used by `ement--update-room-buffers' to insert only new events.
      ;; FIXME: Does this also need to be done for invite-state events?
      (cl-callf2 append (cl-coerce (alist-get 'events timeline) 'list)
                 (alist-get 'new-events (ement-room-local room)))
      ;; Update room's latest-timestamp slot.
      (when (> latest-timestamp (or (ement-room-latest-ts room) 0))
        (setf (ement-room-latest-ts room) latest-timestamp))
      (unless (ement-session-has-synced-p session)
        ;; Only set this token on initial sync, otherwise it would
        ;; overwrite earlier tokens from loading earlier messages.
        (setf (ement-room-prev-batch room) (alist-get 'prev_batch timeline))))
    ;; Run event hook for state and timeline events.
    (cl-loop for event across (alist-get 'events state)
             do (run-hook-with-args 'ement-event-hook event room session)
             (when (ement--sync-messages-p session)
               (ement-progress-update)))
    (cl-loop for event across (alist-get 'events timeline)
             do (run-hook-with-args 'ement-event-hook event room session)
             (when (ement--sync-messages-p session)
               (ement-progress-update)))
    ;; Ephemeral events (do this after state and timeline hooks, so those events will be
    ;; in the hash tables).
    (cl-loop for event across (alist-get 'events ephemeral)
             for event-struct = (ement--make-event event)
             do (push event-struct (ement-room-ephemeral room))
             (ement--process-event event-struct room session))
    (when (ement-session-has-synced-p session)
      ;; NOTE: We don't fill gaps in "limited" requests on initial
      ;; sync, only in subsequent syncs, e.g. after the system has
      ;; slept and awakened.
      ;; NOTE: When not limited, the read value is `:json-false', so
      ;; we must explicitly compare to t.
      (when (eq t (alist-get 'limited timeline))
	;; Timeline was limited: start filling gap.  We start the
	;; gap-filling, retrieving up to the session's current
	;; next-batch token (this function is not called when retrieving
	;; older messages, so the session's next-batch token is only
	;; evaluated once, when this chain begins, and then that token
	;; is passed to repeated calls to `ement-room-retro-to-token'
	;; until the gap is filled).
	(ement-room-retro-to-token room session (alist-get 'prev_batch timeline)
				   (ement-session-next-batch session))))))

(defun ement--push-left-room-events (session left-room)
  "Push events for LEFT-ROOM into that room in SESSION."
  (ement--push-joined-room-events session left-room 'leave))

(defun ement--make-event (event)
  "Return `ement-event' struct for raw EVENT list.
Adds sender to `ement-users' when necessary."
  (pcase-let* (((map content type unsigned redacts
                     ('event_id id) ('origin_server_ts ts)
                     ('sender sender-id) ('state_key state-key))
                event)
               (sender (or (gethash sender-id ement-users)
                           (puthash sender-id (make-ement-user :id sender-id)
                                    ement-users))))
    ;; MAYBE: Handle other keys in the event, such as "room_id" in "invite" events.
    (make-ement-event :id id :sender sender :type type :content content :state-key state-key
                      :origin-server-ts ts :unsigned unsigned
                      ;; Since very few events will be redactions and have this key, we
                      ;; record it in the local slot alist rather than as another slot on
                      ;; the struct.
                      :local (when redacts
                               (ement-alist 'redacts redacts)))))

(defun ement--put-event (event _room session)
  "Put EVENT on SESSION's events table."
  (puthash (ement-event-id event) event (ement-session-events session)))

;; FIXME: These functions probably need to compare timestamps to
;; ensure that older events that are inserted at the head of the
;; events lists aren't used instead of newer ones.

;; TODO: These two functions should be folded into event handlers.

;;;;; Reading/writing sessions

(defun ement--read-sessions ()
  "Return saved sessions alist read from disk.
Returns nil if unable to read `ement-sessions-file'."
  (cl-labels ((plist-to-session (plist)
                (pcase-let* (((map (:user user-data) (:server server-data)
                                   (:token token) (:transaction-id transaction-id))
                              plist)
                             (user (apply #'make-ement-user user-data))
                             (server (apply #'make-ement-server server-data))
                             (session (make-ement-session :user user :server server
                                                          :token token :transaction-id transaction-id)))
                  (setf (ement-session-events session) (make-hash-table :test #'equal))
                  session)))
    (when (file-exists-p ement-sessions-file)
      (pcase-let* ((read-circle t)
                   (sessions (with-temp-buffer
                               (insert-file-contents ement-sessions-file)
                               (read (current-buffer)))))
        (prog1
            (cl-loop for (id . plist) in sessions
                     collect (cons id (plist-to-session plist)))
          (message "Ement: Read sessions."))))))

(defun ement--write-sessions (sessions-alist)
  "Write SESSIONS-ALIST to disk."
  ;; We only record the slots we need.  We record them as a plist
  ;; so that changes to the struct definition don't matter.
  ;; NOTE: If we ever persist more session data (like room data, so we
  ;; could avoid doing an initial sync next time), we should limit the
  ;; amount of session data saved (e.g. room history could grow
  ;; forever on-disk, which probably isn't what we want).

  ;; NOTE: This writes all current sessions, even if there are multiple active ones and only one
  ;; is being disconnected.  That's probably okay, but it might be something to keep in mind.
  (cl-labels ((session-plist (session)
                (pcase-let* (((cl-struct ement-session user server token transaction-id) session)
                             ((cl-struct ement-user (id user-id) username) user)
                             ((cl-struct ement-server (name server-name) uri-prefix) server))
                  (list :user (list :id user-id
                                    :username username)
                        :server (list :name server-name
                                      :uri-prefix uri-prefix)
                        :token token
                        :transaction-id transaction-id))))
    (message "Ement: Writing sessions...")
    (with-temp-file ement-sessions-file
      (pcase-let* ((print-level nil)
                   (print-length nil)
                   ;; Very important to use `print-circle', although it doesn't
                   ;; solve everything.  Writing/reading Lisp data can be tricky...
                   (print-circle t)
                   (sessions-alist-plist (cl-loop for (id . session) in sessions-alist
                                                  collect (cons id (session-plist session)))))
        (prin1 sessions-alist-plist (current-buffer))))
    ;; Ensure permissions are safe.
    (chmod ement-sessions-file #o600)))

(defun ement--kill-emacs-hook ()
  "Function to be added to `kill-emacs-hook'.
Writes Ement session to disk when enabled."
  (ignore-errors
    ;; To avoid interfering with Emacs' exit, We must be careful that
    ;; this function handles errors, so just ignore any.
    (when (and ement-save-sessions
               ement-sessions)
      (ement--write-sessions ement-sessions))))

;;;;; Event handlers

(defvar ement-event-handlers nil
  "Alist mapping event types to functions which process an event of each type.
Each function is called with three arguments: the event, the
room, and the session.  These handlers are run regardless of
whether a room has a live buffer.")

(defun ement--process-event (event room session)
  "Process EVENT for ROOM in SESSION.
Uses handlers defined in `ement-event-handlers'.  If no handler
is defined for EVENT's type, does nothing and returns nil.  Any
errors signaled during processing are demoted in order to prevent
unexpected errors from arresting event processing and syncing."
  (when-let ((handler (alist-get (ement-event-type event) ement-event-handlers nil nil #'equal)))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.  See,
    ;; e.g. <https://github.com/alphapapa/ement.el/pull/61>.
    (with-demoted-errors "Ement (ement--process-event): Error processing event: %S"
      (funcall handler event room session))))

(defmacro ement-defevent (type &rest body)
  "Define an event handling function for events of TYPE, a string.
Around the BODY, the variable `event' is bound to the event being
processed, `room' to the room struct in which the event occurred,
and `session' to the session.  Adds function to
`ement-event-handlers', which see."
  (declare (indent defun))
  `(setf (alist-get ,type ement-event-handlers nil nil #'string=)
         (lambda (event room session)
           ,(concat "`ement-' handler function for " type " events.")
           ,@body)))

;; I love how Lisp macros make it so easy and concise to define these
;; event handlers!

(ement-defevent "m.room.avatar"
  (when ement-room-avatars
    ;; If room avatars are disabled, we don't download avatars at all.  This
    ;; means that, if a user has them disabled and then reenables them, they will
    ;; likely need to reconnect to cause them to be displayed in most rooms.
    (if-let ((url (alist-get 'url (ement-event-content event))))
        (plz-run
         (plz-queue ement-images-queue
           'get (ement--mxc-to-url url session) :as 'binary :noquery t
           :then (lambda (data)
                   (when ement-room-avatars
                     ;; MAYBE: Store the raw image data instead of using create-image here.
                     (let ((image (create-image data nil 'data-p
                                                :ascent 'center
                                                :max-width ement-room-avatar-max-width
                                                :max-height ement-room-avatar-max-height)))
                       (if (not image)
                           (progn
                             (display-warning 'ement (format "Room avatar seems unreadable:  ROOM-ID:%S  AVATAR-URL:%S"
                                                             (ement-room-id room) (ement--mxc-to-url url session)))
                             (setf (ement-room-avatar room) nil
                                   (alist-get 'room-list-avatar (ement-room-local room)) nil))
                         (when (fboundp 'imagemagick-types)
                           ;; Only do this when ImageMagick is supported.
                           ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                           (setf (image-property image :type) 'imagemagick))
                         ;; We set the room-avatar slot to a propertized string that
                         ;; displays as the image.  This seems the most convenient thing to
                         ;; do.  We also unset the cached room-list-avatar so it can be
                         ;; remade.
                         (setf (ement-room-avatar room) (propertize " " 'display image)
                               (alist-get 'room-list-avatar (ement-room-local room)) nil)))))))
      ;; Unset avatar.
      (setf (ement-room-avatar room) nil
            (alist-get 'room-list-avatar (ement-room-local room)) nil))))

(ement-defevent "m.room.create"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map type))) event))
    (when type
      (setf (ement-room-type room) type))))

(ement-defevent "m.room.member"
  "Put/update member on `ement-users' and room's members table."
  (ignore session)
  (pcase-let* (((cl-struct ement-room members) room)
               ((cl-struct ement-event state-key
                           (content (map displayname membership
                                         ('avatar_url avatar-url))))
                event)
               (user (or (gethash state-key ement-users)
                         (puthash state-key
                                  (make-ement-user :id state-key :avatar-url avatar-url
                                                   ;; NOTE: The spec doesn't seem to say whether the
                                                   ;; displayname in the member event applies only to the
                                                   ;; room or is for the user generally, so we'll save it
                                                   ;; in the struct anyway.
                                                   :displayname displayname)
                                  ement-users))))
    (pcase membership
      ("join"
       (puthash state-key user members)
       (puthash user displayname (ement-room-displaynames room)))
      (_ (remhash state-key members)
         (remhash user (ement-room-displaynames room))))))

(ement-defevent "m.room.name"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map name))) event))
    (when name
      ;; Recalculate room name and cache in slot.
      (setf (ement-room-display-name room) (ement--room-display-name room)))))

(ement-defevent "m.room.topic"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map topic))) event))
    (when topic
      (setf (ement-room-topic room) topic))))

(ement-defevent "m.receipt"
  (ignore session)
  (pcase-let (((cl-struct ement-event content) event)
              ((cl-struct ement-room (receipts room-receipts)) room))
    (cl-loop for (event-id . receipts) in content
             do (cl-loop for (user-id . receipt) in (alist-get 'm.read receipts)
                         ;; Users may not have been "seen" yet, so although we'd
                         ;; prefer to key on the user struct, we key on the user ID.
                         ;; Same for events, unfortunately.
                         ;; NOTE: The JSON map keys are converted to symbols by `json-read'.
                         ;; MAYBE: (Should we keep them that way?  It would use less memory, I guess.)
                         do (puthash (symbol-name user-id)
                                     (cons (symbol-name event-id) (alist-get 'ts receipt))
                                     room-receipts)))))

(ement-defevent "m.space.child"
  ;; SPEC: v1.2/11.35.
  (pcase-let* ((space-room room)
               ((cl-struct ement-session rooms) session)
               ((cl-struct ement-room (id parent-room-id)) space-room)
               ((cl-struct ement-event (state-key child-room-id) (content (map via))) event)
               (child-room (cl-find child-room-id rooms :key #'ement-room-id :test #'equal)))
    (if via
        ;; Child being declared: add it.
        (progn
          (cl-pushnew child-room-id (alist-get 'children (ement-room-local space-room)) :test #'equal)
          (when child-room
            ;; The user is also in the child room: link the parent space-room in it.
            ;; FIXME: On initial sync, if the child room hasn't been processed yet, this will fail.
            (cl-pushnew parent-room-id (alist-get 'parents (ement-room-local child-room)) :test #'equal)))
      ;; Child being disowned: remove it.
      (setf (alist-get 'children (ement-room-local space-room))
            (delete child-room-id (alist-get 'children (ement-room-local space-room))))
      (when child-room
        ;; The user is also in the child room: unlink the parent space-room in it.
        (setf (alist-get 'parents (ement-room-local child-room))
              (delete parent-room-id (alist-get 'parents (ement-room-local child-room))))))))

(ement-defevent "m.room.canonical_alias"
  (ignore session)
  (pcase-let (((cl-struct ement-event (content (map alias))) event))
    (setf (ement-room-canonical-alias room) alias)))

(defun ement--link-children (session)
  "Link child rooms in SESSION.
To be called after initial sync."
  ;; On initial sync, when processing m.space.child events, the child rooms may not have
  ;; been processed yet, so we link them again here.
  (pcase-let (((cl-struct ement-session rooms) session))
    (dolist (room rooms)
      (pcase-let (((cl-struct ement-room (id parent-id) (local (map children))) room))
        (when children
          (dolist (child-id children)
            (when-let ((child-room (cl-find child-id rooms :key #'ement-room-id :test #'equal)))
              (cl-pushnew parent-id (alist-get 'parents (ement-room-local child-room)) :test #'equal))))))))

;;;;; Savehist compatibility

;; See <https://github.com/alphapapa/ement.el/issues/216>.

(defvar savehist-save-hook)

(with-eval-after-load 'savehist
  ;; TODO: Consider using a symbol property on our commands and checking that rather than
  ;; symbol names; would avoid consing.
  (defun ement--savehist-save-hook ()
    "Remove all `ement-' commands from `command-history'.
Because when `savehist' saves `command-history', it includes the
interactive arguments passed to the command, which in our case
includes large data structures that should never be persisted!"
    (setf command-history
          (cl-remove-if (pcase-lambda (`(,command . ,_))
                          (string-match-p (rx bos "ement-") (symbol-name command)))
                        command-history)))
  (cl-pushnew 'ement--savehist-save-hook savehist-save-hook))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
