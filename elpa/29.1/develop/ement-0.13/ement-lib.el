;;; ement-lib.el --- Library of Ement functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

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

;; This library provides functions used in other Ement libraries.  It exists so they may
;; be required where needed, without causing circular dependencies.

;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'eieio)
  (require 'ewoc)
  (require 'pcase)
  (require 'subr-x)
  
  (require 'taxy-magit-section)

  (require 'ement-macros))

(require 'cl-lib)

(require 'button)
(require 'color)
(require 'map)
(require 'seq)
(require 'xml)

(require 'ement-api)
(require 'ement-structs)

;;;; Variables

(defvar ement-sessions)
(defvar ement-users)
(defvar ement-ewoc)
(defvar ement-room)
(defvar ement-session)

(defvar ement-room-buffer-name-prefix)
(defvar ement-room-buffer-name-suffix)
(defvar ement-room-leave-kill-buffer)
(defvar ement-room-prism)
(defvar ement-room-prism-color-adjustment)
(defvar ement-room-prism-minimum-contrast)
(defvar ement-room-unread-only-counts-notifications)

;;;; Function declarations

;; Instead of using top-level `declare-function' forms (which can easily become obsolete
;; if not kept with the code that needs them), this allows the use of `(declare (function
;; ...))' forms in each function definition, so that if a function is moved or removed,
;; the `declare-function' goes with it.

;; TODO: Propose this upstream.

(eval-and-compile
  (defun ement--byte-run--declare-function (_name _args &rest values)
    "Return a `declare-function' form with VALUES.
Allows the use of a form like:

  (declare (function FN FILE ...))

inside of a function definition, effectively keeping its
`declare-function' form inside the function definition, ensuring
that stray such forms don't remain if the function is removed."
    `(declare-function ,@values))

  (cl-pushnew '(function ement--byte-run--declare-function) defun-declarations-alist :test #'equal)
  (cl-pushnew '(function ement--byte-run--declare-function) macro-declarations-alist :test #'equal))

;;;; Compatibility

;; These workarounds should be removed when they aren't needed.

(defalias 'ement--json-parse-buffer
  ;; For non-libjansson builds (those that do have libjansson will see a 4-5x improvement
  ;; in the time needed to parse JSON responses).

  ;; TODO: Suggest mentioning in manual and docstrings that `json-read', et al do not use
  ;; libjansson, while `json-parse-buffer', et al do.
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (condition-case err
            (json-parse-buffer :object-type 'alist :null-object nil :false-object :json-false)
          (json-parse-error
           (ement-message "`json-parse-buffer' signaled `json-parse-error'; falling back to `json-read'... (%S)"
                          (error-message-string err))
           (goto-char (point-min))
           (json-read))))
    'json-read))

;;;;; Emacs 28 color features.

;; Copied from Emacs 28.  See <https://github.com/alphapapa/ement.el/issues/99>.

;; TODO(future): Remove these workarounds when dropping support for Emacs <28.

(eval-and-compile
  (unless (boundp 'color-luminance-dark-limit)
    (defconst ement--color-luminance-dark-limit 0.325
      "The relative luminance below which a color is considered \"dark.\"
A \"dark\" color in this sense provides better contrast with
white than with black; see `color-dark-p'.  This value was
determined experimentally.")))

(defalias 'ement--color-dark-p
  (if (fboundp 'color-dark-p)
      'color-dark-p
    (with-suppressed-warnings ((free-vars ement--color-luminance-dark-limit))
      (lambda (rgb)
        "Whether RGB is more readable against white than black.
RGB is a 3-element list (R G B), each component in the range [0,1].
This predicate can be used both for determining a suitable (black or white)
contrast colour with RGB as background and as foreground."
        (unless (<= 0 (apply #'min rgb) (apply #'max rgb) 1)
          (error "RGB components %S not in [0,1]" rgb))
        ;; Compute the relative luminance after gamma-correcting (assuming sRGB),
        ;; and compare to a cut-off value determined experimentally.
        ;; See https://en.wikipedia.org/wiki/Relative_luminance for details.
        (let* ((sr (nth 0 rgb))
               (sg (nth 1 rgb))
               (sb (nth 2 rgb))
               ;; Gamma-correct the RGB components to linear values.
               ;; Use the power 2.2 as an approximation to sRGB gamma;
               ;; it should be good enough for the purpose of this function.
               (r (expt sr 2.2))
               (g (expt sg 2.2))
               (b (expt sb 2.2))
               (y (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))))
          (< y ement--color-luminance-dark-limit))))))

;;;; Functions

;;;;; Commands

(cl-defun ement-create-room
    (session &key name alias topic invite direct-p creation-content
             (then (lambda (data)
                     (message "Created new room: %s" (alist-get 'room_id data))))
             (visibility 'private))
  "Create new room on SESSION.
Then call function THEN with response data.  Optional string
arguments are NAME, ALIAS, and TOPIC.  INVITE may be a list of
user IDs to invite.  If DIRECT-P, set the \"is_direct\" flag in
the request.  CREATION-CONTENT may be an alist of extra keys to
include with the request (see Matrix spec)."
  ;; TODO: Document other arguments.
  ;; SPEC: 10.1.1.
  (declare (indent defun))
  (interactive (list (ement-complete-session)
		     :name (read-string "New room name: ")
		     :alias (read-string "New room alias (e.g. \"foo\" for \"#foo:matrix.org\"): ")
		     :topic (read-string "New room topic: ")
		     :visibility (completing-read "New room visibility: " '(private public))))
  (cl-labels ((given-p (var) (and var (not (string-empty-p var)))))
    (pcase-let* ((endpoint "createRoom")
		 (data (ement-aprog1
			   (ement-alist "visibility" visibility)
			 (when (given-p alias)
			   (push (cons "room_alias_name" alias) it))
			 (when (given-p name)
			   (push (cons "name" name) it))
			 (when (given-p topic)
			   (push (cons "topic" topic) it))
			 (when invite
			   (push (cons "invite" invite) it))
			 (when direct-p
			   (push (cons "is_direct" t) it))
                         (when creation-content
                           (push (cons "creation_content" creation-content) it)))))
      (ement-api session endpoint :method 'post :data (json-encode data)
        :then then))))

(cl-defun ement-create-space
    (session &key name alias topic
             (then (lambda (data)
                     (message "Created new space: %s" (alist-get 'room_id data))))
             (visibility 'private))
  "Create new space on SESSION.
Then call function THEN with response data.  Optional string
arguments are NAME, ALIAS, and TOPIC."
  (declare (indent defun))
  (interactive (list (ement-complete-session)
		     :name (read-string "New space name: ")
		     :alias (read-string "New space alias (e.g. \"foo\" for \"#foo:matrix.org\"): ")
		     :topic (read-string "New space topic: ")
		     :visibility (completing-read "New space visibility: " '(private public))))
  (ement-create-room session :name name :alias alias :topic topic :visibility visibility
    :creation-content (ement-alist "type" "m.space") :then then))

(defun ement-room-leave (room session &optional force-p)
  "Leave ROOM on SESSION.
If FORCE-P, leave without prompting.  ROOM may be an `ement-room'
struct, or a room ID or alias string."
  ;; TODO: Rename `room' argument to `room-or-id'.
  (interactive
   (ement-with-room-and-session
     :prompt-form (ement-complete-room :prompt "Leave room: ")
     (list ement-room ement-session)))
  (cl-etypecase room
    (ement-room)
    (string (setf room (ement-afirst (or (equal room (ement-room-canonical-alias it))
                                         (equal room (ement-room-id it)))
                         (ement-session-rooms session)))))
  (when (or force-p (yes-or-no-p (format "Leave room %s? " (ement--format-room room))))
    (pcase-let* (((cl-struct ement-room id) room)
                 (endpoint (format "rooms/%s/leave" (url-hexify-string id))))
      (ement-api session endpoint :method 'post :data ""
        :then (lambda (_data)
                (when ement-room-leave-kill-buffer
                  ;; NOTE: This generates a symbol and sets its function value to a lambda
                  ;; which removes the symbol from the hook, removing itself from the hook.
                  ;; TODO: When requiring Emacs 27, use `letrec'.
                  (let* ((leave-fn-symbol (gensym (format "ement-leave-%s" room)))
                         (leave-fn (lambda (_session)
                                     (remove-hook 'ement-sync-callback-hook leave-fn-symbol)
                                     ;; FIXME: Probably need to unintern the symbol.
                                     (when-let ((buffer (map-elt (ement-room-local room) 'buffer)))
                                       (when (buffer-live-p buffer)
                                         (kill-buffer buffer))))))
                    (setf (symbol-function leave-fn-symbol) leave-fn)
                    (add-hook 'ement-sync-callback-hook leave-fn-symbol)))
                (ement-message "Left room: %s" (ement--format-room room)))
        :else (lambda (plz-error)
                (pcase-let* (((cl-struct plz-error response) plz-error)
                             ((cl-struct plz-response status body) response)
                             ((map error) (json-read-from-string body)))
                  (pcase status
                    (429 (error "Unable to leave room %s: %s" room error))
                    (_ (error "Unable to leave room %s: %s %S" room status plz-error)))))))))
(defalias 'ement-leave-room #'ement-room-leave)

(defun ement-forget-room (room session &optional force-p)
  "Forget ROOM on SESSION.
If FORCE-P (interactively, with prefix), prompt to leave the room
when necessary, and forget the room without prompting."
  (interactive
   (ement-with-room-and-session
     :prompt-form (ement-complete-room :prompt "Forget room: ")
     (list ement-room ement-session current-prefix-arg)))
  (pcase-let* (((cl-struct ement-room id display-name status) room)
               (endpoint (format "rooms/%s/forget" (url-hexify-string id))))
    (pcase status
      ('join (if (and force-p
                      (yes-or-no-p (format "Leave and forget room %s? (WARNING: You will not be able to rejoin the room to access its content.) "
                                           (ement--format-room room))))
                 (progn
                   ;; TODO: Use `letrec'.
                   (let* ((forget-fn-symbol (gensym (format "ement-forget-%s" room)))
                          (forget-fn (lambda (_session)
                                       (when (equal 'leave (ement-room-status room))
                                         (remove-hook 'ement-sync-callback-hook forget-fn-symbol)
                                         ;; FIXME: Probably need to unintern the symbol.
                                         (ement-forget-room room session 'force)))))
                     (setf (symbol-function forget-fn-symbol) forget-fn)
                     (add-hook 'ement-sync-callback-hook forget-fn-symbol))
                   (ement-leave-room room session 'force))
               (user-error "Room %s is joined (must be left before forgetting)"
                           (ement--format-room room))))
      ('leave (when (or force-p (yes-or-no-p (format "Forget room \"%s\" (%s)? " display-name id)))
                (ement-api session endpoint :method 'post :data ""
                  :then (lambda (_data)
                          ;; NOTE: The spec does not seem to indicate that the action of forgetting
                          ;; a room is synced to other clients, so it seems that we need to remove
                          ;; the room from the session here.
                          (setf (ement-session-rooms session)
                                (cl-remove room (ement-session-rooms session)))
                          ;; TODO: Indicate forgotten in footer in room buffer.
                          (ement-message "Forgot room: %s." (ement--format-room room)))))))))

(defun ement-ignore-user (user-id session &optional unignore-p)
  "Ignore USER-ID on SESSION.
If UNIGNORE-P (interactively, with prefix), un-ignore USER."
  (interactive (list (ement-complete-user-id)
                     (ement-complete-session)
                     current-prefix-arg))
  (pcase-let* (((cl-struct ement-session account-data) session)
               ;; TODO: Store session account-data events in an alist keyed on type.
               ((map ('content (map ('ignored_users ignored-users))))
                (cl-find "m.ignored_user_list" account-data
                         :key (lambda (event) (alist-get 'type event)) :test #'equal)))
    (if unignore-p
        ;; Being map keys, the user IDs have been interned by `json-read'.
        (setf ignored-users (map-delete ignored-users (intern user-id)))
      ;; Empty maps are used to list ignored users.
      (setf (map-elt ignored-users user-id) nil))
    (ement-put-account-data session "m.ignored_user_list" (ement-alist "ignored_users" ignored-users)
      :then (lambda (data)
              (ement-debug "PUT successful" data)
              (message "Ement: User %s %s." user-id (if unignore-p "unignored" "ignored"))))))

(defun ement-invite-user (user-id room session)
  "Invite USER-ID to ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room."
  ;; SPEC: 10.4.2.1.
  (interactive
   (ement-with-room-and-session
     (list (ement-complete-user-id) ement-room ement-session)))
  (pcase-let* ((endpoint (format "rooms/%s/invite"
                                 (url-hexify-string (ement-room-id room))))
               (data (ement-alist "user_id" user-id) ))
    (ement-api session endpoint :method 'post :data (json-encode data)
      ;; TODO: Handle error codes.
      :then (lambda (_data)
              (message "User %s invited to room \"%s\" (%s)" user-id
                       (ement-room-display-name room)
                       (ement-room-id room))))))

(defun ement-list-members (room session bufferp)
  "Show members of ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room.  If BUFFERP (interactively, with
prefix), or if there are many members, show in a new buffer;
otherwise show in echo area."
  (interactive
   (ement-with-room-and-session
     (list ement-room ement-session current-prefix-arg)))
  (pcase-let* (((cl-struct ement-room members (local (map fetched-members-p))) room)
               (list-members
                (lambda (&optional _)
                  (cond ((or bufferp (> (hash-table-count members) 51))
                         ;; Show in buffer.
                         (let* ((buffer (get-buffer-create (format "*Ement members: %s*" (ement-room-display-name room))))
                                (members (cl-sort (cl-loop for user being the hash-values of members
                                                           for id = (ement-user-id user)
                                                           for displayname = (ement--user-displayname-in room user)
                                                           collect (cons displayname id))
                                                  (lambda (a b) (string-collate-lessp a b nil t)) :key #'car))
                                (displayname-width (cl-loop for member in members
                                                            maximizing (string-width (car member))))
                                (format-string (format "%%-%ss <%%s>" displayname-width)))
                           (with-current-buffer buffer
                             (erase-buffer)
                             (save-excursion
                               (dolist (member members)
                                 (insert (format format-string (car member) (cdr member)) "\n"))))
                           (pop-to-buffer buffer)))
                        (t
                         ;; Show in echo area.
                         (message "Members of %s (%s): %s" (ement--room-display-name room)
                                  (hash-table-count members)
                                  (string-join (map-apply (lambda (_id user)
                                                            (ement--user-displayname-in room user))
                                                          members)
                                               ", ")))))))
    (if fetched-members-p
        (funcall list-members)
      (ement--get-joined-members room session
        :then list-members))
    (message "Listing members of %s..." (ement--format-room room))))

(defun ement-send-direct-message (session user-id message)
  "Send a direct MESSAGE to USER-ID on SESSION.
Uses the latest existing direct room with the user, or creates a
new one automatically if necessary."
  ;; SPEC: 13.23.2.
  (interactive
   (let* ((session (ement-complete-session))
	  (user-id (ement-complete-user-id))
	  (message (read-string "Message: ")))
     (list session user-id message)))
  (if-let* ((seen-user (gethash user-id ement-users))
	    (existing-direct-room (ement--direct-room-for-user seen-user session)))
      (progn
        (ement-send-message existing-direct-room session :body message)
        (message "Message sent to %s <%s> in room %S <%s>."
                 (ement--user-displayname-in existing-direct-room seen-user)
                 user-id
                 (ement-room-display-name existing-direct-room) (ement-room-id existing-direct-room)))
    ;; No existing room for user: make new one.
    (message "Creating new room for user %s..." user-id)
    (ement-create-room session :direct-p t :invite (list user-id)
      :then (lambda (data)
              (let* ((room-id (alist-get 'room_id data))
	             (room (or (cl-find room-id (ement-session-rooms session)
                                        :key #'ement-room-id)
		               ;; New room hasn't synced yet: make a temporary struct.
		               (make-ement-room :id room-id)))
                     (direct-rooms-account-data-event-content
                      ;; FIXME: Make account-data a map.
                      (alist-get 'content (cl-find-if (lambda (event)
                                                        (equal "m.direct" (alist-get 'type event)))
                                                      (ement-session-account-data session)))))
                ;; Mark new room as direct: add the room to the account-data event, then
                ;; put the new account data to the server.  (See also:
                ;; <https://github.com/matrix-org/matrix-react-sdk/blob/919aab053e5b3bdb5a150fd90855ad406c19e4ab/src/Rooms.ts#L91>).
                (setf (map-elt direct-rooms-account-data-event-content user-id) (vector room-id))
                (ement-put-account-data session "m.direct" direct-rooms-account-data-event-content)
                ;; Send message to new room.
                (ement-send-message room session :body message)
                (message "Room \"%s\" created for user %s.  Sending message..."
	                 room-id user-id))))))

(defun ement-tag-room (tag room session)
  "Toggle TAG for ROOM on SESSION."
  (interactive
   (ement-with-room-and-session
     (let* ((prompt (format "Toggle tag (%s): " (ement--format-room ement-room)))
            (default-tags
             (ement-alist (propertize "Favourite"
                                      'face (when (ement--room-tagged-p "m.favourite" ement-room)
                                              'transient-value))
                          "m.favourite"
                          (propertize "Low-priority"
                                      'face (when (ement--room-tagged-p "m.lowpriority" ement-room)
                                              'transient-value))
                          "m.lowpriority"))
            (input (completing-read prompt default-tags))
            (tag (alist-get input default-tags (concat "u." input) nil #'string=)))
       (list tag ement-room ement-session))))
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-user (id user-id)) user)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "user/%s/rooms/%s/tags/%s"
                                 (url-hexify-string user-id) (url-hexify-string room-id) (url-hexify-string tag)))
               (method (if (ement--room-tagged-p tag room) 'delete 'put)))
    ;; TODO: "order".
    ;; FIXME: Removing a tag on a left room doesn't seem to work (e.g. to unfavorite a room after leaving it, but not forgetting it).
    (ement-api session endpoint :version "v3" :method method :data (pcase method ('put "{}"))
      :then (lambda (_)
              (ement-message "%s tag %S on %s"
                             (pcase method
                               ('delete "Removed")
                               ('put "Added"))
                             tag (ement--format-room room)) ))))

(defun ement-set-display-name (display-name session)
  "Set DISPLAY-NAME for user on SESSION.
Sets global displayname."
  (interactive
   (let* ((session (ement-complete-session))
          (display-name (read-string "Set display-name to: " nil nil
                                     (ement-user-displayname (ement-session-user session)))))
     (list display-name session)))
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-user (id user-id)) user)
               (endpoint (format "profile/%s/displayname" (url-hexify-string user-id))))
    (ement-api session endpoint :method 'put :version "v3"
      :data (json-encode (ement-alist "displayname" display-name))
      :then (lambda (_data)
              (message "Ement: Display name set to %S for <%s>" display-name
                       (ement-user-id (ement-session-user session)))))))

(defun ement-room-set-display-name (display-name room session)
  "Set DISPLAY-NAME for user in ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room.  Sets the name only in ROOM, not
globally."
  (interactive
   (ement-with-room-and-session
     (let* ((prompt (format "Set display-name in %S to: "
                            (ement--format-room ement-room)))
            (display-name (read-string prompt nil nil
                                       (ement-user-displayname (ement-session-user ement-session)))))
       (list display-name ement-room ement-session))))
  ;; NOTE: This does not seem to be documented in the spec, so we imitate the
  ;; "/myroomnick" command in SlashCommands.tsx from matrix-react-sdk.
  (pcase-let* (((cl-struct ement-room state) room)
               ((cl-struct ement-session user) session)
               ((cl-struct ement-user id) user)
               (member-event (cl-find-if (lambda (event)
                                           (and (equal id (ement-event-state-key event))
                                                (equal "m.room.member" (ement-event-type event))
                                                (equal "join" (alist-get 'membership (ement-event-content event)))))
                                         state)))
    (cl-assert member-event)
    (setf (alist-get 'displayname (ement-event-content member-event)) display-name)
    (ement-put-state room "m.room.member" id (ement-event-content member-event) session
      :then (lambda (_data)
              (message "Ement: Display name set to %S for <%s> in %S" display-name
                       (ement-user-id (ement-session-user session))
                       (ement--format-room room))))))

;;;;;; Describe room

(defvar ement-describe-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ement-describe-room-mode' buffers.")

(define-derived-mode ement-describe-room-mode read-only-mode
  "Ement-Describe-Room" "Major mode for `ement-describe-room' buffers.")

(defun ement-describe-room (room session)
  "Describe ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room."
  (interactive (ement-with-room-and-session (list ement-room ement-session)))
  (cl-labels ((heading (string)
                (propertize (or string "") 'face 'font-lock-builtin-face))
              (id (string)
                (propertize (or string "") 'face 'font-lock-constant-face))
              (member<
                (a b) (string-collate-lessp (car a) (car b) nil t)))
    (pcase-let* (((cl-struct ement-room (id room-id) avatar display-name canonical-alias members timeline status topic
                             (local (map fetched-members-p)))
                  room)
                 ((cl-struct ement-session user) session)
                 ((cl-struct ement-user (id user-id)) user)
                 (inhibit-read-only t))
      (if (not fetched-members-p)
          ;; Members not fetched: fetch them and re-call this command.
          (ement--get-joined-members room session
            :then (lambda (_) (ement-room-describe room session)))
        (with-current-buffer (get-buffer-create (format "*Ement room description: %s*" (or display-name canonical-alias room-id)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; We avoid looping twice by doing a bit more work here and
            ;; returning a cons which we destructure.
            (pcase-let* ((`(,member-pairs . ,name-width)
                          (cl-loop for user being the hash-values of members
                                   for formatted = (ement--format-user user room session)
                                   for id = (format "<%s>" (id (ement-user-id user)))
                                   collect (cons formatted id)
                                   into pairs
                                   maximizing (string-width id) into width
                                   finally return (cons (cl-sort pairs #'member<) width)))
                         ;; We put the MXID first, because users may use Unicode characters
                         ;; in their displayname, which `string-width' does not always
                         ;; return perfect results for, and putting it last prevents
                         ;; alignment problems.
                         (spec (format "%%-%ss %%s" name-width)))
              (save-excursion
                (insert "\"" (propertize (or display-name canonical-alias room-id) 'face 'font-lock-doc-face) "\"" " is a "
                        (propertize (if (ement--space-p room)
                                        "space"
                                      "room")
                                    'face 'font-lock-type-face)
                        " "
                        (propertize (pcase status
                                      ('invite "invited")
                                      ('join "joined")
                                      ('leave "left")
                                      (_ (symbol-name status)))
                                    'face 'font-lock-comment-face)
                        " on session <" (id user-id) ">.\n\n"
                        (heading "Avatar: ") (or avatar "") "\n\n"
                        (heading "ID: ") "<" (id room-id) ">" "\n"
                        (heading "Alias: ") "<" (id canonical-alias) ">" "\n\n"
                        (heading "Topic: ") (propertize (or topic "[none]") 'face 'font-lock-comment-face) "\n\n"
                        (heading "Retrieved events: ") (number-to-string (length timeline)) "\n"
                        (heading "  spanning: ")
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (/ (ement-event-origin-server-ts
                                                (car (cl-sort (copy-sequence timeline) #'< :key #'ement-event-origin-server-ts)))
                                               1000))
                        (heading " to ")
                        (format-time-string "%Y-%m-%d %H:%M:%S\n\n"
                                            (/ (ement-event-origin-server-ts
                                                (car (cl-sort (copy-sequence timeline) #'> :key #'ement-event-origin-server-ts)))
                                               1000))
                        (heading "Members") " (" (number-to-string (hash-table-count members)) "):\n")
                (pcase-dolist (`(,formatted . ,id) member-pairs)
                  (insert "  " (format spec id formatted) "\n")))))
          (unless (eq major-mode 'ement-describe-room-mode)
            ;; Without this check, activating the mode again causes a "Cyclic keymap
            ;; inheritance" error.
            (ement-describe-room-mode))
          (pop-to-buffer (current-buffer)))))))

(defalias 'ement-room-describe #'ement-describe-room)

;;;;;; Push rules

;; NOTE: Although v1.4 of the spec is available and describes setting the push rules using
;; the "v3" API endpoint, the Element client continues to use the "r0" endpoint, which is
;; slightly different.  This implementation will follow Element's initially, because the
;; spec is not simple, and imitating Element's requests will make it easier.

(defun ement-room-notification-state (room session)
  "Return notification state for ROOM on SESSION.
Returns one of nil (meaning default rules are used), `all-loud',
`all', `mentions-and-keywords', or `none'."
  ;; Following the implementation of getRoomNotifsState() in RoomNotifs.ts in matrix-react-sdk.

  ;; TODO: Guest support (in which case the state should be `all').
  ;; TODO: Store account data as a hash table of event types.
  (let ((push-rules (cl-find-if (lambda (alist)
                                  (equal "m.push_rules" (alist-get 'type alist)))
                                (ement-session-account-data session))))
    (cl-labels ((override-mute-rule-for-room-p (room)
                  ;; Following findOverrideMuteRule() in RoomNotifs.ts.
                  (when-let ((overrides (map-nested-elt push-rules '(content global override))))
                    (cl-loop for rule in overrides
                             when (and (alist-get 'enabled rule)
                                       (rule-for-room-p rule room))
                             return rule)))
                (rule-for-room-p (rule room)
                  ;; Following isRuleForRoom() in RoomNotifs.ts.
                  (and (/= 1 (length (alist-get 'conditions rule)))
                       (pcase-let* ((condition (elt (alist-get 'conditions rule) 0))
                                    ((map kind key pattern) condition))
                         (and (equal "event_match" kind)
                              (equal "room_id" key)
                              (equal (ement-room-id room) pattern)))))
                (mute-rule-p (rule)
                  (when-let ((actions (alist-get 'actions rule)))
                    (seq-contains-p actions "dont_notify")))
                ;; NOTE: Although v1.7 of the spec says that "dont_notify" is
                ;; obsolete, the latest revision of matrix-react-sdk (released last week
                ;; as v3.77.1) still works as modeled here.
                (tweak-rule-p (type rule)
                  (when-let ((actions (alist-get 'actions rule)))
                    (and (seq-contains-p actions "notify")
                         (seq-contains-p actions `(set_tweak . ,type) 'seq-contains-p)))))
      ;; If none of these match, nil is returned, meaning that the default rule is used
      ;; for the room.
      (if (override-mute-rule-for-room-p room)
          'none
        (when-let ((room-rule (cl-find-if (lambda (rule)
                                            (equal (ement-room-id room) (alist-get 'rule_id rule)))
                                          (map-nested-elt push-rules '(content global room)))))
          (cond ((not (alist-get 'enabled room-rule))
                 ;; NOTE: According to comment in getRoomNotifsState(), this assumes that
                 ;; the default is to notify for all messages, which "will be 'wrong' for
                 ;; one to one rooms because they will notify loudly for all messages."
                 'all)
                ((mute-rule-p room-rule)
                 ;; According to comment, a room-level mute still allows mentions to
                 ;; notify.  NOTE: See note above.
                 'mentions-and-keywords)
                ((tweak-rule-p "sound" room-rule) 'all-loud)))))))

(defun ement-room-set-notification-state (state room session)
  "Set notification STATE for ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room.  STATE may be nil to set the rules to
default, `all', `mentions-and-keywords', or `none'."
  ;; This merely attempts to reproduce the behavior of Element's simple notification
  ;; options.  It does not attempt to offer all of the features defined in the spec.  And,
  ;; yes, it is rather awkward, having to sometimes* make multiple requests of different
  ;; "kinds" to set the rules for a single room, but that is how the API works.
  ;;
  ;; * It appears that Element only makes multiple requests of different kinds when
  ;; strictly necessary, but coding that logic now would seem likely to be a waste of
  ;; time, given that Element doesn't even use the latest version of the spec yet.  So
  ;; we'll just do the "dumb" thing and always send requests of both "override" and
  ;; "room" kinds, which appears to Just Work™.
  ;;
  ;; TODO: Match rules to these user-friendly notification states for presentation.  See
  ;; <https://github.com/matrix-org/matrix-react-sdk/blob/8c67984f50f985aa481df24778078030efa39001/src/RoomNotifs.ts>.

  ;; TODO: Support `all-loud' ("all_messages_loud").
  (interactive
   (ement-with-room-and-session
     (let* ((prompt (format "Set notification rules for %s: " (ement--format-room ement-room)))
            (available-states (ement-alist "Default" nil
                                           "All messages" 'all
                                           "Mentions and keywords" 'mentions-and-keywords
                                           "None" 'none))
            (selected-rule (completing-read prompt (mapcar #'car available-states) nil t))
            (state (alist-get selected-rule available-states nil nil #'equal)))
       (list state ement-room ement-session))))
  (cl-labels ((set-rule (kind rule queue message-fn)
                (pcase-let* (((cl-struct ement-room (id room-id)) room)
                             (rule-id (url-hexify-string room-id))
                             (endpoint (format "pushrules/global/%s/%s" kind rule-id))
                             (method (if rule 'put 'delete))
                             (then (if rule
                                       ;; Setting rules requires PUTting the rules, then making a second
                                       ;; request to enable them.
                                       (lambda (_data)
                                         (ement-api session (concat endpoint "/enabled") :queue queue :version "r0"
                                           :method 'put :data (json-encode (ement-alist 'enabled t))
                                           :then message-fn))
                                     message-fn)))
                  (ement-api session endpoint :queue queue :method method :version "r0"
                    :data (json-encode rule)
                    :then then
                    :else (lambda (plz-error)
                            (pcase-let* (((cl-struct plz-error response) plz-error)
                                         ((cl-struct plz-response status) response))
                              (pcase status
                                (404 (pcase rule
                                       (`nil
                                        ;; Room already had no rules, so none being found is not an
                                        ;; error.
                                        nil)
                                       (_ ;; Unexpected error: re-signal.
                                        (ement-api-error plz-error))))
                                (_ ;; Unexpected error: re-signal.
                                 (ement-api-error plz-error)))))))))
    (pcase-let* ((available-states
                  (ement-alist
                   nil (ement-alist
                        "override" nil
                        "room" nil)
                   'all (ement-alist
                         "override" nil
                         "room" (ement-alist
                                 'actions (vector "notify" (ement-alist
                                                            'set_tweak "sound"
                                                            'value "default"))))
                   'mentions-and-keywords (ement-alist
                                           "override" nil
                                           "room" (ement-alist
                                                   'actions (vector "dont_notify")))
                   'none (ement-alist
                          "override" (ement-alist
                                      'actions (vector "dont_notify")
                                      'conditions (vector (ement-alist
                                                           'kind "event_match"
                                                           'key "room_id"
                                                           'pattern (ement-room-id room))))
                          "room" nil)))
                 (kinds-and-rules (alist-get state available-states nil nil #'equal)))
      (cl-loop with queue = (make-plz-queue :limit 1)
               with total = (1- (length kinds-and-rules))
               for count from 0
               for message-fn = (if (equal count total)
                                    (lambda (_data)
                                      (message "Set notification rules for room: %s" (ement--format-room room)))
                                  #'ignore)
               for (kind . state) in kinds-and-rules
               do (set-rule kind state queue message-fn)))))

;;;;; Public functions

;; These functions could reasonably be called by code in other packages.

(cl-defun ement-put-state
    (room type key data session
          &key (then (lambda (response-data)
                       (ement-debug "State data put on room" response-data data room session))))
  "Put state event of TYPE with KEY and DATA on ROOM on SESSION.
DATA should be an alist, which will become the JSON request
body."
  (declare (indent defun))
  (pcase-let* ((endpoint (format "rooms/%s/state/%s/%s"
                                 (url-hexify-string (ement-room-id room))
                                 type key)))
    (ement-api session endpoint :method 'put :data (json-encode data)
      ;; TODO: Handle error codes.
      :then then)))

(defun ement-message (format-string &rest args)
  "Call `message' on FORMAT-STRING prefixed with \"Ement: \"."
  ;; TODO: Use this function everywhere we use `message'.
  (apply #'message (concat "Ement: " format-string) args))

(cl-defun ement-upload (session &key data filename then else
                                (content-type "application/octet-stream"))
  "Upload DATA with FILENAME to content repository on SESSION.
THEN and ELSE are passed to `ement-api', which see."
  (declare (indent defun))
  (ement-api session "upload" :method 'post :endpoint-category "media"
    ;; NOTE: Element currently uses "r0" not "v3", so so do we.
    :params (when filename
              (list (list "filename" filename)))
    :content-type content-type :data data :data-type 'binary
    :then then :else else))

(cl-defun ement-complete-session (&key (prompt "Session: "))
  "Return an Ement session selected with completion."
  (pcase (length ement-sessions)
    (0 (user-error "No active sessions.  Call `ement-connect' to log in"))
    (1 (cdar ement-sessions))
    (_ (let* ((ids (mapcar #'car ement-sessions))
              (selected-id (completing-read prompt ids nil t)))
         (alist-get selected-id ement-sessions nil nil #'equal)))))

(declare-function ewoc-locate "ewoc")
(defun ement-complete-user-id ()
  "Return a user-id selected with completion.
Selects from seen users on all sessions.  If point is on an
event, suggests the event's sender as initial input.  Allows
unseen user IDs to be input as well."
  (cl-labels ((format-user (user)
                ;; FIXME: Per-room displaynames are now stored in room structs
                ;; rather than user structs, so to be complete, this needs to
                ;; iterate over all known rooms, looking for the user's
                ;; displayname in that room.
                (format "%s <%s>"
                        (ement-user-displayname user)
			(ement-user-id user))))
    (let* ((display-to-id
	    (cl-loop for key being the hash-keys of ement-users
		     using (hash-values value)
		     collect (cons (format-user value) key)))
           (user-at-point (when (equal major-mode 'ement-room-mode)
                            (when-let ((node (ewoc-locate ement-ewoc)))
                              (when (ement-event-p (ewoc-data node))
                                (format-user (ement-event-sender (ewoc-data node)))))))
	   (selected-user (completing-read "User: " (mapcar #'car display-to-id)
                                           nil nil user-at-point)))
      (or (alist-get selected-user display-to-id nil nil #'equal)
	  selected-user))))

(cl-defun ement-put-account-data
    (session type data &key room
             (then (lambda (received-data)
                     ;; Handle echoed-back account data event (the spec does not explain this,
                     ;; but see <https://github.com/matrix-org/matrix-react-sdk/blob/675b4271e9c6e33be354a93fcd7807253bd27fcd/src/settings/handlers/AccountSettingsHandler.ts#L150>).
                     ;; FIXME: Make session account-data a map instead of a list of events.
                     (if room
                         (push received-data (ement-room-account-data room))
                       (push received-data (ement-session-account-data session)))

                     ;; NOTE: Commenting out this ement-debug form because a bug in Emacs
                     ;; causes this long string to be interpreted as the function's
                     ;; docstring and cause a too-long-docstring warning.

                     ;; (ement-debug "Account data put and received back on session %s:  PUT(json-encoded):%S  RECEIVED:%S"
                     ;;              (ement-user-id (ement-session-user session)) (json-encode data) received-data)
                     )))
  "Put account data of TYPE with DATA on SESSION.
If ROOM, put it on that room's account data.  Also handle the
echoed-back event."
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-session (user (cl-struct ement-user (id user-id)))) session)
               (room-part (if room (format "/rooms/%s" (ement-room-id room)) ""))
               (endpoint (format "user/%s%s/account_data/%s" (url-hexify-string user-id) room-part type)))
    (ement-api session endpoint :method 'put :data (json-encode data)
      :then then)))

(defun ement-redact (event room session &optional reason)
  "Redact EVENT in ROOM on SESSION, optionally for REASON."
  (pcase-let* (((cl-struct ement-event (id event-id)) event)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "rooms/%s/redact/%s/%s"
                                 room-id event-id (ement--update-transaction-id session)))
               (content (ement-alist "reason" reason)))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (lambda (_data)
              (message "Event %s redacted." event-id)))))

;;;;; Inline functions

(defsubst ement--user-color (user)
  "Return USER's color, setting it if necessary.
USER is an `ement-user' struct."
  (or (ement-user-color user)
      (setf (ement-user-color user)
            (ement--prism-color (ement-user-id user)))))

;;;;; Private functions

;; These functions aren't expected to be called by code in other packages (but if that
;; were necessary, they could be renamed accordingly).

;; (defun ement--room-routing (room)
;;   "Return a list of servers to route to ROOM through."
;;   ;; See <https://spec.matrix.org/v1.2/appendices/#routing>.
;;   ;; FIXME: Ensure highest power level user is at least level 50.
;;   ;; FIXME: Ignore servers blocked due to server ACLs.
;;   ;; FIXME: Ignore servers which are IP addresses.
;;   (cl-labels ((most-powerful-user-in
;;                (room))
;;               (servers-by-population-in
;;                (room))
;;               (server-of (user)))
;;     (let (first-server-by-power-level)
;;       (delete-dups
;;        (remq nil
;;              (list
;;               ;; 1.
;;               (or (when-let ((user (most-powerful-user-in room)))
;;                     (setf first-server-by-power-level t)
;;                     (server-of user))
;;                   (car (servers-by-population-in room)))
;;               ;; 2.
;;               (if first-server-by-power-level
;;                   (car (servers-by-population-in room))
;;                 (cl-second (servers-by-population-in room)))
;;               ;; 3.
;;               (cl-third (servers-by-population-in room))))))))

(defun ement--space-p (room)
  "Return non-nil if ROOM is a space."
  (equal "m.space" (ement-room-type room)))

(defun ement--room-in-space-p (room space)
  "Return non-nil if ROOM is in SPACE on SESSION."
  ;; We could use `ement---room-spaces', but since that returns rooms by looking them up
  ;; by ID in the session's rooms list, this is more efficient.
  (pcase-let* (((cl-struct ement-room (id parent-id) (local (map children))) space)
               ((cl-struct ement-room (id child-id) (local (map parents))) room))
    (or (member parent-id parents)
        (member child-id children))))

(defun ement--room-spaces (room session)
  "Return list of ROOM's parent spaces on SESSION."
  ;; NOTE: This only looks in the room's parents list; it doesn't look in every space's children
  ;; list.  This should be good enough, assuming we add to the lists correctly elsewhere.
  (pcase-let* (((cl-struct ement-session rooms) session)
               ((cl-struct ement-room (local (map parents))) room))
    (cl-remove-if-not (lambda (session-room-id)
                        (member session-room-id parents))
                      rooms :key #'ement-room-id)))

(cl-defun ement--prism-color (string &key (contrast-with (face-background 'default nil 'default)))
  "Return a computed color for STRING.
The color is adjusted to have sufficient contrast with the color
CONTRAST-WITH (by default, the default face's background).  The
computed color is useful for user messages, generated room
avatars, etc."
  ;; TODO: Use this instead of `ement-room--user-color'.  (Same algorithm ,just takes a
  ;; string as argument.)
  ;; TODO: Try using HSV somehow so we could avoid having so many strings return a
  ;; nearly-black color.
  (cl-labels ((relative-luminance (rgb)
                ;; Copy of `modus-themes-wcag-formula', an elegant
                ;; implementation by Protesilaos Stavrou.  Also see
                ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
                ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
                (cl-loop for k in '(0.2126 0.7152 0.0722)
                         for x in rgb
                         sum (* k (if (<= x 0.03928)
                                      (/ x 12.92)
                                    (expt (/ (+ x 0.055) 1.055) 2.4)))))
              (contrast-ratio (a b)
                ;; Copy of `modus-themes-contrast'; see above.
                (let ((ct (/ (+ (relative-luminance a) 0.05)
                             (+ (relative-luminance b) 0.05))))
                  (max ct (/ ct))))
              (increase-contrast (color against target toward)
                (let ((gradient (cdr (color-gradient color toward 20)))
                      new-color)
                  (cl-loop do (setf new-color (pop gradient))
                           while new-color
                           until (>= (contrast-ratio new-color against) target)
                           ;; Avoid infinite loop in case of weirdness
                           ;; by returning color as a fallback.
                           finally return (or new-color color)))))
    (let* ((id string)
           (id-hash (float (+ (abs (sxhash id)) ement-room-prism-color-adjustment)))
           ;; TODO: Wrap-around the value to get the color I want.
           (ratio (/ id-hash (float most-positive-fixnum)))
           (color-num (round (* (* 255 255 255) ratio)))
           (color-rgb (list (/ (float (logand color-num 255)) 255)
                            (/ (float (ash (logand color-num 65280) -8)) 255)
                            (/ (float (ash (logand color-num 16711680) -16)) 255)))
           (contrast-with-rgb (color-name-to-rgb contrast-with)))
      (when (< (contrast-ratio color-rgb contrast-with-rgb) ement-room-prism-minimum-contrast)
        (setf color-rgb (increase-contrast color-rgb contrast-with-rgb ement-room-prism-minimum-contrast
                                           (color-name-to-rgb
                                            ;; Ideally we would use the foreground color,
                                            ;; but in some themes, like Solarized Dark,
                                            ;; the foreground color's contrast is too low
                                            ;; to be effective as the value to increase
                                            ;; contrast against, so we use white or black.
                                            (pcase contrast-with
                                              ((or `nil "unspecified-bg")
                                               ;; The `contrast-with' color (i.e. the
                                               ;; default background color) is nil.  This
                                               ;; probably means that we're displaying on
                                               ;; a TTY.
                                               (if (fboundp 'frame--current-backround-mode)
                                                   ;; This function can tell us whether
                                                   ;; the background color is dark or
                                                   ;; light, but it was added in Emacs
                                                   ;; 28.1.
                                                   (pcase (frame--current-backround-mode (selected-frame))
                                                     ('dark "white")
                                                     ('light "black"))
                                                 ;; Pre-28.1: Since faces' colors may be
                                                 ;; "unspecified" on TTY frames, in which
                                                 ;; case we have nothing to compare with, we
                                                 ;; assume that the background color of such
                                                 ;; a frame is black and increase contrast
                                                 ;; toward white.
                                                 "white"))
                                              (_
                                               ;; The `contrast-with` color is usable: test it.
                                               (if (ement--color-dark-p (color-name-to-rgb contrast-with))
                                                   "white" "black")))))))
      (apply #'color-rgb-to-hex (append color-rgb (list 2))))))

(cl-defun ement--format-user (user &optional (room ement-room) (session ement-session))
  "Format `ement-user' USER for ROOM on SESSION.
ROOM defaults to the value of `ement-room'."
  (let ((face (cond ((equal (ement-user-id (ement-session-user session))
                            (ement-user-id user))
                     'ement-room-self)
                    (ement-room-prism
                     `(:inherit ement-room-user :foreground ,(or (ement-user-color user)
                                                                 (setf (ement-user-color user)
                                                                       (ement--prism-color user)))))
                    (t 'ement-room-user))))
    ;; FIXME: If a membership state event has not yet been received, this
    ;; sets the display name in the room to the user ID, and that prevents
    ;; the display name from being used if the state event arrives later.
    (propertize (ement--user-displayname-in room user)
                'face face
                'help-echo (ement-user-id user))))

(cl-defun ement--format-body-mentions
    (body room &key (template "<a href=\"https://matrix.to/#/%s\">%s</a>"))
  "Return string for BODY with mentions in ROOM linkified with TEMPLATE.
TEMPLATE is a format string in which the first \"%s\" is replaced
with the user's MXID and the second with the displayname.  A
mention is qualified by an \"@\"-prefixed displayname or
MXID (optionally suffixed with a colon), or a colon-suffixed
displayname, followed by a blank, question mark, comma, or
period, anywhere in the body."
  ;; Examples:
  ;; "@foo: hi"
  ;; "@foo:matrix.org: hi"
  ;; "foo: hi"
  ;; "@foo and @bar:matrix.org: hi"
  ;; "foo: how about you and @bar ..."
  (declare (indent defun))
  (cl-labels ((members-having-displayname (name members)
                ;; Iterating over the hash table values isn't as efficient as a hash
                ;; lookup, but in most rooms it shouldn't be a problem.
                (cl-loop for user being the hash-values of members
                         when (equal name (ement--user-displayname-in room user))
                         collect user)))
    (pcase-let* (((cl-struct ement-room members) room)
                 (regexp (rx (or bos bow (1+ blank))
                             (or (seq (group
                                       ;; Group 1: full @-prefixed MXID.
                                       "@" (group
                                            ;; Group 2: displayname.  (NOTE: Does not work
                                            ;; with displaynames containing spaces.)
                                            (1+ (seq (optional ".") alnum)))
                                       (optional ":" (1+ (seq (optional ".") alnum))))
                                      (or ":" eow eos (syntax punctuation)))
                                 (seq (group
                                       ;; Group 3: MXID username or displayname.
                                       (1+ (not blank)))
                                      ":" (1+ blank)))))
                 (pos 0) (replace-group) (replacement))
      (while (setf pos (string-match regexp body pos))
        (if (setf replacement
                  (or (when-let (member (gethash (match-string 1 body) members))
                        ;; Found user ID: use it as replacement.
                        (setf replace-group 1)
                        (format template (match-string 1 body)
                                (ement--xml-escape-string (ement--user-displayname-in room member))))
                      (when-let* ((name (or (when (match-string 2 body)
                                              (setf replace-group 1)
                                              (match-string 2 body))
                                            (prog1 (match-string 3 body)
                                              (setf replace-group 3))))
                                  (members (members-having-displayname name members))
                                  (member (when (= 1 (length members))
                                            ;; If multiple members are found with the same
                                            ;; displayname, do nothing.
                                            (car members))))
                        ;; Found displayname: use it and MXID as replacement.
                        (format template (ement-user-id member)
                                (ement--xml-escape-string name)))))
            (progn
              ;; Found member: replace and move to end of replacement.
              (setf body (replace-match replacement t t body replace-group))
              (let ((difference (- (length replacement) (length (match-string 0 body)))))
                (setf pos (if (/= 0 difference)
                              ;; Replacement of a different length: adjust POS accordingly.
                              (+ pos difference)
                            (match-end 0)))))
          ;; No replacement: move to end of match.
          (setf pos (match-end 0))))))
  body)

(defun ement--event-mentions-room-p (event &rest _ignore)
  "Return non-nil if EVENT mentions \"@room\"."
  (pcase-let (((cl-struct ement-event (content (map body))) event))
    (when body
      (string-match-p (rx (or space bos) "@room" eow) body))))

(cl-defun ement-complete-room (&key session (predicate #'identity)
                                    (prompt "Room: ") (suggest t))
  "Return a (room session) list selected from SESSION with completion.
If SESSION is nil, select from rooms in all of `ement-sessions'.
When SUGGEST, suggest current buffer's room (or a room at point
in a room list buffer) as initial input (i.e. it should be set to
nil when switching from one room buffer to another).  PROMPT may
override the default prompt.  PREDICATE may be a function to
select which rooms are offered; it is also applied to the
suggested room."
  (declare (indent defun))
  (pcase-let* ((sessions (if session
                             (list session)
                           (mapcar #'cdr ement-sessions)))
               (name-to-room-session
                (cl-loop for session in sessions
                         append (cl-loop for room in (ement-session-rooms session)
                                         when (funcall predicate room)
                                         collect (cons (ement--format-room room 'topic)
                                                       (list room session)))))
               (names (mapcar #'car name-to-room-session))
               (selected-name (completing-read
                               prompt names nil t
                               (when suggest
                                 (when-let ((suggestion (ement--room-at-point)))
                                   (when (or (not predicate)
                                             (funcall predicate suggestion))
                                     (ement--format-room suggestion 'topic)))))))
    (alist-get selected-name name-to-room-session nil nil #'string=)))

(cl-defun ement-send-message (room session
                                   &key body formatted-body replying-to-event filter then)
  "Send message to ROOM on SESSION with BODY and FORMATTED-BODY.
THEN may be a function to call after the event is sent
successfully.  It is called with keyword arguments for ROOM,
SESSION, CONTENT, and DATA.

REPLYING-TO-EVENT may be an event the message is
in reply to; the message will reference it appropriately.

FILTER may be a function through which to pass the message's
content object before sending (see,
e.g. `ement-room-send-org-filter')."
  (declare (indent defun))
  (cl-assert (not (string-empty-p body)))
  (cl-assert (or (not formatted-body) (not (string-empty-p formatted-body))))
  (pcase-let* (((cl-struct ement-room (id room-id)) room)
               (endpoint (format "rooms/%s/send/m.room.message/%s" (url-hexify-string room-id)
                                 (ement--update-transaction-id session)))
               (formatted-body (when formatted-body
                                 (ement--format-body-mentions formatted-body room)))
               (content (ement-aprog1
                            (ement-alist "msgtype" "m.text"
                                         "body" body)
                          (when formatted-body
                            (push (cons "formatted_body" formatted-body) it)
                            (push (cons "format" "org.matrix.custom.html") it))))
               (then (or then #'ignore)))
    (when filter
      (setf content (funcall filter content room)))
    (when replying-to-event
      (setf content (ement--add-reply content replying-to-event room)))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (apply-partially then :room room :session session
                             ;; Data is added when calling back.
                             :content content :data))))

(defalias 'ement--button-buttonize
  ;; This isn't nice, but what can you do.
  (cond ((version<= "29.1" emacs-version) #'buttonize)
        ((version<= "28.1" emacs-version) (with-suppressed-warnings ((obsolete button-buttonize))
                                            #'button-buttonize))
        ((version< emacs-version "28.1")
         ;; FIXME: This doesn't set the mouse-face to highlight, and it doesn't use the
         ;; default-button category.  Neither does `button-buttonize', of course, but why?
         (lambda (string callback &optional data)
           "Make STRING into a button and return it.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument."
           (propertize string
                       'face 'button
                       'button t
                       'follow-link t
                       'category t
                       'button-data data
                       'keymap button-map
                       'action callback)))))

(defun ement--add-reply (data replying-to-event room)
  "Return DATA adding reply data for REPLYING-TO-EVENT in ROOM.
DATA is an unsent message event's data alist."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id351> "13.2.2.6.1  Rich replies"
  ;; FIXME: Rename DATA.
  (pcase-let* (((cl-struct ement-event (id replying-to-event-id)
                           content (sender replying-to-sender))
                replying-to-event)
               ((cl-struct ement-user (id replying-to-sender-id)) replying-to-sender)
               ((map ('body replying-to-body) ('formatted_body replying-to-formatted-body)) content)
               (replying-to-sender-name (ement--user-displayname-in ement-room replying-to-sender))
               (quote-string (format "> <%s> %s\n\n" replying-to-sender-name replying-to-body))
               (reply-body (alist-get "body" data nil nil #'string=))
               (reply-formatted-body (alist-get "formatted_body" data nil nil #'string=))
               (reply-body-with-quote (concat quote-string reply-body))
               (reply-formatted-body-with-quote
                (format "<mx-reply>
  <blockquote>
    <a href=\"https://matrix.to/#/%s/%s\">In reply to</a>
    <a href=\"https://matrix.to/#/%s\">%s</a>
    <br />
    %s
  </blockquote>
</mx-reply>
%s"
                        (ement-room-id room) replying-to-event-id replying-to-sender-id replying-to-sender-name
                        ;; TODO: Encode HTML special characters.  Not as straightforward in Emacs as one
                        ;; might hope: there's `web-mode-html-entities' and `org-entities'.  See also
                        ;; <https://emacs.stackexchange.com/questions/8166/encode-non-html-characters-to-html-equivalent>.
                        (or replying-to-formatted-body replying-to-body)
                        (or reply-formatted-body reply-body))))
    ;; NOTE: map-elt doesn't work with string keys, so we use `alist-get'.
    (setf (alist-get "body" data nil nil #'string=) reply-body-with-quote
          (alist-get "formatted_body" data nil nil #'string=) reply-formatted-body-with-quote
          data (append (ement-alist "m.relates_to"
                                    (ement-alist "m.in_reply_to"
                                                 (ement-alist "event_id" replying-to-event-id))
                                    "format" "org.matrix.custom.html")
                       data))
    data))

(defun ement--direct-room-for-user (user session)
  "Return last-modified direct room with USER on SESSION, if one exists."
  ;; Loosely modeled on the Element function findDMForUser in createRoom.ts.
  (cl-labels ((membership-event-for-p (event user)
                (and (equal "m.room.member" (ement-event-type event))
                     (equal (ement-user-id user) (ement-event-state-key event))))
              (latest-membership-for (user room)
                (when-let ((latest-membership-event
                            (car
                             (cl-sort
                              ;; I guess we need to check both state and timeline events.
                              (append (cl-remove-if-not (lambda (event)
                                                          (membership-event-for-p event user))
                                                        (ement-room-state room))
                                      (cl-remove-if-not (lambda (event)
                                                          (membership-event-for-p event user))
                                                        (ement-room-timeline room)))
                              (lambda (a b)
                                ;; Sort latest first so we can use the car.
                                (> (ement-event-origin-server-ts a)
                                   (ement-event-origin-server-ts b)))))))
                  (alist-get 'membership (ement-event-content latest-membership-event))))
              (latest-event-in (room)
                (car
                 (cl-sort
                  (append (ement-room-state room)
                          (ement-room-timeline room))
                  (lambda (a b)
                    ;; Sort latest first so we can use the car.
                    (> (ement-event-origin-server-ts a)
                       (ement-event-origin-server-ts b)))))))
    (let* ((direct-rooms (cl-remove-if-not
                          (lambda (room)
                            (ement--room-direct-p room session))
                          (ement-session-rooms session)))
           (direct-joined-rooms
            ;; Ensure that the local user is still in each room.
            (cl-remove-if-not
             (lambda (room)
               (equal "join" (latest-membership-for (ement-session-user session) room)))
             direct-rooms))
           ;; Since we don't currently keep a member list for each room, we look in the room's
           ;; join events to see if the user has joined or been invited.
           (direct-rooms-with-user
            (cl-remove-if-not
             (lambda (room)
               (member (latest-membership-for user room) '("invite" "join")))
             direct-joined-rooms)))
      (car (cl-sort direct-rooms-with-user
                    (lambda (a b)
                      (> (latest-event-in a) (latest-event-in b))))))))

(defun ement--event-replaces-p (a b)
  "Return non-nil if event A replaces event B.
That is, if event A replaces B in their
\"m.relates_to\"/\"m.relations\" and \"m.replace\" metadata."
  (pcase-let* (((cl-struct ement-event (id a-id) (origin-server-ts a-ts)
                           (content (map ('m.relates_to
                                          (map ('rel_type a-rel-type)
                                               ('event_id a-replaces-event-id))))))
                a)
               ((cl-struct ement-event (id b-id) (origin-server-ts b-ts)
                           (content (map ('m.relates_to
                                          (map ('rel_type b-rel-type)
                                               ('event_id b-replaces-event-id)))
                                         ('m.relations
                                          (map ('m.replace
                                                (map ('event_id b-replaced-by-event-id))))))))
                b))
    (or (equal a-id b-replaced-by-event-id)
        (and (equal "m.replace" a-rel-type)
             (or (equal a-replaces-event-id b-id)
                 (and (equal "m.replace" b-rel-type)
                      (equal a-replaces-event-id b-replaces-event-id)
                      (>= a-ts b-ts)))))))

(defun ement--events-equal-p (a b)
  "Return non-nil if events A and B are essentially equal.
That is, A and B are either the same event (having the same event
ID), or one event replaces the other (in their m.relates_to and
m.replace metadata)."
  (or (equal (ement-event-id a) (ement-event-id b))
      (ement--event-replaces-p a b)
      (ement--event-replaces-p b a)))

(defun ement--original-event-for (event session)
  "Return the original of EVENT in SESSION.
If EVENT has metadata indicating that it replaces another event,
return the replaced event; otherwise return EVENT.  If a replaced
event can't be found in SESSION's events table, return an ersatz
one that has the expected ID and same sender."
  (pcase-let (((cl-struct ement-event sender
                          (content (map ('m.relates_to
                                         (map ('event_id replaced-event-id)
                                              ('rel_type relation-type))))))
               event))
    (pcase relation-type
      ("m.replace" (or (gethash replaced-event-id (ement-session-events session))
                       (make-ement-event :id replaced-event-id :sender sender)))
      (_ event))))

(defun ement--format-room (room &optional topic)
  "Return ROOM formatted with name, alias, ID, and optionally TOPIC.
Suitable for use in completion, etc."
  (if topic
      (format "%s%s(<%s>)%s"
              (or (ement-room-display-name room)
                  (setf (ement-room-display-name room)
                        (ement--room-display-name room)))
              (if (ement-room-canonical-alias room)
                  (format " <%s> " (ement-room-canonical-alias room))
                " ")
              (ement-room-id room)
              (if (ement-room-topic room)
                  (format ": \"%s\"" (ement-room-topic room))
                ""))
    (format "%s%s(<%s>)"
            (or (ement-room-display-name room)
                (setf (ement-room-display-name room)
                      (ement--room-display-name room)))
            (if (ement-room-canonical-alias room)
                (format " <%s> " (ement-room-canonical-alias room))
              " ")
            (ement-room-id room))))

(defun ement--members-alist (room)
  "Return alist of member displaynames mapped to IDs seen in ROOM."
  ;; We map displaynames to IDs because `ement-room--format-body-mentions' needs to find
  ;; MXIDs from displaynames.
  (pcase-let* (((cl-struct ement-room timeline) room)
               (members-seen (mapcar #'ement-event-sender timeline))
               (members-alist))
    (dolist (member members-seen)
      ;; Testing with `benchmark-run-compiled', it appears that using `cl-pushnew' is
      ;; about 10x faster than using `delete-dups'.
      (cl-pushnew (cons (ement--user-displayname-in room member)
                        (ement-user-id member))
                  members-alist))
    members-alist))

(defun ement--mxc-to-url (uri session)
  "Return HTTPS URL for MXC URI accessed through SESSION."
  (pcase-let* (((cl-struct ement-session server) session)
               ((cl-struct ement-server uri-prefix) server)
               (server-name) (media-id))
    (string-match (rx "mxc://" (group (1+ (not (any "/"))))
                      "/" (group (1+ anything))) uri)
    (setf server-name (match-string 1 uri)
          media-id (match-string 2 uri))
    (format "%s/_matrix/media/r0/download/%s/%s"
            uri-prefix server-name media-id)))

(defun ement--remove-face-property (string value)
  "Remove VALUE from STRING's `face' properties.
Used to remove the `button' face from buttons, because that face
can cause undesirable underlining."
  (let ((pos 0))
    (cl-loop for next-face-change-pos = (next-single-property-change pos 'face string)
             for face-at = (get-text-property pos 'face string)
             when face-at
             do (put-text-property pos (or next-face-change-pos (length string))
                                   'face (cl-typecase face-at
                                           (atom (if (equal value face-at)
                                                     nil face-at))
                                           (list (remove value face-at)))
                                   string)
             while next-face-change-pos
             do (setf pos next-face-change-pos))))

(cl-defun ement--text-property-search-forward (property predicate string &key (start 0))
  "Return the position at which PROPERTY in STRING matches PREDICATE.
Return nil if not found.  Searches forward from START."
  (declare (indent defun))
  (cl-loop for pos = start then (next-single-property-change pos property string)
           while pos
           when (funcall predicate (get-text-property pos property string))
           return pos))

(cl-defun ement--text-property-search-backward (property predicate string &key (start 0))
  "Return the position at which PROPERTY in STRING matches PREDICATE.
Return nil if not found.  Searches backward from START."
  (declare (indent defun))
  (cl-loop for pos = start then (previous-single-property-change pos property string)
           while (and pos (> pos 1))
           when (funcall predicate (get-text-property (1- pos) property string))
           return pos))

(defun ement--resize-image (image max-width max-height)
  "Return a copy of IMAGE set to MAX-WIDTH and MAX-HEIGHT.
IMAGE should be one as created by, e.g. `create-image'."
  (declare
   ;; This silences a lint warning on our GitHub CI runs, which use a build of Emacs
   ;; without image support.
   (function image-property "image"))
  ;; It would be nice if the image library had some simple functions to do this sort of thing.
  (let ((new-image (cl-copy-list image)))
    (when (fboundp 'imagemagick-types)
      ;; Only do this when ImageMagick is supported.
      ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
      (setf (image-property new-image :type) 'imagemagick))
    (setf (image-property new-image :max-width) max-width
          (image-property new-image :max-height) max-height)
    new-image))

(defun ement--room-alias (room)
  "Return latest m.room.canonical_alias event in ROOM."
  ;; FIXME: This function probably needs to compare timestamps to ensure that older events
  ;; that are inserted at the head of the events lists aren't used instead of newer ones.
  (or (cl-loop for event in (ement-room-timeline room)
               when (equal "m.room.canonical_alias" (ement-event-type event))
               return (alist-get 'alias (ement-event-content event)))
      (cl-loop for event in (ement-room-state room)
               when (equal "m.room.canonical_alias" (ement-event-type event))
               return (alist-get 'alias (ement-event-content event)))))

(declare-function magit-current-section "magit-section")
(declare-function eieio-oref "eieio-core")
(defun ement--room-at-point ()
  "Return room at point.
Works in major-modes `ement-room-mode',
`ement-tabulated-room-list-mode', and `ement-room-list-mode'."
  (pcase major-mode
    ('ement-room-mode ement-room)
    ('ement-tabulated-room-list-mode (tabulated-list-get-id))
    ('ement-room-list-mode
     (cl-typecase (oref (magit-current-section) value)
       (taxy-magit-section nil)
       (t (pcase (oref (magit-current-section) value)
            (`[,room ,_session] room)))))))

(defun ement--room-direct-p (room session)
  "Return non-nil if ROOM on SESSION is a direct chat."
  (cl-labels ((content-contains-room-id (content room-id)
                (cl-loop for (_user-id . room-ids) in content
                         ;; NOTE: room-ids is a vector.
                         thereis (seq-contains-p room-ids room-id))))
    (pcase-let* (((cl-struct ement-session account-data) session)
                 ((cl-struct ement-room id) room))
      (or (cl-loop for event in account-data
                   when (equal "m.direct" (alist-get 'type event))
                   thereis (content-contains-room-id (alist-get 'content event) id))
          (cl-loop
           ;; Invited rooms have no account-data yet, and their
           ;; directness flag is in invite-state events.
           for event in (ement-room-invite-state room)
           thereis (alist-get 'is_direct (ement-event-content event)))))))

(defun ement--room-display-name (room)
  "Return the displayname for ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#calculating-the-display-name-for-a-room>.
  ;; NOTE: The spec seems incomplete, because the algorithm it recommends does not say how
  ;; or when to use "m.room.member" events for rooms without heroes (e.g. invited rooms).
  ;; TODO: Add SESSION argument and use it to remove local user from names.
  (cl-labels ((latest-event (type content-field)
                (or (cl-loop for event in (ement-room-timeline room)
                             when (and (equal type (ement-event-type event))
                                       (not (string-empty-p (alist-get content-field (ement-event-content event)))))
                             return (alist-get content-field (ement-event-content event)))
                    (cl-loop for event in (ement-room-state room)
                             when (and (equal type (ement-event-type event))
                                       (not (string-empty-p (alist-get content-field (ement-event-content event)))))
                             return (alist-get content-field (ement-event-content event)))))
              (member-events-name ()
                (when-let ((member-events (cl-loop for accessor in '(ement-room-timeline ement-room-state ement-room-invite-state)
                                                   append (cl-remove-if-not (apply-partially #'equal "m.room.member")
                                                                            (funcall accessor room)
                                                                            :key #'ement-event-type))))
                  (string-join (delete-dups
                                (mapcar (lambda (event)
                                          (ement--user-displayname-in room (ement-event-sender event)))
                                        member-events))
                               ", ")))
              (heroes-name ()
                (pcase-let* (((cl-struct ement-room summary) room)
                             ((map ('m.heroes hero-ids) ('m.joined_member_count joined-count)
                                   ('m.invited_member_count invited-count))
                              summary))
                  ;; TODO: Disambiguate hero display names.
                  (when hero-ids
                    (cond ((<= (+ joined-count invited-count) 1)
                           ;; Empty room.
                           (empty-room hero-ids joined-count))
                          ((>= (length hero-ids) (1- (+ joined-count invited-count)))
                           ;; Members == heroes.
                           (hero-names hero-ids))
                          ((and (< (length hero-ids) (1- (+ joined-count invited-count)))
                                (> (+ joined-count invited-count) 1))
                           ;; More members than heroes.
                           (heroes-and-others hero-ids joined-count))))))
              (hero-names (heroes)
                (string-join (mapcar #'hero-name heroes) ", "))
              (hero-name (id)
                (if-let ((user (gethash id ement-users)))
                    (ement--user-displayname-in room user)
                  id))
              (heroes-and-others (heroes joined)
                (format "%s, and %s others" (hero-names heroes)
                        (- joined (length heroes))))
              (name-override ()
                (when-let ((event (alist-get "org.matrix.msc3015.m.room.name.override"
                                             (ement-room-account-data room)
                                             nil nil #'equal)))
                  (map-nested-elt event '(content name))))
              (empty-room (heroes joined)
                (pcase (length heroes)
                  (0 "Empty room")
                  ((pred (>= 5)) (format "Empty room (was %s)"
                                         (hero-names heroes)))
                  (_ (format "Empty room (was %s)"
                             (heroes-and-others heroes joined))))))
    (or (name-override)
        (latest-event "m.room.name" 'name)
        (latest-event "m.room.canonical_alias" 'alias)
        (heroes-name)
        (member-events-name)
        (ement-room-id room))))
(defun ement--room-favourite-p (room)
  "Return non-nil if ROOM is tagged as favourite."
  (ement--room-tagged-p "m.favourite" room))

(defun ement--room-low-priority-p (room)
  "Return non-nil if ROOM is tagged as low-priority."
  (ement--room-tagged-p "m.lowpriority" room))

(defun ement--room-tagged-p (tag room)
  "Return non-nil if ROOM has TAG."
  ;; TODO: Use `make-ement-event' on account-data events.
  (pcase-let* (((cl-struct ement-room account-data) room)
               (tag-event (alist-get "m.tag" account-data nil nil #'equal)))
    (when tag-event
      (pcase-let (((map ('content (map tags))) tag-event))
        (cl-typecase tag
          ;; Tags are symbols internally, because `json-read' converts map keys to them.
          (string (setf tag (intern tag))))
        (assoc tag tags)))))

(defun ement--room-unread-p (room session)
  "Return non-nil if ROOM is considered unread for SESSION.
The room is unread if it has a modified, live buffer; if it has
non-zero unread notification counts; or if its fully-read marker
is not at the latest known message event."
  ;; Roughly equivalent to the "red/gray/bold/idle" states listed in
  ;; <https://github.com/matrix-org/matrix-react-sdk/blob/b0af163002e8252d99b6d7075c83aadd91866735/docs/room-list-store.md#list-ordering-algorithm-importance>.
  (pcase-let* (((cl-struct ement-room timeline account-data unread-notifications receipts
                           (local (map buffer)))
                room)
               ((cl-struct ement-session user) session)
               ((cl-struct ement-user (id our-id)) user)
               ((map notification_count highlight_count) unread-notifications)
               (fully-read-event-id (map-nested-elt (alist-get "m.fully_read" account-data nil nil #'equal)
                                                    '(content event_id))))
    ;; MAYBE: Ignore whether the buffer is modified.  Since we have a better handle on how
    ;; Matrix does notifications/unreads/highlights, maybe that's not needed, and it would
    ;; be more consistent to ignore it.
    (or (and buffer (buffer-modified-p buffer))
        (and unread-notifications
             (or (not (zerop notification_count))
                 (not (zerop highlight_count))))
        ;; NOTE: This is *WAY* too complicated, but it seems roughly equivalent to doesRoomHaveUnreadMessages() from
        ;; <https://github.com/matrix-org/matrix-react-sdk/blob/7fa01ffb068f014506041bce5f02df4f17305f02/src/Unread.ts#L52>.
        (when (and (not ement-room-unread-only-counts-notifications)
                   timeline)
          ;; A room should rarely, if ever, have a nil timeline, but in case it does
          ;; (which apparently can happen, given user reports), it should not be
          ;; considered unread.
          (cl-labels ((event-counts-toward-unread-p (event)
                        ;; NOTE: We only consider message events, so membership, reaction,
                        ;; etc. events will not mark a room as unread.  Ideally, I think
                        ;; that join/leave events should, at least optionally, mark a room
                        ;; as unread (e.g. in a 1:1 room with a friend, if the other user
                        ;; left, one would probably want to know, and marking the room
                        ;; unread would help the user notice), but since membership events
                        ;; have to be processed to understand their meaning, it's not
                        ;; straightforward to know whether one should mark a room unread.

                        ;; FIXME: Use code from `ement-room--format-member-event' to
                        ;; distinguish ones that should count.
                        (equal "m.room.message" (ement-event-type event))))
            (let ((our-read-receipt-event-id (car (gethash our-id receipts)))
                  (first-counting-event (cl-find-if #'event-counts-toward-unread-p timeline)))
              (cond ((equal fully-read-event-id (ement-event-id (car timeline)))
                     ;; The fully-read marker is at the last known event: the room is read.
                     nil)
                    ((and (not our-read-receipt-event-id)
                          (when first-counting-event
                            (and (not (equal fully-read-event-id (ement-event-id first-counting-event)))
                                 (not (equal our-id (ement-user-id (ement-event-sender first-counting-event)))))))
                     ;; The room has no read receipt, and the latest message event is not
                     ;; the event at which our fully-read marker is at, and it is not sent
                     ;; by us: the room is unread.  (This is a kind of failsafe to ensure
                     ;; the user doesn't miss any messages, but it's unclear whether this
                     ;; is really correct or best.)
                     t)
                    ((equal our-id (ement-user-id (ement-event-sender (car timeline))))
                     ;; We sent the last event: the room is read.
                     nil)
                    ((and first-counting-event
                          (equal our-id (ement-user-id (ement-event-sender first-counting-event))))
                     ;; We sent the last message event: the room is read.
                     nil)
                    ((cl-loop for event in timeline
                              when (event-counts-toward-unread-p event)
                              return (and (not (equal our-read-receipt-event-id (ement-event-id event)))
                                          (not (equal fully-read-event-id (ement-event-id event)))))
                     ;; The latest message event is not the event at which our
                     ;; read-receipt or fully-read marker are at: the room is unread.
                     t))))))))

(defun ement--update-transaction-id (session)
  "Return SESSION's incremented transaction ID formatted for sending.
Increments ID and appends current timestamp to avoid reuse
problems."
  ;; TODO: Naming things is hard.
  ;; In the event that Emacs isn't killed cleanly and the session isn't saved to disk, the
  ;; transaction ID would get reused the next time the user connects.  To avoid that, we
  ;; append the current time to the ID.  (IDs are just strings, and Element does something
  ;; similar, so this seems reasonable.)
  (format "%s-%s"
          (cl-incf (ement-session-transaction-id session))
          (format-time-string "%s")))

(defun ement--user-displayname-in (room user)
  "Return the displayname for USER in ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#calculating-the-display-name-for-a-user>.
  ;; FIXME: Add step 3 of the spec.  For now we skip to step 4.

  ;; NOTE: Both state and timeline events must be searched.  (A helpful user
  ;; in #matrix-dev:matrix.org, Michael (t3chguy), clarified this for me).
  (if-let ((cached-name (gethash user (ement-room-displaynames room))))
      cached-name
    ;; Put timeline events before state events, because IIUC they should be more recent.
    (cl-labels ((join-displayname-event-p (event)
                  (and (eq user (ement-event-sender event))
                       (equal "m.room.member" (ement-event-type event))
                       (equal "join" (alist-get 'membership (ement-event-content event)))
                       (alist-get 'displayname (ement-event-content event)))))
      ;; FIXME: Should probably sort the relevant events to get the latest one.
      (if-let* ((displayname (or (cl-loop for event in (ement-room-timeline room)
                                          when (join-displayname-event-p event)
                                          return (alist-get 'displayname (ement-event-content event)))
                                 (cl-loop for event in (ement-room-state room)
                                          when (join-displayname-event-p event)
                                          return (alist-get 'displayname (ement-event-content event)))))
                (calculated-name displayname))
          (puthash user calculated-name (ement-room-displaynames room))
        ;; No membership state event: use pre-calculated displayname or ID.
        (or (ement-user-displayname user)
            (ement-user-id user))))))

(defun ement--xml-escape-string (string)
  "Return STRING having been escaped with `xml-escape-string'.
Before Emacs 28, ignores `xml-invalid-character' errors (and any
invalid characters cause STRING to remain unescaped).  After
Emacs 28, uses the NOERROR argument to `xml-escape-string'."
  (with-suppressed-warnings ((callargs xml-escape-string))
    (condition-case _
        (xml-escape-string string 'noerror)
      (wrong-number-of-arguments
       (condition-case _
           (xml-escape-string string)
         (xml-invalid-character
          ;; We still don't want to error on this, so just return the string.
          string))))))

(defun ement--mark-room-direct (room session)
  "Mark ROOM on SESSION as a direct room.
This may be used to mark rooms as direct which, for whatever
reason (like a bug in your favorite client), were not marked as
such when they were created."
  (pcase-let* (((cl-struct ement-room timeline (id room-id)) room)
               ((cl-struct ement-session (user local-user)) session)
               ((cl-struct ement-user (id local-user-id)) local-user)
               (direct-rooms-account-data-event-content
                (alist-get 'content
                           (cl-find-if (lambda (event)
                                         (equal "m.direct" (alist-get 'type event)))
                                       (ement-session-account-data session))))
               (members (delete-dups (mapcar #'ement-event-sender timeline)))
               (other-users (cl-remove local-user-id members
                                       :key #'ement-user-id :test #'equal))
               ((cl-struct ement-user (id other-user-id)) (car other-users))
               ;; The alist keys are MXIDs as symbols.
               (other-user-id (intern other-user-id))
               (existing-direct-rooms-for-user (map-elt direct-rooms-account-data-event-content other-user-id)))
    (cl-assert (= 1 (length other-users)))
    (setf (map-elt direct-rooms-account-data-event-content other-user-id)
          (cl-coerce (append existing-direct-rooms-for-user (list room-id))
                     'vector))
    (ement-put-account-data session "m.direct" direct-rooms-account-data-event-content
      :then (lambda (_data)
              (message "Ement: Room <%s> marked as direct for <%s>." room-id other-user-id)))
    (message "Ement: Marking room as direct...")))

(cl-defun ement--get-joined-members (room session &key then else)
  "Get joined members in ROOM on SESSION and call THEN with response data.
Or call ELSE with error data if request fails.  Also puts members
on `ement-users', updating their displayname and avatar URL
slots, and puts them on ROOM's `members' table."
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-room id members) room)
               (endpoint (format "rooms/%s/joined_members" (url-hexify-string id))))
    (ement-api session endpoint
      :else else
      :then (lambda (data)
              (clrhash members)
              (mapc (lambda (member)
                      (pcase-let* ((`(,id-symbol
                                      . ,(map ('avatar_url avatar-url)
                                              ('display_name display-name)))
                                    member)
                                   (member-id (symbol-name id-symbol))
                                   (user (or (gethash member-id ement-users)
                                             (puthash member-id (make-ement-user :id member-id)
                                                      ement-users))))
                        (setf (ement-user-displayname user) display-name
                              (ement-user-avatar-url user) avatar-url)
                        (puthash member-id user members)))
                    (alist-get 'joined data))
              (setf (alist-get 'fetched-members-p (ement-room-local room)) t)
              (when then
                ;; Finally, call the given callback.
                (funcall then data))))
    (message "Ement: Getting joined members in %s..." (ement--format-room room))))

(cl-defun ement--human-format-duration (seconds &optional abbreviate)
  "Return human-formatted string describing duration SECONDS.
If SECONDS is less than 1, returns \"0 seconds\".  If ABBREVIATE
is non-nil, return a shorter version, without spaces.  This is a
simple calculation that does not account for leap years, leap
seconds, etc."
  ;; Copied from `ts-human-format-duration' (same author).
  (if (< seconds 1)
      (if abbreviate "0s" "0 seconds")
    (cl-macrolet ((format> (place)
                    ;; When PLACE is greater than 0, return formatted string using its symbol name.
                    `(when (> ,place 0)
                       (format "%d%s%s" ,place
                               (if abbreviate "" " ")
                               (if abbreviate
                                   ,(substring (symbol-name place) 0 1)
                                 ,(symbol-name place)))))
                  (join-places (&rest places)
                    ;; Return string joining the names and values of PLACES.
                    `(string-join (delq nil
                                        (list ,@(cl-loop for place in places
                                                         collect `(format> ,place))))
                                  (if abbreviate "" ", "))))
      (pcase-let ((`(,years ,days ,hours ,minutes ,seconds) (ement--human-duration seconds)))
        (join-places years days hours minutes seconds)))))

(defun ement--human-duration (seconds)
  "Return list describing duration SECONDS.
List includes years, days, hours, minutes, and seconds.  This is
a simple calculation that does not account for leap years, leap
seconds, etc."
  ;; Copied from `ts-human-format-duration' (same author).
  (cl-macrolet ((dividef (place divisor)
                  ;; Divide PLACE by DIVISOR, set PLACE to the remainder, and return the quotient.
                  `(prog1 (/ ,place ,divisor)
                     (setf ,place (% ,place ,divisor)))))
    (let* ((seconds (floor seconds))
           (years (dividef seconds 31536000))
           (days (dividef seconds 86400))
           (hours (dividef seconds 3600))
           (minutes (dividef seconds 60)))
      (list years days hours minutes seconds))))

;;; Footer

(provide 'ement-lib)

;;; ement-lib.el ends here
