;;; ement-room-list.el --- List Ement rooms  -*- lexical-binding: t; -*-

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

;; This library implements a room list view using `taxy' and `taxy-magit-section' for
;; dynamic, programmable grouping.

;;; Code:

(require 'button)
(require 'rx)

(require 'persist)
(require 'svg-lib)
(require 'taxy)
(require 'taxy-magit-section)

(require 'ement-lib)

(defgroup ement-room-list nil
  "Group Ement rooms with Taxy."
  :group 'ement)

;;;; Mouse commands

;; Since mouse-activated commands must handle mouse events, we define a simple macro to
;; wrap a command into a mouse-event-accepting one.

(defmacro ement-room-list-define-mouse-command (command)
  "Define a command that calls COMMAND interactively with point at mouse event.
COMMAND should be a form that evaluates to a function symbol; if
a symbol, it should be unquoted.."
  (let ((docstring (format "Call command `%s' interactively with point at EVENT." command))
        (name (intern (format "ement-room-list-mouse-%s" command))))
    `(defun ,name (event)
       ,docstring
       (interactive "e")
       (mouse-set-point event)
       (call-interactively #',command))))

;;;; Variables

(declare-function ement-room-toggle-space "ement-room")

(defvar ement-room-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ement-room-list-RET)
    (define-key map (kbd "SPC") #'ement-room-list-next-unread)
    (define-key map [tab] #'ement-room-list-section-toggle)
    (define-key map [mouse-1] (ement-room-list-define-mouse-command ement-room-list-RET))
    (define-key map [mouse-2] (ement-room-list-define-mouse-command ement-room-list-kill-buffer))
    (define-key map (kbd "k") #'ement-room-list-kill-buffer)
    (define-key map (kbd "s") #'ement-room-toggle-space)
    map)
  "Keymap for `ement-room-list' buffers.
See also `ement-room-list-button-map'.")

(defvar ement-room-list-button-map
  ;; This map is needed because some columns are propertized as buttons, which override
  ;; the main keymap.
  ;; TODO: Is it possible to adjust the button properties to obviate this map?
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (ement-room-list-define-mouse-command ement-room-list-RET))
    (define-key map [mouse-2] (ement-room-list-define-mouse-command ement-room-list-kill-buffer))
    map)
  "Keymap for buttonized text in `ement-room-list' buffers.")

(defvar ement-room-list-timestamp-colors nil
  "List of colors used for timestamps.
Set automatically when `ement-room-list-mode' is activated.")

(defvar ement-room)
(defvar ement-session)
(defvar ement-sessions)
(defvar ement-room-prism-minimum-contrast)

;;;;; Persistent variables

(persist-defvar ement-room-list-visibility-cache nil
                "Applied to `magit-section-visibility-cache', which see.")

;;;; Customization

(defcustom ement-room-list-auto-update t
  "Automatically update the taxy-based room list buffer."
  :type 'boolean)

(defcustom ement-room-list-avatars (display-images-p)
  "Show room avatars in the room list."
  :type 'boolean)

;;;;; Faces

(defface ement-room-list-direct
  ;; In case `font-lock-constant-face' is bold, we set the weight to normal, so it can be
  ;; made bold for unread rooms only.
  '((t (:weight normal :inherit (font-lock-constant-face ement-room-list-name))))
  "Direct rooms.")

(defface ement-room-list-favourite '((t (:inherit (font-lock-doc-face ement-room-list-name))))
  "Favourite rooms.")

(defface ement-room-list-invited
  '((t (:inherit italic ement-room-list-name)))
  "Invited rooms.")

(defface ement-room-list-left
  '((t (:strike-through t :inherit ement-room-list-name)))
  "Left rooms.")

(defface ement-room-list-low-priority '((t (:inherit (font-lock-comment-face ement-room-list-name))))
  "Low-priority rooms.")

(defface ement-room-list-name
  '((t (:inherit font-lock-function-name-face button)))
  "Non-direct rooms.")

(defface ement-room-list-space '((t (:inherit (font-lock-regexp-grouping-backslash ement-room-list-name))))
  "Space rooms."
  :group 'ement-room-list)

(defface ement-room-list-unread
  '((t (:inherit bold ement-room-list-name)))
  "Unread rooms.")

(defface ement-room-list-recent '((t (:inherit font-lock-warning-face)))
  "Latest timestamp of recently updated rooms.
The foreground color is used to generate a gradient of colors
from recent to non-recent for rooms updated in the past 24
hours but at least one hour ago.")

(defface ement-room-list-very-recent '((t (:inherit error)))
  "Latest timestamp of very recently updated rooms.
The foreground color is used to generate a gradient of colors
from recent to non-recent for rooms updated in the past hour.")

;;;; Keys

;; Since some of these keys need access to the session, and room
;; structs don't include the session, we use a two-element vector in
;; which the session is the second element.

(eval-and-compile
  (taxy-define-key-definer ement-room-list-define-key
    ement-room-list-keys "ement-room-list-key" "FIXME: Docstring."))

(ement-room-list-define-key membership (&key name status)
  ;; FIXME: Docstring: status should be a symbol of either `invite', `join', `leave'.
  (cl-labels ((format-membership (membership)
                (pcase membership
                  ('join "Joined")
                  ('invite "Invited")
                  ('leave "[Left]"))))
    (pcase-let ((`[,(cl-struct ement-room (status membership)) ,_session] item))
      (if status
          (when (equal status membership)
            (or name (format-membership membership)))
        (format-membership membership)))))

(ement-room-list-define-key alias (&key name regexp)
  (pcase-let ((`[,(cl-struct ement-room canonical-alias) ,_session] item))
    (when canonical-alias
      (when (string-match-p regexp canonical-alias)
        name))))

(ement-room-list-define-key buffer ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (when buffer
      #("Buffers" 0 7 (help-echo "Rooms with open buffers")))))

(ement-room-list-define-key direct ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-direct-p room session)
      "Direct")))

(ement-room-list-define-key people ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-direct-p room session)
      (propertize "People" 'face 'ement-room-list-direct))))

(ement-room-list-define-key space (&key name id)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-session rooms) session)
               ((cl-struct ement-room type (local (map parents))) room))
    (cl-labels ((format-space (id)
                  (let* ((parent-room (cl-find id rooms :key #'ement-room-id :test #'equal))
                         (space-name (if parent-room
                                         (ement-room-display-name parent-room)
                                       id)))
                    (concat "Space: " space-name))))
      (when-let ((key (if id
                          ;; ID specified.
                          (cond ((or (member id parents)
                                     (equal id (ement-room-id room)))
                                 ;; Room is in specified space.
                                 (or name (format-space id)))
                                ((and (equal type "m.space")
                                      (equal id (ement-room-id room)))
                                 ;; Room is a specified space.
                                 (or name (concat "Space: " (ement-room-display-name room)))))
                        ;; ID not specified.
                        (pcase (length parents)
                          (0 nil)
                          (1
                           ;; TODO: Make the rooms list a hash table to avoid this lookup.
                           (format-space (car parents)))
                          (_
                           ;; TODO: How to handle this better?  (though it should be very rare)
                           (string-join (mapcar #'format-space parents) ", "))))))
        (propertize key 'face 'ement-room-list-space)))))

(ement-room-list-define-key space-p ()
  "Groups rooms that are themselves spaces."
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room type) room))
    (when (equal "m.space" type)
      "Spaces")))

(ement-room-list-define-key name (&key name regexp)
  (pcase-let* ((`[,room ,_session] item)
               (display-name (ement--room-display-name room)))
    (when display-name
      (when (string-match-p regexp display-name)
        (or name regexp)))))

(ement-room-list-define-key latest (&key name newer-than older-than)
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room latest-ts) room)
               (age))
    (when latest-ts
      (setf age (- (time-convert nil 'integer) (/ latest-ts 1000)))
      (cond (newer-than
             (when (<= age newer-than)
               (or name (format "Newer than %s seconds" newer-than))))
            (older-than
             (when (>= age older-than)
               (or name (format "Older than %s seconds" newer-than))))
            (t
             ;; Default to rooms with traffic in the last day.
             (if (<= age 86400)
                 "Last 24 hours"
               "Older than 24 hours"))))))

(ement-room-list-define-key freshness
  (&key (intervals '((86400 . "Past 24h")
                     (604800 . "Past week")
                     (2419200 . "Past month")
                     (31536000 . "Past year"))))
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room latest-ts) room)
               (age))
    (when latest-ts
      (setf age (- (time-convert nil 'integer) (/ latest-ts 1000)))
      (or (alist-get age intervals nil nil #'>)
          "Older than a year"))))

(ement-room-list-define-key session (&optional user-id)
  (pcase-let ((`[,_room ,(cl-struct ement-session
                                    (user (cl-struct ement-user id)))]
               item))
    (pcase user-id
      (`nil id)
      (_ (when (equal user-id id)
           user-id)))))

(ement-room-list-define-key topic (&key name regexp)
  (pcase-let ((`[,(cl-struct ement-room topic) ,_session] item))
    (when (and topic (string-match-p regexp topic))
      name)))

(ement-room-list-define-key unread ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-unread-p room session)
      "Unread")))

(ement-room-list-define-key favourite ()
  :then #'identity
  (pcase-let ((`[,room ,_session] item))
    (when (ement--room-favourite-p room)
      (propertize "Favourite" 'face 'ement-room-list-favourite))))

(ement-room-list-define-key low-priority ()
  :then #'identity
  (pcase-let ((`[,room ,_session] item))
    (when (ement--room-low-priority-p room)
      "Low-priority")))

(defcustom ement-room-list-default-keys
  '(;; First, group all invitations (this group will appear first since the rooms are
    ;; already sorted first).
    ((membership :status 'invite))
    ;; Group all left rooms (this group will appear last, because the rooms are already
    ;; sorted last).
    ((membership :status 'leave))
    ;; Group all favorite rooms, which are already sorted first.
    (favourite)
    ;; Group all low-priority rooms, which are already sorted last, and within that group,
    ;; group them by their space, if any.
    (low-priority space)
    ;; Group other rooms which are opened in a buffer.
    (buffer)
    ;; Group other rooms which are unread.
    (unread)
    ;; Group other rooms which are in a space by freshness, then by space.
    ((and :name "Spaced"
          :keys ((not space-p)
                 space))
     freshness space)
    ;; Group spaces themselves by their parent space (since space headers can't also be
    ;; items, we have to handle them separately; a bit of a hack, but not too bad).
    ((and :name "Spaces" :keys (space-p))
     space)
    ;; Group rooms which aren't in spaces by their freshness.
    ((and :name "Unspaced"
          :keys ((not space)
                 (not people)))
     freshness)
    ;; Group direct rooms by freshness.
    (people freshness))
  "Default keys."
  :type 'sexp)

;;;; Columns

(eval-and-compile
  (taxy-magit-section-define-column-definer "ement-room-list"))

(ement-room-list-define-column #("🐱" 0 1 (help-echo "Avatar")) (:align 'right)
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room avatar display-name
                           (local (map room-list-avatar)))
                room))
    (if ement-room-list-avatars
        (or room-list-avatar
            (let ((new-avatar
                   (if avatar
                       ;; NOTE: We resize every avatar to be suitable for this buffer, rather than using
                       ;; the one cached in the room's struct.  If the buffer's faces change height, this
                       ;; will need refreshing, but it should be worth it to avoid resizing the images on
                       ;; every update.
                       (propertize " " 'display
                                   (ement--resize-image (get-text-property 0 'display avatar)
                                                        nil (frame-char-height)))
                     ;; Room has no avatar: make one.
                     (let* ((string (or display-name (ement--room-display-name room)))
                            (ement-room-prism-minimum-contrast 1)
                            (color (ement--prism-color string :contrast-with "white")))
                       (when (string-match (rx bos (or "#" "!" "@")) string)
                         (setf string (substring string 1)))
                       (propertize " " 'display (svg-lib-tag (substring string 0 1) nil
                                                             :background color :foreground "white"
                                                             :stroke 0))))))
              (setf (alist-get 'room-list-avatar (ement-room-local room)) new-avatar)))
      ;; Avatars disabled: use a two-space string.
      " ")))

(ement-room-list-define-column "Name" (:max-width 25)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-room type) room)
               (display-name (ement--room-display-name room))
               (face))
    (or (when display-name
          ;; TODO: Use code from ement-room-list and put in a dedicated function.
          (setf face (cl-copy-list '(:inherit (ement-room-list-name))))
          ;; In concert with the "Unread" column, this is roughly equivalent to the
          ;; "red/gray/bold/idle" states listed in <https://github.com/matrix-org/matrix-react-sdk/blob/b0af163002e8252d99b6d7075c83aadd91866735/docs/room-list-store.md#list-ordering-algorithm-importance>.
          (when (ement--room-unread-p room session)
            ;; For some reason, `push' doesn't work with `map-elt'...or does it?
            (push 'ement-room-list-unread (map-elt face :inherit)))
          (when (equal "m.space" type)
            (push 'ement-room-list-space (map-elt face :inherit)))
          (when (ement--room-direct-p room session)
            (push 'ement-room-list-direct (map-elt face :inherit)))
          (when (ement--room-favourite-p room)
            (push 'ement-room-list-favourite (map-elt face :inherit)))
          (when (ement--room-low-priority-p room)
            (push 'ement-room-list-low-priority (map-elt face :inherit)))
          (pcase (ement-room-status room)
            ('invite
             (push 'ement-room-list-invited (map-elt face :inherit)))
            ('leave
             (push 'ement-room-list-left (map-elt face :inherit))))
          (propertize display-name
                      'face face
                      'mouse-face 'highlight
                      'keymap ement-room-list-button-map))
        "")))

(ement-room-list-define-column #("Unread" 0 6 (help-echo "Unread events (Notifications:Highlights)")) (:align 'right)
  (pcase-let* ((`[,(cl-struct ement-room unread-notifications) ,_session] item)
               ((map notification_count highlight_count) unread-notifications))
    (if (or (not unread-notifications)
            (and (equal 0 notification_count)
                 (equal 0 highlight_count)))
        ""
      (concat (propertize (number-to-string notification_count)
                          'face (if (zerop highlight_count)
                                    'default
                                  'ement-room-mention))
              ":"
              (propertize (number-to-string highlight_count)
                          'face 'highlight)))))

(ement-room-list-define-column "Latest" ()
  (pcase-let ((`[,(cl-struct ement-room latest-ts) ,_session] item))
    (if latest-ts
        (let* ((difference-seconds (- (float-time) (/ latest-ts 1000)))
               (n (cl-typecase difference-seconds
                    ((number 0 3599) ;; <1 hour: 10-minute periods.
                     (truncate (/ difference-seconds 600)))
                    ((number 3600 86400) ;; 1 hour to 1 day: 24 1-hour periods.
                     (+ 6 (truncate (/ difference-seconds 3600))))
                    (otherwise ;; Difference in weeks.
                     (min (/ (length ement-room-list-timestamp-colors) 2)
                          (+ 24 (truncate (/ difference-seconds 86400 7)))))))
               (face (list :foreground (elt ement-room-list-timestamp-colors n)))
               (formatted-ts (ement--human-format-duration difference-seconds 'abbreviate)))
          (string-match (rx (1+ digit) (repeat 1 alpha)) formatted-ts)
          (propertize (match-string 0 formatted-ts) 'face face
                      'help-echo formatted-ts))
      "")))

(ement-room-list-define-column "Topic" (:max-width 35)
  (pcase-let ((`[,(cl-struct ement-room topic status) ,_session] item))
    ;; FIXME: Can the status and type unified, or is this inherent to the spec?
    (when topic
      (setf topic (replace-regexp-in-string "\n" " " topic 'fixedcase 'literal)))
    (pcase status
      ('invite (concat (propertize "[invited]"
                                   'face 'ement-room-list-invited)
                       " " topic))
      ('leave (concat (propertize "[left]"
                                  'face 'ement-room-list-left)
                      " " topic))
      (_ (or topic "")))))

(ement-room-list-define-column "Members" (:align 'right)
  (pcase-let ((`[,(cl-struct ement-room
                             (summary (map ('m.joined_member_count member-count))))
                 ,_session]
               item))
    (if member-count
        (number-to-string member-count)
      "")))

(ement-room-list-define-column #("Notifications" 0 5 (help-echo "Notification state")) ()
  (pcase-let* ((`[,room ,session] item))
    (pcase (ement-room-notification-state room session)
      ('nil "default")
      ('all-loud "all (loud)")
      ('all "all")
      ('mentions-and-keywords "mentions")
      ('none "none"))))

(ement-room-list-define-column #("B" 0 1 (help-echo "Buffer exists for room")) ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (if buffer
        #("B" 0 1 (help-echo "Buffer exists for room"))
      " ")))

(ement-room-list-define-column "Session" ()
  (pcase-let ((`[,_room ,(cl-struct ement-session (user (cl-struct ement-user id)))] item))
    id))

(unless ement-room-list-columns
  ;; TODO: Automate this or document it
  (setq-default ement-room-list-columns
                (get 'ement-room-list-columns 'standard-value)))

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun ement-room-list-bookmark-make-record ()
  "Return a bookmark record for the `ement-room-list' buffer."
  (list "*Ement Room List*"
        (cons 'handler #'ement-room-list-bookmark-handler)))

(defun ement-room-list-bookmark-handler (bookmark)
  "Show `ement-room-list' room list buffer for BOOKMARK."
  (pcase-let* ((`(,_bookmark-name . ,_) bookmark))
    (unless ement-sessions
      ;; MAYBE: Automatically connect.
      (user-error "No sessions connected: call `ement-connect' first"))
    (ement-room-list)))

;;;; Commands

(defun ement-room-list-section-toggle ()
  "Toggle the section at point."
  ;; HACK: For some reason, when a section's body is hidden, then the buffer is refreshed,
  ;; and then the section's body is shown again, the body is empty--but then, refreshing
  ;; the buffer shows its body.  So we work around that by refreshing the buffer when a
  ;; section is toggled.  In a way, it makes sense to do this anyway, so the user has the
  ;; most up-to-date information in the buffer.  This hack also works around a minor
  ;; visual bug that sometimes causes room avatars to be displayed in a section heading
  ;; when a section is hidden.
  (interactive)
  (ignore-errors
    ;; Ignore an error in case point is past the top-level section.
    (cl-typecase (aref (oref (magit-current-section) value) 0)
      (ement-room
       ;; HACK: Don't hide rooms themselves (they end up permanently hidden).
       nil)
      (otherwise
       (call-interactively #'magit-section-toggle)
       (revert-buffer)))))

;;;###autoload
(defun ement-room-list--after-initial-sync (&rest _ignore)
  "Call `ement-room-list', ignoring arguments.
To be called from `ement-after-initial-sync-hook'."
  (ement-room-list))

;;;###autoload
(defalias 'ement-list-rooms 'ement-room-list)

;;;###autoload
(cl-defun ement-room-list (&key (buffer-name "*Ement Room List*")
                                (keys ement-room-list-default-keys)
                                (display-buffer-action '((display-buffer-reuse-window display-buffer-same-window)))
                                ;; visibility-fn
                                )
  "Show a buffer listing Ement rooms, grouped with Taxy KEYS.
After showing it, its window is selected.  The buffer is named
BUFFER-NAME and is shown with DISPLAY-BUFFER-ACTION; or if
DISPLAY-BUFFER-ACTION is nil, the buffer is not displayed."
  (interactive)
  (let ((window-start 0) (window-point 0)
        format-table column-sizes)
    (cl-labels (;; (heading-face
                ;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
                (format-item (item) (gethash item format-table))
                ;; NOTE: Since these functions take an "item" (which is a [room session]
                ;; vector), they're prefixed "item-" rather than "room-".
                (item-latest-ts (item)
                  (or (ement-room-latest-ts (elt item 0))
                      ;; Room has no latest timestamp.  FIXME: This shouldn't
                      ;; happen, but it can, maybe due to oversights elsewhere.
                      0))
                (item-unread-p (item)
                  (pcase-let ((`[,room ,session] item))
                    (ement--room-unread-p room session)))
                (item-left-p (item)
                  (pcase-let ((`[,(cl-struct ement-room status) ,_session] item))
                    (equal 'leave status)))
                (item-buffer-p (item)
                  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
                    (buffer-live-p buffer)))
                (taxy-unread-p (taxy)
                  (or (cl-some #'item-unread-p (taxy-items taxy))
                      (cl-some #'taxy-unread-p (taxy-taxys taxy))))
                (item-space-p (item)
                  (pcase-let ((`[,(cl-struct ement-room type) ,_session] item))
                    (equal "m.space" type)))
                (item-favourite-p (item)
                  (pcase-let ((`[,room ,_session] item))
                    (ement--room-favourite-p room)))
                (item-low-priority-p (item)
                  (pcase-let ((`[,room ,_session] item))
                    (ement--room-low-priority-p room)))
                (visible-p (section)
                  ;; This is very confusing and doesn't currently work.
                  (let ((value (oref section value)))
                    (if (cl-typecase value
                          (taxy-magit-section (item-unread-p value))
                          (ement-room nil))
                        'show
                      'hide)))
                (item-invited-p (item)
                  (pcase-let ((`[,(cl-struct ement-room status) ,_session] item))
                    (equal 'invite status)))
                (taxy-latest-ts (taxy)
                  (apply #'max most-negative-fixnum
                         (delq nil
                               (list
                                (when (taxy-items taxy)
                                  (item-latest-ts (car (taxy-items taxy))))
                                (when (taxy-taxys taxy)
                                  (cl-loop for sub-taxy in (taxy-taxys taxy)
                                           maximizing (taxy-latest-ts sub-taxy)))))))
                (t<nil (a b) (and a (not b)))
                (t>nil (a b) (and (not a) b))
                (make-fn (&rest args)
                  (apply #'make-taxy-magit-section
                         :make #'make-fn
                         :format-fn #'format-item
                         :level-indent ement-room-list-level-indent
                         ;; :visibility-fn #'visible-p
                         ;; :heading-indent 2
                         :item-indent 2
                         ;; :heading-face-fn #'heading-face
                         args)))
      ;; (when (get-buffer buffer-name)
      ;;   (kill-buffer buffer-name))
      (unless ement-sessions
        (error "Ement: Not connected.  Use `ement-connect' to connect"))
      (if (not (cl-loop for (_id . session) in ement-sessions
                        thereis (ement-session-rooms session)))
          (ement-message "No rooms have been joined")
        (with-current-buffer (get-buffer-create buffer-name)
          (unless (eq 'ement-room-list-mode major-mode)
            (ement-room-list-mode))
          (let* ((room-session-vectors
                  (cl-loop for (_id . session) in ement-sessions
                           append (cl-loop for room in (ement-session-rooms session)
                                           collect (vector room session))))
                 (taxy (cl-macrolet ((first-item
                                       (pred) `(lambda (taxy)
                                                 (when (taxy-items taxy)
                                                   (,pred (car (taxy-items taxy))))))
                                     (name= (name) `(lambda (taxy)
                                                      (equal ,name (taxy-name taxy)))))
                         (thread-last
                           (make-fn
                            :name "Ement Rooms"
                            :take (taxy-make-take-function keys ement-room-list-keys))
                           (taxy-fill room-session-vectors)
                           (taxy-sort #'> #'item-latest-ts)
                           (taxy-sort #'t<nil #'item-invited-p)
                           (taxy-sort #'t<nil #'item-favourite-p)
                           (taxy-sort #'t>nil #'item-low-priority-p)
                           (taxy-sort #'t<nil #'item-unread-p)
                           (taxy-sort #'t<nil #'item-space-p)
                           ;; Within each taxy, left rooms should be sorted last so that one
                           ;; can never be the first room in the taxy (unless it's the taxy
                           ;; of left rooms), which would cause the taxy to be incorrectly
                           ;; sorted last.
                           (taxy-sort #'t>nil #'item-left-p)
                           (taxy-sort* #'string< #'taxy-name)
                           (taxy-sort* #'> #'taxy-latest-ts)
                           (taxy-sort* #'t<nil (name= "Buffers"))
                           (taxy-sort* #'t<nil (first-item item-unread-p))
                           (taxy-sort* #'t<nil (first-item item-favourite-p))
                           (taxy-sort* #'t<nil (first-item item-invited-p))
                           (taxy-sort* #'t>nil (first-item item-space-p))
                           (taxy-sort* #'t>nil (name= "Low-priority"))
                           (taxy-sort* #'t>nil (first-item item-left-p)))))
                 (taxy-magit-section-insert-indent-items nil)
                 (inhibit-read-only t)
                 (format-cons (taxy-magit-section-format-items
                               ement-room-list-columns ement-room-list-column-formatters taxy))
                 (pos (point))
                 (section-ident (when (magit-current-section)
                                  (magit-section-ident (magit-current-section)))))
            (setf format-table (car format-cons)
                  column-sizes (cdr format-cons)
                  header-line-format (taxy-magit-section-format-header
                                      column-sizes ement-room-list-column-formatters))
            (when-let ((window (get-buffer-window (current-buffer))))
              (setf window-point (window-point window)
                    window-start (window-start window)))
            (when ement-room-list-visibility-cache
              (setf magit-section-visibility-cache ement-room-list-visibility-cache))
            (add-hook 'kill-buffer-hook #'ement-room-list--cache-visibility nil 'local)
            ;; Before this point, no changes have been made to the buffer's contents.
            (delete-all-overlays)
            (erase-buffer)
            (save-excursion
              (taxy-magit-section-insert taxy :items 'first
                ;; :blank-between-depth bufler-taxy-blank-between-depth
                :initial-depth 0))
            (if-let* ((section-ident)
                      (section (magit-get-section section-ident)))
                (goto-char (oref section start))
              (goto-char pos))))
        (when display-buffer-action
          (when-let ((window (display-buffer buffer-name display-buffer-action)))
            (select-window window)))
        (when-let ((window (get-buffer-window buffer-name)))
          (set-window-start window window-start)
          (set-window-point window window-point))
        ;; FIXME: Despite all this code to save and restore point and window point and
        ;; window start, when I send a message from the minibuffer, or when I abort
        ;; sending a message from the minibuffer, point is moved to the beginning of the
        ;; buffer.  While the minibuffer is open (and the typing messages are being sent
        ;; to the server, causing it to repeatedly sync), the point stays in the correct
        ;; place.  I can't find any reason why this happens.  It makes no sense.  And
        ;; while trying to debug the problem, somehow Emacs got put into an unbreakable,
        ;; infinite loop twice; even C-g and SIGUSR2 didn't stop it.

        ;; NOTE: In order for `bookmark--jump-via' to work properly, the restored buffer
        ;; must be set as the current buffer, so we have to do this explicitly here.
        (set-buffer buffer-name)))))

(cl-defun ement-room-list-side-window (&key (side 'left))
  "Show room list in side window on SIDE.
Interactively, with prefix, show on right side; otherwise, on
left."
  (interactive (when current-prefix-arg
                 (list :side 'right)))
  (let ((display-buffer-mark-dedicated t))
    ;; Not sure if binding `display-buffer-mark-dedicated' is still necessary.
    (ement-room-list
     :display-buffer-action `(display-buffer-in-side-window
                              (dedicated . t)
                              (side . ,side)
                              (window-parameters
			       (no-delete-other-windows . t))))))

(defun ement-room-list-revert (&optional _ignore-auto _noconfirm)
  "Revert current Ement-Room-List buffer."
  (interactive)
  (with-current-buffer "*Ement Room List*"
    ;; FIXME: This caching of the visibility only supports the main buffer with the
    ;; default name, not any special ones with different names.
    (setf ement-room-list-visibility-cache magit-section-visibility-cache))
  (ement-room-list :display-buffer-action nil))

(defun ement-room-list-kill-buffer (room)
  "Kill ROOM's buffer."
  (interactive
   (ement-with-room-and-session
     (ignore ement-session)
     (list ement-room)))
  (pcase-let (((cl-struct ement-room (local (map buffer))) room)
              (kill-buffer-query-functions))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)
      (ement-room-list-revert))))

(declare-function ement-view-room "ement-room")
(defun ement-room-list-RET ()
  "View room at point, or cycle section at point."
  (declare (function ement-view-space "ement-room"))
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (vector (pcase-let ((`[,room ,session] (oref (magit-current-section) value)))
              (if (ement--space-p room)
                  (ement-view-space room session)
                (ement-view-room room session))))
    (taxy-magit-section (call-interactively #'ement-room-list-section-toggle))
    (null nil)))

(declare-function ement-room-goto-fully-read-marker "ement-room")
(defun ement-room-list-next-unread ()
  "Show next unread room."
  (interactive)
  (when (eobp)
    (goto-char (point-min)))
  (unless (cl-loop with starting-line = (line-number-at-pos)
                   for value = (oref (magit-current-section) value)
                   if (and (vectorp value)
                           (ement--room-unread-p (elt value 0) (elt value 1)))
                   do (progn
                        (ement-view-room (elt value 0)  (elt value 1))
                        (ement-room-goto-fully-read-marker)
                        (cl-return t))
                   else do (forward-line 1)
                   while (and (not (eobp))
                              (> (line-number-at-pos) starting-line)))
    ;; No more unread rooms.
    (message "No more unread rooms")))

(define-derived-mode ement-room-list-mode magit-section-mode "Ement-Room-List"
  :global nil
  (setq-local bookmark-make-record-function #'ement-room-list-bookmark-make-record
              revert-buffer-function #'ement-room-list-revert
              ement-room-list-timestamp-colors (ement-room-list--timestamp-colors)))

;;;; Functions

(defun ement-room-list--cache-visibility ()
  "Save visibility cache.
Sets `ement-room-list-visibility-cache' to the value of
`magit-section-visibility-cache'.  To be called in
`kill-buffer-hook'."
  (ignore-errors
    (when magit-section-visibility-cache
      (setf ement-room-list-visibility-cache magit-section-visibility-cache))))

;;;###autoload
(defun ement-room-list-auto-update (_session)
  "Automatically update the Taxy room list buffer.
+Does so when variable `ement-room-list-auto-update' is non-nil.
+To be called in `ement-sync-callback-hook'."
  (when (and ement-room-list-auto-update
             (buffer-live-p (get-buffer "*Ement Room List*")))
    (with-current-buffer (get-buffer "*Ement Room List*")
      (unless (region-active-p)
        ;; Don't refresh the list if the region is active (e.g. if the user is trying to
        ;; operate on multiple rooms).
        (revert-buffer)))))

(defun ement-room-list--timestamp-colors ()
  "Return a vector of generated latest-timestamp colors for rooms.
Used in `ement-tabulated-room-list' and `ement-room-list'."
  (if (or (equal "unspecified-fg" (face-foreground 'default nil 'default))
          (equal "unspecified-bg" (face-background 'default nil 'default)))
      ;; NOTE: On a TTY, the default face's foreground and background colors may be the
      ;; special values "unspecified-fg"/"unspecified-bg", in which case we can't generate
      ;; gradients, so we just return a vector of "unspecified-fg".  See
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55623>.
      (make-vector 134 "unspecified-fg")
    (cl-coerce
     (append (mapcar
              ;; One face per 10-minute period, from "recent" to 1-hour.
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'ement-room-list-very-recent
                                                                  nil 'default))
                              (color-name-to-rgb (face-foreground 'ement-room-list-recent
                                                                  nil 'default))
                              6))
             (mapcar
              ;; One face per hour, from "recent" to default.
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'ement-room-list-recent
                                                                  nil 'default))
                              (color-name-to-rgb (face-foreground 'default nil 'default))
                              24))
             (mapcar
              ;; One face per week for the last year (actually we
              ;; generate colors for the past two years' worth so
              ;; that the face for one-year-ago is halfway to
              ;; invisible, and we don't use colors past that point).
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'default nil 'default))
                              (color-name-to-rgb (face-background 'default nil 'default))
                              104)))
     'vector)))

;;;; Footer

(provide 'ement-room-list)

;;; ement-room-list.el ends here
