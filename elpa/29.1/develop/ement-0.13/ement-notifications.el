;;; ement-notifications.el --- Notifications support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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

;; This library implements support for Matrix notifications.  It differs from
;; `ement-notify', which implements a kind of bespoke notification system for events
;; received via sync requests rather than Matrix's own notifications endpoint.  These two
;; libraries currently integrate somewhat, as newly arriving events are handled and
;; notified about by `ement-notify', and old notifications are fetched and listed by
;; `ement-notifications' in the same "*Ement Notifications*" buffer.

;; In the future, these libraries will likely be consolidated and enhanced to more closely
;; follow the Matrix API's and Element client's examples.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'ement-lib)
(require 'ement-room)
(require 'ement-notify)

;;;; Structs

(cl-defstruct ement-notification
  "Represents a Matrix notification."
  room-id event readp)

(defun ement-notifications--make (notification)
  "Return an `ement-notification' struct for NOTIFICATION.
NOTIFICATION is an alist representing a notification returned
from the \"/notifications\" endpoint.  The notification's event
is passed through `ement--make-event'."
  (declare (function ement--make-event "ement"))
  (pcase-let (((map room_id _actions _ts event read) notification))
    (make-ement-notification :room-id room_id :readp read
                             :event (ement--make-event event))))

;;;; Variables

(declare-function ement-room-list "ement-room-list")
(defvar ement-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'ement-notify-reply)
    (define-key map (kbd "M-g M-l") #'ement-room-list)
    (define-key map (kbd "M-g M-m") #'ement-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'ement-notify-switch-to-notifications-buffer)
    (define-key map [remap scroll-down-command] #'ement-notifications-scroll-down-command)
    (define-key map [remap mwheel-scroll] #'ement-notifications-mwheel-scroll)
    (make-composed-keymap (list map button-buffer-map) 'view-mode-map))
  "Map for Ement notification buffers.")

(defvar ement-notifications-hook '(ement-notifications-log-to-buffer)
  "Functions called for `ement-notifications' notifications.
Each function is called with two arguments, the session and the
`ement-notification' struct.")

(defvar-local ement-notifications-retro-loading nil
  "Non-nil when earlier messages are being loaded.
Used to avoid overlapping requests.")

(defvar-local ement-notifications-metadata nil
  "Metadata for `ement-notifications' buffers.")

;; Variables from other files.
(defvar ement-ewoc)
(defvar ement-session)
(defvar ement-notify-prism-background)
(defvar ement-room-message-format-spec)
(defvar ement-room-sender-in-left-margin)

;;;; Commands

;;;###autoload
(cl-defun ement-notifications
    (session &key from limit only
             (then (apply-partially #'ement-notifications-callback session)) else)
  "Show the notifications buffer for SESSION.
FROM may be a \"next_token\" token from a previous request.
LIMIT may be a maximum number of events to return.  ONLY may be
the string \"highlight\" to only return notifications that have
the highlight tweak set.  THEN and ELSE may be callbacks passed
to `ement-api', which see."
  (interactive (list (ement-complete-session)
                     :only (when current-prefix-arg
                             "highlight")))
  (if-let ((buffer (get-buffer "*Ement Notifications*")))
      (switch-to-buffer buffer)
    (let ((endpoint "notifications")
          (params (remq nil
                        (list (when from
                                (list "from" from))
                              (when limit
                                (list "limit" (number-to-string limit)))
                              (when only
                                (list "only" only))))))
      (ement-api session endpoint :params params :then then :else else)
      (ement-message "Fetching notifications for <%s>..." (ement-user-id (ement-session-user session))))))

(cl-defun ement-notifications-callback (session data &key (buffer (ement-notifications--log-buffer)))
  "Callback for `ement-notifications' on SESSION which receives DATA."
  (pcase-let (((map notifications next_token) data))
    (with-current-buffer buffer
      (setf (map-elt ement-notifications-metadata :next-token) next_token)
      (cl-loop for notification across notifications
               do (run-hook-with-args 'ement-notifications-hook
                                      session (ement-notifications--make notification)))
      ;; TODO: Pass start/end nodes to `ement-room--insert-ts-headers' if possible.
      (ement-room--insert-ts-headers)
      (switch-to-buffer (current-buffer)))))

(defun ement-notifications-scroll-down-command ()
  "Scroll down, and load NUMBER earlier messages when at top."
  (interactive)
  (condition-case _err
      (scroll-down nil)
    (beginning-of-buffer
     (call-interactively #'ement-notifications-retro))))

(defun ement-notifications-mwheel-scroll (event)
  "Scroll according to EVENT, loading earlier messages when at top."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (mwheel-scroll event)
    (when (= (point-min) (window-start))
      (call-interactively #'ement-notifications-retro))))

(cl-defun ement-notifications-retro (session number)
  ;; FIXME: Naming things is hard.
  "Retrieve NUMBER older notifications on SESSION."
  ;; FIXME: Support multiple sessions.
  (interactive (list (ement-complete-session)
                     (cl-typecase current-prefix-arg
                       (null 100)
                       (list (read-number "Number of messages: "))
                       (number current-prefix-arg))))
  (cl-assert (eq 'ement-notifications-mode major-mode))
  (cl-assert (map-elt ement-notifications-metadata :next-token) nil
             "No more notifications for %s" (ement-user-id (ement-session-user ement-session)))
  (let ((buffer (current-buffer)))
    (unless ement-notifications-retro-loading
      (ement-notifications
       session :limit number
       :from (map-elt ement-notifications-metadata :next-token)
       ;; TODO: Use a :finally for resetting `ement-notifications-retro-loading'?
       :then (lambda (data)
               (unwind-protect
                   (ement-notifications-callback session data :buffer buffer)
                 (setf (buffer-local-value 'ement-notifications-retro-loading buffer) nil)))
       :else (lambda (plz-error)
               (setf (buffer-local-value 'ement-notifications-retro-loading buffer) nil)
               (ement-api-error plz-error)))
      (ement-message "Loading %s earlier messages..." number)
      (setf ement-notifications-retro-loading t))))

;;;; Functions

(cl-defun ement-notifications-log-to-buffer (session notification &key (buffer-name "*Ement Notifications*"))
  "Log EVENT in ROOM on SESSION to \"*Ement NOTIFICATIONS*\" buffer."
  (with-demoted-errors "ement-notifications-log-to-buffer: %S"
    (with-current-buffer (ement-notifications--log-buffer :name buffer-name)
      (save-window-excursion
        (when-let ((buffer-window (get-buffer-window (current-buffer))))
          ;; Select the buffer's window to avoid EWOC bug.  (See #191.)
          (select-window buffer-window))
        ;; TODO: Use the :readp slot to mark unread events.
        (save-mark-and-excursion
          (pcase-let* (((cl-struct ement-notification room-id event) notification)
                       (ement-session session)
                       (ement-room (or (cl-find room-id (ement-session-rooms session)
                                                :key #'ement-room-id :test #'equal)
                                       (error "ement-notifications-log-to-buffer: Can't find room <%s>; discarding notification" room-id)))
                       (ement-room-sender-in-left-margin nil)
                       (ement-room-message-format-spec "%o%O »%W %S> %B%R%t")
                       (new-node (ement-room--insert-event event))
                       (inhibit-read-only t)
                       (start) (end))
            (ewoc-goto-node ement-ewoc new-node)
            (setf start (point))
            (if-let (next-node (ewoc-next ement-ewoc new-node))
                (ewoc-goto-node ement-ewoc next-node)
              (goto-char (point-max)))
            (setf end (- (point) 2))
            (add-text-properties start end
                                 (list 'button '(t)
                                       'category 'default-button
                                       'action #'ement-notify-button-action
                                       'session session
                                       'room ement-room
                                       'event event))
            ;; Remove button face property.
            (alter-text-property start end 'face
                                 (lambda (face)
                                   (pcase face
                                     ('button nil)
                                     ((pred listp) (remq 'button face))
                                     (_ face))))
            (when ement-notify-prism-background
              (add-face-text-property start end (list :background (ement-notifications--room-background-color ement-room)
                                                      :extend t)))))))))

(defun ement-notifications--room-background-color (room)
  "Return a background color on which to display ROOM's messages."
  (or (alist-get 'notify-background-color (ement-room-local room))
      (setf (alist-get 'notify-background-color (ement-room-local room))
            (let ((color (color-desaturate-name
                          (ement--prism-color (ement-room-id room) :contrast-with (face-foreground 'default))
                          50)))
              (if (ement--color-dark-p (color-name-to-rgb (face-background 'default)))
                  (color-darken-name color 25)
                (color-lighten-name color 25))))))

(cl-defun ement-notifications--log-buffer (&key (name "*Ement Notifications*"))
  "Return an Ement notifications buffer named NAME."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (ement-notifications-mode)
        (current-buffer))))

;;;; Mode

(define-derived-mode ement-notifications-mode ement-room-mode "Ement Notifications"
  (setf ement-room-sender-in-left-margin nil
        left-margin-width 0
        right-margin-width 8)
  (setq-local ement-room-message-format-spec "[%o%O] %S> %B%R%t"
              bookmark-make-record-function #'ement-notifications-bookmark-make-record))

;;;; Bookmark support

(require 'bookmark)

(defun ement-notifications-bookmark-make-record ()
  "Return a bookmark record for the current `ement-notifications' buffer."
  (list (buffer-name)
        ;; It seems silly to have to record the buffer name twice, but the
        ;; `bookmark-make-record' function seems to override the bookmark name sometimes,
        ;; which makes the result useless unless we save the buffer name separately.
        (cons 'buffer-name (buffer-name))
        (cons 'handler #'ement-notifications-bookmark-handler)))

(defun ement-notifications-bookmark-handler (bookmark)
  "Show `ement-notifications' buffer for BOOKMARK."
  (pcase-let ((`(,_bookmark-name . ,(map buffer-name)) bookmark))
    (switch-to-buffer (ement-notifications--log-buffer :name buffer-name))))

;;; Footer

(provide 'ement-notifications)

;;; ement-notifications.el ends here
