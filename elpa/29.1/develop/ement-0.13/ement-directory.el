;;; ement-directory.el --- Public room directory support                       -*- lexical-binding: t; -*-

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

;; This library provides support for viewing and searching public room directories on
;; Matrix homeservers.

;; To make rendering the list flexible and useful, we'll use `taxy-magit-section'.

;;; Code:

;;;; Requirements

(require 'ement)
(require 'ement-room-list)

(require 'taxy)
(require 'taxy-magit-section)

;;;; Variables

(defvar ement-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ement-directory-RET)
    (define-key map [mouse-1] #'ement-directory-mouse-1)
    (define-key map (kbd "+") #'ement-directory-next)
    map))

(defgroup ement-directory nil
  "Options for room directories."
  :group 'ement)

;;;; Mode

(define-derived-mode ement-directory-mode magit-section-mode "Ement-Directory"
  :global nil)

(defvar-local ement-directory-etc nil
  "Alist storing information in `ement-directory' buffers.")

;;;;; Keys

(eval-and-compile
  (taxy-define-key-definer ement-directory-define-key
    ement-directory-keys "ement-directory-key" "FIXME: Docstring."))

;; TODO: Other keys like guest_can_join, world_readable, etc.  (Last-updated time would be
;; nice, but the server doesn't include that in the results.)

(ement-directory-define-key joined-p ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) ement-directory-etc))
    (when (cl-find id (ement-session-rooms session)
                   :key #'ement-room-id :test #'equal)
      "Joined")))

(ement-directory-define-key size (&key < >)
  (pcase-let (((map ('num_joined_members size)) item))
    (cond ((and < (< size <))
           (format "< %s members" <))
          ((and > (> size >))
           (format "> %s members" >)))))

(ement-directory-define-key space-p ()
  "Groups rooms that are themselves spaces."
  (pcase-let (((map ('room_type type)) item))
    (when (equal "m.space" type)
      "Spaces")))

(ement-directory-define-key people-p ()
  (pcase-let (((map ('room_id id) ('room_type type)) item)
              ((map session) ement-directory-etc))
    (pcase type
      ("m.space" nil)
      (_ (when-let ((room (cl-find id (ement-session-rooms session)
                                   :key #'ement-room-id :test #'equal))
                    ((ement--room-direct-p room session)))
           (propertize "People" 'face 'ement-room-list-direct))))))

(defcustom ement-directory-default-keys
  '((joined-p
     (people-p)
     (and :name "Rooms"
          :keys ((not people-p))))
    (space-p)
    ((size :> 10000))
    ((size :> 1000))
    ((size :> 100))
    ((size :> 10))
    ((size :< 11)))
  "Default keys."
  :type 'sexp)

;;;; Columns

(defvar-local ement-directory-room-avatar-cache (make-hash-table)
  ;; Use a buffer-local variable so that the cache is cleared when the buffer is closed.
  "Hash table caching room avatars for the `ement-directory' room list.")

(eval-and-compile
  (taxy-magit-section-define-column-definer "ement-directory"))

;; TODO: Fetch avatars (with queueing and async updating/insertion?).

(ement-directory-define-column #("✓" 0 1 (help-echo "Joined")) ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) ement-directory-etc))
    (if (cl-find id (ement-session-rooms session)
                 :key #'ement-room-id :test #'equal)
        "✓"
      " ")))

(ement-directory-define-column "Name" (:max-width 25)
  (pcase-let* (((map name ('room_id id) ('room_type type)) item)
               ((map session) ement-directory-etc)
               (room)
               (face (pcase type
                       ("m.space" 'ement-room-list-space)
                       (_ (if (and (setf room (cl-find id (ement-session-rooms session)
                                                       :key #'ement-room-id :test #'equal))
                                   (ement--room-direct-p room session))
                              'ement-room-list-direct
                            'ement-room-list-name)))))
    (propertize (or name (ement--room-display-name room))
                'face face)))

(ement-directory-define-column "Alias" (:max-width 25)
  (pcase-let (((map ('canonical_alias alias)) item))
    (or alias "")))

(ement-directory-define-column "Size" (:align 'right)
  (pcase-let (((map ('num_joined_members size)) item))
    (number-to-string size)))

(ement-directory-define-column "Topic" (:max-width 50)
  (pcase-let (((map topic) item))
    (if topic
        (replace-regexp-in-string "\n" " | " topic nil t)
      "")))

(ement-directory-define-column "ID" ()
  (pcase-let (((map ('room_id id)) item))
    id))

(unless ement-directory-columns
  ;; TODO: Automate this or document it
  (setq-default ement-directory-columns
                '("Name" "Alias" "Size" "Topic" "ID")))

;;;; Commands

;; TODO: Pagination of results.

;;;###autoload
(cl-defun ement-directory (&key server session since (limit 100))
  "View the public room directory on SERVER with SESSION.
Show up to LIMIT rooms.  Interactively, with prefix, prompt for
server and LIMIT.

SINCE may be a next-batch token."
  (interactive (let* ((session (ement-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: " nil nil
                                               (ement-server-name (ement-session-server session)))
                                (ement-server-name (ement-session-server session))))
                      (args (list :server server :session session)))
                 (when current-prefix-arg
                   (cl-callf plist-put args
                     :limit (read-number "Limit number of rooms: " 100)))
                 args))
  (pcase-let ((revert-function (lambda (&rest _ignore)
                                 (interactive)
                                 (ement-directory :server server :session session :limit limit)))
              (endpoint "publicRooms")
              (params (list (list "limit" limit))))
    (when since
      (cl-callf append params (list (list "since" since))))
    (ement-api session endpoint :params params
      :then (lambda (results)
              (pcase-let (((map ('chunk rooms) ('next_batch next-batch)
                                ('total_room_count_estimate remaining))
                           results))
                (ement-directory--view rooms :append-p since
                  :buffer-name (format "*Ement Directory: %s*" server)
                  :root-section-name (format "Ement Directory: %s" server)
                  :init-fn (lambda ()
                             (setf (alist-get 'server ement-directory-etc) server
                                   (alist-get 'session ement-directory-etc) session
                                   (alist-get 'next-batch ement-directory-etc) next-batch
                                   (alist-get 'limit ement-directory-etc) limit)
                             (setq-local revert-buffer-function revert-function)
                             (when remaining
                               ;; FIXME: The server seems to report all of the rooms on
                               ;; the server as remaining even when searching for a
                               ;; specific term like "emacs".
                               ;; TODO: Display this in a more permanent place (like a
                               ;; header or footer).
                               (message
                                (substitute-command-keys
                                 "%s rooms remaining (use \\[ement-directory-next] to fetch more)")
                                remaining)))))))
    (ement-message "Listing %s rooms on %s..." limit server)))

;;;###autoload
(cl-defun ement-directory-search (query &key server session since (limit 1000))
  "View public rooms on SERVER matching QUERY.
QUERY is a string used to filter results."
  (interactive (let* ((session (ement-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: " nil nil
                                               (ement-server-name (ement-session-server session)))
                                (ement-server-name (ement-session-server session))))
                      (query (read-string (format "Search for rooms on %s matching: " server)))
                      (args (list query :server server :session session)))
                 (when current-prefix-arg
                   (cl-callf plist-put (cdr args)
                     :limit (read-number "Limit number of rooms: " 1000)))
                 args))
  ;; TODO: Handle "include_all_networks" and "third_party_instance_id".  See § 10.5.4.
  (pcase-let* ((revert-function (lambda (&rest _ignore)
                                  (interactive)
                                  (ement-directory-search query :server server :session session)))
               (endpoint "publicRooms")
               (data (rassq-delete-all nil
                                       (ement-alist "limit" limit
                                                    "filter" (ement-alist "generic_search_term" query)
                                                    "since" since))))
    (ement-api session endpoint :method 'post :data (json-encode data)
      :then (lambda (results)
              (pcase-let (((map ('chunk rooms) ('next_batch next-batch)
                                ('total_room_count_estimate remaining))
                           results))
                (ement-directory--view rooms :append-p since
                  :buffer-name (format "*Ement Directory: \"%s\" on %s*" query server)
                  :root-section-name (format "Ement Directory: \"%s\" on %s" query server)
                  :init-fn (lambda ()
                             (setf (alist-get 'server ement-directory-etc) server
                                   (alist-get 'session ement-directory-etc) session
                                   (alist-get 'next-batch ement-directory-etc) next-batch
                                   (alist-get 'limit ement-directory-etc) limit
                                   (alist-get 'query ement-directory-etc) query)
                             (setq-local revert-buffer-function revert-function)
                             (when remaining
                               (message
                                (substitute-command-keys
                                 "%s rooms remaining (use \\[ement-directory-next] to fetch more)")
                                remaining)))))))
    (ement-message "Searching for %S on %s..." query server)))

(defun ement-directory-next ()
  "Fetch next batch of results in `ement-directory' buffer."
  (interactive)
  (pcase-let (((map next-batch query limit server session) ement-directory-etc))
    (unless next-batch
      (user-error "No more results"))
    (if query
        (ement-directory-search query :server server :session session :limit limit :since next-batch)
      (ement-directory :server server :session session :limit limit :since next-batch))))

(defun ement-directory-mouse-1 (event)
  "Call `ement-directory-RET' at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'ement-directory-RET))

(defun ement-directory-RET ()
  "View or join room at point, or cycle section at point."
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (null nil)
    (list (pcase-let* (((map ('name name) ('room_id room-id)) (oref (magit-current-section) value))
                       ((map session) ement-directory-etc)
                       (room (cl-find room-id (ement-session-rooms session)
                                      :key #'ement-room-id :test #'equal)))
            (if room
                (ement-view-room room session)
              ;; Room not joined: prompt to join.  (Don't use the alias in the prompt,
              ;; because multiple rooms might have the same alias, e.g. when one is
              ;; upgraded or tombstoned.)
              (when (yes-or-no-p (format "Join room \"%s\" <%s>? " name room-id))
                (ement-join-room room-id session)))))
    (taxy-magit-section (call-interactively #'magit-section-cycle))))

;;;; Functions

(cl-defun ement-directory--view (rooms &key init-fn append-p
                                       (buffer-name "*Ement Directory*")
                                       (root-section-name "Ement Directory")
                                       (keys ement-directory-default-keys)
                                       (display-buffer-action '(display-buffer-same-window)))
  "View ROOMS in an `ement-directory-mode' buffer.
ROOMS should be a list of rooms from an API request.  Calls
INIT-FN immediately after activating major mode.  Sets
BUFFER-NAME and ROOT-SECTION-NAME, and uses
DISPLAY-BUFFER-ACTION.  KEYS are a list of `taxy' keys.  If
APPEND-P, add ROOMS to buffer rather than replacing existing
contents.  To be called by `ement-directory-search'."
  (declare (indent defun))
  (let (column-sizes window-start)
    (cl-labels ((format-item (item)
                  ;; NOTE: We use the buffer-local variable `ement-directory-etc' rather
                  ;; than a closure variable because the taxy-magit-section struct's format
                  ;; table is not stored in it, and we can't reuse closures' variables.
                  ;; (It would be good to store the format table in the taxy-magit-section
                  ;; in the future, to make this cleaner.)
                  (gethash item (alist-get 'format-table ement-directory-etc)))
                ;; NOTE: Since these functions take an "item" (which is a [room session]
                ;; vector), they're prefixed "item-" rather than "room-".
                (size (item)
                  (pcase-let (((map ('num_joined_members size)) item))
                    size))
                (t<nil (a b) (and a (not b)))
                (t>nil (a b) (and (not a) b))
                (make-fn (&rest args)
                  (apply #'make-taxy-magit-section
                         :make #'make-fn
                         :format-fn #'format-item
                         ;; FIXME: Should we reuse `ement-room-list-level-indent' here?
                         :level-indent ement-room-list-level-indent
                         ;; :visibility-fn #'visible-p
                         ;; :heading-indent 2
                         :item-indent 2
                         ;; :heading-face-fn #'heading-face
                         args)))
      (with-current-buffer (get-buffer-create buffer-name)
        (unless (eq 'ement-directory-mode major-mode)
          ;; Don't obliterate buffer-local variables.
          (ement-directory-mode))
        (when init-fn
          (funcall init-fn))
        (pcase-let* ((taxy (if append-p
                               (alist-get 'taxy ement-directory-etc)
                             (make-fn
                              :name root-section-name
                              :take (taxy-make-take-function keys ement-directory-keys))))
                     (taxy-magit-section-insert-indent-items nil)
                     (inhibit-read-only t)
                     (pos (point))
                     (section-ident (when (magit-current-section)
                                      (magit-section-ident (magit-current-section))))
                     (format-cons))
          (setf taxy (thread-last taxy
                                  (taxy-fill (cl-coerce rooms 'list))
                                  (taxy-sort #'> #'size)
                                  (taxy-sort* #'string> #'taxy-name))
                (alist-get 'taxy ement-directory-etc) taxy
                format-cons (taxy-magit-section-format-items
                             ement-directory-columns ement-directory-column-formatters taxy)
                (alist-get 'format-table ement-directory-etc) (car format-cons)
                column-sizes (cdr format-cons)
                header-line-format (taxy-magit-section-format-header
                                    column-sizes ement-directory-column-formatters)
                window-start (if (get-buffer-window buffer-name)
                                 (window-start (get-buffer-window buffer-name))
                               0))
          (delete-all-overlays)
          (erase-buffer)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first
              ;; :blank-between-depth bufler-taxy-blank-between-depth
              :initial-depth 0))
          (goto-char pos)
          (when (and section-ident (magit-get-section section-ident))
            (goto-char (oref (magit-get-section section-ident) start)))))
      (display-buffer buffer-name display-buffer-action)
      (when (get-buffer-window buffer-name)
        (set-window-start (get-buffer-window buffer-name) window-start))
      ;; NOTE: In order for `bookmark--jump-via' to work properly, the restored buffer
      ;; must be set as the current buffer, so we have to do this explicitly here.
      (set-buffer buffer-name))))

;;;; Spaces

;; Viewing spaces and the rooms in them.

;;;###autoload
(defun ement-view-space (space session)
  ;; TODO: Use this for spaces instead of `ement-view-room' (or something like that).
  ;; TODO: Display space's topic in the header or something.
  "View child rooms in SPACE on SESSION.
SPACE may be a room ID or an `ement-room' struct."
  ;; TODO: "from" query parameter.
  (interactive (ement-complete-room :predicate #'ement--space-p
                 :prompt "Space: "))
  (pcase-let* ((id (cl-typecase space
                     (string space)
                     (ement-room (ement-room-id space))))
               (endpoint (format "rooms/%s/hierarchy" id))
               (revert-function (lambda (&rest _ignore)
                                  (interactive)
                                  (ement-view-space space session))))
    (ement-api session endpoint :version "v1"
      :then (lambda (results)
              (pcase-let (((map rooms ('next_batch next-batch))
                           results))
                (ement-directory--view rooms ;; :append-p since
                  ;; TODO: Use space's alias where possible.
                  :buffer-name (format "*Ement Directory: space %s" (ement--format-room space session))
                  :root-section-name (format "*Ement Directory: rooms in %s %s"
                                             (propertize "space"
                                                         'face 'font-lock-type-face)
                                             (ement--format-room space session))
                  :init-fn (lambda ()
                             (setf (alist-get 'session ement-directory-etc) session
                                   (alist-get 'next-batch ement-directory-etc) next-batch
                                   ;; (alist-get 'limit ement-directory-etc) limit
                                   (alist-get 'space ement-directory-etc) space)
                             (setq-local revert-buffer-function revert-function)
                             ;; TODO: Handle next batches.
                             ;; (when remaining
                             ;;   (message
                             ;;    (substitute-command-keys
                             ;;     "%s rooms remaining (use \\[ement-directory-next] to fetch more)")
                             ;;    remaining))
                             )))))))

;;;; Footer

(provide 'ement-directory)
;;; ement-directory.el ends here
