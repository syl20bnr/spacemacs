;;; forge-notify.el --- Notify support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'forge)

;;; Options

(defcustom forge-notifications-github-kludge 'pending-again
  "The kludge used to work around Github's abysmal notification API."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type '(choice
          (const :tag "Every updated notification becomes unread"
                 always-unread)
          (const :tag "Unless unread, updated notifications become pending"
                 pending-again)
          (const :tag (concat "Unless unread or non-nil local status, "
                              "updated notifications become pending")
                 pending-if-unset)))

(defcustom forge-notifications-repo-slug-width 28
  "Width of repository slugs in `forge-notifications-mode' buffers."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type (if (>= emacs-major-version 28) 'natnum 'number))

;;; Class

(defclass forge-notification (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-table              :initform 'notification)
   (closql-primary-key        :initform 'id)
   (closql-order-by           :initform [(desc id)])
   (id                        :initarg :id)
   (thread-id                 :initarg :thread-id)
   (repository                :initarg :repository)
   (type                      :initarg :type)
   (topic                     :initarg :topic)
   (url                       :initarg :url)
   (title                     :initarg :title)
   (reason                    :initarg :reason)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)))

;;; Query
;;;; Get

(cl-defmethod forge-get-repository ((notify forge-notification))
  "Return the object for the repository that NOTIFY belongs to."
  (and-let* ((id (oref notify repository)))
    (closql-get (forge-db) id 'forge-repository)))

(cl-defmethod forge-get-topic ((notify forge-notification))
  (and-let* ((repo (forge-get-repository notify)))
    (forge-get-topic repo (oref notify topic))))

(cl-defmethod forge-get-notification ((id string))
  (closql-get (forge-db) id 'forge-notification))

(cl-defmethod forge-get-notification ((topic forge-topic))
  (and-let* ((row (car (forge-sql [:select * :from notification
                                   :where (and (= repository $s1)
                                               (= topic $s2))]
                                  (oref topic repository)
                                  (oref topic number)))))
    (closql--remake-instance 'forge-notification (forge-db) row)))

;;;; Current

(defun forge-current-notification (&optional demand)
  "Return the current notification, casting a topic if necessary.
If there is no such notification and DEMAND is non-nil, then
signal an error."
  (or (magit-section-value-if 'notification)
      (and-let* ((topic (forge-current-topic)))
        (forge-get-notification topic))
      (and demand (user-error "No current notification"))))

(defun forge-notification-at-point (&optional demand)
  "Return the notification at point, casting a topic if necessary.
If there is no such notification and DEMAND is non-nil, then
signal an error."
  (or (magit-section-value-if 'notification)
      (and-let* ((topic (forge-topic-at-point)))
        (forge-get-notification topic))
      (and demand (user-error "No notification at point"))))

;;;; List

(defun forge--ls-notifications (status)
  (let* ((status (ensure-list status))
         (savedp (memq 'saved status))
         (status (remq 'saved status)))
    (mapcar
     (lambda (row) (closql--remake-instance 'forge-notification (forge-db) row))
     (if (seq-set-equal-p status '(unread pending done) #'eq)
         (forge-sql [:select * :from notification :order-by [(desc updated)]])
       (forge-sql
        `[:select :distinct notification:*
          :from [notification (as issue topic)]
          :where (and (= notification:topic topic:id)
                      ,@(and status '((in topic:status $v1)))
                      ,@(and savedp '((= topic:saved-p 't))))
          :union
          :select :distinct notification:*
          :from [notification (as pullreq topic)]
          :where (and (= notification:topic topic:id)
                      ,@(and status '((in topic:status $v1)))
                      ,@(and savedp '((= topic:saved-p 't))))
          :order-by [(desc notification:updated)]]
        (vconcat status))))))

;;; Mode

(defvar-keymap forge-notifications-mode-map
  :doc "Keymap for `forge-notifications-mode'."
  :parent magit-mode-map
  "<remap> <magit-refresh>" #'magit-refresh-buffer
  "L" #'forge-notification-menu)

(define-derived-mode forge-notifications-mode magit-mode "Forge Notifications"
  "Mode for looking at forge notifications."
  (hack-dir-local-variables-non-file-buffer))

(defun forge-notifications-setup-buffer (&optional create)
  (let* ((name "*forge-notifications*")
         (magit-generate-buffer-name-function (lambda (_mode _value) name))
         (default-directory "/"))
    (if create
        (magit-setup-buffer-internal #'forge-notifications-mode t
                                     '((forge-buffer-unassociated-p t))
                                     name)
      (get-buffer name))))

(defun forge-notifications-refresh-buffer ()
  (forge-insert-notifications))

(defvar forge-notifications-display-style 'flat)
(defvar forge-notifications-selection '(unread pending))

;;; Commands

(transient-define-prefix forge-notification-menu ()
  "Control list of notifications and notification at point."
  :transient-suffix t
  :transient-non-suffix t
  :transient-switch-frame nil
  :refresh-suffixes t
  [:hide always ("q" forge-menu-quit-list)]
  [["Type"
    ("t"   "topics...        " forge-topics-menu     :transient replace)
    (:info "notifications    " :face forge-active-suffix)
    ("r"   "repositories...  " forge-repository-menu :transient replace)
    ""]
   ["Selection"
    ("I" forge-notifications-display-inbox)
    ("S" forge-notifications-display-saved)
    ("D" forge-notifications-display-done)
    ("A" forge-notifications-display-all)]]
  [["Set state"
    ("s o" forge-topic-state-set-open)
    ("s c" forge-issue-state-set-completed)
    ("s u" forge-issue-state-set-unplanned)
    ("s m" forge-pullreq-state-set-merged)
    ("s x" forge-pullreq-state-set-closed)
    """Set status"
    ("s i" forge-topic-status-set-unread)
    ("s p" forge-topic-status-set-pending)
    ("s d" forge-topic-status-set-done)
    ("s s" forge-topic-toggle-saved)]
   ["Group"
    ("f" forge-notifications-style-flat)
    ("g" forge-notifications-style-nested)]
   ["Margin"
    (magit-toggle-margin)
    (magit-cycle-margin-style)
    ("e" magit-toggle-margin-details)]]
  (interactive)
  (forge-list-notifications)
  (transient-setup 'forge-notification-menu))

;;;###autoload
(defun forge-list-notifications ()
  "List notifications."
  (interactive)
  (forge-notifications-setup-buffer t))

(transient-define-suffix forge-notifications-display-inbox ()
  "List unread and pending notifications."
  :description "inbox"
  :inapt-if (lambda () (equal forge-notifications-selection '(unread pending)))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection '(unread pending))
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-saved ()
  "List saved notifications."
  :description "saved"
  :inapt-if (lambda () (eq forge-notifications-selection 'saved))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection 'saved)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-done ()
  "List done notifications."
  :description "done"
  :inapt-if (lambda () (eq forge-notifications-selection 'done))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection 'done)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-display-all ()
  "List all notifications."
  :description "all"
  :inapt-if (lambda () (equal forge-notifications-selection '(unread pending done)))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-selection '(unread pending done))
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-style-flat ()
  "Show a flat notification list."
  :description "flat list"
  :inapt-if (lambda () (eq forge-notifications-display-style 'flat))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-display-style 'flat)
  (forge-refresh-buffer))

(transient-define-suffix forge-notifications-style-nested ()
  "Group notifications by repository."
  :description "by repository"
  :inapt-if (lambda () (eq forge-notifications-display-style 'nested))
  :inapt-face 'forge-active-suffix
  (interactive)
  (unless (derived-mode-p 'forge-notifications-mode)
    (user-error "Not in notification buffer"))
  (setq forge-notifications-display-style 'nested)
  (forge-refresh-buffer))

;;; Sections

;; The double-prefix is necessary due to a limitation of magit-insert-section.
(defvar-keymap forge-forge-repo-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-this-repository
  "<remap> <magit-visit-thing>"  #'forge-visit-this-repository)

(defun forge-insert-notifications ()
  (let* ((notifs (forge--ls-notifications forge-notifications-selection))
         (status forge-notifications-selection))
    (magit-insert-section (notifications)
      (magit-insert-heading
        (cond
         ((eq status 'unread) "Unread notifications")
         ((eq status 'saved) "Saved notifications")
         ((eq status 'done) "Done notifications")
         ((not (listp status))
          (format "Notifications %s" status))
         ((seq-set-equal-p status '(unread pending)) "Inbox")
         ((seq-set-equal-p status '(unread pending done)) "Notifications")
         ((format "Notifications %s" status))))
      (if (eq forge-notifications-display-style 'flat)
          (magit-insert-section-body
            (if (not notifs)
                (insert "(empty)\n")
              (dolist (notif notifs)
                (forge-insert-notification notif))
              (insert ?\n)))
        (pcase-dolist (`(,_ . ,notifs)
                       (--group-by (oref it repository) notifs))
          (let ((repo (forge-get-repository (car notifs))))
            (magit-insert-section (forge-repo repo)
              (magit-insert-heading
                (concat (propertize (format "%s/%s"
                                            (oref repo owner)
                                            (oref repo name))
                                    'font-lock-face 'bold)
                        (format " (%s)" (length notifs))))
              (magit-insert-section-body
                (dolist (notif notifs)
                  (forge-insert-notification notif))
                (insert ?\n)))))))))

(defun forge-insert-notification (notif)
  (with-slots (type title url) notif
    (pcase type
      ((or 'issue 'pullreq)
       (forge--insert-topic (forge-get-topic notif)))
      ('commit
       (magit-insert-section (ncommit nil) ; !commit
         (string-match "[^/]*\\'" url)
         (insert
          (format "%s %s\n"
                  (propertize (substring (match-string 0 url)
                                         0 (magit-abbrev-length))
                              'font-lock-face 'magit-hash)
                  (magit-log-propertize-keywords
                   nil
                   (propertize title 'font-lock-face
                               (if-let ((topic (oref notif topic))
                                        (! (eq (oref topic status) 'unread)))
                                   'forge-topic-unread
                                 'forge-topic-open)))))))
      (_
       ;; The documentation does not mention what "types"
       ;; exist.  Make it obvious that this is something
       ;; we do not know how to handle properly yet.
       (magit-insert-section (notification notif)
         (insert (propertize (format "(%s) %s\n" type title)
                             'font-lock-face 'error)))))))

;;; _
(provide 'forge-notify)
;;; forge-notify.el ends here
