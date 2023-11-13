;;; forge-notify.el --- Notify support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

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

;;; Class

(defclass forge-notification (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-table              :initform 'notification)
   (closql-primary-key        :initform 'id)
   (closql-order-by           :initform [(desc id)])
   (id                        :initarg :id)
   (thread-id                 :initarg :thread-id)
   (repository                :initarg :repository)
   (forge                     :initarg :forge)
   (reason                    :initarg :reason)
   (unread-p                  :initarg :unread-p)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)
   (title                     :initarg :title)
   (type                      :initarg :type)
   (topic                     :initarg :topic)
   (url                       :initarg :url)))

;;; Special

(cl-defmethod forge-topic-mark-read ((_ forge-repository) topic)
  (oset topic unread-p nil))

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
      (and demand (user-error "No notication at point"))))

;;;; List

(defun forge--ls-notifications-all ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :order-by [(desc updated)]])))

(defun forge--ls-notifications-recent ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :order-by [(desc updated)]
                      :limit 100])))

(defun forge--ls-notifications-pending ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :where (isnull done-p)
                      :order-by [(desc updated)]])))

(defun forge--ls-notifications-unread ()
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-notification (forge-db) row))
          (forge-sql [:select * :from notification
                      :where (notnull unread-p)
                      :order-by [(desc id)]])))

;;; Mode

(defvar-keymap forge-notifications-mode-map
  :doc "Keymap for `forge-notifications-mode'."
  :parent magit-mode-map)

(define-derived-mode forge-notifications-mode magit-mode "Forge Notifications"
  "Mode for looking at forge notifications."
  (hack-dir-local-variables-non-file-buffer))

(defun forge-notifications-setup-buffer ()
  ;; There should only ever be one such buffer.
  (cl-letf (((symbol-function 'magit-get-mode-buffer)
             (lambda (&rest _)
               (get-buffer-create "*forge-notifications*"))))
    (magit-setup-buffer #'forge-notifications-mode nil
      (forge-buffer-unassociated-p t))))

(defun forge-notifications-refresh-buffer ()
  (forge-insert-notifications))

;;; Commands

(defvar forge-notifications-display-style 'flat)
(defvar forge-notifications-display-list-function
  #'forge--ls-notifications-all)

(defun forge-set-notifications-display-style ()
  "Set the value of `forge-notifications-display-style' and refresh."
  (interactive)
  (unless (eq major-mode 'forge-notifications-mode)
    (user-error "Not in forge-notifications-mode"))
  (setq forge-notifications-display-style
        (magit-read-char-case "Display notifications " t
          (?g "[g]rouped by repository" 'nested)
          (?f "as a [f]lat list"        'flat)))
  (magit-refresh))

(defun forge-set-notifications-display-selection ()
  "Set the value of `forge-notifications-display-list-function' and refresh."
  (interactive)
  (unless (eq major-mode 'forge-notifications-mode)
    (user-error "Not in forge-notifications-mode"))
  (setq forge-notifications-display-list-function
        (magit-read-char-case "Display " t
          (?a "[a]ll"     #'forge--ls-notifications-all)
          (?r "[r]ecent"  #'forge--ls-notifications-recent)
          (?p "[p]ending" #'forge--ls-notifications-pending)
          (?u "[u]nread"  #'forge--ls-notifications-unread)))
  (magit-refresh))

;;; Sections

;; The double-prefix is necessary due to a limitation of magit-insert-section.
(defvar-keymap forge-forge-repo-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-this-repository
  "<remap> <magit-visit-thing>"  #'forge-visit-this-repository)

(defun forge-insert-notifications ()
  (when-let ((notifs (funcall forge-notifications-display-list-function)))
    (magit-insert-section (notifications)
      (magit-insert-heading
        (concat (pcase forge-notifications-display-list-function
                  ('forge--ls-notifications-all     "All")
                  ('forge--ls-notifications-recent  "Recent")
                  ('forge--ls-notifications-pending "Pending")
                  ('forge--ls-notifications-unread  "Unread"))
                " notifications:"))
      (if (eq forge-notifications-display-style 'flat)
          (magit-insert-section-body
            (dolist (notif notifs)
              (forge-insert-notification notif))
            (insert ?\n))
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
  (with-slots (type title url unread-p) notif
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
                   nil (propertize title 'font-lock-face
                                   (if unread-p
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
