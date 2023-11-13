;;; forge-issue.el --- Issue support  -*- lexical-binding:t -*-

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
(require 'forge-post)
(require 'forge-topic)

;;; Classes

(defclass forge-issue (forge-topic)
  ((closql-table         :initform 'issue)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform 'repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (repository           :initarg :repository)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (unread-p             :initarg :unread-p :initform nil)
   (locked-p             :initarg :locked-p)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-table (issue-assignee assignee))
   (project-cards) ; projectsCards
   (edits) ; userContentEdits
   (labels               :closql-table (issue-label label))
   (participants)
   (posts                :closql-class forge-issue-post)
   (reactions)
   (timeline)
   (marks                :closql-table (issue-mark mark))
   (note                 :initarg :note :initform nil)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   ))

(defclass forge-issue-post (forge-post)
  ((closql-table         :initform 'issue-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'issue)
   (closql-class-prefix  :initform "forge-issue-")
   (id                   :initarg :id)
   (issue                :initarg :issue)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ))

;;; Query
;;;; Get

(cl-defmethod forge-get-repository ((post forge-issue-post))
  (forge-get-repository (forge-get-issue post)))

(cl-defmethod forge-get-topic ((post forge-issue-post))
  (forge-get-issue post))

(cl-defmethod forge-get-issue ((issue forge-issue))
  issue)

(cl-defmethod forge-get-issue ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-issue repo number)
              'forge-issue))

(cl-defmethod forge-get-issue ((number integer))
  (and-let* ((repo (forge-get-repository t)))
    (forge-get-issue repo number)))

(cl-defmethod forge-get-issue ((id string))
  (closql-get (forge-db) id 'forge-issue))

(cl-defmethod forge-get-issue ((post forge-issue-post))
  (closql-get (forge-db)
              (oref post issue)
              'forge-issue))

;;;; Current

(defun forge-current-issue (&optional demand)
  "Return the issue at point or being visited.
If there is no such issue and DEMAND is non-nil, then signal
an error."
  (or (forge-issue-at-point)
      (and (derived-mode-p 'forge-topic-mode)
           (forge-issue-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current issue"))))

(defun forge-issue-at-point (&optional demand)
  "Return the issue at point.
If there is no such issue and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-issue)
      (magit-section-value-if 'issue)
      (and (derived-mode-p 'forge-topic-list-mode)
           (and-let* ((id (tabulated-list-get-id))
                      (topic (forge-get-topic id)))
             (and (forge-issue-p topic)
                  topic)))
      (and demand (user-error "No issue at point"))))

(put 'forge-issue 'thing-at-point #'forge-thingatpt--issue)
(defun forge-thingatpt--issue ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at "#\\([0-9]+\\)\\_>")
         (forge-get-issue repo (string-to-number (match-string 1))))))

;;;; List

(defun forge-ls-issues (repo &optional type select)
  (forge-ls-topics repo 'forge-issue type select))

(defun forge--ls-recent-issues (repo)
  (forge-ls-recent-topics repo 'issue))

(defun forge--ls-assigned-issues (repo)
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-issue (forge-db) row))
          (forge-sql
           [:select $i1 :from [issue issue_assignee assignee]
            :where (and (= issue_assignee:issue issue:id)
                        (= issue_assignee:id    assignee:id)
                        (= issue:repository     $s2)
                        (= assignee:login       $s3)
                        (isnull issue:closed))
            :order-by [(desc updated)]]
           (vconcat (closql--table-columns (forge-db) 'issue t))
           (oref repo id)
           (ghub--username repo))))

(defun forge--ls-authored-issues (repo)
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-issue (forge-db) row))
          (forge-sql
           [:select $i1 :from [issue]
            :where (and (= issue:repository $s2)
                        (= issue:author     $s3)
                        (isnull issue:closed))
            :order-by [(desc updated)]]
           (vconcat (closql--table-columns (forge-db) 'issue t))
           (oref repo id)
           (ghub--username repo))))

;;; Read

(defun forge-read-issue (prompt &optional type)
  "Read an issue with completion using PROMPT.
TYPE can be `open', `closed', or nil to select from all issues.
TYPE can also be t to select from open issues, or all issues if
a prefix argument is in effect."
  (when (eq type t)
    (setq type (if current-prefix-arg nil 'open)))
  (let* ((default (forge-current-issue))
         (repo    (forge-get-repository (or default t)))
         (choices (mapcar #'forge--format-topic-choice
                          (forge-ls-issues repo type))))
    (cdr (assoc (magit-completing-read
                 prompt choices nil nil nil nil
                 (and default
                      (setq default (forge--format-topic-choice default))
                      (member default choices)
                      (car default)))
                choices))))

;;; Insert

(defvar-keymap forge-issues-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-issues
  "<remap> <magit-visit-thing>"  #'forge-list-issues
  "C-c C-n"                      #'forge-create-issue)

(defvar-keymap forge-issue-section-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic)

(defun forge-insert-issues ()
  "Insert a list of mostly recent and/or open issues.
Also see option `forge-topic-list-limit'."
  (forge--insert-issues "Issues" #'forge--ls-recent-issues))

(defun forge-insert-assigned-issues ()
  "Insert a list of open issues that are assigned to you."
  (forge--insert-issues "Assigned issues" #'forge--ls-assigned-issues))

(defun forge-insert-authored-issues ()
  "Insert a list of open issues that are authored by you."
  (forge--insert-issues "Authored issues" #'forge--ls-assigned-issues))

(defun forge--insert-issues (heading getter)
  (when-let ((repo (forge--assert-insert-topics-get-repository t)))
    (forge--insert-topics 'issues heading (funcall getter repo))))

;;; _
(provide 'forge-issue)
;;; forge-issue.el ends here
