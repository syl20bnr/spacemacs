;;; forge-pullreq.el --- Pullreq support  -*- lexical-binding:t -*-

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
(require 'forge-post)
(require 'forge-topic)

;;; Classes

(defclass forge-pullreq (forge-topic)
  ((closql-table         :initform 'pullreq)
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
   (merged               :initarg :merged)
   (status               :initarg :status :initform nil)
   (locked-p             :initarg :locked-p)
   (editable-p           :initarg :editable-p)
   (cross-repo-p         :initarg :cross-repo-p)
   (base-ref             :initarg :base-ref)
   (base-repo            :initarg :base-repo)
   (head-ref             :initarg :head-ref)
   (head-user            :initarg :head-user)
   (head-repo            :initarg :head-repo)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-table (pullreq-assignee assignee))
   (project-cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels               :closql-table (pullreq-label label))
   (participants)
   (posts                :closql-class forge-pullreq-post)
   (reactions)
   (review-requests      :closql-table (pullreq-review-request assignee))
   (reviews)
   (timeline)
   (marks                :closql-table (pullreq-mark mark))
   (note                 :initarg :note :initform nil)
   (base-rev             :initarg :base-rev)
   (head-rev             :initarg :head-rev)
   (draft-p              :initarg :draft-p)
   (their-id             :initarg :their-id)
   (slug                 :initarg :slug)
   (saved-p              :initarg :saved-p :initform nil)
   ))

(defclass forge-pullreq-post (forge-post)
  ((closql-table         :initform 'pullreq-post)
   (closql-primary-key   :initform 'id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform 'pullreq)
   (closql-class-prefix  :initform "forge-pullreq-")
   (id                   :initarg :id)
   (pullreq              :initarg :pullreq)
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

(cl-defmethod forge-get-repository ((post forge-pullreq-post))
  (forge-get-repository (forge-get-pullreq post)))

(cl-defmethod forge-get-topic ((post forge-pullreq-post))
  (forge-get-pullreq post))

(cl-defmethod forge-get-pullreq ((pullreq forge-pullreq))
  pullreq)

(cl-defmethod forge-get-pullreq ((repo forge-repository) number)
  (closql-get (forge-db)
              (forge--object-id 'forge-pullreq repo number)
              'forge-pullreq))

(cl-defmethod forge-get-pullreq ((number integer))
  (and-let* ((repo (forge-get-repository t)))
    (forge-get-pullreq repo number)))

(cl-defmethod forge-get-pullreq ((id string))
  (closql-get (forge-db) id 'forge-pullreq))

(cl-defmethod forge-get-pullreq ((post forge-pullreq-post))
  (closql-get (forge-db)
              (oref post pullreq)
              'forge-pullreq))

(cl-defmethod forge-get-pullreq ((_(eql :branch)) branch)
  (and branch
       (and-let* ((branch (cdr (magit-split-branch-name branch)))
                  (number (magit-get "branch" branch "pullRequest")))
         (forge-get-pullreq (string-to-number number)))))

;;;; Current

(defun forge-current-pullreq (&optional demand)
  "Return the pull-request at point or being visited.
If there is no such pull-request and DEMAND is non-nil, then signal
an error."
  (or (forge-pullreq-at-point)
      (and (derived-mode-p 'forge-topic-mode)
           (forge-pullreq-p forge-buffer-topic)
           forge-buffer-topic)
      (and demand (user-error "No current pull-request"))))

(defun forge-pullreq-at-point (&optional demand)
  "Return the pull-request at point.
If there is no such pull-request and DEMAND is non-nil, then signal
an error."
  (or (thing-at-point 'forge-pullreq)
      (magit-section-value-if 'pullreq)
      (forge-get-pullreq :branch (magit-branch-at-point))
      (and (derived-mode-p 'forge-topic-list-mode)
           (and-let* ((id (tabulated-list-get-id))
                      (topic (forge-get-topic id)))
             (and (forge-pullreq-p topic)
                  topic)))
      (and demand (user-error "No pull-request at point"))))

(put 'forge-pullreq 'thing-at-point #'forge-thingatpt--pullreq)
(defun forge-thingatpt--pullreq ()
  (and-let* ((repo (forge--repo-for-thingatpt)))
    (and (thing-at-point-looking-at
          (if (forge-gitlab-repository--eieio-childp repo)
              "[#!]\\([0-9]+\\)\\_>"
            "#\\([0-9]+\\)\\_>"))
         (forge-get-pullreq repo (string-to-number (match-string 1))))))

;;;; List

(defun forge-ls-pullreqs (repo &optional type select)
  (forge-ls-topics repo 'forge-pullreq type select))

(defun forge--ls-recent-pullreqs (repo)
  (forge-ls-recent-topics repo 'pullreq))

(defun forge--ls-assigned-pullreqs (repo)
  (forge--select-pullreqs repo
    [:from pullreq
     :join pullreq_assignee :on (= pullreq_assignee:pullreq pullreq:id)
     :join assignee         :on (= pullreq_assignee:id      assignee:id)
     :where (and (= pullreq:repository $s2)
                 (= assignee:login     $s3)
                 (isnull pullreq:closed))]
    (ghub--username repo)))

(defun forge--ls-requested-reviews (repo)
  (forge--select-pullreqs repo
    [:from pullreq
     :join pullreq_review_request :on (= pullreq_review_request:pullreq pullreq:id)
     :join assignee               :on (= pullreq_review_request:id      assignee:id)
     :where (and (= pullreq:repository $s2)
                 (= assignee:login     $s3)
                 (isnull pullreq:closed))]
    (ghub--username repo)))

(defun forge--ls-authored-pullreqs (repo)
  (forge--select-pullreqs repo
    [:from [pullreq]
     :where (and (= pullreq:repository $s2)
                 (= pullreq:author     $s3)
                 (isnull pullreq:closed))]
    (ghub--username repo)))

(defun forge--ls-labeled-pullreqs (repo label)
  (forge--select-pullreqs repo
    [:from pullreq
     :join pullreq_label :on (= pullreq_label:pullreq pullreq:id)
     :join label         :on (= pullreq_label:id      label:id)
     :where (and (= pullreq:repository  $s2)
                 (= label:name        $s3)
                 (isnull pullreq:closed))]
    label))

(defun forge--ls-owned-pullreqs ()
  (forge--select-pullreqs nil
    [:from [pullreq repository]
     :where (and (= pullreq:repository repository:id)
                 (in repository:owner $v2)
                 (not (in repository:name $v3))
                 (isnull pullreq:closed))
     :order-by [(asc repository:owner)
                (asc repository:name)
                (desc pullreq:number)]]
    (vconcat (mapcar #'car forge-owned-accounts))
    (vconcat forge-owned-ignored)))

(defun forge--select-pullreqs (repo query &rest args)
  (declare (indent 1))
  (let ((db (forge-db)))
    (mapcar (lambda (row)
              (closql--remake-instance 'forge-pullreq db row))
            (apply #'forge-sql
                   (vconcat [:select $i1]
                            query
                            (and (not (cl-find :order-by query))
                                 [:order-by [(desc updated)]]))
                   (vconcat (closql--table-columns db 'pullreq t))
                   (if repo
                       (cons (oref repo id) args)
                     args)))))

;;; Read

(defun forge-read-pullreq (prompt &optional type)
  "Read a pull-request with completion using PROMPT.
TYPE can be `open', `closed', or nil to select from all
pull-requests.  TYPE can also be t to select from open
pull-requests, or all pull-requests if a prefix argument
is in effect."
  (when (eq type t)
    (setq type (if current-prefix-arg nil 'open)))
  (let* ((default (forge-current-pullreq))
         (repo    (forge-get-repository (or default t)))
         (choices (mapcar #'forge--format-topic-choice
                          (forge-ls-pullreqs repo type))))
    (cdr (assoc (magit-completing-read
                 prompt choices nil nil nil nil
                 (and default
                      (setq default (forge--format-topic-choice default))
                      (member default choices)
                      (car default)))
                choices))))

;;; Utilities

(defun forge--pullreq-branch-internal (pullreq)
  (let ((branch (oref pullreq head-ref)))
    ;; It is invalid for a branch name to begin with a colon, yet
    ;; that is what Gitlab uses when a pull-request's source branch
    ;; has been deleted.  On Github this is simply nil in the same
    ;; situation.
    (and branch (not (string-prefix-p ":" branch)) branch)))

(defun forge--pullreq-branch-active (pullreq)
  (let* ((number (number-to-string (oref pullreq number)))
         (branch-n (format "pr-%s" number))
         (branch (forge--pullreq-branch-internal pullreq)))
    (or (and (magit-branch-p branch)
             (equal (magit-get "branch" branch "pullRequest") number)
             branch)
        (and (magit-branch-p branch-n)
             (equal (magit-get "branch" branch-n "pullRequest") number)
             branch-n))))

(defun forge--pullreq-ref (pullreq)
  (let ((ref (format "refs/pullreqs/%s" (oref pullreq number))))
    (and (magit-rev-verify ref) ref)))

(defun forge--pullreq-range (pullreq &optional endpoints)
  (and-let* ((head (forge--pullreq-ref pullreq)))
    (concat (forge--get-remote) "/" (oref pullreq base-ref)
            (if endpoints "..." "..")
            head)))

;;; Insert

(defvar-keymap forge-pullreqs-section-map
  "<remap> <magit-browse-thing>" #'forge-browse-pullreqs
  "<remap> <magit-visit-thing>"  #'forge-list-pullreqs
  "C-c C-n"                      #'forge-create-pullreq)

(defvar-keymap forge-pullreq-section-map
  "<remap> <magit-visit-thing>"  #'forge-visit-this-topic)

(defun forge-insert-pullreqs ()
  "Insert a list of mostly recent and/or open pull-requests.
Also see option `forge-topic-list-limit'."
  (forge--insert-pullreqs "Pull requests"
                          #'forge--ls-recent-pullreqs))

(defun forge-insert-assigned-pullreqs ()
  "Insert a list of open pull-requests that are assigned to you."
  (forge--insert-pullreqs "Assigned pull requests"
                          #'forge--ls-assigned-pullreqs))

(defun forge-insert-requested-reviews ()
  "Insert a list of pull-requests that are awaiting your review."
  (forge--insert-pullreqs "Pull requests awaiting review"
                          #'forge--ls-requested-reviews))

(defun forge-insert-authored-pullreqs ()
  "Insert a list of open pullreqs that are authored by you."
  (forge--insert-pullreqs "Authored pullreqs"
                          #'forge--ls-authored-pullreqs))

(defun forge--insert-pullreqs (heading getter)
  (when-let ((repo (forge--assert-insert-topics-get-repository)))
    (forge--insert-topics 'pullreqs heading (funcall getter repo))))

(defun forge--insert-pullreq-commits (pullreq &optional all)
  (cl-letf (((symbol-function #'magit-cancel-section) (lambda ())))
    (if all
        ;; Numeric pr ref, pr branch (if it exists) and api
        ;; pr range may be out of sync.  Just show them all.
        (magit-insert-section-body
          (magit--insert-log nil
           (delq nil (list (concat "^" (or (oref pullreq base-rev)
                                           (concat (forge--get-remote) "/"
                                                   (oref pullreq base-ref))))
                           (forge--pullreq-ref pullreq)
                           (forge--pullreq-branch-active pullreq)
                           (and-let* ((branch (oref pullreq head-ref)))
                             (and (magit-local-branch-p branch) branch))))
           (seq-uniq (cons "--graph" magit-buffer-log-args)))
          (magit-make-margin-overlay nil t))
      (when-let ((range (forge--pullreq-range pullreq)))
        (magit-insert-section-body
          (magit--insert-log nil range magit-buffer-log-args)
          (magit-make-margin-overlay nil t))))))

;;; _
(provide 'forge-pullreq)
;;; forge-pullreq.el ends here
