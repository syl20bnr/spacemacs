;;; ghub-graphql.el --- Access Github API using GrapthQL  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

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

(require 'ghub)
(require 'gsexp)
(require 'treepy)

;; Needed for Emacs < 27.
(eval-when-compile (require 'json))
(declare-function json-read-from-string "json" (string))
(declare-function json-encode "json" (object))

(eval-when-compile (require 'pp)) ; Needed for Emacs < 29.
(eval-when-compile (require 'subr-x))

;;; Api

(defvar ghub-graphql-items-per-request 100
  "Number of GraphQL items to query for entities that return a collection.

Adjust this value if you're hitting query timeouts against larger
repositories.")

(cl-defun ghub-graphql (graphql
                        &optional variables
                        &key username auth host forge
                        headers silent
                        callback errorback value extra)
  "Make a GraphQL request using GRAPHQL and VARIABLES.
Return the response as a JSON-like alist.  Even if the response
contains `errors', do not raise an error.  GRAPHQL is a GraphQL
string.  VARIABLES is a JSON-like alist.  The other arguments
behave as for `ghub-request' (which see)."
  (cl-assert (not (stringp variables)))
  (cl-assert (or (stringp graphql)
                 (memq (car-safe graphql) '(query mutation))))
  (unless (stringp graphql)
    (setq graphql (gsexp-encode (ghub--graphql-prepare-query graphql))))
  (ghub-request "POST"
                (if (eq forge 'gitlab) "/api/graphql" "/graphql")
                nil
                :payload `((query . ,graphql)
                           ,@(and variables `((variables ,@variables))))
                :headers headers :silent silent
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback
                :extra extra :value value))

(cl-defun ghub-graphql-rate-limit (&key username auth host)
  "Return rate limit information."
  (let-alist (ghub-graphql
              '(query (rateLimit limit cost remaining resetAt))
              nil :username username :auth auth :host host)
    .data.rateLimit))

(cl-defun ghub--repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (let-alist (ghub-graphql
              '(query (repository [(owner $owner String!)
                                   (name  $name  String!)]
                                  id))
              `((owner . ,owner)
                (name  . ,name))
              :username username :auth auth :host host)
    .data.repository.id))

;;; Api (drafts)

(defconst ghub-fetch-repository-sparse
  '(query
    (repository
     [(owner $owner String!)
      (name  $name  String!)]
     name
     id
     createdAt
     updatedAt
     nameWithOwner
     (parent nameWithOwner)
     description
     homepageUrl
     (defaultBranchRef name)
     isArchived
     isFork
     isLocked
     isMirror
     isPrivate
     hasIssuesEnabled
     hasWikiEnabled
     (licenseInfo name)
     (stargazers totalCount)
     (watchers totalCount))))

(defconst ghub-fetch-repository
  `(query
    (repository
     ,@(cdr (cadr ghub-fetch-repository-sparse))
     (assignableUsers [(:edges t)]
                      id
                      login
                      name)
     (issues         [(:edges t)
                      (:singular issue number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     id
                     state
                     stateReason
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     locked
                     (milestone id)
                     body
                     (assignees [(:edges t)]
                                id)
                     (comments  [(:edges t)]
                                id
                                databaseId
                                (author login)
                                createdAt
                                updatedAt
                                body)
                     (labels    [(:edges t)]
                                id))
     (labels         [(:edges t)
                      (:singular label id)]
                     id
                     name
                     color
                     description)
     (milestones     [(:edges t)
                      (:singular milestone id)]
                     id
                     number
                     title
                     createdAt
                     updatedAt
                     dueOn
                     closedAt
                     description)
     (pullRequests   [(:edges t)
                      (:singular pullRequest number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     id
                     state
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     mergedAt
                     isDraft
                     locked
                     maintainerCanModify
                     isCrossRepository
                     (milestone id)
                     body
                     (baseRef name
                              (repository nameWithOwner))
                     baseRefOid
                     (headRef name
                              (repository (owner login)
                                          nameWithOwner))
                     headRefOid
                     (assignees [(:edges t)]
                                id)
                     (reviewRequests [(:edges t)]
                                     (requestedReviewer "... on User { id }\n"))
                     (comments  [(:edges t)]
                                id
                                databaseId
                                (author login)
                                createdAt
                                updatedAt
                                body)
                     (labels    [(:edges t)]
                                id)))))

(defconst ghub-fetch-repository-review-threads
  '(query
    (repository
     [(owner $owner String!)
      (name  $name  String!)]
     (pullRequests   [(:edges t)
                      (:singular pullRequest number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     baseRefOid
                     headRefOid
                     (reviewThreads [(:edges t)]
                                    id
                                    line
                                    originalLine
                                    diffSide
                                    (resolvedBy login)
                                    (comments [(:edges t)]
                                              id
                                              databaseId
                                              (author login)
                                              createdAt
                                              updatedAt
                                              body
                                              (replyTo databaseId)
                                              (originalCommit oid)
                                              path))))))

(cl-defun ghub-fetch-repository ( owner name callback
                                  &optional until
                                  &key username auth host forge
                                  headers errorback sparse)
  "Asynchronously fetch forge data about the specified repository.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (if sparse
                            ghub-fetch-repository-sparse
                          ghub-fetch-repository)
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge
                        :headers  headers
                        :errorback errorback))

(cl-defun ghub-fetch-issue ( owner name number callback
                             &optional until
                             &key username auth host forge
                             headers errorback)
  "Asynchronously fetch forge data about the specified issue.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (ghub--graphql-prepare-query
                         ghub-fetch-repository
                         `(repository issues (issue . ,number)))
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository issue)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge
                        :headers  headers
                        :errorback errorback))

(cl-defun ghub-fetch-pullreq ( owner name number callback
                               &optional until
                               &key username auth host forge
                               headers errorback)
  "Asynchronously fetch forge data about the specified pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (ghub--graphql-prepare-query
                         ghub-fetch-repository
                         `(repository pullRequests (pullRequest . ,number)))
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository pullRequest)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge
                        :headers  headers
                        :errorback errorback))

(cl-defun ghub-fetch-review-threads ( owner name number callback
                                      &optional until
                                      &key username auth host forge
                                      headers errorback)
  "Asynchronously fetch forge data about the review threads from a pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (ghub--graphql-prepare-query
                         ghub-fetch-repository-review-threads
                         `(repository pullRequests (pullRequest . ,number)))
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository pullRequest)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge
                        :headers  headers
                        :errorback errorback))

;;; Internal

(cl-defstruct (ghub--graphql-req
               (:include ghub--req)
               (:constructor ghub--make-graphql-req)
               (:copier nil))
  (query     nil :read-only t)
  (query-str nil :read-only nil)
  (variables nil :read-only t)
  (until     nil :read-only t)
  (buffer    nil :read-only t)
  (pages     0   :read-only nil))

(cl-defun ghub--graphql-vacuum ( query variables callback
                                 &optional until
                                 &key narrow username auth host forge
                                 headers errorback)
  "Make a GraphQL request using QUERY and VARIABLES.
See Info node `(ghub)GraphQL Support'."
  (unless host
    (setq host (ghub--host forge)))
  (unless (or username (stringp auth) (eq auth 'none))
    (setq username (ghub--username host forge)))
  (ghub--graphql-retrieve
   (ghub--make-graphql-req
    :url       (url-generic-parse-url
                (format "https://%s/graphql"
                        (if (string-suffix-p "/v3" host)
                            (substring host 0 -3)
                          host)))
    :method    "POST"
    :headers   (ghub--headers headers host auth username forge)
    :handler   #'ghub--graphql-handle-response
    :query     query
    :variables variables
    :until     until
    :buffer    (current-buffer)
    :callback  (and (not (eq callback 'synchronous))
                    (let ((buf (current-buffer)))
                      (if narrow
                          (lambda (data)
                            (let ((path narrow) key)
                              (while (setq key (pop path))
                                (setq data (cdr (assq key data)))))
                            (ghub--graphql-set-mode-line buf nil)
                            (funcall (or callback #'ghub--graphql-pp-response)
                                     data))
                        (lambda (data)
                          (ghub--graphql-set-mode-line buf nil)
                          (funcall (or callback #'ghub--graphql-pp-response)
                                   data)))))
    :errorback (and (not (eq callback 'synchronous))
                    errorback))))

(cl-defun ghub--graphql-retrieve (req &optional lineage cursor)
  (let ((p (cl-incf (ghub--graphql-req-pages req))))
    (when (> p 1)
      (ghub--graphql-set-mode-line req "Fetching page %s" p)))
  (setf (ghub--graphql-req-query-str req)
        (gsexp-encode
         (ghub--graphql-prepare-query
          (ghub--graphql-req-query req)
          lineage cursor)))
  (ghub--retrieve
   (let ((json-false nil))
     (ghub--encode-payload
      `((query     . ,(ghub--graphql-req-query-str req))
        (variables . ,(ghub--graphql-req-variables req)))))
   req))

(defun ghub--graphql-prepare-query (query &optional lineage cursor)
  (when lineage
    (setq query (ghub--graphql-narrow-query query lineage cursor)))
  (let ((loc (ghub--alist-zip query))
        variables)
    (cl-block nil
      (while t
        (let ((node (treepy-node loc)))
          (when (and (vectorp node)
                     (listp (aref node 0)))
            (let ((alist (cl-coerce node 'list))
                  vars)
              (when-let ((edges (cadr (assq :edges alist))))
                (push (list 'first (if (numberp edges)
                                       edges
                                     ghub-graphql-items-per-request))
                      vars)
                (setq loc  (treepy-up loc))
                (setq node (treepy-node loc))
                (setq loc  (treepy-replace
                            loc `(,(car  node)
                                  ,(cadr node)
                                  (pageInfo endCursor hasNextPage)
                                  (edges (node ,@(cddr node))))))
                (setq loc  (treepy-down loc))
                (setq loc  (treepy-next loc)))
              (dolist (elt alist)
                (cond ((keywordp (car elt)))
                      ((length= elt 3)
                       (push (list (nth 0 elt) (nth 1 elt)) vars)
                       (push (list (nth 1 elt) (nth 2 elt)) variables))
                      ((length= elt 2)
                       (push elt vars))))
              (setq loc (treepy-replace loc (vconcat (nreverse vars)))))))
        (if (treepy-end-p loc)
            (let ((node (copy-sequence (treepy-node loc))))
              (when variables
                (push (cl-coerce variables 'vector)
                      (cdr node)))
              (cl-return node))
          (setq loc (treepy-next loc)))))))

(defun ghub--graphql-handle-response (status req)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((headers (ghub--handle-response-headers status req))
                 (payload (ghub--handle-response-payload req))
                 (payload (ghub--handle-response-error status payload req))
                 (err     (plist-get status :error))
                 (errors  (cdr (assq 'errors payload)))
                 (errors  (and errors (cons 'ghub-graphql-error errors))))
            (if (or err errors)
                (if-let ((errorback (ghub--req-errorback req)))
                    (funcall errorback (or err errors) headers status req)
                  (ghub--signal-error (or err errors)))
              (ghub--graphql-walk-response req (assq 'data payload)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--graphql-walk-response (req data)
  (let* ((loc (ghub--req-value req))
         (loc (if (not loc)
                  (ghub--alist-zip data)
                (setq data (ghub--graphql-narrow-data
                            data (ghub--graphql-lineage loc)))
                (setf (alist-get 'edges data)
                      (append (alist-get 'edges (treepy-node loc))
                              (or (alist-get 'edges data)
                                  (error "BUG: Expected new nodes"))))
                (treepy-replace loc data))))
    (cl-block nil
      (while t
        (when (eq (car-safe (treepy-node loc)) 'edges)
          (setq loc (treepy-up loc))
          (pcase-let ((`(,key . ,val) (treepy-node loc)))
            (let-alist val
              (let* ((cursor (and .pageInfo.hasNextPage
                                  .pageInfo.endCursor))
                     (until  (cdr (assq (intern (format "%s-until" key))
                                        (ghub--graphql-req-until req))))
                     (nodes  (mapcar #'cdar .edges))
                     (nodes  (if until
                                 (seq-take-while
                                  (lambda (node)
                                    (or (string> (cdr (assq 'updatedAt node))
                                                 until)
                                        (setq cursor nil)))
                                  nodes)
                               nodes)))
                (cond (cursor
                       (setf (ghub--req-value req) loc)
                       (ghub--graphql-retrieve req
                                               (ghub--graphql-lineage loc)
                                               cursor)
                       (cl-return))
                      ((setq loc (treepy-replace loc (cons key nodes)))))))))
        (cond ((not (treepy-end-p loc))
               (setq loc (treepy-next loc)))
              ((ghub--req-callback req)
               (funcall (ghub--req-callback req)
                        (treepy-root loc))
               (cl-return))
              ((cl-return (treepy-root loc))))))))

(defun ghub--graphql-lineage (loc)
  (let (lineage)
    (while (treepy-up loc)
      (push (car (treepy-node loc)) lineage)
      (setq loc (treepy-up loc)))
    lineage))

(defun ghub--graphql-narrow-data (data lineage)
  (let (key)
    (while (setq key (pop lineage))
      (if (consp (car lineage))
          (progn (pop lineage)
                 (setf data (cadr data)))
        (setq data (assq key (cdr data))))))
  data)

(defun ghub--graphql-narrow-query (query lineage cursor)
  (if (consp (car lineage))
      (let* ((child  (cddr query))
             (alist  (cl-coerce (cadr query) 'list))
             (single (cdr (assq :singular alist))))
        `(,(car single)
          ,(vector (list (cadr single) (cdr (car lineage))))
          ,@(if (cdr lineage)
                (ghub--graphql-narrow-query child (cdr lineage) cursor)
              child)))
    (let* ((child  (or (assq (car lineage) (cdr query))
                       ;; Alias
                       (cl-find-if (lambda (c)
                                     (eq (car-safe (car-safe c))
                                         (car lineage)))
                                   query)
                       ;; Edges
                       (cl-find-if (lambda (c)
                                     (and (listp c)
                                          (vectorp (cadr c))
                                          (eq (cadr (assq :singular
                                                          (cl-coerce (cadr c)
                                                                     'list)))
                                              (car lineage))))
                                   (cdr query))
                       (error "BUG: Failed to narrow query")))
           (object (car query))
           (args   (and (vectorp (cadr query))
                        (cadr query))))
      `(,object
        ,@(and args (list args))
        ,(cond ((cdr lineage)
                (ghub--graphql-narrow-query child (cdr lineage) cursor))
               (cursor
                `(,(car child)
                  ,(vconcat `((after ,cursor))
                            (cadr child))
                  ,@(cddr child)))
               (t
                child))))))

(defun ghub--alist-zip (root)
  (let ((branchp (lambda (elt) (and (listp elt) (listp (cdr elt)))))
        (make-node (lambda (_ children) children)))
    (treepy-zipper branchp #'identity make-node root)))

(defun ghub--graphql-set-mode-line (buf string &rest args)
  (when (ghub--graphql-req-p buf)
    (setq buf (ghub--graphql-req-buffer buf)))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq mode-line-process
            (and string (concat " " (apply #'format string args))))
      (force-mode-line-update t))))

(defun ghub--graphql-pp-response (data)
  (require 'pp) ; needed for Emacs < 29.
  (pp-display-expression data "*Pp Eval Output*"))

;;; _
(provide 'ghub-graphql)
;;; ghub-graphql.el ends here
