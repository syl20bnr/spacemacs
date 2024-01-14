;;; forge-db.el --- Database implementation  -*- lexical-binding:t -*-

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

(require 'closql)
(require 'compat)
(require 'eieio)
(require 'emacsql)

;; For `closql--db-update-schema':
(declare-function forge--object-id "forge-core")
(declare-function forge-get-issue "forge-core")
(declare-function forge-get-pullreq "forge-core")
(declare-function forge-get-repository "forge-core" (demand))

(eval-when-compile
  (cl-pushnew 'number eieio--known-slot-names)
  (cl-pushnew 'value eieio--known-slot-names))

;;; Options

(defcustom forge-database-file
  (expand-file-name "forge-database.sqlite" user-emacs-directory)
  "The file used to store the forge database."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'file)

;;; Core

(defclass forge-database (closql-database)
  ((name         :initform "Forge")
   (object-class :initform 'forge-repository)
   (file         :initform 'forge-database-file)
   (schemata     :initform 'forge--db-table-schemata)
   (version      :initform 12)))

(defvar forge--override-connection-class nil)

(defun forge-db (&optional livep)
  (closql-db 'forge-database livep forge--override-connection-class))

(defun forge-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (forge-db) (apply #'format sql args))
    (apply #'emacsql (forge-db) sql args)))

(defun forge-connect-database-once ()
  "Try to connect Forge database on first use of `magit-status' only."
  (remove-hook 'magit-status-mode-hook #'forge-connect-database-once)
  (forge-db))
(add-hook 'magit-status-mode-hook #'forge-connect-database-once)

(defun forge-enable-sql-logging ()
  "Enable logging Forge's SQL queries."
  (interactive)
  (let ((conn (oref (forge-db) connection)))
    (emacsql-enable-debugging conn)
    (switch-to-buffer-other-window (oref conn log-buffer))))

;;; Schemata

(defconst forge--db-table-schemata
  '((repository
     [(class :not-null)
      (id :not-null :primary-key)
      forge-id
      forge
      owner
      name
      apihost
      githost
      remote
      sparse-p
      created
      updated
      pushed
      parent
      description
      homepage
      default-branch
      archived-p
      fork-p
      locked-p
      mirror-p
      private-p
      issues-p
      wiki-p
      stars
      watchers
      (assignees :default eieio-unbound)
      (forks     :default eieio-unbound)
      (issues    :default eieio-unbound)
      (labels    :default eieio-unbound)
      (revnotes  :default eieio-unbound)
      (pullreqs  :default eieio-unbound)
      selective-p
      worktree
      (milestones :default eieio-unbound)
      issues-until
      pullreqs-until
      ])

    (assignee
     [(repository :not-null)
      (id :not-null :primary-key)
      login
      name
      forge-id] ; Needed for Gitlab.
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (fork
     [(parent :not-null)
      (id :not-null :primary-key)
      owner
      name]
     (:foreign-key
      [parent] :references repository [id]
      :on-delete :cascade))

    (issue
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      number
      state
      author
      title
      created
      updated
      closed
      status
      locked-p
      milestone
      body
      (assignees    :default eieio-unbound)
      (cards        :default eieio-unbound)
      (edits        :default eieio-unbound)
      (labels       :default eieio-unbound)
      (participants :default eieio-unbound)
      (posts        :default eieio-unbound)
      (reactions    :default eieio-unbound)
      (timeline     :default eieio-unbound)
      (marks        :default eieio-unbound)
      note
      their-id
      slug
      saved-p]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (issue-assignee
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade))

    (issue-label
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references label [id]
      :on-delete :cascade))

    (issue-mark
     [(issue :not-null)
      (id :not-null)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references mark [id]
      :on-delete :cascade))

    (issue-post
     [(class :not-null)
      (id :not-null :primary-key)
      issue
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade))

    (label
     [(repository :not-null)
      (id :not-null :primary-key)
      name
      color
      description]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (mark
     [;; For now this is always nil because it seems more useful to
      ;; share marks between repositories.  We cannot omit this slot
      ;; though because `closql--iref' expects `id' to be the second
      ;; slot.
      repository
      (id :not-null :primary-key)
      name
      face
      description])

    (milestone
     [(repository :not-null)
      (id :not-null :primary-key)
      number
      title
      created
      updated
      due
      closed
      description]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (notification
     [(class :not-null)
      (id :not-null :primary-key)
      thread-id
      repository
      type
      topic
      url
      title
      reason
      last-read
      updated]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      number
      state
      author
      title
      created
      updated
      closed
      merged
      status
      locked-p
      editable-p
      cross-repo-p
      base-ref
      base-repo
      head-ref
      head-user
      head-repo
      milestone
      body
      (assignees       :default eieio-unbound)
      (cards           :default eieio-unbound)
      (commits         :default eieio-unbound)
      (edits           :default eieio-unbound)
      (labels          :default eieio-unbound)
      (participants    :default eieio-unbound)
      (posts           :default eieio-unbound)
      (reactions       :default eieio-unbound)
      (review-requests :default eieio-unbound)
      (reviews         :default eieio-unbound)
      (timeline        :default eieio-unbound)
      (marks           :default eieio-unbound)
      note
      base-rev
      head-rev
      draft-p
      their-id
      slug
      saved-p]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))

    (pullreq-assignee
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (pullreq-label
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references label [id]
      :on-delete :cascade))

    (pullreq-mark
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade)
     (:foreign-key
      [id] :references mark [id]
      :on-delete :cascade))

    (pullreq-post
     [(class :not-null)
      (id :not-null :primary-key)
      pullreq
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (pullreq-review-request
     [(pullreq :not-null)
      (id :not-null)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (revnote
     [(class :not-null)
      (id :not-null :primary-key)
      repository
      commit
      file
      line
      author
      body]
     (:foreign-key
      [repository] :references repository [id]
      :on-delete :cascade))))

(cl-defmethod closql--db-update-schema ((db forge-database))
  (let ((code-version (oref-default 'forge-database version))
        (version (closql--db-get-version db)))
    (when (< version code-version)
      (forge--backup-database db))
    (closql-with-transaction db
      (when (= version 2)
        (message "Upgrading Forge database from version 2 to 3...")
        (emacsql db [:create-table pullreq-review-request $S1]
                 (cdr (assq 'pullreq-review-request forge--db-table-schemata)))
        (closql--db-set-version db (setq version 3))
        (message "Upgrading Forge database from version 2 to 3...done"))
      (when (= version 3)
        (message "Upgrading Forge database from version 3 to 4...")
        (emacsql db [:drop-table notification])
        (pcase-dolist (`(,table . ,schema) forge--db-table-schemata)
          (when (memq table '(notification
                              mark issue-mark pullreq-mark))
            (emacsql db [:create-table $i1 $S2] table schema)))
        (emacsql db [:alter-table issue   :add-column marks :default $s1] 'eieio-unbound)
        (emacsql db [:alter-table pullreq :add-column marks :default $s1] 'eieio-unbound)
        (closql--db-set-version db (setq version 4))
        (message "Upgrading Forge database from version 3 to 4...done"))
      (when (= version 4)
        (message "Upgrading Forge database from version 4 to 5...")
        (emacsql db [:alter-table repository :add-column selective-p :default nil])
        (closql--db-set-version db (setq version 5))
        (message "Upgrading Forge database from version 4 to 5...done"))
      (when (= version 5)
        (message "Upgrading Forge database from version 5 to 6...")
        (emacsql db [:alter-table repository :add-column worktree :default nil])
        (closql--db-set-version db (setq version 6))
        (message "Upgrading Forge database from version 5 to 6...done"))
      (when (= version 6)
        (message "Upgrading Forge database from version 6 to 7...")
        (emacsql db [:alter-table issue   :add-column note :default nil])
        (emacsql db [:alter-table pullreq :add-column note :default nil])
        (emacsql db [:create-table milestone $S1]
                 (cdr (assq 'milestone forge--db-table-schemata)))
        (emacsql db [:alter-table repository :add-column milestones :default $s1]
                 'eieio-unbound)
        (pcase-dolist (`(,repo-id ,issue-id ,milestone)
                       (emacsql db [:select [repository id milestone]
                                    :from issue
                                    :where (notnull milestone)]))
          (unless (stringp milestone)
            (oset (forge-get-issue issue-id) milestone
                  (forge--object-id repo-id (cdar milestone)))))
        (pcase-dolist (`(,repo-id ,pullreq-id ,milestone)
                       (emacsql db [:select [repository id milestone]
                                    :from pullreq
                                    :where (notnull milestone)]))
          (unless (stringp milestone)
            (oset (forge-get-pullreq pullreq-id) milestone
                  (forge--object-id repo-id (cdar milestone)))))
        (closql--db-set-version db (setq version 7))
        (message "Upgrading Forge database from version 6 to 7...done"))
      (when (= version 7)
        (message "Upgrading Forge database from version 7 to 8...")
        (emacsql db [:alter-table pullreq :add-column base-rev :default nil])
        (emacsql db [:alter-table pullreq :add-column head-rev :default nil])
        (emacsql db [:alter-table pullreq :add-column draft-p  :default nil])
        (closql--db-set-version db (setq version 8))
        (message "Upgrading Forge database from version 7 to 8...done"))
      (when (= version 8)
        (message "Upgrading Forge database from version 8 to 9...")
        (emacsql db [:alter-table pullreq :add-column their-id :default nil])
        (emacsql db [:alter-table issue   :add-column their-id :default nil])
        (closql--db-set-version db (setq version 9))
        (message "Upgrading Forge database from version 8 to 9...done"))
      (when (= version 9)
        (message "Upgrading Forge database from version 9 to 10...")
        (emacsql db [:alter-table pullreq :add-column slug :default nil])
        (emacsql db [:alter-table issue   :add-column slug :default nil])
        (dolist (o (closql-entries (forge-db) nil 'forge-pullreq))
          (oset o slug
                (format "%s%s"
                        (if (and (fboundp
                                  'forge-gitlab-repository--eieio-childp)
                                 (forge-gitlab-repository--eieio-childp
                                  (forge-get-repository o)))
                            "!"
                          "#")
                        (oref o number))))
        (dolist (o (closql-entries (forge-db) nil 'forge-issue))
          (oset o slug (format "#%s" (oref o number))))
        (closql--db-set-version db (setq version 10))
        (message "Upgrading Forge database from version 9 to 10...done"))
      (when (= version 10)
        (message "Upgrading Forge database from version 10 to 11...")
        (emacsql db [:drop-table notification])
        (emacsql db [:create-table notification $S1]
                 (cdr (assq 'notification forge--db-table-schemata)))
        (emacsql db [:alter-table pullreq :rename-column unread-p :to status])
        (emacsql db [:alter-table issue   :rename-column unread-p :to status])
        (emacsql db [:alter-table pullreq :add-column saved-p :default nil])
        (emacsql db [:alter-table issue   :add-column saved-p :default nil])
        (closql--db-set-version db (setq version 11))
        (message "Upgrading Forge database from version 10 to 11...done"))
      (when (= version 11)
        (message "Upgrading Forge database from version 11 to 12...")
        (emacsql db [:drop-table notification])
        (emacsql db [:create-table notification $S1]
                 (cdr (assq 'notification forge--db-table-schemata)))
        (dolist (id (emacsql db [:select id :from issue :where (= state 'closed)]))
          (oset (closql-get db (car id) 'forge-issue) state 'completed))
        (dolist (id (emacsql db [:select id :from issue :where (isnull status)]))
          (oset (closql-get db (car id) 'forge-issue) status 'done))
        (dolist (id (emacsql db [:select id :from pullreq :where (= state 'closed)]))
          (oset (closql-get db (car id) 'forge-pullreq) state 'rejected))
        (dolist (id (emacsql db [:select id :from pullreq :where (isnull status)]))
          (oset (closql-get db (car id) 'forge-pullreq) status 'done))
        (emacsql db [:alter-table repository :add-column issues-until :default nil])
        (emacsql db [:alter-table repository :add-column pullreqs-until :default nil])
        (closql--db-set-version db (setq version 12))
        (message "Upgrading Forge database from version 11 to 12...done"))
      )
    (cl-call-next-method)))

(defun forge--backup-database (db)
  (let ((dst (concat (file-name-sans-extension forge-database-file)
                     (format "-v%s" (caar (emacsql (oref db connection)
                                                   [:pragma user-version])))
                     (format-time-string "-%Y%m%d-%H%M")
                     ".sqlite")))
    (message "Copying Forge database to %s..." dst)
    (copy-file forge-database-file dst)
    (message "Copying Forge database to %s...done" dst)))

;;; _
(provide 'forge-db)
;;; forge-db.el ends here
