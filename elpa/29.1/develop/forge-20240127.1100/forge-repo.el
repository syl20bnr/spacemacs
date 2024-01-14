;;; forge-repo.el --- Repository support  -*- lexical-binding:t -*-

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
(require 'eieio)

;;; Classes

(defclass forge-repository (forge-object)
  ((closql-class-prefix       :initform "forge-")
   (closql-class-suffix       :initform "-repository")
   (closql-table              :initform 'repository)
   (closql-primary-key        :initform 'id)
   (issues-url-format         :initform nil :allocation :class)
   (issue-url-format          :initform nil :allocation :class)
   (issue-post-url-format     :initform nil :allocation :class)
   (pullreqs-url-format       :initform nil :allocation :class)
   (pullreq-url-format        :initform nil :allocation :class)
   (pullreq-post-url-format   :initform nil :allocation :class)
   (commit-url-format         :initform nil :allocation :class)
   (branch-url-format         :initform nil :allocation :class)
   (remote-url-format         :initform nil :allocation :class)
   (create-issue-url-format   :initform nil :allocation :class)
   (create-pullreq-url-format :initform nil :allocation :class)
   (pullreq-refspec           :initform nil :allocation :class)
   (id                        :initform nil :initarg :id)
   (forge-id                  :initform nil :initarg :forge-id)
   (forge                     :initform nil :initarg :forge)
   (owner                     :initform nil :initarg :owner)
   (name                      :initform nil :initarg :name)
   (apihost                   :initform nil :initarg :apihost)
   (githost                   :initform nil :initarg :githost)
   (remote                    :initform nil :initarg :remote)
   (sparse-p                  :initform t)
   (created                   :initform nil)
   (updated                   :initform nil)
   (pushed                    :initform nil)
   (parent                    :initform nil)
   (description               :initform nil)
   (homepage                  :initform nil)
   (default-branch            :initform nil)
   (archived-p                :initform nil)
   (fork-p                    :initform nil)
   (locked-p                  :initform nil)
   (mirror-p                  :initform nil)
   (private-p                 :initform nil)
   (issues-p                  :initform t)
   (wiki-p                    :initform nil)
   (stars                     :initform nil)
   (watchers                  :initform nil)
   (assignees                 :closql-table assignee)
   (forks                     :closql-table fork)
   (issues                    :closql-class forge-issue)
   (labels                    :closql-table label)
   (pullreqs                  :closql-class forge-pullreq)
   (revnotes                  :closql-class forge-revnote)
   (selective-p               :initform nil)
   (worktree                  :initform nil)
   (milestones                :closql-table milestone)
   (issues-until              :initform nil)
   (pullreqs-until            :initform nil))
  :abstract t)

(defclass forge-unusedapi-repository (forge-repository) () :abstract t)

(defclass forge-noapi-repository (forge-repository) () :abstract t)

(cl-defmethod slot-missing ((object forge-repository)
                            slot-name operation &optional _new-value)
  (if (and (eq operation 'oref)
           (eq slot-name 'slug))
      (concat (oref object owner) "/"
              (oref object name))
    (cl-call-next-method)))

;;; Query
;;;; Get

(defvar-local forge-buffer-repository nil)
(put 'forge-buffer-repository 'permanent-local t)

(defvar-local forge-buffer-unassociated-p nil)

(defconst forge--signal-no-entry '(t stub create))

(defun forge--get-remote (&optional warn)
  (let* ((remotes (magit-list-remotes))
         (config (magit-get "forge.remote"))
         (remote (if (cdr remotes)
                     (or (car (member config remotes))
                         (car (member "upstream" remotes))
                         (car (member "origin" remotes)))
                   (car remotes))))
    (when (and warn config remote (not (equal config remote)))
      (message "Ignored forge.remote=%s; no such remote.\nSee %s." config
               "https://magit.vc/manual/forge/Repository-Detection.html"))
    remote))

(cl-defmethod forge-get-repository ((_(eql :id)) id)
  (closql-get (forge-db) id 'forge-repository))

(cl-defmethod forge-get-repository ((demand symbol) &optional remote)
  "Return the current forge repository.

First check if `forge-buffer-repository', or if that is nil, then
the repository for `forge-buffer-topic', satisfies DEMAND.  If so,
then return that repository.

Otherwise return the repository for `default-directory', if that
exists and satisfies DEMAND.  If that fails too, then return nil
or signal an error, depending on DEMAND."
  (or (and-let* ((repo (or forge-buffer-repository
                           (and forge-buffer-topic
                                (forge-get-repository forge-buffer-topic)))))
        (and (not (and (memq demand forge--signal-no-entry)
                       (oref repo sparse-p)))
             repo))
      (magit--with-refresh-cache
          (list default-directory 'forge-get-repository demand)
        (if (not (magit-gitdir))
            (when (memq demand forge--signal-no-entry)
              (error
               "Cannot determine Forge repository outside of Git repository"))
          (unless remote
            (setq remote (forge--get-remote 'warn)))
          (if-let ((url (and remote
                             (magit-git-string "remote" "get-url" remote))))
              (and-let* ((repo (forge-get-repository url remote demand)))
                (progn ; work around debbugs#31840
                  (oset repo worktree (magit-toplevel))
                  repo))
            (when (memq demand forge--signal-no-entry)
              (error
               "Cannot determine forge repository.  %s\nSee %s."
               (cond (remote (format "No url configured for %S." remote))
                     ((and-let* ((config (magit-get "forge.remote")))
                        (format "Value of `forge.remote' is %S but %s"
                                config "that remote does not exist.")))
                     ((magit-list-remotes) "Cannot decide on remote to use.")
                     (t "No remote configured."))
               "https://magit.vc/manual/forge/Repository-Detection.html")))))))

(cl-defmethod forge-get-repository ((url string) &optional remote demand)
  "Return the repository at URL."
  (if-let ((parts (forge--split-url url)))
      (forge-get-repository parts remote demand)
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  %s isn't a forge url" url))))

(cl-defmethod forge-get-repository (((host owner name) list)
                                    &optional remote demand)
  "((GITHOST OWNER NAME) &optional REMOTE DEMAND)

Return the repository identified by GITHOST, OWNER and NAME.
See `forge-alist' for valid Git hosts."
  (if-let ((spec (assoc host forge-alist)))
      (pcase-let ((`(,githost ,apihost ,forge ,class) spec))
        (let* ((row (car (forge-sql [:select * :from repository
                                     :where (and (= forge $s1)
                                                 (= owner $s2)
                                                 (= name  $s3))]
                                    forge owner name)))
               (obj (and row (closql--remake-instance class (forge-db) row))))
          (when obj
            (oset obj apihost apihost)
            (oset obj githost githost)
            (oset obj remote  remote))
          (cond ((and (eq demand t)
                      (or (not obj)
                          (oref obj sparse-p)))
                 (error "Cannot use `%s' in %S yet.\n%s"
                        this-command (magit-toplevel)
                        "Use `M-x forge-add-repository' before trying again."))
                ((and obj
                      (oref obj sparse-p)
                      (eq demand 'full))
                 (setq obj nil)))
          (when (and (memq demand '(create stub maybe))
                     (not obj))
            (pcase-let ((`(,id . ,forge-id)
                         (forge--repository-ids
                          class host owner name
                          (memq demand '(stub maybe)))))
              ;; The repo might have been renamed on the forge.  #188
              (unless (setq obj (forge-get-repository :id id))
                (setq obj (funcall class
                                   :id       id
                                   :forge-id forge-id
                                   :forge    forge
                                   :owner    owner
                                   :name     name
                                   :apihost  apihost
                                   :githost  githost
                                   :remote   remote))
                (when (eq demand 'create)
                  (closql-insert (forge-db) obj)))))
          obj))
    (when (memq demand forge--signal-no-entry)
      (error "Cannot determine forge repository.  No entry for %S in %s"
             host 'forge-alist))))

(cl-defmethod forge-get-repository ((repo forge-repository))
  repo)

(defun forge--get-full-repository ()
  (forge-get-repository 'full))

;;;; Current

(defun forge-current-repository ()
  "Return the repository at point or being visited."
  (or (forge-repository-at-point)
      (forge-get-repository nil)))

(defun forge-repository-at-point (&optional demand)
  "Return the repository at point.
If there is no such repository and DEMAND is non-nil, then signal
an error."
  (or (magit-section-value-if 'forge-repo)
      (and-let* ((topic (forge-topic-at-point nil 'not-thingatpt)))
        (forge-get-repository topic))
      (and (derived-mode-p 'forge-repository-list-mode)
           (and-let* ((id (tabulated-list-get-id)))
             (forge-get-repository :id id)))
      (and demand (user-error "No repository at point"))))

;;; Identity

(cl-defmethod forge--repository-ids ((class (subclass forge-repository))
                                     host owner name &optional stub)
  "Return (OUR-ID . THEIR-ID) of the specified repository.
If optional STUB is non-nil, then the IDs are not guaranteed to
be unique.  Otherwise this method has to make an API request to
retrieve THEIR-ID, the repository's ID on the forge.  In that
case OUR-ID derives from THEIR-ID and is unique across all
forges and hosts."
  (pcase-let* ((`(,_githost ,apihost ,id ,_class)
                (or (assoc host forge-alist)
                    (error "No entry for %S in forge-alist" host)))
               (path (format "%s/%s" owner name))
               (their-id (and (not stub)
                              (ghub-repository-id
                               owner name
                               :host apihost
                               :auth 'forge
                               :forge (forge--ghub-type-symbol class)))))
    (cons (base64-encode-string
           (format "%s:%s" id
                   (cond (stub path)
                         ((eq class 'forge-github-repository)
                          ;; This is base64 encoded, according to
                          ;; https://docs.github.com/en/graphql/reference/scalars#id.
                          ;; Unfortunately that is not always true.
                          ;; E.g., https://github.com/dit7ya/roamex.
                          (condition-case nil
                              (base64-decode-string their-id)
                            (error their-id)))
                         (t their-id)))
           t)
          (or their-id path))))

(cl-defmethod forge--repository-ids ((_class (subclass forge-noapi-repository))
                                     host owner name &optional _stub)
  (let ((their-id (if owner (concat owner "/" name) name)))
    (cons (base64-encode-string
           (format "%s:%s"
                   (nth 3 (or (assoc host forge-alist)
                              (error "No entry for %S in forge-alist" host)))
                   their-id)
           t)
          their-id)))

;;; Read

(defun forge-read-repository (prompt)
  (let ((choice (magit-completing-read
                 prompt
                 (mapcar (pcase-lambda (`(,host ,owner ,name))
                           (format "%s/%s @%s" owner name host))
                         (forge-sql [:select [githost owner name]
                                     :from repository]))
                 nil t nil nil
                 (and-let* ((default (forge-current-repository)))
                   (format "%s/%s @%s"
                           (oref default owner)
                           (oref default name)
                           (oref default githost))))))
    (save-match-data
      (if (string-match "\\`\\(.+\\)/\\([^/]+\\) @\\(.+\\)\\'" choice)
          (forge-get-repository (list (match-string 3 choice)
                                      (match-string 1 choice)
                                      (match-string 2 choice)))
        (error "BUG")))))

(defun forge-read-host (prompt &optional class)
  (magit-completing-read
   prompt
   (if class
       (seq-keep (pcase-lambda (`(,githost ,_apihost ,_id ,c))
                   (and (child-of-class-p c class) githost))
                 forge-alist)
     (mapcar #'car forge-alist))
   nil t))

;;; Miscellaneous

(defun forge--as-githost (host)
  (or (car (car (cl-member host forge-alist :test #'equal :key #'car)))
      (car (car (cl-member host forge-alist :test #'equal :key #'cadr)))
      (car (car (cl-member host forge-alist :test #'equal :key #'caddr)))
      (user-error "Cannot determine githost for %S" host)))

(defun forge--as-apihost (host)
  (or (cadr (car (cl-member host forge-alist :test #'equal :key #'cadr)))
      (cadr (car (cl-member host forge-alist :test #'equal :key #'car)))
      (cadr (car (cl-member host forge-alist :test #'equal :key #'caddr)))
      (user-error "Cannot determine githost for %S" host)))

(cl-defmethod forge--topics-until ((repo forge-repository) until type)
  (if (oref repo sparse-p)
      until
    (let ((slot (intern (format "%ss-until" type))))
      (or (eieio-oref repo slot)
          (eieio-oset repo slot
                      (caar (forge-sql [:select [updated] :from $i1
                                        :where (= repository $s2)
                                        :order-by [(desc updated)]
                                        :limit 1]
                                       type (oref repo id))))))))

(cl-defmethod forge--format ((repo forge-repository) format-or-slot &optional spec)
  (format-spec
   (if (symbolp format-or-slot)
       (eieio-oref repo format-or-slot)
     format-or-slot)
   (pcase-let* (((eieio githost owner name) repo)
                (path (if owner (concat owner "/" name) name)))
     `(,@spec
       (?h . ,githost)
       (?o . ,owner)
       (?n . ,name)
       (?p . ,path)
       (?P . ,(string-replace "/" "%2F" path))))))

(defun forge--set-field-callback ()
  (let ((buf (current-buffer)))
    (lambda (&rest _)
      (with-current-buffer buf
        (forge-pull nil nil nil
                    (lambda ()
                      (with-current-buffer buf
                        (forge-refresh-buffer)
                        (when (and transient--showp
                                   (memq transient-current-command
                                         '(forge-topic-menu
                                           forge-topics-menu
                                           forge-notification-menu)))
                          (transient--refresh-transient)))))))))

(defvar forge--mode-line-buffer nil)

(defun forge--msg (repo echo done format &rest args)
  (let ((msg (apply #'format format args)))
    (when repo
      (setq msg (string-replace
                 "REPO"
                 (concat (oref repo owner) "/" (oref repo name))
                 msg)))
    (when (and echo msg)
      (message "%s%s" msg (if done "...done" "...")))
    (when (buffer-live-p forge--mode-line-buffer)
      (with-current-buffer forge--mode-line-buffer
        (setq mode-line-process
              (if done
                  nil
                (concat " " (propertize msg 'font-lock-face
                                        'magit-mode-line-process)))))
      (force-mode-line-update t))))

(cl-defmethod ghub--host ((repo forge-repository))
  (cl-call-next-method (forge--ghub-type-symbol (eieio-object-class repo))))

(cl-defmethod ghub--username ((repo forge-repository))
  (let ((sym (forge--ghub-type-symbol (eieio-object-class repo))))
    (cl-call-next-method (ghub--host sym) sym)))

(defun forge--ghub-type-symbol (class)
  (pcase-exhaustive class
    ;; This package does not define a `forge-gitlab-http-repository'
    ;; class, but we suggest at #9 that users define such a class if
    ;; they must connect to a Gitlab instance that uses http instead
    ;; of https.
    ((or 'forge-gitlab-repository 'forge-gitlab-http-repository) 'gitlab)
    ('forge-github-repository    'github)
    ('forge-gitea-repository     'gitea)
    ('forge-gogs-repository      'gogs)
    ('forge-bitbucket-repository 'bitbucket)))

;;; _
(provide 'forge-repo)
;;; forge-repo.el ends here
