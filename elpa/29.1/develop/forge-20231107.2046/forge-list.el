;;; forge-list.el --- Tabulated-list interface  -*- lexical-binding:t -*-

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

(defvar x-stretch-cursor)

;;; Options

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defvar forge-topic-list-columns
  '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
    ("Title" 35 t nil title  nil)
    ))

(defvar forge-global-topic-list-columns
  '(("Owner"    15 t   nil repository:owner nil)
    ("Name"     20 t   nil repository:name  nil)
    ("#"         5 forge-topic-list-sort-by-number (:right-align t) number nil)
    ("Title"    35 t   nil title nil)
    ))

(defvar forge-repository-list-columns
  '(("Owner"    20 t   nil owner nil)
    ("Name"     20 t   nil name  nil)
    ("N"         1 t   nil sparse-p nil)
    ("S"         1 t   nil selective-p nil)
    ("Worktree" 99 t   nil worktree nil)
    ))

(defcustom forge-owned-accounts nil
  "An alist of accounts that are owned by you.
This should include your username as well as any organization
that you own.  Used by the commands `forge-list-owned-issues',
`forge-list-owned-pullreqs' and `forge-fork'.

Each element has the form (ACCOUNT . PLIST).  The following
properties are currently being used:

`remote-name' The default name suggested by `forge-fork' for a
  fork created within this account.  If unspecified, then the
  name of the account is used."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (cons (string :tag "Account") plist)))

(defcustom forge-owned-ignored nil
  "A list of repositories that are ignored when listing those owned by you.
This is a list of package names.  Used by the commands
`forge-list-owned-issues' and `forge-list-owned-pullreqs'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (string :tag "Name")))

;;; Variables

(defvar-local forge--tabulated-list-columns nil)
(put 'forge--tabulated-list-columns 'permanent-local t)

(defvar-local forge--tabulated-list-query nil)
(put 'forge--tabulated-list-query 'permanent-local t)

;;; Modes
;;;; Topics

(defvar-keymap forge-topic-list-mode-map
  :doc "Local keymap for Forge-Topic-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-topic
  "<return>" #'forge-visit-this-topic
  "o"        #'forge-browse-this-topic
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  "Topics"
  "Major mode for browsing a list of topics."
  (setq-local x-stretch-cursor  nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "#" nil)))

(define-derived-mode forge-issue-list-mode forge-topic-list-mode
  "Issues"
  "Major mode for browsing a list of issues.")

(define-derived-mode forge-pullreq-list-mode forge-topic-list-mode
  "Pull-Requests"
  "Major mode for browsing a list of pull-requests.")

(defun forge-topic-list-setup (mode id buffer-name columns fn)
  (declare (indent 4))
  (let* ((repo (forge-get-repository :id id))
         (topdir (magit-toplevel)))
    (with-current-buffer
        (get-buffer-create
         (or buffer-name
             (format "*%s: %s/%s*"
                     (substring (symbol-name mode) 0 -5)
                     (oref repo owner)
                     (oref repo name))))
      (setq forge--tabulated-list-columns (or columns forge-topic-list-columns))
      (setq forge--tabulated-list-query fn)
      (setq forge-buffer-repository repo)
      (when topdir
        (setq default-directory topdir))
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (funcall mode))
      (forge-topic-list-refresh)
      (add-hook 'tabulated-list-revert-hook
                #'forge-topic-list-refresh nil t)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun forge-topic-list-refresh ()
  (setq tabulated-list-format
        (vconcat (--map `(,@(-take 3 it)
                          ,@(-flatten (nth 3 it)))
                        forge--tabulated-list-columns)))
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar #'forge--tablist-format-entry
                (funcall forge--tabulated-list-query))))

;;;; Repository

(defvar-keymap forge-repository-list-mode-map
  :doc "Local keymap for Forge-Repository-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-repository
  "<return>" #'forge-visit-this-repository
  "o"        #'forge-browse-this-repository
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(define-derived-mode forge-repository-list-mode tabulated-list-mode
  "Repositories"
  "Major mode for browsing a list of repositories."
  (setq-local x-stretch-cursor  nil)
  (setq forge--tabulated-list-columns forge-repository-list-columns)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Owner" nil))
  (setq tabulated-list-format
        (vconcat (--map `(,@(-take 3 it)
                          ,@(-flatten (nth 3 it)))
                        forge-repository-list-columns)))
  (tabulated-list-init-header))

(defun forge-repository-list-setup (fn buf)
  (with-current-buffer (get-buffer-create buf)
    (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
      (forge-repository-list-mode))
    (funcall fn)
    (add-hook 'tabulated-list-revert-hook fn nil t)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defun forge-repository-list-refresh ()
  (setq tabulated-list-entries
        (mapcar #'forge--tablist-format-entry
                (forge-sql [:select $i1 :from repository
                            :order-by [(asc owner) (asc name)]]
                           (forge--tablist-columns-vector)))))

(defun forge-repository-list-owned-refresh ()
  (setq tabulated-list-entries
        (mapcar #'forge--tablist-format-entry
                (forge-sql [:select $i1 :from repository
                            :where (and (in owner $v2)
                                        (not (in name $v3)))
                            :order-by [(asc owner) (asc name)]]
                           (forge--tablist-columns-vector)
                           (vconcat (mapcar #'car forge-owned-accounts))
                           (vconcat forge-owned-ignored)))))

;;; Commands
;;;; Topic

;;;###autoload
(defun forge-list-topics (id)
  "List topics of the current repository in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-topic-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from issue   :where (= repository $s2) :union
                  :select $i1 :from pullreq :where (= repository $s2)]
                 (forge--tablist-columns-vector)
                 id))))

;;;; Issue

;;;###autoload
(defun forge-list-issues (id)
  "List issues of the current repository in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-issue-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from issue :where (= repository $s2)]
                 (forge--tablist-columns-vector)
                 id))))

;;;###autoload
(defun forge-list-labeled-issues (id label)
  "List issues of the current repository that have LABEL.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)
                     (magit-completing-read
                      "Label"
                      (mapcar #'cadr (oref (forge-get-repository t) labels))
                      nil t)))
  (forge-topic-list-setup #'forge-issue-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [issue issue_label label]
                  :where (and (= issue_label:issue issue:id)
                              (= issue_label:id    label:id)
                              (= issue:repository  $s2)
                              (= label:name        $s3)
                              (isnull issue:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'issue)
                 id label))))

;;;###autoload
(defun forge-list-assigned-issues (id)
  "List issues of the current repository that are assigned to you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-issue-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [issue issue_assignee assignee]
                  :where (and (= issue_assignee:issue issue:id)
                              (= issue_assignee:id    assignee:id)
                              (= issue:repository     $s2)
                              (= assignee:login       $s3)
                              (isnull issue:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'issue)
                 id (ghub--username (forge-get-repository :id id))))))

;;;###autoload
(defun forge-list-owned-issues ()
  "List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge-topic-list-setup #'forge-issue-list-mode nil "My issues"
                          forge-global-topic-list-columns
    (lambda ()
      (forge-sql [:select $i1 :from [issue repository]
                  :where (and (= issue:repository repository:id)
                              (in repository:owner $v2)
                              (not (in repository:name $v3))
                              (isnull issue:closed))
                  :order-by [(asc repository:owner)
                             (asc repository:name)
                             (desc issue:number)]]
                 (forge--tablist-columns-vector 'issue)
                 (vconcat (mapcar #'car forge-owned-accounts))
                 (vconcat forge-owned-ignored)))))

;;;; Pullreq

;;;###autoload
(defun forge-list-pullreqs (id)
  "List pull-requests of the current repository in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from pullreq :where (= repository $s2)]
                 (forge--tablist-columns-vector)
                 id))))

;;;###autoload
(defun forge-list-labeled-pullreqs (id label)
  "List pull-requests of the current repository that have LABEL.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)
                     (magit-completing-read
                      "Label"
                      (mapcar #'cadr (oref (forge-get-repository t) labels))
                      nil t)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [pullreq pullreq_label label]
                  :where (and (= pullreq_label:pullreq pullreq:id)
                              (= pullreq_label:id    label:id)
                              (= pullreq:repository  $s2)
                              (= label:name        $s3)
                              (isnull pullreq:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'pullreq)
                 id label))))

;;;###autoload
(defun forge-list-assigned-pullreqs (id)
  "List pull-requests of the current repository that are assigned to you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [pullreq pullreq_assignee assignee]
                  :where (and (= pullreq_assignee:pullreq pullreq:id)
                              (= pullreq_assignee:id      assignee:id)
                              (= pullreq:repository       $s2)
                              (= assignee:login           $s3)
                              (isnull pullreq:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'pullreq)
                 id (ghub--username (forge-get-repository :id id))))))

;;;###autoload
(defun forge-list-requested-reviews (id)
  "List pull-requests of the current repository that are awaiting your review.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [pullreq pullreq_review_request assignee]
                  :where (and (= pullreq_review_request:pullreq pullreq:id)
                              (= pullreq_review_request:id      assignee:id)
                              (= pullreq:repository       $s2)
                              (= assignee:login           $s3)
                              (isnull pullreq:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'pullreq)
                 id (ghub--username (forge-get-repository :id id))))))

;;;###autoload
(defun forge-list-owned-pullreqs ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge-topic-list-setup #'forge-pullreq-list-mode nil "My pullreqs"
                          forge-global-topic-list-columns
    (lambda ()
      (forge-sql [:select $i1 :from [pullreq repository]
                  :where (and (= pullreq:repository repository:id)
                              (in repository:owner $v2)
                              (not (in repository:name $v3))
                              (isnull pullreq:closed))
                  :order-by [(asc repository:owner)
                             (asc repository:name)
                             (desc pullreq:number)]]
                 (forge--tablist-columns-vector 'pullreq)
                 (vconcat (mapcar #'car forge-owned-accounts))
                 (vconcat forge-owned-ignored)))))

;;;###autoload
(defun forge-list-authored-pullreqs (id)
  "List open pull-requests of the current repository that are authored by you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [pullreq]
                  :where (and (= pullreq:repository       $s2)
                              (= pullreq:author           $s3)
                              (isnull pullreq:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'pullreq)
                 id (ghub--username (forge-get-repository :id id))))))

;;;###autoload
(defun forge-list-authored-issues (id)
  "List open issues from the current repository that are authored by you.
List them in a separate buffer."
  (interactive (list (oref (forge-get-repository t) id)))
  (forge-topic-list-setup #'forge-pullreq-list-mode id nil nil
    (lambda ()
      (forge-sql [:select $i1 :from [issue]
                  :where (and (= issue:repository       $s2)
                              (= issue:author           $s3)
                              (isnull issue:closed))
                  :order-by [(desc updated)]]
                 (forge--tablist-columns-vector 'issue)
                 id (ghub--username (forge-get-repository :id id))))))

;;;; Notifications

;;;###autoload
(defun forge-list-notifications ()
  "List notifications."
  (interactive)
  (forge-notifications-setup-buffer))

;;;; Repository

;;;###autoload
(defun forge-list-repositories ()
  "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database."
  (interactive)
  (forge-repository-list-setup #'forge-repository-list-refresh
                               "*Forge Repositories*"))

;;;###autoload
(defun forge-list-owned-repositories ()
  "List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  (interactive)
  (forge-repository-list-setup #'forge-repository-list-owned-refresh
                               "*Forge Owned Repositories*"))

;;; Miscellaneous

(defun forge-topic-list-sort-by-number (a b)
  "Sort the `tabulated-list-entries' by topic number.
This assumes that `number' is the first column, otherwise
it silently fails."
  (ignore-errors
    (> (read (aref (cadr a) 0))
       (read (aref (cadr b) 0)))))

(defun forge--tablist-columns-vector (&optional table)
  (let ((columns (cons 'id (--map (nth 4 it) forge--tabulated-list-columns))))
    (vconcat (if table
                 (let ((table (symbol-name table)))
                   (--map (let ((col (symbol-name it)))
                            (if (string-search ":" col)
                                it
                              (intern (concat table ":" col))))
                          columns))
               columns))))

(defun forge--tablist-format-entry (row)
  (list (car row)
        (vconcat
         (cl-mapcar (lambda (val col)
                      (if-let ((pp (nth 5 col)))
                          (funcall pp val)
                        (if val (format "%s" val) "")))
                    (cdr row)
                    forge--tabulated-list-columns))))

;;; _
(provide 'forge-list)
;;; forge-list.el ends here
