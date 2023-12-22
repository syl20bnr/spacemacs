;;; forge-commands.el --- Commands  -*- lexical-binding:t -*-

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

(defcustom forge-add-pullreq-refspec t
  "Whether the pull-request refspec is added when setting up a repository.

This controls whether running `forge-pull' for the first time in
a repository also adds a refspec that fetches all pull-requests.
In repositories with huge numbers of pull-requests you might want
to not do so, in which case you should set this option to `ask'.

You can also set this to nil and later add the refspec explicitly
for a repository using the command `forge-add-pullreq-refspec'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(choice (const :tag "Always add refspec" t)
                 (const :tag "Ask every time" ask)
                 (const :tag "Never add refspec" nil)))

(defcustom forge-checkout-worktree-read-directory-function
  'forge-checkout-worktree-default-read-directory-function
  "Function used by `forge-checkout-worktree' to read worktree directory.
Takes the pull-request as only argument and must return a directory."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'function)

;;; Dispatch

;;;###autoload (autoload 'forge-dispatch "forge-commands" nil t)
(transient-define-prefix forge-dispatch ()
  "Dispatch a forge command."
  [:if forge--get-full-repository
   ["Create"
    ("c i" "issue"             forge-create-issue)
    ("c p" "pull-request"      forge-create-pullreq)
    ("c u" "pull-request from issue"
     forge-create-pullreq-from-issue
     :if forge--get-github-repository)
    ("c f" "fork or remote"    forge-fork)]]
  [:if forge--get-full-repository
   ["List"
    ("t" "topics...         "  forge-topics-menu       :transient replace)
    ("n" "notifications...  "  forge-notification-menu :transient replace)
    ("r" "repositories...   "  forge-repository-menu   :transient replace)]
   ["Fetch"
    ("f f" "all topics       " forge-pull)
    ("f t" "one topic        " forge-pull-topic)
    ("f n" "notifications    " forge-pull-notifications)]
   ["API Commands"
    :if forge--get-full-repository
    (7 "M" "merge" forge-merge)]]
  [:if forge--get-full-repository
   [:description (lambda () (forge-dispatch--format-description "Visit"))
    ("v t" "topic"         forge-visit-topic)
    ("v i" "issue"         forge-visit-issue)
    ("v p" "pull-request"  forge-visit-pullreq)]
   [:description (lambda () (forge-dispatch--format-description "Browse"))
    ("b t" "topic"         forge-browse-topic)
    ("b i" "issue"         forge-browse-issue)
    ("b p" "pull-request"  forge-browse-pullreq)]
   ["Browse"
    ("b r" "remote"        forge-browse-remote)
    ("b I" "issues"        forge-browse-issues)
    ("b P" "pull-requests" forge-browse-pullreqs)]]
  [["Configure"
    :if forge--get-full-repository
    ("a  " forge-add-repository)
    ("R  " forge-add-pullreq-refspec)
    ("s r" forge-forge.remote)
    ("s l" forge-forge.graphqlItemLimit)
    ("s s" forge-toggle-display-in-status-buffer)
    ("s c" forge-toggle-closed-visibility)]]
  [[:description (lambda ()
                   (if (magit-gitdir)
                       "Forge doesn't know about this Git repository yet"
                     "Not inside a Git repository"))
    :if-not forge--get-full-repository
    ("a" "add repository to database" forge-add-repository)
    ("f" "fetch notifications"        forge-pull-notifications)
    ("l" "list notifications"         forge-list-notifications)]])

(defun forge-dispatch--format-description (action)
  (concat
   (propertize (concat action " ") 'face 'transient-heading)
   (propertize "[" 'face 'transient-inactive-value)
   (cond
    (prefix-arg
     (concat
      (format (propertize "open(%s)" 'face 'transient-inactive-value)
              (propertize (or (ignore-errors
                                (key-description
                                 (car (where-is-internal 'transient-quit-one
                                                         transient-base-map))))
                              "C-g")
                          'face 'transient-key))
      (propertize "|" 'face 'transient-inactive-value)
      (propertize "any" 'face 'transient-heading)))
    ((concat
      (propertize "open" 'face 'transient-heading)
      (propertize "|" 'face 'transient-inactive-value)
      (format (propertize "any(%s)" 'face 'transient-inactive-value)
              (propertize (or (ignore-errors
                                (key-description
                                 (car (where-is-internal 'universal-argument))))
                              "C-p")
                          'face 'transient-key)))))
   (propertize "]" 'face 'transient-inactive-value)))

;;; Pull

;;;###autoload
(defun forge-pull (&optional repo until interactive)
  "Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'.
\n(fn &optional REPO UNTIL)"
  (interactive
   (list nil
         (and current-prefix-arg
              (let ((repo (forge-current-repository)))
                (or (not repo) (oref repo sparse-p)))
              (forge-read-date "Limit pulling to topics updates since: "))
         t))
  (let (create)
    (when (or (not repo) (oref repo sparse-p))
      (setq repo (forge-current-repository))
      (unless repo
        (setq repo (forge-get-repository 'create))
        (setq create t)))
    (when (or create interactive (magit-git-config-p "forge.autoPull" t))
      (forge--zap-repository-cache repo)
      (when (and interactive
                 (oref repo selective-p)
                 (yes-or-no-p
                  (format "Always pull all of %s/%s's topics going forward?"
                          (oref repo owner)
                          (oref repo name))))
        (oset repo selective-p nil))
      (setq forge--mode-line-buffer (current-buffer))
      (when-let* ((remote  (oref repo remote))
                  (refspec (oref repo pullreq-refspec)))
        (when (and create
                   (not (member refspec (magit-get-all "remote" remote "fetch")))
                   (or (eq forge-add-pullreq-refspec t)
                       (and (eq forge-add-pullreq-refspec 'ask)
                            (y-or-n-p (format "Also add %S refspec? " refspec)))))
          (magit-call-git "config" "--add"
                          (format "remote.%s.fetch" remote)
                          refspec)))
      (forge--msg repo t nil "Pulling REPO")
      (when-let ((worktree (oref repo worktree)))
        (let ((default-directory worktree))
          (forge--pull repo until))))))

(defun forge-read-date (prompt)
  (cl-block nil
    (while t
      (let ((str (read-from-minibuffer prompt)))
        (cond ((string-equal str "")
               (cl-return nil))
              ((string-match-p
                "\\`[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}\\'" str)
               (cl-return str))))
      (message "Please enter a date in the format YYYY-MM-DD.")
      (sit-for 1))))

(cl-defmethod forge--pull ((repo forge-noapi-repository) _until) ; NOOP
  (forge--msg repo t t "Pulling from REPO is not supported"))

(cl-defmethod forge--pull ((repo forge-unusedapi-repository) _until)
  (oset repo sparse-p nil)
  (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))

(defun forge--git-fetch (buf dir repo)
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))
    (let ((default-directory dir))
      (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))))

;;;###autoload
(defun forge-pull-notifications ()
  "Fetch notifications for all repositories from the current forge."
  (interactive)
  (if-let ((repo (forge-get-repository 'maybe)))
      (let ((class (eieio-object-class repo)))
        (if (eq class 'forge-github-repository)
            (forge--pull-notifications class (oref repo githost))
          (user-error "Fetching notifications not supported for forge %S"
                      (oref repo forge))))
    (forge--pull-notifications 'forge-github-repository "github.com")))

;;;###autoload
(defun forge-pull-topic (topic)
  "Read a TOPIC and pull data about it from its forge."
  (interactive (list (forge-read-topic "Pull topic" nil t)))
  (let ((repo (forge-get-repository t)))
    (forge--zap-repository-cache repo)
    (forge--pull-topic repo
                       (if (numberp topic)
                           (forge-issue :repository (oref repo id)
                                        :number topic)
                         (forge-get-topic topic)))))

(cl-defmethod forge--pull-topic ((repo forge-repository) _topic)
  (error "Fetching an individual topic not implemented for %s"
         (eieio-object-class repo)))

(defun forge--zap-repository-cache (&optional repo)
  (when-let ((r (if repo
                    (oref repo worktree)
                  (magit-repository-local-repository))))
    (magit-repository-local-delete (list 'forge-ls-recent-topics 'issue) r)
    (magit-repository-local-delete (list 'forge-ls-recent-topics 'pullreq) r)))

;;; Browse

;;;###autoload
(defun forge-browse-issues ()
  "Visit the current repository's issues using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository 'stub)
                             'issues-url-format)))

;;;###autoload
(defun forge-browse-pullreqs ()
  "Visit the current repository's pull-requests using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository 'stub)
                             'pullreqs-url-format)))

;;;###autoload
(defun forge-browse-topic (topic)
  "Read a TOPIC and visit it using a browser.
By default only offer open topics but with a prefix argument
also offer closed topics."
  (interactive (list (forge-read-pullreq "Browse topic" t)))
  (forge--browse-topic topic))

;;;###autoload
(defun forge-browse-issue (issue)
  "Read an ISSUE and visit it using a browser.
By default only offer open issues but with a prefix argument
also offer closed issues."
  (interactive (list (forge-read-issue "Browse issue" t)))
  (forge--browse-topic issue))

;;;###autoload
(defun forge-browse-pullreq (pull-request)
  "Read a PULL-REQUEST and visit it using a browser.
By default only offer open pull-requests but with a prefix
argument also offer closed pull-requests."
  (interactive (list (forge-read-pullreq "Browse pull-request" t)))
  (forge--browse-topic pull-request))

(defun forge--browse-topic (topic)
  (let ((obj (forge-get-topic topic)))
    (browse-url (forge-get-url obj))
    (forge-topic-mark-read topic)))

;;;###autoload
(defun forge-browse-commit (commit)
  "Read a COMMIT and visit it using a browser."
  (interactive
   (list (or (magit-completing-read "Browse commit"
                                    (magit-list-branch-names)
                                    nil nil nil 'magit-revision-history
                                    (magit-branch-or-commit-at-point))
             (user-error "Nothing selected"))))
  (browse-url (forge-get-url :commit commit)))

;;;###autoload
(defun forge-browse-branch (branch)
  "Read a BRANCH and visit it using a browser."
  (interactive (list (magit-read-branch "Browse branch")))
  (browse-url (forge-get-url :branch branch)))

;;;###autoload
(defun forge-browse-remote (remote)
  "Read a REMOTE and visit it using a browser."
  (interactive (list (magit-read-remote "Browse remote" nil t)))
  (browse-url (forge-get-url :remote remote)))

;;;###autoload
(defun forge-browse-repository (repository)
  "Read a REPOSITORY and visit it using a browser."
  (interactive (list (forge-read-repository "Browse repository")))
  (browse-url (forge-get-url repository)))

;;;###autoload
(defun forge-browse-this-topic ()
  "Visit the topic at point using a browser."
  (interactive)
  (forge-browse-topic (forge-topic-at-point t)))

;;;###autoload
(defun forge-browse-this-repository ()
  "Visit the repository at point using a browser."
  (interactive)
  (forge-browse-repository (forge-repository-at-point t)))

;;;###autoload
(defun forge-copy-url-at-point-as-kill ()
  "Copy the url of the thing at point."
  (interactive)
  (if-let ((target (forge--browse-target)))
      (let ((url (if (stringp target) target (forge-get-url target))))
        (kill-new url)
        (message "Copied \"%s\"" url))
    (user-error "Nothing at point with a URL")))

;;;###autoload
(defun forge-browse ()
  "Visit the thing at point using a browser."
  (interactive)
  (if-let ((target (forge--browse-target)))
      (if (stringp target)
          (browse-url target)
        (browse-url (forge-get-url target))
        (when (cl-typep target 'forge-topic)
          (forge-topic-mark-read target)))
    (user-error "Nothing to browse here")))

(defun forge--browse-target ()
  (or (and-let* ((branch (magit--painted-branch-at-point)))
        (forge-get-url :branch branch))
      (and-let* ((commit (magit-commit-at-point)))
        (forge-get-url :commit commit))
      (and-let* ((branch (magit-branch-at-point)))
        (forge-get-url :branch branch))
      (and-let* ((remote (magit-remote-at-point)))
        (forge-get-url :remote remote))
      (forge-post-at-point)
      (forge-current-topic)
      (and magit-buffer-revision
           (forge-get-url :commit magit-buffer-revision))
      (forge-current-repository)))

;;;; Urls

(cl-defgeneric forge-get-url (obj)
  "Return the URL for a forge object.")

(cl-defmethod forge-get-url ((issue forge-issue))
  (forge--format issue 'issue-url-format))

(cl-defmethod forge-get-url ((pullreq forge-pullreq))
  (forge--format pullreq 'pullreq-url-format))

(cl-defmethod forge-get-url ((repo forge-repository))
  (forge--format repo 'remote-url-format))

(cl-defmethod forge-get-url ((_(eql :commit)) commit)
  (let ((repo (forge-get-repository 'stub)))
    (unless (magit-list-containing-branches
             commit "-r" (concat (oref repo remote) "/*"))
      (if-let* ((branch (car (magit-list-containing-branches commit "-r")))
                (remote (cdr (magit-split-branch-name branch))))
          (setq repo (forge-get-repository 'stub remote))
        (message "%s does not appear to be available on any remote.  %s"
                 commit "You might have to push it first.")))
    (forge--format repo 'commit-url-format
                   `((?r . ,(magit-commit-p commit))))))

(cl-defmethod forge-get-url ((_(eql :branch)) branch)
  (let (remote)
    (if (magit-remote-branch-p branch)
        (let ((cons (magit-split-branch-name branch)))
          (setq remote (car cons))
          (setq branch (cdr cons)))
      (unless (setq remote (or (magit-get-push-remote branch)
                               (magit-get-upstream-remote branch)))
        (user-error "Cannot determine remote for %s" branch)))
    (forge--format (forge-get-repository 'stub remote)
                   'branch-url-format
                   `((?r . ,branch)))))

(cl-defmethod forge-get-url ((_(eql :remote)) remote)
  (forge--format (forge-get-repository 'stub remote) 'remote-url-format))

(cl-defmethod forge-get-url ((post forge-post))
  (forge--format post (let ((topic (forge-get-parent post)))
                        (cond ((forge--childp topic 'forge-issue)
                               'issue-post-url-format)
                              ((forge--childp topic 'forge-pullreq)
                               'pullreq-post-url-format)))))

(cl-defmethod forge-get-url ((notify forge-notification))
  (oref notify url))

;;; Visit

;;;###autoload
(defun forge-visit-topic (topic)
  "Read a TOPIC and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics."
  (interactive (list (forge-read-topic "View topic" t)))
  (forge-topic-setup-buffer (forge-get-topic topic)))

;;;###autoload
(defun forge-visit-issue (issue)
  "Read an ISSUE and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics."
  (interactive (list (forge-read-issue "View issue" t)))
  (forge-topic-setup-buffer (forge-get-issue issue)))

;;;###autoload
(defun forge-visit-pullreq (pull-request)
  "Read a PULL-REQUEST and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics."
  (interactive (list (forge-read-pullreq "View pull-request" t)))
  (forge-topic-setup-buffer (forge-get-pullreq pull-request)))

;;;###autoload
(defun forge-visit-this-topic ()
  "Visit the topic at point."
  (interactive)
  (forge-topic-setup-buffer (forge-topic-at-point)))

;;;###autoload
(defun forge-visit-this-repository ()
  "Visit the repository at point."
  (interactive)
  (let* ((repo (forge-repository-at-point))
         (worktree (oref repo worktree)))
    (cond
     ((eq transient-current-command 'forge-repository-menu)
      (if-let ((buffer (forge-topic-get-buffer repo)))
          (switch-to-buffer buffer)
        (forge-list-topics repo))
      (transient-setup 'forge-topics-menu))
     ((and worktree (file-directory-p worktree))
      (magit-status-setup-buffer worktree))
     ((forge-list-topics repo)))))

;;; Create

(defun forge-create-issue ()
  "Create a new issue for the current repository."
  (interactive)
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-issue"
               (forge--format repo "Create new issue on %p"))))
    (when buf
      (with-current-buffer buf
        (setq forge--buffer-post-object repo)
        (setq forge--submit-post-function #'forge--submit-create-issue))
      (forge--display-post-buffer buf))))

(defun forge-create-pullreq (source target)
  "Create a new pull-request for the current repository."
  (interactive (forge-create-pullreq--read-args))
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-pullreq"
               (forge--format repo "Create new pull-request on %p")
               source target)))
    (with-current-buffer buf
      (setq forge--buffer-base-branch target)
      (setq forge--buffer-head-branch source)
      (setq forge--buffer-post-object repo)
      (setq forge--submit-post-function #'forge--submit-create-pullreq)
      (run-hooks 'forge-create-pullreq-hook))
    (forge--display-post-buffer buf)))

(defun forge-create-pullreq-from-issue (issue source target)
  "Convert an existing ISSUE into a pull-request."
  (interactive (cons (forge-read-issue "Convert issue")
                     (forge-create-pullreq--read-args)))
  (setq issue (forge-get-issue issue))
  (forge--create-pullreq-from-issue (forge-get-repository issue)
                                    issue source target))

(defun forge-create-pullreq--read-args ()
  (let* ((source  (magit-completing-read
                   "Source branch"
                   (magit-list-remote-branch-names)
                   nil t nil 'magit-revision-history
                   (or (and-let* ((d (magit-branch-at-point)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t)))
                       (and-let* ((d (magit-get-current-branch)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t))))))
         (repo    (forge-get-repository t))
         (remote  (oref repo remote))
         (targets (delete source (magit-list-remote-branch-names remote)))
         (target  (magit-completing-read
                   "Target branch" targets nil t nil 'magit-revision-history
                   (let* ((d (cdr (magit-split-branch-name source)))
                          (d (and (magit-branch-p d) d))
                          (d (and d (magit-get-upstream-branch d)))
                          (d (and d (if (magit-remote-branch-p d)
                                        d
                                      (magit-get-upstream-branch d))))
                          (d (or d (concat remote "/"
                                           (or (oref repo default-branch)
                                               "master")))))
                     (car (member d targets))))))
    (list source target)))

(defun forge-create-post (&optional quote)
  "Create a new post on an existing topic.
If the region is active, then quote that part of the post.
Otherwise and with a prefix argument quote the post that
point is currently on."
  (interactive (list current-prefix-arg))
  (unless (derived-mode-p 'forge-topic-mode)
    (user-error "This command is only available from topic buffers"))
  (let* ((topic forge-buffer-topic)
         (buf (forge--prepare-post-buffer
               (forge--format topic "%i;new-comment")
               (forge--format topic "New comment on #%i of %p")))
         (quote (cond
                 ((not (magit-section-match 'post)) nil)
                 ((use-region-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))
                 (quote
                  (let ((section (magit-current-section)))
                    (string-trim-right
                     (buffer-substring-no-properties (oref section content)
                                                     (oref section end))))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object topic)
      (setq forge--submit-post-function #'forge--submit-create-post)
      (when quote
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert (replace-regexp-in-string "^" "> " quote) "\n\n")))
    (forge--display-post-buffer buf)))

;;; Edit

(defun forge-edit-post ()
  "Edit the current post."
  (interactive)
  (let* ((post (forge-post-at-point t))
         (buf (cl-typecase post
                (forge-topic
                 (forge--prepare-post-buffer
                  (forge--format post "%i")
                  (forge--format post "Edit #%i of %p")))
                (forge-post
                 (forge--prepare-post-buffer
                  (forge--format post "%i;%I")
                  (forge--format post "Edit comment on #%i of %p"))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object post)
      (setq forge--submit-post-function #'forge--submit-edit-post)
      (erase-buffer)
      (when (cl-typep post 'forge-topic)
        (insert "# " (oref post title) "\n\n"))
      (insert (oref post body)))
    (forge--display-post-buffer buf)))

(defun forge-edit-topic-title (title)
  "Edit the TITLE of the current topic."
  (interactive (list (read-string "Title: "
                                  (oref (forge-current-topic t) title))))
  (forge--topic-set 'title title))

(defun forge-edit-topic-state ()
  "Close or reopen the current topic."
  (interactive)
  (let* ((topic (forge-current-topic t))
         (state (oref topic state)))
    (when (eq state 'merged)
      (user-error "Merged pull-requests cannot be reopened"))
    (if (magit-y-or-n-p (format "%s %s %s"
                                (if (eq state 'open) "Close" "Reopen")
                                (oref topic slug)
                                (oref topic title)))
        (forge--topic-set 'state (if (eq state 'open) 'closed 'open))
      (user-error "Abort"))))

(defun forge-edit-topic-draft ()
  "Toggle whether the current pull-request is a draft."
  (interactive)
  (forge--topic-set 'draft (not (oref (forge-current-pullreq t) draft-p))))

(defun forge-edit-topic-milestone (milestone)
  "Edit what MILESTONE the current topic belongs to."
  (interactive
   (let ((topic (forge-current-topic t)))
     (list (magit-completing-read
            "Milestone"
            (mapcar #'caddr (oref (forge-get-repository topic) milestones))
            nil t (forge--get-topic-milestone topic)))))
  (forge--topic-set 'milestone milestone))

(defun forge-edit-topic-labels (labels)
  "Edit the LABELS of the current topic."
  (interactive
   (let* ((topic (forge-current-topic t))
          (repo  (forge-get-repository topic))
          (crm-separator ","))
     (list (magit-completing-read-multiple
            "Labels: "
            (mapcar #'cadr (oref repo labels))
            nil t
            (mapconcat #'car (closql--iref topic 'labels) ",")))))
  (forge--topic-set 'labels labels))

(defun forge-edit-topic-marks (marks)
  "Edit the MARKS of the current topic."
  (interactive (list (forge-read-marks "Marks: " (forge-current-topic t))))
  (oset (forge-current-topic t) marks marks)
  (forge-refresh-buffer))

(defun forge-edit-topic-assignees (assignees)
  "Edit the ASSIGNEES of the current topic."
  (interactive
   (let* ((topic (forge-current-topic t))
          (repo  (forge-get-repository topic))
          (value (closql--iref topic 'assignees))
          (choices (mapcar #'cadr (oref repo assignees)))
          (crm-separator ","))
     (list (magit-completing-read-multiple
            "Assignees: " choices nil
            (if (forge--childp repo 'forge-gitlab-repository)
                t ; Selecting something else would fail later on.
              'confirm)
            (mapconcat #'car value ",")))))
  (forge--topic-set 'assignees assignees))

(defun forge-edit-topic-review-requests (review-requests)
  "Edit the REVIEW-REQUESTS of the current pull-request."
  (interactive
   (let* ((topic (forge-current-topic t))
          (repo  (forge-get-repository topic))
          (value (closql--iref topic 'review-requests))
          (choices (mapcar #'cadr (oref repo assignees)))
          (crm-separator ","))
     (list (magit-completing-read-multiple
            "Request review from: " choices nil
            'confirm
            (mapconcat #'car value ",")))))
  (forge--topic-set 'review-requests review-requests))

(defun forge-edit-topic-note ()
  "Edit your private note about the current topic."
  (interactive)
  (let* ((topic (forge-current-topic t))
         (buf (forge--prepare-post-buffer
               (forge--format topic "%i;note")
               (forge--format topic "New note on #%i of %p"))))
    (with-current-buffer buf
      (setq forge--buffer-post-object topic)
      (setq forge--submit-post-function #'forge--save-note)
      (erase-buffer)
      (when-let ((note (oref topic note)))
        (save-excursion (insert note ?\n))))
    (forge--display-post-buffer buf)))

;;; Delete

(defun forge-delete-comment ()
  "Delete the comment at point."
  (interactive)
  (let ((comment (forge-comment-at-point t)))
    (when (yes-or-no-p "Do you really want to delete the selected comment? ")
      (forge--delete-comment (forge-get-repository t) comment))))

;;; Branch

;;;###autoload
(defun forge-branch-pullreq (pullreq)
  "Create and configure a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Branch pull request" t)))
  (let ((pullreq (forge-get-pullreq pullreq)))
    (if-let ((branch (forge--pullreq-branch-active pullreq)))
        (progn (message "Branch %S already exists and is configured" branch)
               branch)
      (forge--branch-pullreq (forge-get-repository pullreq) pullreq)
      (forge-refresh-buffer))))

(cl-defmethod forge--branch-pullreq ((pullreq forge-pullreq))
  (forge--branch-pullreq (forge-get-repository pullreq) pullreq))

(cl-defmethod forge--branch-pullreq ((_repo forge-unusedapi-repository) pullreq)
  ;; We don't know enough to do a good job.
  (let* ((number (oref pullreq number))
         (branch (format "pr-%s" number)))
    (when (magit-branch-p branch)
      (user-error "Branch `%s' already exists" branch))
    (magit-git "branch" branch (forge--pullreq-ref pullreq))
    ;; More often than not this is the correct target branch.
    (magit-call-git "branch" branch "--set-upstream-to=master")
    (magit-set (number-to-string number) "branch" branch "pullRequest")
    branch))

(cl-defmethod forge--branch-pullreq ((repo forge-repository) pullreq)
  (let* ((number (oref pullreq number))
         (branch-n (format "pr-%s" number))
         (branch (or (forge--pullreq-branch-internal pullreq) branch-n))
         (pullreq-ref (format "refs/pullreqs/%s" number)))
    (cond ((and-let* ((pr-branch (oref pullreq head-ref)))
             (string-search ":" pr-branch))
           ;; Such a branch name would be invalid.  If we encounter
           ;; it anyway, then that means that the source branch and
           ;; the merge-request ref are missing.  Luckily Gitlab no
           ;; longer does this, but we nevertheless have to deal
           ;; with merge-requests that have been lost in time.
           (error "Cannot check out this merge-request because %s"
                  "on old Gitlab version discarded the source branch"))
          ((not (eq (oref pullreq state) 'open))
           (magit-git "branch" "--force" branch pullreq-ref))
          (t
           (let ((upstream  (oref repo remote))
                 (pr-remote (oref pullreq head-user))
                 (pr-branch (oref pullreq head-ref)))
             (cond ((not (oref pullreq cross-repo-p))
                    (let ((tracking (concat upstream "/" pr-branch)))
                      (unless (magit-branch-p tracking)
                        (magit-call-git "fetch" upstream))
                      (forge--setup-pullreq-branch branch tracking)
                      (magit-branch-maybe-adjust-upstream branch tracking)
                      (magit-set upstream "branch" branch "pushRemote")
                      (magit-set upstream "branch" branch "pullRequestRemote")))
                   ((not pr-branch)
                    ;; The pullreq branch (on Github) has been deleted.
                    (setq pr-remote nil)
                    (setq branch branch-n)
                    (forge--setup-pullreq-branch branch pullreq-ref)
                    (magit-set upstream "branch" branch "pushRemote"))
                   (t
                    ;; For prs within the upstream we are more permissive,
                    ;; but any request to merge a branch with a well known
                    ;; name from fork, is highly suspicious and likely the
                    ;; result of a contributor not bothering to name their
                    ;; feature branch.
                    (when (and (member branch magit-main-branch-names)
                               (magit-branch-p branch))
                      (setq branch branch-n))
                    (forge--setup-pullreq-remote pullreq)
                    (forge--setup-pullreq-branch
                     branch (concat pr-remote "/" pr-branch))
                    (if (and (oref pullreq editable-p)
                             (equal branch pr-branch))
                        (magit-set pr-remote "branch" branch "pushRemote")
                      (magit-set upstream "branch" branch "pushRemote"))))
             (when pr-remote
               (magit-set pr-remote "branch" branch "pullRequestRemote"))
             (magit-set "true" "branch" branch "rebase")
             (magit-git "branch" branch
                        (let ((base-ref (oref pullreq base-ref)))
                          (concat "--set-upstream-to="
                                  (if (or magit-branch-prefer-remote-upstream
                                          (not (magit-branch-p base-ref)))
                                      (concat upstream "/" base-ref)
                                    base-ref)))))))
    (magit-set (number-to-string number) "branch" branch "pullRequest")
    (magit-set (oref pullreq title) "branch" branch "description")
    branch))

(defun forge--setup-pullreq-branch (branch tracking)
  (if (magit-branch-p branch)
      (unless (magit-rev-equal branch tracking)
        (message "Existing branch %s diverged from %s" branch tracking))
    (magit-git "branch" branch tracking)))

(defun forge--setup-pullreq-remote (pullreq)
  (let* ((pr-remote (oref pullreq head-user))
         (pr-branch (oref pullreq head-ref))
         (repo (forge-get-repository pullreq))
         (host (oref repo githost))
         (fork (oref pullreq head-repo)))
    (if (magit-remote-p pr-remote)
        (let ((url (magit-git-string "remote" "get-url" pr-remote))
              (fetch (magit-get-all "remote" pr-remote "fetch")))
          (unless (forge--url-equal url (format "git@%s:%s.git" host fork))
            (user-error "Remote `%s' already exists but does not point to %s"
                        pr-remote url))
          (unless (or (member (format "+refs/heads/*:refs/remotes/%s/*"
                                      pr-remote)
                              fetch)
                      (member (format "+refs/heads/%s:refs/remotes/%s/%s"
                                      pr-branch pr-remote pr-branch)
                              fetch))
            (magit-git "remote" "set-branches" "--add" pr-remote pr-branch)
            (magit-git "fetch" pr-remote)))
      (let ((url (magit-git-string "remote" "get-url" (oref repo remote))))
        (magit-git
         "remote" "add" "-f" "--no-tags"
         "-t" pr-branch pr-remote
         (cond ((or (string-prefix-p "git@" url)
                    (string-prefix-p "ssh://git@" url))
                (format "git@%s:%s.git" host fork))
               ((string-prefix-p "https://" url)
                (format "https://%s/%s.git" host fork))
               ((string-prefix-p "git://" url)
                (format "git://%s/%s.git" host fork))
               ((string-prefix-p "http://" url)
                (format "http://%s/%s.git" host fork))
               ((error "%s has an unexpected format" url))))))))

;;;###autoload
(defun forge-checkout-pullreq (pullreq)
  "Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Checkout pull request" t)))
  (magit--checkout (forge--branch-pullreq (forge-get-pullreq pullreq)))
  (forge-refresh-buffer))

;;;###autoload
(defun forge-checkout-worktree (path pullreq)
  "Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information."
  (interactive
   (let ((id (forge-read-pullreq "Checkout pull request" t)))
     (list (funcall forge-checkout-worktree-read-directory-function
                    (forge-get-pullreq id))
           id)))
  (when (and (file-exists-p path)
             (not (and (file-directory-p path)
                       (length= (directory-files path) 2))))
    (user-error "%s already exists and isn't empty" path))
  (magit-worktree-checkout path
                           (forge--branch-pullreq (forge-get-pullreq pullreq))))

(defun forge-checkout-worktree-default-read-directory-function (pullreq)
  (pcase-let* (((eieio number head-ref) pullreq)
               (path (read-directory-name
                      (format "Checkout #%s in new worktree: " number)
                      (file-name-directory
                       (directory-file-name default-directory))
                      nil nil
                      (let ((branch (forge--pullreq-branch-internal pullreq)))
                        (if (string-match-p "\\`pr-[0-9]+\\'" branch)
                            (number-to-string number)
                          (format "%s-%s" number
                                  (string-replace "/" "-" head-ref)))))))
    (when (equal path "")
      (user-error "The empty string isn't a valid path"))
    path))

;;; Marks

(defun forge-create-mark (name face description)
  "Define a new mark that topics can be marked with."
  (interactive
   (list (read-string "Name: ")
         (magit-read-char-case "Set appearance using " nil
           (?n "a face [n]ame"
               (read-face-name "Face name: "))
           (?s "face [s]exp"
               (read-from-minibuffer
                "Face sexp: "
                "(:background \"\" :foreground \"\" :box t)"
                read-expression-map t)))
         (let ((str (read-string "Description: ")))
           (and (not (equal str "")) str))))
  (forge-sql [:insert-into mark :values $v1]
             (vector nil (forge--uuid) name face description)))

(defun forge-edit-mark (id name face description)
  "Define a new mark that topics can be marked with."
  (interactive
   (pcase-let ((`(,id ,name ,face ,description)
                (forge-read-mark "Edit mark")))
     (list id
           (read-string "Name: " name)
           (magit-read-char-case "Set appearance using " nil
             (?n "a face [n]ame"
                 (read-face-name "Face name: " (and (symbolp face) face)))
             (?s "face [s]exp"
                 (read-from-minibuffer
                  "Face sexp: "
                  (if (listp face)
                      (format "%S" face)
                    "(:background \"\" :foreground \"\" :box t)")
                  read-expression-map t)))
           (let ((str (read-string "Description: " nil nil description)))
             (and (not (equal str "")) str)))))
  (forge-sql [:update mark
              :set (= [name face description] $v1)
              :where (= id $s2)]
             (vector name face description) id))

(defun forge-read-mark (prompt)
  "Read a topic.  Return (ID NAME FACE DESCRIPTION)."
  (let* ((marks (forge-sql [:select [id name face description] :from mark]))
         (name (completing-read prompt (mapcar #'cadr marks) nil t)))
    (--first (equal (cadr it) name) marks)))

(defun forge-read-marks (prompt &optional topic)
  "Read multiple mark names and return the respective ids."
  (let ((marks (forge-sql [:select [name id] :from mark]))
        (crm-separator ","))
    (--map (cadr (assoc it marks))
           (magit-completing-read-multiple
            prompt (mapcar #'car marks) nil t
            (and topic
                 (mapconcat #'car (closql--iref topic 'marks) ","))))))

(defun forge-toggle-mark (mark)
  "Toggle MARK for the current topic."
  (let* ((topic (forge-current-topic t))
         (value (mapcar #'car (closql--iref topic 'marks)))
         (value (if (member mark value)
                    (delete mark value)
                  (cons mark value)))
         (marks (forge-sql [:select [name id] :from mark])))
    (oset topic marks (--map (cadr (assoc it marks)) value))
    (forge-refresh-buffer)))

;;; Remotely

;;;###autoload
(defun forge-fork (fork remote)
  "Fork the current repository to FORK and add it as a REMOTE.
If the fork already exists, then that isn't an error; the remote
is added anyway.  Currently this only supports Github and Gitlab."
  (interactive
   (let ((fork (magit-completing-read "Fork to"
                                      (mapcar #'car forge-owned-accounts))))
     (list fork
           (read-string "Remote name: "
                        (or (plist-get (cdr (assoc fork forge-owned-accounts))
                                       'remote-name)
                            fork)))))
  (let ((repo (forge-get-repository 'stub)))
    (forge--fork-repository repo fork)
    (magit-remote-add remote
                      (magit-clone--format-url (oref repo githost) fork
                                               (oref repo name))
                      (list "--fetch"))))

;;;###autoload
(defun forge-merge (pullreq method)
  "Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request PULLREQ to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged."
  (interactive
   (list (forge-read-pullreq "Merge pull-request" t)
         (if (forge--childp (forge-get-repository t) 'forge-gitlab-repository)
             (magit-read-char-case "Merge method " t
               (?m "[m]erge"  'merge)
               (?s "[s]quash" 'squash))
           (magit-read-char-case "Merge method " t
             (?m "[m]erge"  'merge)
             (?s "[s]quash" 'squash)
             (?r "[r]ebase" 'rebase)))))
  (let ((pullreq (forge-get-pullreq pullreq)))
    (forge--merge-pullreq (forge-get-repository pullreq)
                          pullreq
                          (magit-rev-hash
                           (forge--pullreq-branch-internal pullreq))
                          method))
  (forge-pull))

;;;###autoload
(defun forge-rename-default-branch ()
  "Rename the default branch to NEWNAME.
Change the name on the upstream remote and locally, and update
the upstream remotes of local branches accordingly."
  (interactive)
  (let* ((repo (forge-get-repository 'full))
         (_ (unless (forge-github-repository-p repo)
              (user-error "Updating default branch not supported for forge `%s'"
                          (oref repo forge))))
         (remote (or (and (fboundp 'forge--get-remote)
                          (forge--get-remote))
                     (magit-get-some-remote)
                     (user-error "No remote configured")))
         (symref (format "refs/remotes/%s/HEAD" remote))
         (oldhead (progn
                    (message "Determining old default branch...")
                    (magit-git "fetch" "--prune")
                    (magit-git "remote" "set-head" "--auto" remote)
                    (message "Determining old default branch...done")
                    (magit-git-string "symbolic-ref" "--short" symref)))
         (oldname (if oldhead
                      (cdr (magit-split-branch-name oldhead))
                    (error "Cannot determine old default branch")))
         (default (and (not (equal oldname "main")) "main"))
         (newname (read-string
                   (format "Rename default branch `%s' to%s: "
                           oldname
                           (if default (format " (default: %s)" default) ""))
                   nil nil default)))
    (message "Renaming default branch...")
    (forge--set-default-branch repo newname oldname)
    (forge-refresh-buffer)
    (message "Renaming default branch...done")))

;;; Configuration

(transient-define-infix forge-forge.remote ()
  "Change the local value of the `forge.remote' Git variable."
  :class 'magit--git-variable:choices
  :variable "forge.remote"
  :choices #'magit-list-remotes
  :default (lambda (_) (forge--get-remote t)))

(transient-define-infix forge-forge.graphqlItemLimit ()
  "Change the maximum number of GraphQL entities to pull at once."
  :if #'forge--get-github-repository
  :class 'magit--git-variable
  :variable "forge.graphqlItemLimit"
  :reader #'read-string
  :default (lambda () (number-to-string ghub-graphql-items-per-request)))

(transient-define-suffix forge-toggle-display-in-status-buffer ()
  "Toggle whether to display topics in the current status buffer."
  :inapt-if-not (lambda ()
                  (and (eq major-mode 'magit-status-mode)
                       (forge-get-repository nil)))
  :description (lambda ()
                 (if forge-display-in-status-buffer
                     "hide all topics"
                   "display topics"))
  :transient t
  (interactive)
  (setq forge-display-in-status-buffer (not forge-display-in-status-buffer))
  (forge-refresh-buffer))

(transient-define-suffix forge-toggle-closed-visibility ()
  "Toggle whether to display recently closed topics.
This only affect the current status buffer."
  :inapt-if-not (lambda ()
                  (and forge-display-in-status-buffer
                       (eq major-mode 'magit-status-mode)
                       (forge-get-repository nil)))
  :description (lambda ()
                 (if (or (atom forge-topic-list-limit)
                         (> (cdr forge-topic-list-limit) 0))
                     "hide closed topics"
                   "display recently closed topics"))
  :transient t
  (interactive)
  (magit-repository-local-delete (list 'forge-ls-recent-topics 'issue))
  (magit-repository-local-delete (list 'forge-ls-recent-topics 'pullreq))
  (make-local-variable 'forge-topic-list-limit)
  (if (atom forge-topic-list-limit)
      (setq forge-topic-list-limit (cons forge-topic-list-limit 5))
    (setcdr forge-topic-list-limit (* -1 (cdr forge-topic-list-limit))))
  (forge-refresh-buffer))

;;;###autoload (autoload 'forge-add-pullreq-refspec "forge-commands" nil t)
(transient-define-suffix forge-add-pullreq-refspec ()
  "Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE."
  :if-not 'forge--pullreq-refspec
  :description "add pull-request refspec"
  (interactive)
  (let* ((repo    (forge-get-repository 'stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (if (member refspec fetch)
        (message "Pull-request refspec is already active")
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec)
      (magit-git-fetch remote (magit-fetch-arguments)))))

(defun forge--pullreq-refspec ()
  (let* ((repo    (forge-get-repository 'stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (car (member refspec fetch))))

;;; Add repositories

;;;###autoload (autoload 'forge-add-repository "forge-commands" nil t)
(transient-define-suffix forge-add-repository (url)
  "Add a repository to the database.
Offer to either pull topics (now and in the future) or to only
pull individual topics when the user invokes `forge-pull-topic'."
  :description (lambda ()
                 (format "add %srepository to database"
                         (if (forge-get-repository nil) "another " "")))
  (interactive
   (let ((str (magit-read-string-ns
               "Add repository to database (url or name)"
               (and-let* ((repo (forge-get-repository 'stub))
                          (remote (oref repo remote)))
                 (magit-git-string "remote" "get-url" remote)))))
     (if (string-match-p "\\(://\\|@\\)" str)
         (list str)
       (list (magit-clone--name-to-url str)))))
  (if (forge-get-repository url nil 'full)
      (user-error "%s is already tracked in Forge database" url)
    (let ((repo (forge-get-repository url nil 'create)))
      (oset repo sparse-p nil)
      (magit-read-char-case "Pull " nil
        (?a "[a]ll topics"
            (forge-pull repo))
        (?i "[i]ndividual topics (useful for casual contributors)"
            (oset repo selective-p t)
            (forge--pull repo nil))))))

;;;###autoload
(defun forge-add-user-repositories (host user)
  "Add all of USER's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment."
  (interactive
   (list (forge-read-host "Add repositories from Github host"
                          'forge-github-repository)
         (read-string "User: ")))
  (forge--add-user-repos 'forge-github-repository host user))

;;;###autoload
(defun forge-add-organization-repositories (host organization)
  "Add all of ORGANIZATION's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment."
  (interactive
   (list (forge-read-host "Add repositories from Github host"
                          'forge-github-repository)
         (read-string "Organization: ")))
  (forge--add-organization-repos 'forge-github-repository host organization))

;;; Cleanup

;;;###autoload
(defun forge-remove-repository (repository)
  "Remove a repository from the database."
  (interactive
   (pcase-let* ((repo (forge-read-repository "Remove repository from db"))
                ((eieio githost owner name) repo))
     (if (yes-or-no-p (format "Do you really want to remove \"%s/%s @%s\" %s? "
                              owner name githost "from the database"))
         (list repo)
       (user-error "Abort"))))
  (closql-delete repository)
  (forge-refresh-buffer))

;;;###autoload
(defun forge-remove-topic-locally (topic)
  "Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local database that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database."
  (interactive (list (forge-read-topic "Delete topic LOCALLY only")))
  (setq topic (forge-get-topic topic))
  (closql-delete topic)
  (if (and (derived-mode-p 'forge-topic-mode)
           (eq (oref topic id)
               (oref forge-buffer-topic id)))
      (kill-buffer (current-buffer))
    (forge-refresh-buffer)))

;;;###autoload
(defun forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p forge-database-file)
             (yes-or-no-p "Really trash Forge's database file? "))
    (when-let ((db (forge-db t)))
      (emacsql-close db))
    (delete-file forge-database-file t)
    (forge-refresh-buffer)))

;;; Miscellaneous

(defun forge-enable-sql-logging ()
  "Enable logging Forge's SQL queries."
  (interactive)
  (let ((db (forge-db)))
    (emacsql-enable-debugging db)
    (switch-to-buffer-other-window (emacsql-log-buffer (oref db connection)))))

(magit-define-section-jumper forge-jump-to-pullreqs "Pull requests" pullreqs)
(magit-define-section-jumper forge-jump-to-issues "Issues" issues)

;;; _
(provide 'forge-commands)
;;; forge-commands.el ends here
