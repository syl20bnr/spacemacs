;;; magit-gh-pulls.el --- GitHub pull requests extension for Magit

;; Copyright (C) 2011-2015  Yann Hodique, Alexander Yakushev

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: git tools
;; Version: 0.5
;; URL: https://github.com/sigma/magit-gh-pulls
;; Package-Requires: ((emacs "24") (gh "0.4.3") (magit "2.1.0") (pcache "0.2.3") (s "1.6.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a Magit extension for manipulating GitHub pull requests

;; No configuration is needed in the repository if any of your remotes contain a
;; URL to Github's remote repository. If for some reason you don't have any
;; Github remotes in your config, you can specify username and repository
;; explicitly:

;; $ git config magit.gh-pulls-repo <user>/<repo> # your github repository

;; Add these lines to your init.el:

;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; There are currently 4 bindings for pull requests:
;; # g g --- refreshes the list of pull requests
;; # g f --- fetches the commits associated with the pull request at point
;; # g b --- helps you creating a topic branch from a review request
;; # g m --- merges the PR on top of the current branch
;; # g c --- creates a PR from the current branch

;; Then, you can do whatever you want with the commit objects associated with
;; the pull request (merge, cherry-pick, diff, ...)

;;; Code:

(require 'eieio)

(require 'magit)
(require 'gh-pulls)
(require 'pcache)
(require 'gh)
(require 's)

(defvar magit-gh-pulls-maybe-filter-pulls 'identity
  "Filter function which should validate pulls you want to be
  viewed in magit. It receives a list of pull requests and should
  return a list of pull requests.")

(defvar magit-gh-pulls-collapse-commits nil
  "Collapse commits in pull requests listing.")

(defun magit-gh-pulls-get-api ()
  (gh-pulls-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-pulls-get-repo-from-config ()
  (let* ((cfg (magit-get "magit" "gh-pulls-repo")))
    (when cfg
      (let* ((split (split-string cfg "/")))
        (cons (car split) (cadr split))))))

(defun magit-gh-pulls-parse-url (url)
  (let ((creds (s-match "github.com[:/]\\([^/]+\\)/\\([^/]+\\)/?$" url)))
    (when creds
      (cons (cadr creds) (s-chop-suffix ".git" (caddr creds))))))

(defun magit-gh-pulls-guess-repo-from-origin ()
  (let ((creds nil))
    (dolist (remote (magit-git-lines "remote") creds)
      (let ((parsed (magit-gh-pulls-parse-url
                     (magit-get "remote" remote "url"))))
        (when parsed
          (setq creds parsed))))))

(defun magit-gh-pulls-guess-repo ()
  (or (magit-gh-pulls-get-repo-from-config)
      (magit-gh-pulls-guess-repo-from-origin)))

(defun magit-gh-pulls-requests-cached-p (api user proj)
  (let ((cache-repo (format "/repos/%s/%s/pulls" user proj))
        (cached? nil))
    (pcache-map (oref api :cache)
                (lambda (key _) (when (equal (car key) cache-repo)
                             (setq cached? t))))
    cached?))

(defun magit-gh-pulls-insert-gh-pulls ()
  (condition-case print-section
      (progn
        (let* ((repo (magit-gh-pulls-guess-repo)))
          (when repo
            (let* ((api (magit-gh-pulls-get-api))
                   (user (car repo))
                   (proj (cdr repo))
                   (cached? (magit-gh-pulls-requests-cached-p api user proj))
                   (stubs (when cached?
                            (funcall magit-gh-pulls-maybe-filter-pulls
                                     (oref (gh-pulls-list api user proj) :data))))
                   (branch (magit-get-current-branch)))
              (when (or (> (length stubs) 0) (not cached?))
                (magit-insert-section (pulls)
                  (magit-insert-heading "Pull Requests:")
                  (dolist (stub stubs)
                    (let* ((id (oref stub :number))
                           (req (oref (gh-pulls-get api user proj id) :data))
                           (base-sha (oref (oref req :base) :sha))
                           (base-ref (oref (oref req :base) :ref))
                           (head-sha (oref (oref req :head) :sha))
                           ;; branch has been deleted in the meantime...
                           (invalid (equal (oref (oref req :head) :ref) head-sha))
                           (have-commits
                            (and (eql 0 (magit-git-exit-code "cat-file" "-e" base-sha))
                                 (eql 0 (magit-git-exit-code "cat-file" "-e" head-sha))))
                           (applied (and have-commits
                                         (not (magit-git-string
                                               "rev-list"
                                               "--cherry-pick" "--right-only"
                                               (format "HEAD...%s" head-sha)
                                               "--not"
                                               (format "%s" base-sha)))))
                           (heading
                            (format "[%s@%s] %s\n"
                                    (propertize (number-to-string id)
                                                'face 'magit-tag)
                                    (if (string= base-ref branch)
                                        (propertize base-ref
                                                    'face 'magit-branch-local)
                                      base-ref)
                                    (propertize
                                     (oref req :title) 'face
                                     (cond (applied 'magit-cherry-equivalent)
                                           (have-commits nil)
                                           (invalid 'error)
                                           (t 'italic)))))
                           (info (list user proj id)))
                      (cond
                       (have-commits
                        (magit-insert-section
                          (pull info (not magit-gh-pulls-collapse-commits))
                          (magit-insert heading)
                          (magit-insert-heading)
                          (when (and have-commits (not applied))
                            (magit-git-wash
                                (apply-partially 'magit-log-wash-log 'cherry)
                              "cherry" "-v" (magit-abbrev-arg)
                              base-sha head-sha))))
                       (invalid
                        (magit-insert-section (invalid-pull info)
                          (magit-insert heading)))
                       (t
                        (magit-insert-section (unfetched-pull info)
                          (magit-insert heading))))))
                  (when (not cached?)
                    (insert "Press `# g g` to update the pull request list.\n\n"))
                  (when (> (length stubs) 0)
                    (insert "\n"))))))))
    (error nil)))

(defun magit-gh-pulls-guess-topic-name (req)
  (let ((user (oref (oref req :user) :login))
        (topic (oref (oref req :head) :ref)))
    (format "%s/%s" user topic)))

(defun magit-gh-section-req-data (&optional section)
  (oref (apply #'gh-pulls-get
               (magit-gh-pulls-get-api)
               (magit-section-value (or section (magit-current-section))))
        :data))

(defun magit-gh-pulls-create-branch ()
  (interactive)
  (magit-section-case
    (pull
     (let* ((req (magit-gh-section-req-data))
            (branch (read-from-minibuffer
                     "Branch name: " (magit-gh-pulls-guess-topic-name req)))
            (base (magit-read-branch-or-commit
                   "Branch base: "
                   (oref (oref req :base) :ref)))
            (inhibit-magit-refresh t))
       (magit-branch branch base)
       (magit-merge (oref (oref req :head) :sha)))
     (magit-refresh))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-merge-pull-request ()
  (interactive)
  (magit-section-case
    (pull
     (let* ((req (magit-gh-section-req-data))
            (branch (magit-gh-pulls-guess-topic-name req))
            (base (oref (oref req :base) :ref))
            (inhibit-magit-refresh t))
       (magit-branch branch base)
       (magit-merge (oref (oref req :head) :sha))
       (magit-checkout base)
       (magit-merge branch)
       (magit-call-git "branch" "-D" branch))
     (magit-refresh))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-fetch-commits ()
  (interactive)
  (magit-section-case
    (unfetched-pull
     (let* ((req (magit-gh-section-req-data))
            (head (oref req :head)))
       (magit-run-git "fetch" (oref (oref head :repo) :git-url)
                      (oref head :ref))))
    (pull nil)
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-url-for-pull (info)
  "Return github url for a pull request using INFO."
  (let ((url "https://github.com/%s/%s/pull/%s"))
    (apply 'format url info)))

(defun magit-gh-pulls-open-in-browser ()
  (interactive)
  (let ((info (magit-section-value (magit-current-section))))
    (magit-section-case
      (pull           (browse-url (magit-gh-pulls-url-for-pull info)))
      (unfetched-pull (browse-url (magit-gh-pulls-url-for-pull info))))))

(defun magit-gh-pulls-purge-cache ()
  (let* ((api (magit-gh-pulls-get-api))
         (cache (oref api :cache))
         (repo (magit-gh-pulls-guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))


(defun magit-gh-pulls-build-req (user proj)
  (let ((current (or (cdr (magit-get-remote-branch))
                     (magit-get-current-branch))))
    (let* ((base
            (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name user)
                           :repo (make-instance 'gh-repos-repo :name proj)
                           :ref (completing-read "Base (master):" '() nil nil nil nil "master")))
           (head
            (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name user)
                           :repo (make-instance 'gh-repos-repo :name proj)
                           :ref (completing-read (format "Head (%s):" current) '() nil nil nil nil current)))
           (title (read-string "Title: "))
           (body (read-string "Description: "))
           (req (make-instance 'gh-pulls-request :head head :base base :body body :title title)))
      req)))

(defun magit-gh-pulls-create-pull-request ()
  (interactive)
  (let ((repo (magit-gh-pulls-guess-repo)))
    (when repo
      (let* ((current-branch (magit-get-current-branch))
            (api (magit-gh-pulls-get-api))
            (user (car repo))
            (proj (cdr repo))
            (req (magit-gh-pulls-build-req user proj))
            (a (gh-pulls-new api user proj req)))
        (kill-new (oref (oref a :data) :html-url))))))

(defun magit-gh-pulls-reload ()
  (interactive)
  (let ((creds (magit-gh-pulls-guess-repo)))
    (if (not (and creds (car creds) (cdr creds)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-pulls-purge-cache)
      (gh-pulls-list (magit-gh-pulls-get-api) (car creds) (cdr creds))
      (magit-refresh))))

(easy-menu-define magit-gh-pulls-extension-menu
  nil
  "GitHub Pull Requests extension menu"
  '("GitHub Pull Requests"
    :visible magit-gh-pulls-mode
    ["Reload pull request" magit-gh-pulls-reload]
    ["Create pull request branch" magit-gh-pulls-create-branch]
    ["Fetch pull request commits" magit-gh-pulls-fetch-commits]
    ["Open pull request in browser" magit-gh-pulls-open-in-browser]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-gh-pulls-extension-menu)

(defvar magit-gh-pulls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "# g b") 'magit-gh-pulls-create-branch)
    (define-key map (kbd "# g f") 'magit-gh-pulls-fetch-commits)
    (define-key map (kbd "# g g") 'magit-gh-pulls-reload)
    (define-key map (kbd "# g m") 'magit-gh-pulls-merge-pull-request)
    (define-key map (kbd "# g c") 'magit-gh-pulls-create-pull-request)
    (define-key map (kbd "# g o") 'magit-gh-pulls-open-in-browser)
    map))

(defvar magit-gh-pulls-mode-lighter " Pulls")

;;;###autoload
(define-minor-mode magit-gh-pulls-mode "Pull requests support for Magit"
  :lighter  magit-gh-pulls-mode-lighter
  :require 'magit-gh-pulls
  :keymap  'magit-gh-pulls-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-pulls-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-pulls-insert-gh-pulls
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-pulls-insert-gh-pulls))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-pulls ()
  "Unconditionally turn on `magit-pulls-mode'."
  (magit-gh-pulls-mode 1))

(ert-deftest test-magit-gh-pulls-parse-url-git-at ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git@github.com:sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls/"))))

(ert-deftest test-magit-gh-pulls-parse-url-http ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "http://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-git ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magic-gh-pulls-parse-url-invalid ()
  (should (eq nil (magit-gh-pulls-parse-url "http://google.com"))))

(ert-deftest test-magic-gh-pulls-parse-url-garbage ()
  (should (eq nil (magit-gh-pulls-parse-url "08h3fiuandiu"))))

(provide 'magit-gh-pulls)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-pulls.el ends here
