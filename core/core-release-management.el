;;; core-spacemacs.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst spacemacs-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst spacemacs-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")
(defconst spacemacs-checkversion-remote "checkversion"
  "Name of the remote repository used to check for new version.")
(defconst spacemacs-checkversion-branch "master"
  "Name of the branch used to check for new version.")

(defvar dotspacemacs-check-for-update)
(defvar spacemacs-version)
;; new version variables
(defvar spacemacs-new-version nil
  "If non-nil a new Spacemacs version is available.")
;; used to be "6 hours" but we now check for new versions only at startup
(defvar spacemacs-version-check-interval nil
  "Time between two version checks.")
(defvar spacemacs-version-check-lighter "[+]"
  "Text displayed in the mode-line when a new version is available.")
(defvar spacemacs-version--check-timer nil
  "The current timer for new version check.")
(defvar spacemacs-version--last-startup-check-file
  (expand-file-name (concat spacemacs-cache-directory "last-version-check"))
  "File where the last startup version check time is stored.")
(defvar spacemacs-version--last-startup-check-time nil
  "Time of last version check.")
(defvar spacemacs-version--startup-check-interval (* 3600 24)
  "Minimum number of seconds between two version checks at startup.")
(defvar spacemacs-revision--last nil
  "Last detected git revision of `spacemacs-start-directory' or nil.
NOTE: This variable will be set asynchronously after Spacemacs startup.")
(defvar spacemacs-revision--file
  (expand-file-name (concat spacemacs-cache-directory "spacemacs-revision"))
  "File where the last revision of `spacemacs-start-directory' is saved.")
(defvar spacemacs-revision--changed-hook nil
  "Hooks to be ran when Spacemacs detects revision change.")

(defun spacemacs/switch-to-version (&optional version)
  "Switch spacemacs to VERSION.

VERSION is a string with the format `x.x.x'.
IMPORTANT: The switch is performed by hard resetting the current branch.
If VERSION is nil then a prompt will ask for a version number.
If the current version is not `master' and not `develop' then
a prompt will ask for confirmation before actually switching to the
specified version.
It is not possible to switch version when you are on `develop' branch,
users on `develop' branch must manually pull last commits instead."
  (interactive)
  (let ((branch (spacemacs//git-get-current-branch))
        (dirty (spacemacs//git-working-directory-dirty)))
    (unless version
      (message "Getting version information...")
      (let ((last-version (spacemacs/get-last-version)))
        (setq version (read-string
                       (format "Version (default %s [latest]): " last-version)
                       nil nil last-version))))
    (cond ((string-equal "develop" branch)
           (message (concat "Cannot switch version because you are on "
                            "develop.\n"
                            "You have to manually `pull --rebase' the latest "
                            "commits.")))
          (dirty
           (message (concat "Your Emacs directory is not clean.\n"
                            "git status:\n%s") dirty))
          ((string-equal version spacemacs-version)
           (message "You are already on the latest version."))
          ((or (string-equal "master" branch)
               (yes-or-no-p
                (format (concat "You are not on master. "
                                "This command will switch branches.\n"
                                "Are you sure that you want to switch "
                                "to version %s ? ")
                        version)))
           (let ((tag (concat "v" version)))
             (if (spacemacs//git-hard-reset-to-tag tag)
                 (progn
                   (setq spacemacs-version version)
                   (message "Successfully switched to version %s" version))
               (message "An error occurred while switching to version %s"
                        version))))
          (t (message "Update aborted.")))))

(defun spacemacs/check-for-new-version (force &optional interval)
  "Periodically check for new for new Spacemacs version.
Update `spacemacs-new-version' variable if any new version has been
found."
  (interactive "P")
  (cond
   ((and (not force)
         (not dotspacemacs-check-for-update))
    (message "Skipping check for new version (reason: dotfile)"))
   ((and (not force)
         (string-equal "develop" (spacemacs//git-get-current-branch)))
    (message "Skipping check for new version (reason: develop branch)"))
   ((and (not force)
         (not (spacemacs//can-check-for-new-version-at-startup)))
    (message (concat "Skipping check for new version "
                     "(reason: last check is too recent)")))
   ((require 'async nil t)
    (message "Start checking for new version...")
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`spacemacs-start-directory\\'")
        (load (concat spacemacs-start-directory
                      "core/core-load-paths"))
        (require 'core-spacemacs)
        (spacemacs/get-last-version))
     (lambda (result)
       (if result
           (if (or (version< result spacemacs-version)
                   (string= result spacemacs-version)
                   (if spacemacs-new-version
                       (string= result spacemacs-new-version)))
               (message "Spacemacs is up to date.")
             (message "New version of Spacemacs available: %s" result)
             (setq spacemacs-new-version result))
         (message "Unable to check for new version."))))
    (when interval
      (setq spacemacs-version--check-timer
            (run-at-time t (timer-duration interval)
                         'spacemacs/check-for-new-version))))
   (t (message "Skipping check for new version (reason: async not loaded)"))))

(defun spacemacs/git-get-current-branch-rev ()
  "Returns the hash of the head commit on the current branch.
Returns nil if an error occurred."
  (let ((proc-buffer "git-get-current-branch-head-hash")
        (default-directory (file-truename spacemacs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "rev-parse" "--short" "HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-min))
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun spacemacs/get-new-version-lighter-face (current new)
  "Return the new version lighter face given the difference between the CURRENT
version and the NEW version."
  (let* ((lcur (version-to-list current))
         (lnew (version-to-list new))
         (scur (spacemacs//compute-version-score lcur))
         (snew (spacemacs//compute-version-score lnew))
         (diff (- snew scur)))
    (cond
     ((< diff 3000) 'spacemacs-mode-line-new-version-lighter-success-face)
     ((< diff 5000) 'spacemacs-mode-line-new-version-lighter-warning-face)
     (t 'spacemacs-mode-line-new-version-lighter-error-face))))

(defun spacemacs/get-last-version ()
  "Return the last tagged version."
  (interactive)
  (spacemacs//get-last-version spacemacs-repository
                               spacemacs-repository-owner
                               spacemacs-checkversion-remote
                               spacemacs-checkversion-branch))

(defun spacemacs//can-check-for-new-version-at-startup ()
  "Return non-nil if the version check at startup can be performed."
  (when (file-exists-p spacemacs-version--last-startup-check-file)
    (load spacemacs-version--last-startup-check-file))
  (let ((result
         (or (null spacemacs-version--last-startup-check-time)
             (> (- (float-time) spacemacs-version--last-startup-check-time)
                spacemacs-version--startup-check-interval))))
    (when result
      (setq spacemacs-version--last-startup-check-time (float-time))
      (spacemacs/dump-vars-to-file '(spacemacs-version--last-startup-check-time)
                                   spacemacs-version--last-startup-check-file))
    result))

(defun spacemacs//get-last-version (repo owner remote branch)
  "Return the last tagged version of BRANCH on REMOTE repository from
OWNER REPO."
  (let ((url (format "https://github.com/%s/%s" owner repo)))
    (spacemacs//git-remove-remote remote)
    (spacemacs//git-add-remote remote url)
    ;; removing this call according to issue #6692 proposal
    ;; (spacemacs//git-fetch-remote remote)
    (spacemacs//git-fetch-tags remote branch))
  (let ((version (spacemacs//git-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

(defun spacemacs//git-has-remote (remote)
  "Return non nil if REMOTE is declared."
  (let ((proc-buffer "git-has-remote")
        (default-directory (file-truename spacemacs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
      (with-current-buffer proc-buffer
        (prog2
            (goto-char (point-min))
            (re-search-forward (format "^%s$" remote) nil 'noerror)
          (kill-buffer proc-buffer))))))

(defun spacemacs//git-add-remote (remote url)
  "Add a REMOTE with URL, return t if no error."
  (let ((proc-buffer "git-add-remote")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer proc-buffer))))

(defun spacemacs//git-remove-remote (remote)
  "Remove a REMOTE, return t if no error."
  (let ((proc-buffer "git-remove-remote")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "remove" remote))
      (kill-buffer proc-buffer))))

(defun spacemacs//git-fetch-remote (remote)
  "Fetch last commits from REMOTE, return t if no error."
  (let ((proc-buffer "git-fetch-remote")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote))
      (kill-buffer proc-buffer))))

(defun spacemacs//git-fetch-tags (remote branch)
  "Fetch the tags for BRANCH in REMOTE repository."
  (let ((proc-buffer "git-fetch-tags")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1
        ;;;; original comment: seems necessary to fetch first
        ;; but we remove this according to issue #6692 proposal
        ;; (eq 0 (process-file "git" nil proc-buffer nil
        ;;                     "fetch" remote branch))
        ;; explicitly fetch the new tags
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun spacemacs//git-hard-reset-to-tag (tag)
  "Hard reset the current branch to specified TAG."
  (let ((proc-buffer "git-hard-reset")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1 (eq 0 (process-file "git" nil proc-buffer nil
                               "reset" "--hard" tag))
      (kill-buffer proc-buffer)
      (spacemacs//revision-update))))

(defun spacemacs//git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let ((proc-buffer "git-latest-tag")
        (default-directory (file-truename spacemacs-start-directory))
        (where (format "%s/%s" remote branch)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "describe" "--tags" "--abbrev=0"
                              "--match=v*" where "FETCH_HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-max))
              (forward-line -1)
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun spacemacs//git-checkout (branch)
  "Checkout the given BRANCH. Return t if there is no error."
  (let ((proc-buffer "git-checkout")
        (default-directory (file-truename spacemacs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "checkout" branch))
      (kill-buffer proc-buffer))))

(defun spacemacs//git-get-current-branch ()
  "Return the current branch. Return nil if an error occurred."
  (let ((proc-buffer "git-get-current-branch")
        (default-directory (file-truename spacemacs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "symbolic-ref" "--short" "-q" "HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-min))
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun spacemacs//git-working-directory-dirty ()
  "Non-nil if the user's emacs directory is not clean.
Returns the output of git status --porcelain."
  (let ((proc-buffer "git-working-directory-dirty")
        (default-directory (file-truename spacemacs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "status" "--porcelain"))
      (with-current-buffer proc-buffer
        (prog1
            (when (and (buffer-string)
                       ;;simplecheckforanytext
                       (string-match-p "[^ \t\n]" (buffer-string)))
              (replace-regexp-in-string "\n\\'" "" (buffer-string)))
          (kill-buffer proc-buffer))))))

(defun spacemacs//deffaces-new-version-lighter (state)
  "Define a new version lighter face for the given STATE."
  (let* ((fname (intern
                 (format "spacemacs-mode-line-new-version-lighter-%s-face"
                         (symbol-name state))))
         (foreground (face-foreground state)))
    (eval `(defface ,fname '((t ()))
             ,(format "Color for new version lighter in mode line (%s)."
                      (symbol-name state))
             :group 'spacemacs))
    (set-face-attribute fname nil
                        :foreground foreground
                        :box (face-attribute 'mode-line :box))))

(defun spacemacs//compute-version-score (version)
  "Returns an integer from the version list.
Example: (1 42 3) = 1 042 003"
  (let ((i -1))
    (cl-reduce '+ (mapcar (lambda (n) (setq i (1+ i)) (* n (expt 10 (* i 3))))
                          (reverse version)))))

(defun spacemacs/set-new-version-lighter-mode-line-faces ()
  "Define or set the new version lighter mode-line faces."
  (mapcar 'spacemacs//deffaces-new-version-lighter
          '(error warning success)))
(spacemacs/set-new-version-lighter-mode-line-faces)
(add-hook 'spacemacs-post-theme-change-hook
          'spacemacs/set-new-version-lighter-mode-line-faces)

(defun spacemacs//revision-update ()
  "Update saved revision value and trigger `spacemacs-revision--changed-hook'."
  (let ((proc-buffer "spacemacs//revision-update:git-get-current-rev"))
    (when (eq 0 (process-file "git" nil proc-buffer nil "rev-parse" "HEAD"))
      (with-current-buffer proc-buffer
        (goto-char 1)
        (setq spacemacs-revision--last (current-word))
        (kill-buffer proc-buffer))))
  (spacemacs/dump-vars-to-file '(spacemacs-revision--last)
                               spacemacs-revision--file)
  (run-hooks 'spacemacs-revision--changed-hook))

(defun spacemacs//revision-check ()
  "Update saved value of the current revision asynchronously.
If old and new revisions are different `spacemacs-revision--changed-hook'
 will be triggered."
  (when (file-exists-p spacemacs-revision--file)
    (load spacemacs-revision--file nil t))
  (require 'async)
  (async-start
   `(lambda ()
      (let ((proc-buffer "spacemacs//revision-check:git-get-current-rev")
            (default-directory (file-truename ,spacemacs-start-directory))
            (new_rev))
        (when (eq 0 (process-file "git" nil proc-buffer nil "rev-parse" "HEAD"))
          (with-current-buffer proc-buffer
            (goto-char 1)
            (setq new_rev (current-word))
            (kill-buffer proc-buffer)))
        (with-temp-file ,spacemacs-revision--file
          (insert (format "(setq spacemacs-revision--last %S)" new_rev))
          (make-directory (file-name-directory ,spacemacs-revision--file) t))
        new_rev))
   (lambda (new_rev)
     (unless (string= spacemacs-revision--last new_rev)
       (setq spacemacs-revision--last new_rev)
       (run-hooks 'spacemacs-revision--changed-hook)))))

(provide 'core-release-management)
