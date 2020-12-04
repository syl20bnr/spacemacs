;;; core-space-macs.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(defconst space-macs-repository "space-macs"
  "Name of the Space-macs remote repository.")
(defconst space-macs-repository-owner "syl20bnr"
  "Name of the Space-macs remote repository owner.")
(defconst space-macs-checkversion-remote "checkversion"
  "Name of the remote repository used to check for new version.")
(defconst space-macs-checkversion-branch "master"
  "Name of the branch used to check for new version.")

(defvar dotspace-macs-check-for-update)
(defvar space-macs-version)
;; new version variables
(defvar space-macs-new-version nil
  "If non-nil a new Space-macs version is available.")
;; used to be "6 hours" but we now check for new versions only at startup
(defvar space-macs-version-check-interval nil
  "Time between two version checks.")
(defvar space-macs-version-check-lighter "[+]"
  "Text displayed in the mode-line when a new version is available.")
(defvar space-macs-version--check-timer nil
  "The current timer for new version check.")
(defvar space-macs-version--last-startup-check-file
  (expand-file-name (concat space-macs-cache-directory "last-version-check"))
  "File where the last startup version check time is stored.")
(defvar space-macs-version--last-startup-check-time nil
  "Time of last version check.")
(defvar space-macs-version--startup-check-interval (* 3600 24)
  "Minimum number of seconds between two version checks at startup.")

(defun space-macs/switch-to-version (&optional version)
  "Switch space-macs to VERSION.

VERSION is a string with the format `x.x.x'.
IMPORTANT: The switch is performed by hard resetting the current branch.
If VERSION is nil then a prompt will ask for a version number.
If the current version is not `master' and not `develop' then
a prompt will ask for confirmation before actually switching to the
specified version.
It is not possible to switch version when you are on `develop' branch,
users on `develop' branch must manually pull last commits instead."
  (interactive)
  (let ((branch (space-macs//git-get-current-branch))
        (dirty (space-macs//git-working-directory-dirty)))
    (unless version
      (message "Getting version information...")
      (let ((last-version (space-macs/get-last-version)))
        (setq version (read-string
                       (format "Version (default %s [latest]): " last-version)
                       nil nil last-version))))
    (cond ((string-equal "develop" branch)
           (message (concat "Cannot switch version because you are on "
                            "develop.\n"
                            "You have to manually `pull --rebase' the latest "
                            "commits.")))
          (dirty
           (message (concat "Your e-macs directory is not clean.\n"
                            "git status:\n%s") dirty))
          ((string-equal version space-macs-version)
           (message "You are already on the latest version."))
          ((or (string-equal "master" branch)
               (yes-or-no-p
                (format (concat "You are not on master. "
                                "This command will switch branches.\n"
                                "Are you sure that you want to switch "
                                "to version %s ? ")
                        version)))
           (let ((tag (concat "v" version)))
             (if (space-macs//git-hard-reset-to-tag tag)
                 (progn
                   (setq space-macs-version version)
                   (message "Successfully switched to version %s" version))
               (message "An error occurred while switching to version %s"
                        version))))
          (t (message "Update aborted.")))))

(defun space-macs/check-for-new-version (force &optional interval)
  "Periodically check for new for new Space-macs version.
Update `space-macs-new-version' variable if any new version has been
found."
  (interactive "P")
  (cond
   ((and (not force)
         (not dotspace-macs-check-for-update))
    (message "Skipping check for new version (reason: dotfile)"))
   ((and (not force)
         (string-equal "develop" (space-macs//git-get-current-branch)))
    (message "Skipping check for new version (reason: develop branch)"))
   ((and (not force)
         (not (space-macs//can-check-for-new-version-at-startup)))
    (message (concat "Skipping check for new version "
                     "(reason: last check is too recent)")))
   ((require 'async nil t)
    (message "Start checking for new version...")
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`space-macs-start-directory\\'")
        (load-file (concat space-macs-start-directory
                           "core/core-load-paths.el"))
        (require 'core-space-macs)
        (space-macs/get-last-version))
     (lambda (result)
       (if result
           (if (or (version< result space-macs-version)
                   (string= result space-macs-version)
                   (if space-macs-new-version
                       (string= result space-macs-new-version)))
               (message "Space-macs is up to date.")
             (message "New version of Space-macs available: %s" result)
             (setq space-macs-new-version result))
         (message "Unable to check for new version."))))
    (when interval
      (setq space-macs-version--check-timer
            (run-at-time t (timer-duration interval)
                         'space-macs/check-for-new-version))))
   (t (message "Skipping check for new version (reason: async not loaded)"))))

(defun space-macs/git-get-current-branch-rev ()
  "Returns the hash of the head commit on the current branch.
Returns nil if an error occurred."
  (let ((proc-buffer "git-get-current-branch-head-hash")
        (default-directory (file-truename space-macs-start-directory)))
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

(defun space-macs/get-new-version-lighter-face (current new)
  "Return the new version lighter face given the difference between the CURRENT
version and the NEW version."
  (let* ((lcur (version-to-list current))
         (lnew (version-to-list new))
         (scur (space-macs//compute-version-score lcur))
         (snew (space-macs//compute-version-score lnew))
         (diff (- snew scur)))
    (cond
     ((< diff 3000) 'space-macs-mode-line-new-version-lighter-success-face)
     ((< diff 5000) 'space-macs-mode-line-new-version-lighter-warning-face)
     (t 'space-macs-mode-line-new-version-lighter-error-face))))

(defun space-macs/get-last-version ()
  "Return the last tagged version."
  (interactive)
  (space-macs//get-last-version space-macs-repository
                               space-macs-repository-owner
                               space-macs-checkversion-remote
                               space-macs-checkversion-branch))

(defun space-macs//can-check-for-new-version-at-startup ()
  "Return non-nil if the version check at startup can be performed."
  (when (file-exists-p space-macs-version--last-startup-check-file)
    (load space-macs-version--last-startup-check-file))
  (let ((result
         (or (null space-macs-version--last-startup-check-time)
             (> (- (float-time) space-macs-version--last-startup-check-time)
                space-macs-version--startup-check-interval))))
    (when result
      (setq space-macs-version--last-startup-check-time (float-time))
      (space-macs/dump-vars-to-file '(space-macs-version--last-startup-check-time)
                                   space-macs-version--last-startup-check-file))
    result))

(defun space-macs//get-last-version (repo owner remote branch)
  "Return the last tagged version of BRANCH on REMOTE repository from
OWNER REPO."
  (let ((url (format "https://github.com/%s/%s" owner repo)))
    (space-macs//git-remove-remote remote)
    (space-macs//git-add-remote remote url)
    ;; removing this call according to issue #6692 proposal
    ;; (space-macs//git-fetch-remote remote)
    (space-macs//git-fetch-tags remote branch))
  (let ((version (space-macs//git-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

(defun space-macs//git-has-remote (remote)
  "Return non nil if REMOTE is declared."
  (let ((proc-buffer "git-has-remote")
       (default-directory (file-truename space-macs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
        (with-current-buffer proc-buffer
          (prog2
              (goto-char (point-min))
              (re-search-forward (format "^%s$" remote) nil 'noerror)
            (kill-buffer proc-buffer))))))

(defun space-macs//git-add-remote (remote url)
  "Add a REMOTE with URL, return t if no error."
  (let ((proc-buffer "git-add-remote")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer proc-buffer))))

(defun space-macs//git-remove-remote (remote)
  "Remove a REMOTE, return t if no error."
  (let ((proc-buffer "git-remove-remote")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "remove" remote))
      (kill-buffer proc-buffer))))

(defun space-macs//git-fetch-remote (remote)
  "Fetch last commits from REMOTE, return t if no error."
  (let ((proc-buffer "git-fetch-remote")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote))
      (kill-buffer proc-buffer))))

(defun space-macs//git-fetch-tags (remote branch)
  "Fetch the tags for BRANCH in REMOTE repository."
  (let ((proc-buffer "git-fetch-tags")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        ;;;; original comment: seems necessary to fetch first
        ;; but we remove this according to issue #6692 proposal
        ;; (eq 0 (process-file "git" nil proc-buffer nil
        ;;                     "fetch" remote branch))
        ;; explicitly fetch the new tags
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun space-macs//git-hard-reset-to-tag (tag)
  "Hard reset the current branch to specified TAG."
  (let ((proc-buffer "git-hard-reset")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "reset" "--hard" tag))
      (kill-buffer proc-buffer))))

(defun space-macs//git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let ((proc-buffer "git-latest-tag")
       (default-directory (file-truename space-macs-start-directory))
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

(defun space-macs//git-checkout (branch)
  "Checkout the given BRANCH. Return t if there is no error."
  (let ((proc-buffer "git-checkout")
       (default-directory (file-truename space-macs-start-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "checkout" branch))
      (kill-buffer proc-buffer))))

(defun space-macs//git-get-current-branch ()
   "Return the current branch. Return nil if an error occurred."
   (let ((proc-buffer "git-get-current-branch")
        (default-directory (file-truename space-macs-start-directory)))
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

(defun space-macs//git-working-directory-dirty ()
  "Non-nil if the user's e-macs directory is not clean.
Returns the output of git status --porcelain."
  (let ((proc-buffer "git-working-directory-dirty")
       (default-directory (file-truename space-macs-start-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "status" "--porcelain"))
      (with-current-buffer proc-buffer
        (prog1
            (when (and (buffer-string)
                       ;;simplecheckforanytext
                       (string-match-p "[^ \t\n]" (buffer-string)))
              (replace-regexp-in-string "\n\\'" "" (buffer-string)))
          (kill-buffer proc-buffer))))))

(defun space-macs//deffaces-new-version-lighter (state)
  "Define a new version lighter face for the given STATE."
  (let* ((fname (intern
                 (format "space-macs-mode-line-new-version-lighter-%s-face"
                         (symbol-name state))))
         (foreground (face-foreground state)))
    (eval `(defface ,fname '((t ()))
             ,(format "Color for new version lighter in mode line (%s)."
                      (symbol-name state))
             :group 'space-macs))
    (set-face-attribute fname nil
                        :foreground foreground
                        :box (face-attribute 'mode-line :box))))

(defun space-macs//compute-version-score (version)
  "Returns an integer from the version list.
Example: (1 42 3) = 1 042 003"
  (let ((i -1))
    (cl-reduce '+ (mapcar (lambda (n) (setq i (1+ i)) (* n (expt 10 (* i 3))))
                       (reverse version)))))

(defun space-macs/set-new-version-lighter-mode-line-faces ()
  "Define or set the new version lighter mode-line faces."
  (mapcar 'space-macs//deffaces-new-version-lighter
          '(error warning success)))
(space-macs/set-new-version-lighter-mode-line-faces)
(add-hook 'space-macs-post-theme-change-hook
          'space-macs/set-new-version-lighter-mode-line-faces)

(provide 'core-release-management)


