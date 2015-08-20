;;; core-spacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst spacemacs-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")
(defconst spacemacs-checkversion-remote "checkversion"
  "Name of the remote repository used to check for new version.")
(defconst spacemacs-checkversion-branch "master"
  "Name of the branch used to check for new version.")

;; new version variables
(defvar spacemacs-new-version nil
  "If non-nil a new Spacemacs version is available.")
(defvar spacemacs-version-check-timer nil
  "The current timer for new version check.")
(defvar spacemacs-version-check-interval "6 hours"
  "Time between two version checks.")
(defvar spacemacs-version-check-lighter "[+]"
  "Text displayed in the mode-line when a new version is available.")

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
  (let ((branch (spacemacs/git-get-current-branch)))
    (if (string-equal "develop" branch)
        (message (concat "Cannot switch version because you are on develop.\n"
                         "You have to manually `pull --rebase' last commits."))
      (unless version (setq version (read-string "version: "
                                                 (spacemacs/get-last-version))))
      (when (or (string-equal "master" branch)
                (yes-or-no-p (format (concat "You are not on master, are you "
                                             "sure that you want to switch to "
                                             "version %s ? ") version)))
        (let ((tag (concat "v" version)))
          (if (spacemacs/git-hard-reset-to-tag tag)
              (progn
                (setq spacemacs-version version)
                (message "Succesfully switched to version %s" version))
            (message "An error occurred while switching to version %s"
                     version)))))))

(defun spacemacs/check-for-new-version (&optional interval)
  "Periodicly check for new for new Spacemacs version.
Update `spacemacs-new-version' variable if any new version has been
found."
  (if (string-equal "develop" (spacemacs/git-get-current-branch))
      (message "Skipping check for new version because you are on develop.")
    (message "Start checking for new version...")
    (async-start
     (lambda ()
       (load-file (concat user-emacs-directory "core/core-load-paths.el"))
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
      (setq spacemacs-version-check-timer
            (run-at-time t (timer-duration interval)
                         'spacemacs/check-for-new-version)))))

(defun spacemacs/get-last-version ()
  "Return the last tagged version."
  (spacemacs//get-last-version spacemacs-repository
                               spacemacs-repository-owner
                               spacemacs-checkversion-remote
                               spacemacs-checkversion-branch))

(defun spacemacs//get-last-version (repo owner remote branch)
  "Return the last tagged version of BRANCH on REMOTE repository from
OWNER REPO."
  (let ((url (format "https://github.com/%s/%s" owner repo)))
    (spacemacs/git-remove-remote remote)
    (spacemacs/git-add-remote remote url)
    (spacemacs/git-fetch-remote remote)
    (spacemacs/git-fetch-tags remote branch))
  (let ((version (spacemacs/git-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

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

(defun spacemacs/git-has-remote (remote)
  "Return non nil if REMOTE is declared."
  (let((proc-buffer "git-has-remote")
       (default-directory (file-truename user-emacs-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
        (with-current-buffer proc-buffer
          (prog2
              (goto-char (point-min))
              (re-search-forward (format "^%s$" remote) nil 'noerror)
            (kill-buffer proc-buffer))))))

(defun spacemacs/git-add-remote (remote url)
  "Add a REMOTE with URL, return t if no error."
  (let((proc-buffer "git-add-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-remove-remote (remote)
  "Remove a REMOTE, return t if no error."
  (let((proc-buffer "git-remove-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "remove" remote))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-fetch-remote (remote)
  "Fetch last commits from REMOTE, return t if no error."
  (let((proc-buffer "git-remove-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-fetch-tags (remote branch)
  "Fetch the tags for BRANCH in REMOTE repository."
  (let((proc-buffer "git-fetch-tags")
       (default-directory (file-truename user-emacs-directory)))
    (prog2
        ;; seems necessary to fetch first
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote branch))
        ;; explicitly fetch the new tags
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-hard-reset-to-tag (tag)
  "Hard reset the current branch to specifed TAG."
  (let((proc-buffer "git-hard-reset")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "reset" "--hard" tag))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let((proc-buffer "git-latest-tag")
       (default-directory (file-truename user-emacs-directory))
       (where (format "%s/%s" remote branch)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "describe" "--tags" "--abbrev=0"
                              "--match=v*" where "FETCH_HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
                (end-of-buffer)
                (forward-line -1)
                (replace-regexp-in-string
                 "\n$" ""
                 (buffer-substring (line-beginning-position)
                                   (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun spacemacs/git-checkout (branch)
  "Checkout the given BRANCH. Return t if there is no error."
  (let((proc-buffer "git-checkout")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "checkout" branch))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-get-current-branch ()
   "Return the current branch. Return nil if an error occurred."
   (let((proc-buffer "git-get-current-branch")
        (default-directory (file-truename user-emacs-directory)))
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

(defun spacemacs/git-get-current-branch-rev ()
  "Returns the hash of the head commit on the current branch.
Returns nil if an error occurred."
  (let((proc-buffer "git-get-current-branch-head-hash")
       (default-directory (file-truename user-emacs-directory)))
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

(defun spacemacs//deffaces-new-version-lighter (state)
  "Define a new version lighter face for the given STATE."
  (let* ((fname (intern (format "spacemacs-mode-line-new-version-lighter-%s-face"
                                (symbol-name state))))
         (foreground (face-foreground state)))
    (eval `(defface ,fname '((t ()))
             ,(format "Color for new version lighter in mode line (%s)."
                      (symbol-name state))
             :group 'spacemacs))
    (set-face-attribute fname nil
                        :foreground foreground
                        :box (face-attribute 'mode-line :box))))

(defun spacemacs/set-new-version-lighter-mode-line-faces ()
  "Define or set the new version lighter mode-line faces."
  (mapcar 'spacemacs//deffaces-new-version-lighter
          '(error warning success)))
(spacemacs/set-new-version-lighter-mode-line-faces)

(defun spacemacs//compute-version-score (version)
  "Returns an integer from the version list.
Example: (1 42 3) = 1 042 003"
  (let ((i -1))
    (reduce '+ (mapcar (lambda (n) (setq i (1+ i)) (* n (expt 10 (* i 3))))
                       (reverse version)))))

(provide 'core-release-management)
