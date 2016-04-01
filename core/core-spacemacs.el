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
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(require 'subr-x nil 'noerror)
(require 'core-emacs-backports)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-spacemacs-buffer)
(require 'core-toggle)
(require 'core-micro-state)

(defconst spacemacs-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst spacemacs-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")
(defconst spacemacs-checkversion-remote "checkversion"
  "Name of the remote repository used to check for new version.")
(defconst spacemacs-checkversion-branch "master"
  "Name of the branch used to check for new version.")

(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'starter-kit
  :prefix 'spacemacs-)

;; new version variables
(defvar spacemacs-new-version nil
  "If non-nil a new Spacemacs version is available.")
(defvar spacemacs-version-check-timer nil
  "The current timer for new version check.")
(defvar spacemacs-version-check-interval "6 hours"
  "Time between two version checks.")
(defvar spacemacs-version-check-lighter "[+]"
  "Text displayed in the mode-line when a new version is available.")

;; loading progress bar variables
(defvar spacemacs-loading-char ?█)
(defvar spacemacs-loading-string "")
(defvar spacemacs-loading-counter 0)
;; (defvar spacemacs-loading-text "Loading")
;; (defvar spacemacs-loading-done-text "Ready!")
(defvar spacemacs-loading-dots-chunk-count 3)
(defvar spacemacs-loading-dots-count (window-total-size nil 'width))
(defvar spacemacs-loading-dots-chunk-size
  (/ spacemacs-loading-dots-count spacemacs-loading-dots-chunk-count))
(defvar spacemacs-loading-dots-chunk-threshold 0)

(define-derived-mode spacemacs-mode special-mode "Spacemacs"
  "Spacemacs major mode for startup screen."
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  ;; motion state since this is a special mode
  (add-to-list 'evil-motion-state-modes 'spacemacs-mode))

(defun spacemacs/init ()
  "Create the special buffer for `spacemacs-mode' and perform startup
initialization."
  ;; dotfile init
  (dotspacemacs/load-file)
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  ;; spacemacs init
  (switch-to-buffer (get-buffer-create spacemacs-buffer-name))
  (spacemacs/set-mode-line "")
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
  ;; default theme
  (let ((default-theme (car dotspacemacs-themes)))
    (spacemacs/load-theme default-theme)
    (setq-default spacemacs--cur-theme default-theme)
    (setq-default spacemacs--cycle-themes (cdr dotspacemacs-themes)))
  ;; removes the GUI elements
  (unless (or (not (boundp 'tool-bar-mode)) (eq tool-bar-mode -1))
    (tool-bar-mode -1))
  (unless (or (not (boundp 'scroll-bar-mode)) (eq scroll-bar-mode -1))
    (scroll-bar-mode -1))
  ;; tooltips in echo-aera
  (unless (or (not (boundp 'tooltip-mode)) (eq tooltip-mode -1))
    (tooltip-mode -1))
  (setq tooltip-use-echo-area t)
  (unless (or (not (boundp 'tooltip-mode))
              (eq window-system 'mac)
              (eq tooltip-mode -1))
    (menu-bar-mode -1))
  ;; for convenience and user support
  (unless (boundp 'tool-bar-mode)
    (spacemacs/message (concat "No graphical support detected, you won't be"
                               "able to launch a graphical instance of Emacs"
                               "with this build.")))
  ;; font
  (if (find-font (font-spec :name (car dotspacemacs-default-font)))
      (spacemacs/set-default-font dotspacemacs-default-font)
    (spacemacs/message "Warning: Cannot find font \"%s\"!"
                       (car dotspacemacs-default-font)))
  ;; banner
  (spacemacs//insert-banner-and-buttons)
  (setq-default evil-want-C-u-scroll t)
  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs|call-func dotspacemacs/init "Executing user init...")
  ;; dash is required to prevent a package.el bug with f on 24.3.1
  (spacemacs/load-or-install-package 'dash t)
  ;; bind-key is required by use-package
  (spacemacs/load-or-install-package 'bind-key t)
  (spacemacs/load-or-install-package 'use-package t)
  ;; evil and evil-leader must be installed at the beginning of the boot sequence
  ;; use C-u as scroll-up (must be set before actually loading evil)
  (spacemacs/load-or-install-package 'evil t)
  (spacemacs/load-or-install-package 'evil-leader t)
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  (spacemacs/set-new-version-lighter-mode-line-faces)
  (add-hook 'after-init-hook 'spacemacs/goto-link-line)
  (spacemacs-mode))

(defun spacemacs//get-package-directory (pkg)
  "Return the directory of PKG. Return nil if not found."
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (reduce (lambda (x y) (if x x y))
                         (mapcar (lambda (x)
                                   (if (string-match
                                        (concat "/" (symbol-name pkg) "-") x) x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (if dir (file-name-as-directory dir))))))

(defun spacemacs/load-or-install-package (pkg &optional log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If LOG is non-nil a message is displayed in spacemacs-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (condition-case nil
      (require pkg)
    (error
     ;; not installed, we try to initialize package.el only if required to
     ;; precious seconds during boot time
     (require 'cl)
     (let ((pkg-elpa-dir (spacemacs//get-package-directory pkg)))
       (if pkg-elpa-dir
           (add-to-list 'load-path pkg-elpa-dir)
         ;; install the package
         (when log
           (spacemacs/append-to-buffer
            (format "(Bootstrap) Installing %s...\n" pkg))
           (spacemacs//redisplay))
         (package-refresh-contents)
         (package-install pkg)
         (setq pkg-elpa-dir (spacemacs//get-package-directory pkg)))
       (require pkg nil 'noerror)
       (when file-to-load
         (load-file (concat pkg-elpa-dir file-to-load)))
       pkg-elpa-dir))))

(defun spacemacs/display-and-copy-version ()
  "Echo the current spacemacs version and copy it."
  (interactive)
  (let ((msg (format "Spacemacs v.%s" spacemacs-version)))
    (message msg) (kill-new msg)))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defun spacemacs/get-last-version (repo owner remote branch)
  "Return the last tagged version of BRANCH on REMOTE repository from
OWNER REPO."
  (let ((url (format "http://github.com/%s/%s" owner repo)))
    (unless (spacemacs/git-has-remote remote)
      (spacemacs/git-declare-remote remote url)))
  (spacemacs/git-fetch-tags remote branch)
  (let ((version (spacemacs/git-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

(defun spacemacs/check-for-new-version (&optional interval)
  "Periodicly check for new for new Spacemacs version.
Update `spacemacs-new-version' variable if any new version has been
found."
  (message "Start checking for new version...")
  (async-start
   (lambda ()
     (load-file (concat user-emacs-directory "core/core-load-paths.el"))
     (require 'core-spacemacs)
     (spacemacs/get-last-version spacemacs-repository
                                 spacemacs-repository-owner
                                 spacemacs-checkversion-remote
                                 spacemacs-checkversion-branch))
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
                       'spacemacs/check-for-new-version))))

(defun spacemacs/git-has-remote (remote)
  "Return non nil if REMOTE is declared."
  (let((proc-buffer "git-has-remote")
       (default-directory user-emacs-directory))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
        (with-current-buffer proc-buffer
          (prog2
              (goto-char (point-min))
              (re-search-forward (format "^%s$" remote) nil 'noerror)
            (kill-buffer proc-buffer))))))

(defun spacemacs/git-declare-remote (remote url)
  "Declare a new REMOTE pointing to URL, return t if no error."
  (let((proc-buffer "git-declare-remote")
       (default-directory user-emacs-directory))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-fetch-tags (remote branch)
  "Fetch the tags for BRANCH in REMOTE repository."
  (let((proc-buffer "git-fetch-tags")
       (default-directory user-emacs-directory))
    (prog2
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote branch))
        ;; explicitly fetch the new tags
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let((proc-buffer "git-latest-tag")
       (default-directory user-emacs-directory)
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
  (let ((result 0)
        (rev (reverse version)))
    (dotimes (i 3)
      (setq result (+ result (* (nth i rev) (expt 10 (* i 3))))))
    result))

(defun spacemacs//compute-version-score (version)
  "Returns an integer from the version list.
Example: (1 42 3) = 1 042 003"
  (let ((i -1))
    (reduce '+ (mapcar (lambda (n) (setq i (1+ i)) (* n (expt 10 (* i 3))))
                       (reverse version)))))

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

(defun spacemacs/setup-after-init-hook ()
  "Add post init processing."
  (add-hook
   'after-init-hook
   (lambda ()
     ;; Ultimate configuration decisions are given to the user who can defined
     ;; them in his/her ~/.spacemacs file
     (dotspacemacs|call-func dotspacemacs/config "Calling dotfile config...")
     ;; from jwiegley
     ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
     (let ((elapsed (float-time
                     (time-subtract (current-time) emacs-start-time))))
       (spacemacs/append-to-buffer
        (format "[%s packages loaded in %.3fs]\n"
                (configuration-layer//initialized-packages-count)
                elapsed)))
     (when configuration-layer-error-count
       ;; ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
        ;; (vc-mode vc-mode)
       ;; "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
       (spacemacs/set-mode-line
        (format (concat "%s errors at startup! "
                        "Spacemacs may not be able to operate properly.")
                configuration-layer-error-count))
       (force-mode-line-update))
     (spacemacs/check-for-new-version spacemacs-version-check-interval))))

(provide 'core-spacemacs)
