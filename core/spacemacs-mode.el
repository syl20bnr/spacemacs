;;; spacemacs-mode.el --- Spacemacs Core File
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
(require 'emacs-backports)
(require 'themes-support)

(defconst spacemacs-version "0.48.1"
  "Spacemacs version.")
(defconst spacemacs-min-version "24.3"
  "Mininal required version of Emacs.")

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

;; paths
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(defconst spacemacs-banner-directory
  (expand-file-name (concat spacemacs-core-directory "banners/"))
  "Spacemacs banners directory.")
(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst spacemacs-directory
  (expand-file-name (concat user-emacs-directory "spacemacs/"))
  "Spacemacs base directory.")
(defconst spacemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Spacemacs storage area for persistent files.")
(defconst pcache-directory
  (concat spacemacs-cache-directory "pcache"))
(unless (file-exists-p spacemacs-cache-directory)
    (make-directory spacemacs-cache-directory))
(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

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
(defvar spacemacs-title-length 75)
(defvar spacemacs-loading-counter 0)
(defvar spacemacs-loading-text "Loading")
(defvar spacemacs-loading-done-text "Ready!")
(defvar spacemacs-loading-dots-chunk-count 3)
(defvar spacemacs-loading-dots-count
  (- spacemacs-title-length
     (length spacemacs-loading-text)
     (length spacemacs-loading-done-text)))
(defvar spacemacs-loading-dots-chunk-size
  (/ spacemacs-loading-dots-count spacemacs-loading-dots-chunk-count))
(defvar spacemacs-loading-dots-chunk-threshold 0)
(defvar spacemacs-solarized-dark-createdp nil)

(define-derived-mode spacemacs-mode special-mode "spacemacs-mode"
  "Spacemacs major mode for startup screen."
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
  ;; theme
  (spacemacs/load-default-theme)
  ;; remove GUI elements if supported
  (when window-system
    ;; those unless tests are for the case when the user has a ~/.emacs file
    ;; were he/she ;; removes the GUI elements
    (unless (eq tool-bar-mode -1)
      (tool-bar-mode -1))
    (unless (eq scroll-bar-mode -1)
      (scroll-bar-mode -1))
    ;; tooltips in echo-aera
    (unless (eq tooltip-mode -1)
      (tooltip-mode -1))
    (setq tooltip-use-echo-area t))
  (unless (eq window-system 'mac)
    (menu-bar-mode -1))
  ;; for convenience and user support
  (unless (boundp 'tool-bar-mode)
    (spacemacs/message (concat "No graphical support detected, you won't be"
                               "able to launch a graphical instance of Emacs"
                               "with this build.")))
  ;; font
  ;; Dynamic font size depending on the system
  (let ((font "Source Code Pro"))
    (when (member font (font-family-list))
      (pcase window-system
        (`x (spacemacs/set-font font 10))
        (`mac (spacemacs/set-font font 12))
        (`w32
         (spacemacs/set-font font 9)
         (let ((fallback-font "Lucida Sans Unicode"))
           ;; window numbers
           (set-fontset-font "fontset-default"
                             '(#x2776 . #x2793) fallback-font nil 'append)
           ;; mode-line circled letters
           (set-fontset-font "fontset-default"
                             '(#x24b6 . #x24fe) fallback-font nil 'append)
           ;; mode-line additional characters (i.e. golden ratio)
           (set-fontset-font "fontset-default"
                             '(#x2295 . #x22a1) fallback-font nil 'append)
           ;; new version lighter
           (set-fontset-font "fontset-default"
                             '(#x2190 . #x2200) fallback-font nil 'append)))
        (other (spacemacs/set-font font 10)))))
  
  ;; banner
  (spacemacs//insert-banner)
  ;; bind-key is required by use-package
  (spacemacs/load-or-install-package 'bind-key t)
  (spacemacs/load-or-install-package 'use-package t)
  ;; evil and evil-leader must be installed at the beginning of the boot sequence
  ;; use C-u as scroll-up (must be set before actually loading evil)
  (setq-default evil-want-C-u-scroll t)
  (setq-default evil-want-fine-undo nil)
  (spacemacs/load-or-install-package 'evil t)
  (spacemacs/load-or-install-package 'evil-leader t)
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  (spacemacs/set-new-version-lighter-mode-line-faces)
  ;; motion state since this is a special mode
  (add-to-list 'evil-motion-state-modes 'spacemacs-mode))

(defun spacemacs/initialize ()
  "Create the special buffer for `spacemacs-mode' and perform startup
initialization."
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
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
           (redisplay))
         (package-refresh-contents)
         (package-install pkg)
         (setq pkg-elpa-dir (spacemacs//get-package-directory pkg)))
       (require pkg nil 'noerror)
       (when file-to-load
         (load-file (concat pkg-elpa-dir file-to-load)))
       pkg-elpa-dir))))

(defun spacemacs/emacs-version-ok ()
  (version<= spacemacs-min-version emacs-version))

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
     (add-to-list 'load-path (concat user-emacs-directory "core/"))
     (require 'spacemacs-mode)
     (spacemacs/get-last-version spacemacs-repository
                                 spacemacs-repository-owner
                                 spacemacs-checkversion-remote
                                 spacemacs-checkversion-branch))
   (lambda (result)
     (when result
       (unless (or (version< result spacemacs-version)
                   (string= result spacemacs-version)
                   (if spacemacs-new-version
                       (string= result spacemacs-new-version)))
         (message "New version of Spacemacs available: %s" result)
         (setq spacemacs-new-version result)))))
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
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote branch))
      (kill-buffer proc-buffer))))

(defun spacemacs/git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let((proc-buffer "git-latest-tag")
       (default-directory user-emacs-directory)
       (where (format "%s/%s" remote branch)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "describe" "--tags" "--abbrev=0"
                              "--match=v*" where))
      (with-current-buffer proc-buffer
        (prog1
            (if (buffer-string)
                (replace-regexp-in-string "\n$" "" (buffer-string)))
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

(defun spacemacs/set-font (font size &optional options)
  (let* ((fontstr (if options
                      (format "%s-%s:%s" font size options)
                    (format "%s-%s" font size))))
    (spacemacs/message (format "Set default font: %s" fontstr))
    (add-to-list 'default-frame-alist (cons 'font fontstr))
    (set-default-font fontstr)))

(defun spacemacs//insert-banner ()
  "Choose a banner and insert in spacemacs buffer.

Doge special banner can be reachable via `999', `doge' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (cond
                 ((eq 'random dotspacemacs-startup-banner)
                  (spacemacs//choose-random-banner))
                 ((eq 'random* dotspacemacs-startup-banner)
                  (spacemacs//choose-random-banner t))
                 ((eq 'doge dotspacemacs-startup-banner)
                  (spacemacs//get-banner-path 999))
                 ((integerp dotspacemacs-startup-banner)
                  (spacemacs//get-banner-path dotspacemacs-startup-banner))))
        (buffer-read-only nil))
    (when banner
      (spacemacs/message (format "Banner: %s" banner))
      (insert-file-contents banner)
      (spacemacs//inject-version-in-buffer)
      (spacemacs/insert-buttons))))

(defun spacemacs//choose-random-banner (&optional all)
  "Return the full path of a banner chosen randomly.

If ALL is non-nil then truly all banners can be selected."
  (let* ((files (directory-files spacemacs-banner-directory t))
         (count (length files))
         ;; -2 then +2 to remove `.' and `..'
         (choice (+ 2 (random (- count (if all 2 3))))))
    (nth choice files)))

(defun spacemacs//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat spacemacs-banner-directory (format "%03d-banner.txt" index)))

(defun spacemacs//inject-version-in-buffer ()
  "Inject the current version of spacemacs in the first line of the
buffer, right justified."
  (save-excursion
    (beginning-of-buffer)
    (let* ((maxcol spacemacs-title-length)
           (injected (format "(%s)" spacemacs-version))
           (pos (- maxcol (length injected)))
           (buffer-read-only nil))
      ;; fill the first line with spaces if required
      (when (< (line-end-position) maxcol)
        (end-of-line)
        (insert-char ?\s (- maxcol (line-end-position))))
      (goto-char pos)
      (delete-char (length injected))
      (insert injected))))

(defun spacemacs/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'."
  (message "(Spacemacs) %s" (apply 'format msg args)))

(defun spacemacs/append-to-buffer (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/replace-last-line-of-buffer (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/loading-animation ()
  "Display LOADING-TITLE with trailing dots of max length
SPACEMACS-TITLE-LENGTH. New loading title is displayed by chunk
of size LOADING-DOTS-CHUNK-THRESHOLD."
  (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
  (if (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
      (progn
        (setq spacemacs-loading-counter 0)
        (let ((i 0))
          (while (< i spacemacs-loading-dots-chunk-size)
            (setq spacemacs-loading-text (concat spacemacs-loading-text "."))
            (setq i (1+ i))))
        (spacemacs/replace-last-line-of-buffer spacemacs-loading-text)
        (redisplay))))

(defun spacemacs/insert-buttons ()
  (goto-char (point-max))
  (insert "   ")
  (insert-button "Homepage" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs"))
                 'follow-link t)
  (insert " ")
  (insert-button "Documentation" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md"))
                 'follow-link t)
  (insert " ")
  (insert-button "Gitter Chat" 'action
                 (lambda (b) (browse-url "https://gitter.im/syl20bnr/spacemacs"))
                 'follow-link t)
  (insert " ")
  (insert-button "Messages Buffer" 'action (lambda (b) (switch-to-buffer "*Messages*")) 'follow-link t)
  (insert " ")
  (insert-button "Spacemacs Folder" 'action (lambda (b) (find-file user-emacs-directory)) 'follow-link t)
  (insert "\n")
  (insert "                            ")
  (insert-button "Update Spacemacs" 'action (lambda (b) (configuration-layer/update-packages)) 'follow-link t)
  (insert "\n\n")
  )

(provide 'spacemacs-mode)
