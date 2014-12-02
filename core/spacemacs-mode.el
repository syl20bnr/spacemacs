(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(defconst spacemacs-min-version "24.3"
  "Mininal required version of Emacs.")

(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(add-to-list 'load-path spacemacs-core-directory)

(defconst spacemacs-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Spacemacs templates directory.")

;; additional paths
(defconst user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "User home directory (~/).")
(defconst spacemacs-directory
  (expand-file-name (concat user-emacs-directory "spacemacs/"))
  "Spacemacs base directory.")
(defconst spacemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Spacemacs storage area for persistent files.")
(if (not (file-exists-p spacemacs-cache-directory))
    (make-directory spacemacs-cache-directory))
(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

(defvar spacemacs-title-length 70)
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

(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'starter-kit
  :prefix 'spacemacs-)

(define-derived-mode spacemacs-mode special-mode "spacemacs-mode"
  "Spacemacs major mode for startup screen."
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
  ;; Unless Emacs stock themes
  (unless (memq dotspacemacs-default-theme (custom-available-themes))
    (cond
     ;; Spacemacs default theme
     ((or (eq 'solarized-light dotspacemacs-default-theme)
          (eq 'solarized-dark dotspacemacs-default-theme))
      (add-to-list 'load-path (concat spacemacs-directory
                                      "extensions/solarized-theme/"))
      ;; solarized dependency
      (spacemacs/load-or-install-package 'dash)
      (require 'solarized)
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))
     (t 
      ;; other themes
      ;; we assume that the package name is suffixed with `-theme'
      ;; if not we will handle the special themes as we get issues in the tracker.
      (let ((pkg (format "%s-theme" (symbol-name dotspacemacs-default-theme))))
        (spacemacs/load-or-install-package (intern pkg))))))
  (load-theme dotspacemacs-default-theme t)
  ;; font
  ;; Dynamic font size depending on the system
  (let ((font "Source Code Pro"))
    (when (member font (font-family-list))
      (pcase window-system
        (`x (spacemacs/set-font font 10))
        (`mac (spacemacs/set-font font 12))
        (`w32 (spacemacs/set-font font 9))
        (other (spacemacs/set-font font 10)))))
  ;; remove GUI elements
  (unless (eq tool-bar-mode -1)
    (tool-bar-mode -1)
    (when (not (eq window-system 'mac))
      (menu-bar-mode -1))
    (scroll-bar-mode -1))
  ;; motion state since this is a special mode
  (eval-after-load 'evil
    '(add-to-list 'evil-motion-state-modes 'spacemacs-mode)))

(defun spacemacs/load-or-install-package (pkg)
  "Load PKG package. PKG will be installed if it is not already
installed."
  (condition-case nil
      (require pkg)
    (error
     ;; not installed, we try to initialize package.el only if required to
     ;; precious seconds during boot time
     (require 'cl)
     (let* ((elpa-dir (concat user-emacs-directory "elpa/"))
            (pkg-elpa-dir
             (if (file-exists-p elpa-dir)
                 (reduce (lambda (x y) (if x x y))
                         (mapcar (lambda (x)
                                   (if (string-match (symbol-name pkg) x) x))
                                 (directory-files elpa-dir))
                         :initial-value nil))))
       (if pkg-elpa-dir
           (add-to-list 'load-path (concat user-emacs-directory "elpa/"
                                           pkg-elpa-dir))
         ;; install the package
         (config-system/package.el-initialize)
         (package-refresh-contents)
         (package-install pkg))
       (require pkg)))))

(defun spacemacs/emacs-version-ok ()
  (not (version< emacs-version spacemacs-min-version)))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defun spacemacs/set-font (font size &optional options)
  (let* ((fontstr (if options
                       (format "%s-%s:%s" font size options)
                     (format "%s-%s" font size))))
    (spacemacs/message (format "Set default font: %s" fontstr))
    (add-to-list 'default-frame-alist (cons 'font fontstr))
    (set-default-font fontstr)))

(defun spacemacs/buffer ()
  "Create and initialize the spacemacs startup buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
  (spacemacs-mode)
  (let ((buffer-read-only nil))
    (insert-file-contents (concat spacemacs-core-directory "banner.txt"))
    (unless (spacemacs/emacs-version-ok)
      (spacemacs/append-to-buffer
       (format "\nError: Minimal required Emacs version for Spacemacs is %s "
               spacemacs-min-version))
      (spacemacs/append-to-buffer (format
                                   "whereas current Emacs version is %s.\n"
                                   emacs-version))
      (spacemacs/append-to-buffer "Spacemacs is disabled.\n")
      (redisplay))))

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
