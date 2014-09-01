;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;; ---------------------------------------------------------------------------
;; Locations
;; ---------------------------------------------------------------------------
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "Emacs home directory.")

(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

(defvar user-org-directory
  (expand-file-name (concat user-emacs-directory "my-org/"))
  "Org files directory.")

(defvar spacemacs-config-directory
  (expand-file-name (concat user-emacs-directory "spacemacs/"))
  "Configuration scripts.")

(defvar spacemacs-extensions-directory
  (expand-file-name (concat user-emacs-directory "extensions/"))
  "Additional extensions.")

(defvar spacemacs-init-extension-directory
  (expand-file-name (concat user-emacs-directory "init-extension/"))
  "Extension initialization.")

(defvar spacemacs-init-package-directory
  (expand-file-name (concat user-emacs-directory "init-package/"))
  "Package initialization.")

(defvar host-directory
  (expand-file-name (concat user-emacs-directory "host/" system-name "/"))
  "Host specific configurations")

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path spacemacs-extensions-directory)
(add-to-list 'load-path spacemacs-config-directory)
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------
(require 'se-funcs)
(require 'se-macros)
(require 'se-extensions)
(require 'se-packages)

;; install and initialize extensions and packages
;; pre extensions
(se/load-and-initialize-extensions se/pre-extensions
                                   spacemacs-extensions-directory
                                   spacemacs-init-extension-directory)
;; packages
(se/install-missing-packages se/packages)
(se/initialize-packages spacemacs-init-package-directory)
;; post extensions
(se/load-and-initialize-extensions se/post-extensions
                                   spacemacs-extensions-directory
                                   spacemacs-init-extension-directory)

(require 'se-keybindings)

;; Emacs configuration, the following configurations files are loaded
;; in this order:
;; - spacemacs configuration (emacs_path/spacemacs/se-config.el)
;; - host specific configuration (emacs_path/host/<hostname>/host-config.el)
;; - user specific configuration (~/.spacemacs)
(require 'se-config)
;; host specific configuration in emacs_path/host/<hostname>
(defun load-config (directory)
  (progn (when (file-exists-p directory)
           (dolist (l (directory-files directory nil "^[^#].*el$"))
             (load (concat directory l))))))
(load-config host-directory)
;; load ~/.spacemacs file
;; store your personnal configuration in this file
;; you can override previous settings here
(require 'my-config (concat user-home-directory ".spacemacs"))

;; Set first theme of the list
(cycle-my-theme)

;; ---------------------------------------------------------------------------
;; Post initialization
;; ---------------------------------------------------------------------------
;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; Display load times after init.el and after all buffers has been loaded
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

