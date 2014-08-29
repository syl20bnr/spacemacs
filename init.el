;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;; Locations ==================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "Emacs home directory.")

(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

(defvar user-org-directory
  (expand-file-name (concat user-emacs-directory "my-org/"))
  "Org files directory.")

(defvar user-config-directory
  (expand-file-name (concat user-emacs-directory "config/"))
  "Configuration scripts.")

(defvar user-extensions-directory
  (expand-file-name (concat user-emacs-directory "extensions/"))
  "Additional extensions.")

(defvar user-init-extension-directory
  (expand-file-name (concat user-emacs-directory "init-extension/"))
  "Extension initialization.")

(defvar user-init-package-directory
  (expand-file-name (concat user-emacs-directory "init-package/"))
  "Package initialization.")

(defvar host-directory
  (expand-file-name (concat user-emacs-directory "host/" system-name "/"))
  "Host specific configurations")

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-extensions-directory)
(add-to-list 'load-path (expand-file-name (concat user-dropbox-directory "emacs/")))

;; Setup ======================================================================
(require 'my-funcs)
(require 'my-macros)
(require 'pre-extensions)
(require 'packages)
(require 'post-extensions)
(require 'my-keybindings)

(load-user-config)
(load-host-config)

;; Set first theme of the list
(cycle-my-theme)

;; Post initialization  =======================================================
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

