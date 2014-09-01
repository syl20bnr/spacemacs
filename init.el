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

(add-to-list 'load-path user-emacs-directory)

;; Spacemaces configs

(defvar spacemacs-config-directory
  (expand-file-name (concat user-emacs-directory "spacemacs/"))
  "Spacemacs configuration scripts.")

(defvar spacemacs-extensions-directory
  (expand-file-name (concat spacemacs-config-directory "extensions/"))
  "Spacemacs extensions.")

(defvar spacemacs-init-extension-directory
  (expand-file-name (concat spacemacs-config-directory "init-extension/"))
  "Spacemacs Extensions initialization.")

(defvar spacemacs-init-package-directory
  (expand-file-name (concat spacemacs-config-directory "init-package/"))
  "Spacemacs Packages initialization.")

(add-to-list 'load-path spacemacs-config-directory)

;; Contributions configs

(defvar contrib-config-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Contributions configuration scripts.")

(defvar contrib-extensions-directory
  (expand-file-name (concat contrib-config-directory "extensions/"))
  "Contributions extensions.")

(defvar contrib-init-extension-directory
  (expand-file-name (concat contrib-config-directory "init-extension/"))
  "Contributions Extensions initialization.")

(defvar contrib-init-package-directory
  (expand-file-name (concat contrib-config-directory "init-package/"))
  "Contributions Package initialization.")

(add-to-list 'load-path contrib-config-directory)

;; Host configs

(defvar host-directory
  (expand-file-name (concat user-emacs-directory "host/" system-name "/"))
  "Host specific configurations")

;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------

;; Spacemacs setup

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

;; Contributions setup

(require 'co-funcs nil 'noerror)
(require 'co-macros nil 'noerror)
(require 'co-extensions nil 'noerror)
(require 'co-packages nil 'noerror)
;; install and initialize extensions and packages
;; pre extensions
(se/load-and-initialize-extensions co/pre-extensions
                                   contrib-extensions-directory
                                   contrib-init-extension-directory)
;; packages
(se/install-missing-packages co/packages)
(se/initialize-packages contrib-init-package-directory)
;; post extensions
(se/load-and-initialize-extensions co/post-extensions
                                   contrib-extensions-directory
                                   contrib-init-extension-directory)

;; User setup

;; load ~/.spacemacs file
;; store your personnal configuration in this file
;; you can override previous settings here
(require 'my-config (concat user-home-directory ".spacemacs"))

;; Host setup

;; host specific configuration in emacs_path/host/<hostname>
(defun load-config (directory)
  (progn (when (file-exists-p directory)
           (dolist (l (directory-files directory nil "^[^#].*el$"))
             (load (concat directory l))))))
(load-config host-directory)

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

