(require 'cl)

;; Locations ==================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "Emacs home directory.")

(defvar user-pre-directory
  (expand-file-name (concat user-emacs-directory "pre/"))
  "Pre-configuration scripts.")

(defvar user-post-directory
  (expand-file-name (concat user-emacs-directory "post/"))
  "Post-configuration setup.")

(defvar user-extensions-directory
  (expand-file-name (concat user-emacs-directory "extensions/"))
  "Additional extensions.")

(defvar host-directory
  (expand-file-name (concat user-emacs-directory "host/" system-name "/"))
  "Host specific configurations")

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-extensions-directory)

;; Pre-config =================================================================
(progn (when (file-exists-p user-pre-directory)
    (dolist (l (directory-files user-pre-directory nil "^[^#].*el$"))
      (load (concat user-pre-directory l)))))

;; Package setup ==============================================================
(require 'funcs)
(require 'funcs-virga)
(require 'packages)
(require 'keybindings)

;; Post-config ================================================================
(progn (when (file-exists-p user-post-directory)
    (dolist (l (directory-files user-post-directory nil "^[^#].*el$"))
      (load (concat user-post-directory l)))))

;; Host config ================================================================
(progn (when (file-exists-p host-directory)
    (dolist (l (directory-files host-directory nil "^[^#].*el$"))
      (load (concat host-directory l)))))

;; TODO refactor auto-loading functions for configuration files
