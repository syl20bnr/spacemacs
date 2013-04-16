(require 'cl)

;; Locations ==================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "Emacs home directory.")

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

(defun load-user-config ()
  (progn (when (file-exists-p user-config-directory)
           (dolist (l (directory-files user-config-directory nil "^[^#].*el$"))
             (load (concat user-config-directory l))))))
(defun load-host-config ()
  (progn (when (file-exists-p host-directory)
           (dolist (l (directory-files host-directory nil "^[^#].*el$"))
             (load (concat host-directory l))))))

;; Setup ======================================================================

(load-user-config)
(require 'my-funcs)
(require 'my-macros)
(require 'pre-extensions)
(require 'packages)
(require 'post-extensions)
(require 'my-keybindings)
(require 'my-keychords)
(load-host-config)

;; Customized settings =====================================================
(require 'custom-settings)
