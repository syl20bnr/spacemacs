(require 'cl)

(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "The user's home directory.")

(defvar user-projects-dir
  (expand-file-name (concat user-home-directory "Projects/"))
  "The directory containing the user's checked out source code.")

(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "The user's Dropbox root directory.")

(add-to-list 'load-path user-emacs-directory)

(require 'my-funcs)

;;number colon mode
(global-linum-mode t)
;;no tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; whitespace-mode
(setq-default show-trailing-whitespace t)
;; Inhibit startup message
(setq inhibit-startup-screen t)
;; Cursor, please do not blink
(blink-cursor-mode nil)
;; Do not make backup files
(setq make-backup-files nil)
;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)
;; Show column number in mode line
(setq column-number-mode t)
;; When point is on paranthesis, highlight the matching one
(show-paren-mode t)
;; auto-save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Config files
(progn
  (setq user-emacs-config-dir (concat user-emacs-directory "config/"))
  (when (file-exists-p user-emacs-config-dir)
    (dolist (l (directory-files user-emacs-config-dir nil "^[^#].*el$"))
      (load (concat user-emacs-config-dir l)))))

(require 'my-packages)
(require 'my-keybindings)
