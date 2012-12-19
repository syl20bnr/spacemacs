(require 'cl)

;; Locations ===================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "The user's home directory.")

(defvar user-custom-modes-dir
  (expand-file-name (concat user-emacs-directory "custom-modes/"))
  "The directory containing the user's custom modes.")

(add-to-list 'load-path user-emacs-directory)

;; Global config ===============================================================
;; font size
(set-face-attribute 'default nil :height 110)
;; number colon mode
(global-linum-mode t)
;; full screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)
;; no blink
(blink-cursor-mode (- (*) (*) (*)))
;; save session
 (desktop-save-mode 1)
;; tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
;; whitespace-mode
(setq-default show-trailing-whitespace nil)
;; Inhibit startup message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")
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
(setq redisplay-dont-pause t)
;; use only spaces and no tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
;; move focus to newly split window
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; Config files ================================================================
(progn
  (setq user-emacs-config-dir (concat user-emacs-directory "config/"))
  (when (file-exists-p user-emacs-config-dir)
    (dolist (l (directory-files user-emacs-config-dir nil "^[^#].*el$"))
      (load (concat user-emacs-config-dir l)))))

(require 'my-funcs)
(require 'my-packages)
(require 'my-keybindings)

;; Python ======================================================================
;; clone git repository https://github.com/gabrielelanaro/emacs-for-python
;; =============================================================================
(setq ropemacs-global-prefix "C-x /") ;; avoid conflict with p4 global prefix
(load-file (concat user-emacs-directory "emacs-for-python/epy-init.el"))
(require 'epy-setup)
(require 'epy-python)
(require 'epy-completion)
(require 'epy-editing)
(require 'epy-bindings)
(require 'epy-nose)
(epy-setup-checker "pyflakes %f")
(epy-setup-ipython)
;; line hightlighting
(global-hl-line-mode t)
(set-face-background 'hl-line "#073642")
;; identation highlighting
;; (require 'highlight-indentation)
;; (add-hook 'python-mode-hook 'highlight-indentation)
;; disable auto-pairing
;;(setq skeleton-pair nil)
(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-c\C-c" 'syl-python-compile)))

;; Custom modes ================================================================
(load-file (concat user-custom-modes-dir "heartbeat-cursor.el"))
