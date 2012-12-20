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
