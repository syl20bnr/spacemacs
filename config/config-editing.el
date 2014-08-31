;; Global ---------------------------------------------------------------------
;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)
;; font size
;;(set-face-attribute 'default nil :height 110)
;; whitespace-mode
(setq-default show-trailing-whitespace nil)
;; When point is on paranthesis, highlight the matching one
(show-paren-mode t)
;; use only spaces and no tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
;; Text -----------------------------------------------------------------------
(setq longlines-show-hard-newlines t)
