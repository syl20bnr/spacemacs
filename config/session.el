;; Inhibit startup message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")
;; save session
(desktop-save-mode 1)
;; auto-save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq redisplay-dont-pause t)
;; Do not make backup files
(setq make-backup-files nil)
