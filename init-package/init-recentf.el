(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (progn
    (setq recentf-exclude '("~/.emacs.d/.recentf"))
    (setq recentf-save-file (concat user-emacs-directory "/.recentf"))
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 'never)
    (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))))



