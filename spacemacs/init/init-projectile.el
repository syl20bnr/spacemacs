(use-package projectile
  :commands (projectile-switch-to-buffer
             projectile-invalidate-cache
             projectile-dired
             projectile-find-file
             helm-projectile
             projectile-kill-buffers
             projectile-grep
             projectile-replace)
  :init
  (evil-leader/set-key
    "pb" 'projectile-switch-to-buffer
    "pC" 'projectile-invalidate-cache
    "pd" 'projectile-dired
    "pF" 'projectile-find-file
    "pf" 'helm-projectile
    "pk" 'projectile-kill-buffers
    "pg" 'projectile-grep
    "pr" 'projectile-replace))
