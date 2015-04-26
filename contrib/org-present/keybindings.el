(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)
            (org-present-hide-cursor)
            (org-present-read-only)
            (define-key evil-normal-state-local-map (kbd "h") 'org-present-prev)
            (define-key evil-normal-state-local-map (kbd "l") 'org-present-next)
            (define-key evil-normal-state-local-map (kbd "q") 'org-present-quit)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)
            (org-present-show-cursor)
            (org-present-read-write)
            (define-key evil-normal-state-local-map (kbd "h") nil)
            (define-key evil-normal-state-local-map (kbd "l") nil)
            (define-key evil-normal-state-local-map (kbd "q") nil)))
