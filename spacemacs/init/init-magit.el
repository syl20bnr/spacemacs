(use-package magit
  :defer t
  :init
  (evil-leader/set-key "gs" 'magit-status)
  :config
  (progn
    ;; full screen magit-status
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
      "K" 'magit-discard-item
      "L" 'magit-key-mode-popup-logging)
    (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-status-mode-map 'emacs
      "f" 'magit-key-mode-popup-fetching
      "K" 'magit-discard-item
      "l" 'magit-key-mode-popup-logging
      "h" 'magit-toggle-diff-refine-hunk)

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))

