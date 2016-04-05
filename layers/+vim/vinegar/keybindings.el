(define-key evil-normal-state-map (kbd "-") 'dired-jump)

(add-hook 'dired-mode-hook 'vinegar/dired-setup)
