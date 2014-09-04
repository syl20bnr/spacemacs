(use-package helm
  :defer t
  :init
  (progn 
    (add-to-hooks 'helm-mode '(erlang-mode-hook
                               markdown-mode-hook
                               org-mode-hook
                               prog-mode-hook
                               ))
    (evil-leader/set-key
      ":"   'helm-M-x
      "bs"  'helm-mini
      "kil"  'helm-how-kill-ring
      "hg"  'helm-bookmarks))
  :config
  (progn
    ;; helm keybindings tweaks
    ;; use home row keys
    ;; the original hot key of helm-keyboard-quit is "C-g"
    (define-key helm-map (kbd "f")
      (lambda () (interactive) (fd-trigger 'helm-keyboard-quit)))
    ;; helm navigation on hjkl
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-l") 'helm-previous-source))
)
