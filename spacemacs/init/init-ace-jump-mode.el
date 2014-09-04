(use-package ace-jump-mode
  :defer t
  :init
  (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
  :config
  (progn 
    ;; ace-jump quick access
    (define-key evil-normal-state-map "," 'ace-jump-mode)
    (define-key evil-normal-state-map (kbd "C-,") 'ace-jump-word-mode)
    (evil-leader/set-key "," 'ace-jump-mode-pop-mark)))
