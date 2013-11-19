(use-package ace-jump-mode
  :commands  ace-jump-mode
  :config
  (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
  )
