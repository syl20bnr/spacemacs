(use-package stripe-buffer
  :disabled t
  :init
  (progn
    (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
    (add-hook 'dired-mode-hook 'stripe-listify-buffer)
    (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)))
