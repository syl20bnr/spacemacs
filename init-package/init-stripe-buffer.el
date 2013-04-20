(require 'stripe-buffer)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
