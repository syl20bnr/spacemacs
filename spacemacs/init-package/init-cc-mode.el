(use-package cc-mode
  :defer t
  :config
  (progn
    (add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state t)))
    (add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state t)))))
