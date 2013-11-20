(use-package jedi
  :defer t
  :init
  (setq jedi:setup-keys t)
  :config
  (progn
    (setq jedi:complete-on-dot t)
    (add-hook 'python-mode-hook 'jedi:setup)))
