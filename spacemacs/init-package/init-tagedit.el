(use-package tagedit
  :defer t
  :config
  (progn
    (tagedit-add-experimental-features)
    (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

