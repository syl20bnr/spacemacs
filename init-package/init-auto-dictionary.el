(use-package auto-dictionary
  :defer t
  :config
  (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1))))
