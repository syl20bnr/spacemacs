(use-package auto-dictionary
  :disabled t
  :defer t
  :init
  (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1))))
