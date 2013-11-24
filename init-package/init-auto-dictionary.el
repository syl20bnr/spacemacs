(use-package auto-dictionary
  :defer t
  :init
  (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1))))
