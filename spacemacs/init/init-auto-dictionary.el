(use-package auto-dictionary
  :disabled t
  :defer t
  :init
  (progn 
    (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))
    (evil-leader/set-key
      "sd" 'adict-change-dictionary)))
