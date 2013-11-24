(use-package flyspell
  :defer t
  :init
  (progn
    (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
    (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))))
