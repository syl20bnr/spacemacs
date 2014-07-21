(use-package js2-mode
  :init
  (progn (add-hook 'js-mode-hook 'js2-minor-mode)
         (add-hook 'js2-mode-hook 'ac-js2-mode)))
