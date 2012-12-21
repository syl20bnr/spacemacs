(autoload 'autopair-global-mode "autopair" nil t)
  (autopair-global-mode)
  (add-hook 'lisp-mode-hook #'(lambda () (setq autopair-dont-activate t)))
