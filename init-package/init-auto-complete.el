(require 'auto-complete-config)
(setq ac-dwim t)
(ac-config-default)

(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-hook 'python-mode-hook 'ac-python-mode-setup)
