;; Key Bindings ================================================================
(global-set-key (kbd "C-x x") 'kill-this-buffer)
(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-c\C-c" 'syl-python-compile)))

(provide 'keybindings)
