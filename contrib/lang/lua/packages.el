(defvar lua-packages
  '(
    flycheck
    lua-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun lua/post-init-flycheck ()
  (add-hook 'lua-mode-hook 'flycheck-mode))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :config
    (progn
      (setq lua-indent-level 2
            lua-indent-string-contents t)
      (evil-leader/set-key-for-mode 'lua-mode "md" 'lua-search-documentation)
      (evil-leader/set-key-for-mode 'lua-mode "msb" 'lua-send-buffer)
      (evil-leader/set-key-for-mode 'lua-mode "msf" 'lua-send-defun)
      (evil-leader/set-key-for-mode 'lua-mode "msl" 'lua-send-current-line)
      (evil-leader/set-key-for-mode 'lua-mode "msr" 'lua-send-region))))
