(defvar lua-packages
  '(
    lua-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

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
      (evil-leader/set-key-for-mode 'lua-mode "ml" 'lua-send-buffer))))
