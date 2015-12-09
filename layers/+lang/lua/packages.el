(setq lua-packages
  '(
    company
    flycheck
    lua-mode
    ))

(defun lua/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'lua-mode-hook))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :config
    (progn
      (setq lua-indent-level 2
            lua-indent-string-contents t)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode "d" 'lua-search-documentation)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sb" 'lua-send-buffer)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sf" 'lua-send-defun)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sl" 'lua-send-current-line)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sr" 'lua-send-region))))

(defun lua/post-init-company ()
  (add-hook 'lua-mode-hook 'company-mode))
