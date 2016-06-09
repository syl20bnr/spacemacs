(setq lua-packages
  '(
    company
    flycheck
    lua-mode
    ))

(defun lua/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'lua-mode))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :init
    (progn
      (setq lua-indent-level 2
            lua-indent-string-contents t)
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode
        "d" 'lua-search-documentation
        "sb" 'lua-send-buffer
        "sf" 'lua-send-defun
        "sl" 'lua-send-current-line
        "sr" 'lua-send-region))))

(defun lua/post-init-company ()
  (add-hook 'lua-mode-hook 'company-mode))
