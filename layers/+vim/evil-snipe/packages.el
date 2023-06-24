(setq evil-snipe-packages
      '(
        evil-snipe
        magit
        ranger
        ))

(defun evil-snipe/init-evil-snipe ()
  (use-package evil-snipe
    :init
    (setq evil-snipe-scope 'whole-buffer
          evil-snipe-enable-highlight t
          evil-snipe-enable-incremental-highlight t
          evil-snipe-auto-disable-substitute t
          evil-snipe-show-prompt nil
          evil-snipe-smart-case t)
    :config
    (spacemacs|hide-lighter evil-snipe-local-mode)
    (evil-snipe-mode 1)
    (when  evil-snipe-enable-alternate-f-and-t-behaviors
      (setq evil-snipe-repeat-scope 'whole-buffer)
      (evil-snipe-override-mode 1))))

(defun evil-snipe/post-init-magit ()
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)
  (when evil-snipe-enable-alternate-f-and-t-behaviors
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)))

(defun evil-snipe/post-init-ranger ()
  (add-hook 'ranger-mode-hook 'turn-off-evil-snipe-mode)
  (when evil-snipe-enable-alternate-f-and-t-behaviors
    (add-hook 'ranger-mode-hook 'turn-off-evil-snipe-override-mode)))
