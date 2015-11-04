(setq evil-snipe-packages '(evil-snipe))

(defun evil-snipe/init-evil-snipe ()
  (use-package evil-snipe
    :diminish evil-snipe-local-mode
    :init
    (setq evil-snipe-scope 'whole-buffer
          evil-snipe-enable-highlight t
          evil-snipe-enable-incremental-highlight t
          evil-snipe-auto-disable-substitute t
          evil-snipe-show-prompt nil
          evil-snipe-smart-case t)
    :config
    (progn
      (if evil-snipe-enable-alternate-f-and-t-behaviors
          (progn
            (setq evil-snipe-repeat-scope 'whole-buffer)
            (evil-snipe-override-mode 1)
            (when (configuration-layer/layer-usedp 'git)
              (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
              (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)))
        (evil-snipe-mode 1)
        (when (configuration-layer/layer-usedp 'git)
          (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
          (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode))))))
