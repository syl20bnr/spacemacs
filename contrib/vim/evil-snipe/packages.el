(setq evil-snipe-packages '(evil-snipe))

(defun evil-snipe/init-evil-snipe ()
  (use-package evil-snipe
    :diminish evil-snipe-mode
    :init
    (progn
      (if (configuration-layer/package-usedp 'magit)
          (add-hook 'magit-status-mode-hook (lambda () (evil-snipe-mode -1))))
      (setq evil-snipe-scope 'whole-buffer
            evil-snipe-enable-highlight t
            evil-snipe-enable-incremental-highlight t
            evil-snipe-enable-half-cursor nil
            evil-snipe-show-prompt nil
            evil-snipe-smart-case t)
      (when evil-snipe-enable-alternate-f-and-t-behaviors
        (setq evil-snipe-repeat-scope 'whole-buffer
              evil-snipe-override-evil t))
      (global-evil-snipe-mode))))
