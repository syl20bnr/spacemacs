(setq evil-snipe-packages '(evil-snipe))

(defun evil-snipe/init-evil-snipe ()
  (use-package evil-snipe
    :diminish evil-snipe-mode
    :init
    (progn
      (setq evil-snipe-scope 'whole-buffer
            evil-snipe-enable-highlight t
            evil-snipe-enable-incremental-highlight t
            evil-snipe-enable-half-cursor nil
            evil-snipe-show-prompt nil
            evil-snipe-smart-case t)
      (when evil-snipe-enable-alternate-f-and-t-behaviors
        (setq evil-snipe-repeat-scope 'whole-buffer
              evil-snipe-override-evil t))
      (add-hook 'prog-mode-hook 'evil-snipe-mode)
      (add-hook 'text-mode-hook 'evil-snipe-mode))))
