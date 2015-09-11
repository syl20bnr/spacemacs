(setq evil-snipe-packages '(evil-snipe))

(defun evil-snipe/init-evil-snipe ()
  (use-package evil-snipe
    :diminish evil-snipe-mode
    :init
    (setq evil-snipe-scope 'whole-buffer
          evil-snipe-enable-highlight t
          evil-snipe-enable-incremental-highlight t
          evil-snipe-auto-disable-substitute t
          evil-snipe-show-prompt nil
          evil-snipe-smart-case t)
    :config
    (progn
      (evil-snipe-mode 1)
      (when evil-snipe-enable-alternate-f-and-t-behaviors
        (setq evil-snipe-repeat-scope 'whole-buffer)
        (evil-snipe-override-mode 1)))))
