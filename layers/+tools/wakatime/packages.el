(setq wakatime-packages '(wakatime-mode))

(defun wakatime/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'wakatime-mode)
    :config
    (defun spacemacs/wakatime-dashboard ()
      (interactive)
      (browse-url "wakatime.com/dashboard"))
    (evil-leader/set-key
      "aW" 'spacemacs/wakatime-dashboard)))
