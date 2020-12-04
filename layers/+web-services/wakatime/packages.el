(setq wakatime-packages '(wakatime-mode))

(defun wakatime/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'wakatime-mode)
    :config
    (defun space-macs/wakatime-dashboard ()
      (interactive)
      (browse-url "https://wakatime.com/dashboard"))
    (space-macs/set-leader-keys
      "aW" 'space-macs/wakatime-dashboard)))


