(setq wakatime-packages '(wakatime-mode))

(defun wakatime/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer t
    :init (add-hook 'prog-mode-hook 'wakatime-mode)))
