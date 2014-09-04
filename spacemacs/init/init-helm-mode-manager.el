(use-package helm-mode-manager
  :defer t
  :init
  (evil-leader/set-key
    "hM"    'helm-switch-major-mode
    "hm"    'helm-disable-minor-mode
    "h C-m" 'helm-enable-minor-mode))
