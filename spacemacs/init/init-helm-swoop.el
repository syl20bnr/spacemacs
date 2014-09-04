(use-package helm-swoop
  :defer t
  :init
  (evil-leader/set-key
    "hS"    'helm-multi-swoop
    "hs"    'helm-swoop
    "h C-s" 'helm-multi-swoop-all))
