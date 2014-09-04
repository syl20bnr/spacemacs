(use-package git-messenger
  :defer t
  :init
  (evil-leader/set-key
    "gm" 'git-messenger:popup-message))
