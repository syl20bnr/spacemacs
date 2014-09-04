(use-package move-text
  :defer t
  :init
  (evil-leader/set-key
    "xmj" 'move-text-down
    "xmk" 'move-text-up)
)
