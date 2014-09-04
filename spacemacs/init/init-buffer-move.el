(use-package buffer-mode
  :defer t
  :init
  (evil-leader/set-key
    "bmh" 'buf-move-left
    "bmj" 'buf-move-down
    "bmk" 'buf-move-up
    "bml" 'buf-move-right))
