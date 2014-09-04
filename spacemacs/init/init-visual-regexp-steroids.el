(use-package visual-regexp-steroids
  :defer t
  :init
  (evil-leader/set-key
    "rR" 'vr/query-replace
    "rr" 'vr/replace))

