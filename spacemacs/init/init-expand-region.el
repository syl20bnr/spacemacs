(use-package expand-region
  :defer t
  :init
  (evil-leader/set-key "v" 'er/expand-region)
  :config
  (custom-set-variables
   '(expand-region-contract-fast-key "V")
   '(expand-region-reset-fast-key "r")))
