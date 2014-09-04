(use-package smeargle
  :defer t
  :init
  (evil-leader/set-key
    "gcC" 'smeargle-clear
    "gcc" 'smeargle-commits
    "gct" 'smeargle))
