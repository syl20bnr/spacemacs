(require 'expand-region)
(require 'evil)
(key-chord-define evil-visual-state-map "kl" 'er/expand-region)
(key-chord-define evil-visual-state-map "hj" 'er/contract-region)
