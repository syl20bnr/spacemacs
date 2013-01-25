;; expand region --------------------------------------------------------------
(key-chord-define evil-visual-state-map "re" 'er/expand-region)
(key-chord-define evil-visual-state-map "rr" 'er/contract-region)
;; evil -----------------------------------------------------------------------
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
(key-chord-define evil-visual-state-map "fd" 'evil-normal-state)
(key-chord-define evil-emacs-state-map  "fd" 'evil-normal-state)
(key-chord-define evil-motion-state-map "fd" 'evil-normal-state)

(provide 'my-keychords)
