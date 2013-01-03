(setq evil-mode-line-format 'before)
(setq evil-want-C-u-scroll t)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("orange" box))
(setq evil-visual-state-cursor '("black" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-motion-state-cursor '("purple" box))

(evil-mode 1)

;; This is an endless debate and is just a matter of convention
;; I prefer to stay on the original character when leaving insert mode
;; (initiated with 'i').
(setq evil-move-cursor-back nil)

(add-hook 'evil-emacs-state-entry-hook 'evil-normal-state)
