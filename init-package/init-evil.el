(setq evil-mode-line-format 'before)
(setq evil-want-C-u-scroll t)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("orange" box))
(setq evil-visual-state-cursor '("black" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-motion-state-cursor '("purple" box))

(evil-mode 1)
;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;;Exit insert mode by pressing j and then k quickly
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
;; This is an endless debate and is just a matter of convention
;; I prefer to stay on the original character when leaving insert mode
;; (initiated with 'i')
(setq evil-move-cursor-back nil)
