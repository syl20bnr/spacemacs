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

;; evil-emacs-state is annoying, the following function and hook automatically
;; switch back to evil-normal-state whenever the evil-emacs-state is entered.
;; It allows a more consistent navigation experience among all mode maps.
;; To enter special commands of custom mode maps, just enter the insert mode :-)
(defun evil-emacs-state-2-evil-normal-state ()
  (evil-normal-state)
  (remove-hook 'post-command-hook 'evil-emacs-state-2-evil-normal-state))
(add-hook 'evil-emacs-state-entry-hook
  (lambda ()
    (add-hook 'post-command-hook 'evil-emacs-state-2-evil-normal-state)))
