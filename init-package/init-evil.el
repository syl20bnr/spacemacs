(setq evil-mode-line-format 'before)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("orange" box))
(setq evil-visual-state-cursor '("black" box))
(setq evil-insert-state-cursor '("green3" box))
(setq evil-motion-state-cursor '("purple" box))

(evil-mode 1)

;; This is an endless debate and is just a matter of convention
;; I prefer to stay on the original character when leaving insert mode
;; (initiated with 'i').
(setq evil-move-cursor-back nil)

;; ;; evil-emacs-state is annoying, the following function and hook automatically
;; ;; switch to evil-insert-state whenever the evil-emacs-state is entered.
;; ;; It allows a more consistent navigation experience among all mode maps.
;; (defun evil-emacs-state-2-evil-insert-state ()
;;   (evil-insert-state)
;;   (remove-hook 'post-command-hook 'evil-emacs-state-2-evil-insert-state))
;; (add-hook 'evil-emacs-state-entry-hook
;;   (lambda ()
;;     (add-hook 'post-command-hook 'evil-emacs-state-2-evil-insert-state)))
;; ;; same thing for motion state but switch in normal mode instead
;; (defun evil-motion-state-2-evil-normal-state ()
;;   (evil-normal-state)
;;   (remove-hook 'post-command-hook 'evil-motion-state-2-evil-normal-state))
;; (add-hook 'evil-motion-state-entry-hook
;;   (lambda ()
;;     (add-hook 'post-command-hook 'evil-motion-state-2-evil-normal-state)))

