;; evil -----------------------------------------------------------------------
;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; ace-jump -------------------------------------------------------------------
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
;; auto-complete --------------------------------------------------------------
(global-set-key (kbd "M-SPC") 'ac-fuzzy-complete)
(define-key ac-complete-mode-map (kbd "C-j") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-k") 'ac-previous)
(define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
;; mu4e -----------------------------------------------------------------------
(define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)
;; multiple-cursors -----------------------------------------------------------
(global-set-key (kbd "C-M-m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-M-m e") 'mc/edit-ends-of-lines)

(provide 'my-keybindings)
