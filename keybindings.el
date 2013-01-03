;; emacs ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "em" 'smex)
;; evil -----------------------------------------------------------------------
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;; ace-jump -------------------------------------------------------------------
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
;; elisp ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "ed"
                                     'elisp-slime-nav-find-elisp-thing-at-point)
(key-chord-define evil-normal-state-map "ev" 'eval-sexp)
(key-chord-define evil-normal-state-map "EV" 'eval-and-replace)
(key-chord-define evil-normal-state-map "EV" 'eval-and-replace)
;; windows --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "wh" 'evil-window-left)
(key-chord-define evil-normal-state-map "wj" 'evil-window-down)
(key-chord-define evil-normal-state-map "wk" 'evil-window-up)
(key-chord-define evil-normal-state-map "wl" 'evil-window-right)
(key-chord-define evil-normal-state-map "wm" 'toggle-maximize-buffer)
(key-chord-define evil-normal-state-map "wr" 'rotate-windows)
;; buffers --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "bk" 'ido-kill-buffer)
(key-chord-define evil-normal-state-map "BK" 'delete-current-buffer-file)
(key-chord-define evil-normal-state-map "bo" 'kill-other-buffers)
(key-chord-define evil-normal-state-map "br" 'rename-current-buffer-file)
(key-chord-define evil-normal-state-map "bs" 'ido-switch-buffer)
;; files ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "fo" 'ido-find-file)
(key-chord-define evil-normal-state-map "fp" 'find-file-in-project)
;; terminals ------------------------------------------------------------------
(key-chord-define evil-normal-state-map "te" 'eshell)
(key-chord-define evil-normal-state-map "th" 'multi-term-prev)
(key-chord-define evil-normal-state-map "tl" 'multi-term-next)
(key-chord-define evil-normal-state-map "tn" 'multi-term)
(key-chord-define evil-normal-state-map "tt" 'last-used-multi-term)
;; git ------------------------------------------------------------------------
(key-chord-define evil-normal-state-map "gs" 'magit-status)
;; errors ---------------------------------------------------------------------
(key-chord-define evil-normal-state-map "rh" 'previous-error)
(key-chord-define evil-normal-state-map "rl" 'next-error)

(provide 'keybindings)
