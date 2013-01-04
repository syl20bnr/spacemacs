;; evil -----------------------------------------------------------------------
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)

;; buffers --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "bh" 'switch-to-prev-buffer)
(key-chord-define evil-normal-state-map "bk" 'ido-kill-buffer)
(key-chord-define evil-normal-state-map "BK" 'delete-current-buffer-file)
(key-chord-define evil-normal-state-map "bl" 'switch-to-next-buffer)
(key-chord-define evil-normal-state-map "bo" 'kill-other-buffers)
(key-chord-define evil-normal-state-map "br" 'rename-current-buffer-file)
(key-chord-define evil-normal-state-map "bs" 'ido-switch-buffer)
;; elisp ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "ed"
                                     'elisp-slime-nav-find-elisp-thing-at-point)
(key-chord-define evil-normal-state-map "ev" 'eval-sexp)
(key-chord-define evil-normal-state-map "EV" 'eval-and-replace)
(key-chord-define evil-normal-state-map "EV" 'eval-and-replace)
;; emacs ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "em" 'smex)
;; files ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "fo" 'ido-find-file)
(key-chord-define evil-normal-state-map "fp" 'find-file-in-project)
;; fold-this ------------------------------------------------------------------
(key-chord-define evil-visual-state-map "fj" 'fold-this)
(key-chord-define evil-visual-state-map "FJ" 'fold-this-all)
(key-chord-define evil-normal-state-map "fk" 'fold-this-unfold-at-point)
(key-chord-define evil-normal-state-map "FK" 'fold-this-unfold-all)
;; git ------------------------------------------------------------------------
(key-chord-define evil-normal-state-map "gs" 'magit-status)
;; move-text ------------------------------------------------------------------
(key-chord-define evil-normal-state-map "vj" 'move-text-down)
(key-chord-define evil-visual-state-map "vj" 'move-text-down)
(key-chord-define evil-normal-state-map "vk" 'move-text-up)
(key-chord-define evil-visual-state-map "vk" 'move-text-up)
;; multiple-cursors -----------------------------------------------------------
(key-chord-define evil-visual-state-map "ma" 'mc/mark-all-like-this)
(key-chord-define evil-visual-state-map "mc" 'mac-mc-acquire-cursors-at-beginning)
(key-chord-define evil-normal-state-map "mj" 'mc/mark-next-lines)
(key-chord-define evil-normal-state-map "mk" 'mc/mark-previous-lines)
(key-chord-define evil-visual-state-map "ml" 'mc/mark-next-like-this)
(key-chord-define evil-visual-state-map "mm" 'mac-mc-edit-beginnings-of-lines-tb)
(key-chord-define evil-visual-state-map "MM" 'mac-mc-edit-ends-of-lines-tb)
(key-chord-define evil-visual-state-map "mn" 'mac-mc-edit-beginnings-of-lines-bt)
(key-chord-define evil-visual-state-map "MN" 'mac-mc-edit-ends-of-lines-bt)
;; errors ---------------------------------------------------------------------
(key-chord-define evil-normal-state-map "rh" 'previous-error)
(key-chord-define evil-normal-state-map "rl" 'next-error)
;; terminals ------------------------------------------------------------------
(key-chord-define evil-normal-state-map "te" 'eshell)
(key-chord-define evil-normal-state-map "th" 'multi-term-prev)
(key-chord-define evil-normal-state-map "tl" 'multi-term-next)
(key-chord-define evil-normal-state-map "tn" 'multi-term)
(key-chord-define evil-normal-state-map "tt" 'last-used-multi-term)
;; windows --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "wb" 'split-window-right)
(key-chord-define evil-normal-state-map "wc" 'delete-window)
(key-chord-define evil-normal-state-map "wh" 'evil-window-left)
(key-chord-define evil-normal-state-map "wj" 'evil-window-down)
(key-chord-define evil-normal-state-map "wk" 'evil-window-up)
(key-chord-define evil-normal-state-map "wl" 'evil-window-right)
(key-chord-define evil-normal-state-map "wm" 'toggle-maximize-buffer)
(key-chord-define evil-normal-state-map "wr" 'rotate-windows)
(key-chord-define evil-normal-state-map "wv" 'split-window-below)

(provide 'my-keychords)
