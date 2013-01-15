;; evil -----------------------------------------------------------------------
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map  "jk" 'evil-normal-state)
(key-chord-define evil-motion-state-map "jk" 'evil-normal-state)

;; applications ---------------------------------------------------------------
(key-chord-define evil-normal-state-map "ae" 'erc-start-or-switch)
(key-chord-define evil-normal-state-map "am" 'mu4e-up-to-date-status)
(key-chord-define evil-normal-state-map "at" 'twit)
;; buffers --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "bk" 'ido-kill-buffer)
(key-chord-define evil-normal-state-map "BK" 'delete-current-buffer-file)
(key-chord-define evil-normal-state-map "bn" 'switch-to-next-buffer)
(key-chord-define evil-normal-state-map "bo" 'kill-other-buffers)
(key-chord-define evil-normal-state-map "bp" 'switch-to-prev-buffer)
(key-chord-define evil-normal-state-map "br" 'rename-current-buffer-file)
(key-chord-define evil-normal-state-map "bs" 'ido-switch-buffer)
;; multiple-cursors -----------------------------------------------------------
(key-chord-define evil-visual-state-map "ca" 'mc/mark-all-like-this)
(key-chord-define evil-visual-state-map "cb" 'mac-mc-edit-beginnings-of-lines-tb)
(key-chord-define evil-visual-state-map "CB" 'mac-mc-edit-ends-of-lines-tb)
(key-chord-define evil-visual-state-map "cc" 'mac-mc-acquire-cursors-at-beginning)
(key-chord-define evil-visual-state-map "ce" 'mac-mc-edit-beginnings-of-lines-bt)
(key-chord-define evil-visual-state-map "CE" 'mac-mc-edit-ends-of-lines-bt)
(key-chord-define evil-normal-state-map "cj" 'mc/mark-next-lines)
(key-chord-define evil-visual-state-map "cn" 'mc/mark-next-like-this)
(key-chord-define evil-normal-state-map "cp" 'mc/mark-previous-lines)
;; elisp ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "ed" 'elisp-slime-nav-find-elisp-thing-at-point)
(key-chord-define evil-normal-state-map "ev" 'eval-sexp)
(key-chord-define evil-normal-state-map "EV" 'eval-and-replace)
;; emacs ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "em" 'smex)
;; expand region --------------------------------------------------------------
(key-chord-define evil-visual-state-map "re" 'er/expand-region)
(key-chord-define evil-visual-state-map "rr" 'er/contract-region)
;; files ----------------------------------------------------------------------
(key-chord-define evil-normal-state-map "fo" 'ido-find-file)
(key-chord-define evil-normal-state-map "fp" 'find-file-in-project)
(key-chord-define evil-normal-state-map "fs" 'sr-speedbar-toggle-and-select)
;; fold-this -g----------------------------------------------------------------
(key-chord-define evil-visual-state-map "fj" 'fold-this)
(key-chord-define evil-visual-state-map "FJ" 'fold-this-all)
(key-chord-define evil-normal-state-map "fk" 'fold-this-unfold-at-point)
(key-chord-define evil-normal-state-map "FK" 'fold-this-unfold-all)
;; git ------------------------------------------------------------------------
(key-chord-define evil-normal-state-map "gs" 'magit-status)
;; line numbers ---------------------------------------------------------------
(key-chord-define evil-normal-state-map "ln" 'global-linum-mode)
;; move-text ------------------------------------------------------------------
(key-chord-define evil-normal-state-map "vj" 'move-text-down)
(key-chord-define evil-visual-state-map "vj" 'move-text-down)
(key-chord-define evil-normal-state-map "vk" 'move-text-up)
(key-chord-define evil-visual-state-map "vk" 'move-text-up)
;; errors ---------------------------------------------------------------------
(key-chord-define evil-normal-state-map "rp" 'previous-error)
(key-chord-define evil-normal-state-map "rn" 'next-error)
;; terminals ------------------------------------------------------------------
(key-chord-define evil-normal-state-map "tc" 'multi-term)
(key-chord-define evil-normal-state-map "te" 'eshell)
(key-chord-define evil-normal-state-map "tn" 'multi-term-next)
(key-chord-define evil-normal-state-map "tp" 'multi-term-prev)
(key-chord-define evil-normal-state-map "tt" 'last-used-multi-term)
;; windows --------------------------------------------------------------------
(key-chord-define evil-normal-state-map "wc" 'delete-window)
(key-chord-define evil-normal-state-map "wd" 'toggle-current-window-dedication)
(key-chord-define evil-normal-state-map "wh" 'evil-window-left)
(key-chord-define evil-normal-state-map "wj" 'evil-window-down)
(key-chord-define evil-normal-state-map "wk" 'evil-window-up)
(key-chord-define evil-normal-state-map "wl" 'evil-window-right)
(key-chord-define evil-normal-state-map "wm" 'toggle-maximize-buffer)
(key-chord-define evil-normal-state-map "wn" 'evenly-split-window-right)
(key-chord-define evil-normal-state-map "WN" 'evenly-split-window-below)
(key-chord-define evil-normal-state-map "wo" 'other-window)
(key-chord-define evil-normal-state-map "wr" 'rotate-windows)
;; centered cursor ------------------------------------------------------------
(key-chord-define evil-normal-state-map "zz" 'global-centered-cursor-mode)

(provide 'my-keychords)
