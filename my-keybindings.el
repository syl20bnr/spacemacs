
;; First unset shortcuts which shadow evil leader =============================
(eval-after-load "compile"
  (define-key compilation-mode-map (kbd "SPC") nil))

;; Regular shortcuts ==========================================================

;; evil -----------------------------------------------------------------------
;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
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

;; evil-leader shortcuts ======================================================

;; M-x ------------------------------------------------------------------------
(evil-leader/set-key ":" 'smex)
;; ace-jump -------------------------------------------------------------------
(evil-leader/set-key "SPC" 'ace-jump-mode)
;; applications ---------------------------------------------------------------
(evil-leader/set-key "ae" 'erc-start-or-switch)
(evil-leader/set-key "am" 'mu4e-up-to-date-status)
(evil-leader/set-key "at" 'twit)
;; buffers --------------------------------------------------------------------
(evil-leader/set-key "bk" 'ido-kill-buffer)
(evil-leader/set-key "BK" 'delete-current-buffer-file)
(evil-leader/set-key "bn" 'switch-to-next-buffer)
(evil-leader/set-key "bo" 'kill-other-buffers)
(evil-leader/set-key "bp" 'switch-to-prev-buffer)
(evil-leader/set-key "br" 'rename-current-buffer-file)
(evil-leader/set-key "bs" 'ido-switch-buffer)
;; multiple-cursors -----------------------------------------------------------
;; (evil-leader/set-key "ca" 'mc/mark-all-like-this)
;; (evil-leader/set-key "cb" 'mac-mc-edit-beginnings-of-lines-tb)
;; (evil-leader/set-key "CB" 'mac-mc-edit-ends-of-lines-tb)
;; (evil-leader/set-key "cc" 'mac-mc-acquire-cursors-at-beginning)
;; (evil-leader/set-key "ce" 'mac-mc-edit-beginnings-of-lines-bt)
;; (evil-leader/set-key "CE" 'mac-mc-edit-ends-of-lines-bt)
;; (evil-leader/set-key "cj" 'mc/mark-next-lines)
;; (evil-leader/set-key "cn" 'mc/mark-next-like-this)
;; (evil-leader/set-key "cp" 'mc/mark-previous-lines)
;; files ----------------------------------------------------------------------
(evil-leader/set-key "fo" 'ido-find-file)
(evil-leader/set-key "fp" 'find-file-in-project)
(evil-leader/set-key "fs" 'sr-speedbar-toggle-and-select)
;; fold-this ------------------------------------------------------------------
(evil-leader/set-key "fj" 'fold-this)
(evil-leader/set-key "FJ" 'fold-this-all)
(evil-leader/set-key "fk" 'fold-this-unfold-at-point)
(evil-leader/set-key "FK" 'fold-this-unfold-all)
;; git ------------------------------------------------------------------------
(evil-leader/set-key "gs" 'magit-status)
;; auto-highlight-symbol ------------------------------------------------------
(evil-leader/set-key "he" 'ahs-edit-mode)
(evil-leader/set-key "hh" 'auto-highlight-symbol-mode)
(evil-leader/set-key "hn" 'ahs-forward)
(evil-leader/set-key "hp" 'ahs-backward)
;; line numbers ---------------------------------------------------------------
(evil-leader/set-key "ln" 'global-linum-mode)
;; move-text ------------------------------------------------------------------
(evil-leader/set-key "mj" 'move-text-down)
(evil-leader/set-key "mk" 'move-text-up)
;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key "nr" 'narrow-to-region)
(evil-leader/set-key "np" 'narrow-to-page)
(evil-leader/set-key "nf" 'narrow-to-defun)
(evil-leader/set-key "nw" 'widen)
;; projectile -----------------------------------------------------------------
(evil-leader/set-key "pb" 'projectile-switch-to-buffer)
(evil-leader/set-key "pC" 'projectile-invalidate-cache)
(evil-leader/set-key "pd" 'projectile-dired)
(evil-leader/set-key "pf" 'projectile-recentf)
(evil-leader/set-key "pF" 'projectile-find-file)
(evil-leader/set-key "pk" 'projectile-kill-buffers)
(evil-leader/set-key "pg" 'projectile-grep)
(evil-leader/set-key "po" 'projectile-multi-occur)
(evil-leader/set-key "pr" 'projectile-replace)
;; errors ---------------------------------------------------------------------
(evil-leader/set-key "rp" 'previous-error)
(evil-leader/set-key "rn" 'next-error)
;; terminals ------------------------------------------------------------------
(evil-leader/set-key "tc" 'multi-term)
(evil-leader/set-key "te" 'eshell)
(evil-leader/set-key "tn" 'multi-term-next)
(evil-leader/set-key "tp" 'multi-term-prev)
(evil-leader/set-key "tt" 'last-used-multi-term)
;; windows --------------------------------------------------------------------
(evil-leader/set-key "wb" 'evenly-split-window-right)
(evil-leader/set-key "wc" 'delete-window)
(evil-leader/set-key "wd" 'toggle-current-window-dedication)
(evil-leader/set-key "wh" 'evil-window-left)
(evil-leader/set-key "wj" 'evil-window-down)
(evil-leader/set-key "wk" 'evil-window-up)
(evil-leader/set-key "wl" 'evil-window-right)
(evil-leader/set-key "wm" 'toggle-maximize-buffer)
(evil-leader/set-key "wr" 'rotate-windows)
(evil-leader/set-key "wv" 'evenly-split-window-below)
(evil-leader/set-key "ww" 'other-window)
;; centered cursor ------------------------------------------------------------
(evil-leader/set-key "zz" 'global-centered-cursor-mode)

(provide 'my-keybindings)
