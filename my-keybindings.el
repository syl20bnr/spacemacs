;; Regular shortcuts ==========================================================

;; emacs ----------------------------------------------------------------------
;; switch meta for super in order to play nicely with i3wm which I use with
;; alt modifier.
(setq x-super-keysym 'meta)
(setq x-meta-keysym 'super)
;; simple and more consistent keyboard quit key bindings
;; thanks to Bin Chen for the idea (http://blog.binchen.org/?p=735)
(global-set-key (kbd "f")
  (lambda () (interactive) (fd-trigger 'keyboard-quit)))
(define-key minibuffer-local-map (kbd "f")
  (lambda () (interactive) (fd-trigger 'abort-recursive-edit)))
;; the original hot key of helm-keyboard-quit is "C-g"
(define-key helm-map (kbd "f")
  (lambda () (interactive) (fd-trigger 'helm-keyboard-quit)))
;; evil -----------------------------------------------------------------------
;; easier toggle for emacs-state
(evil-set-toggle-key "s-`")
;; returns to normal mode
(define-key evil-insert-state-map "f"
  (lambda () (interactive) (fd-trigger 'evil-normal-state)))
(define-key evil-visual-state-map "f"
  (lambda () (interactive) (fd-trigger 'evil-exit-visual-state)))
(define-key evil-emacs-state-map "f"
  (lambda () (interactive) (fd-trigger 'evil-normal-state)))
(define-key evil-motion-state-map "f"
  (lambda () (interactive) (fd-trigger 'evil-normal-state)))
;; set back go to char key bindings in normal modes
(define-key evil-normal-state-map   "f" 'evil-find-char)
(define-key evil-operator-state-map "f" 'evil-find-char)
;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
;; ace-jump quick access ------------------------------------------------------
;; I want a very quick trigger, evil-leader is too slow for this
;; pop mark is performed using the evil-leader + ,
(define-key evil-normal-state-map "," 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-,") 'ace-jump-word-mode)
;; helm tweaks ----------------------------------------------------------------
;; use home row keys
(eval-after-load "helm"
  '(progn
     (define-key helm-map (kbd "C-j") 'helm-next-line)
     (define-key helm-map (kbd "C-k") 'helm-previous-line)
     (define-key helm-map (kbd "C-h") 'helm-next-source)
     (define-key helm-map (kbd "C-l") 'helm-previous-source)))
;; quick navigation -----------------------------------------------------------
(define-key evil-normal-state-map (kbd "L")
  (lambda () (interactive)
    (evil-window-bottom)
    (evil-scroll-line-to-center nil)))
(define-key evil-normal-state-map (kbd "H")
  (lambda () (interactive)
    (evil-window-top)
    (evil-scroll-line-to-center nil)))
;; org ------------------------------------------------------------------------
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)))

;; evil-leader shortcuts ======================================================

;; M-x ------------------------------------------------------------------------
(evil-leader/set-key ":" 'execute-extended-command)
;; Key bindings help ----------------------------------------------------------
(evil-leader/set-key "?" 'helm-descbinds)
;; ace-jump -------------------------------------------------------------------
(evil-leader/set-key "," 'ace-jump-mode-pop-mark)
;; shell command  -------------------------------------------------------------
(evil-leader/set-key "S" 'shell-command)
;; magic wand  ----------------------------------------------------------------
(evil-leader/set-key "RET" 'wand:execute)
;; switch back and forth between two last buffers -----------------------------
(evil-leader/set-key "TAB"
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t))))
;; switch window by number ----------------------------------------------------
(evil-leader/set-key
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9)
;; applications ---------------------------------------------------------------
(evil-leader/set-key
  "ac"  'calc-dispatch
  "ad"  'dired
  "ae"  'erc-start-or-switch
  "ag"  'magit-status
  "ap"  'proced
  "ase" 'eshell
  "asi" 'shell
  "ast" 'multi-term
  "at"  'twit
  "au"  'undo-tree-visualize
  "ay"  'helm-c-yas-complete)
;; buffers --------------------------------------------------------------------
(evil-leader/set-key
  "bd"  'delete-current-buffer-file
  "bK"  'kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'kill-matching-buffers-rudely
  "bmh" 'buf-move-left
  "bmj" 'buf-move-down
  "bmk" 'buf-move-up
  "bml" 'buf-move-right
  "bn"  'switch-to-next-buffer
  "bp"  'switch-to-prev-buffer
  "br"  'rename-current-buffer-file
  "bs"  'helm-mini
  "bw"  'toggle-read-only)
;; Cycling settings -----------------------------------------------------------
(evil-leader/set-key "ct" 'cycle-my-theme)
;; errors ---------------------------------------------------------------------
(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)
;; editors --------------------------------------------------------------------
(evil-leader/set-key "eds" 'string-edit-at-point)
;; find -----------------------------------------------------------------------
(evil-leader/set-key
  "ff" 'ido-find-file
  "fg" 'rgrep)
;; file -----------------------------------------------------------------------
(evil-leader/set-key
  "fi" 'find-user-init-file
  "fS" 'evil-write-all
  "fs" 'evil-write
  "fy" 'camdez/show-buffer-file-name)
;; flycheck -------------------------------------------------------------------
(evil-leader/set-key
  "fl" 'flycheck-list-errors
  "fn" 'flycheck-next-error
  "fp" 'flycheck-previous-error)
;; show -----------------------------------------------------------------------
(evil-leader/set-key
  "gm" 'git-messenger:popup-message
  "gcC" 'smeargle-clear
  "gcc" 'smeargle-commits
  "gct" 'smeargle
  "gs" 'magit-status)
;; auto-highlight-symbol ------------------------------------------------------
(evil-leader/set-key
  "he" 'ahs-edit-mode
  "hn" 'ahs-forward
  "hp" 'ahs-backward)
;; helm -----------------------------------------------------------------------
(evil-leader/set-key
  "h:"    'helm-helm-commands
  "hc"    'helm-css-scss
  "hg"    'helm-bookmarks
  "hk"    'helm-make
  "hM"    'helm-switch-major-mode
  "hm"    'helm-disable-minor-mode
  "h C-m" 'helm-enable-minor-mode
  "hS"    'helm-multi-swoop
  "hs"    'helm-swoop
  "h C-s" 'helm-multi-swoop-all
  "ht"    'helm-themes)
;; insert stuff ---------------------------------------------------------------
(evil-leader/set-key
  "ij"  (lambda (count)
          "Insert a new line below with no identation."
          (interactive "p")
          (save-excursion
            (evil-move-end-of-line)
            (while (> count 0)
              (insert "\n")
              (setq count (1- count)))))
  "ik" 'evil-insert-line-above)
;; format ---------------------------------------------------------------------
(evil-leader/set-key
  "ji" (lambda () (interactive) (join-line 1) (sp-newline))
  "jj" 'sp-newline
  "jk" 'evil-join)
;; navigation -----------------------------------------------------------------
(evil-leader/set-key
  "jh" (lambda () (interactive) (push-mark (point)) (evil-beginning-of-line))
  "jl" (lambda () (interactive) (push-mark (point)) (evil-end-of-line)))
;; bookmarks ------------------------------------------------------------------
(evil-leader/set-key
  "kd" 'bookmark-delete
  "kg" 'bookmark-jump
  "kr" 'bookmark-rename
  "ks" 'bookmark-set)
;; Compilation ----------------------------------------------------------------
(evil-leader/set-key "cc" 'compile)
;; match it  ------------------------------------------------------------------
(evil-leader/set-key
  "Md" 'evilmi-delete-items
  "Mi" 'evilmi-select-items)
;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; projectile -----------------------------------------------------------------
(evil-leader/set-key
  "pb" 'projectile-switch-to-buffer
  "pC" 'projectile-invalidate-cache
  "pd" 'projectile-dired
  "pF" 'projectile-find-file
  "pf" 'helm-projectile
  "pk" 'projectile-kill-buffers
  "pg" 'projectile-grep
  "pr" 'projectile-replace)
;; perforce -------------------------------------------------------------------
(evil-leader/set-key
  "p4a" 'p4-add
  "p4d" 'p4-delete
  "p4D" 'p4-describe
  "p4e" 'p4-edit
  "p4R" 'p4-revert
  "p4r" 'p4-rename
  "p4S" 'p4-submit)
;; quickrun -------------------------------------------------------------------
(evil-leader/set-key
  "qba" 'quickrun-arg
  "qbc" 'quickrun-compile-only
  "qbs" 'quickrun-shell
  "qbx" 'quickrun
  "qeb" 'eval-buffer
  "qex" 'eval-last-sexp
  "qh"  'helm-quickrun
  "qrr" 'quickrun-replace-region
  "qrx" 'quickrun-region)
;; replace --------------------------------------------------------------------
(evil-leader/set-key
  "rR" 'vr/query-replace
  "rr" 'vr/replace)
;; misc -----------------------------------------------------------------------
(evil-leader/set-key
  "kil"  'helm-show-kill-ring
  "reg"  'evil-show-registers)
;; spell check  ---------------------------------------------------------------
(evil-leader/set-key
  "sc" 'cofi/helm-flyspell-correct
  "sd" 'adict-change-dictionary
  "sn" 'flyspell-goto-next-error)
;; toggle ---------------------------------------------------------------------
(evil-leader/set-key
  "t8" 'toggle-fill-column-indicator
  "ta" 'auto-complete-mode
  "tc" 'rainbow-mode
  "tF" 'fringe-mode
  "tf" 'toggle-fullscreen
  "th" 'auto-highlight-symbol-mode
  "tm" 'powerline-minor-modes-toggle
  "tN" 'global-linum-mode
  "tn" 'cofi/evil-toggle-relative-lines
  "tw" 'toggle-read-only)
;; selection ------------------------------------------------------------------
(evil-leader/set-key "v" 'er/expand-region)
;; window ---------------------------------------------------------------------
;; (evil-leader/set-key "wb" 'evenly-split-window-right)
(evil-leader/set-key
  "w2"  'layout-double-columns
  "w3"  'layout-triple-columns
  "wb"  'split-window-right
  "wc"  'delete-window
  "wd"  'toggle-current-window-dedication
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "wM"  'toggle-maximize-centered-buffer
  "wm"  'toggle-maximize-buffer
  "wp"  'popwin:close-popup-window
  "wr"  'rotate-windows
  "wR"  'rotate-windows-backward
;; "wv"  'evenly-split-window-below)
  "wv"  'split-window-below
  "wsh" 'shrink-window-horizontally
  "wsj" 'shrink-window
  "wsk" 'enlarge-window
  "wsl" 'enlarge-window-horizontally
  "wU"  'winner-redo
  "wu"  'winner-undo
  "ww"  'other-window)
;; text -----------------------------------------------------------------------
(evil-leader/set-key
  "x="  (lambda() (interactive) (text-scale-set 0))
  "x+"  'text-scale-increase
  "x-"  'text-scale-decrease
  "xdw" 'delete-trailing-whitespace
  "xmj" 'move-text-down
  "xmk" 'move-text-up
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'count-words-analysis
  "xwc" 'count-words-region)
;; google translate -----------------------------------------------------------
(evil-leader/set-key
  "xgl" 'set-google-translate-languages
  "xgQ" 'google-translate-query-translate-reverse
  "xgq" 'google-translate-query-translate
  "xgT" 'google-translate-at-point-reverse
  "xgt" 'google-translate-at-point)
;; centered cursor ------------------------------------------------------------
(evil-leader/set-key "zz" 'global-centered-cursor-mode)

;; evil-leader modes specific =================================================

;; Lisps ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "mB"  'sp-backward-barf-sexp
  "mb"  'sp-forward-barf-sexp
  "mc"  'sp-convolute-sexp
  "mD"  'sp-kill
  "md"  'elisp-slime-nav-describe-elisp-thing-at-point
  "mf"  'eval-defun
  "mg"  'elisp-slime-nav-find-elisp-thing-at-point
  "mjj" 'sp-split-sexp
  "mjk" 'sp-splice-sexp-killing-forward
  "mjl" 'sp-join-sexps
  "mk"  'sp-splice-sexp-killing-backward
  ;; Eval the current line
  "ml"  (lambda () (interactive) (save-excursion (evil-end-of-line)
                                                 (eval-last-sexp nil)))
  "mr"  'sp-raise-sexp
  "mS"  'sp-backward-slurp-sexp
  "ms"  'sp-forward-slurp-sexp
  "mta"  (lambda () (interactive) (ert t))
  "mtf" 'ert)
;; erlang ---------------------------------------------------------------------
(evil-leader/set-key-for-mode 'erlang-mode
  "mc" 'edts-who-calls
  "md" 'edts-find-doc
  "me" 'edts-code-next-issue
  "mG" 'edts-find-global-function
  "mg" 'edts-find-source-under-point
  "mh" 'edts-find-header-source
  "ml" 'edts-find-local-function
  "mm" 'edts-find-macro-source
  "mr" 'edts-find-record-source
  "mx" 'edts-refactor-extract-function)
;; magit ----------------------------------------------------------------------
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
;; python ---------------------------------------------------------------------
(evil-leader/set-key-for-mode 'python-mode
  "mT1" 'nosetests-pdb-one
  "mt1" 'nosetests-one
  "mTa" 'nosetests-pdb-all
  "mta" 'nosetests-all
  "mb"  'python-add-breakpoint
  "md"  'pylookup-lookup
  "mg"  'jedi:goto-definition
  "mTm" 'nosetests-pdb-module
  "mtm" 'nosetests-module
  "mTs" 'nosetests-pdb-suite
  "mts" 'nosetests-suite
  "m RET" 'quickrun
)
;; R --------------------------------------------------------------------------
(evil-leader/set-key-for-mode 'ess-mode
  "mB" 'ess-eval-buffer-and-go
  "mb" 'ess-eval-buffer
  "mF" 'ess-eval-function-and-go
  "mf" 'ess-eval-function
  "mi" 'R
  "mL" 'ess-eval-line-and-go
  "ml" 'ess-eval-line
  "mp" 'ess-R-object-popup
  "mR" 'ess-eval-region-and-go
  "mr" 'ess-eval-region
  "mS" 'ess-eval-function-or-paragraph-and-step
  "ms" 'ess-eval-region-or-line-and-step
  "mvp" 'ess-R-dv-pprint
  "mvt" 'ess-R-dv-ctable
)

(provide 'my-keybindings)
