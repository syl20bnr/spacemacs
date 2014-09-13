;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; simple and more consistent keyboard quit key bindings
;; thanks to Bin Chen for the idea (http://blog.binchen.org/?p=735)
(global-set-key (kbd "f")
  (lambda () (interactive) (fd-trigger 'keyboard-quit)))
(define-key minibuffer-local-map (kbd "f")
  (lambda () (interactive) (fd-trigger 'abort-recursive-edit)))
;; evil
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
;; quick navigation
(define-key evil-normal-state-map (kbd "L")
  (lambda () (interactive)
    (evil-window-bottom)
    (evil-scroll-line-to-center nil)))
(define-key evil-normal-state-map (kbd "H")
  (lambda () (interactive)
    (evil-window-top)
    (evil-scroll-line-to-center nil)))

;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; switch window
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
;; shell command  -------------------------------------------------------------
(evil-leader/set-key "S" 'shell-command)
;; switch back and forth between two last buffers -----------------------------
(evil-leader/set-key "TAB"
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t))))
;; applications ---------------------------------------------------------------
(evil-leader/set-key
  "ac"  'calc-dispatch
  "ad"  'dired
  "ai"  'irc
  "ap"  'proced
  "ase" 'eshell
  "asi" 'shell
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(evil-leader/set-key
  "bd"  'delete-current-buffer-file
  "bK"  'kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'kill-matching-buffers-rudely
  "bn"  'switch-to-next-buffer
  "bp"  'switch-to-prev-buffer
  "bR"  (lambda () (interactive) (revert-buffer nil t))
  "br"  'rename-current-buffer-file
  "bw"  'toggle-read-only)
;; Cycling settings -----------------------------------------------------------
(evil-leader/set-key "ct" 'spacemacs/cycle-spacemacs-theme)
;; errors ---------------------------------------------------------------------
(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)
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
;; replace J (no leader) key binding for a more frequent action:
;; go and indent line below the point
;; <SPC> J split the current line at point and indent it
;; evil-join can still be perfomed with <SPC> j k
(define-key evil-normal-state-map "J" (lambda () (interactive) (join-line 1) (sp-newline)))
(evil-leader/set-key
  "J" (lambda () (interactive) (sp-split-sexp 1) (sp-newline))
  "jJ" 'sp-split-sexp
  "jj" 'sp-newline
  "jk" 'evil-join)
;; navigation -----------------------------------------------------------------
(evil-leader/set-key
  "jh" (lambda () (interactive) (push-mark (point)) (evil-beginning-of-line))
  "jl" (lambda () (interactive) (push-mark (point)) (evil-end-of-line)))
;; Compilation ----------------------------------------------------------------
(evil-leader/set-key "cc" 'compile)
;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; misc -----------------------------------------------------------------------
(evil-leader/set-key
  "reg"  'evil-show-registers)
;; spell check  ---------------------------------------------------------------
(evil-leader/set-key
  "sc" 'cofi/helm-flyspell-correct
  "sn" 'flyspell-goto-next-error)
;; toggle ---------------------------------------------------------------------
(evil-leader/set-key
  "t8" 'toggle-fill-column-indicator
  "tF" 'fringe-mode
  "tf" 'toggle-fullscreen
  "tN" 'global-linum-mode
  "tn" 'cofi/evil-toggle-relative-lines
  "tw" 'toggle-read-only)
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
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'count-words-analysis
  "xwc" 'count-words-region)
;; google translate -----------------------------------------------------------
(evil-leader/set-key
  "xgl" 'set-google-translate-languages)

;; ---------------------------------------------------------------------------
;; evil-leader modes specific key bindings
;; ---------------------------------------------------------------------------

;; Lisps ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "mB"  'sp-backward-barf-sexp
  "mb"  'sp-forward-barf-sexp
  "mc"  'sp-convolute-sexp
  "mD"  'sp-kill
  "md"  'elisp-slime-nav-describe-elisp-thing-at-point
  "mf"  'eval-defun
  "mg"  'elisp-slime-nav-find-elisp-thing-at-point
  "mhv" 'describe-variable
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
