;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)
;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)

;; ---------------------------------------------------------------------------
;; Make <escape> quit as much as possible
;; ---------------------------------------------------------------------------
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(evil-leader/set-key "u" 'universal-argument)
;; shell command  -------------------------------------------------------------
(evil-leader/set-key "!" 'shell-command)
;; switch back and forth between two last buffers -----------------------------
(evil-leader/set-key "TAB" 'spacemacs/last-buffer)
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
  "be"  'spacemacs/safe-erase-buffer
  "bK"  'kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'kill-matching-buffers-rudely
  "bn"  'switch-to-next-buffer
  "bp"  'switch-to-prev-buffer
  "bR"  'spacemacs/safe-revert-buffer
  "br"  'rename-current-buffer-file
  "bw"  'toggle-read-only)
;; Cycling settings -----------------------------------------------------------
(evil-leader/set-key "Tn" 'spacemacs/cycle-spacemacs-theme)
;; describe functions ---------------------------------------------------------
(evil-leader/set-key
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdv" 'describe-variable)
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
  "fei" 'find-user-init-file
  "fes" 'find-spacemacs-file
  "fec" 'find-contrib-file
  "fed" 'find-dotfile
  "fS" 'evil-write-all
  "fs" 'evil-write
  "fy" 'show-and-copy-buffer-filename)
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
;; <SPC> j k key binding for a frequent action: go and indent line below the point
;; <SPC> J split the current line at point and indent it
(evil-leader/set-key
  "J"  'sp-split-sexp
  "jJ" 'spacemacs/split-and-new-line
  "jj" 'sp-newline
  "jk" 'evil-goto-next-line-and-indent)
;; navigation -----------------------------------------------------------------
(evil-leader/set-key
  "jh" 'spacemacs/push-mark-and-goto-beginning-of-line
  "jl" 'spacemacs/push-mark-and-goto-end-of-line)
;; Compilation ----------------------------------------------------------------
(evil-leader/set-key "cc" 'helm-make-projectile)
(evil-leader/set-key "cC" 'compile)
(evil-leader/set-key "cr" 'recompile)
;; narrow & widen -------------------------------------------------------------
(unless (ht-contains? config-system-all-packages 'fancy-narrow)
  (evil-leader/set-key
    "nr" 'narrow-to-region
    "np" 'narrow-to-page
    "nf" 'narrow-to-defun
    "nw" 'widen))
;; spell check  ---------------------------------------------------------------
(evil-leader/set-key
  "Sc" 'cofi/helm-flyspell-correct
  "Sn" 'flyspell-goto-next-error)
;; toggle ---------------------------------------------------------------------
(evil-leader/set-key
  "t8" 'toggle-fill-column-indicator
  "tF" 'toggle-frame-fullscreen
  "tf" 'fringe-mode
  "tl" 'toggle-truncate-lines
  "tL" 'visual-line-mode
  "tM" 'toggle-frame-maximized
  "tn" 'global-linum-mode)
;; window ---------------------------------------------------------------------
;; (evil-leader/set-key "wb" 'evenly-split-window-right)
(evil-leader/set-key
  "w2"  'layout-double-columns
  "w3"  'layout-triple-columns
  "wc"  'delete-window
  "wC"  'delete-other-windows
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
  "wo"  'other-frame
  "wr"  'rotate-windows
  "wR"  'rotate-windows-backward
;; "wv"  'evenly-split-window-below)
  "ws"  'split-window-below
  "wS"  'spacemacs/resize-window-overlay-map
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "ww"  'other-window)
;; text -----------------------------------------------------------------------
(evil-leader/set-key
  "x="  'spacemacs/reset-font-size
  "x+"  'spacemacs/scale-up-font
  "x-"  'spacemacs/scale-down-font
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
;; Lisps ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "mD"  'elisp-slime-nav-describe-elisp-thing-at-point
  "mg"  'elisp-slime-nav-find-elisp-thing-at-point
  "mhv" 'describe-variable
  "mta" 'spacemacs/ert-run-tests-buffer
  "mtf" 'ert)
