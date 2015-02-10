;;; keybindings.el --- Spacemacs Layer key-bindings File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)
;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)

;; Make <escape> quit as much as possible
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
  "b0"  'beginning-of-buffer
  "b$"  'end-of-buffer
  "bb"  'spacemacs/alternate-buffer ;; switch back and forth between two last buffers
  "TAB" 'spacemacs/alternate-buffer
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
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdp" 'describe-package
  "hdt" 'describe-theme
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
  "fD"  'delete-current-buffer-file
  "fei" 'find-user-init-file
  "fes" 'find-spacemacs-file
  "fec" 'find-contrib-file
  "fed" 'find-dotfile
  "fev" 'spacemacs/display-and-copy-version
  "fj" 'dired-jump
  "fo" 'spacemacs/open-in-external-app
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
(unless (configuration-layer/package-declaredp 'fancy-narrow)
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
(spacemacs|add-toggle fill-column-indicator
                      :status nil
                      :on (toggle-fill-column-indicator)
                      :documentation "Display the fill column indicator."
                      :evil-leader "t8")
(spacemacs|add-toggle fringe
                      :status (not (equal fringe-mode 0))
                      :on (call-interactively 'fringe-mode)
                      :off (fringe-mode 0)
                      :documentation "Display the fringe in GUI mode."
                      :evil-leader "tf")
(spacemacs|add-toggle fullscreen-frame
                      :status nil
                      :on (spacemacs/toggle-frame-fullscreen)
                      :documentation "Display the current frame in full screen."
                      :evil-leader "tF")
(spacemacs|add-toggle highlight-current-line-globally
                      :status global-hl-line-mode
                      :on (global-hl-line-mode)
                      :off (global-hl-line-mode -1)
                      :documentation "Globally Highlight the current line."
                      :evil-leader "th")
(spacemacs|add-toggle truncate-lines
                      :status nil
                      :on (toggle-truncate-lines)
                      :documentation "Truncate the long lines (no wrap)."
                      :evil-leader "tl")
(spacemacs|add-toggle visual-line-navigation
                      :status visual-line-mode
                      :on (visual-line-mode)
                      :off (visual-line-mode -1)
                      :documentation "Move point according to visual lines."
                      :evil-leader "tL")
(spacemacs|add-toggle maximize-frame
                      :if (version< "24.3.50" emacs-version)
                      :status nil
                      :on (toggle-frame-maximized)
                      :documentation "Maximize the current frame."
                      :evil-leader "tM")
(spacemacs|add-toggle line-numbers
                      :status linum-mode
                      :on (global-linum-mode)
                      :off (global-linum-mode -1)
                      :documentation "Show the line numbers."
                      :evil-leader "tn")
(spacemacs|add-toggle transparent-frame
                      :status nil
                      :on (toggle-transparency)
                      :documentation "Make the current frame non-opaque."
                      :evil-leader "tt")
(spacemacs|add-toggle tool-bar
                      :if window-system
                      :status tool-bar-mode
                      :on (tool-bar-mode)
                      :off (tool-bar-mode -1)
                      :documentation "Display the tool bar in GUI mode."
                      :evil-leader "tT")
(spacemacs|add-toggle menu-bar
                      :if (or window-system (version<= "24.3.1" emacs-version))
                      :status menu-bar-mode
                      :on (menu-bar-mode)
                      :off (menu-bar-mode -1)
                      :documentation "Display the menu bar."
                      :evil-leader "tU")
(spacemacs|add-toggle whitespaces
                      :status whitespace-mode
                      :on (whitespace-mode)
                      :off (whitespace-mode -1)
                      :documentation "Display the whitespaces."
                      :evil-leader "t SPC")
;; quit -----------------------------------------------------------------------
(evil-leader/set-key
  "qs" 'spacemacs/save-buffers-kill-emacs
  "qq" 'spacemacs/prompt-kill-emacs
  "qQ" 'spacemacs/kill-emacs
  "qz" 'spacemacs/frame-killer)
;; window ---------------------------------------------------------------------
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

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
  "wr"  'spacemacs/resize-window-overlay-map
  "wR"  'rotate-windows
  ;; "wv"  'evenly-split-window-below)
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "w/"  'split-window-right
  "ww"  'other-window)
;; text -----------------------------------------------------------------------
(evil-leader/set-key
  "zx="  'spacemacs/reset-font-size
  "zx+"  'spacemacs/scale-up-font
  "zx-"  'spacemacs/scale-down-font
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
