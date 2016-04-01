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
  "en" 'spacemacs/next-error
  "ep" 'spacemacs/previous-error
  "eN" 'spacemacs/previous-error)
;; file -----------------------------------------------------------------------
(evil-leader/set-key
  "fD"  'delete-current-buffer-file
  "fei" 'find-user-init-file
  "fes" 'find-spacemacs-file
  "fec" 'find-contrib-file
  "fed" 'find-dotfile
  "fev" 'spacemacs/display-and-copy-version
  "ff" 'ido-find-file
  "fF" 'helm-find-files
  "fg" 'rgrep
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
  "Sd" 'ispell-change-dictionary
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
(spacemacs|add-toggle auto-fill-mode
                      :status auto-fill-function
                      :on (auto-fill-mode)
                      :off (auto-fill-mode -1)
                      :documentation "Break line beyond `current-fill-column` while editing."
                      :evil-leader "t C-f")
(spacemacs|add-toggle debug-on-error
                      :status nil
                      :on (toggle-debug-on-error)
                      :documentation "Toggle display of backtrace when an error happens."
                      :evil-leader "t D")
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
  "wb"  'switch-to-minibuffer-window
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
  "wm"  'toggle-maximize-buffer
  "wM"  'toggle-maximize-centered-buffer
  "wo"  'other-frame
  "wR"  'rotate-windows
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right)
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
;; emacs-lisp -----------------------------------------------------------------
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "me$" 'lisp-state-eval-sexp-end-of-line
  "mee" 'eval-last-sexp
  "mef" 'eval-defun
  "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
  "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point
  "m,"  'lisp-state-toggle-lisp-state
  "mtb" 'spacemacs/ert-run-tests-buffer
  "mtq" 'ert)

;; ---------------------------------------------------------------------------
;; Micro-states
;; ---------------------------------------------------------------------------

;; Window Manipulation Micro State

(defun spacemacs/shrink-window-horizontally (delta)
  "Wrap `spacemacs/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun spacemacs/shrink-window (delta)
  "Wrap `spacemacs/shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun spacemacs/enlarge-window (delta)
  "Wrap `spacemacs/enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun spacemacs/enlarge-window-horizontally (delta)
  "Wrap `spacemacs/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defun spacemacs//window-manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
  [?]                       display this help
  [0,9]                     go to numbered window
  [-] [/] [s] [v] [S] [V]   split windows bellow|right and focus
  [c] [C]                   close current|other windows
  [g]                       toggle golden-ratio
  [h] [j] [k] [l]           go to left|bottom|top|right
  [H] [J] [K] [L]           move windows to far/very left|bottom|top|right
  [[] []] [{] [}]           shrink/enlarge horizontaly and verticaly respectively
  [o] [w]                   other frame|window
  [R]                       rotate windows
  [u] [U]                   restore previous|next window layout")

(defun spacemacs//window-manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[h] [j] [k] [l] to move focus, "
          "[H] [J] [K] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

(defun spacemacs//window-manipulation-resize-doc ()
  "Dynamic help string when resizing windows."
  (format
   (concat "[%sx%s] Resize window: [[] []] shrink/enlarge horizontally, "
           "[{] [}] shrink/enlarge vertically.")
   (window-total-width) (window-total-height)))

(defun spacemacs//window-manipulation-split-doc ()
  "Help string for moving between windows"
  (concat "[-], [s] to split horizontally,  [/], [v] to split vertically, "
          "[S], [V] to split and focus"))

(defun spacemacs//window-manipulation-number-doc ()
  "Help string for selecting window with number."
  (format "(selected window #%s) press [0,9] to select the corresponding numbered window."
          (window-numbering-get-number-string)))

(defun spacemacs//window-manipulation-layout-doc ()
  "Help string for layout manipulation"
  (concat "[c]lose window, [C]lose other windows, "
          "[u]ndo window layout, [U] redo window layout."))

(defun spacemacs//window-manipulation-gratio-doc ()
  "Help string for golden ratio"
  (format "(golden-ration %s) toggle with [g]"
          (if (symbol-value golden-ratio-mode) "enabled" "disabled")))

(spacemacs|define-micro-state window-manipulation
  :doc "[?] for help"
  :evil-leader "w."
  :bindings
  ("?" nil                                   :doc (spacemacs//window-manipulation-full-doc))
  ("0" select-window-0                       :doc (spacemacs//window-manipulation-number-doc))
  ("1" select-window-1                       :doc (spacemacs//window-manipulation-number-doc))
  ("2" select-window-2                       :doc (spacemacs//window-manipulation-number-doc))
  ("3" select-window-3                       :doc (spacemacs//window-manipulation-number-doc))
  ("4" select-window-4                       :doc (spacemacs//window-manipulation-number-doc))
  ("5" select-window-5                       :doc (spacemacs//window-manipulation-number-doc))
  ("6" select-window-6                       :doc (spacemacs//window-manipulation-number-doc))
  ("7" select-window-7                       :doc (spacemacs//window-manipulation-number-doc))
  ("8" select-window-8                       :doc (spacemacs//window-manipulation-number-doc))
  ("9" select-window-9                       :doc (spacemacs//window-manipulation-number-doc))
  ("-" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("[" spacemacs/shrink-window-horizontally  :doc (spacemacs//window-manipulation-resize-doc))
  ("]" spacemacs/enlarge-window-horizontally :doc (spacemacs//window-manipulation-resize-doc))
  ("{" spacemacs/shrink-window               :doc (spacemacs//window-manipulation-resize-doc))
  ("}" spacemacs/enlarge-window              :doc (spacemacs//window-manipulation-resize-doc))
  ("c" delete-window                         :doc (spacemacs//window-manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (spacemacs//window-manipulation-layout-doc))
  ("g" spacemacs/toggle-golden-ratio         :doc (spacemacs//window-manipulation-gratio-doc))
  ("h" evil-window-left                      :doc (spacemacs//window-manipulation-move-doc))
  ("j" evil-window-down                      :doc (spacemacs//window-manipulation-move-doc))
  ("k" evil-window-up                        :doc (spacemacs//window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (spacemacs//window-manipulation-move-doc))
  ("H" evil-window-move-far-left             :doc (spacemacs//window-manipulation-move-doc))
  ("J" evil-window-move-very-bottom          :doc (spacemacs//window-manipulation-move-doc))
  ("K" evil-window-move-very-top             :doc (spacemacs//window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (spacemacs//window-manipulation-move-doc))
  ("o" other-frame                           :doc (spacemacs//window-manipulation-move-doc))
  ("R" rotate-windows                        :doc (spacemacs//window-manipulation-move-doc))
  ("s" split-window-below                    :doc (spacemacs//window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("u" winner-undo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (spacemacs//window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("w" other-window                          :doc (spacemacs//window-manipulation-move-doc)))

;; end of Window Manipulation Micro State
