;;; keybindings.el --- Spacemacs Defaults Layer key-bindings File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------

;; We define prefix commands only for the sake of which-key
(setq spacemacs/key-binding-prefixes '(("SPC" "M-x")
                                       ("TAB" "last buffer")
                                       ("!"   "shell cmd")
                                       ("*"   "search project w/input")
                                       ("/"   "search project")
                                       ("?"   "show keybindings")
                                       ("a"   "applications")
                                       ("A"   "other applications")
                                       ("b"   "buffers")
                                       ("bc"  "indirect buffers")
                                       ("bN"  "new buffer")
                                       ("c"   "compile/comments")
                                       ("C"   "capture/colors")
                                       ("d"   "documentation")
                                       ("e"   "errors")
                                       ("f"   "files")
                                       ("fC"  "files/convert")
                                       ("fe"  "emacs(spacemacs)")
                                       ("fv"  "variables")
                                       ("fy"  "yank path")
                                       ("F"   "frame")
                                       ("g"   "git/versions-control")
                                       ("h"   "help")
                                       ("hd"  "help-describe")
                                       ("hP"  "profiler")
                                       ("hT"  "tutorials")
                                       ("i"   "insertion")
                                       ("j"   "jump/join/split")
                                       ("jj"  "avy timer")
                                       ("jl"  "avy line")
                                       ("js"  "split sexp")
                                       ("jw"  "avy word")
                                       ("k"   "lisp")
                                       ("kd"  "delete")
                                       ("kD"  "delete-backward")
                                       ("k`"  "hybrid")
                                       ("m"   "major mode commands")
                                       ("n"   "narrow/numbers")
                                       ("N"   "navigation")
                                       ("o"   "user bindings")
                                       ("p"   "projects")
                                       ("q"   "quit")
                                       ("r"   "registers/rings/resume")
                                       ("s"   "search/symbol")
                                       ("sa"  "ag")
                                       ("sg"  "grep")
                                       ("sk"  "ack")
                                       ("sp"  "search project")
                                       ("sP"  "search project w/input")
                                       ("sr"  "ripgrep")
                                       ("st"  "pt")
                                       ("sw"  "web")
                                       ("t"   "toggles")
                                       ("tC"  "colors")
                                       ("tE"  "editing-styles")
                                       ("tEe" "emacs (holy-mode)")
                                       ("tEh" "hybrid (hybrid-mode)")
                                       ("th"  "highlight")
                                       ("tm"  "modeline")
                                       ("T"   "UI toggles/themes")
                                       ("C-t" "other toggles")
                                       ("u"   "universal arg")
                                       ("v"   "expand region")
                                       ("w"   "windows")
                                       ("wc"  "centered")
                                       ("wp"  "popup")
                                       ("x"   "text")
                                       ("xa"  "align")
                                       ("xd"  "delete")
                                       ("xg"  "google-translate")
                                       ("xj"  "justification")
                                       ("xl"  "lines")
                                       ("xt"  "transpose")
                                       ("xw"  "words")
                                       ("z"   "zoom")))
(mapc (lambda (x) (apply #'spacemacs/declare-prefix x))
      spacemacs/key-binding-prefixes)

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'spacemacs/toggle-maximize-buffer)
;; adds two spacing modes while preserving just-one-space behaviour
(define-key global-map (kbd "M-SPC") 'cycle-spacing)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'spacemacs/md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'spacemacs/mu-select-linum)
(global-set-key (kbd "<left-margin> <double-mouse-1>") 'spacemacs/select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'spacemacs/mu-select-linum)

;; ---------------------------------------------------------------------------
;; spacemacs leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(spacemacs/set-leader-keys "u" 'universal-argument)
(when (memq dotspacemacs-editing-style '(vim hybrid))
  (define-key universal-argument-map
    (kbd (concat dotspacemacs-leader-key " u"))
    'universal-argument-more))
;; shell command  -------------------------------------------------------------
(spacemacs/set-leader-keys "!" 'shell-command)
;; applications ---------------------------------------------------------------
(spacemacs/set-leader-keys
  "ac"  'calc-dispatch
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(spacemacs/set-leader-keys
  "TAB"   'spacemacs/alternate-buffer
  "bd"    'spacemacs/kill-this-buffer
  "be"    'spacemacs/safe-erase-buffer
  "bh"    'spacemacs/home
  "bH"    'spacemacs/switch-to-help-buffer
  "b C-d" 'spacemacs/kill-other-buffers
  "b C-S-d" 'spacemacs/kill-matching-buffers-rudely
  "bn"    'next-buffer
  "bm"    'spacemacs/switch-to-messages-buffer
  "b N h" 'spacemacs/new-empty-buffer-left
  "b N C-i" 'make-indirect-buffer
  "b N i" 'clone-indirect-buffer
  "b N I" 'clone-indirect-buffer-other-window-without-purpose
  "b N j" 'spacemacs/new-empty-buffer-below
  "b N k" 'spacemacs/new-empty-buffer-above
  "b N l" 'spacemacs/new-empty-buffer-right
  "b N f" 'spacemacs/new-empty-buffer-new-frame
  "b N n" 'spacemacs/new-empty-buffer
  "bP"    'spacemacs/copy-clipboard-to-whole-buffer
  "bp"    'previous-buffer
  "bR"    'spacemacs/safe-revert-buffer
  "bs"    'spacemacs/switch-to-scratch-buffer
  "bu"    'spacemacs/reopen-killed-buffer
  "bx"    'kill-buffer-and-window
  "bY"    'spacemacs/copy-whole-buffer-to-clipboard
  "bw"    'read-only-mode)
(dotimes (i 9)
  (let ((n (+ i 1)))
    (spacemacs/set-leader-keys (format "b%i" n)
      (intern (format "buffer-to-window-%s" n)))))
;; Cycling settings -----------------------------------------------------------
(spacemacs|define-transient-state theme
  :title "Themes Transient State"
  :doc "\n[_n_/_<right>_] next  [_N_/_p_/_<left>_] previous  [_t_/_<up>_] helm-themes"
  :bindings
  ("n" spacemacs/cycle-spacemacs-theme)
  ("N" spacemacs/cycle-spacemacs-theme-backward)
  ("p" spacemacs/cycle-spacemacs-theme-backward)
  ("t" helm-themes)
  ("<up>" helm-themes)
  ("<right>" spacemacs/cycle-spacemacs-theme)
  ("<left>" spacemacs/cycle-spacemacs-theme-backward))
(spacemacs/set-leader-keys "Tn"
  'spacemacs/theme-transient-state/spacemacs/cycle-spacemacs-theme)
(spacemacs/set-leader-keys "TN"
  'spacemacs/theme-transient-state/spacemacs/cycle-spacemacs-theme-backward)
;; errors ---------------------------------------------------------------------
(spacemacs/set-leader-keys
  "ez" 'spacemacs/last-error
  "en" 'spacemacs/next-error
  "eN" 'spacemacs/previous-error
  "ep" 'spacemacs/previous-error)
(spacemacs|define-transient-state error
  :title "Error transient state"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (spacemacs/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'emacs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when spacemacs--gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- spacemacs--gne-cur-line
                                              spacemacs--gne-min-line)))
                                (1+ (- spacemacs--gne-max-line
                                       spacemacs--gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" spacemacs/next-error "next")
  ("p" spacemacs/previous-error "prev")
  ("N" spacemacs/previous-error "prev")
  ("z" recenter-top-bottom "recenter")
  ("q" nil "quit" :exit t)
  :evil-leader "e.")
;; file -----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "fA" 'spacemacs/find-file-and-replace-buffer
  "fc" 'spacemacs/copy-file
  "fD" 'spacemacs/delete-current-buffer-file
  "fei" 'spacemacs/find-user-init-file
  "fed" 'spacemacs/find-dotfile
  "feD" 'spacemacs/ediff-dotfile-and-template
  "fee" 'spacemacs/edit-env
  "feE" 'dotspacemacs/call-user-env
  "fe C-e" 'spacemacs/force-init-spacemacs-env
  "feR" 'dotspacemacs/sync-configuration-layers
  "fev" 'spacemacs/display-and-copy-version
  "feU"  'configuration-layer/update-packages
  "fCd" 'spacemacs/unix2dos
  "fCu" 'spacemacs/dos2unix
  "fi" 'spacemacs/insert-file
  "fg" 'rgrep
  "fl" 'find-file-literally
  "fE" 'spacemacs/sudo-edit
  "fo" 'spacemacs/open-file-or-directory-in-external-app
  "fR" 'spacemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fyc" 'spacemacs/copy-file-path-with-line-column
  "fyd" 'spacemacs/copy-directory-path
  "fyl" 'spacemacs/copy-file-path-with-line
  "fyn" 'spacemacs/copy-file-name
  "fyN" 'spacemacs/copy-file-name-base
  "fyy" 'spacemacs/copy-file-path)
;; frame ----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "Ff" 'spacemacs/find-file-other-frame
  "Fd" 'delete-frame
  "FD" 'delete-other-frames
  "Fb" 'spacemacs/switch-to-buffer-other-frame
  "FB" 'spacemacs/display-buffer-other-frame
  "Fo" 'other-frame
  "FO" 'spacemacs/dired-other-frame
  "Fn" 'make-frame)
;; help -----------------------------------------------------------------------
(defalias 'emacs-tutorial 'help-with-tutorial)
(spacemacs/set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdl" 'spacemacs/describe-last-keys
  "hdp" 'describe-package
  "hdP" 'configuration-layer/describe-package
  "hds" 'spacemacs/describe-system-info
  "hdt" 'describe-theme
  "hdv" 'describe-variable
  "hI"  'spacemacs/report-issue
  "hn"  'view-emacs-news
  "hPs" 'profiler-start
  "hPk" 'profiler-stop
  "hPr" 'profiler-report
  "hPw" 'profiler-report-write-profile
  "hTe" 'emacs-tutorial)
;; insert stuff ---------------------------------------------------------------
(spacemacs/set-leader-keys
  "iJ" 'spacemacs/insert-line-below-no-indent
  "iK" 'spacemacs/insert-line-above-no-indent
  "ik" 'spacemacs/evil-insert-line-above
  "ij" 'spacemacs/evil-insert-line-below)
;; format ---------------------------------------------------------------------
(spacemacs/set-leader-keys
  "j(" 'check-parens
  "j=" 'spacemacs/indent-region-or-buffer
  "j+" 'spacemacs/iwb-region-or-buffer
  "jo" 'open-line
  "jS" 'spacemacs/split-and-new-line
  "jk" 'spacemacs/evil-goto-next-line-and-indent)

;; navigation/jumping ---------------------------------------------------------
(spacemacs/set-leader-keys
  "j0" 'spacemacs/push-mark-and-goto-beginning-of-line
  "j$" 'spacemacs/push-mark-and-goto-end-of-line
  "jc" 'goto-last-change
  "jf" 'find-function
  "jv" 'find-variable)

;; Compilation ----------------------------------------------------------------
(spacemacs/set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cd" 'spacemacs/close-compilation-window)
(with-eval-after-load 'compile
  (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))
;; narrow & widen -------------------------------------------------------------
(spacemacs/set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; toggle ---------------------------------------------------------------------
(spacemacs|add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(spacemacs|add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Toggle between line wrapping or truncation (no wrap)."
  :evil-leader "tl")
(spacemacs|add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (spacemacs//init-visual-line-keys)
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")
(spacemacs|add-toggle visual-line-navigation-globally
  :status global-visual-line-mode
  :on
  (progn
    (global-visual-line-mode)
    (spacemacs//init-visual-line-keys)
    (evil-normalize-keymaps))
  :off
  (progn
    (global-visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines globally."
  :evil-leader "t C-S-l")
(spacemacs|add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(spacemacs|add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(spacemacs|add-toggle fringe
  :if (fboundp 'fringe-mode)
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(spacemacs|add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (spacemacs/toggle-frame-fullscreen)
  :off (spacemacs/toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(spacemacs|add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(spacemacs|add-toggle mode-line
  :status (not hidden-mode-line-mode)
  :on (hidden-mode-line-mode -1)
  :off (hidden-mode-line-mode)
  :documentation "Toggle the visibility of modeline."
  :evil-leader "tmT")
(spacemacs|add-toggle display-time
  :mode display-time-mode
  :documentation "Display time in modeline."
  :evil-leader "tmt")
(spacemacs|add-toggle syntax-highlighting
  :mode font-lock-mode
  :documentation "Toggle syntax highlighting."
  :evil-leader "ths")
(spacemacs|add-toggle zero-based-column-indexing
  :documentation "Toggle column indexing starting at 0 versus 1.

This is achieved by the built in functionality available in emacs 26 by changing
the value of the `column-number-indicator-zero-based' variable. Functionality
that does not take into account `column-number-indicator-zero-based' will not
respond to this toggle."
  :status (bound-and-true-p column-number-indicator-zero-based)
  :on (setq column-number-indicator-zero-based t)
  :off (setq column-number-indicator-zero-based nil)
  :on-message (concat
               "Column indexing starts at 0 (current column is "
               (number-to-string (current-column))
               ")")
  :off-message (concat
                "Column indexing starts at 1 (current column is "
                (number-to-string (1+ (current-column)))
                ")")
  :evil-leader "tz")

(spacemacs|add-toggle transparent-frame
  :status nil
  :on (spacemacs/toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(spacemacs|add-toggle tool-bar
  :if window-system
  :mode tool-bar-mode
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(spacemacs|add-toggle menu-bar
  :if window-system
  :mode menu-bar-mode
  :documentation "Display the menu bar."
  :evil-leader "Tm")
(spacemacs|add-toggle semantic-stickyfunc
  :mode semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "TS")
(spacemacs|add-toggle semantic-stickyfunc-globally
  :mode global-semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-S")
;; quit -----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "qs" 'spacemacs/save-buffers-kill-emacs
  "qq" 'spacemacs/prompt-kill-emacs
  "qQ" 'spacemacs/kill-emacs
  "qf" 'spacemacs/frame-killer)
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

(spacemacs/set-leader-keys
  "w TAB"  'spacemacs/alternate-window
  "w1"  'spacemacs/window-split-single-column
  "w2"  'spacemacs/window-split-double-columns
  "w3"  'spacemacs/window-split-triple-columns
  "w4"  'spacemacs/window-split-grid
  "wb"  'spacemacs/switch-to-minibuffer-window
  "wd"  'spacemacs/delete-window
  "wt"  'spacemacs/toggle-current-window-dedication
  "wf"  'follow-mode
  "wF"  'make-frame
  "wH"  'evil-window-move-far-left
  "w <S-left>"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "w <S-down>"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "w <S-up>"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
  "wm"  'spacemacs/toggle-maximize-buffer
  "wcc"  'spacemacs/toggle-centered-buffer
  "wcC"  'spacemacs/toggle-distraction-free
  "wc."  'spacemacs/centered-buffer-transient-state
  "wo"  'other-frame
  "wr"  'spacemacs/rotate-windows-forward
  "wR"  'spacemacs/rotate-windows-backward
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "wx"  'kill-buffer-and-window
  "w/"  'split-window-right
  "w="  'balance-windows-area
  "w+"  'spacemacs/window-layout-toggle
  "w_"  'spacemacs/maximize-horizontally
  "w|"  'spacemacs/maximize-vertically)
;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(spacemacs/set-leader-keys
  "xa%" 'spacemacs/align-repeat-percent
  "xa&" 'spacemacs/align-repeat-ampersand
  "xa(" 'spacemacs/align-repeat-left-paren
  "xa)" 'spacemacs/align-repeat-right-paren
  "xa{" 'spacemacs/align-repeat-left-curly-brace
  "xa}" 'spacemacs/align-repeat-right-curly-brace
  "xa[" 'spacemacs/align-repeat-left-square-brace
  "xa]" 'spacemacs/align-repeat-right-square-brace
  "xa," 'spacemacs/align-repeat-comma
  "xa." 'spacemacs/align-repeat-decimal
  "xa:" 'spacemacs/align-repeat-colon
  "xa;" 'spacemacs/align-repeat-semicolon
  "xa=" 'spacemacs/align-repeat-equal
  "xa\\" 'spacemacs/align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'spacemacs/align-repeat-math-oper
  "xar" 'spacemacs/align-repeat
  "xa|" 'spacemacs/align-repeat-bar
  "xc"  'count-region
  "xd SPC" 'cycle-spacing
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xlc" 'spacemacs/sort-lines-by-column
  "xlC" 'spacemacs/sort-lines-by-column-reverse
  "xld" 'spacemacs/duplicate-line-or-region
  "xls" 'spacemacs/sort-lines
  "xlS" 'spacemacs/sort-lines-reverse
  "xlu" 'spacemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xte" 'transpose-sexps
  "xtl" 'transpose-lines
  "xtp" 'transpose-paragraphs
  "xts" 'transpose-sentences
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'spacemacs/count-words-analysis
  "x TAB" 'indent-rigidly)

(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)

;; shell ----------------------------------------------------------------------
(with-eval-after-load 'shell
  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

;; ---------------------------------------------------------------------------
;; Transient-states
;; ---------------------------------------------------------------------------

;; Buffer transient state

(spacemacs|define-transient-state buffer
  :title "Buffer Selection Transient State"
  :doc "
 [_C-1_.._C-9_] goto nth window            [_n_/_<right>_]^^  next buffer       [_b_]   buffer list
 [_1_.._9_]     move buffer to nth window  [_N_/_p_/_<left>_] previous buffer   [_C-d_] bury buffer
 [_M-1_.._M-9_] swap buffer w/ nth window  [_d_]^^^^          kill buffer       [_o_]   other window
 ^^^^                                      [_z_]^^^^          recenter          [_q_]   quit"
  :bindings
  ("n" next-buffer)
  ("<right>" next-buffer)
  ("p" previous-buffer)
  ("N" previous-buffer)
  ("o" other-window)
  ("<left>" previous-buffer)
  ("b" (cond ((configuration-layer/layer-used-p 'helm)
              (helm-buffers-list))
             ((configuration-layer/layer-used-p 'ivy)
              (ivy-switch-buffer))))
  ("d" spacemacs/kill-this-buffer)
  ("C-d" bury-buffer)
  ("z" recenter-top-bottom)
  ("q" nil :exit t)
  ("1" move-buffer-window-no-follow-1)
  ("2" move-buffer-window-no-follow-2)
  ("3" move-buffer-window-no-follow-3)
  ("4" move-buffer-window-no-follow-4)
  ("5" move-buffer-window-no-follow-5)
  ("6" move-buffer-window-no-follow-6)
  ("7" move-buffer-window-no-follow-7)
  ("8" move-buffer-window-no-follow-8)
  ("9" move-buffer-window-no-follow-9)
  ("M-1" swap-buffer-window-no-follow-1)
  ("M-2" swap-buffer-window-no-follow-2)
  ("M-3" swap-buffer-window-no-follow-3)
  ("M-4" swap-buffer-window-no-follow-4)
  ("M-5" swap-buffer-window-no-follow-5)
  ("M-6" swap-buffer-window-no-follow-6)
  ("M-7" swap-buffer-window-no-follow-7)
  ("M-8" swap-buffer-window-no-follow-8)
  ("M-9" swap-buffer-window-no-follow-9)
  ("C-1" winum-select-window-1)
  ("C-2" winum-select-window-2)
  ("C-3" winum-select-window-3)
  ("C-4" winum-select-window-4)
  ("C-5" winum-select-window-5)
  ("C-6" winum-select-window-6)
  ("C-7" winum-select-window-7)
  ("C-8" winum-select-window-8)
  ("C-9" winum-select-window-9))
(spacemacs/set-leader-keys "b." 'spacemacs/buffer-transient-state/body)

;; end of Buffer transient state

;; Window Manipulation Transient State

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

(defvar spacemacs--window-manipulation-ts-full-hint-toggle nil
  "Display window-manipulation transient-state documentation.")

(defun spacemacs//window-manipulation-ts-toggle-hint ()
  "Toggle the full hint docstring for the window manipulation transient-state."
  (interactive)
  (setq spacemacs--window-manipulation-ts-full-hint-toggle
        (not spacemacs--window-manipulation-ts-full-hint-toggle)))

(defun spacemacs//window-manipulation-ts-hint ()
  "Return a condensed/full hint for the window manipulation transient state"
  (concat
   " "
   (if spacemacs--window-manipulation-ts-full-hint-toggle
       spacemacs--window-manipulation-ts-full-hint
     (concat spacemacs--window-manipulation-ts-minified-hint
             "  ([" (propertize "?" 'face 'hydra-face-red) "] help)"))))

(spacemacs|transient-state-format-hint window-manipulation
  spacemacs--window-manipulation-ts-minified-hint "
Select: _w_ _h_ _j_ _k_ _l_ _0_.._9_ Move: _H_ _J_ _K_ _L_ _r_ _R_ Split: _s_ _v_ Resize: _[_ _]_ _{_ _}_ _m_ _|_ ___")

(spacemacs|transient-state-format-hint window-manipulation
  spacemacs--window-manipulation-ts-full-hint
  (format "\n [_?_] toggle help
 Select^^^^               Move^^^^              Split^^^^^^               Resize^^             Other^^
 ──────^^^^─────────────  ────^^^^────────────  ─────^^^^^^─────────────  ──────^^───────────  ─────^^──────────────────
 [_j_/_k_]  down/up       [_J_/_K_] down/up     [_s_]^^^^ horizontal      [_[_] shrink horiz   [_u_] restore prev layout
 [_h_/_l_]  left/right    [_H_/_L_] left/right  [_S_]^^^^ horiz & follow  [_]_] enlarge horiz  [_U_] restore next layout
 [_0_.._9_] window 0..9   [_r_]^^   rotate fwd  [_v_]^^^^ vertical        [_{_] shrink verti   [_d_] close current
 [_w_]^^    other window  [_R_]^^   rotate bwd  [_V_]^^^^ verti & follow  [_}_] enlarge verti  [_D_] close other
 [_o_]^^    other frame   ^^^^                  [_m_/_|_/___] maximize    %s^^^^^^^^^^^^^^^^^^ [_q_] quit"
          (if (configuration-layer/package-used-p 'golden-ratio)
              "[_g_] golden-ratio  "
            "^^^^                  ")))

(spacemacs|define-transient-state window-manipulation
  :title "Window Manipulation TS"
  :hint-is-doc t
  :dynamic-hint (spacemacs//window-manipulation-ts-hint)
  :bindings
  ("q" nil :exit t)
  ("?" spacemacs//window-manipulation-ts-toggle-hint)
  ("0" winum-select-window-0)
  ("1" winum-select-window-1)
  ("2" winum-select-window-2)
  ("3" winum-select-window-3)
  ("4" winum-select-window-4)
  ("5" winum-select-window-5)
  ("6" winum-select-window-6)
  ("7" winum-select-window-7)
  ("8" winum-select-window-8)
  ("9" winum-select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" spacemacs/shrink-window-horizontally)
  ("]" spacemacs/enlarge-window-horizontally)
  ("{" spacemacs/shrink-window)
  ("}" spacemacs/enlarge-window)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("o" other-frame)
  ("r" spacemacs/rotate-windows-forward)
  ("R" spacemacs/rotate-windows-backward)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("m" spacemacs/toggle-maximize-buffer)
  ("_" spacemacs/maximize-horizontally)
  ("|" spacemacs/maximize-vertically)
  ("w" other-window))
(spacemacs/set-leader-keys
  "w." 'spacemacs/window-manipulation-transient-state/body
  "w[" 'spacemacs/window-manipulation-transient-state/spacemacs/shrink-window-horizontally
  "w]" 'spacemacs/window-manipulation-transient-state/spacemacs/enlarge-window-horizontally
  "w{" 'spacemacs/window-manipulation-transient-state/spacemacs/shrink-window
  "w}" 'spacemacs/window-manipulation-transient-state/spacemacs/enlarge-window)

;; end of Window Manipulation Transient State

;; text Manipulation Transient State

(defun spacemacs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun spacemacs/scale-up-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 1))

(defun spacemacs/scale-down-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size -1))

(defun spacemacs/reset-font-size ()
  "Reset the font size."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 0))

(spacemacs|define-transient-state scale-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_/_k_] scale up [_-_/___/_j_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" spacemacs/scale-up-font)
  ("=" spacemacs/scale-up-font)
  ("k" spacemacs/scale-up-font)
  ("-" spacemacs/scale-down-font)
  ("_" spacemacs/scale-down-font)
  ("j" spacemacs/scale-down-font)
  ("0" spacemacs/reset-font-size)
  ("q" nil :exit t))

(spacemacs/set-leader-keys "zx" 'spacemacs/scale-font-transient-state/body)

;; end of Text Manipulation Transient State

;; Transparency transient-state

(defun spacemacs/toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (dotfile-setting (cons dotspacemacs-active-transparency
                               dotspacemacs-inactive-transparency)))
    (if (equal alpha dotfile-setting)
        (spacemacs/disable-transparency frame)
      (spacemacs/enable-transparency frame dotfile-setting))))

(defun spacemacs/enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values. The
default value for ALPHA is based on
`dotspacemacs-active-transparency' and
`dotspacemacs-inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                           (cons dotspacemacs-active-transparency
                                 dotspacemacs-inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun spacemacs/disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun spacemacs/increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun spacemacs/decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
                           (cons decreased-alpha decreased-alpha)))))

(spacemacs|define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :doc "\n[_+_/_=_/_k_] increase transparency [_-_/___/_j_] decrease [_T_] toggle [_q_] quit"
  :bindings
  ("+" spacemacs/increase-transparency)
  ("=" spacemacs/increase-transparency)
  ("k" spacemacs/increase-transparency)
  ("-" spacemacs/decrease-transparency)
  ("_" spacemacs/decrease-transparency)
  ("j" spacemacs/decrease-transparency)
  ("T" spacemacs/toggle-transparency)
  ("q" nil :exit t))
(spacemacs/set-leader-keys "TT"
  'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)

;; end of Transparency Transient State
