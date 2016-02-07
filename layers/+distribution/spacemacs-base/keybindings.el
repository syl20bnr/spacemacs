;;; keybindings.el --- Spacemacs Base Layer key-bindings File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'spacemacs/toggle-maximize-buffer)

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
  "ad"  'dired
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(spacemacs/set-leader-keys
  "bd"  'kill-this-buffer
  "TAB" 'spacemacs/alternate-buffer
  "bh"  'spacemacs/home
  "be"  'spacemacs/safe-erase-buffer
  "bK"  'spacemacs/kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'spacemacs/kill-matching-buffers-rudely
  "bP"  'spacemacs/copy-clipboard-to-whole-buffer
  "bn"  'spacemacs/next-useful-buffer
  "bN"  'spacemacs/new-empty-buffer
  "bp"  'spacemacs/previous-useful-buffer
  "bR"  'spacemacs/safe-revert-buffer
  "bs"  'spacemacs/switch-to-scratch-buffer
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)
;; Cycling settings -----------------------------------------------------------
(spacemacs/set-leader-keys "Tn" 'spacemacs/cycle-spacemacs-theme)
;; errors ---------------------------------------------------------------------
(spacemacs/set-leader-keys
  "en" 'spacemacs/next-error
  "eN" 'spacemacs/previous-error
  "ep" 'spacemacs/previous-error)
;; file -----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "fc" 'spacemacs/copy-file
  "fD" 'spacemacs/delete-current-buffer-file
  "fei" 'spacemacs/find-user-init-file
  "fed" 'spacemacs/find-dotfile
  "feD" 'spacemacs/ediff-dotfile-and-template
  "feR" 'dotspacemacs/sync-configuration-layers
  "fev" 'spacemacs/display-and-copy-version
  "fCd" 'spacemacs/unix2dos
  "fCu" 'spacemacs/dos2unix
  "fg" 'rgrep
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fE" 'spacemacs/sudo-edit
  "fo" 'spacemacs/open-in-external-app
  "fR" 'spacemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'spacemacs/show-and-copy-buffer-filename)
;; help -----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdl" 'spacemacs/describe-last-keys
  "hdp" 'describe-package
  "hds" 'spacemacs/describe-system-info
  "hdt" 'describe-theme
  "hdv" 'describe-variable
  "hI"  'spacemacs/report-issue
  "hn"  'view-emacs-news)
;; insert stuff ---------------------------------------------------------------
(spacemacs/set-leader-keys
  "iJ" 'spacemacs/insert-line-below-no-indent
  "iK" 'spacemacs/insert-line-above-no-indent
  "ik" 'spacemacs/evil-insert-line-above
  "ij" 'spacemacs/evil-insert-line-below)
;; format ---------------------------------------------------------------------
;; <SPC> j k key binding for a frequent action: go and indent line below the point
;; <SPC> J split the current line at point and indent it
(spacemacs/set-leader-keys
  "jo" 'open-line
  "j=" 'spacemacs/indent-region-or-buffer
  "jS" 'spacemacs/split-and-new-line
  "jk" 'spacemacs/evil-goto-next-line-and-indent)

;; navigation/jumping ---------------------------------------------------------
(spacemacs/set-leader-keys
  "j0" 'spacemacs/push-mark-and-goto-beginning-of-line
  "j$" 'spacemacs/push-mark-and-goto-end-of-line
  "jb" 'bookmark-jump
  "jd" 'dired-jump
  "jD" 'dired-jump-other-window
  "jf" 'find-function-at-point
  "ji" 'spacemacs/jump-in-buffer
  "jv" 'find-variable-at-point)

;; Compilation ----------------------------------------------------------------
(spacemacs/set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cq" 'spacemacs/close-compilation-window)

;; narrow & widen -------------------------------------------------------------
(spacemacs/set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)
;; toggle ---------------------------------------------------------------------
(spacemacs|add-toggle highlight-current-line-globally
  :status global-hl-line-mode
  :on (global-hl-line-mode)
  :off (global-hl-line-mode -1)
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(spacemacs|add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Truncate long lines (no wrap)."
  :evil-leader "tl")
(spacemacs|add-toggle visual-line-navigation
  :status visual-line-mode
  :on (progn
        (visual-line-mode)
        (define-key evil-motion-state-map "j" 'evil-next-visual-line)
        (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
        (when (bound-and-true-p evil-escape-mode)
          (evil-escape-mode -1)
          (setq evil-escape-motion-state-shadowed-func nil)
          (define-key evil-motion-state-map "j" 'evil-next-visual-line)
          (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
          (evil-escape-mode)))
  :off (progn
         (visual-line-mode -1)
         (define-key evil-motion-state-map "j" 'evil-next-line)
         (define-key evil-motion-state-map "k" 'evil-previous-line)
         (when (bound-and-true-p evil-escape-mode)
           (evil-escape-mode -1)
           (setq evil-escape-motion-state-shadowed-func nil)
           (define-key evil-motion-state-map "j" 'evil-next-line)
           (define-key evil-motion-state-map "k" 'evil-previous-line)
           (evil-escape-mode)))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")
(spacemacs|add-toggle line-numbers
  :status linum-mode
  :on (linum-mode)
  :off (linum-mode -1)
  :documentation "Show the line numbers."
  :evil-leader "tn")
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
  :if (version< "24.3.50" emacs-version)
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
  :evil-leader "tmt")
(spacemacs|add-toggle transparent-frame
  :status nil
  :on (spacemacs/toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(spacemacs|add-toggle tool-bar
  :if window-system
  :status tool-bar-mode
  :on (tool-bar-mode)
  :off (tool-bar-mode -1)
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(spacemacs|add-toggle menu-bar
  :if (or window-system (version<= "24.3.1" emacs-version))
  :status menu-bar-mode
  :on (menu-bar-mode)
  :off (menu-bar-mode -1)
  :documentation "Display the menu bar."
  :evil-leader "Tm")
(spacemacs|add-toggle semantic-stickyfunc
  :status semantic-stickyfunc-mode
  :on (semantic-stickyfunc-mode)
  :off (semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "Ts")
(spacemacs|add-toggle semantic-stickyfunc-globally
  :status global-semantic-stickyfunc-mode
  :on (global-semantic-stickyfunc-mode)
  :off (global-semantic-stickyfunc-mode -1)
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-s")
;; quit -----------------------------------------------------------------------
(spacemacs/set-leader-keys
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

(spacemacs/set-leader-keys
  "w2"  'spacemacs/layout-double-columns
  "w3"  'spacemacs/layout-triple-columns
  "wb"  'spacemacs/switch-to-minibuffer-window
  "wd"  'delete-window
  "wD"  'ace-delete-window
  "wt"  'spacemacs/toggle-current-window-dedication
  "wf"  'follow-mode
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
  "wM"  'spacemacs/toggle-maximize-centered-buffer
  "wo"  'other-frame
  "wR"  'spacemacs/rotate-windows
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right
  "w="  'balance-windows)
;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(spacemacs/set-leader-keys
  "xa&" 'spacemacs/align-repeat-ampersand
  "xa(" 'spacemacs/align-repeat-left-paren
  "xa)" 'spacemacs/align-repeat-right-paren
  "xa," 'spacemacs/align-repeat-comma
  "xa." 'spacemacs/align-repeat-decimal
  "xa:" 'spacemacs/align-repeat-colon
  "xa;" 'spacemacs/align-repeat-semicolon
  "xa=" 'spacemacs/align-repeat-equal
  "xaa" 'align
  "xac" 'align-current
  "xam" 'spacemacs/align-repeat-math-oper
  "xar" 'spacemacs/align-repeat
  "xa|" 'spacemacs/align-repeat-bar
  "xc"  'count-region
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xls" 'spacemacs/sort-lines
  "xlu" 'spacemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'spacemacs/count-words-analysis)
;; shell ----------------------------------------------------------------------
(with-eval-after-load 'shell
  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

;; ivy/helm keys --------------------------------------------------------------

(defvar spacemacs--hjkl-completion-navigation-functions nil
  "Hook to adjust hjkl keys for completion (helm/ivy) navigation.
Each function in the hook is run with a single argument, which
when true should disable the hjkl keys.")

;; ---------------------------------------------------------------------------
;; Transient-states
;; ---------------------------------------------------------------------------

;; Buffer transient state

(spacemacs|define-transient-state buffer
  :title "Buffer Selection Transient State"
  :bindings
  ("n" spacemacs/next-useful-buffer "next")
  ("N" spacemacs/previous-useful-buffer "previous")
  ("p" spacemacs/previous-useful-buffer "previous")
  ("K" kill-this-buffer "kill")
  ("q" nil "quit" :exit t))
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

(spacemacs|define-transient-state window-manipulation
  :title "Window Manipulation Transient State"
  :doc
  "
Select^^^^               Move^^^^              Split^^                Resize^^                     Other^^
------^^^^------------- -----^^^^------------ ------^^-------------- -------^^------------------- ------^^-------------------
[_j_/_k_] down/up        [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
[_h_/_l_] left/right     [_h_/_l_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
[_0_-_9_] window N       [_R_]^^   rotate      [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
[_w_]^^   other window   ^^^^                  [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
[_o_]^^   other frame    ^^^^                  ^^                     ^^                           [_D_] close other
^^^^                     ^^^^                  ^^                     ^^                           [_g_] golden-ratio %`golden-ratio-mode
"
  :bindings
  ("q" nil :exit t)
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" spacemacs/shrink-window-horizontally)
  ("]" spacemacs/enlarge-window-horizontally)
  ("{" spacemacs/shrink-window)
  ("}" spacemacs/enlarge-window)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("g" spacemacs/toggle-golden-ratio)
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
  ("R" spacemacs/rotate-windows)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))
(spacemacs/set-leader-keys "w."
  'spacemacs/window-manipulation-transient-state/body)

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
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" spacemacs/scale-up-font)
  ("=" spacemacs/scale-up-font)
  ("-" spacemacs/scale-down-font)
  ("0" spacemacs/reset-font-size)
  ("q" nil :exit t))
(spacemacs/set-leader-keys "zx" 'spacemacs/scale-font-transient-state/body)

;; end of Text Manipulation Transient State

;; Transparency transient-state

(defun spacemacs/toggle-transparency ()
  "Toggle between transparent or opaque display."
  (interactive)
  (let* ((frame (selected-frame))
         (alpha (frame-parameter frame 'alpha))
         (dotfile-setting (cons dotspacemacs-active-transparency
                                dotspacemacs-inactive-transparency)))
    (set-frame-parameter
     frame 'alpha
     (if (not (equal alpha dotfile-setting))
         dotfile-setting
       '(100 . 100)))))

(defun spacemacs/increase-transparency ()
  "Increase transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter (selected-frame) 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun spacemacs/decrease-transparency ()
  "Decrease transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter (selected-frame) 'alpha
                           (cons decreased-alpha decreased-alpha)))))

(spacemacs|define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :bindings
  ("+" spacemacs/increase-transparency "increase")
  ("-" spacemacs/decrease-transparency "decrease")
  ("T" spacemacs/toggle-transparency "toggle")
  ("q" nil "quit" :exit t))
(spacemacs/set-leader-keys "TT"
  'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)

;; end of Transparency Transient State
