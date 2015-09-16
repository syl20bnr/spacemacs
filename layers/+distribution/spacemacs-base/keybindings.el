;;; keybindings.el --- Spacemacs Base Layer key-bindings File
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

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'spacemacs/toggle-maximize-buffer)

;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
;; can do the same thing and with fuzzy matching and other features.
(eval-after-load 'dired
  '(progn
     (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
     (define-key dired-mode-map "j" 'spacemacs/helm-find-files)))

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
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
;; evil-leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(evil-leader/set-key "u" 'universal-argument)
(when (memq dotspacemacs-editing-style '(vim hybrid))
  (define-key universal-argument-map
    (kbd (concat dotspacemacs-leader-key " u"))
    'universal-argument-more))
;; shell command  -------------------------------------------------------------
(evil-leader/set-key "!" 'shell-command)
;; applications ---------------------------------------------------------------
(evil-leader/set-key
  "ac"  'calc-dispatch
  "ad"  'dired
  "ap"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(evil-leader/set-key
  "bd"  'kill-this-buffer
  "TAB" 'spacemacs/alternate-buffer
  "bh"  'spacemacs/home
  "be"  'spacemacs/safe-erase-buffer
  "bK"  'spacemacs/kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'spacemacs/kill-matching-buffers-rudely
  "bP"  'spacemacs/copy-clipboard-to-whole-buffer
  "bn"  'spacemacs/next-useful-buffer
  "bp"  'spacemacs/previous-useful-buffer
  "bR"  'spacemacs/safe-revert-buffer
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)
;; Cycling settings -----------------------------------------------------------
(evil-leader/set-key "Tn" 'spacemacs/cycle-spacemacs-theme)
;; describe functions ---------------------------------------------------------
(defmacro spacemacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "spacemacs/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (evil-leader/set-key ,keys ',func-name))))
(spacemacs||set-helm-key "hdb" describe-bindings)
(spacemacs||set-helm-key "hdc" describe-char)
(spacemacs||set-helm-key "hdf" describe-function)
(spacemacs||set-helm-key "hdk" describe-key)
(spacemacs||set-helm-key "hdm" describe-mode)
(spacemacs||set-helm-key "hdp" describe-package)
(evil-leader/set-key "hds" 'spacemacs/describe-system-info)
(spacemacs||set-helm-key "hdt" describe-theme)
(spacemacs||set-helm-key "hdv" describe-variable)
(spacemacs||set-helm-key "hL"  helm-locate-library)
;; search functions -----------------------------------------------------------
(spacemacs||set-helm-key "sww" helm-wikipedia-suggest)
(spacemacs||set-helm-key "swg" helm-google-suggest)
;; errors ---------------------------------------------------------------------
(evil-leader/set-key
  "en" 'spacemacs/next-error
  "ep" 'spacemacs/previous-error
  "eN" 'spacemacs/previous-error)
;; file -----------------------------------------------------------------------
(evil-leader/set-key
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
  "fo" 'spacemacs/open-in-external-app
  "fR" 'spacemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'spacemacs/write-file
  "fy" 'spacemacs/show-and-copy-buffer-filename)
;; insert stuff ---------------------------------------------------------------
(evil-leader/set-key
  "iJ" 'spacemacs/insert-line-below-no-indent
  "iK" 'spacemacs/insert-line-above-no-indent
  "ik" 'spacemacs/evil-insert-line-above
  "ij" 'spacemacs/evil-insert-line-below)
;; format ---------------------------------------------------------------------
;; <SPC> j k key binding for a frequent action: go and indent line below the point
;; <SPC> J split the current line at point and indent it
(evil-leader/set-key
  "J"  'sp-split-sexp
  "jj" 'sp-newline
  "jo" 'open-line
  "j=" 'spacemacs/indent-region-or-buffer
  "jJ" 'spacemacs/split-and-new-line
  "jk" 'spacemacs/evil-goto-next-line-and-indent)

;; navigation -----------------------------------------------------------------
(evil-leader/set-key
  "jh" 'spacemacs/push-mark-and-goto-beginning-of-line
  "jl" 'spacemacs/push-mark-and-goto-end-of-line)
;; Compilation ----------------------------------------------------------------
(evil-leader/set-key "cC" 'compile)
(evil-leader/set-key "cr" 'recompile)
;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key
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
  :status nil
  :on (toggle-truncate-lines)
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
  :on (global-linum-mode)
  :off (global-linum-mode -1)
  :documentation "Show the line numbers."
  :evil-leader "tn")
(spacemacs|add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(spacemacs|add-toggle debug-on-error
  :status nil
  :on (toggle-debug-on-error)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(spacemacs|add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(spacemacs|add-toggle fullscreen-frame
  :status nil
  :on (spacemacs/toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(spacemacs|add-toggle maximize-frame
  :if (version< "24.3.50" emacs-version)
  :status nil
  :on (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(spacemacs|add-toggle mode-line
  :status hidden-mode-line-mode
  :on (hidden-mode-line-mode)
  :off (hidden-mode-line-mode -1)
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
  "w2"  'spacemacs/layout-double-columns
  "w3"  'spacemacs/layout-triple-columns
  "wb"  'spacemacs/switch-to-minibuffer-window
  "wc"  'delete-window
  "wd"  'spacemacs/toggle-current-window-dedication
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
(evil-leader/set-key
  "xaa" 'align
  "xar" 'spacemacs/align-repeat
  "xam" 'spacemacs/align-repeat-math-oper
  "xa." 'spacemacs/align-repeat-decimal
  "xa," 'spacemacs/align-repeat-comma
  "xa;" 'spacemacs/align-repeat-semicolon
  "xa:" 'spacemacs/align-repeat-colon
  "xa=" 'spacemacs/align-repeat-equal
  "xa&" 'spacemacs/align-repeat-ampersand
  "xa|" 'spacemacs/align-repeat-bar
  "xa(" 'spacemacs/align-repeat-left-paren
  "xa)" 'spacemacs/align-repeat-right-paren
  "xdw" 'delete-trailing-whitespace
  "xls" 'spacemacs/sort-lines
  "xlu" 'spacemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'spacemacs/count-words-analysis
  "xwc" 'count-words-region)
;; google translate -----------------------------------------------------------
(evil-leader/set-key
  "xgl" 'spacemacs/set-google-translate-languages)
;; shell ----------------------------------------------------------------------
(eval-after-load "shell"
  '(progn
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input)))

;; ---------------------------------------------------------------------------
;; Micro-states
;; ---------------------------------------------------------------------------

;; Buffer micro state

(spacemacs|define-micro-state buffer
  :doc "[n]ext [p]revious [K]ill [q]uit"
  :disable-evil-leader t
  :evil-leader "b."
  :bindings
  ("K" kill-this-buffer)
  ("n" spacemacs/next-useful-buffer)
  ("N" spacemacs/previous-useful-buffer)
  ("p" spacemacs/previous-useful-buffer)
  ("q" nil :exit t))

;; end of Buffer micro state

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
  [-] [/] [s] [v] [S] [V]   split windows below|right and focus
  [c] [C]                   close current|other windows
  [g]                       toggle golden-ratio
  [h] [j] [k] [l]           go to left|bottom|top|right
  [H] [J] [K] [L]           move windows to far/very left|bottom|top|right
  [[] []] [{] [}]           shrink/enlarge horizontally and vertically respectively
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
  :use-minibuffer t
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
  ("<left>" evil-window-left                 :doc (spacemacs//window-manipulation-move-doc))
  ("j" evil-window-down                      :doc (spacemacs//window-manipulation-move-doc))
  ("<down>" evil-window-down                 :doc (spacemacs//window-manipulation-move-doc))
  ("k" evil-window-up                        :doc (spacemacs//window-manipulation-move-doc))
  ("<up>" evil-window-up                     :doc (spacemacs//window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (spacemacs//window-manipulation-move-doc))
  ("<right>" evil-window-right               :doc (spacemacs//window-manipulation-move-doc))
  ("H" evil-window-move-far-left             :doc (spacemacs//window-manipulation-move-doc))
  ("<S-left>" evil-window-move-far-left      :doc (spacemacs//window-manipulation-move-doc))
  ("J" evil-window-move-very-bottom          :doc (spacemacs//window-manipulation-move-doc))
  ("<S-down>" evil-window-move-very-bottom   :doc (spacemacs//window-manipulation-move-doc))
  ("K" evil-window-move-very-top             :doc (spacemacs//window-manipulation-move-doc))
  ("<S-up>" evil-window-move-very-top        :doc (spacemacs//window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (spacemacs//window-manipulation-move-doc))
  ("<S-right>" evil-window-move-far-right    :doc (spacemacs//window-manipulation-move-doc))
  ("o" other-frame                           :doc (spacemacs//window-manipulation-move-doc))
  ("R" spacemacs/rotate-windows              :doc (spacemacs//window-manipulation-move-doc))
  ("s" split-window-below                    :doc (spacemacs//window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("u" winner-undo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (spacemacs//window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("w" other-window                          :doc (spacemacs//window-manipulation-move-doc)))

;; end of Window Manipulation Micro State

;; text Manipulation Micro State

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

(spacemacs|define-micro-state scale-font
  :doc "[+] scale up [-] scale down [=] reset font [q]uit"
  :evil-leader "zx"
  :bindings
  ("+" spacemacs/scale-up-font)
  ("-" spacemacs/scale-down-font)
  ("=" spacemacs/reset-font-size)
  ("q" nil :exit t))

;; end of Text Manipulation Micro State

;; Transparency micro-state

(defun spacemacs/toggle-transparency ()
  "Toggle between transparent or opaque display."
  (interactive)
  ;; Define alpha if it's nil
  (if (eq (frame-parameter (selected-frame) 'alpha) nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  ;; Do the actual toggle
  (if (/= (cadr (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (set-frame-parameter (selected-frame) 'alpha
                         (list dotspacemacs-active-transparency
                               dotspacemacs-inactive-transparency)))
  ;; Immediately enter the micro-state, but also keep toggle
  ;; accessible from helm-spacemacs
  (spacemacs/scale-transparency-micro-state))

(defun spacemacs/increase-transparency ()
  "Increase transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter (selected-frame) 'alpha (list increased-alpha increased-alpha)))))

(defun spacemacs/decrease-transparency ()
  "Decrease transparency of current frame."
  (interactive)
  (let* ((current-alpha (car (frame-parameter (selected-frame) 'alpha)))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter (selected-frame) 'alpha (list decreased-alpha decreased-alpha)))))

(spacemacs|define-micro-state scale-transparency
  :doc "[+] increase [-] decrease [T] toggle transparency [q] quit"
  :bindings
  ("+" spacemacs/increase-transparency)
  ("-" spacemacs/decrease-transparency)
  ("T" spacemacs/toggle-transparency)
  ("q" nil :exit t))

;; end of Transparency Micro State
