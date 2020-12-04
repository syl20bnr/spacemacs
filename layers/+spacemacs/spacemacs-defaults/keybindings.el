;;; keybindings.el --- Space-macs Defaults Layer key-bindings File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------

;; We define prefix commands only for the sake of which-key
(setq space-macs/key-binding-prefixes `((,dotspace-macs-e-macs-command-key "M-x")
                                       ("!"   "shell cmd")
                                       ("*"   "search project w/input")
                                       ("/"   "search project")
                                       ("?"   "show keybindings")
                                       ("a"   "applications")
                                       ("ac"   "chat")
                                       ("ae"   "email")
                                       ("af"   "fun")
                                       ("ar"   "readers")
                                       ("am"   "music")
                                       ("at"  "tools")
                                       ("ats"  "shells")
                                       ("aw"  "web-services")
                                       ("c"   "compile/comments")
                                       ("C"   "capture/colors")
                                       ("e"   "errors")
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
                                       ("tEe" "e-macs (holy-mode)")
                                       ("tEh" "hybrid (hybrid-mode)")
                                       ("th"  "highlight")
                                       ("tm"  "modeline")
                                       ("tt"  "timeclock")
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
                                       ("xR"  "Randomize")
                                       ("xt"  "transpose")
                                       ("xw"  "words")
                                       ("z"   "zoom")))
(mapc (lambda (x) (apply #'space-macs/declare-prefix x))
      space-macs/key-binding-prefixes)

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'space-macs/toggle-maximize-buffer)
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
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'space-macs/md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'space-macs/mu-select-linum)
(global-set-key (kbd "<left-margin> <double-mouse-1>") 'space-macs/select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'space-macs/mu-select-linum)

;; ---------------------------------------------------------------------------
;; space-macs leader key bindings
;; ---------------------------------------------------------------------------

;; Universal argument ---------------------------------------------------------
(space-macs/set-leader-keys "u" 'universal-argument)
(when (memq dotspace-macs-editing-style '(vim hybrid))
  (define-key universal-argument-map
    (kbd (concat dotspace-macs-leader-key " u"))
    'universal-argument-more))
;; shell command  -------------------------------------------------------------
(space-macs/set-leader-keys "!" 'shell-command)
;; kmacros --------------------------------------------------------------------
(space-macs|spacebind
 "Operations on rectangular selections of text."
 :global
 (("K" "Keyboard Macros"
   ("c" "Counter"
    ("a" kmacro-add-counter "Increment counter")
    ("c" kmacro-insert-counter "Insert counter")
    ("C" kmacro-set-counter "Set counter...")
    ("f" kmacro-set-format "Set display format..."))
   ("e" "Edit"
    ("b" kmacro-bind-to-key "Assign key binding...")
    ("e" kmacro-edit-macro-repeat "Edit last macro")
    ("l" kmacro-edit-lossage "Create macro from lossage...")
    ("n" kmacro-name-last-macro "Name last macro...")
    ("r" kmacro-to-register "Write macro to register...")
    ("s" kmacro-step-edit-macro "Step by step edit..."))
   ("k" kmacro-start-macro-or-insert-counter "Start macro/Insert counter")
   ("K" kmacro-end-or-call-macro "Stop or Run")
   ("r" "Ring"
    ("L" kmacro-view-ring-2nd "Display ring head")
    ("d" kmacro-delete-ring-head "Delete ring head")
    ("l" kmacro-call-ring-2nd-repeat "Run 2nd macro in ring")
    ("n" kmacro-cycle-ring-next "Next in ring")
    ("p" kmacro-cycle-ring-previous "Previous in ring")
    ("s" kmacro-swap-ring "Swap first two"))
   ("v" kmacro-view-macro-repeat "View last macro"))))
;; rectangles ------------------------------------------------------------------
(space-macs|spacebind
 "Operations on rectangular selections of text."
 :global
 (("C-v" "Rectangles"
   ("c" close-rectangle "Delete whitespace after")
   ("d" delete-rectangle "Delete text")
   ("e" rectangle-exchange-point-and-mark "Go to corner")
   ("i" copy-rectangle-to-register "Copy into register...")
   ("k" kill-rectangle "Delete and save")
   ("l" rectangle-left-char "Move left past EOL")
   ("m" rectangle-mark-mode "Toggle region as rectangular")
   ("n" rectangle-next-line "Go to next line past EOL")
   ("N" rectangle-number-lines "Insert line number")
   ("o" open-rectangle "Shift text right")
   ("p" rectangle-previous-line "Go to prev. line past EOL")
   ("r" rectangle-right-char "Move right past EOL")
   ("s" string-rectangle "Replace lines with string...")
   ("x" clear-rectangle "Blank out rectangle")
   ("y" yank-rectangle "Paste last rectangle"))))
;; applications ---------------------------------------------------------------
(space-macs/set-leader-keys
  "a*"  'calc-dispatch
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; easy pg ----------------------------------------------------------------------
(space-macs|spacebind
 "Encrypt / decrypt files with Easy PG"
 :global
 (("a" "applications"
   ("Y"  "easy pg"
    ("d" epa-decrypt-file "Decrypt file to...")
    ("D" epa-delete-keys  "Delete keys...")
    ("e" epa-encrypt-file "Encrypt file...")
    ("i" epa-insert-keys  "Insert keys...")
    ("k" epa-list-keys "List keys...")
    ("K" epa-list-secret-keys "List secret keys...")
    ("x" epa-export-keys "Export keys...")
    ("s"  "sign"
     ("f" epa-sign-file "Sign file...")
     ("m" epa-sign-mail "Sign mail...")
     ("r" epa-sign-region "Sign region..."))
    ("v"  "verify"
     ("f" epa-verify-file "Verify file...")
     ("r" epa-verify-region "Verify region...")
     ("c" epa-verify-cleartext-in-region "Verify cleartext region..."))))))
;; buffers --------------------------------------------------------------------
(space-macs|spacebind
 "Compare buffers, files and directories."
 :global
 (("TAB" space-macs/alternate-buffer "Last buffer")
  ("b" "Buffers"
   ("N"  "New buffer"
    ("C-i" make-indirect-buffer "New indirect buffer...")
    ("f" space-macs/new-empty-buffer-new-frame "New buffer (new frame)")
    ("h" space-macs/new-empty-buffer-left "New buffer (left split)")
    ("i" clone-indirect-buffer "Clone buffer")
    ("I" clone-indirect-buffer-other-window-without-purpose "Clone buffer (other window)")
    ("j" space-macs/new-empty-buffer-below "New buffer (open below)")
    ("k" space-macs/new-empty-buffer-above "New buffer (open above)")
    ("l" space-macs/new-empty-buffer-right "New buffer (right split)")
    ("n" space-macs/new-empty-buffer "New buffer"))
   ("1" buffer-to-window-1 "Move buffer to window 1")
   ("2" buffer-to-window-2 "Move buffer to window 2")
   ("3" buffer-to-window-3 "Move buffer to window 3")
   ("4" buffer-to-window-4 "Move buffer to window 4")
   ("5" buffer-to-window-5 "Move buffer to window 5")
   ("6" buffer-to-window-6 "Move buffer to window 6")
   ("7" buffer-to-window-7 "Move buffer to window 7")
   ("8" buffer-to-window-8 "Move buffer to window 8")
   ("9" buffer-to-window-9 "Move buffer to window 9")
   ("C-d" space-macs/kill-other-buffers "Kill other buffers...")
   ("C-S-d" space-macs/kill-matching-buffers-rudely "Kill buffers...")
   ("d" space-macs/kill-this-buffer "Kill buffer")
   ("e" space-macs/safe-erase-buffer "Erase...")
   ("h" space-macs/home "Space-macs home buffer")
   ("H" space-macs/switch-to-help-buffer "Help buffer")
   ("n" next-buffer "Next buffer")
   ("m" space-macs/switch-to-messages-buffer "Messages buffer")
   ("P" space-macs/copy-clipboard-to-whole-buffer "Paste and replace buffer")
   ("p" previous-buffer "Previous buffer")
   ("R" space-macs/safe-revert-buffer "Revert buffer...")
   ("s" space-macs/switch-to-scratch-buffer "Scratch buffer")
   ("u" space-macs/reopen-killed-buffer "Reopen last killed buffer")
   ("x" kill-buffer-and-window "Kill buffer and close window")
   ("Y" space-macs/copy-whole-buffer-to-clipboard "Copy buffer")
   ("w" read-only-mode "Toggle read-only"))))
;; Cycling settings -----------------------------------------------------------
(space-macs|define-transient-state theme
  :title "Themes Transient State"
  :doc "\n[_n_/_<right>_] next  [_N_/_p_/_<left>_] previous  [_t_/_<up>_] list themes"
  :bindings
  ("n" space-macs/cycle-space-macs-theme)
  ("N" space-macs/cycle-space-macs-theme-backward)
  ("p" space-macs/cycle-space-macs-theme-backward)
  ("t" space-macs/theme-loader)
  ("<up>" space-macs/theme-loader)
  ("<right>" space-macs/cycle-space-macs-theme)
  ("<left>" space-macs/cycle-space-macs-theme-backward))
(space-macs/set-leader-keys "Tn"
  'space-macs/theme-transient-state/space-macs/cycle-space-macs-theme)
(space-macs/set-leader-keys "TN"
  'space-macs/theme-transient-state/space-macs/cycle-space-macs-theme-backward)
;; errors ---------------------------------------------------------------------
(space-macs/set-leader-keys
  "ez" 'space-macs/last-error
  "en" 'space-macs/next-error
  "eN" 'space-macs/previous-error
  "ep" 'space-macs/previous-error)
(space-macs|define-transient-state error
  :title "Error Transient State"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (space-macs/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'e-macs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when space-macs--gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- space-macs--gne-cur-line
                                              space-macs--gne-min-line)))
                                (1+ (- space-macs--gne-max-line
                                       space-macs--gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" space-macs/next-error "next")
  ("p" space-macs/previous-error "prev")
  ("N" space-macs/previous-error "prev")
  ("z" recenter-top-bottom "recenter")
  ("q" nil "quit" :exit t)
  :evil-leader "e.")
;; ediff ----------------------------------------------------------------------
(space-macs|spacebind
 "Compare buffers, files and directories."
 :global
 (("D" "Diff/Compare"
   ("b"  "Buffers"
    ("3" ediff-buffers3 "Between 3 buffers...")
    ("b" ediff-buffers "Between 2 buffers...")
    ("B" ediff-backup "With backup file...")
    ("p" ediff-patch-buffer "With a patch..."))
   ("d" "Directories"
    ("3" ediff-directories3 "Between 3 directories...")
    ("d" ediff-directories "Between 2 directories...")
    ("r" ediff-directory-revisions "Using SCM revisions..."))
   ("f" "Files"
    ("." space-macs/ediff-dotfile-and-template "With Space-macs dotfile")
    ("3" ediff-files3 "Between 3 files...")
    ("f" ediff-files "Between 2 files...")
    ("p" ediff-patch-file "With a patch...")
    ("v" ediff-revision "Between file revisions..."))
   ("m" "Merge"
    ("b" "Buffers"
     ("3" ediff-merge-buffers-with-ancestor "3-way merge...")
     ("b" ediff-merge-buffers "2-way merge..."))
    ("d" "Directories"
     ("3" ediff-merge-directories-with-ancestor "3-way merge...")
     ("d" ediff-merge-directories "2-way merge..."))
    ("f" "Files"
     ("3" ediff-merge-files-with-ancestor "3-way merge...")
     ("f" ediff-merge-files "2-way merge..."))
    ("r" "Revisions"
     ("3" ediff-merge-revisions-with-ancestor "3-way merge...")
     ("r" ediff-merge-revisions "2-way merge...")))
   ("r" "Regions"
    ("l" ediff-regions-linewise "Between 2 regions (linewise)...")
    ("w" ediff-regions-wordwise "Between 2 regions (wordwise)..."))
   ("w" "Windows"
    ("l" ediff-windows-linewise "Linewise between visible text...")
    ("w" ediff-windows-wordwise "Wordwise between visible text..."))
   ("s" ediff-show-registry "Show registry")
   ("h" ediff-documentation "Documentation"))))
;; file -----------------------------------------------------------------------
(space-macs|spacebind
 "Files manipulation."
 :global
 (("f" "Files"
   ("A" space-macs/find-file-and-replace-buffer "Set another file for buffer...")
   ("c" space-macs/copy-file "Copy file to new file...")
   ("D" space-macs/delete-current-buffer-file "Delete...")
   ("i" space-macs/insert-file "Insert file content...")
   ("l" find-file-literally "Open file literally...")
   ("E" space-macs/sudo-edit "Open using sudo...")
   ("o" space-macs/open-file-or-directory-in-external-app "Open with external app")
   ("R" space-macs/rename-current-buffer-file "Rename...")
   ("S" evil-write-all "Save all")
   ("s" save-buffer "Save")
   ("C"  "Convert"
    ("d" space-macs/unix2dos "Convert to DOS")
    ("u" space-macs/dos2unix "Convert to UNIX"))
   ("e" "e-macs/Space-macs"
    ("C-e" space-macs/force-init-space-macs-env "Recreate env. variables file")
    ("c" space-macs/recompile-elpa "Recompile packages")
    ("i" space-macs/find-user-init-file "Open e-macs \"init.el\"")
    ("d" space-macs/find-dotfile "Open Space-macs dotfile")
    ("D" space-macs/ediff-dotfile-and-template "Diff. with dotfile template")
    ("e" space-macs/edit-env "Open \".space-macs.env\"")
    ("E" dotspace-macs/call-user-env "Refresh env. variables")
    ("R" dotspace-macs/sync-configuration-layers "Reload configuration")
    ("v" space-macs/display-and-copy-version "Copy Space-macs version")
    ("U" configuration-layer/update-packages "Update packages..."))
   ("v" "Variables"
    ("d" add-dir-local-variable "Add directory-local variable...")
    ("f" add-file-local-variable "Add bottom file variable...")
    ("p" add-file-local-variable-prop-line "Add top file property...")
    )
   ("y" "Yank/Copy"
    ("c" space-macs/copy-file-path-with-line-column "File path with line and column")
    ("d" space-macs/copy-directory-path "Directory path")
    ("l" space-macs/copy-file-path-with-line "File path with line number")
    ("n" space-macs/copy-file-name "File name")
    ("N" space-macs/copy-file-name-base "File name without extension")
    ("y" space-macs/copy-file-path "File path")
    ("b" space-macs/copy-buffer-name "Buffer name")))))
;; frame ----------------------------------------------------------------------
(space-macs/set-leader-keys
  "Ff" 'space-macs/find-file-other-frame
  "Fd" 'delete-frame
  "FD" 'delete-other-frames
  "Fb" 'space-macs/switch-to-buffer-other-frame
  "FB" 'space-macs/display-buffer-other-frame
  "Fo" 'other-frame
  "FO" 'space-macs/dired-other-frame
  "Fn" 'make-frame)
;; help -----------------------------------------------------------------------
(defalias 'e-macs-tutorial 'help-with-tutorial)
(space-macs/set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdl" 'space-macs/describe-last-keys
  "hdp" 'describe-package
  "hdP" 'configuration-layer/describe-package
  "hds" 'space-macs/describe-system-info
  "hdt" 'describe-text-properties
  "hdT" 'describe-theme
  "hdv" 'describe-variable
  "hI"  'space-macs/report-issue
  "hn"  'view-e-macs-news
  "hPs" 'profiler-start
  "hPk" 'profiler-stop
  "hPr" 'profiler-report
  "hPw" 'profiler-report-write-profile
  "hTe" 'e-macs-tutorial)
;; insert stuff ---------------------------------------------------------------
(space-macs/set-leader-keys
  "iJ" 'space-macs/insert-line-below-no-indent
  "iK" 'space-macs/insert-line-above-no-indent
  "ik" 'space-macs/evil-insert-line-above
  "ij" 'space-macs/evil-insert-line-below
  "ib" 'insert-buffer)
;; format ---------------------------------------------------------------------
(space-macs/set-leader-keys
  "j(" 'check-parens
  "j=" 'space-macs/indent-region-or-buffer
  "j+" 'space-macs/iwb-region-or-buffer
  "jo" 'open-line
  "jS" 'space-macs/split-and-new-line
  "jk" 'space-macs/evil-goto-next-line-and-indent)

;; navigation/jumping ---------------------------------------------------------
(space-macs/set-leader-keys
  "j0" 'space-macs/push-mark-and-goto-beginning-of-line
  "j$" 'space-macs/push-mark-and-goto-end-of-line
  "jc" 'goto-last-change
  "jf" 'find-function
  "jv" 'find-variable)

;; Compilation ----------------------------------------------------------------
(space-macs/set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cn" 'next-error
  "cN" 'previous-error
  "cd" 'space-macs/show-hide-compilation-window
  "cb" 'space-macs/switch-to-compilation-buffer)
(with-eval-after-load 'compile
  (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))
;; narrow & widen -------------------------------------------------------------
(space-macs/set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nR" 'space-macs/narrow-to-region-indirect-buffer
  "nP" 'space-macs/narrow-to-page-indirect-buffer
  "nF" 'space-macs/narrow-to-defun-indirect-buffer
  "nw" 'widen)
;; toggle ---------------------------------------------------------------------
(space-macs|add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(space-macs|add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Toggle between line wrapping or truncation (no wrap)."
  :evil-leader "tl")
(space-macs|add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (space-macs//init-visual-line-keys)
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")
(space-macs|add-toggle visual-line-navigation-globally
  :status global-visual-line-mode
  :on
  (progn
    (global-visual-line-mode)
    (space-macs//init-visual-line-keys)
    (evil-normalize-keymaps))
  :off
  (progn
    (global-visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines globally."
  :evil-leader "t C-S-l")
(space-macs|add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(space-macs|add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(space-macs|add-toggle fringe
  :if (fboundp 'fringe-mode)
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(space-macs|add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (space-macs/toggle-frame-fullscreen)
  :off (space-macs/toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(space-macs|add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(space-macs|add-toggle mode-line
  :status (not hidden-mode-line-mode)
  :on (hidden-mode-line-mode -1)
  :off (hidden-mode-line-mode)
  :documentation "Toggle the visibility of modeline."
  :evil-leader "tmT")
(space-macs|add-toggle display-time
  :mode display-time-mode
  :documentation "Display time in modeline."
  :evil-leader "tmt")
(space-macs|add-toggle syntax-highlighting
  :mode font-lock-mode
  :documentation "Toggle syntax highlighting."
  :evil-leader "ths")
(space-macs|add-toggle zero-based-column-indexing
  :documentation "Toggle column indexing starting at 0 versus 1.

This is achieved by the built in functionality available in e-macs 26 by changing
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

(space-macs|add-toggle transparent-frame
  :status nil
  :on (space-macs/toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(space-macs|add-toggle tool-bar
  :if window-system
  :mode tool-bar-mode
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(space-macs|add-toggle menu-bar
  :if window-system
  :mode menu-bar-mode
  :documentation "Display the menu bar."
  :evil-leader "Tm")
;; quit -----------------------------------------------------------------------
(space-macs/set-leader-keys
  "qs" 'space-macs/save-buffers-kill-e-macs
  "qq" 'space-macs/prompt-kill-e-macs
  "qQ" 'space-macs/kill-e-macs
  "qf" 'space-macs/frame-killer)
;; timeclock ------------------------------------------------------------------
(space-macs/set-leader-keys
  "ttc" 'timeclock-change
  "tte" 'timeclock-workday-elapsed-string
  "ttg" 'timeclock-workday-remaining-string
  "tti" 'timeclock-in
  "ttl" 'timeclock-when-to-leave-string
  "ttm" 'timeclock-modeline-display
  "tto" 'timeclock-out
  "ttr" 'timeclock-reread-log
  "tts" 'timeclock-status-string
  "ttu" 'timeclock-update-mode-line
  "ttv" 'timeclock-visit-timelog
  "ttw" 'timeclock-when-to-leave-string)
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

(space-macs/set-leader-keys
  "w TAB"  'space-macs/alternate-window
  "w1"  'space-macs/window-split-single-column
  "w2"  'space-macs/window-split-double-columns
  "w3"  'space-macs/window-split-triple-columns
  "w4"  'space-macs/window-split-grid
  "wb"  'space-macs/switch-to-minibuffer-window
  "wd"  'space-macs/delete-window
  "wt"  'space-macs/toggle-current-window-dedication
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
  "wm"  'space-macs/toggle-maximize-buffer
  "wcc"  'space-macs/toggle-centered-buffer
  "wcC"  'space-macs/toggle-distraction-free
  "wc."  'space-macs/centered-buffer-transient-state
  "wo"  'other-frame
  "wr"  'space-macs/rotate-windows-forward
  "wR"  'space-macs/rotate-windows-backward
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
  "w+"  'space-macs/window-layout-toggle
  "w_"  'space-macs/maximize-horizontally
  "w|"  'space-macs/maximize-vertically)
;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(space-macs/set-leader-keys
  "xa%" 'space-macs/align-repeat-percent
  "xa&" 'space-macs/align-repeat-ampersand
  "xa(" 'space-macs/align-repeat-left-paren
  "xa)" 'space-macs/align-repeat-right-paren
  "xa{" 'space-macs/align-repeat-left-curly-brace
  "xa}" 'space-macs/align-repeat-right-curly-brace
  "xa[" 'space-macs/align-repeat-left-square-brace
  "xa]" 'space-macs/align-repeat-right-square-brace
  "xa," 'space-macs/align-repeat-comma
  "xa." 'space-macs/align-repeat-decimal
  "xa:" 'space-macs/align-repeat-colon
  "xa;" 'space-macs/align-repeat-semicolon
  "xa=" 'space-macs/align-repeat-equal
  "xa\\" 'space-macs/align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'space-macs/align-repeat-math-oper
  "xar" 'space-macs/align-repeat
  "xa|" 'space-macs/align-repeat-bar
  "xc"  'count-region
  "xd SPC" 'cycle-spacing
  "xdl" 'delete-blank-lines
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xlc" 'space-macs/sort-lines-by-column
  "xlC" 'space-macs/sort-lines-by-column-reverse
  "xld" 'space-macs/duplicate-line-or-region
  "xlk" 'space-macs/kill-back-to-indentation
  "xlr" 'space-macs/randomize-lines
  "xls" 'space-macs/sort-lines
  "xlS" 'space-macs/sort-lines-reverse
  "xlu" 'space-macs/uniquify-lines
  "xtc" 'transpose-chars
  "xte" 'transpose-sexps
  "xtl" 'transpose-lines
  "xtp" 'transpose-paragraphs
  "xts" 'transpose-sentences
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'space-macs/count-words-analysis
  "xwr" 'space-macs/randomize-words
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

(space-macs|define-transient-state buffer
  :title "Buffer Transient State"
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
  ("d" space-macs/kill-this-buffer)
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
(space-macs/set-leader-keys "b." 'space-macs/buffer-transient-state/body)

;; end of Buffer Transient State

;; Window Transient State

(defun space-macs/shrink-window-horizontally (delta)
  "Wrap `space-macs/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun space-macs/shrink-window (delta)
  "Wrap `space-macs/shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun space-macs/enlarge-window (delta)
  "Wrap `space-macs/enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun space-macs/enlarge-window-horizontally (delta)
  "Wrap `space-macs/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defvar space-macs--window-ts-full-hint-toggle nil
  "Display window transient state documentation.")

(defun space-macs//window-ts-toggle-hint ()
  "Toggle the full hint docstring for the window transient state."
  (interactive)
  (setq space-macs--window-ts-full-hint-toggle
        (not space-macs--window-ts-full-hint-toggle)))

(defun space-macs//window-ts-hint ()
  "Return a condensed/full hint for the window transient state"
  (concat
   " "
   (if space-macs--window-ts-full-hint-toggle
       space-macs--window-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
             space-macs--window-ts-minified-hint))))

(space-macs|transient-state-format-hint window
  space-macs--window-ts-minified-hint "\n
Select: _a_ _h_ _j_ _k_ _l_ _w_ _0_.._9_ Move: _H_ _J_ _K_ _L_ _r_ _R_ Split: _s_ _v_ Resize: _[_ _]_ _{_ _}_ _m_ _|_ ___")

(space-macs|transient-state-format-hint window
  space-macs--window-ts-full-hint
  (format "\n[_?_] toggle help
 Select^^^^               Move^^^^              Split^^^^^^               Resize^^             Other^^
 â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
 [_j_/_k_]  down/up       [_J_/_K_] down/up     [_s_]^^^^ horizontal      [_[_] shrink horiz   [_d_] close current
 [_h_/_l_]  left/right    [_H_/_L_] left/right  [_S_]^^^^ horiz & follow  [_]_] enlarge horiz  [_D_] close other
 [_0_.._9_] window 0..9   [_r_]^^   rotate fwd  [_v_]^^^^ vertical        [_{_] shrink verti   [_u_] restore prev layout
 [_a_]^^    ace-window    [_R_]^^   rotate bwd  [_V_]^^^^ verti & follow  [_}_] enlarge verti  [_U_] restore next layout
 [_o_]^^    other frame   ^^^^                  [_m_/_|_/___] maximize    %s^^^^^^^^^^^^^^^^^  [_q_] quit
 [_w_]^^    other window"
          (if (configuration-layer/package-used-p 'golden-ratio)
              ;; the following strings need to be the same length as:
              ;; %s^^^^^^^^^^^^^^^^^ (above) to keep the following key aligned
              "[_g_] golden-ratio "
            "^^                 ")))

(space-macs|define-transient-state window
  :title "Window Transient State"
  :hint-is-doc t
  :dynamic-hint (space-macs//window-ts-hint)
  :bindings
  ("?" space-macs//window-ts-toggle-hint)
  ;; Select
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
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
  ("a" ace-window)
  ("o" other-frame)
  ("w" other-window)
  ;; Move
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("r" space-macs/rotate-windows-forward)
  ("R" space-macs/rotate-windows-backward)
  ;; Split
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("-" split-window-below-and-focus)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("/" split-window-right-and-focus)
  ("m" space-macs/toggle-maximize-buffer)
  ("|" space-macs/maximize-vertically)
  ("_" space-macs/maximize-horizontally)
  ;; Resize
  ("[" space-macs/shrink-window-horizontally)
  ("]" space-macs/enlarge-window-horizontally)
  ("{" space-macs/shrink-window)
  ("}" space-macs/enlarge-window)
  ;; Other
  ("d" delete-window)
  ("D" delete-other-windows)
  ("u" winner-undo)
  ("U" winner-redo)
  ("q" nil :exit t))
(space-macs/set-leader-keys
  "w." 'space-macs/window-transient-state/body
  "w[" 'space-macs/window-transient-state/space-macs/shrink-window-horizontally
  "w]" 'space-macs/window-transient-state/space-macs/enlarge-window-horizontally
  "w{" 'space-macs/window-transient-state/space-macs/shrink-window
  "w}" 'space-macs/window-transient-state/space-macs/enlarge-window)

;; end of Window Transient State

;; Text Transient State

(defun space-macs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun space-macs/scale-up-font ()
  "Scale up the font."
  (interactive)
  (space-macs/scale-up-or-down-font-size 1))

(defun space-macs/scale-down-font ()
  "Scale up the font."
  (interactive)
  (space-macs/scale-up-or-down-font-size -1))

(defun space-macs/reset-font-size ()
  "Reset the font size."
  (interactive)
  (space-macs/scale-up-or-down-font-size 0))

(space-macs|define-transient-state scale-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_/_k_] scale up [_-_/___/_j_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" space-macs/scale-up-font)
  ("=" space-macs/scale-up-font)
  ("k" space-macs/scale-up-font)
  ("-" space-macs/scale-down-font)
  ("_" space-macs/scale-down-font)
  ("j" space-macs/scale-down-font)
  ("0" space-macs/reset-font-size)
  ("q" nil :exit t))

(space-macs/set-leader-keys "zx" 'space-macs/scale-font-transient-state/body)

;; end of Text Transient State

;; Transparency transient-state

(defun space-macs/toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (dotfile-setting (cons dotspace-macs-active-transparency
                               dotspace-macs-inactive-transparency)))
    (if (equal alpha dotfile-setting)
        (space-macs/disable-transparency frame)
      (space-macs/enable-transparency frame dotfile-setting))))

(defun space-macs/enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values. The
default value for ALPHA is based on
`dotspace-macs-active-transparency' and
`dotspace-macs-inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                           (cons dotspace-macs-active-transparency
                                 dotspace-macs-inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun space-macs/disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun space-macs/increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun space-macs/decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
                           (cons decreased-alpha decreased-alpha)))))

(space-macs|define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :doc "\n[_+_/_=_/_k_] increase transparency [_-_/___/_j_] decrease [_T_] toggle [_q_] quit"
  :bindings
  ("+" space-macs/increase-transparency)
  ("=" space-macs/increase-transparency)
  ("k" space-macs/increase-transparency)
  ("-" space-macs/decrease-transparency)
  ("_" space-macs/decrease-transparency)
  ("j" space-macs/decrease-transparency)
  ("T" space-macs/toggle-transparency)
  ("q" nil :exit t))
(space-macs/set-leader-keys "TT"
  'space-macs/scale-transparency-transient-state/space-macs/toggle-transparency)

;; end of Transparency Transient State


