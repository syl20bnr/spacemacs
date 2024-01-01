;;; keybindings.el --- Spacemacs Defaults Layer key-bindings File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------

;; We define prefix commands only for the sake of which-key
(setq spacemacs/key-binding-prefixes `(,dotspacemacs-emacs-command-key "M-x"
                                       "!"   "shell cmd"
                                       "*"   "search project w/input"
                                       "/"   "search project"
                                       "?"   "show keybindings"
                                       "a"   "applications"
                                       "ac"  "chat"
                                       "ae"  "email"
                                       "af"  "fun"
                                       "ar"  "readers"
                                       "am"  "music"
                                       "at"  "tools"
                                       "ats" "shells"
                                       "atT" "translate"
                                       "aw"  "web-services"
                                       "c"   "compile/comments"
                                       "C"   "capture/colors"
                                       "e"   "errors"
                                       "g"   "git/versions-control"
                                       "h"   "help"
                                       "hd"  "help-describe"
                                       "hP"  "profiler"
                                       "hT"  "tutorials"
                                       "i"   "insertion"
                                       "j"   "jump/join/split"
                                       "jj"  "avy timer"
                                       "jl"  "avy line"
                                       "js"  "split sexp"
                                       "jw"  "avy word"
                                       "k"   "lisp"
                                       "kd"  "delete"
                                       "kD"  "delete-backward"
                                       "k`"  "hybrid"
                                       "m"   "major mode commands"
                                       "n"   "narrow/numbers"
                                       "N"   "navigation"
                                       "o"   "user bindings"
                                       "p"   "projects"
                                       "q"   "quit"
                                       "r"   "registers/rings/resume"
                                       "rd"  "purpose-toggle-window"
                                       "s"   "search/symbol"
                                       "sa"  "ag"
                                       "sg"  "grep"
                                       "sk"  "ack"
                                       "sp"  "search project"
                                       "sP"  "search project w/input"
                                       "sr"  "ripgrep"
                                       "st"  "pt"
                                       "sw"  "web"
                                       "t"   "toggles"
                                       "tC"  "colors"
                                       "tE"  "editing-styles"
                                       "tEe" "emacs (holy-mode)"
                                       "tEh" "hybrid (hybrid-mode)"
                                       "th"  "highlight"
                                       "tm"  "modeline"
                                       "tt"  "timeclock"
                                       "T"   "UI toggles/themes"
                                       "C-t" "other toggles"
                                       "u"   "universal arg"
                                       "v"   "expand region"
                                       "w"   "windows"
                                       "wc"  "centered"
                                       "wp"  "popup"
                                       "x"   "text"
                                       "xa"  "align"
                                       "xd"  "delete"
                                       "xg"  "google-translate"
                                       "xj"  "justification"
                                       "xl"  "lines"
                                       "xR"  "Randomize"
                                       "xt"  "transpose"
                                       "xw"  "words"
                                       "z"   "zoom"))
(apply #'spacemacs/declare-prefix spacemacs/key-binding-prefixes)

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

;; Also bind C-n C-p in minibuffer
(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)

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
;; kmacros --------------------------------------------------------------------
(spacemacs|spacebind
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
(spacemacs|spacebind
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
(spacemacs/set-leader-keys
  "a*"  'calc-dispatch
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; easy pg ----------------------------------------------------------------------
(spacemacs|spacebind
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
(spacemacs|spacebind
 "Compare buffers, files and directories."
 :global
 (("TAB" spacemacs/alternate-buffer "Last buffer")
  ("b" "Buffers"
   ("N"  "New buffer"
    ("C-i" make-indirect-buffer "New indirect buffer...")
    ("f" spacemacs/new-empty-buffer-new-frame "New buffer (new frame)")
    ("h" spacemacs/new-empty-buffer-left "New buffer (left split)")
    ("i" clone-indirect-buffer "Clone buffer")
    ("I" clone-indirect-buffer-other-window-without-purpose "Clone buffer (other window)")
    ("j" spacemacs/new-empty-buffer-below "New buffer (open below)")
    ("k" spacemacs/new-empty-buffer-above "New buffer (open above)")
    ("l" spacemacs/new-empty-buffer-right "New buffer (right split)")
    ("n" spacemacs/new-empty-buffer "New buffer"))
   ("1" buffer-to-window-1 "Move buffer to window 1")
   ("2" buffer-to-window-2 "Move buffer to window 2")
   ("3" buffer-to-window-3 "Move buffer to window 3")
   ("4" buffer-to-window-4 "Move buffer to window 4")
   ("5" buffer-to-window-5 "Move buffer to window 5")
   ("6" buffer-to-window-6 "Move buffer to window 6")
   ("7" buffer-to-window-7 "Move buffer to window 7")
   ("8" buffer-to-window-8 "Move buffer to window 8")
   ("9" buffer-to-window-9 "Move buffer to window 9")
   ("C-d" spacemacs/kill-other-buffers "Kill other buffers...")
   ("C-S-d" spacemacs/kill-matching-buffers-rudely "Kill buffers...")
   ("d" spacemacs/kill-this-buffer "Kill buffer")
   ("e" spacemacs/safe-erase-buffer "Erase...")
   ("h" spacemacs/home "Spacemacs home buffer")
   ("H" spacemacs/switch-to-help-buffer "Help buffer")
   ("n" next-buffer "Next buffer")
   ("m" spacemacs/switch-to-messages-buffer "Messages buffer")
   ("P" spacemacs/copy-clipboard-to-whole-buffer "Paste and replace buffer")
   ("p" previous-buffer "Previous buffer")
   ("R" spacemacs/safe-revert-buffer "Revert buffer...")
   ("s" spacemacs/switch-to-scratch-buffer "Scratch buffer")
   ("u" spacemacs/reopen-killed-buffer "Reopen last killed buffer")
   ("x" kill-buffer-and-window "Kill buffer and close window")
   ("Y" spacemacs/copy-whole-buffer-to-clipboard "Copy buffer")
   ("w" read-only-mode "Toggle read-only"))))
;; Cycling settings -----------------------------------------------------------
(spacemacs|define-transient-state theme
  :title "Themes Transient State"
  :doc "\n[_n_/_<right>_] next  [_N_/_p_/_<left>_] previous  [_t_/_<up>_] list themes"
  :bindings
  ("n" spacemacs/cycle-spacemacs-theme)
  ("N" spacemacs/cycle-spacemacs-theme-backward)
  ("p" spacemacs/cycle-spacemacs-theme-backward)
  ("t" spacemacs/theme-loader)
  ("<up>" spacemacs/theme-loader)
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
  :title "Error Transient State"
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
;; ediff ----------------------------------------------------------------------
(spacemacs|spacebind
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
    ("." spacemacs/ediff-dotfile-and-template "With Spacemacs dotfile")
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
(spacemacs|spacebind
 "Files manipulation."
 :global
 (("f" "Files"
   ("A" spacemacs/find-file-and-replace-buffer "Set another file for buffer...")
   ("c" spacemacs/save-as "Save file or active region as a new file...")
   ("D" spacemacs/delete-current-buffer-file "Delete...")
   ("i" spacemacs/insert-file "Insert file content...")
   ("l" find-file-literally "Open file literally...")
   ("E" spacemacs/sudo-edit "Open using sudo...")
   ("o" spacemacs/open-file-or-directory-in-external-app "Open with external app")
   ("R" spacemacs/rename-current-buffer-file "Rename...")
   ("S" evil-write-all "Save all")
   ("s" save-buffer "Save")
   ("C"  "Convert"
    ("d" spacemacs/unix2dos "Convert to DOS")
    ("u" spacemacs/dos2unix "Convert to UNIX"))
   ("e" "Emacs/Spacemacs"
    ("C-e" spacemacs/force-init-spacemacs-env "Recreate env. variables file")
    ("c" spacemacs/recompile-elpa "Recompile packages")
    ("i" spacemacs/find-user-init-file "Open Emacs \"init.el\"")
    ("I" spacemacs/find-user-early-init-file "Open Emacs \"early-init.el\"")
    ("d" spacemacs/find-dotfile "Open Spacemacs dotfile")
    ("D" spacemacs/ediff-dotfile-and-template "Diff. with dotfile template")
    ("e" spacemacs/edit-env "Open \".spacemacs.env\"")
    ("E" dotspacemacs/call-user-env "Refresh env. variables")
    ("R" dotspacemacs/sync-configuration-layers "Reload configuration")
    ("v" spacemacs/display-and-copy-version "Copy Spacemacs version")
    ("U" configuration-layer/update-packages "Update packages..."))
   ("v" "Variables"
    ("d" add-dir-local-variable "Add directory-local variable...")
    ("f" add-file-local-variable "Add bottom file variable...")
    ("p" add-file-local-variable-prop-line "Add top file property..."))

   ("y" "Yank/Copy"
    ("c" spacemacs/copy-file-path-with-line-column "File path with line and column")
    ("d" spacemacs/copy-directory-path "Directory path")
    ("l" spacemacs/copy-file-path-with-line "File path with line number")
    ("n" spacemacs/copy-file-name "File name")
    ("N" spacemacs/copy-file-name-base "File name without extension")
    ("y" spacemacs/copy-file-path "File path")
    ("b" spacemacs/copy-buffer-name "Buffer name")))))
;; frame ----------------------------------------------------------------------
(spacemacs|spacebind
 "Frames"
 :global
 (("F" "Frames"
   ("f" spacemacs/find-file-other-frame "Find file other frame...")
   ("d" delete-frame "Delete frame")
   ("D" delete-other-frames "Delete other frames")
   ("b" spacemacs/switch-to-buffer-other-frame "Switch to buffer other frame...")
   ("B" spacemacs/display-buffer-other-frame "Display buffer other frame...")
   ("o" other-frame "Switch to other frame")
   ("O" spacemacs/dired-other-frame "Dired other frame...")
   ("n" make-frame "Make frame"))))
;; help -----------------------------------------------------------------------
(defalias 'emacs-tutorial 'help-with-tutorial)
(spacemacs/set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdK" 'describe-keymap
  "hdl" 'spacemacs/describe-last-keys
  "hdp" 'describe-package
  "hdP" 'configuration-layer/describe-package
  "hds" 'spacemacs/describe-system-info
  "hdt" 'describe-text-properties
  "hdT" 'describe-theme
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
  "ij" 'spacemacs/evil-insert-line-below
  "ib" 'insert-buffer)
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
  "cn" 'next-error
  "cN" 'previous-error
  "cd" 'spacemacs/show-hide-compilation-window
  "cb" 'spacemacs/switch-to-compilation-buffer)
(with-eval-after-load 'compile
  (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))
;; narrow & widen -------------------------------------------------------------
(spacemacs/set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nR" 'spacemacs/narrow-to-region-indirect-buffer
  "nP" 'spacemacs/narrow-to-page-indirect-buffer
  "nF" 'spacemacs/narrow-to-defun-indirect-buffer
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
(spacemacs|add-toggle gui-elements
  :if window-system
  :status (or menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode)
  :on  (spacemacs//toggle-gui-elements 1)
  :off (spacemacs//toggle-gui-elements 0)
  :documentation "Toggle menubar, toolbar, scrollbar, and tooltip modes."
  :evil-leader "Tg")
;; quit -----------------------------------------------------------------------
(spacemacs/set-leader-keys
  "qs" 'spacemacs/save-buffers-kill-emacs
  "qq" 'spacemacs/prompt-kill-emacs
  "qQ" 'spacemacs/kill-emacs
  "qf" 'spacemacs/frame-killer)
;; timeclock ------------------------------------------------------------------
(spacemacs/set-leader-keys
  "ttc" 'timeclock-change
  "tte" 'timeclock-workday-elapsed-string
  "ttg" 'timeclock-workday-remaining-string
  "tti" 'timeclock-in
  "ttl" 'timeclock-when-to-leave-string
  "ttm" 'timeclock-mode-line-display
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
  ;; "wcc"  'spacemacs/toggle-centered-buffer
  ;; "wcC"  'spacemacs/toggle-distraction-free
  ;; "wc."  'spacemacs/centered-buffer-transient-state
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
  "xdl" 'delete-blank-lines
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xlc" 'spacemacs/sort-lines-by-column
  "xlC" 'spacemacs/sort-lines-by-column-reverse
  "xld" 'spacemacs/duplicate-line-or-region
  "xlk" 'spacemacs/kill-back-to-indentation
  "xlr" 'spacemacs/randomize-lines
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
  "xwr" 'spacemacs/randomize-words
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
  ("C-1" spacemacs/winum-select-window-1)
  ("C-2" spacemacs/winum-select-window-2)
  ("C-3" spacemacs/winum-select-window-3)
  ("C-4" spacemacs/winum-select-window-4)
  ("C-5" spacemacs/winum-select-window-5)
  ("C-6" spacemacs/winum-select-window-6)
  ("C-7" spacemacs/winum-select-window-7)
  ("C-8" spacemacs/winum-select-window-8)
  ("C-9" spacemacs/winum-select-window-9))
(spacemacs/set-leader-keys "b." 'spacemacs/buffer-transient-state/body)

;; end of Buffer Transient State

;; Window Transient State

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

(defvar spacemacs--window-ts-full-hint-toggle nil
  "Display window transient state documentation.")

(defun spacemacs//window-ts-toggle-hint ()
  "Toggle the full hint docstring for the window transient state."
  (interactive)
  (setq spacemacs--window-ts-full-hint-toggle
        (not spacemacs--window-ts-full-hint-toggle)))

(defun spacemacs//window-ts-hint ()
  "Return a condensed/full hint for the window transient state"
  (concat
   " "
   (if spacemacs--window-ts-full-hint-toggle
       spacemacs--window-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
             spacemacs--window-ts-minified-hint))))

(spacemacs|transient-state-format-hint window
  spacemacs--window-ts-minified-hint "\n
Select: _a_ _h_ _j_ _k_ _l_ _w_ _0_.._9_ Move: _H_ _J_ _K_ _L_ _r_ _R_ Split: _s_ _v_ Resize: _[_ _]_ _{_ _}_ _m_ _|_ ___")

(spacemacs|transient-state-format-hint window
  spacemacs--window-ts-full-hint
  (format "\n[_?_] toggle help
 Select^^^^               Move^^^^              Split^^^^^^               Resize^^             Other^^
 ──────^^^^─────────────  ────^^^^────────────  ─────^^^^^^─────────────  ──────^^───────────  ─────^^──────────────────
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

(spacemacs|define-transient-state window
  :title "Window Transient State"
  :hint-is-doc t
  :dynamic-hint (spacemacs//window-ts-hint)
  :bindings
  ("?" spacemacs//window-ts-toggle-hint)
  ;; Select
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("0" spacemacs/winum-select-window-0)
  ("1" spacemacs/winum-select-window-1)
  ("2" spacemacs/winum-select-window-2)
  ("3" spacemacs/winum-select-window-3)
  ("4" spacemacs/winum-select-window-4)
  ("5" spacemacs/winum-select-window-5)
  ("6" spacemacs/winum-select-window-6)
  ("7" spacemacs/winum-select-window-7)
  ("8" spacemacs/winum-select-window-8)
  ("9" spacemacs/winum-select-window-9)
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
  ("r" spacemacs/rotate-windows-forward)
  ("R" spacemacs/rotate-windows-backward)
  ;; Split
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("-" split-window-below-and-focus)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("/" split-window-right-and-focus)
  ("m" spacemacs/toggle-maximize-buffer)
  ("|" spacemacs/maximize-vertically)
  ("_" spacemacs/maximize-horizontally)
  ;; Resize
  ("[" spacemacs/shrink-window-horizontally)
  ("]" spacemacs/enlarge-window-horizontally)
  ("{" spacemacs/shrink-window)
  ("}" spacemacs/enlarge-window)
  ;; Other
  ("d" delete-window)
  ("D" delete-other-windows)
  ("u" winner-undo)
  ("U" winner-redo)
  ("q" nil :exit t))
(spacemacs/set-leader-keys
  "w." 'spacemacs/window-transient-state/body
  "w[" 'spacemacs/window-transient-state/spacemacs/shrink-window-horizontally
  "w]" 'spacemacs/window-transient-state/spacemacs/enlarge-window-horizontally
  "w{" 'spacemacs/window-transient-state/spacemacs/shrink-window
  "w}" 'spacemacs/window-transient-state/spacemacs/enlarge-window)

;; end of Window Transient State

;; Text Transient State

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

;; end of Text Transient State

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

;; Background Transparency transient-state

(defun spacemacs/enable-background-transparency (&optional frame alpha-background)
  "Enable background transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values. The
default value for ALPHA is based on `dotspacemacs-background-transparency'"
  (interactive)
  (message (number-to-string dotspacemacs-background-transparency))
  (let ((alpha-setting (or alpha-background dotspacemacs-background-transparency)))
    (message (number-to-string alpha-setting))
    (set-frame-parameter frame 'alpha-background alpha-setting)))

(defun spacemacs/disable-background-transparency (&optional frame)
  "Disable background transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha-background 100))


(defun spacemacs/toggle-background-transparency (&optional frame)
  "Toggle between transparent and opaque background state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha-background (frame-parameter frame 'alpha-background))
        (dotfile-setting dotspacemacs-background-transparency))
    (if (equal alpha-background dotfile-setting)
        (spacemacs/disable-background-transparency frame)
      (spacemacs/enable-background-transparency frame dotfile-setting))))

(defun spacemacs/increase-background-transparency (&optional frame)
  "Increase background transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (frame-parameter frame 'alpha-background) 100))
         (message current-alpha)
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha-background increased-alpha))))

(defun spacemacs/decrease-background-transparency (&optional frame)
  "Decrease backrgound transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (frame-parameter frame 'alpha-background) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha-background decreased-alpha))))

(spacemacs|define-transient-state scale-background-transparency
  :title "Frame Background Transparency Transient State"
  :doc "\n[_+_/_=_/_k_] increase transparency [_-_/___/_j_] decrease [_T_] toggle [_q_] quit"
  :bindings
  ("+" spacemacs/increase-background-transparency)
  ("=" spacemacs/increase-background-transparency)
  ("k" spacemacs/increase-background-transparency)
  ("-" spacemacs/decrease-background-transparency)
  ("_" spacemacs/decrease-background-transparency)
  ("j" spacemacs/decrease-background-transparency)
  ("T" spacemacs/toggle-background-transparency)
  ("q" nil :exit t))

(spacemacs/set-leader-keys "TB"
  'spacemacs/scale-background-transparency-transient-state/spacemacs/toggle-background-transparency)
