;;; dos.el --- major mode for editing Dos scripts (batch files)

;; Copyright (C) 2003, 2008, 2009, 2010, 2011, 2012, 2013 Arni Magnusson

;; Author:   Arni Magnusson
;; Version:  2.18
;; Keywords: languages
;; URL:      http://emacswiki.org/emacs/dos.el

(defconst dos-mode-version "2.18" "Dos Mode version number.")

;;; Commentary:
;;
;; Major mode for editing Dos scripts. Provides syntax highlighting, templates, and smaller tools. The syntax groups for
;; highlighting are:
;;
;; Face                          Example
;; dos-label-face                :LABEL
;; font-lock-comment-face        rem
;; font-lock-builtin-face        copy
;; font-lock-keyword-face        goto
;; font-lock-warning-face        cp
;; font-lock-constant-face       [call] prog
;; font-lock-variable-name-face  %var%
;; font-lock-type-face           -option
;;
;; Installation:
;;
;; 1. Copy this file (dos.el) to a directory in `load-path', or edit .emacs to add the directory to `load-path':
;;      (add-to-list 'load-path "mypath/dos")
;; 2. Byte-compile this file to dos.elc for faster startup:
;;      M-x byte-compile-file
;; 3. Edit .emacs so that `dos-mode' is autoloaded and assigned to *.bat files:
;;      (autoload 'dos-mode "dos" "Edit Dos scripts." t)
;;      (add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
;;
;; Customization:
;;
;; See documentation of variable `dos-mode-hook' and function `dos-outline' for ideas.
;;
;; Usage:
;;
;; See documentation of function `dos-mode'.
;;
;; Acknowledgements:
;;
;; Inspired by `batch-mode' (Agnar Renolen) and `cmd-mode' (Tadamegu Furukawa).

;;; History:
;;
;; 22 Jan 2013  2.18 Moved keywords "mkdir" and "rmdir" from `font-lock-warning-face' to `font-lock-builtin-face'.
;; 30 Mar 2012  2.17 Improved documentation.
;; 14 Feb 2011  2.16 Improved highlighting of variable names.
;; 20 Sep 2010  2.15 Changed :LABEL highlighting to new `dos-label-face'. Improved highlighting of variable names.
;;  8 Jul 2010  2.14 Added user function `dos-mode-version'.
;; 29 Jun 2010  2.13 Added keyword "erase".
;; 16 Apr 2010  2.12 Added ;;;###autoload cookie.
;; 29 Sep 2009  2.11 Improved highlighting of strings.
;; 18 Sep 2009  2.10 Improved highlighting of comments.
;; 27 May 2009  2.9  Improved documentation.
;; 26 May 2009  2.8  Added user function `dos-help-mode'. Renamed user function `dos-help' to `dos-help-cmd'. Added
;;                   internal variable `dos-menu', providing GUI menu.
;; 18 May 2009  2.7  Improved highlighting of scripts following call and goto.
;; 23 Apr 2009  2.6  Improved highlighting of --options.
;; 21 Apr 2009  2.5  Improved highlighting of variable names.
;; 24 Mar 2009  2.4  Improved highlighting of scripts/labels following call and goto.
;; 18 Mar 2009  2.3  Added support for @rem highlighting. Improved highlighting of -options and scripts/labels following
;;                   call and goto.
;; 11 Mar 2009  2.2  Added user functions `dos-help', `dos-run', and `dos-run-args'. Improved highlighting of scripts
;;                   and labels following call and goto.
;; 10 Mar 2009  2.1  Added keywords "at", "attrib", "cd", "cmd", "color", "doskey", "path", "popd", "prompt", "pushd",
;;                   "sort", and "start". Added support for highlighting /options.
;;  9 Mar 2009  2.0  Complete rewrite. Added user functions `dos-outline' and `dos-sep'. Added internal variables
;;                   `dos-font-lock-keywords', `dos-mode-abbrev-table', `dos-mode-map', and `dos-mode-syntax-table'.
;;                   Added local variables `comment-start', `imenu-generic-expression', and `outline-regexp'. Added
;;                   keyword "copy". Added support for ::comment highlighting. Added commentary on installation,
;;                   customization, and usage.
;; 18 Feb 2009  1.1  Changed face names, added keyword "cls", and removed `dos-template' line containing "[-help]".
;;  5 Dec 2008  1.0  Added support for underscored_variable highlighting.
;; 22 Aug 2003  0.9  Created main function `dos-mode', user variable `dos-mode-hook', user functions `dos-template' and
;;                   `dos-template-mini', and local variable `font-lock-defaults'.

;;; Code:

;; 1  Preamble

(require 'outline)
(defgroup dos nil "Major mode for editing Dos scripts." :tag "Dos" :group 'languages)

;; 2  User variables

(defcustom dos-mode-hook nil
  "Hook for `dos-mode'.\n
If you want to set syntax colors or keybindings, here is an example that does
that:\n
\(defun my-dos-hook ()
  (set-face-attribute 'font-lock-doc-face nil
                      :foreground \"black\" :weight 'bold)
  (local-set-key [down-mouse-3] 'imenu))
\(add-hook 'dos-mode-hook 'my-dos-hook)"
  :tag   "Hook"
  :type  'hook
  :group 'dos)
(defface dos-label-face '((t :weight bold)) "Font Lock mode face used to highlight Dos labels." :group 'dos)

;; 3  Internal variables

(defvar dos-font-lock-keywords
  (eval-when-compile
    (let ((COMMANDS
           '("at"       "attrib"   "cd"       "cls"      "color"    "copy"     "date"     "defined"  "del"      "dir"
             "doskey"   "echo"     "endlocal" "erase"    "exist"    "fc"       "find"     "md"       "mkdir"    "more"
             "move"     "path"     "pause"    "popd"     "prompt"   "pushd"    "ren"      "rd"       "rmdir"    "set"
             "setlocal" "shift"    "sort"     "time"     "title"    "type"     "xcopy"))
          (CONTROLFLOW
           '("call"     "cmd"      "do"       "else"     "equ"      "exit"     "for"      "geq"      "goto"     "gtr"
             "if"       "in"       "leq"      "lss"      "neq"      "not"      "start"))
          (LINUX
           '("cat"      "cp"       "ls"       "mv"       "rm")))
      (list
       '("\\<\\(call\\|goto\\)\\>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?" (2 font-lock-constant-face t))
       '("^[ \t]*\\(@?rem\\>\\|::\\).*"              (0 font-lock-comment-face t))
       '("^:[^:].*" .                                   'dos-label-face)
       '("\\<\\(defined\\|set\\)\\>[ \t]*\\(\\w+\\)" (2 font-lock-variable-name-face))
       '("%\\(\\w+\\)%?"                             (1 font-lock-variable-name-face))
       '("!\\(\\w+\\)!?"                             (1 font-lock-variable-name-face)) ; delayed-expansion !variable!
       '("[ =][-/]+\\(\\w+\\)"                       (1 font-lock-type-face append))
       (cons (regexp-opt COMMANDS    'words) font-lock-builtin-face)
       (cons (regexp-opt CONTROLFLOW 'words) font-lock-keyword-face)
       (cons (regexp-opt LINUX       'words) font-lock-warning-face)))))
(defvar dos-menu
  '("Dos"
    ["Run"           dos-run          ] ; :help "Run script"
    ["Run with Args" dos-run-args     ] ; :help "Run script with args"
    "--"
    ["Imenu"         imenu            ] ; :help "Navigate with imenu"
    "--"
    ["Template"      dos-template     ] ; :help "Insert template"
    ["Mini Template" dos-template-mini] ; :help "Insert minimal template"
    "--"
    ["Help (cmd)"    dos-help-cmd     ]   ; :help "Show help page for Dos command"
    ["Help (mode)"   dos-help-mode    ]   ; :help "Show help page for Emacs dos-mode"
    ["Version"       dos-mode-version ])) ; :help "Show Dos Mode version"
(defvar dos-mode-abbrev-table nil)(define-abbrev-table 'dos-mode-abbrev-table ())
(defvar dos-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil dos-menu)
    (define-key map [f11]         'dos-outline      )
    (define-key map [S-f12]       'dos-template-mini)
    (define-key map [f12]         'dos-template     )
    (define-key map [?\C-c ?\C-.] 'dos-mode-version )
    (define-key map [?\C-c ?\C-/] 'dos-help-cmd     )
    (define-key map [?\C-c ?\C- ] 'dos-sep          )
    (define-key map [?\C-c ?\C-a] 'dos-run-args     )
    (define-key map [?\C-c ?\C-c] 'dos-run          )
    (define-key map [?\C-c ?\C-m] 'dos-help-mode    )
    (define-key map [?\C-c ?\C-v] 'dos-run          )
    map))
(defvar dos-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?~  "w" table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?-  "w" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?{  "w" table)
    (modify-syntax-entry ?}  "w" table)
    (modify-syntax-entry ?\\ "." table)
    table))

;; 4  User functions

(defun dos-help-cmd (cmd) "Show help for Dos command." (interactive "sHelp: ")(shell-command (concat "help " cmd)))
(defun dos-help-mode () "Show help page for `dos-mode'." (interactive)
  (describe-function 'dos-mode)(switch-to-buffer "*Help*")(delete-other-windows)(message nil))
(defun dos-mode-version () "Show Dos Mode version number." (interactive)
  (message (concat "Dos Mode version " dos-mode-version)))
(defun dos-outline () "Navigate within Dos script using outline-mode.

If you haven't already configured an `outline-mode-hook', here is an example
that makes it easy to return to `dos-mode':

\(defun my-outline-hook ()
  (local-set-key [mouse-1] 'outline-mouse-select)
  (local-set-key [return]  'dos-mode            )
  (defun outline-mouse-select () \"Select position and return to `dos-mode'.\"
    (interactive)(dos-mode)(beginning-of-line)))
\(add-hook 'outline-mode-hook 'my-outline-hook)
"
  (interactive)(let ((outreg outline-regexp))(outline-mode)(setq outline-regexp "^:[^:]"))(outline-mode)(hide-body))
(defun dos-run () "Run Dos script." (interactive)(save-buffer)(shell-command buffer-file-name))
(defun dos-run-args (args) "Run Dos script with ARGS." (interactive "sArgs: ")
  (shell-command (concat buffer-file-name " " args)))
(defun dos-sep () "Insert & separator." (interactive)(insert " & "))
(defun dos-template () "Insert Dos template." (interactive)
  (goto-char (point-min))(insert "
@echo off
setlocal
if [%1]==[] goto HELP
if [%1]==[--help] goto HELP
REM ####################################################################################################################
REM                                                                                                                    #
REM Script:                                                                                                            #
REM                                                                                                                    #
REM Purpose:                                                                                                           #
REM                                                                                                                    #
REM Args:                                                                                                              #
REM                                                                                                                    #
REM Notes:                                                                                                             #
REM                                                                                                                    #
REM Warning:                                                                                                           #
REM                                                                                                                    #
REM Requires:                                                                                                          #
REM                                                                                                                    #
REM Returns:                                                                                                           #
REM                                                                                                                    #
REM ####################################################################################################################
\nrem Pop args until file=%1
set par=default
:STARTLOOP
if [%2]==[] goto ENDLOOP
if %1==-flag set par=%2 & shift & shift
goto STARTLOOP
:ENDLOOP\n\n\n
:HELP
echo Usage:
echo.\n
:EOF
")(goto-char (point-min))(delete-char 1)(search-forward ":   ")(overwrite-mode t))
(defun dos-template-mini () "Insert minimal Dos template." (interactive)
  (goto-char (point-min))(insert "@echo off\nsetlocal\n\n"))

;; 5  Main function

;;;###autoload
(defun dos-mode () "Major mode for editing Dos scripts.\n
The `dos-help-mode' command shows this page.\n
Start a new script from `dos-template' or `dos-template-mini'. Navigate between
sections using `dos-outline', `imenu', or `outline-minor-mode'. Use `dos-sep' to
save keystrokes. Read help for Dos command with `dos-help-cmd'. Run script using
`dos-run' and `dos-run-args'.

\\{dos-mode-map}"
  (interactive)(kill-all-local-variables)(setq major-mode 'dos-mode)(setq mode-name "Dos")
  (set (make-local-variable 'comment-start) "rem")
  (set (make-local-variable 'imenu-generic-expression) '((nil "^:[^:].*" 0)))
  (set (make-local-variable 'font-lock-defaults) '(dos-font-lock-keywords nil t)) ; case-insensitive keywords
  (set (make-local-variable 'outline-regexp) ":[^:]")
  (set-syntax-table dos-mode-syntax-table)
  (setq local-abbrev-table dos-mode-abbrev-table)
  (use-local-map dos-mode-map)
  (run-mode-hooks 'dos-mode-hook))

(provide 'dos)

;;; dos.el ends here
