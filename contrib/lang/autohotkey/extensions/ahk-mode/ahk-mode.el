;;; ahk-mode.el --- Major mode for editing AHK (AutoHotkey) scripts. -*- coding: utf-8 -*-

;; Copyright © 2008, 2009, 2010, 2011, 2012 by Xah Lee

;; Author:   Xah Lee ( http://xahlee.org/ )
;; Author:   Robert Widhopf-Fenk
;; Author:   Rich Alesi
;; Keywords: ahk, AutoHotkey, hotkey, keyboard shortcut, automation

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software Foundation; either
;; GPL version 2 or 3.

;;; Commentary:

;; A major mode for editing AutoHotkey (AHK) script. Taken from snippets
;; supplied by the above authors in addition to the latest syntax files from the
;; latest build of SCITE for download location and documentation, see:

;;; INSTALL

;; Open the file, then type “Alt+x eval-buffer”. You are done. Open
;; any ahk script, then type “Alt+x ahk-mode”, you'll see the
;; source code syntax colored.

;; To have emacs automatically load the file when it restarts, and
;; automatically use the mode when opening files ending in “.ahk”, do this:

;; ① Put the file 〔ahk-mode.el〕 in the dir 〔~/.emacs.d/〕
;; ② Put the following lines in your emacs init file (usually at 〔~/.emacs〕).
;; (autoload 'ahk-mode "ahk-mode" "Load ahk-mode for editing AutoHotkey scripts." t)

;;; FEATURES

;; When opening a script file you will get:
;; - syntax highlighting
;; - indention, completion and command help (bound to "TAB")
;; - insertion of command templates (bound to "C-c C-i")

;;; HISTORY

;; version 1.4, 2014_07_18 latest merge with syntax tables
;; version 1.2.2, 2012-05-21 modified syntax table so “_” is part of word.
;; version 1.2.1, 2011-10-15 Minor changes. No visible behavior change.
;; version 1.2, 2010-02-17 fixed a defect where if source contains “"C:\"”, everything after is badly syntax colored. Thanks to “xinlu.h” and “iain.tuddenham”. Detail at http://code.google.com/p/ergoemacs/issues/detail?id=66
;; version 1.1, 2010-01-14 Added indentation feature. (press Tab to indent.)
;; version 1.0, 2010-01-09 First version.

(eval-when-compile
  (require 'font-lock)
  (require 'cl))

;;; Code:

(defconst ahk-mode-version "")
(setq ahk-mode-version "1.4")

(defgroup ahk-mode nil
  "Major mode for editing AutoHotkey script."
  :group 'languages
  :prefix "ahk-")

(defvar ahk-mode-command-name-face 'ahk-mode-command-name-face "Face name to use for AHK command names.")

(defcustom ahk-mode-hook '(ahk-mode-hook-activate-filling)
  "Hook functions run by `ahk-mode'."
  :type 'hook
  :group 'ahk-mode)

(defcustom ahk-indentation 2
  "The indentation level."
  :type 'integer
  :group 'ahk-mode)

(defface ahk-mode-command-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight AHK command names."
  :group 'languages)

(defvar ahk-path-exe-optional nil)

(defvar ahk-registry "HKEY_CLASSES_ROOT\\AutoHotkeyScript\\Shell\\Open\\Command")

(defvar ahk-path-exe-installed
  (let ((reg-data (shell-command-to-string (format "reg query \"%s\"" ahk-registry))))
    ;; from
    ;; "C:\\Program Files (x86)\\AutoHotkey\\AutoHotkey.exe"
    ;; to
    ;; "C:/Program Files (x86)/AutoHotkey/AutoHotkey.exe"
    (replace-regexp-in-string "\\\\" "/" (cadr (split-string reg-data "\\\"")))))

(defvar ahk-path-exe-installed-p
  (file-exists-p ahk-path-exe-installed))

;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.ahk$"  . ahk-mode))

(defvar ahk-mode-map nil "Keymap for ahk-mode")
(progn
  (setq ahk-mode-map (make-sparse-keymap))
  (define-key ahk-mode-map (kbd "C-c C-r") 'ahk-lookup-ahk-ref)
  ;; (define-key ahk-mode-map (kbd "M-TAB") 'ahk-complete-symbol)
  ;; (define-key ahk-mode-map [remap comment-dwim] 'ahk-comment-dwim)
  ;; (define-key ahk-mode-map (kbd "C-c C-h") 'ahk-www-help-at-point)
  ;; (define-key ahk-mode-map (kbd "C-c C-c") 'ahk-comment-region)
  ;; (define-key ahk-mode-map (kbd "C-c C-i") 'ahk-insert-command-template)
  ;; (define-key ahk-mode-map "\t" 'ahk-indent-line-and-complete)
  ;; (define-key ahk-mode-map "{" 'ahk-electric-brace)
  ;; (define-key ahk-mode-map "}" 'ahk-electric-brace)
  ;; (define-key ahk-mode-map "\r" 'ahk-electric-return)
  )


(easy-menu-define ahk-menu ahk-mode-map "AHK Mode Commands"
  '("AHK"
    ["Insert Command Template" ahk-insert-command-template]
    ["Lookup webdocs on command" ahk-www-help-at-point]
    ["Keyword Completion" ahk-complete-symbol]))

;;; syntax table
(defvar ahk-mode-syntax-table nil "Syntax table for `ahk-mode'.")
(setq ahk-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; these are also allowed in variable names
        (modify-syntax-entry ?#  "w" synTable)
        (modify-syntax-entry ?_  "w" synTable)
        (modify-syntax-entry ?@  "w" synTable)
        (modify-syntax-entry ?[  "w" synTable)
        (modify-syntax-entry ?]  "w" synTable)
        ;; some additional characters used in paths and switches
        (modify-syntax-entry ?\\  "w" synTable)
        ;; (modify-syntax-entry ?/  "w" synTable)
        (modify-syntax-entry ?. "." synTable)
        (modify-syntax-entry ?: "." synTable)
        (modify-syntax-entry ?- "." synTable)
        ;; for multiline comments (taken from cc-mode)
        (modify-syntax-entry ?/  ". 14" synTable)
        (modify-syntax-entry ?*  ". 23"   synTable)
        ;; Give CR the same syntax as newline, for selective-display
        (modify-syntax-entry ?\^m "> b" synTable)
        (modify-syntax-entry ?\n "> b"  synTable)
        (modify-syntax-entry ?` "\\" synTable) ; ` is escape
        (modify-syntax-entry ?\; "< b" synTable)
        (modify-syntax-entry ?! "." synTable)
        (modify-syntax-entry ?$ "." synTable)
        (modify-syntax-entry ?% "." synTable)
        (modify-syntax-entry ?^ "." synTable)
        (modify-syntax-entry ?& "." synTable)
        (modify-syntax-entry ?~ "." synTable)
        (modify-syntax-entry ?' "." synTable)
        (modify-syntax-entry ?| "." synTable)
        (modify-syntax-entry ?? "." synTable)
        (modify-syntax-entry ?< "." synTable)
        (modify-syntax-entry ?> "." synTable)
        (modify-syntax-entry ?, "." synTable)
        synTable)
)

;;; functions

(defun run-this-ahk-script ()
  (interactive)
  (lexical-let*
      ((file (shell-quote-argument (buffer-file-name)))
       (optional-ahk-exe (and (stringp ahk-path-exe-optional)
                              (file-exists-p ahk-path-exe-optional)))
       (ahk-exe-path (shell-quote-argument (if optional-ahk-exe
                                               ahk-path-exe-optional
                                             ahk-path-exe-installed))))
    (if (and (stringp ahk-path-exe-optional)
             (not optional-ahk-exe))
        (error "Error: optional-ahk-exe is not found.")
      (save-window-excursion
        (async-shell-command (format "%s %s" ahk-exe-path file))))))

(defun ahk-lookup-ahk-ref ()
  "Look up current word in AutoHotkey's reference doc.
If a there is a text selection (a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
 (interactive)
 (let (myword myurl)
   (setq myword
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myword (replace-regexp-in-string " " "%20" myword))
  (setq myurl (concat "http://www.autohotkey.com/docs/commands/" myword ".htm" ))
  (browse-url myurl)
   ))

(defun ahk-mode-hook-activate-filling ()
  "Activates `auto-fill-mode' and `filladapt-mode'."
  (auto-fill-mode 1)
  (if (locate-library "filladapt")
      (filladapt-mode 1)))

;;;; indentation
(defun ahk-calc-indention (str &optional offset)
  (let ((i (* (or offset 0) ahk-indentation)))
    (while (string-match "\t" str)
      (setq i (+ i tab-width)
            str (replace-match "" nil t str)))
    (setq i (+ i (length str)))
    i))

; the follwing regexp is used to detect if a condition is a one line statement or not,
; i.e. it matches one line statements but should not match those where the THEN resp.
; ELSE body is on its own line ...
(defvar ahk-one-line-if-regexp
  (concat "^\\([ \t]*\\)" ;; this is used for indention
          "\\("
          "If\\(Not\\)?\\("
            (regexp-opt '("InString" "InStr"
                          "Less" "Greater" "Equal"
                          "LessOrEqual" "GreaterOrEqual"
                          ))
            "\\)[^,\n]*,[^,\n]*,[^,\n]*,"
          "\\|"
          "If\\(Not\\)?Exist[^,\n]*,[^,\n]*,"
          "\\|"
          "Else[ \t]+\\([^I\n][^f\n][^ \n]\\)"
          "\\)"))

;; TODO write a unit test for indentation
(defun ahk-indent-line ()
  "Indent the current line."
  (interactive)

  (let ((indent 0)
        (opening-brace nil) (else nil) (closing-brace) (block-skip nil)
        (case-fold-search t))
    ;; do a backward search to determine the indention level
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^;")
          (setq indent 0)
        ;; save type of current line
        (setq opening-brace (looking-at "^\\([ \t]*\\)[{(]"))
        (setq else          (looking-at "^\\([ \t]*\\)Else[ \r\n]"))
        (setq closing-brace (looking-at "^\\([ \t]*\\)[)}]"))
        ;; check previous non-empty line
        (skip-chars-backward " \r\t\n")
        (beginning-of-line)
        (when (looking-at "^\\([ \t]*\\)[)}]")
          (goto-char (match-end 0))
          (backward-list)
          (skip-chars-backward " \r\t\n")
          (beginning-of-line)
          (setq block-skip t))
        ;; skip commented lines backward
        (while (and (looking-at "^;") (not (bobp)))
          (forward-line -1))
        ;; is it a label
        (if (looking-at "^[^: \n]+:")
            (if (and (not opening-brace)
                     (not block-skip)
                     (looking-at "^[^: ]+:\\([^:\n]*:\\)?[ \t]*$"))
                (setq indent ahk-indentation)
              (setq indent 0))
          ;; is it an opening { or (
          (if (looking-at "^\\([ \t]*\\)[{(]")
              (setq indent (ahk-calc-indention (match-string 1) 1))
            ;; is it a Return at the first level?
            (if (and (looking-at "^\\([ \t]*\\)[rR]eturn")
                     (= (ahk-calc-indention (match-string 1)) ahk-indentation))
                (setq indent (ahk-calc-indention (match-string 1) -1))
              ;; If/Else with body on next line, but not opening { or (
              (if (and (not opening-brace)
                       (not block-skip)
                       (looking-at "^\\([ \t]*\\)\\(If\\|Else\\)")
                       (not (looking-at ahk-one-line-if-regexp)))
                  (setq indent (ahk-calc-indention (match-string 1) 1))
                ;; two lines back was a If/Else thus indent like it
                (if (and (not opening-brace)
;                         (not else)
                         (save-excursion
                           (beginning-of-line)
                           (skip-chars-backward " \r\t\n")
                           (beginning-of-line)
                           (setq indent nil)
                           ;; backtrace nested Ifs
                           (while (and (looking-at "^\\([ \t]*\\)\\(If\\|Else\\)")
                                       (not (looking-at ahk-one-line-if-regexp)))
                             (setq indent (ahk-calc-indention (match-string 1)))
                             (beginning-of-line)
                             (skip-chars-backward " \r\t\n")
                             (beginning-of-line))
                           indent))
                    (setq indent indent)
                  ;; the last resort, indent as the last line
                  (if (looking-at "^\\([ \t]*\\)")
                      (setq indent (ahk-calc-indention (match-string 1)))))))))))
    ;; check for special tokens
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\([ \t]*\\)[})]")
          (setq indent (- indent ahk-indentation))
        (if (or (looking-at "^[ \t]*[^,: \t\n]*:")
                (looking-at "^;;;"))
            (setq indent 0))))

    ;; set negative indention to 0
    (if (< indent 0)
        (setq indent 0))

    (let ((point (point-marker)))
      (beginning-of-line)
      (if (looking-at "^[ \t]+")
          (replace-match ""))
      (indent-to indent)
      (if  (not (marker-position point))
          (if (re-search-forward "[^ \t]" (point-max) t)
              (goto-char (1- (point))))
        (goto-char point)
        (set-marker point nil)))
    (if (bolp)
        (goto-char (+ (point) indent)))))

(defun ahk-indent-region (start end)
  "Indent lines in region START to END."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (ahk-indent-line)
      (forward-line 1))
    (ahk-indent-line)
    (set-marker end nil)))

;; (defun ahk-newline-and-indent-dwim ()
;;   "newline-and-indent-dwim for ahk"
;;   (interactive)
;;   (let ((this-line (get-string-of-current-line))
;;         (indent-line-function (lambda () (insert "    "))))
;;     (cond
;;      ((or (equal "{" (format "%c" (char-before)))
;;           (ahk-electric-brace-and-comment-p this-line))
;;       (ahk-newline-and-indent-and-electric-brace))
;;      ((or (equal ":" (format "%c" (char-before)))
;;           (ahk-colon-and-comment-p this-line))
;;       (ahk-newline-and-indent-and-indent))
;;      (t
;;       (ahk-only-newline-and-indent)))))

;;;; commenting
;; (defun ahk-comment-region (start end &optional arg)
;;   "Comment or uncomment each line in the region from START to END.
;; If no region is active use the current line."
;;   (interactive (if (region-active-p)
;;                    (list (region-beginning)
;;                          (region-end)
;;                          current-prefix-arg)
;;                  (let (start end)
;;                    (beginning-of-line)
;;                    (setq start (point))
;;                    (forward-line)
;;                    (setq end (point))
;;                    (list start end current-prefix-arg))))
;;   (save-excursion
;;     (comment-region start end arg)))

;; ;; implementation using “newcomment.el”.
;; (defun ahk-comment-dwim (arg)
;; "Comment or uncomment current line or region in a smart way.
;; For detail, see `comment-dwim'."
;;    (interactive "*P")
;;    (require 'newcomment)
;;    (let ((deactivate-mark nil) (comment-start ";") (comment-end ""))
;;      (comment-dwim arg)))

;; (defun ahk-comment-dwims (arg)
;;   ;; comment-dwim for ahk
;;   (interactive "*P")
;;   (let ((indent-line-function (lambda () (insert ""))))
;;     (comment-dwim arg)))

;;; font-lock

(defvar ahk-commands
  '("AllowSameLineComments" "ClipboardTimeout" "CommentFlag" "ErrorStdOut" "EscapeChar" "HotkeyInterval" "HotkeyModifierTimeout" "Hotstring" "IfWinActive" "IfWinExist" "IfWinNotActive" "IfWinNotExist" "Include" "IncludeAgain" "InstallKeybdHook" "InstallMouseHook" "KeyHistory" "LTrim" "MaxHotkeysPerInterval" "MaxMem" "MaxThreads" "MaxThreadsBuffer" "MaxThreadsPerHotkey" "NoEnv" "NoTrayIcon" "Persistent" "SingleInstance" "UseHook" "WinActivateForce" "AutoTrim" "BlockInput" "Break" "Click" "ClipWait" "Continue" "Control" "ControlClick" "ControlFocus" "ControlGet" "ControlGetFocus" "ControlGetPos" "ControlGetText" "ControlMove" "ControlSend" "ControlSendRaw" "ControlSetText" "CoordMode" "Critical" "DetectHiddenText" "DetectHiddenWindows" "Drive" "DriveGet" "DriveSpaceFree" "Edit" "Else" "EnvAdd" "EnvDiv" "EnvGet" "EnvMult" "EnvSet" "EnvSub" "EnvUpdate" "Exit" "ExitApp" "FileAppend" "FileCopy" "FileCopyDir" "FileCreateDir" "FileCreateShortcut" "FileDelete" "FileGetAttrib" "FileGetShortcut" "FileGetSize" "FileGetTime" "FileGetVersion" "FileInstall" "FileMove" "FileMoveDir" "FileRead" "FileReadLine" "FileRecycle" "FileRecycleEmpty" "FileRemoveDir" "FileSelectFile" "FileSelectFolder" "FileSetAttrib" "FileSetTime" "FormatTime" "GetKeyState" "Gosub" "Goto" "GroupActivate" "GroupAdd" "GroupClose" "GroupDeactivate" "Gui" "GuiControl" "GuiControlGet" "Hotkey" "If" "IfEqual" "IfExist" "IfGreater" "IfGreaterOrEqual" "IfInString" "IfLess" "IfLessOrEqual" "IfMsgBox" "IfNotEqual" "IfNotExist" "IfNotInString" "IfWinActive" "IfWinExist" "IfWinNotActive" "IfWinNotExist" "ImageSearch" "IniDelete" "IniRead" "IniWrite" "Input" "InputBox" "KeyHistory" "KeyWait" "ListHotkeys" "ListLines" "ListVars" "Loop" "Menu" "MouseClick" "MouseClickDrag" "MouseGetPos" "MouseMove" "MsgBox" "OnExit" "OutputDebug" "Pause" "PixelGetColor" "PixelSearch" "PostMessage" "Process" "Progress" "Random" "RegDelete" "RegRead" "RegWrite" "Reload" "Repeat" "Return" "Run" "RunAs" "RunWait" "Send" "SendEvent" "SendInput" "SendMessage" "SendMode" "SendPlay" "SendRaw" "SetBatchLines" "SetCapslockState" "SetControlDelay" "SetDefaultMouseSpeed" "SetEnv" "SetFormat" "SetKeyDelay" "SetMouseDelay" "SetNumlockState" "SetScrollLockState" "SetStoreCapslockMode" "SetTimer" "SetTitleMatchMode" "SetWinDelay" "SetWorkingDir" "Shutdown" "Sleep" "Sort" "SoundBeep" "SoundGet" "SoundGetWaveVolume" "SoundPlay" "SoundSet" "SoundSetWaveVolume" "SplashImage" "SplashTextOff" "SplashTextOn" "SplitPath" "StatusBarGetText" "StatusBarWait" "StringCaseSense" "StringGetPos" "StringLeft" "StringLen" "StringLower" "StringMid" "StringReplace" "StringRight" "StringSplit" "StringTrimLeft" "StringTrimRight" "StringUpper" "Suspend" "SysGet" "Thread" "ToolTip" "Transform" "TrayTip" "URLDownloadToFile" "While" "WinActivate" "WinActivateBottom" "WinClose" "WinGet" "WinGetActiveStats" "WinGetActiveTitle" "WinGetClass" "WinGetPos" "WinGetText" "WinGetTitle" "WinHide" "WinKill" "WinMaximize" "WinMenuSelectItem" "WinMinimize" "WinMinimizeAll" "WinMinimizeAllUndo" "WinMove" "WinRestore" "WinSet" "WinSetTitle" "WinShow" "WinWait" "WinWaitActive" "WinWaitClose" "WinWaitNotActive")
  "AHK keywords.")

(defvar ahk-functions
  '("Abs" "ACos" "Asc" "ASin" "ATan" "Ceil" "Chr" "Cos" "DllCall" "Exp" "FileExist" "Floor" "GetKeyState" "IL_Add" "IL_Create" "IL_Destroy" "InStr" "IsFunc" "IsLabel" "Ln" "Log" "LV_Add" "LV_Delete" "LV_DeleteCol" "LV_GetCount" "LV_GetNext" "LV_GetText" "LV_Insert" "LV_InsertCol" "LV_Modify" "LV_ModifyCol" "LV_SetImageList" "Mod" "NumGet" "NumPut" "OnMessage" "RegExMatch" "RegExReplace" "RegisterCallback" "Round" "SB_SetIcon" "SB_SetParts" "SB_SetText" "Sin" "Sqrt" "StrLen" "SubStr" "Tan" "TV_Add" "TV_Delete" "TV_GetChild" "TV_GetCount" "TV_GetNext" "TV_Get" "TV_GetParent" "TV_GetPrev" "TV_GetSelection" "TV_GetText" "TV_Modify" "VarSetCapacity" "WinActive" "WinExist")
  "AHK functions.")

(defvar ahk-keywords
  '("ACos" "ASin" "ATan" "Abort" "AboveNormal" "Abs" "Add" "All" "Alnum" "Alpha" "AltSubmit" "AltTab" "AltTabAndMenu" "AltTabMenu" "AltTabMenuDismiss" "AlwaysOnTop" "And" "Asc" "AutoSize" "Background" "BackgroundTrans" "BelowNormal" "Between" "BitAnd" "BitNot" "BitOr" "BitShiftLeft" "BitShiftRight" "BitXOr" "Border" "Bottom" "Bottom" "Button" "Buttons" "ByRef" "Cancel" "Cancel" "Capacity" "Caption" "Ceil" "Center" "Center" "Check" "Check3" "Checkbox" "Checked" "CheckedGray" "Choose" "ChooseString" "Chr" "Click" "Close" "Close" "Color" "ComboBox" "Contains" "ControlList" "Cos" "Count" "DDL" "Date" "DateTime" "Days" "Default" "Delete" "DeleteAll" "Delimiter" "Deref" "Destroy" "Digit" "Disable" "Disabled" "DropDownList" "Eject" "Enable" "Enabled" "Error" "ExStyle" "Exist" "Exp" "Expand" "FileSystem" "First" "Flash" "Float" "FloatFast" "Floor" "Focus" "Font" "Grid" "Group" "GroupBox" "GuiClose" "GuiContextMenu" "GuiDropFiles" "GuiEscape" "GuiSize" "HKCC" "HKCR" "HKCU" "HKEY_CLASSES_ROOT" "HKEY_CURRENT_CONFIG" "HKEY_CURRENT_USER" "HKEY_LOCAL_MACHINE" "HKEY_USERS" "HKLM" "HKU" "HScroll" "Hdr" "Hidden" "Hide" "High" "Hours" "ID" "IDLast" "Icon" "IconSmall" "Ignore" "ImageList" "In" "Integer" "IntegerFast" "Interrupt" "Is" "Join" "LTrim" "Label" "Label" "LastFound" "LastFoundExist" "Left" "Limit" "Lines" "List" "ListBox" "ListView" "Ln" "Lock" "Log" "Logoff" "Low" "Lower" "Lowercase" "MainWindow" "Margin" "MaxSize" "Maximize" "MaximizeBox" "MinMax" "MinSize" "Minimize" "MinimizeBox" "Minutes" "Mod" "MonthCal" "Mouse" "Move" "Multi" "NA" "No" "NoActivate" "NoDefault" "NoHide" "NoIcon" "NoMainWindow" "NoSort" "NoSortHdr" "NoStandard" "NoTab" "NoTimers" "Normal" "Not" "Number" "Number" "Off" "Ok" "On" "Or" "OwnDialogs" "Owner" "Parse" "Password" "Password" "Pic" "Picture" "Pixel" "Pos" "Pow" "Priority" "ProcessName" "REG_BINARY" "REG_DWORD" "REG_EXPAND_SZ" "REG_MULTI_SZ" "REG_SZ" "RGB" "RTrim" "Radio" "Range" "Read" "ReadOnly" "Realtime" "Redraw" "Region" "Relative" "Rename" "Report" "Resize" "Restore" "Retry" "Right" "Round" "Screen" "Seconds" "Section" "Section" "Serial" "SetLabel" "ShiftAltTab" "Show" "Sin" "Single" "Slider" "SortDesc" "Sqrt" "Standard" "Status" "StatusBar" "StatusCD" "Style" "Submit" "SysMenu" "Tab" "Tab2" "TabStop" "Tan" "Text" "Text" "Theme" "Tile" "Time" "Tip" "ToggleCheck" "ToggleEnable" "ToolWindow" "Top" "Top" "Topmost" "TransColor" "Transparent" "Tray" "TreeView" "TryAgain" "Type" "UnCheck" "Unicode" "Unlock" "UpDown" "Upper" "Uppercase" "UseErrorLevel" "VScroll" "Vis" "VisFirst" "Visible" "Wait" "WaitClose" "WantCtrlA" "WantF2" "WantReturn" "Wrap" "Xdigit" "Yes" "ahk_class" "ahk_group" "ahk_id" "ahk_pid" "bold" "global" "italic" "local" "norm" "static" "strike" "underline" "xm" "xp" "xs" "ym" "yp" "ys" "{AltDown}" "{AltUp}" "{Blind}" "{Click}" "{CtrlDown}" "{CtrlUp}" "{LWinDown}" "{LWinUp}" "{RWinDown}" "{RWinUp}" "{Raw}" "{ShiftDown}" "{ShiftUp}")
  "AHK lang keywords.")

(defvar ahk-variables
  '("A_AhkPath" "A_AhkVersion" "A_AppData" "A_AppDataCommon" "A_AutoTrim" "A_BatchLines" "A_CaretX" "A_CaretY" "A_ComputerName" "A_ControlDelay" "A_Cursor" "A_DD" "A_DDD" "A_DDDD" "A_DefaultMouseSpeed" "A_Desktop" "A_DesktopCommon" "A_DetectHiddenText" "A_DetectHiddenWindows" "A_EndChar" "A_EventInfo" "A_ExitReason" "A_FormatFloat" "A_FormatInteger" "A_Gui" "A_GuiEvent" "A_GuiControl" "A_GuiControlEvent" "A_GuiHeight" "A_GuiWidth" "A_GuiX" "A_GuiY" "A_Hour" "A_IconFile" "A_IconHidden" "A_IconNumber" "A_IconTip" "A_Index" "A_IPAddress1" "A_IPAddress2" "A_IPAddress3" "A_IPAddress4" "A_ISAdmin" "A_IsCompiled" "A_IsCritical" "A_IsPaused" "A_IsSuspended" "A_KeyDelay" "A_Language" "A_LastError" "A_LineFile" "A_LineNumber" "A_LoopField" "A_LoopFileAttrib" "A_LoopFileDir" "A_LoopFileExt" "A_LoopFileFullPath" "A_LoopFileLongPath" "A_LoopFileName" "A_LoopFileShortName" "A_LoopFileShortPath" "A_LoopFileSize" "A_LoopFileSizeKB" "A_LoopFileSizeMB" "A_LoopFileTimeAccessed" "A_LoopFileTimeCreated" "A_LoopFileTimeModified" "A_LoopReadLine" "A_LoopRegKey" "A_LoopRegName" "A_LoopRegSubkey" "A_LoopRegTimeModified" "A_LoopRegType" "A_MDAY" "A_Min" "A_MM" "A_MMM" "A_MMMM" "A_Mon" "A_MouseDelay" "A_MSec" "A_MyDocuments" "A_Now" "A_NowUTC" "A_NumBatchLines" "A_OSType" "A_OSVersion" "A_PriorHotkey" "A_ProgramFiles" "A_Programs" "A_ProgramsCommon" "A_ScreenHeight" "A_ScreenWidth" "A_ScriptDir" "A_ScriptFullPath" "A_ScriptName" "A_Sec" "A_Space" "A_StartMenu" "A_StartMenuCommon" "A_Startup" "A_StartupCommon" "A_StringCaseSense" "A_Tab" "A_Temp" "A_ThisFunc" "A_ThisHotkey" "A_ThisLabel" "A_ThisMenu" "A_ThisMenuItem" "A_ThisMenuItemPos" "A_TickCount" "A_TimeIdle" "A_TimeIdlePhysical" "A_TimeSincePriorHotkey" "A_TimeSinceThisHotkey" "A_TitleMatchMode" "A_TitleMatchModeSpeed" "A_UserName" "A_WDay" "A_WinDelay" "A_WinDir" "A_WorkingDir" "A_YDay" "A_YEAR" "A_YWeek" "A_YYYY" "Clipboard" "ClipboardAll" "ComSpec" "ErrorLevel" "ProgramFiles" "True" "False" )
  "AHK variables.")

(defvar ahk-keys
  '("Alt" "AltDown" "AltUp" "AppsKey" "BS" "BackSpace" "Browser_Back" "Browser_Favorites" "Browser_Forward" "Browser_Home" "Browser_Refresh" "Browser_Search" "Browser_Stop" "CapsLock" "Control" "Ctrl" "CtrlBreak" "CtrlDown" "CtrlUp" "Del" "Delete" "Down" "End" "Enter" "Esc" "Escape" "F1" "F10" "F11" "F12" "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F2" "F20" "F21" "F22" "F23" "F24" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "Home" "Ins" "Insert" "Joy1" "Joy10" "Joy11" "Joy12" "Joy13" "Joy14" "Joy15" "Joy16" "Joy17" "Joy18" "Joy19" "Joy2" "Joy20" "Joy21" "Joy22" "Joy23" "Joy24" "Joy25" "Joy26" "Joy27" "Joy28" "Joy29" "Joy3" "Joy30" "Joy31" "Joy32" "Joy4" "Joy5" "Joy6" "Joy7" "Joy8" "Joy9" "JoyAxes" "JoyButtons" "JoyInfo" "JoyName" "JoyPOV" "JoyR" "JoyU" "JoyV" "JoyX" "JoyY" "JoyZ" "LAlt" "LButton" "LControl" "LCtrl" "LShift" "LWin" "LWinDown" "LWinUp" "Launch_App1" "Launch_App2" "Launch_Mail" "Launch_Media" "Left" "MButton" "Media_Next" "Media_Play_Pause" "Media_Prev" "Media_Stop" "NumLock" "Numpad0" "Numpad1" "Numpad2" "Numpad3" "Numpad4" "Numpad5" "Numpad6" "Numpad7" "Numpad8" "Numpad9" "NumpadAdd" "NumpadClear" "NumpadDel" "NumpadDiv" "NumpadDot" "NumpadDown" "NumpadEnd" "NumpadEnter" "NumpadHome" "NumpadIns" "NumpadLeft" "NumpadMult" "NumpadPgdn" "NumpadPgup" "NumpadRight" "NumpadSub" "NumpadUp" "PGDN" "PGUP" "Pause" "PrintScreen" "RAlt" "RButton" "RControl" "RCtrl" "RShift" "RWin" "RWinDown" "RWinUp" "Right" "ScrollLock" "Shift" "ShiftDown" "ShiftUp" "Space" "Tab" "Up" "Volume_Down" "Volume_Mute" "Volume_Up" "WheelDown" "WheelLeft" "WheelRight" "WheelUp" "XButton1" "XButton2")
  "AHK keywords for keys.")

(defvar ahk-operators
  '(","
    "\\\\"
    "\\^=" "\\!=" "&&" "&="
    "\\*\\*" "\\*=" "\\*" "//" "//=" "/=" "/"
    "\\+\\+" "\\+=" "\\+" "--" "-=" "-"
    "\\.=" ":=" ":"
    "<<" "<<=" "<=" "<" "==" "=" ">=" ">>" ">>=" ">"
    "\\?"
    )
  "AHK operators.")

  (defvar ahk-commands-regexp (regexp-opt ahk-commands 'words))
  (defvar ahk-functions-regexp (regexp-opt ahk-functions 'words))
  (defvar ahk-keywords-regexp (regexp-opt ahk-keywords 'words))
  (defvar ahk-variables-regexp (regexp-opt ahk-variables 'words))
  (defvar ahk-keys-regexp (regexp-opt ahk-keys 'words))
  (defvar ahk-operators-regexp (regexp-opt ahk-operators 'words))

(defvar ahk-font-lock-keywords nil )
(setq ahk-font-lock-keywords
      `(
        ("\\s-*;.*$" . font-lock-comment-face)
        ("^/\\*\\(.*\r?\n\\)*\\(\\*/\\)?" . font-lock-comment-face)
                                        ;           '(ahk-fontify-comment .
        ("^\\([^ \t\n:^=]+\\):" . (1 font-lock-builtin-face))
        ("%[^% ]+%" . font-lock-variable-name-face)
        (,ahk-commands-regexp . ahk-mode-command-name-face)
        (,ahk-functions-regexp . font-lock-function-name-face)
        (,ahk-keywords-regexp . font-lock-keyword-face)
        (,ahk-variables-regexp . font-lock-variable-name-face)
        (,ahk-keys-regexp . font-lock-constant-face)
        (,ahk-operators-regexp . font-lock-warning-face)
        ;; note: order matters
        ))

;; keyword completion
(defvar ahk-kwdList nil "AHK keywords.")

(defvar ahk-all-keywords nil "list of all ahk keywords")
(setq ahk-all-keywords (append ahk-commands ahk-functions ahk-keywords ahk-variables))


(setq ahk-kwdList (make-hash-table :test 'equal))
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-commands)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-functions)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-keywords)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-variables)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-keys)
(put 'ahk-kwdList 'risky-local-variable t)


(defun ahk-completion-at-point ()
  "Complete the current work using the list of all syntax's."
  (interactive)
  (let ((pt (point)))
    (if (and (or (save-excursion (re-search-backward "\\<\\w+"))
                 (looking-at "\\<\\w+"))
             (= (match-end 0) pt))
        (let ((start (match-beginning 0))
              (prefix (match-string 0))
              (completion-ignore-case t)
              completions)
          (list start pt (all-completions prefix ahk-all-keywords) :exclusive 'no)))))

  ;; (interactive)
  ;; (let ((pt (point)) ;; collect point
  ;;       start end)

  ;;   (save-excursion ;; collect the program name
  ;;     (comint-bol)
  ;;     (re-search-forward "\\(\\S +\\)\\s ?"))
  ;;   (if (and (>= pt (match-beginning 1))
  ;;            (<= pt (match-end 1)))
  ;;       () ;; if we're still entering the command, pass completion on to
  ;;     ;; comint-completion-at-point by returning nil

  ;;     (let ((command (match-string-no-properties 1)))
;; (when (member* command my-commands :test 'string= :key 'car)
;;   ;; If the command is one of my-commands, use the associated completions
;;   (goto-char pt)
;;   (let ((start
;;          (save-excursion
;;            (skip-syntax-backward "^ ")
;;            (point))))

;;     (list start pt (cdr (assoc command my-commands)) :exclusive 'no)))))))

;; (defun ahk-complete ()
;;   "Indent current line when at the beginning or complete current command."
;;   (interactive)

;;   (if (looking-at "\\w+")
;;       (goto-char (match-end 0)))

;;   (let ((end (point)))
;;     (if (and (or (save-excursion (re-search-backward "\\<\\w+"))
;;                  (looking-at "\\<\\w+"))
;;              (= (match-end 0) end))
;;         (let ((start (match-beginning 0))
;;               (prefix (match-string 0))
;;               (completion-ignore-case t)
;;               completions)
;;           (setq completions (all-completions prefix ahk-all-keywords))
;;           (if (eq completions nil)
;;               nil;(error "Unknown command prefix <%s>!" prefix)
;;             (if (> (length completions) 1)
;;                 (setq completions
;;                       (completing-read "Complete command: "
;;                                        (mapcar (lambda (c) (list c))
;;                                                completions)
;;                                        nil t prefix)))


;;             (delete-region start end)
;;             (if (listp completions) (setq completions (car completions)))
;;             (insert completions)
;;             (let ((help (assoc completions ahk-all-keywords)))
;;               (if help (message "%s" (mapconcat 'identity help ""))))
;;             )))))

;; (defun ahk-complete-symbol ()
;;   "Perform keyword completion on word before cursor.
;; Keywords include all AHK's event handlers, functions, and CONSTANTS."
;;   (interactive)
;;   (let ((posEnd (point))
;;          (meat (thing-at-point 'symbol))
;;          maxMatchResult)

;;     (when (not meat) (setq meat ""))

;;     (setq maxMatchResult (try-completion meat ahk-kwdList))
;;     (cond ((eq maxMatchResult t))
;;           ((null maxMatchResult)
;;            (message "Can't find completion for “%s”" meat)
;;            (ding))
;;           ((not (string= meat maxMatchResult))
;;            (delete-region (- posEnd (length meat)) posEnd)
;;            (insert maxMatchResult))
;;           (t (message "Making completion list...")
;;              (with-output-to-temp-buffer "*Completions*"
;;                (display-completion-list
;;                 (all-completions meat ahk-kwdList)
;;                 meat))
;;              (message "Making completion list...%s" "done")))))

;; clear memory
(setq ahk-commands nil)
(setq ahk-functions nil)
(setq ahk-keywords nil)
(setq ahk-variables nil)
(setq ahk-keys nil)

(define-derived-mode ahk-mode prog-mode "Autohotkey Mode"
  "Major mode for editing AutoHotkey script (AHK).

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key Bindings
\\{ahk-mode-map}"

  (interactive)
  (kill-all-local-variables)

  (c-mode) ; for indentation
  (set-syntax-table ahk-mode-syntax-table)

  (setq major-mode 'ahk-mode
        mode-name "AHK"
        indent-region-function 'ahk-indent-region)

  (use-local-map ahk-mode-map)
  (easy-menu-add ahk-menu)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((ahk-font-lock-keywords) nil t))

  ;; clear memory
  (setq ahk-commands-regexp nil)
  (setq ahk-functions-regexp nil)
  (setq ahk-keywords-regexp nil)
  (setq ahk-variables-regexp nil)
  (setq ahk-keys-regexp nil)

  ;; set commenting options
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")

  (add-hook 'completion-at-point-functions 'ahk-completion-at-point nil t)

  (run-mode-hooks 'ahk-mode-hook))

(add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode))

(provide 'ahk-mode)

;;; ahk-mode.el ends here
