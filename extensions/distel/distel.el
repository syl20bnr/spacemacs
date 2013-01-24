;;; distel.el --- Top-level of distel package, loads all subparts

;; Prerequisites
(require 'erlang)
(require 'easy-mmode)

(provide 'distel)

;; Customization

(defgroup distel '()
  "Distel and erlang-extended-mode development tools."
  :group 'tools)

(defcustom distel-tags-compliant nil
  "Tags compliant, i.e. let M-. ask for confirmation."
  :type 'boolean
  :group 'distel)

(defcustom distel-inhibit-backend-check nil
  "Don't check for the 'distel' module when we connect to new nodes."
  :type 'boolean
  :group 'distel)

;; Compatibility with XEmacs
(unless (fboundp 'define-minor-mode)
  (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

;; Distel modules

(require 'erl)
(require 'erl-service)
(require 'edb)

(require 'distel-ie)

(defun distel-setup ()
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook))

(defun distel-erlang-mode-hook ()
  "Function to enable the Distel extensions to Erlang mode.
You can add this to `erlang-mode-hook' with:
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)"
  (erlang-extended-mode t))

;; Extended feature key bindings (C-c C-d prefix)

(define-minor-mode erlang-extended-mode
  "Extensions to erlang-mode for communicating with a running Erlang node.

These commands generally communicate with an Erlang node. The first
time you use one, you will be prompted for the name of the node to
use. This name will be cached for future commands. To override the
cache, give a prefix argument with C-u before using the command.
\\<erlang-extended-mode-map>

\\[erl-choose-nodename]	- Set Erlang nodename.
\\[erl-ping]	- Check connection between Emacs and Erlang.

\\[erl-find-source-under-point]		- Jump from a function call to its definition.
\\[erl-find-source-unwind]		- Jump back from a function definition (multi-level).

\\[erl-reload-module]	- (Re)load an Erlang module.
\\[erl-reload-modules]	- Reload all Erlang modules that are out of date.
\\[erl-find-module]	- Find a module.
\\[erl-who-calls]	- Who calls function under point.

\\[erl-process-list]	- List all Erlang processes (\"pman\").
\\[edb-toggle-interpret]	- Toggle debug interpreting of the module.
C-c C-d b/\\[edb-toggle-breakpoint]	- Toggle a debugger breakpoint at the current line.
\\[edb-monitor]	- Popup the debugger's process monitor buffer.
\\[edb-synch-breakpoints]	- Synchronizes current breakpoints to erlang.
\\[edb-save-dbg-state]	- Save set of interpreted modules and breakpoints.
\\[edb-restore-dbg-state]	- Restore saved set of interpreted modules and breakpoints.

\\[erl-eval-expression]	- Evaluate an erlang expression from the minibuffer.
\\[erl-ie-show-session]	- Create an interactive \"session\" buffer.

\\[erl-complete]		- Complete a module or remote function name.
\\[erl-refactor-subfunction]	- Refactor expressions in the region as a new function.

\\[erl-find-sig-under-point]	- Show the signature for the function under point.
\\[erl-find-doc-under-point]	- Show the HTML documentation for the function under point.
\\[erl-find-sig]	- Show the signature for a function.
\\[erl-find-doc]	- Show the HTML documentation for a function.
\\[erl-fdoc-describe]	- Describe a function with fdoc.
\\[erl-fdoc-apropos]	- Describe functions matching a regexp with fdoc.

\\[fprof]	- Profile (with fprof) an expression from the minibuffer.
\\[fprof-analyse]	- View profiler results from an \"fprof:analyse\" file.

 \"fdoc\" works by looking at the source code. The HTML doc functions
needs the OTP HTML docs to be installed. who-calls makes use of
xref, and can take quite some time to initialize.

  Most commands that pop up new buffers will save your original
window configuration, so that you can restore it by pressing
'q'. Use `describe-mode' (\\[describe-mode]) on any Distel buffer
when you want to know what commands are available. To get more
information about a particular command, use \"\\[describe-key]\"
followed by the command's key sequence. For general information
about Emacs' online help, use \"\\[help-for-help]\".
"
  nil
  nil
  ;; Fake keybinding list just to get the keymap created.
  ;;
  ;; define-minor-mode is very inconvenient for redefining keybindings
  ;; so we do that by hand, below.
  '(("\M-." 'undefined)))

(defconst distel-keys
  '(("\C-c\C-di" edb-toggle-interpret)
    ("\C-x "     edb-toggle-breakpoint)
    ("\C-c\C-db" edb-toggle-breakpoint)
    ("\C-c\C-ds" edb-synch-breakpoints)
    ("\C-c\C-dS" edb-save-dbg-state)
    ("\C-c\C-dR" edb-restore-dbg-state)
    ("\C-c\C-dm" edb-monitor)          
    ("\C-c\C-d:" erl-eval-expression)
    ("\C-c\C-dL" erl-reload-module)
    ("\C-c\C-dr" erl-reload-modules)
    ("\C-c\C-dp" fprof)
    ("\C-c\C-dP" fprof-analyse)
    ("\C-c\C-d." erl-find-source-under-point)
    ("\C-c\C-d," erl-find-source-unwind)
    ("\C-c\C-dl" erl-process-list)
    ("\C-\M-i"   erl-complete)	; M-TAB
    ("\M-?"      erl-complete)	; Some windowmanagers hijack M-TAB..
    ("\C-c\C-de" erl-ie-show-session)
    ("\C-c\C-df" erl-refactor-subfunction)
    ("\C-c\C-dF" erl-find-module)
    ("\C-c\C-dg" erl-ping)
    ("\C-c\C-dA" erl-show-arglist)
    ("\C-c\C-dh" erl-find-doc-under-point)
    ("\C-c\C-dH" erl-find-doc)
    ("\C-c\C-dz" erl-find-sig-under-point)
    ("\C-c\C-dZ" erl-find-sig)
    ("\C-c\C-dd" erl-fdoc-describe)
    ("\C-c\C-da" erl-fdoc-apropos)
    ("\C-c\C-dw" erl-who-calls)
    ("\C-c\C-dn" erl-choose-nodename)
    ("("         erl-openparen)
    ;; Possibly "controversial" shorter keys
    ("\M-."      erl-find-source-under-point)	; usually `find-tag'
    ("\M-*"      erl-find-source-unwind) ; usually `pop-tag-mark'
    ("\M-,"      erl-find-source-unwind) ; usually `tags-loop-continue'
    ;;("\M-/"      erl-complete) ; usually `dabbrev-expand'
    )
  "Keys to bind in distel-mode-map.")

(defun distel-bind-keys ()
  "Bind `distel-keys' in `erlang-extended-mode-map'."
  (interactive)
  (dolist (spec distel-keys)
    (define-key erlang-extended-mode-map (car spec) (cadr spec))))

(distel-bind-keys)

;; Setup mode-line info for erlang-extended-mode
;;
;; NB: Would use the LIGHTER argument for define-minor-mode, but it's
;; not working portably: my copy of Emacs21 disagrees with emacs20 and
;; xemacs on whether it should be quoted.

(defvar distel-modeline-node nil
  "When non-nil the short name of the currently connected node.")

(add-to-list 'minor-mode-alist
	     '(erlang-extended-mode
	       (" EXT"
                (distel-modeline-node (":" distel-modeline-node) "")
                (edb-module-interpreted "<interpreted>" ""))))

(add-hook 'erlang-extended-mode-hook
	  '(lambda ()
	     (if erlang-extended-mode
		 (distel-init)
	       (distel-finish))))

(defun distel-init ()
  (setq erlang-menu-items
	(erlang-menu-add-below 'distel-menu-items
			       'erlang-menu-compile-items
			       erlang-menu-items))
  (erlang-menu-init))

(defun distel-finish ()
  (setq erlang-menu-items
	(erlang-menu-delete 'distel-menu-items erlang-menu-items))
  (erlang-menu-init))
  
(defvar distel-menu-items
  '(nil
    ("Distel"
     (("List all erlang processes" erl-process-list)
      ("Eval an erlang expression" erl-eval-expression)
      ("Reload an erlang module" erl-reload-module)
      ("Reload all changed erlang modules" erl-reload-modules)
      nil
      ("Profile an erlang expression" fprof)
      ("View profiler results" fprof-analyse)
      nil
      ("Toggle debug interpreting of the module" edb-toggle-interpret)
      ("Toggle a breakpoint at current line" edb-toggle-breakpoint)
      ("Synchronizes current breakpoints to erlang" edb-synch-breakpoints)
      ("Save debugger state" edb-save-dbg-state)
      ("Restore debugger state" edb-restore-dbg-state)
      ("Popup the debugger process monitor" edb-monitor)
      nil
      ("Create an interactive erlang session buffer" erl-ie-show-session)
      nil
      ("Specify which node to connect to" erl-choose-nodename)
      )))
  "*Description of the Distel menu used by Erlang Extended mode.

Please see the documentation of `erlang-menu-base-items'.")


;; Bug reportage

(defvar distel-bugs-address "distel-hackers@lists.sourceforge.net"
  "Email address to send distel bugs to.")

(eval-when-compile (require 'font-lock))

(defun report-distel-problem (summary)
  "Report a bug to the distel-hackers mailing list."
  (interactive (list (read-string "One-line summary: ")))
  (compose-mail distel-bugs-address
		(format "PROBLEM: %s" summary))
  (require 'font-lock)
  (insert (propertize "\
;; Distel bug report form.
;;
;; This is an email message buffer for you to fill in information
;; about your problem. When finished, you can enter \"C-c C-c\" to
;; send the report to the distel-hackers mailing list - or update the
;; 'To: ' header line to send it somewhere else.
;;
;; Please describe the problem in detail in this blank space:

"
		      'face font-lock-comment-face))
  (save-excursion
    (insert (propertize "\


;; Below is some automatically-gathered debug information. Please make
;; sure it doesn't contain any secrets that you don't want to send. If
;; you decide to censor it or have any other special notes, please
;; describe them here:



"
			'face font-lock-comment-face))
    (insert "[ ---- Automatically gathered trace information ---- ]\n\n")
    (insert (format "Emacs node name: %S\n\n" erl-node-name))
    (insert (format "Node of most recent command: %S\n\n" erl-nodename-cache))
    (insert "Recent *Messages*:\n")
    (distel-indented-insert (distel-last-lines "*Messages*" 15) 2)
    (insert "\n\n")
    (when erl-nodename-cache
      (insert (format "Recent interactions with %S:\n" erl-nodename-cache))
      (distel-indented-insert (distel-last-lines
			       (format "*trace %S*" erl-nodename-cache) 50)
			      2))
    (insert "\n\nThe End.\n\n")))

(defun distel-last-lines (buffer n)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line (- n))
      (buffer-substring (point) (point-max)))))

(defun distel-indented-insert (string level)
  (let ((pos (point)))
    (insert string)
    (indent-rigidly pos (point) level)))

