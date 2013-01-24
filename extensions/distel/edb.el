;;; edb.el --- Erlang debugger front-end

(eval-when-compile (require 'cl))
(require 'erl)
(require 'erl-service)
(require 'erlang)
(require 'ewoc)

(eval-and-compile
  (autoload 'erlang-extended-mode "distel"))

(when (featurep 'xemacs)
  (require 'overlay))

;; Hack for XEmacs compatibility..
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))

;; ----------------------------------------------------------------------
;; Configurables

(defcustom edb-popup-monitor-on-event t
  "*Automatically popup the monitor on interesting events.
An interesting event is an unattached process reaching a breakpoint,
or an attached process exiting."
  :type 'boolean
  :group 'distel)

(defface edb-breakpoint-face
  `((((type tty) (class color))
     (:background "red" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "darkred" :foreground "white"))
    (((class color) (background light))
     (:background "tomato" :foreground "black"))
    (t (:background "gray")))
  "Face for marking a breakpoint definition."
  :group 'distel)

(defface edb-breakpoint-stale-face
  `((((type tty) (class color))
     (:background "yellow" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "purple4"))
    (((class color) (background light))
     (:background "medium purple" :foreground "black"))
    (t (:background "dark gray")))
  "Face for marking a stale breakpoint definition."
  :group 'distel)

;; ----------------------------------------------------------------------
;; Integration with erlang-extended-mode buffers.

(make-variable-buffer-local
 (defvar edb-module-interpreted nil
   "Non-nil means that the buffer's Erlang module is interpreted.
This variable is meaningful in erlang-extended-mode buffers.
The interpreted status refers to the node currently being monitored by
edb."))

(defun edb-setup-source-buffer ()
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'edb-delete-buffer-breakpoints)
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'edb-make-breakpoints-stale)
  (edb-update-interpreted-status)
  (when edb-module-interpreted
    (edb-create-buffer-breakpoints (edb-module))))

(add-hook 'erlang-extended-mode-hook
	  'edb-setup-source-buffer)

;; ----------------------------------------------------------------------
;; EDB minor mode for erlang-mode source files

(defun edb-toggle-interpret (node module file)
  "Toggle debug-interpreting of the current buffer's module."
  (interactive (list (erl-target-node)
		     (edb-module)
		     buffer-file-name))
  (when (edb-ensure-monitoring node)
    (erl-spawn
      (erl-set-name "EDB RPC to toggle interpretation of %S on %S"
		    module node)
      (erl-send-rpc node 'distel 'debug_toggle (list module file))
      (erl-receive (module)
	  ((['rex 'interpreted]
	    (message "Interpreting: %S" module))
	   (['rex 'uninterpreted]
	    (message "Stopped interpreting: %S" module))
	   (['rex ['badrpc reason]]
	    (message "Failed to interpret-toggle: %S" reason)))))))

(defun edb-module ()
  (if (erlang-get-module)
      (intern (erlang-get-module))
    (error "Can't determine module for current buffer")))

(defun edb-toggle-breakpoint (node module line)
  "Toggle a breakpoint on the current line."
  (interactive (list (erl-target-node)
		     (edb-module)
		     (edb-line-number)))
  (unless (edb-module-interpreted-p module)
    (error "Module is not interpreted, can't set breakpoints."))
  (if edb-buffer-breakpoints-stale
      (edb-toggle-stale-breakpoint module line)
    (edb-toggle-real-breakpoint node module line)))

(defun edb-toggle-stale-breakpoint (module line)
  (let ((overlay (edb-first (lambda (ov) (overlay-get ov 'edb-breakpoint))
			    (overlays-in (line-beginning-position)
					 (1+ (line-end-position))))))
    (if overlay
	(delete-overlay overlay)
      (edb-create-breakpoint module line))))

(defun edb-toggle-real-breakpoint (node module line)
  (when (edb-ensure-monitoring node)
    (erl-spawn
      (erl-set-name "EDB RPC to toggle of breakpoint %S:%S on %S"
		    module line node)
      (erl-send-rpc node 'distel 'break_toggle (list module line))
      (erl-receive (module line)
	  ((['rex 'enabled]
	    (message "Enabled breakpoint at %S:%S" module line))
	   (['rex 'disabled]
	    (message "Disabled breakpoint at %S:%S" module line)))))))

(defun edb-module-interpreted-p (module)
  (assoc module edb-interpreted-modules))

(defun edb-line-number ()
  "Current line number."
  ;; Taken from `count-lines' in gud.el
  (save-restriction
    (widen)
    (+ (count-lines 1 (point))
       (if (bolp) 1 0))))

(defun edb-save-dbg-state (node)
  "Save debugger state (modules to interpret and breakpoints).
Use edb-restore-dbg-state to restore the state to the erlang node."
  (interactive (list (erl-target-node)))
  (let ((do-save nil))
    (when (or (null edb-saved-interpreted-modules)
	      (y-or-n-p "You already have a saved debugger state, continue? "))
      (setq edb-saved-interpreted-modules edb-interpreted-modules)
      (edb-save-breakpoints node)
      (message "Debugger state saved."))))

(defun edb-restore-dbg-state (node)
  "Restore debugger state (modules to interpret and breakpoints)."
  (interactive (list (erl-target-node)))
  (if edb-saved-interpreted-modules
      (when (edb-ensure-monitoring node)
	(erl-spawn
	  (erl-set-name "EDB RPC to restore debugger state on %S" node)
	  (erl-send-rpc node 'distel 'debug_add
			(list edb-saved-interpreted-modules))
	  (erl-receive (node)
	      ((['rex 'ok]
		(when (edb-restore-breakpoints
		       node
		       (lambda ()
			 (message "Debugger state restored.")))))))))
    (message "No saved debugger state, aborting.")))


;; ----------------------------------------------------------------------
;; Monitor process

(defvar edb-monitor-buffer nil
  "Monitor process/viewer buffer.")

(defvar edb-monitor-node nil
  "Node we are debug-monitoring.")

(defvar edb-monitor-mode-map nil
  "Keymap for Erlang debug monitor mode.")

(defvar edb-interpreted-modules '()
  "Set of (module filename) being interpreted on the currently monitored node.")

(defvar edb-saved-interpreted-modules '()
  "Set of (module filename) to interpret if edb-restore-dbg-state is called.")

(unless edb-monitor-mode-map
  (setq edb-monitor-mode-map (make-sparse-keymap))
  (define-key edb-monitor-mode-map [return] 'edb-attach-command)
  (define-key edb-monitor-mode-map [(control m)] 'edb-attach-command)
  (define-key edb-monitor-mode-map [?a] 'edb-attach-command)
  (define-key edb-monitor-mode-map [?q] 'erl-bury-viewer)
  (define-key edb-monitor-mode-map [?k] 'erl-quit-viewer))

(defvar edb-processes nil
  "EWOC of processes running interpreted code.")

(defstruct (edb-process
	    (:constructor nil)
	    (:constructor make-edb-process (pid mfa status info)))
  pid mfa status info)

(defun edb-monitor-mode ()
  "Major mode for viewing debug'able processes.

Available commands:
\\[edb-attach-command]	- Attach to the process at point.
\\[erl-bury-viewer]	- Hide the monitor window.
\\[erl-quit-viewer]	- Quit monitor."
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq erl-old-window-configuration (current-window-configuration))
  (use-local-map edb-monitor-mode-map)
  (setq mode-name "EDB Monitor")
  (setq major-mode 'edb-monitor-mode))

(defun edb-monitor-insert-process (p)
  (let ((buffer-read-only nil)
	(text (edb-monitor-format (erl-pid-to-string (edb-process-pid p))
				  (edb-process-mfa p)
				  (edb-process-status p)
				  (edb-process-info p))))
    (put-text-property 0 (length text) 'erl-pid (edb-process-pid p) text)
    (insert text)))

(defun edb-monitor-format (pid mfa status info)
  (format "%s %s %s %s"
	  (padcut pid 12)
	  (padcut mfa 21)
	  (padcut status 9)
	  (cut info 21)))

(defun padcut (s w)
  (let ((len (length s)))
    (cond ((= len w) s)
	  ((< len w) (concat s (make-string (- w len) ? )))
	  ((> len w) (substring s 0 w)))))

(defun cut (s w)
  (if (> (length s) w)
      (substring s 0 w)
    s))

(defun edb-monitor-header ()
  (edb-monitor-format "PID" "Initial Call" "Status" "Info"))

(defun edb-monitor (node)
  (interactive (list (erl-target-node)))
  (when (edb-ensure-monitoring node)
    (unless (get-buffer-window edb-monitor-buffer)
      ;; Update the restorable window configuration
      (with-current-buffer edb-monitor-buffer
	(setq erl-old-window-configuration
	      (current-window-configuration))))
    (pop-to-buffer edb-monitor-buffer)
    (condition-case nil
        (progn 
          (search-forward "break")
          (move-beginning-of-line))
      (error nil))))
    ;;    (goto-char (point-max))
    ;;   (forward-line -2)))

(defun edb-ensure-monitoring (node)
  "Make sure the debug monitor is watching the node.
Returns NIL if this cannot be ensured."
  (if (edb-monitor-node-change-p node)
      (when (y-or-n-p (format "Attach debugger to %S instead of %S? "
			      node edb-monitor-node))
	;; Kill existing edb then start again
	(kill-buffer edb-monitor-buffer)
	(edb-start-monitor node))
    (if (edb-monitor-live-p)
	t
      (edb-start-monitor node))))

(defun edb-monitor-node-change-p (node)
  "Do we have to detach/reattach to debug on NODE?"
  (and (edb-monitor-live-p)
       (not (equal node edb-monitor-node))))

(defun edb-monitor-live-p ()
  "Are we actively debug-monitoring a node?"
  (and edb-monitor-buffer
       (buffer-live-p edb-monitor-buffer)))

(defun edb-monitor-buffer-name (node)
  (format "*edb %S*" node))

(defun edb-start-monitor (node)
  "Start debug-monitoring NODE."
  (erl-spawn
    (erl-set-name "EDB Monitor on %S" node)
    (setq edb-monitor-node node)
    (setq edb-monitor-buffer (current-buffer))
    (rename-buffer (edb-monitor-buffer-name node))
    (edb-monitor-mode)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'edb-monitor-cleanup)
    (erl-send-rpc node 'distel 'debug_subscribe (list erl-self))
    (erl-receive (node)
	((['rex [interpreted breaks snapshot]]
	  (setq edb-interpreted-modules interpreted)
	  (edb-init-breakpoints breaks)
	  (edb-update-source-buffers)
	  (setq edb-processes
		(ewoc-create 'edb-monitor-insert-process
			     (edb-monitor-header)))
	  (mapc (lambda (item)
		  (mlet [pid mfa status info] item
		    (ewoc-enter-last edb-processes
				     (make-edb-process pid
						       mfa
						       status
						       info))))
		snapshot)
	  (&edb-monitor-loop))))))

(defun &edb-monitor-loop ()
  "Monitor process main loop.
Tracks events and state changes from the Erlang node."
  (erl-receive ()
      ((['int ['new_status pid status info]]
	 (let ((proc (edb-monitor-lookup pid)))
	   (if (null proc)
	       (message "Unknown process: %s" (erl-pid-to-string pid))
	     (setf (edb-process-status proc) (symbol-name status))
	     (setf (edb-process-info proc) info)
	     (when (and edb-popup-monitor-on-event
			(edb-interesting-event-p pid status info))
	       (display-buffer (current-buffer))))))
       ;;
       (['int ['new_process (pid mfa status info)]]
	 (ewoc-enter-last edb-processes
			  (make-edb-process pid
					    mfa
					    (symbol-name status)
					    info)))
       ;;
       (['int ['interpret mod file]]
	(push (list mod file) edb-interpreted-modules)
	(edb-update-source-buffers mod))
       ;;
       (['int ['no_interpret mod]]
	(setq edb-interpreted-modules
	      (assq-delete-all mod edb-interpreted-modules))
	(edb-update-source-buffers mod))
       ;;
       (['int ['no_break mod]]
	(edb-delete-breakpoints mod))
       ;;
       (['int ['new_break [[mod line] _info]]]
	(edb-create-breakpoint mod line))
       ;;
       (['int ['delete_break [mod line]]]
	(edb-delete-breakpoint mod line)))
    (ewoc-refresh edb-processes)    
    (&edb-monitor-loop)))

(defun edb-get-buffer (mod)
  (edb-get-buffer2 mod (buffer-list)))

(defun edb-get-buffer2 (mod bufl)
  (if (null bufl) nil
    (with-current-buffer (car bufl)
      (if (and erlang-extended-mode
	       (eq (edb-source-file-module-name) mod))
	  (car bufl)
	(edb-get-buffer2 mod (cdr bufl))))))


(defun edb-interesting-event-p (pid status info)
  (or (and (eq status 'exit)
	   (edb-attached-p pid))
      (and (eq status 'break)
	   (not (edb-attached-p pid)))))

(defun edb-update-interpreted-status ()
  "Update `edb-module-interpreted' for current buffer."
  (when erlang-extended-mode
    (let ((mod (edb-source-file-module-name)))
      (if (and mod (assq mod edb-interpreted-modules))
	  (setq edb-module-interpreted t)
	(setq edb-module-interpreted nil)
	;; the erlang debugger automatically removes breakpoints when a
	;; module becomes uninterpreted, so we match it here
	(edb-delete-breakpoints (edb-source-file-module-name))))
    (force-mode-line-update)))

(defun edb-update-source-buffers (&optional mod)
  "Update the debugging state of all Erlang buffers.
When MOD is given, only update those visiting that module."
  (mapc (lambda (buf)
	  (with-current-buffer buf
	    (when (and erlang-extended-mode
		       (or (null mod)
			   (eq (edb-source-file-module-name) mod)))
	      (edb-update-interpreted-status))))
	(buffer-list)))

(defun edb-source-file-module-name ()
  "Return the Erlang module of the current buffer as a symbol, or NIL."
  (let ((name (erlang-get-module)))
    (if name (intern name) nil)))

(defun edb-monitor-lookup (pid)
  (car (ewoc-collect edb-processes
		     (lambda (p) (equal (edb-process-pid p) pid)))))

(defun edb-monitor-cleanup ()
  "Cleanup state after the edb process exits."
  (setq edb-interpreted-modules '())
  (edb-delete-all-breakpoints)
  (edb-update-source-buffers)
  (setq edb-monitor-node nil))

;; ----------------------------------------------------------------------
;; Attach process

(make-variable-buffer-local
 (defvar edb-pid nil
   "Pid of attached process."))

(make-variable-buffer-local
 (defvar edb-node nil
   "Node of attached process."))

(make-variable-buffer-local
 (defvar edb-module nil
   "Current module source code in attach buffer."))

(make-variable-buffer-local 
 (defvar edb-variables-buffer nil
   "Buffer showing variable bindings of attached process."))

(make-variable-buffer-local 
 (defvar edb-attach-buffer nil
   "True if buffer is attach buffer."))

(defvar edb-attach-with-new-frame nil
  "When true, attaching to a process opens a new frame.")

;; Attach setup

(defun edb-attach-command ()
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if pid
	(progn (when edb-attach-with-new-frame 
		 (select-frame (make-frame)))
	       (edb-attach pid))
      (error "No process at point."))))

(defun edb-attach (pid)
  (let ((old-window-config (current-window-configuration)))
    (delete-other-windows)
    (switch-to-buffer (edb-attach-buffer pid))
    (setq erl-old-window-configuration old-window-config)))

(defun edb-attach-buffer (pid)
  (let ((bufname (edb-attach-buffer-name pid)))
    (or (get-buffer bufname)
	(edb-new-attach-buffer pid))))

(defun edb-new-attach-buffer (pid)
  "Start a new attach process and returns its buffer."
  (erl-pid->buffer
   (erl-spawn
     (erl-set-name "EDB Attach to process %S on %S"
		   (erl-pid-id pid)
		   (erl-pid-node pid))
     (rename-buffer (edb-attach-buffer-name pid))
     ;; We must inhibit the erlang-new-file-hook, otherwise we trigger
     ;; it by entering erlang-mode in an empty buffer
     (let ((erlang-new-file-hook nil))
       (erlang-mode))
     (erlang-extended-mode t)
     (edb-attach-mode t)
     (setq edb-attach-buffer t)
     (message "Entered debugger. Press 'h' for help.")
     (setq buffer-read-only t)
     (erl-send-rpc (erl-pid-node pid)
		   'distel 'debug_attach (list erl-self pid))
     (erl-receive ()
	 ((['rex pid]
	   (assert (erl-pid-p pid))
	   (setq edb-pid pid)
	   (setq edb-node (erl-pid-node pid))
	   (save-excursion (edb-make-variables-window))))
       (&edb-attach-loop)))))

;; Variables listing window

(defun edb-make-variables-window ()
  "Make a window and buffer for viewing variable bindings.
The *Variables* buffer is killed with the current buffer."
  (split-window-vertically (edb-variables-window-height))
  (let ((vars-buf (edb-make-variables-buffer)))
    (setq edb-variables-buffer vars-buf)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
	      (lambda () (kill-buffer edb-variables-buffer)))
    (other-window 1)
    (switch-to-buffer vars-buf)
    (other-window -1)))

(defun edb-variables-window-height ()
  (- (min (/ (window-height) 2) 12)))

(defun edb-make-variables-buffer ()
  "Create the edb variable list buffer."
  (let ((meta-pid edb-pid))
    (with-current-buffer (generate-new-buffer "*Variables*")
      (edb-variables-mode)
      (setq edb-pid meta-pid)
      (current-buffer))))

(defun edb-variables-mode ()
  (kill-all-local-variables)
  (setq major-mode 'edb-variables)
  (setq mode-name "EDB Variables")
  (setq buffer-read-only t)
  (use-local-map edb-variables-mode-map))

(defvar edb-variables-mode-map nil
  "Keymap for EDB variables viewing.")

(when (null edb-variables-mode-map)
  (setq edb-variables-mode-map (make-sparse-keymap))
  (define-key edb-variables-mode-map [?m]          'edb-show-variable)
  (define-key edb-variables-mode-map [(control m)] 'edb-show-variable))

(defun edb-show-variable ()
  "Pop a window showing the full value of the variable at point."
  (interactive)
  (let ((var (get-text-property (point) 'edb-variable-name)))
    (if (null var)
	(message "No variable at point")
      (edb-attach-meta-cmd `[get_binding ,var]))))

;; Attach process states

(defun &edb-attach-loop ()
  "Attached process loop."
  (erl-receive ()
      ((['location mod line pos max]
 	(let ((msg (format "Location: %S:%S (Stack pos: %S/%S)"
			   mod line pos max)))
 	  (setq header-line-format msg))
	(&edb-attach-goto-source mod line))
       (['status status]
	(unless (memq status '(running idle))
	  (message "Unrecognised status: %S" status))
	(setq header-line-format (format "Status: %S" status))
	(setq overlay-arrow-position nil)
	(&edb-attach-loop))
       (['variables vars]
	;; {variables, [{Name, String}]}
	(when (buffer-live-p edb-variables-buffer)
	  (with-current-buffer edb-variables-buffer
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (mapc (lambda (b)
		      (let ((name   (tuple-elt b 1))
			    (string (tuple-elt b 2)))
			(put-text-property 0 (length string)
					   'edb-variable-name name
					   string)
			(insert string)))
		    vars))))
	(&edb-attach-loop))
       (['message msg]
	(message msg)
	(&edb-attach-loop))
       (['show_variable value]
	(save-excursion (display-message-or-view value "*Variable Value*"))
	(&edb-attach-loop))
       (other
	(message "Other: %S" other)
	(&edb-attach-loop)))))

(defun &edb-attach-goto-source (module line)
  "Display MODULE:LINE in the attach buffer and reenter attach loop."
  (if (eq edb-module module)
      (progn (edb-attach-goto-line line)
	     (&edb-attach-loop))
    (&edb-attach-find-source module line)))

(defun &edb-attach-find-source (module line)
  "Load the source code for MODULE into current buffer at LINE.
Once loaded, reenters the attach loop."
  (erl-send-rpc edb-node 'distel 'find_source (list module))
  (erl-receive (module line)
      ((['rex ['ok path]]
	(if (file-regular-p path)
	    (progn (setq edb-module module)
		   (let ((buffer-read-only nil))
		     (erase-buffer)
		     (insert-file-contents path))
		   (edb-delete-buffer-breakpoints)
		   (edb-create-buffer-breakpoints module)
		   (edb-attach-goto-line line))
	  (message "No such file: %s" path))))
    (&edb-attach-loop)))

(defun edb-attach-goto-line (line)
  (goto-line line)
  (setq overlay-arrow-string "=>")
  (setq overlay-arrow-position (copy-marker (point))))

(defun edb-attach-buffer-name (pid)
  (format "*edbproc %s on %S*"
	  (erl-pid-to-string pid)
	  (erl-pid-node pid)))

(defun edb-attached-p (pid)
  "Non-nil when we have an attach buffer viewing PID."
  (buffer-live-p (get-buffer (edb-attach-buffer-name pid))))

;; ----------------------------------------------------------------------
;; Attach minor mode and commands

(define-minor-mode edb-attach-mode
  "Minor mode for debugging an Erlang process.

Available commands:
\\<edb-attach-mode-map>
\\[edb-attach-help]	- Popup this help text.
\\[erl-quit-viewer]	- Quit the viewer (doesn't kill the process)
\\[edb-attach-step]	- Step (into expression)
\\[edb-attach-next]	- Next (over expression)
\\[edb-attach-up]	- Up to the next stack frame
\\[edb-attach-down]	- Down to the next stack frame
\\[edb-attach-continue]	- Continue (until breakpoint)
\\[edb-toggle-breakpoint]	- Toggle a breakpoint on the current line."
  nil
  " (attached)"
  '(([? ] . edb-attach-step)
    ([?n] . edb-attach-next)
    ([?c] . edb-attach-continue)
    ([?u] . edb-attach-up)
    ([?d] . edb-attach-down)
    ([?q] . erl-quit-viewer)
    ([?h] . edb-attach-help)
    ([?b] . edb-toggle-breakpoint)))

(defun edb-attach-help ()
  (interactive)
  (describe-function 'edb-attach-mode))

(defun edb-attach-step ()
  (interactive)
  (edb-attach-meta-cmd 'step))
(defun edb-attach-next ()
  (interactive)
  (edb-attach-meta-cmd 'next))
(defun edb-attach-continue ()
  (interactive)
  (edb-attach-meta-cmd 'continue))
(defun edb-attach-up ()
  (interactive)
  (edb-attach-meta-cmd 'up))
(defun edb-attach-down ()
  (interactive)
  (edb-attach-meta-cmd 'down))

(defun edb-attach-meta-cmd (cmd)
  (erl-send edb-pid `[emacs meta ,cmd]))

;; ----------------------------------------------------------------------
;; Breakpoints

(defvar edb-breakpoints '()
  "List of all breakpoints on the currently monitored node.")

(defvar edb-saved-breakpoints '()
  "List of breakpoints to set if edb-restore-dbg-state is called.")

(make-variable-buffer-local 
 (defvar edb-buffer-breakpoints nil
   "List of active buffer breakpoints."))

(make-variable-buffer-local 
 (defvar edb-buffer-breakpoints-stale nil
   "Nil if the breakpoints in the buffer are stale (out of synch)."))

;; breakpoints
(defun make-bp (mod line) (list mod line))
(defun bp-mod (bp) (car bp))
(defun bp-line (bp) (cadr bp))

;; buffer breakpoints
(defun make-bbp (mod line ov) (list mod line ov))
(defun bbp-mod (bbp) (car bbp))
(defun bbp-line (bbp) (cadr bbp))
(defun bbp-ov (bbp) (caddr bbp))

(defun edb-init-breakpoints (breaks)
  (setq edb-breakpoints
	(mapcar (lambda (pos)
		  (let ((mod (aref pos 0))
			(line (aref pos 1)))
		    (make-bp mod line)))
		breaks))
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when erlang-extended-mode
	 (edb-create-buffer-breakpoints (edb-source-file-module-name)))))
   (buffer-list)))
  

(defun edb-create-breakpoint (mod line)
  "Updates all internal structures in all buffers with new breakpoint."
  (push (make-bp mod line) edb-breakpoints)
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (if (and erlang-extended-mode
		(eq (edb-source-file-module-name) mod))
	   (let ((bbp (make-bbp mod line (edb-make-breakpoint-overlay line))))
	     (push bbp edb-buffer-breakpoints)))))
   (buffer-list)))

(defun edb-delete-all-breakpoints ()
  "Updates all internal structures in all buffers."
  (edb-del-breakpoints
   (lambda (bp) t)
   (lambda (bbp) t)))

(defun edb-delete-breakpoints (mod)
  "Updates all internal structures in all buffers."
  (edb-del-breakpoints
   (lambda (bp) (eq (bp-mod bp) mod))
   (lambda (bbp) (eq (bbp-mod bbp) mod))
   mod))

(defun edb-delete-breakpoint (mod line)
  "Updates all internal structures in all buffers."
  (edb-del-breakpoints
   (lambda (bp) (and (eq (bp-mod bp) mod)
		     (eq (bp-line bp) line)))
   (lambda (bbp) (and (eq (bbp-mod bbp) mod)
		      (eq (bbp-line bbp) line)))
   mod))

(defun edb-create-buffer-breakpoints (mod)
  "Creates buffer breakpoints in the current buffer."
  (when edb-buffer-breakpoints
    ;; remove old/stale breakpoints
    (edb-delete-buffer-breakpoints))
  (setq edb-buffer-breakpoints (edb-mk-bbps mod)))

(defun edb-delete-buffer-breakpoints ()
  "Deletes all buffer breakpoints in the current buffer."
  (setq edb-buffer-breakpoints 
	(edb-del-bbps edb-buffer-breakpoints (lambda (bbp) t))))

(defun edb-del-breakpoints (bp-f bbp-f &optional mod)
  "Updates all internal structures in all buffers."
  (setq edb-breakpoints (erl-remove-if bp-f edb-breakpoints))
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (if (and erlang-extended-mode
		(or (not mod)
		    (eq (edb-source-file-module-name) mod)))
	   (setq edb-buffer-breakpoints
		 (edb-del-bbps edb-buffer-breakpoints bbp-f)))))
   (buffer-list)))

(defun edb-synch-breakpoints (node module)
  "Synchronizes the breakpoints in the current buffer to erlang.
I.e. deletes all old breakpoints, and re-applies them at the current line."
  (interactive (list (erl-target-node)
		     (edb-module)))
  (when (edb-ensure-monitoring node)
    (let ((id (lambda (r) r)))
      (mapc (lambda (new-bbp)
	      (let ((bbp (car new-bbp))
		    (new-line (cdr new-bbp)))
		(erl-rpc id nil node 'distel 'break_delete
			 (list (bbp-mod bbp) (bbp-line bbp)))
		(erl-rpc id nil node 'distel 'break_add
			 (list module new-line))))
	    (edb-new-bbps))
      (setq edb-buffer-breakpoints-stale nil))))

(defun edb-make-breakpoints-stale (begin end length)
  "Make breakpoints in the current buffer stale.
Has no effect if the buffer's module is not interpreted, or the
breakpoints are already marked as stale."
  (when (and (not edb-attach-buffer)
	     (not edb-buffer-breakpoints-stale)
	     edb-module-interpreted)
    (mapc (lambda (bbp)
	    (let ((ov (bbp-ov bbp)))
	      (overlay-put ov 'face 'edb-breakpoint-stale-face)))
	  edb-buffer-breakpoints)
    (setq edb-buffer-breakpoints-stale t)))

(defun edb-save-breakpoints (node)
  (let ((modules '()))
    (setq edb-saved-breakpoints '())
    (mapc
     (lambda (buf)
       (with-current-buffer buf
	 (if erlang-extended-mode
	     (let ((cur-mod (edb-source-file-module-name)))
	       (unless (member cur-mod modules)
		 (let ((new-lines (mapcar (lambda (new-bbp) (cdr new-bbp))
					  (edb-new-bbps))))
		   (push cur-mod modules)
		   (push (list cur-mod new-lines) edb-saved-breakpoints)))))))
     (buffer-list))
    (mapc (lambda (bp)
	    (unless (member (bp-mod bp) modules)
	      (push (list (bp-mod bp) (list (bp-line bp)))
		    edb-saved-breakpoints)))
	  edb-breakpoints)))

(defun edb-restore-breakpoints (node cont)
  (erl-send-rpc node 'distel 'break_restore (list edb-saved-breakpoints))
  (erl-receive (cont)
      ((['rex 'ok]
	(funcall cont))
       (['rex ['badrpc reason]]
	(message "Failed to restore breakpoints: %S" reason)))))

(defun edb-new-bbps ()
  (mapcar (lambda (bbp)
	    (let* ((new-pos (overlay-start (bbp-ov bbp)))
		   (new-line (+ (count-lines (point-min) new-pos) 1)))
	      (cons bbp new-line)))
	  edb-buffer-breakpoints))

(defun edb-mk-bbps (mod)
  (zf
   (lambda (bp)
     (let ((bmod (bp-mod bp))
	   (line (bp-line bp)))
       (if (eq bmod mod)
	   (let ((ov (edb-make-breakpoint-overlay line)))
	     (make-bbp bmod line ov))
	 nil)))
   edb-breakpoints))

(defun edb-del-bbps (list pred)
  (zf
   (lambda (bbp)
     (cond ((funcall pred bbp)
	    (delete-overlay (bbp-ov bbp))
	    nil)
	   (t bbp)))
   list))

(defun edb-make-breakpoint-overlay (line)
  "Creats an overlay at line"
  (save-excursion
    (goto-line line)
    (let ((ov (make-overlay (line-beginning-position)
			    (line-beginning-position 2)
			    (current-buffer)
			    t
			    nil)))
      (overlay-put ov 'edb-breakpoint t)
      (if edb-buffer-breakpoints-stale
	  (overlay-put ov 'face 'edb-breakpoint-stale-face)
	(overlay-put ov 'face 'edb-breakpoint-face))
      ov)))
    
(defun zf (f l)
  (let ((res nil))
    (dolist (x l)
      (let ((r (funcall f x)))
	(if r (push r res))))
    res))

(defun edb-first (pred list)
  "Return the first element of LIST that satisfies PRED."
  (loop for x in list
	when (funcall pred x) return x))

(provide 'edb)
