;;; erl-service.el --- High-level calls to Erlang services.

;;;; Frontmatter
;;
;; This module implements Emacs commands - i.e. M-x'able key-bind'able
;; sort - for doing stuff with Erlang nodes.
;;
;; The general implementation strategy is to make RPCs to the "distel"
;; erlang module, which does most of the work for us.

(require 'erlang)
(eval-when-compile (require 'cl))
(require 'erl)

;;;; Base framework

;;;;; Target node

(defvar erl-nodename-cache nil
  "The name of the node most recently contacted, for reuse in future
commands. Using C-u to bypasses the cache.")

(defvar erl-nodename-history nil
  "The historical list of node names that have been selected.")

(defun erl-target-node ()
  "Return the name of the default target node for commands.
Force node selection if no such node has been choosen yet, or when
invoked with a prefix argument." 
  (or (and (not current-prefix-arg) erl-nodename-cache)
      (erl-choose-nodename)))

(defun erl-set-cookie ()
  "Prompt the user for the cookie."
  (interactive)
  (let* ((cookie (read-string "Cookie: ")))
    (if (string= cookie "")
        (setq derl-cookie nil)
      (setq derl-cookie cookie))))

(defun erl-get-cookie ()
  "Print the cookie."
  (interactive)
  (message "Cookie: %s" derl-cookie))

(defun erl-choose-nodename ()
  "Prompt the user for the nodename to connect to in future."
  (interactive)
  (let* ((nodename-string (if erl-nodename-cache
			      (symbol-name erl-nodename-cache)
			    nil))
	 (name-string (read-string (if nodename-string
				       (format "Node (default %s): "
					       nodename-string)
				     "Node: ")
				   nil
				   'erl-nodename-history
				   nodename-string))
         (name (intern (if (string-match "@" name-string)
                           name-string
			 (concat name-string
				 "@" (erl-determine-hostname))))))
    (when (string= name-string "")
      (error "No node name given"))
    (setq erl-nodename-cache name)
    (setq distel-modeline-node name-string)
    (force-mode-line-update))
  erl-nodename-cache)

;;;;; Call MFA lookup

(defun erl-read-call-mfa ()
  "Read module, function, arity at point or from user.
Returns the result in a list: module and function as strings, arity as
integer."
  (interactive) ; for testing
  (let* ((mfa-at-point (erl-mfa-at-point))
         (mfa (if (or (null mfa-at-point)
                      current-prefix-arg
                      distel-tags-compliant)
                  (erl-parse-mfa 
		   (read-string 
		    "Function reference: "
		    (if current-prefix-arg nil (erl-format-mfa mfa-at-point))))
                mfa-at-point)))
    mfa))

(defun erl-format-mfa (mfa)
  "Format (MOD FUN ARITY) as MOD:FUN/ARITY.
If MFA is nil then return nil.
If only MOD is nil then return FUN/ARITY."
  (if mfa
      (destructuring-bind (m f a) mfa
        (if m (format "%s:%s/%S" m f a) (format "%s/%S" f a)))))

(defun erl-parse-mfa (string &optional default-module)
  "Parse MFA from a string using `erl-mfa-at-point'."
  (when (null default-module) (setq default-module (erl-buffer-module-name)))
  (with-temp-buffer
    (with-syntax-table erlang-mode-syntax-table
      (insert string)
      (goto-char (point-min))
      (erl-mfa-at-point default-module))))

(defun erl-buffer-module-name ()
  "Return the current buffer's module name, or nil."
  (erlang-get-module))

(defun erl-mfa-at-point (&optional default-module)
  "Return the module, function, arity of the function reference at point.
If not module-qualified then use DEFAULT-MODULE."
  (when (null default-module) (setq default-module (erl-buffer-module-name)))
  (save-excursion
    (erl-goto-end-of-call-name)
    (let ((arity (erl-arity-at-point))
	  (mf (erlang-get-function-under-point)))
      (if (null mf)
	  nil
        (destructuring-bind (module function) mf
          (list (or module default-module) function arity))))))

;;; FIXME: Merge with erlang.el!
(defun erl-arity-at-point ()
  "Get the number of arguments in a function reference.
Should be called with point directly before the opening ( or /."
  ;; Adapted from erlang-get-function-arity.
  (save-excursion
    (cond ((looking-at "/")
	   ;; form is /<n>, like the /2 in foo:bar/2
	   (forward-char)
	   (let ((start (point)))
	     (if (re-search-forward "[0-9]+" nil t)
                 (ignore-errors (car (read-from-string (match-string 0)))))))
	  ((looking-at "[\n\r ]*(")
	   (goto-char (match-end 0))
	   (condition-case nil
	       (let ((res 0)
		     (cont t))
		 (while cont
		   (cond ((eobp)
			  (setq res nil)
			  (setq cont nil))
			 ((looking-at "\\s *)")
			  (setq cont nil))
			 ((looking-at "\\s *\\($\\|%\\)")
			  (forward-line 1))
			 ((looking-at "\\s *,")
			  (incf res)
			  (goto-char (match-end 0)))
			 (t
			  (when (zerop res)
			    (incf res))
			  (forward-sexp 1))))
		 res)
	     (error nil))))))

;;;; Backend code checking

(add-hook 'erl-nodeup-hook 'erl-check-backend)

(defun erl-check-backend (node _fsm)
  "Check if we have the 'distel' module available on `node'.
If not then try to send the module over as a binary and load it in."
  (unless distel-inhibit-backend-check
    (erl-spawn
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code ensure_loaded (distel)
			     ,(erl-group-leader)]])
      (erl-receive (node)
	  ((['rex ['error _]]
	    (&erl-load-backend node))
	   (_ t))))))

(defvar distel-ebin-directory
  (file-truename
   (concat (file-name-directory 
            (or (locate-library "distel") load-file-name)) "ebin"))
   "Directory where beam files are located.")

(defun &erl-load-backend (node)
  (let ((modules '()))
    (dolist (file (directory-files distel-ebin-directory))
      (when (string-match "^\\(.*\\)\\.beam$" file)
	(let ((module (intern (match-string 1 file)))
	      (filename (concat distel-ebin-directory "/" file)))
	  (push (list module filename) modules))))
    (if (null modules)
	(erl-warn-backend-problem "don't have beam files")
      (&erl-load-backend-modules node modules))))

(defun &erl-load-backend-modules (node modules)
  (message "loading = %S" (car modules))
  (if (null modules)
      (message "(Successfully uploaded backend modules into node)")
    (let* ((module (caar modules))
	   (filename (cadar modules))
	   (content (erl-file-to-string filename))
	   (binary (erl-binary content)))
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code load_binary ,(list module filename binary)
			     ,(erl-group-leader)]])
      (erl-receive (node modules)
	  ((['rex ['error reason]]
	    (erl-warn-backend-problem reason))
	   (['rex _]
	    (&erl-load-backend-modules node (rest modules))))))))

(defun erl-warn-backend-problem (reason)
  (with-current-buffer (get-buffer-create "*Distel Warning*")
    (erase-buffer)
    (insert (format "\
Distel Warning: node `%s' can't seem to load the `distel' module.

This means that most Distel commands won't function correctly, because
the supporting library is not available. Please check your node's code
path, and make sure that Distel's \"ebin\" directory is included.

The most likely cause of this problem is either:

  a) Your ~/.erlang file doesn't add Distel to your load path (the
     Distel \"make config_install\" target can set this up for you.)

  b) Your system's boot script doesn't consult your ~/.erlang file to
     read your code path setting.

To disable this warning in future, set `distel-inhibit-backend-check' to t.

"
		    node))
    (display-buffer (current-buffer))
    (error "Unable to load or upload distel backend: %S" reason)))

(defun erl-file-to-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;;;; RPC

(defun erl-rpc (k kargs node m f a)
  "Call {M,F,A} on NODE and deliver the result to the function K.
The first argument to K is the result from the RPC, followed by the
elements of KARGS."
  (erl-spawn
    (erl-send-rpc node m f a)
    (erl-rpc-receive k kargs)))

(defun erl-send-rpc (node mod fun args)
  "Send an RPC request on NODE to apply(MOD, FUN, ARGS).
The reply will be sent back as an asynchronous message of the form:
    [rex Result]
On an error, Result will be [badrpc Reason]."
  (let ((m 'distel)
	(f 'rpc_entry)
	(a (list mod fun args)))
    (erl-send (tuple 'rex node)
	      ;; {Who, {call, M, F, A, GroupLeader}}
	      (tuple erl-self (tuple 'call m f a (erl-group-leader))))))

(defun erl-rpc-receive (k kargs)
  "Receive the reply to an `erl-rpc'."
  (erl-receive (k kargs)
      ((['rex reply] (apply k (cons reply kargs))))))

(defun erpc (node m f a)
  "Make an RPC to an erlang node."
  (interactive (list (erl-target-node)
		     (intern (read-string "Module: "))
		     (intern (read-string "Function: "))
		     (eval-minibuffer "Args: ")))
  (erl-rpc (lambda (result) (message "RPC result: %S" result))
	   nil
	   node
	   m f a))

(defun erl-ping (node)
  "Ping the NODE, uploading distel code as a side effect."
  (interactive (list (erl-target-node)))
  (erl-spawn
    (erl-send-rpc node 'erlang 'node nil)
    (erl-receive (node)
	((['rex response]
          (if (equal node response)
              (message "Successfully communicated with remote node %S"
                       node)
            (message "Failed to communicate with node %S: %S"
                     node response)))))))

;;;; Process list

(defun erl-process-list (node)
  "Show a list of all processes running on NODE.
The listing is requested asynchronously, and popped up in a buffer
when ready."
  (interactive (list (erl-target-node)))
  (erl-rpc #'erl-show-process-list (list node)
	   node 'distel 'process_list '()))

(defun erl-show-process-list (reply node)
  (with-current-buffer (get-buffer-create (format "*plist %S*" node))
    (process-list-mode)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (let ((header (tuple-elt reply 1))
	    (infos (tuple-elt reply 2)))
	(put-text-property 0 (length header) 'face 'bold header)
	(insert header)
	(mapc #'erl-insert-process-info infos))
      (goto-char (point-min))
      (next-line 1))
    (select-window (display-buffer (current-buffer)))))

(defun erl-insert-process-info (info)
  "Insert INFO into the buffer.
INFO is [PID SUMMARY-STRING]."
  (let ((pid (tuple-elt info 1))
	(text (tuple-elt info 2)))
    (put-text-property 0 (length text) 'erl-pid pid text)
    (insert text)))

;; Process list major mode

(defvar erl-viewed-pid nil
  "PID being viewed.")
(make-variable-buffer-local 'erl-viewed-pid)
(defvar erl-old-window-configuration nil
  "Window configuration to return to when viewing is finished.")
(make-variable-buffer-local 'erl-old-window-configuration)

(defun erl-quit-viewer (&optional bury)
  "Quit the current view and restore the old window config.
When BURY is non-nil, buries the buffer instead of killing it."
  (interactive)
  (let ((cfg erl-old-window-configuration))
    (if bury
	(bury-buffer)
      (kill-this-buffer))
    (set-window-configuration cfg)))

(defun erl-bury-viewer ()
  "Bury the current view and restore the old window config."
  (interactive)
  (erl-quit-viewer t))

(defvar process-list-mode-map nil
  "Keymap for Process List mode.")

(when (null process-list-mode-map)
  (setq process-list-mode-map (make-sparse-keymap))
  (define-key process-list-mode-map [?u] 'erl-process-list)
  (define-key process-list-mode-map [?q] 'erl-quit-viewer)
  (define-key process-list-mode-map [?k] 'erl-pman-kill-process)
  (define-key process-list-mode-map [return] 'erl-show-process-info)
  (define-key process-list-mode-map [(control m)] 'erl-show-process-info)
  (define-key process-list-mode-map [?i] 'erl-show-process-info-item)
  (define-key process-list-mode-map [?b] 'erl-show-process-backtrace)
  (define-key process-list-mode-map [?m] 'erl-show-process-messages))

(defun process-list-mode ()
  "Major mode for viewing Erlang process listings.

Available commands:

\\[erl-quit-viewer]	- Quit the process listing viewer, restoring old window config.
\\[erl-process-list]	- Update the process list.
\\[erl-pman-kill-process]	- Send an EXIT signal with reason 'kill' to process at point.
\\[erl-show-process-info]	- Show process_info for process at point.
\\[erl-show-process-info-item]	- Show a piece of process_info for process at point.
\\[erl-show-process-backtrace]	- Show a backtrace for the process at point.
\\[erl-show-process-messages]	- Show the message queue for the process at point."
  (interactive)
  (kill-all-local-variables)
  (use-local-map process-list-mode-map)
  (setq mode-name "Process List")
  (setq major-mode 'process-list-mode)
  (setq erl-old-window-configuration (current-window-configuration))
  (run-hooks 'process-list-mode-hook))

(defun erl-show-process-info ()
  "Show information about process at point in a summary buffer."
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if (null pid)
	(message "No process at point.")
      (erl-view-process pid))))

(defun erl-show-process-info-item (item)
  "Show a piece of information about process at point."
  (interactive (list (intern (read-string "Item: "))))
  (let ((pid (get-text-property (point) 'erl-pid)))
    (cond ((null pid)
	   (message "No process at point."))
	  ((string= "" item)
	   (erl-show-process-info))
	  (t
	   (erl-spawn
	     (erl-send-rpc (erl-pid-node pid)
			   'distel 'process_info_item (list pid item))
	     (erl-receive (item pid)
		 ((['rex ['ok string]]
		   (display-message-or-view string "*pinfo item*"))
		  (other
		   (message "Error from erlang side of process_info:\n  %S"
			    other)))))))))

(defun display-message-or-view (msg bufname &optional select)
  "Like `display-buffer-or-message', but with `view-buffer-other-window'.
That is, if a buffer pops up it will be in view mode, and pressing q
will get rid of it.

Only uses the echo area for single-line messages - or more accurately,
messages without embedded newlines. They may still need to wrap or
truncate to fit on the screen."
  (if (string-match "\n.*[^\\s-]" msg)
      ;; Contains a newline with actual text after it, so display as a
      ;; buffer
      (with-current-buffer (get-buffer-create bufname)
	(setq buffer-read-only t)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert msg)
	  (goto-char (point-min))
	  (let ((win (display-buffer (current-buffer))))
	    (when select (select-window win)))))
    ;; Print only the part before the newline (if there is
    ;; one). Newlines in messages are displayed as "^J" in emacs20,
    ;; which is ugly
    (string-match "[^\r\n]*" msg)
    (message (match-string 0 msg))))

(defun erl-show-process-messages ()
  (interactive)
  (erl-show-process-info-item 'messages))
(defun erl-show-process-backtrace ()
  (interactive)
  (erl-show-process-info-item 'backtrace))

(defun erl-pman-kill-process ()
  "Kill process at point in a summary buffer."
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if (null pid)
	(message "No process at point.")
      (message "Sent EXIT (kill) signal ")
      (erl-exit 'kill pid))))

;;;; Single process viewer

(defun erl-view-process (pid)
  (let ((buf (get-buffer (erl-process-view-buffer-name pid))))
    (if buf
	(select-window (display-buffer buf))
      (erl-spawn
	(process-view-mode)
	(setq erl-old-window-configuration (current-window-configuration))
	(setq erl-viewed-pid pid)
	(erl-send-rpc (erl-pid-node pid)
		      'distel 'process_summary_and_trace (list erl-self pid))
	(erl-receive (pid)
	    ((['rex ['error reason]]
	      (message "%s" reason))
	     (['rex ['badrpc reason]]
	      (message "Bad RPC: %s" reason))
	     (['rex summary]
	      (rename-buffer (erl-process-view-buffer-name pid))
	      (erase-buffer)
	      (insert summary)
	      (setq buffer-read-only t)
	      (goto-char (point-min))
	      (select-window (display-buffer (current-buffer)))
	      (&erl-process-trace-loop))
	     (other
	      (message "Unexpected reply: %S" other))))))))

(defun erl-process-view-buffer-name (pid)
  (format "*pinfo %S on %S*"
	  (erl-pid-id pid) (erl-pid-node pid)))

(defvar process-view-mode-map nil
  "Keymap for Process View mode.")

(unless process-view-mode-map
  (setq process-view-mode-map (make-sparse-keymap))
  (define-key process-view-mode-map [?q] 'erl-quit-viewer))

(defun process-view-mode ()
  "Major mode for viewing an Erlang process."
  (interactive)
  (kill-all-local-variables)
  (use-local-map process-view-mode-map)
  (setq mode-name "Process View")
  (setq major-mode 'process-view)
  (run-hooks 'process-view-mode-hook))

(defun &erl-process-trace-loop ()
  (erl-receive ()
      ((['trace_msg text]
	(goto-char (point-max))
	(let ((buffer-read-only nil))
	  (insert text))))
    (&erl-process-trace-loop)))

;;;; fprof

(defvar fprof-entries nil
  "Alist of Tag -> Properties.
Tag is a symbol like foo:bar/2
Properties is an alist of:
  'text     -> String
  'callers  -> list of Tag
  'callees  -> list of Tag
  'beamfile -> String | undefined")

(defvar fprof-header nil
  "Header listing for fprof text entries.
This is received from the Erlang module.")

(defun fprof (node expr)
  "Profile a function and summarise the results."
  (interactive (list (erl-target-node)
		     (erl-add-terminator (read-string "Expression: "))))
  (erl-spawn
    (erl-send-rpc node 'distel 'fprof (list expr))
    (fprof-receive-analysis)))

(defun fprof-analyse (node filename)
  "View an existing profiler analysis from a file."
  (interactive (list (erl-target-node)
		     (read-string "Filename: ")))
  (erl-spawn
    (erl-send-rpc node 'distel 'fprof_analyse (list filename))
    (fprof-receive-analysis)))

(defun fprof-receive-analysis ()
  (message "Waiting for fprof reply...")
  (erl-receive ()
      ((['rex ['ok preamble header entries]]
	(message "Got fprof reply, drawing...")
	(fprof-display preamble header entries))
       (other (message "Unexpected reply: %S" other)))))


(defun fprof-display (preamble header entries)
  "Display profiler results in the *fprof* buffer."
  (setq fprof-entries '())
  (setq fprof-header header)
  (with-current-buffer (get-buffer-create "*fprof*")
    (use-local-map (make-sparse-keymap))
    (define-key (current-local-map) [return] 'fprof-show-detail)
    (define-key (current-local-map) [(control m)] 'fprof-show-detail)
    (define-key (current-local-map) [?f] 'fprof-find-source)
    (define-key (current-local-map) [?q] 'kill-this-buffer)
    (setq tab-width 10)
    (erase-buffer)
    (insert preamble)
    (insert fprof-header)
    (mapc #'fprof-add-entry entries)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))

(defun fprof-add-entry (entry)
  "Add a profiled function entry."
  (mcase entry
    (['process title info-list]
     (insert "\n")
     (insert title "\n")
     (dolist (info info-list)
       (insert "  " info "\n"))
     (insert "\n"))
    (['tracepoint tag mfa text callers callees beamfile]
     (push `(,tag . ((text 	. ,text)
		     (mfa 	. ,mfa)
		     (callers 	. ,callers)
		     (callees 	. ,callees)
		     (beamfile 	. ,beamfile)))
	   fprof-entries)
     (fprof-insert text tag))))

(defun fprof-insert (text tag)
  (put-text-property 0 (length text) 'fprof-tag tag text)
  (insert text))

(defun fprof-show-detail ()
  "Show more detail about the profiled function at point.
The extra detail is a list of callers and callees, showing how much
time the function spent while called from each caller, and how much
time it spent in subfunctions."
  (interactive)
  (let* ((tag     (fprof-tag-at-point))
	 (props   (cdr (assq tag fprof-entries)))
	 (text    (cdr (assq 'text    props)))
	 (callers (cdr (assq 'callers props)))
	 (callees (cdr (assq 'callees props)))
	 (buf     (get-buffer-create "*fprof detail*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert fprof-header)
      (insert text "\n")
      (insert "Callers:\n")
      (mapc #'fprof-insert-by-tag callers)
      (insert "\n")
      (insert "Callees:\n")
      (mapc #'fprof-insert-by-tag callees)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun fprof-insert-by-tag (tag)
  (let ((text (fprof-lookup tag 'text)))
    (put-text-property 0 (length text) 'fprof-tag tag text)
    (insert text)))

(defun fprof-find-source ()
  (interactive)
  (let ((beamfile (fprof-lookup (fprof-tag-at-point) 'beamfile)))
    (if (eq beamfile 'undefined)
	(message "Don't know where that's implemented.")
      (let* ((src (fprof-sourcefile beamfile))
	     (mfa (fprof-lookup (fprof-tag-at-point) 'mfa))
	     (arity (caddr mfa))
	     (orig-window (selected-window)))
	(when src
	  (with-current-buffer (find-file-other-window src)
	    (goto-char (point-min))
	    ;; Find the right function/arity
	    (let (found)
	      (while (and (not found)
			  (re-search-forward (concat "^" (symbol-name (cadr mfa)))))
		(beginning-of-line)
		(if (eq (erlang-get-function-arity) arity)
		    (setq found t)
		  (forward-line)))
	      (if found
		  (recenter 5))))
	  (select-window orig-window))))))

(defun fprof-tag-at-point ()
  (or (get-text-property (point) 'fprof-tag)
      (error "No function tag at point.")))

(defun fprof-lookup (tag property)
  (cdr (assq property (cdr (assq tag fprof-entries)))))

(defun fprof-sourcefile (beamfile)
  (let ((string beamfile))
    (when (string-match "ebin" string)
      (setq string (replace-match "src" t t string)))
    (if (null (string-match "beam" string))
	nil
      (setq string (replace-match "erl" t t string))
      (if (file-exists-p string)
	  string
	nil))))

;;

(defun erl-eval-expression (node string)
  (interactive (list (erl-target-node)
		     (erl-add-terminator (read-from-minibuffer
					  "Expression: "
					  (if (equal mark-active nil)
					      ""
					    (copy-region-as-kill (mark) (point))
					    (current-kill 0))))))
  (erl-spawn
    (erl-send-rpc node
		  'distel
		  'eval_expression
		  (list string))
    (erl-receive ()
	((['rex ['ok string]]
	  (display-message-or-view string "*Expression Result*"))
	 (['rex ['error reason]]
	  (message "Error: %S" reason))
	 (other
	  (message "Unexpected: %S" other))))))

(defun erl-add-terminator (s)
  "Make sure S terminates with a dot (.)"
  (if (string-match "\\.\\s *$" s)
      s
    (concat s ".")))

(defun erl-reload-modules (node)
  "reload all out-of-date modules"
  (interactive (list (erl-target-node)))
  (erl-rpc (lambda (result) (message "load: %s" result)) nil 
           node 'distel 'reload_modules ()))


(defvar erl-reload-dwim nil
  "Do What I Mean when reloading beam files. If erl-reload-dwim is non-nil, 
and the module cannot be found in the load path, we attempt to find the correct
directory, add it to the load path and retry the load.
We also don't prompt for the module name.")

(defun erl-reload-module (node module)
  "Reload a module."
  (interactive (list (erl-target-node)
		     (if erl-reload-dwim 
			 (erlang-get-module)
		       (let* ((module (erlang-get-module))
			      (prompt (if module
					  (format "Module (default %s): " module)
					"Module: ")))
			 (intern (read-string prompt nil nil module))))))
  (if (and (equal node edb-monitor-node)
	   (assq module edb-interpreted-modules))
      (erl-reinterpret-module node module)
    ;;    (erl-eval-expression node (format "c:l('%s')." module))))
    (erl-do-reload node module)))

(defun erl-do-reload (node module)
  (let ((fname (if erl-reload-dwim (buffer-file-name) nil)))
    (erl-rpc (lambda (result) (message "load: %s" result)) nil 
	     node 'distel 'reload_module (list module fname))))

(defun erl-reinterpret-module (node module)
  ;; int:i(SourcePath).
  (erl-send-rpc node
		'int 'i (list (cadr (assq module edb-interpreted-modules)))))

;;;; Definition finding

(defvar erl-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

(defun erl-find-source-under-point ()
  "Goto the source code that defines the function being called at point.
For remote calls, contacts an Erlang node to determine which file to
look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Same directory as the beam file
    Again with /ebin/ replaced with /src/
    Again with /ebin/ replaced with /erl/
    Directory where source file was originally compiled

  Otherwise, report that the file can't be found.

When `distel-tags-compliant' is non-nil, or a numeric prefix argument
is given, the user is prompted for the function to lookup (with a
default.)"
  (interactive)
  (apply #'erl-find-source
         (or (erl-read-call-mfa) (error "No call at point."))))

(defun erl-find-source-unwind ()
  "Unwind back from uses of `erl-find-source-under-point'."
  (interactive)
  (unless (ring-empty-p erl-find-history-ring)
    (let* ((marker (ring-remove erl-find-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
	;; If this buffer was deleted, recurse to try the next one
	(erl-find-source-unwind)))))

(defun erl-goto-end-of-call-name ()
  "Go to the end of the function or module:function at point."
  ;; We basically just want to do forward-sexp iff we're not already
  ;; in the right place
  (unless (or (member (char-before) '(?  ?\t ?\n))
              (and (not (eobp))
                   (member (char-syntax (char-after (point))) '(?w ?_))))
    (backward-sexp))
  (forward-sexp)
  ;; Special case handling: On some emacs installations (Tobbe's
  ;; machine), the (forward-sexp) won't skip over the : in a remote
  ;; function call. This is a workaround for that. The issue seems to
  ;; be that the emacs considers : to be punctuation (syntax class
  ;; '.'), whereas my emacs calls it a symbol separator (syntax class
  ;; '_'). FIXME.
  (when (eq (char-after) ?:)
    (forward-sexp)))

(defun erl-find-module ()
  (interactive)
  (erl-find-source (read-string "module: ")))
 
(defun erl-find-source (module &optional function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (ring-insert-at-beginning erl-find-history-ring
			    (copy-marker (point-marker)))
  (if (equal module (erlang-get-module))
      (when function
	(erl-search-function function arity))
    (let ((node (or erl-nodename-cache (erl-target-node))))
      (erl-spawn
	(erl-send-rpc node 'distel 'find_source (list (intern module)))
	(erl-receive (function arity)
	    ((['rex ['ok path]]
	      (find-file path)
	      (when function
		(erl-search-function function arity)))
	     (['rex ['error reason]]
	      ;; Remove the history marker, since we didn't go anywhere
	      (ring-remove erl-find-history-ring)
	      (message "Error: %s" reason))))))))

(defun erl-find-doc-under-point ()
  "Find the html documentation for the (possibly incomplete) OTP 
function under point"
  (interactive)
  (if (require 'w3m nil t)
      (erl-do-find-doc 'link 'point)
    (erl-find-sig-under-point)))

(defun erl-find-doc ()
  (interactive)
  (if (require 'w3m nil t)
      (erl-do-find-doc 'link nil)
    (erl-find-sig)))

(defun erl-find-sig-under-point ()
  "Find the signatures for the (possibly incomplete) OTP function under point"
  (interactive)
  (erl-do-find-doc 'sig 'point))

(defun erl-find-sig ()
  (interactive)
  (erl-do-find-doc 'sig nil))

(defun erl-do-find-doc (what how &optional module function ari)
  "Find the documentation for an OTP mfa. 
if WHAT is 'link, tries to get a link to the html docs, and open 
it in a w3m buffer. if WHAT is nil, prints the function signature 
in the mini-buffer.
If HOW is 'point, tries to find the mfa at point; if HOW is nil, 
prompts for an mfa."
  (destructuring-bind 
      (mod fun ari)
      (or (if (null how)
	      (erl-parse-mfa (read-string "Function reference: ") "-")
	    (erl-mfa-at-point))
	  (error "No call at point."))
    (let ((node (or erl-nodename-cache (erl-target-node)))
	  (arity (or ari -1))
	  (module (if (equal mod "-") fun mod))
	  (function (if (equal mod "-") nil fun)))
      (erl-spawn
	(erl-send-rpc node 'otp_doc 'distel (list what module function arity))
	(erl-receive ()
	    ((['rex nil]
	      (message "No doc found."))
	     (['rex 'no_html]
	      (message "no html docs installed"))
	     (['rex ['mfas string]]
	      (message "candidates: %s" string))
	     (['rex ['sig string]]
	      (message "%s" string))
	     (['rex ['link link]]
	      (w3m-browse-url link))
	     (['rex [reaso reason]]
	      (message "Error: %s %s" reaso reason))))))))

(defun erl-search-function (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
	(str (concat "\n" function "("))
	(searching t))
    (goto-char (point-min))
    (while searching
      (cond ((search-forward str nil t)
	     (backward-char)
	     (when (or (null arity)
		       (eq (erl-arity-at-point) arity))
	       (beginning-of-line)
	       (setq searching nil)))
	    (t
	     (setq searching nil)
	     (goto-char origin)
	     (if arity
		 (message "Couldn't find function %S/%S" function arity)
	       (message "Couldn't find function %S" function)))))))

(defun erl-read-symbol-or-nil (prompt)
  "Read a symbol, or NIL on empty input."
  (let ((s (read-string prompt)))
    (if (string= s "")
	nil
      (intern s))))

;;;; Completion

(defun erl-complete (node)
  "Complete the module or remote function name at point."
  (interactive (list (erl-target-node)))
  (let ((win (get-buffer-window "*Completions*" 0)))
    (if win (with-selected-window win (bury-buffer))))
  (let ((end (point))
	(beg (ignore-errors 
	       (save-excursion (backward-sexp 1)
			       ;; FIXME: see erl-goto-end-of-call-name
			       (when (eql (char-before) ?:)
				 (backward-sexp 1))
			       (point)))))
    (when beg
      (let* ((str (buffer-substring-no-properties beg end))
	     (buf (current-buffer))
	     (continuing (equal last-command (cons 'erl-complete str))))
	(setq this-command (cons 'erl-complete str))
	(if (string-match "^\\(.*\\):\\(.*\\)$" str)
	    ;; completing function in module:function
	    (let ((mod (intern (match-string 1 str)))
		  (pref (match-string 2 str))
		  (beg (+ beg (match-beginning 2))))
	      (erl-spawn
		(erl-send-rpc node 'distel 'functions (list mod pref))
		(&erl-receive-completions "function" beg end pref buf
					  continuing
					  #'erl-complete-sole-function)))
	  ;; completing just a module
	  (erl-spawn
	    (erl-send-rpc node 'distel 'modules (list str))
	    (&erl-receive-completions "module" beg end str buf continuing
				      #'erl-complete-sole-module)))))))

(defun &erl-receive-completions (what beg end prefix buf continuing sole)
  (let ((state (erl-async-state buf)))
    (erl-receive (what state beg end prefix buf continuing sole)
	((['rex ['ok completions]]
	  (when (equal state (erl-async-state buf))
	    (with-current-buffer buf
	      (erl-complete-thing what continuing beg end prefix
				  completions sole))))
	 (['rex ['error reason]]
	  (message "Error: %s" reason))
	 (other
	  (message "Unexpected reply: %S" other))))))

(defun erl-async-state (buffer)
  "Return an opaque state for BUFFER.
This is for making asynchronous operations: if the state when we get a
reply is not equal to the state when we started, then the user has
done something - modified the buffer, or moved the point - so we may
want to cancel the operation."
  (with-current-buffer buffer
    (cons (buffer-modified-tick)
	  (point))))

(defun erl-complete-thing (what scrollable beg end pattern completions sole)
  "Complete a string in the buffer.
WHAT is a string that says what we're completing.
SCROLLABLE is a flag saying whether this is a repeated command that
may scroll the completion list.
BEG and END are the buffer positions around what we're completing.
PATTERN is the string to complete from.
COMPLETIONS is a list of potential completions (strings.)
SOLE is a function which is called when a single completion is selected."
  ;; This function, and `erl-maybe-scroll-completions', are basically
  ;; cut and paste programming from `lisp-complete-symbol'. The fancy
  ;; Emacs completion packages (hippie and pcomplete) looked too
  ;; scary.
  (or (and scrollable (erl-maybe-scroll-completions))
      (let* ((completions (erl-make-completion-alist completions))
	     (completion (try-completion pattern completions)))
	(cond ((eq completion t)
	       (message "Sole completion")
	       (apply sole '()))
	      ((null completion))
;	       (message "Can't find completion for %s \"%s\"" what pattern)
;	       (ding))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion)
	       (if (eq t (try-completion completion completions))
		   (apply sole '())))
	      (t
	       (message "Making completion list...")
	       (let ((list (all-completions pattern completions)))
		 (setq list (sort list 'string<))
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done"))))))

(defun erl-complete-sole-module ()
  (insert ":"))

(defun erl-complete-sole-function ()
  (let ((call (erlang-get-function-under-point)))
    (insert "(")
    (erl-print-arglist call (erl-target-node))))


(defun erl-make-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun erl-maybe-scroll-completions ()
  "Scroll the completions buffer if it is visible.
Returns non-nil iff the window was scrolled."
  (let ((window (get-buffer-window "*Completions*")))
    (when (and window (window-live-p window) (window-buffer window)
	       (buffer-name (window-buffer window)))
      ;; If this command was repeated, and
      ;; there's a fresh completion window with a live buffer,
      ;; and this command is repeated, scroll that window.
      (with-current-buffer (window-buffer window)
	(if (pos-visible-in-window-p (point-max) window)
	    (set-window-start window (point-min))
	  (save-selected-window
	    (select-window window)
	    (scroll-up))))
      t)))

;;;; Refactoring

(defun erl-refactor-subfunction (node name start end)
  "Refactor the expression(s) in the region as a function.

The expressions are replaced with a call to the new function, and the
function itself is placed on the kill ring for manual placement. The
new function's argument list includes all variables that become free
during refactoring - that is, the local variables needed from the
original function.

New bindings created by the refactored expressions are *not* exported
back to the original function. Thus this is not a \"pure\"
refactoring.

This command requires Erlang syntax_tools package to be available in
the node, version 1.2 (or perhaps later.)"
  (interactive (list (erl-target-node)
		     (read-string "Function name: ")
		     (region-beginning)
		     (region-end)))
  ;; Skip forward over whitespace
  (setq start (save-excursion
                (goto-char start)
                (skip-chars-forward " \t\r\n")
                (point)))
  ;; Skip backwards over trailing syntax
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward ". ,;\r\n\t")
              (point)))
  (let ((buffer (current-buffer))
	(text   (erl-refactor-strip-macros
                 (buffer-substring-no-properties start end))))
    (erl-spawn
      (erl-send-rpc node 'distel 'free_vars (list text))
      (erl-receive (name start end buffer text)
	  ((['rex ['badrpc rsn]]
	    (message "Refactor failed: %S" rsn))
	   (['rex ['error rsn]]
	    (message "Refactor failed: %s" rsn))
	   (['rex ['ok free-vars]]
	    (with-current-buffer buffer
	      (let ((arglist
		     (concat "(" (mapconcat 'symbol-name free-vars ", ") ")"))
		    (body
		     (buffer-substring-no-properties start end)))
		;; rewrite the original as a call
		(delete-region start end)
                (goto-char start)
		(insert (format "%s%s" name arglist))
		(indent-according-to-mode)
		;; Now generate the function and stick it on the kill ring
		(kill-new (with-temp-buffer
			    (insert (format "%s%s ->\n%s.\n" name arglist body))
			    (erlang-mode)
			    (indent-region (point-min) (point-max) nil)
			    (buffer-string)))
		(message "Saved `%s' definition on kill ring." name)))))))))

(defun erl-refactor-strip-macros (text)
  "Removed all use of macros in TEXT.
We do this by making a bogus expansion of each macro, such that the
expanded code should probably still have the right set of free
variables."
  (with-temp-buffer
    (save-excursion (insert text))
    (while (re-search-forward "\\?[A-Za-z_]+" nil t)
      (replace-match "deadmacro" t))
    (buffer-string)))

;;;; fdoc interface

(defface erl-fdoc-name-face
    '((t (:bold t)))
  "Face for function names in `fdoc' results."
  :group 'distel)

(defun maybe-select-db-rebuild ()
  (and current-prefix-arg
       (equal (read-string "Rebuild DB (yes/no)? " "no") "yes")))

(defun erl-fdoc-apropos (node regexp rebuild-db)
  (interactive (list (erl-target-node)
		     (read-string "Regexp: ")
                     (maybe-select-db-rebuild)))
  (unless (string= regexp "")
    (erl-spawn
      (erl-send-rpc node 'distel 'apropos (list regexp
						(if rebuild-db 'true 'false)))
      (message "Sent request; waiting for results..")
      (erl-receive ()
	  ((['rex ['ok matches]]
	    (erl-show-fdoc-matches matches))
	   (['rex ['badrpc rsn]]
	    (message "fdoc RPC failed: %S" rsn))
	   (other
	    (message "fdoc unexpected result: %S" other)))))))

(defun erl-show-fdoc-matches (matches)
  "Show MATCHES from fdoc. Each match is [MOD FUNC ARITY DOC]."
  (if (null matches)
      (message "No matches.")
    (display-message-or-view
     (with-temp-buffer
       (dolist (match matches)
	 (mlet [mod func arity doc] match
	   (let ((entry (format "%s:%s/%s" mod func arity)))
	     (put-text-property 0 (length entry)
				'face 'erl-fdoc-name-face
				entry)
	     (insert entry ":\n"))
	   (let ((start (point)))
	     (insert doc)
	     (indent-rigidly start (point) 2)
	     (insert "\n"))))
       (buffer-string))
     "*Erlang fdoc results*")))

(defvar erl-module-function-arity-regexp
  ;; Nasty scary not-really-correct stuff.. now I know how perl guys feel
  (let* ((module-re   "[^:]*")
	 (fun-re      "[^/]*")
	 (arity-re    "[0-9]*")
	 (the-module  (format "\\(%s\\)" module-re))
	 (maybe-arity (format "\\(/\\(%s\\)\\)?" arity-re))
	 (maybe-fun-and-maybe-arity
	  (format "\\(:\\(%s\\)%s\\)?" fun-re maybe-arity)))
    (concat "^" the-module maybe-fun-and-maybe-arity "$"))
    "Regexp matching \"module[:function[/arity]]\".
The match positions are erl-mfa-regexp-{module,function,arity}-match.")

(defvar erl-mfa-regexp-module-match   1)
(defvar erl-mfa-regexp-function-match 3)
(defvar erl-mfa-regexp-arity-match    5)

(defun erl-fdoc-describe (node rebuild-db)
  (interactive (list (erl-target-node)
                     (maybe-select-db-rebuild)))
  (let* ((mfa (erl-read-call-mfa))
	 (defaultstr (if (null mfa)
			 nil
		       (concat (if (first mfa)  (format "%s:" (first mfa)) "")
			       (if (second mfa) (format "%s"  (second mfa)) "")
			       (if (third mfa)  (format "/%S" (third mfa))))))
	 (prompt (format "M[:F[/A]]: %s"
			 (if defaultstr
			     (format "(default %s) " defaultstr)
			   "")))
	 (mfastr (read-string prompt nil nil defaultstr)))
    (if (not (string-match erl-module-function-arity-regexp mfastr))
	(error "Bad input.")
      (let ((mod (match-string erl-mfa-regexp-module-match mfastr))
	    (fun (ignore-errors (match-string erl-mfa-regexp-function-match mfastr)))
	    (arity (ignore-errors (match-string erl-mfa-regexp-arity-match mfastr))))
	(if (string= mod "")
	    (error "Bad spec -- which module?")
	  (erl-spawn
	    (erl-send-rpc node 'distel 'describe
			  (list (intern mod)
				(if fun (intern fun) '_)
				(if arity (string-to-int arity) '_)
				(if rebuild-db 'true 'false)))
	    (message "Sent request; waiting for results..")
	    (erl-receive ()
		((['rex ['ok matches]]
		  (erl-show-fdoc-matches matches))
		 (['rex ['badrpc rsn]]
		  (message "fdoc RPC failed: %S" rsn))
		 (['rex ['error rsn]]
		  (message "fdoc failed: %S" rsn))
		 (other
		  (message "fdoc unexpected result: %S" other))))))))))

;;;; Argument lists

(defun erl-openparen ()
  "Insert a '(' character and show arglist information."
  (interactive)
  (erl-show-arglist)
  (insert "("))

(defun erl-show-arglist ()
  "Show arglist information."
  (interactive)
  (let ((call (erlang-get-function-under-point)))
    (erl-print-arglist call erl-nodename-cache)))

(defun erl-print-arglist (call node &optional ins-buffer)
  (when (and node (member node erl-nodes))
    ;; Don't print arglists when we're defining a function (when the
    ;; "call" is at the start of the line)
    (unless (save-excursion
	      (skip-chars-backward "a-zA-Z0-9_:'(")
	      (bolp))
      (let* ((call-mod (car call))
	     (mod (or call-mod (erlang-get-module)))
	     (fun (cadr call)))
	(when fun
	  (erl-spawn
	    (erl-send-rpc node 'distel 'get_arglists
			  (list mod fun))
	    (erl-receive (call-mod fun ins-buffer)
		((['rex 'error])
		 (['rex arglists]
		  (let ((argss (erl-format-arglists arglists)))
		    (if ins-buffer
			(with-current-buffer ins-buffer (insert argss))
		      (message "%s:%s%s"  call-mod fun argss))))))))))))

(defun erl-format-arglists (arglists)
  (setq arglists (sort* arglists '< :key 'length))
  (format "%s"
          (mapconcat 'identity
                     (mapcar (lambda (arglist)
                               (format "(%s)"
                                       (mapconcat 'identity arglist ", ")))
                             arglists)
                     " | ")))

;;;; Cross-reference

(defun erl-who-calls (node)
  (interactive (list (erl-target-node)))
  (apply #'erl-find-callers
         (or (erl-read-call-mfa) (error "No call at point."))))

(defun erl-find-callers (mod fun arity)
  (erl-spawn
    (erl-send-rpc (erl-target-node) 'distel 'who_calls
                  (list (intern mod) (intern fun) arity))
    (message "Request sent..")
    (erl-receive ()
        ((['rex ['error reason]]
          (message "Error: %s" reason))
         (['rex calls]
          (with-current-buffer (get-buffer-create "*Erlang Calls*")
	    (erl-who-calls-mode)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (dolist (call calls)
                (mlet [m f a line] call
		  (erl-propertize-insert (list 'module m
					       'function f
					       'arity a
					       'line line
					       'face 'bold)
					 (format "%s:%s/%S\n" m f a))))
	      ;; Remove the final newline to ensure all lines contain xref's
	      (backward-char 1)
	      (delete-char 1))
            (goto-char (point-min))
            (message "")
            (pop-to-buffer (current-buffer))))
         ))))

(define-derived-mode erl-who-calls-mode fundamental-mode
  "who-calls"
  "Distel Who-Calls Mode. Goto caller by pressing RET.

\\{erl-who-calls-mode-map}")

(define-key erl-who-calls-mode-map (kbd "RET") 'erl-goto-caller)

(defun erl-goto-caller ()
  "Goto the caller that is at point."
  (interactive)
  (let ((line (get-text-property (line-beginning-position) 'line))
	(module (get-text-property (line-beginning-position) 'module))
	(node (or erl-nodename-cache (erl-target-node))))
    (erl-spawn
      (erl-send-rpc node 'distel 'find_source (list (intern module)))
      (erl-receive (line)
	  ((['rex ['ok path]]
	    (find-file path)
	    (goto-line line))
	   (['rex ['error reason]]
	    (message "Error: %s" reason)))))))

(defmacro erl-propertize-insert (props &rest body)
  "Execute and insert BODY and add PROPS to all the text that is inserted."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn (insert ,@body))
	 (add-text-properties ,start (point) ,props)))))

(provide 'erl-service)

