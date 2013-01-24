;;; erl.el --- Erlang-style process runtime system.

;;; Commentary:
;;
;; This module provides an Erlang-like runtime system in
;; Emacs. Processes are Emacs buffers with local variables containing
;; the pid, mailbox, etc.
;;
;; When a process is spawned it gets assigned a pid and a new buffer,
;; and its initial function is called in that buffer. This function
;; can do some initial processing, and then call (erl-continue K),
;; where K is a continuation function to called the next time the
;; process is scheduled. Usually the process won't be scheduled until
;; it receives a new message, which it can then retreive from its
;; mailbox. If the process returns without setting a new continuation,
;; it terminates with 'normal' status.

(eval-when-compile (require 'cl))
(provide 'erl)				; avoid recursive require
(require 'derl)
(require 'erl-service)
(require 'patmatch)

;; Process ID structure.
;;
;; Exactly matches the [ERL-TAG erl-pid NODE ID SERIAL CREATION] vector used
;; in the `erlext' mapping, so don't change it!
(defstruct (erl-pid
	    (:type vector)
	    :named
	    (:initial-offset 1)		; make room for erl-tag (TYPE)
	    (:constructor nil)		; no default constructor
	    (:constructor %make-erl-local-pid (&optional (id (incf erl-pid-counter))
							(node erl-node-name)
							(serial 0)
							(creation 0))))
  node id serial creation)

(defun make-erl-local-pid (&optional id)
  "Make a node-local pid."
  (let ((pid (if id
		 (%make-erl-local-pid id)
	       (%make-erl-local-pid))))
    ;; Tag the first element of the pid
    (setf (elt pid 0) erl-tag)
    pid))

;; Global book keeping state

(defvar erl-node-name nil		; initialised below
  "Node name for Emacs.")

(defun erl-determine-hostname ()
  "Figure out the short-names hostname."
  (let ((fqdn (system-name)))
    (if (string-match "\\.local$" fqdn)
	fqdn
      (if (string-match "[^\\.]+" fqdn)
	  (match-string 0 fqdn)
	(error "erl: Can't determine hostname.")))))

(when (null erl-node-name)
  (setq erl-node-name
	(intern (format "distel_%S@%s" (emacs-pid) (erl-determine-hostname)))))

(defconst erl-null-pid (make-erl-local-pid 0)
  "\"Null process\", the /dev/null of erl processes.
Any messages sent to this process are quietly discarded.  When code
isn't running in the buffer of a particular process, it's running as
the null process.")

(defvar erl-pid-counter 0
  "Counter for PIDs.")

(defvar erl-process-buffer-alist nil
  "Automatically-maintained association list of (PID-ID . BUFFER)
mappings for local processes.")

(defvar erl-schedulable-processes nil
  "List of processes which can be scheduled to run.")

(defvar erl-in-scheduler-loop nil
  "True when the scheduler loop is on the call stack, i.e. when
schedulable processes are guaranteed to be executed before control is
passed back to Emacs.")

(defvar erl-default-group-leader erl-null-pid
  ;; Initialized to a real process further down
  "Default group_leader for new processes.
Processes spawned by other processes will inherit their GL, but
\"brand new\" ones will use this.")

(defvar erl-popup-on-output t
  "Popup *erl-output* when new output arrives.")

(defvar erl-stop-on-error nil
  "*When non-nil, prevents the scheduler from catching Elisp errors.
Such errors are allowed to propagate, so you can debug them using
`debug-on-error'.

Because this interrupts the scheduling of processes, you must use the
command `erl-schedule' to continue.")

(defvar erl-ref-counter 0
  "Unique reference id counter.")

(defvar erl-nodes nil
  "List of nodes that we are currently connected to.")

;; Process-local variables

(eval-when-compile
(defmacro defprocvar (symbol &optional initvalue docstring)
  "Define SYMBOL as a buffer-local process variable."
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     ;; stop major modes' `kill-all-local-variables' from rubbing out
     ;; the process state
     (put ',symbol 'permanent-local t))))

;; FIXME - what's the right incantation to have defprocvar fontified
;; as a keyword?

(defprocvar erl-self erl-null-pid
  "Current process' pid.
Always bound in a process' buffer, but in
other buffers defaults to `erl-null-pid'.")
(defprocvar erl-mailbox nil
  "Process mailbox.
Contains messages for the process, which it's supposed to process and
remove.  Messages are ordered from oldest to newest.")
(defprocvar erl-group-leader nil
  "Group leader process.")
(defprocvar erl-continuation nil
  "Function for the scheduler to call to run the process.")
(defprocvar erl-continuation-args nil
  "Arguments for continuation function.")
(defprocvar erl-links nil
  "Process links.")
(defprocvar erl-trap-exit nil
  "True when trapping exits from linked processes.")
(defprocvar erl-exit-reason nil
  "Exit reason, or nil if the process is alive.")
(defprocvar erl-reductions 0
  "Number of \"reductions\".
Actually the number of times the process has been run invoked,
typically by the scheduler.")
(defprocvar erl-process-name nil
  "Name of the process, a string.
This can be set and then later used to help with debugging.
An example name would be \"Process list for z@cockatoo\"")

(defmacro with-erl-process (pid &rest body)
  "Execute BODY in PID's buffer.  This is a full context-switch."
  `(with-current-buffer (erl-pid->buffer ,pid)
     ,@body))

;; Bindings capture helpers

(defmacro capture-bindings (&rest vars)
  "Create a data structure representing the bidings of VARS.
These bindings can be restored by `with-bindings' or
`call-with-bindings'."
  `(list ',vars (list ,@vars)))

(defmacro with-bindings (bindings &rest body)
  "Run BODY with BINDINGS restored.
BINDINGS is created by `capture-bindings'"
  `(call-with-bindings ,bindings (lambda () ,@body)))

(defun call-with-bindings (bindings fn)
  "Call FN with BINDINGS restored.
BINDINGS is created by `capture-bindings'"
  (let ((vars (car bindings))
	(vals (cadr bindings)))
    (eval `(apply (lambda ,vars (funcall ,fn)) ',vals))))

;; Process API functions

(defmacro erl-spawn-async (&rest body)
  "Spawn a new erl process and have it execute BODY.

Since the process may be scheduled for later, BODY is not guaranteed
to run in the current dynamic environment; see `erl-spawn' for an
alternative."
  `(erl-spawn-fun (lambda () ,@body)))

(defmacro erl-spawn (&rest body)
  "Spawn a new process and run it in the current dynamic environment."
  `(erl-spawn-fun (lambda () ,@body) t))

(defmacro erl-spawn-link-async (&rest body)
  "Same as `erl-spawn-async', but links with the process."
  `(erl-spawn-fun (lambda () ,@body) nil t))

(defmacro erl-spawn-link (&rest body)
  "Same as `erl-spawn', but links with the process."
  `(erl-spawn-fun (lambda () ,@body) t t))

(defun erl-link (pid)
  "Link the current process with PID."
  (unless (equal pid erl-self)
    (erl-add-link erl-self pid)
    (erl-add-link pid erl-self)))

(defun erl-unlink (pid)
  "Unlink the current process from PID."
  (erl-remove-link erl-self pid)
  (erl-remove-link pid erl-self))

(defun erl-send (who message)
  "Send the term MESSAGE to the process WHO.
WHO can be a pid, a registered name (symbol), or a tuple of
\[REGISTERED-NAME NODE]."
  (cond ((erl-null-pid-p who)
	 (erl-lose-msg message))
	((erl-local-pid-p who)
	 (when (erl-local-pid-alive-p who)
	   (erl-deliver-message who message)))
	((erl-remote-pid-p who)
	 (erl-dist-send who message))
	((symbolp who)
	 (let ((proc (erl-whereis who)))
	   (if proc
	       (erl-send proc message)
	     (erl-exit (tuple 'badarg (tuple 'not-registered who))))))
	((tuplep who)			; [tuple NAME NODE]
	 (erl-dist-reg-send (tuple-elt who 2) (tuple-elt who 1) message))
	(t
	 (error "Bad pid: %S" who))))

(defun erl-exit (why &optional who)
  "Exit the current process.
Like the erlang BIF exit/1."
  (if who
      (erl-send-exit erl-self who why)
    (signal 'erl-exit-signal (list why))))

(defun erl-exit/2 (who why)
  "Exit a process.
Like the erlang BIF exit/2."
  (erl-exit why who))

(defun erl-continue (k &rest args)
  "Yield control and arrange for K to be called with ARGS the next
time this process is scheduled.  Note that the process is not
\"scheduled out\" automatically, and the caller should return
normally."
  (setq erl-continuation k)
  (setq erl-continuation-args args))

(defun erl-reschedule ()
  "Return immedaitely (via `throw') to the scheduler.
Also makes the current process immediately reschedulable."
  ;; the scheduler loop will catch this and know what to do
  (throw 'schedule-out 'reschedule))

(defun erl-idle ()
  (erl-receive ()
      ()))

(defun erl-make-ref ()
  "Make a unique reference object."
  (vector erl-tag 'erl-ref erl-node-name (incf erl-ref-counter) 0))

(defun erl-binaryp (x)
  (and (eq (elt x 0) erl-tag) (eq (elt x 1) 'erl-binary)))

(defun erl-binary-string (x)
  (elt x 2))

(defun erl-binary (string)
  (vector erl-tag 'erl-binary string))

;; receive


(defmacro erl-receive (vars clauses &rest after)
  "Receive a message, matched by pattern.
If the mailbox contains a matching message, the pattern's body is
executed immediately.  Otherwise, `erl-continue' is used to make the
process continue matching when new messages arrive.  The crucial
difference from Erlang's receive is that erl-receive returns
immediately when nothing is matched, but will automatically resume
after a new message arrives and the process is rescheduled.

Since the the process may return and be rescheduled before the
matching message is received, the clause's body might not be executed
in the original dynamic environment.  Consequently, any local variable
bindings that need to be preserved should be named in VARS.

After a pattern has been matched and executed, the AFTER forms are
then executed.

The overall syntax for receive is:

  (erl-receive (VAR-NAME ...)
      ((PATTERN . BODY)
       ...)
    . AFTER)

The pattern syntax is the same as `pmatch'."
  `(erl-start-receive (capture-bindings ,@vars)
			,(mcase-parse-clauses clauses)
			(lambda () ,@after)))

(defun erl-start-receive (bs clauses after)
  ;; Setup a continuation and immediately return to the scheduler
  ;; loop, which will call us back.
  (when (equal erl-self erl-null-pid)
    (error "No process context for erl-receive"))
  (erl-continue #'erl-receive* bs clauses after)
  (erl-reschedule))

(defun erl-receive* (bs clauses after)
  (erl-receive-loop bs clauses after erl-mailbox))

(defun erl-receive-loop (bs clauses after msgs &optional acc)
  (if (null msgs) 
      (erl-continue #'erl-receive* bs clauses after)
    (let ((action
	   ;; We restore the bindings incase they are referred to in patterns
	   (with-bindings bs
	     (mcase-choose (car msgs) clauses))))
      (if (null action)
	  (erl-receive-loop bs clauses after (cdr msgs) (cons (car msgs) acc))
	(setq erl-mailbox (append (reverse acc) (cdr msgs)))
	(with-bindings bs
	  (funcall action)
	  (funcall after))))))

(defun erl-register (name &optional process)
  "Register PROCESS with NAME."
  (if (get-buffer (regname-to-bufname name))
      (erl-exit (tuple 'badarg (tuple 'already-registered name)))
    (with-erl-process (or process erl-self)
      (rename-buffer (regname-to-bufname name)))))

(defun erl-whereis (name)
  "Get the PID of the process registered with NAME, or nil if the name
is unregistered."
  (let ((buf (get-buffer (regname-to-bufname name))))
    (if buf
	(with-current-buffer buf erl-self))))

(defun regname-to-bufname (name)
  (format "*reg %S*" name))

(defalias 'erl-term-to-binary #'erlext-term-to-binary)
(defalias 'erl-binary-to-term #'erlext-binary-to-term)

(defun erl-group-leader ()
  (or erl-group-leader erl-default-group-leader))

;; Guts
;;
;; The scheduler works without ever being explicitly called.  It runs
;; when a message arrives from a remote node or when a new process is
;; spawned, and it continues to schedule processes until none are
;; runnable.
;;
;; The %ugly-variable-names are to avoid shadowing any existing
;; dynamic bindings between the caller and the process being invoked -
;; that's a bastard to debug.

(put 'erl-exit-signal
     'error-conditions
     '(error erl-exit-signal))

(defun erl-spawn-fun (%init-function &optional %run-now-p %link)
  "Spawn a new erl process to call INIT-FUNCTION.
If RUN-NOW-P is true, the process is called immediately with the
current dynamic environment/bindings.  Otherwise, the process is made
schedulable.  The scheduler loop is entered if we aren't being called
by it already.
If LINK is true, the process is linked before being run."
  (let* ((%pid (make-erl-local-pid))
	 (%buffer (get-buffer-create (erl-pid-buffer-name %pid)))
	 (%gl (or erl-group-leader erl-default-group-leader)))
    (with-current-buffer %buffer
      (setq erl-self %pid)
      (setq erl-group-leader %gl)
      (setq erl-continuation %init-function)
      (setq erl-continuation-args nil)
      (erl-enroll-process))
    (when %link (erl-link %pid))
    (if %run-now-p
	(let ((erl-in-scheduler-loop t))
	  (erl-run-process %pid))
      (erl-make-schedulable %pid))
    (erl-maybe-schedule)
    %pid))

(defun erl-deliver-message (pid message)
  "Deliver MESSAGE to the mailbox of the local process PID.
Invokes the scheduler if necessary."
  (with-erl-process pid
    (setq erl-mailbox (append erl-mailbox (list message))))
  (erl-make-schedulable pid)
  (erl-maybe-schedule))

(defun erl-schedule ()
  "Enter scheduler loop until no process is runnable."
  (interactive)
  (let ((erl-in-scheduler-loop t))
    (while (erl-schedule-once)))
  ;; post-condition
  (assert (null erl-schedulable-processes)))

(defun erl-schedule-once ()
  "Schedule the next process to run.  Returns true if a process was scheduled."
  (when erl-schedulable-processes
    (erl-schedule-process (pop erl-schedulable-processes))
    t))

(defun erl-maybe-schedule ()
  "Schedule processes, unless the scheduler is already running."
  (unless erl-in-scheduler-loop
    (erl-schedule)))

(defun erl-schedule-process (%pid)
  (cond ((not (erl-local-pid-alive-p %pid))
	 (message "STRANGE: %S scheduled but dead; removing" %pid)
	 (erl-make-unschedulable %pid))
	((with-erl-process %pid (null erl-continuation))
	 (message "STRANGE: %S is a zombie! killing" %pid)
	 (with-erl-process %pid (erl-terminate 'zombie)))
	(t
	 (while (eq 'reschedule (erl-run-process %pid))))))

(defun erl-run-process (%pid)
  "Run a process.
Calls the current continuation from within the process' buffer."
  (with-erl-process %pid
    (incf erl-reductions)
    ;; The %ugly-names are to avoid shadowing the caller's dynamic
    ;; bindings.
    (let ((%k erl-continuation)
	  (%kargs erl-continuation-args)
	  (%buffer (current-buffer)))
      (setq erl-continuation nil)
      (setq erl-continuation-args nil)
      (if erl-stop-on-error
	  (erl-invoke %k %kargs)
	(condition-case data
	    (erl-invoke %k %kargs)
	  (error (erl-terminate `[emacs-error ,(format "%S" data)])))))))

(defun erl-invoke (%k %kargs)
  (condition-case data
      (prog1 (catch 'schedule-out
	       (prog1 nil (save-current-buffer (apply %k %kargs))))
	(unless erl-continuation
	  (erl-terminate 'normal)))
    (erl-exit-signal (erl-terminate (cadr data)))))

(defun erl-make-schedulable (pid)
  "Add PID to the list of runnable processes, so that it will execute
during the next `erl-schedule'."
  (unless (member pid erl-schedulable-processes)
    (setq erl-schedulable-processes
	  (append erl-schedulable-processes (list pid)))))

(defun erl-make-unschedulable (pid)
  "Remove PID from the list of schedulable processes."
  (setq erl-schedulable-processes
	(remove pid erl-schedulable-processes)))

(defun erl-terminate (why)
  "Exit the current process."
  (unless (eq why 'normal)
    (message "EXIT: %s %S %s" (erl-pid-to-string erl-self) why
	     (if erl-process-name
		 (concat "\n  Process name: " erl-process-name)
	       "")))
  (setq erl-exit-reason why)
  (erl-make-unschedulable erl-self)
  (kill-buffer (erl-pid->buffer erl-self)))

(defun erl-add-link (from to)
  "Unidirectionally add a link."
  (unless (erl-null-pid-p from)
    (with-erl-process from
      (add-to-list 'erl-links to))))

(defun erl-remove-link (from to)
  "Unidirectionally remove a link."
  (unless (erl-null-pid-p from)
    (with-erl-process from
      (setq erl-links (remove to erl-links)))))

(defun erl-set-name (fmt &rest args)
  "Set `erl-process-name' to (apply 'format (FMT . ARGS))."
  (setq erl-process-name (apply 'format (cons fmt args))))

;; PID utilities

(defun erl-pid-buffer-name (pid)
  (unless (equal (erl-pid-node pid) erl-node-name)
    (error "Not a local pid: %S" pid))
  (format "*pid <%S.%S.%S>*"
	  0
	  (erl-pid-id pid)
	  (erl-pid-serial pid)))

(defun erl-pid->buffer (pid)
  "Get PID's buffer."
  (or (cdr (assoc (erl-pid-id pid) erl-process-buffer-alist))
      (error "No buffer for pid %S" pid)))

(defun erl-null-pid-p (p)
  (equal p erl-null-pid))

(defun erl-local-pid-alive-p (pid)
  "Is PID a live local process?"
  (when (erl-local-pid-p pid)
    (let ((buffer (cdr (assoc (erl-pid-id pid) erl-process-buffer-alist))))
      (and buffer
	   (buffer-live-p buffer)
	   (with-erl-process pid
	     (null erl-exit-reason))))))

(defun erl-local-pid-p (x)
  "True iff X is the pid of a local process."
  (and (erl-pid-p x)
       (equal (erl-pid-node x) erl-node-name)))

(defun erl-remote-pid-p (x)
  "True iff X is the pid of a remote process."
  (and (erl-pid-p x)
       (not (erl-local-pid-p x))))

(defun erl-pid-to-string (pid)
  ;; FIXME: number nodes
  (let ((n (if (eq (erl-pid-node pid) erl-node-name)
	       "0" ; local
	     "?")))
    (format "<%s.%S.%S>" n (erl-pid-id pid) (erl-pid-serial pid))))

(defun erl-lose-msg (msg)
  "Log and discard a message sent to the null process."
  (with-current-buffer (get-buffer-create "*erl-lost-msgs*")
    (goto-char (point-max))
    (insert (format "%S\n" msg))))

(defun erl-enroll-process ()
  "Setup pid->buffer mapping state for the current process."
  (push (cons (erl-pid-id erl-self) (current-buffer))
	erl-process-buffer-alist)
  (make-local-variable 'kill-buffer-hook)
  (put 'kill-buffer-hook 'permanent-local t)  
  (add-hook 'kill-buffer-hook 'erl-unenroll-process)
  (add-hook 'kill-buffer-hook 'erl-propagate-exit))

(defun erl-remove-if (predicate list)
  "Return a copy of LIST with all items satisfying PREDICATE removed."
  (let (out)
    (while list
      (unless (funcall predicate (car list))
	(push (car list) out))
      (setq list (cdr list)))
    (nreverse out)))

(defun erl-unenroll-process ()
  (setq erl-process-buffer-alist
	(erl-remove-if #'(lambda (x) (eq (erl-pid-id erl-self) (car x)))
		   erl-process-buffer-alist)))

(defun erl-propagate-exit ()
  (when (null erl-exit-reason)
    (setq erl-exit-reason 'killed))
  (unless (eq erl-exit-reason 'normal)
    (mapc #'(lambda (proc) (erl-send-exit erl-self proc erl-exit-reason))
	  erl-links)))

(defun erl-send-exit (from to rsn)
  (cond ((erl-local-pid-alive-p to)
	 (erl-deliver-exit from to rsn))
	((erl-remote-pid-p to)
	 (erl-dist-exit from to rsn))))

(defun erl-deliver-exit (from to rsn)
  (with-erl-process to
    (cond (erl-exit-reason	; already terminated?
	   t)
	  (erl-trap-exit
	   (erl-deliver-message to (tuple 'EXIT from rsn))
	   (erl-unlink from))
	  (t
	   (erl-terminate rsn)))))

(defun erl-nodedown-exit (local remote)
  "Send an exit to LOCAL from REMOTE caused by a communications failure."
  (when (erl-local-pid-alive-p local)
    (with-erl-process local
      (setq erl-links (remove remote erl-links))
      (erl-deliver-exit remote local 'noconnection))))

(defun impossible (&optional reason)
  "Raise an error because something \"impossible\" has happened."
  (if reason
      (error "Impossible: %s" reason)
    (error "The impossible has occured")))

(defun nyi ()
  (error "Not yet implemented!"))

;; Initialisation

(defun erl-nodeup (node proc)
  (pushnew node erl-nodes)
  (message "nodeup: %S" node))

(defun erl-nodedown (node)
  (setq erl-nodes (remove node erl-nodes))
  (message "nodedown: %S" node))

;; These hooks are defined in derl.el
(add-hook 'erl-nodeup-hook 'erl-nodeup)
(add-hook 'erl-nodedown-hook 'erl-nodedown)

;; Emacs indentation
(put 'with-erl-process 'lisp-indent-function 1)
(put 'erl-spawn 'lisp-indent-function 'defun)
(put 'erl-spawn-async 'lisp-indent-function 'defun)
(put 'erl-receive 'lisp-indent-function 2)
(put 'with-bindings 'lisp-indent-function 1)

;; Initial processes

(defun &erl-group-leader-loop ()
  (erl-receive ()
      ((['put_chars s]
	(if (eq s nil)
	    nil
        (condition-case err
            (save-excursion
              (with-current-buffer (get-buffer-create "*erl-output*")
                (save-selected-window
                  (if erl-popup-on-output
                      (select-window (or (get-buffer-window (current-buffer))
                                         (display-buffer (current-buffer)))))
                  (goto-char (point-max))
                  (insert s))))
          (error (message "Error in group leader: %S" err))))))
    (&erl-group-leader-loop)))

(when (null erl-group-leader)
  (setq erl-default-group-leader
        (erl-spawn
          (erl-register 'group-leader)
          (&erl-group-leader-loop))))

