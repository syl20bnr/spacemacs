; Network state machine engine

(eval-when-compile (require 'cl))

(defcustom fsm-use-debug-buffer nil
  "*Store fsm debug messages in a buffer."
  :type 'boolean
  :group 'distel)

(defvar fsm-buffer-p nil
  "Set to t in buffers belonging to FSMs, for sanity-checking.")
(defvar fsm-state nil
  "Current state.")
(defvar fsm-process nil
  "Socket associated with this FSM.")
(defvar fsm-cont nil
  "Continuation function called with the result of the FSM, if it
  terminates successfully (with fsm-terminate).")
(defvar fsm-fail-cont nil
  "Continuation function called with the result of the FSM, if it
  terminates in failure.")
(defvar fsm-work-buffer nil
  "Buffer used for creating messages, dynamically bound in
  `fsm-build-message'")
(defvar fsm-put-data-in-buffer nil
  "When set to `t', new data is appended to the FSM's buffer in
addition to being passed as an argument.")
(defvar fsm-cleanup-hook nil)

(make-variable-buffer-local 'fsm-buffer-p)
(make-variable-buffer-local 'fsm-state)
(make-variable-buffer-local 'fsm-process)
(make-variable-buffer-local 'fsm-cont)
(make-variable-buffer-local 'fsm-fail-cont)
(make-variable-buffer-local 'fsm-work-buffer)
(make-variable-buffer-local 'fsm-put-data-in-buffer)

(defmacro with-error-cleanup (cleanup &rest body)
  "Execute BODY, and if it hits an error run CLEANUP."
  (let ((success (make-symbol "success")))
    `(let (,success)
       (unwind-protect
	   (prog1 (progn ,@body)
	     (setq ,success t))
	 (unless ,success ,cleanup)))))

(put 'with-error-cleanup 'lisp-indent-function 1)

;; ----------------------------------------------------------------------
;; External API
;; ----------------------------------------------------------------------

(defun fsm-open-socket (host port)
  (let ((buf (generate-new-buffer " *net-fsm*")))
    (with-error-cleanup (kill-buffer buf)
      (let ((p (open-network-stream "netfsm" buf host port)))
	(set-process-coding-system p 'no-conversion 'no-conversion)
	(if (fboundp 'set-process-filter-multibyte)
	  (set-process-filter-multibyte p nil))
	p))))

(defun fsm-connect (host port state0 &optional init-arg cont fail-cont buffer)
  "Connect to HOST on PORT and initialize a state machine in
STATE0 to handle the socket.

INIT-ARG is passed to the state machine as the `init' event's
argument. CONT is a function which is called with the FSM's result if
it terminates successfully. FAIL-CONT is called with no arguments if
the FSM fails."
  (with-error-cleanup (funcall fail-cont)
    (let ((socket (fsm-open-socket host port)))
      (fsm-attach socket state0 init-arg cont fail-cont buffer))))

(defun fsm-attach (socket state0 &optional init-arg cont fail-cont buffer)
  "Attach a new FSM to SOCKET, starting in STATE0.

INIT-ARG is passed to the state machine as the `init' event's
argument. CONT is a function which is called with the FSM's result if
it terminates successfully. FAIL-CONT is called with no arguments if
the FSM fails."
  (when buffer
    (replace-process-buffer socket buffer))
  (with-current-buffer (process-buffer socket)
    (unless (featurep 'xemacs)
      (set-buffer-multibyte nil))
    (setq fsm-buffer-p t)
    (setq fsm-state state0)
    (setq fsm-process socket)
    (setq fsm-cont cont)
    (setq fsm-fail-cont fail-cont)
    (set-process-sentinel socket #'fsm-sentinel)
    (set-process-filter   socket #'fsm-filter)
    (init-fsm init-arg)))

(defmacro with-fsm (fsm &rest body)
  "Execute BODY in the context (buffer) of FSM."
  `(with-current-buffer (process-buffer ,fsm)
     ,@body))

;; ----------------------------------------------------------------------
;; FSM API
;; ----------------------------------------------------------------------

(defun fsm-change-state (next-state &optional run-now)
  "Change to `next-state'."
  (fsm-debug "STATE: %S -> %S\n" fsm-state next-state)
  (setq fsm-state next-state)
  (when run-now
    (fsm-event 'data "")))

(defun fsm-event (event &optional arg)
  "Process `event' in the current state."
  (assert-fsm-invariants)
  (fsm-debug "EVENT: %S - %S\n" event arg)
  (with-error-cleanup
      (fsm-fail (format "Error on event %S in state %S"
			event fsm-state))
	(funcall fsm-state event arg)))

(defun fsm-terminate (&optional result)
  "Terminate an FSM with success. The continuation function, if
available, is called with RESULT."
  (fsm-debug "TERM : %S\n" result)
  (assert-fsm-invariants)
  (let ((cont fsm-cont))
    (fsm-shutdown)
    (when cont
      (funcall cont result))))

(defun fsm-fail (&optional why)
  "Terminate an FSM with failure."
  (if why
      (fsm-debug "FAIL : %S (buffer: %S)\n" why (current-buffer))
    (fsm-debug "FAIL : (buffer: %S)\n" (current-buffer)))
  (let ((cont fsm-fail-cont))
    (fsm-shutdown)
    (when cont
      (funcall cont))))

(defun fsm-send-string (string)
  "Send a string to the FSM's socket."
  (fsm-debug "SEND : %S\n" string)
  (process-send-string fsm-process string))

(defun fsm-send-bytes (chars)
  "Send a list of bytes to the FSM's socket."
  (fsm-send-string (apply #'string chars)))

(defun fsm-debug (fmt &rest args)
  "Print a debugging message to the *fsm-debug* buffer."
  (if fsm-use-debug-buffer
      (with-current-buffer (get-buffer-create "*fsm-debug*")
	(unless (featurep 'xemacs)
	  (set-buffer-multibyte nil))
	(goto-char (point-max))
	(insert (apply #'format (cons fmt (mapcar #'summarise args)))))))

(defun check-event (event &rest allowed)
  "Ensure that an event is allowed. If EVENT is not one of ALLOWED, an
error is signaled."
  (unless (memq event allowed)
    (error "Can't handle event %S in state %S" event fsm-state)))

;; ------------------------------------------------------------
;; Message building
;; ------------------------------------------------------------

(defmacro fsm-build-message (&rest body)
  "Execute BODY, and return the message that it creates via calls to
fsm-{insert,encode}*."
  `(let ((fsm-work-buffer (let ((default-enable-multibyte-characters nil))
			     (generate-new-buffer " *fsm-msg*"))))
     (unwind-protect
	 (progn ,@body
		(with-current-buffer fsm-work-buffer (buffer-string)))
       (kill-buffer fsm-work-buffer))))

(defmacro fsm-with-message-buffer (&rest body)
  "Execute BODY in the work buffer setup by fsm-build-message. When
called outside fsm-build-message, BODY is just executed in the current
buffer."
  `(with-current-buffer (or fsm-work-buffer
			    (current-buffer)) ,@body))

(put 'fsm-build-message 'lisp-indent-function 'defun)
(put 'fsm-with-message-buffer 'lisp-indent-function 1)

(defun fsm-encode (n size)
  "Encode N as a SIZE-byte integer."
  (ecase size
    ((1) (fsm-encode1 n))
    ((2) (fsm-encode2 n))
    ((4) (fsm-encode4 n))))
(defun fsm-encode1 (n)
  "Encode N as a 1-byte integer."
  (fsm-with-message-buffer
      (insert n)))
(defun fsm-encode2 (n)
  "Encode N as a 2-byte big-endian integer."
  (fsm-with-message-buffer
      (insert (logand (ash n -8) 255)
	      (logand n          255))))
(defun fsm-encode4 (n)
  "Encode N as a 4-byte big-endian integer."
  (fsm-with-message-buffer
      (insert (logand (ash n -24) 255)
	      (logand (ash n -16) 255)
	      (logand (ash n -8)  255)
	      (logand n           255))))
(defun fsm-insert (&rest args)
  "Insert ARGS (characters or strings) into the encoding buffer."
  (fsm-with-message-buffer
      (apply #'insert args)))

;; ----------------------------------------------------------------------
;; Internals
;; ----------------------------------------------------------------------

(defun init-fsm (init-arg)
  "Deliver initial events: INIT, and possibly DATA if some has arrived."
  (let ((data (buffer-string)))
    (erase-buffer)
    (fsm-event 'init init-arg)
    (unless (= 0 (length data))
      (fsm-deliver-data data))))

(defun fsm-filter (socket string)
  (with-current-buffer (process-buffer socket)
    (when fsm-state (fsm-deliver-data string))))

(defun fsm-deliver-data (data)
  (when fsm-put-data-in-buffer
    ;; incorporate the new data into the buffer
    (goto-char (point-max))
    (insert data))
  (fsm-event 'data data))

(defun fsm-sentinel (socket event)
  (with-current-buffer (process-buffer socket)
    (fsm-event 'closed event)))

(defun fsm-shutdown ()
  (setq fsm-state nil)
  (when fsm-process
    (set-process-sentinel fsm-process nil)
    (kill-buffer (process-buffer fsm-process))))

(defun assert-fsm-invariants ()
  (assert fsm-buffer-p)
  (assert (not (null fsm-state))))

(defun summarise (x)
  (if (stringp x)
      (with-temp-buffer
	(insert x)
	(goto-char (point-min))
	(while (search-forward "\n" nil t)
	  (replace-match "\\n" nil t))
	(elide-string (buffer-string) 30))
    x))

(defun elide-string (s len)
  (if (> (length s) len)
      (concat (substring s 0 (- len 3)) "...")
    s))

(defun replace-process-buffer (process buffer)
  (let ((oldbuffer (process-buffer process)))
    (set-process-buffer process buffer)
    (kill-buffer oldbuffer)))

(provide 'net-fsm)
