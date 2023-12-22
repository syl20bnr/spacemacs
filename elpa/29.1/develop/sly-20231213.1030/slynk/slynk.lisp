;;;; slynk.lisp --- Server for SLY commands.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file defines the "Slynk" TCP server for Emacs to talk to. The
;;; code in this file is purely portable Common Lisp. We do require a
;;; smattering of non-portable functions in order to write the server,
;;; so we have defined them in `slynk-backend.lisp' and implemented
;;; them separately for each Lisp implementation. These extensions are
;;; available to us here via the `SLYNK-BACKEND' package.

(defpackage :slynk
  (:use :cl :slynk-backend :slynk-match :slynk-rpc)
  (:export #:startup-multiprocessing
           #:start-server
           #:create-server
           #:stop-server
           #:restart-server
           #:ed-in-emacs
           #:inspect-in-emacs
           #:print-indentation-lossage
           #:invoke-sly-debugger
           #:slynk-debugger-hook
           #:emacs-inspect
           ;;#:inspect-slot-for-emacs
           #:authenticate-client
           #:*loopback-interface*
           #:*buffer-readtable*
           #:process-requests)
  ;; These are user-configurable variables:
  (:export #:*communication-style*
           #:*dont-close*
           #:*fasl-pathname-function*
           #:*log-events*
           #:*log-output*
           #:*configure-emacs-indentation*
           #:*readtable-alist*
           #:*global-debugger*
           #:*sly-db-quit-restart*
           #:*backtrace-printer-bindings*
           #:*default-worker-thread-bindings*
           #:*macroexpand-printer-bindings*
           #:*slynk-pprint-bindings*
           #:*string-elision-length*
           #:*inspector-verbose*
           #:*require-module*
           #:*eval-for-emacs-wrappers*
           #:*debugger-extra-options*
           ;; These are exceptions: they are defined later in
           ;; slynk-mrepl.lisp
           ;;
           #:*globally-redirect-io*
           #:*use-dedicated-output-stream*
           #:*dedicated-output-stream-port*
           #:*dedicated-output-stream-buffering*
           ;; This is SETFable.
           #:debug-on-slynk-error
           ;; These are re-exported directly from the backend:
           #:buffer-first-change
           #:frame-source-location
           #:gdb-initial-commands
           #:restart-frame
           #:sly-db-step
           #:sly-db-break
           #:sly-db-break-on-return
           #:default-directory
           #:set-default-directory
           #:quit-lisp
           #:eval-for-emacs
           #:eval-in-emacs
           #:y-or-n-p-in-emacs
           #:*find-definitions-right-trim*
           #:*find-definitions-left-trim*
           #:*after-toggle-trace-hook*
           #:*echo-number-alist*
           #:*present-number-alist*))

(in-package :slynk)


;;;; Top-level variables, constants, macros

(defconstant cl-package (find-package :cl)
  "The COMMON-LISP package.")

(defconstant +keyword-package+ (find-package :keyword)
  "The KEYWORD package.")

(defconstant default-server-port 4005
  "The default TCP port for the server (when started manually).")

(defvar *slynk-debug-p* t
  "When true, print extra debugging information.")

(defvar *m-x-sly-from-emacs* nil
  "Bound to non-nil in START-SERVER.")

(defvar *backtrace-pprint-dispatch-table*
  (let ((table (copy-pprint-dispatch nil)))
    (flet ((print-string (stream string)
             (cond (*print-escape*
                    (escape-string string stream
                                   :map '((#\" . "\\\"")
                                          (#\\ . "\\\\")
                                          (#\newline . "\\n")
                                          (#\return . "\\r"))))
                   (t (write-string string stream)))))
      (set-pprint-dispatch 'string  #'print-string 0 table)
      table)))

(defvar *backtrace-printer-bindings*
  `((*print-pretty*           . t)
    (*print-readably*         . nil)
    (*print-level*            . 4)
    (*print-length*           . 6)
    (*print-lines*            . 1)
    (*print-right-margin*     . 200)
    (*print-pprint-dispatch*  . ,*backtrace-pprint-dispatch-table*))
  "Pretter settings for printing backtraces.")

(defvar *default-worker-thread-bindings* '()
  "An alist to initialize dynamic variables in worker threads.
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE.")

(defun call-with-bindings (alist fun)
  "Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."
  (if (null alist)
      (funcall fun)
      (let* ((rlist (reverse alist))
             (vars (mapcar #'car rlist))
             (vals (mapcar #'cdr rlist)))
        (progv vars vals
          (funcall fun)))))

(defmacro with-bindings (alist &body body)
  "See `call-with-bindings'.
Bindings appearing earlier in the list take priority"
  `(call-with-bindings ,alist (lambda () ,@body)))

;;; The `DEFSLYFUN' macro defines a function that Emacs can call via
;;; RPC.

(defvar *slyfuns* (make-hash-table)
  "A map of Sly functions.")

(defmacro defslyfun (name arglist &body rest)
  "A DEFUN for functions that Emacs can call by RPC."
  `(progn
     (defun ,name ,arglist ,@rest)
     (setf (gethash ',name *slyfuns*) #',name)
     ;; see <http://www.franz.com/support/documentation/6.2/\
     ;; doc/pages/variables/compiler/\
     ;; s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name (symbol-package ',name)))))

(defun missing-arg ()
  "A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."
  (error "A required &KEY or &OPTIONAL argument was not supplied."))


;;;; Hooks
;;;
;;; We use Emacs-like `add-hook' and `run-hook' utilities to support
;;; simple indirection. The interface is more CLish than the Emacs
;;; Lisp one.

(defmacro add-hook (place function)
  "Add FUNCTION to the list of values on PLACE."
  `(pushnew ,function ,place))

(defun run-hook (functions &rest arguments)
  "Call each of FUNCTIONS with ARGUMENTS."
  (dolist (function functions)
    (apply function arguments)))

(defvar *new-connection-hook* '()
  "This hook is run each time a connection is established.
The connection structure is given as the argument.
Backend code should treat the connection structure as opaque.")

(defvar *connection-closed-hook* '()
  "This hook is run when a connection is closed.
The connection as passed as an argument.
Backend code should treat the connection structure as opaque.")

(defvar *pre-reply-hook* '()
  "Hook run (without arguments) immediately before replying to an RPC.")

(defvar *after-init-hook* '()
  "Hook run after user init files are loaded.")


;;;; Connections
;;;
;;; Connection structures represent the network connections between
;;; Emacs and Lisp. 
;;;
(defstruct (connection
             (:constructor %make-connection)
             (:conc-name connection-)
             (:print-function print-connection))
  ;; The listening socket. (usually closed)
  ;; 
  (socket           (missing-arg) :type t :read-only t)
  ;; Character I/O stream of socket connection.  Read-only to avoid
  ;; race conditions during initialization.
  ;; 
  (socket-io        (missing-arg) :type stream :read-only t)
  ;; An alist of (ID . CHANNEL) entries. Channels are good for
  ;; streaming data over the wire (see their description in sly.el)
  ;;
  (channel-counter 0 :type number)
  (channels '() :type list)
  ;; A list of LISTENER objects. Each listener has a couple of streams
  ;; and an environment (an alist of bindings)
  ;;
  (listeners '() :type list)
  ;; A list of INSPECTOR objects. Each inspector has its own history
  ;; of inspected objects. An inspector might also be tied to a
  ;; specific thread.
  ;; 
  (inspectors '() :type list)
  ;;Cache of macro-indentation information that
  ;; has been sent to Emacs.  This is used for preparing deltas to
  ;; update Emacs's knowledge.  Maps: symbol ->
  ;; indentation-specification
  ;; 
  (indentation-cache (make-hash-table :test 'eq) :type hash-table)
  ;; The list of packages represented in the cache:
  ;; 
  (indentation-cache-packages '())
  ;; The communication style used.
  ;; 
  (communication-style nil :type (member nil :spawn :sigio :fd-handler))
  )

(defun print-connection (conn stream depth)
  (declare (ignore depth))
  (print-unreadable-object (conn stream :type t :identity t)))

(defstruct (singlethreaded-connection (:include connection)
                                      (:conc-name sconn.))
  ;; The SIGINT handler we should restore when the connection is
  ;; closed.
  saved-sigint-handler
  ;; A queue of events.  Not all events can be processed in order and
  ;; we need a place to stored them.
  (event-queue '() :type list)
  ;; A counter that is incremented whenever an event is added to the
  ;; queue.  This is used to detected modifications to the event queue
  ;; by interrupts.  The counter wraps around.
  (events-enqueued 0 :type fixnum))

(defstruct (multithreaded-connection (:include connection)
                                     (:conc-name mconn.))
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them.
  reader-thread
  control-thread
  auto-flush-thread
  indentation-cache-thread
  ;; List of threads that are currently processing requests.  We use
  ;; this to find the newest/current thread for an interrupt.  In the
  ;; future we may store here (thread . request-tag) pairs so that we
  ;; can interrupt specific requests.
  (active-threads '() :type list)
  )

(defvar *emacs-connection* nil
  "The connection to Emacs currently in use.")

(defun make-connection (socket stream style)
  (let ((conn (funcall (ecase style
                         (:spawn
                          #'make-multithreaded-connection)
                         ((:sigio nil :fd-handler)
                          #'make-singlethreaded-connection))
                       :socket socket
                       :socket-io stream
                       :communication-style style)))
    (run-hook *new-connection-hook* conn)
    (send-to-sentinel `(:add-connection ,conn))
    conn))

(defslyfun ping (tag)
  tag)

(defun safe-backtrace ()
  (ignore-errors
    (call-with-debugging-environment
     (lambda () (backtrace 0 nil)))))

(define-condition slynk-error (error)
  ((backtrace :initarg :backtrace :reader slynk-error.backtrace)
   (condition :initarg :condition :reader slynk-error.condition))
  (:report (lambda (c s) (princ (slynk-error.condition c) s)))
  (:documentation "Condition which carries a backtrace."))

(defun signal-slynk-error (condition &optional (backtrace (safe-backtrace)))
  (error 'slynk-error :condition condition :backtrace backtrace))

(defvar *debug-on-slynk-protocol-error* nil
  "When non-nil invoke the system debugger on errors that were
signalled during decoding/encoding the wire protocol.  Do not set this
to T unless you want to debug slynk internals.")

(defmacro with-slynk-error-handler ((connection) &body body)
  "Close the connection on internal `slynk-error's."
  (let ((conn (gensym)))
  `(let ((,conn ,connection))
     (handler-case
         (handler-bind ((slynk-error
                         (lambda (condition)
                           (when *debug-on-slynk-protocol-error*
                             (invoke-default-debugger condition)))))
           (progn . ,body))
       (slynk-error (condition)
         (close-connection ,conn
                           (slynk-error.condition condition)
                           (slynk-error.backtrace condition)))))))

(defmacro with-panic-handler ((connection) &body body)
  "Close the connection on unhandled `serious-condition's."
  (let ((conn (gensym)))
    `(let ((,conn ,connection))
       (handler-bind ((serious-condition
                        (lambda (condition)
                          (close-connection ,conn condition (safe-backtrace))
                          (abort condition))))
         . ,body))))

(add-hook *new-connection-hook* 'notify-backend-of-connection)
(defun notify-backend-of-connection (connection)
  (declare (ignore connection))
  (emacs-connected))


;;;; Utilities

;; stolen from Hunchentoot
(defmacro defvar-unbound (name &optional (doc-string ""))
  "Convenience macro to declare unbound special variables with a
documentation string."
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc-string)
     ',name))


;;;;; Logging

(defvar *slynk-io-package*
  (let ((package (make-package :slynk-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defvar *log-events* nil)
(defvar *log-output* nil) ; should be nil for image dumpers

(defun init-log-output ()
  (unless *log-output*
    (setq *log-output* (real-output-stream *error-output*))))

(add-hook *after-init-hook* 'init-log-output)

(defun real-input-stream (stream)
  (typecase stream
    (synonym-stream
     (real-input-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-input-stream (two-way-stream-input-stream stream)))
    (t stream)))

(defun real-output-stream (stream)
  (typecase stream
    (synonym-stream
     (real-output-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-output-stream (two-way-stream-output-stream stream)))
    (t stream)))

(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-pretty* nil)
          (*package* *slynk-io-package*))
      (when *enable-event-history*
        (setf (aref *event-history* *event-history-index*)
              (format nil "~?" format-string args))
        (setf *event-history-index*
              (mod (1+ *event-history-index*) (length *event-history*))))
      (when *log-events*
        (write-string (escape-non-ascii (format nil "~?" format-string args))
                      *log-output*)
        (force-output *log-output*)))))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun clear-event-history ()
  (fill *event-history* nil)
  (setq *event-history-index* 0))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t
         (write-string
          (escape-non-ascii (format nil "Unexpected event: ~A~%" event))
          stream))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c)
  (<= (char-code c) 127))


;;;;; Helper macros

(defmacro destructure-case (value &body patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands
                               ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))



;;; Channels

(defmacro channels () `(connection-channels *emacs-connection*))
(defmacro channel-counter () `(connection-channel-counter *emacs-connection*))

(defclass channel ()
  ((id     :initform (incf (channel-counter))
           :reader channel-id)
   (thread :initarg :thread :initform (current-thread)
           :reader channel-thread)
   (name   :initarg :name   :initform nil)))

(defmethod initialize-instance :after ((ch channel) &key)
  ;; FIXME: slightly fugly, but I need this to be able to name the
  ;; thread according to the channel's id.
  ;;
  (with-slots (thread) ch
    (when (use-threads-p)
      (setf thread (spawn-channel-thread *emacs-connection* ch)))
    (slynk-backend:send thread `(:serve-channel ,ch)))
  (setf (channels) (nconc (channels) (list ch))))

(defmethod print-object ((c channel) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (id name) c
      (format stream "~d ~a" id name))))

(defmethod drop-unprocessed-events (channel)
  ;; FIXME: perhaps this should incorporate most
  ;; behaviour from it's :after spec currently in slynk-mrepl.lisp)
  (declare (ignore channel)))

(defun find-channel (id)
  (find id (channels) :key #'channel-id))

(defun find-channel-thread (channel)
  (channel-thread channel))

(defun channel-thread-id (channel)
  (slynk-backend:thread-id (channel-thread channel)))

(defmethod close-channel (channel &key)
  (let ((probe (find-channel (channel-id channel))))
    (cond (probe (setf (channels) (delete probe (channels))))
          (t (error "Can't close invalid channel: ~a" channel)))))

(defgeneric channel-send (channel selector args)
  (:documentation "Send to CHANNEL the message SELECTOR with ARGS."))

(defmacro define-channel-method (selector (channel &rest args) &body body)
  `(defmethod channel-send (,channel (selector (eql ',selector)) args)
     (destructuring-bind ,args args
       . ,body)))

(define-channel-method :teardown ((c channel))
  (if (use-threads-p)
      ;; eventually calls CLOSE-CHANNEL
      (throw 'stop-processing 'listener-teardown)
      (close-channel c)))

(defun send-to-remote-channel (channel-id msg)
  (send-to-emacs `(:channel-send ,channel-id ,msg)))


;;; Listeners
(defclass listener ()
  ((out :initarg :out :type stream :reader listener-out)
   (in  :initarg :in :type stream :reader listener-in)
   (env)))

(defmacro listeners () `(connection-listeners *emacs-connection*))

(defmethod initialize-instance :after ((l listener) &key initial-env) 
  (with-slots (out in env) l
    (let ((io (make-two-way-stream in out)))
      (setf env
            (append
             initial-env
             `((cl:*standard-output* . ,out)
               (cl:*standard-input*  . ,in)
               (cl:*trace-output*    . ,out)
               (cl:*error-output*    . ,out)
               (cl:*debug-io*        . ,io)
               (cl:*query-io*        . ,io)
               (cl:*terminal-io*     . ,io)))))
    (assert out nil "Must have an OUT stream")
    (assert in nil "Must have an IN stream")
    (assert env nil "Must have an ENV"))
  (setf (listeners) (nconc (listeners)
                           (list l))))

(defun call-with-listener (listener fn &optional saving)
  (with-slots (env) listener
    (with-bindings env
      (unwind-protect (funcall fn)
        (when saving
          (loop for binding in env
                do (setf (cdr binding) (symbol-value (car binding)))))))))

(defmacro with-listener-bindings (listener &body body)
  "Execute BODY inside LISTENER's environment"
  `(call-with-listener ,listener (lambda () ,@body)))

(defmacro saving-listener-bindings (listener &body body)
  "Execute BODY inside LISTENER's environment, update it afterwards."
  `(call-with-listener ,listener (lambda () ,@body) 'saving))

(defmacro with-default-listener ((connection) &body body)
  "Execute BODY with in CONNECTION's default listener."
  (let ((listener-sym (gensym))
        (body-fn-sym (gensym)))
    `(let ((,listener-sym (default-listener ,connection))
           (,body-fn-sym #'(lambda () ,@body)))
       (if ,listener-sym
           (with-listener-bindings ,listener-sym
             (funcall ,body-fn-sym))
           (funcall ,body-fn-sym)))))

(defun default-listener (connection)
  (first (connection-listeners connection)))

(defun flush-listener-streams (listener)
  (with-slots (in out) listener
    (force-output out)
    #-armedbear
    (slynk-gray::reset-stream-line-column out)
    (clear-input in)))

(defmethod close-listener (l)
  (with-slots (in out) l (close in) (close out))
  (setf (listeners) (delete l (listeners))))


;;;; Interrupt handling

;; Usually we'd like to enter the debugger when an interrupt happens.
;; But for some operations, in particular send&receive, it's crucial
;; that those are not interrupted when the mailbox is in an
;; inconsistent/locked state. Obviously, if send&receive don't work we
;; can't communicate and the debugger will not work.  To solve that
;; problem, we try to handle interrupts only at certain safe-points.
;;
;; Whenever an interrupt happens we call the function
;; INVOKE-OR-QUEUE-INTERRUPT.  Usually this simply invokes the
;; debugger, but if interrupts are disabled the interrupt is put in a
;; queue for later processing.  At safe-points, we call
;; CHECK-SLY-INTERRUPTS which looks at the queue and invokes the
;; debugger if needed.
;;
;; The queue for interrupts is stored in a thread local variable.
;; WITH-CONNECTION sets it up.  WITH-SLY-INTERRUPTS allows
;; interrupts, i.e. the debugger is entered immediately.  When we call
;; "user code" or non-problematic code we allow interrupts.  When
;; inside WITHOUT-SLY-INTERRUPTS, interrupts are queued.  When we
;; switch from "user code" to more delicate operations we need to
;; disable interrupts.  In particular, interrupts should be disabled
;; for SEND and RECEIVE-IF.

;; If true execute interrupts, otherwise queue them.
;; Note: `with-connection' binds *pending-sly-interrupts*.
(defvar *sly-interrupts-enabled*)

(defmacro with-interrupts-enabled% (flag body)
  `(progn
     ,@(if flag '((check-sly-interrupts)))
     (multiple-value-prog1
         (let ((*sly-interrupts-enabled* ,flag))
           ,@body)
       ,@(if flag '((check-sly-interrupts))))))

(defmacro with-sly-interrupts (&body body)
  `(with-interrupts-enabled% t ,body))

(defmacro without-sly-interrupts (&body body)
  `(with-interrupts-enabled% nil ,body))

(defun queue-thread-interrupt (thread function)
  (interrupt-thread thread
                    (lambda ()
                      ;; safely interrupt THREAD
                      (when (invoke-or-queue-interrupt function)
                        (wake-thread thread)))))

(defun invoke-or-queue-interrupt (function)
  (log-event "invoke-or-queue-interrupt: ~a~%" function)
  (cond ((not (boundp '*sly-interrupts-enabled*))
         (without-sly-interrupts
           (funcall function)))
        (*sly-interrupts-enabled*
         (log-event "interrupts-enabled~%")
         (funcall function))
        (t
         (setq *pending-sly-interrupts*
               (nconc *pending-sly-interrupts*
                      (list function)))
         (cond ((cdr *pending-sly-interrupts*)
                (log-event "too many queued interrupts~%")
                (with-simple-restart (continue "Continue from interrupt")
                  (handler-bind ((serious-condition #'invoke-sly-debugger))
                    (check-sly-interrupts))))
               (t
                (log-event "queue-interrupt: ~a~%" function)
                (when *interrupt-queued-handler*
                  (funcall *interrupt-queued-handler*))
                t)))))

;; Thread local variable used for flow-control.
;; It's bound by `with-connection'.
(defvar *send-counter*)

(defmacro with-connection ((connection) &body body)
  "Execute BODY in the context of CONNECTION."
  `(let ((connection ,connection)
         (function (lambda () . ,body)))
     (if (eq *emacs-connection* connection)
         (funcall function)
         (let ((*emacs-connection* connection)
               (*pending-sly-interrupts* '())
               (*send-counter* 0))
           (without-sly-interrupts
             (with-slynk-error-handler (connection)
               (with-default-listener (connection)
                 (call-with-debugger-hook #'slynk-debugger-hook
                                          function))))))))

(defun call-with-retry-restart (msg thunk)
  (loop (with-simple-restart (retry "~a" msg)
          (return (funcall thunk)))))

(defmacro with-retry-restart ((&key (msg "Retry.")) &body body)
  (check-type msg string)
  `(call-with-retry-restart ,msg (lambda () ,@body)))


;;;;; Sentinel
;;;
;;; The sentinel thread manages some global lists.
;;; FIXME: Overdesigned?

(defvar *connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *servers* '()
  "A list ((server-socket port thread) ...) describing the listening sockets.
Used to close sockets on server shutdown or restart.")

;; FIXME: we simply access the global variable here.  We could ask the
;; sentinel thread instead but then we still have the problem that the
;; connection could be closed before we use it.
(defun default-connection ()
  "Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."
  (car *connections*))

(defun start-sentinel ()
  (unless (find-registered 'sentinel)
    (let ((thread (spawn #'sentinel :name "Slynk Sentinel")))
      (register-thread 'sentinel thread))))

(defun sentinel ()
  (catch 'exit-sentinel
    (loop (sentinel-serve (receive)))))

(defun send-to-sentinel (msg)
  (let ((sentinel (find-registered 'sentinel)))
    (cond (sentinel (send sentinel msg))
          (t (sentinel-serve msg)))))

(defun sentinel-serve (msg)
  (destructure-case msg
    ((:add-connection conn)
     (push conn *connections*))
    ((:close-connection connection condition backtrace)
     (close-connection% connection condition backtrace)
     (sentinel-maybe-exit))
    ((:add-server socket port thread)
     (push (list socket port thread) *servers*))
    ((:stop-server key port)
     (sentinel-stop-server key port)
     (sentinel-maybe-exit))))

(defun sentinel-stop-server (key value)
  (let ((probe (find value *servers* :key (ecase key
                                            (:socket #'car)
                                            (:port #'cadr)))))
    (cond (probe
           (setq *servers* (delete probe *servers*))
           (destructuring-bind (socket _port thread) probe
             (declare (ignore _port))
             (ignore-errors (close-socket socket))
             (when (and thread
                        (thread-alive-p thread)
                        (not (eq thread (current-thread))))
               (ignore-errors (kill-thread thread)))))
          (t
           (warn "No server for ~s: ~s" key value)))))

(defun sentinel-maybe-exit ()
  (when (and (null *connections*)
             (null *servers*)
             (and (current-thread)
                  (eq (find-registered 'sentinel)
                      (current-thread))))
    (register-thread 'sentinel nil)
    (throw 'exit-sentinel nil)))


;;;;; Misc

(defun use-threads-p ()
  (eq (connection-communication-style *emacs-connection*) :spawn))

(defun current-thread-id ()
  (thread-id (current-thread)))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))


;;;;; Symbols

;; FIXME: this docstring is more confusing than helpful.
(defun symbol-status (symbol &optional (package (symbol-package symbol)))
  "Returns one of

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is

     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)

        You can check that with: (AND (SYMBOL-STATUS S P) T)


     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.

                   Or more simply, if S is not _inherited_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS
                                        (NOT (EQ STATUS :INHERITED))))


     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.

                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.

        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)


     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS
                                        (NOT (EQ STATUS :EXTERNAL))))


        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does.

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited
        from other packages, but only about the symbols directly specific
        to the package in question.
"
  (when package     ; may be NIL when symbol is completely uninterned.
    (check-type symbol symbol) (check-type package package)
    (multiple-value-bind (present-symbol status)
        (find-symbol (symbol-name symbol) package)
      (and (eq symbol present-symbol) status))))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (eq (symbol-status symbol package) :external))

(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :CONSTANT, :GENERIC-FUNCTION,
:TYPESPEC, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (flet ((type-specifier-p (s)
           (or (documentation s 'type)
               (not (eq (type-specifier-arglist s) :not-available)))))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol)
                                                  :constant :boundp) result))
      (when (fboundp symbol)            (push :fboundp result))
      (when (type-specifier-p symbol)   (push :typespec result))
      (when (find-class symbol nil)     (push :class result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (and (fboundp symbol)
                 (typep (ignore-errors (fdefinition symbol))
                        'generic-function))
        (push :generic-function result))
      result)))


;;;; TCP Server

(defvar *communication-style* (preferred-communication-style))

(defvar *dont-close* nil
  "Default value of :dont-close argument to start-server and
  create-server.")

(defparameter *loopback-interface* "localhost")

(defun start-server (port-file
                     &key (style *communication-style*)
                       (dont-close *dont-close*))
  "Start the server and write the listen port number to PORT-FILE.
This is the entry point for Emacs."
  (setq *m-x-sly-from-emacs* t)
  (setup-server 0
                (lambda (port) (announce-server-port port-file port))
                style dont-close nil))

(defun create-server (&key (port default-server-port)
                        (style *communication-style*)
                        (dont-close *dont-close*)
                        interface
                        backlog)
  "Start a SLYNK server on PORT running in STYLE.
If DONT-CLOSE is true then the listen socket will accept multiple
connections, otherwise it will be closed after the first.

Optionally, an INTERFACE could be specified and swank will bind
the PORT on this interface. By default, interface is \"localhost\"."
  (let ((*loopback-interface* (or interface
                                  *loopback-interface*)))
    (setup-server port #'simple-announce-function
                  style dont-close backlog)))

(defun find-external-format-or-lose (coding-system)
  (or (find-external-format coding-system)
      (error "Unsupported coding system: ~s" coding-system)))

(defmacro restart-loop (form &body clauses)
  "Executes FORM, with restart-case CLAUSES which have a chance to modify FORM's
environment before trying again (by returning normally) or giving up (through an
explicit transfer of control), all within an implicit block named nil.
e.g.: (restart-loop (http-request url) (use-value (new) (setq url new)))"
  `(loop (restart-case (return ,form) ,@clauses)))

(defun socket-quest (port backlog)
  "Attempt o create a socket on PORT.
Add a restart, prompting user to enter a new port if PORT is already
taken."
  (restart-loop (create-socket *loopback-interface* port :backlog backlog)
    (use-value (&optional (new-port (1+ port)))
      :report (lambda (stream) (format stream "Try a port other than ~D" port))
      :interactive
      (lambda ()
        (format *query-io* "Enter port (defaults to ~D): " (1+ port))
        (finish-output *query-io*)      ; necessary for tunnels
        (ignore-errors (list (parse-integer (read-line *query-io*)))))
      (setq port new-port))))

(defun setup-server (port announce-fn style dont-close backlog)
  (init-log-output)
  (let* ((socket (socket-quest port backlog))
         (port (local-port socket)))
    (funcall announce-fn port)
    (labels ((serve () (accept-connections socket style dont-close))
             (note () (send-to-sentinel `(:add-server ,socket ,port
                                                      ,(current-thread))))
             (serve-loop () (note) (loop do (serve) while dont-close)))
      (ecase style
        (:spawn (initialize-multiprocessing
                 (lambda ()
                   (start-sentinel)
                   (spawn #'serve-loop :name (format nil "Slynk ~s" port)))))
        ((:fd-handler :sigio)
         (note)
         (add-fd-handler socket #'serve))
        ((nil) (serve-loop))))
    port))

(defun stop-server (port)
  "Stop server running on PORT."
  (send-to-sentinel `(:stop-server :port ,port)))

(defun restart-server (&key (port default-server-port)
                       (style *communication-style*)
                       (dont-close *dont-close*))
  "Stop the server listening on PORT, then start a new SLYNK server
on PORT running in STYLE. If DONT-CLOSE is true then the listen socket
will accept multiple connections, otherwise it will be closed after the
first."
  (stop-server port)
  (sleep 5)
  (create-server :port port :style style :dont-close dont-close))

(defun accept-connections (socket style dont-close)
  (unwind-protect
       (let ((client (accept-connection socket :external-format nil
                                               :buffering t)))
         (authenticate-client client)
         (serve-requests (make-connection socket client style)))
    (unless dont-close
      (send-to-sentinel `(:stop-server :socket ,socket)))))

(defun authenticate-client (stream)
  (let ((secret (sly-secret)))
    (when secret
      (set-stream-timeout stream 20)
      (let ((first-val (read-packet stream)))
        (unless (and (stringp first-val) (string= first-val secret))
          (error "Incoming connection doesn't know the password.")))
      (set-stream-timeout stream nil))))

(defun sly-secret ()
  "Finds the magic secret from the user's home directory.  Returns nil
if the file doesn't exist; otherwise the first line of the file."
  (with-open-file (in
                   (merge-pathnames (user-homedir-pathname) #p".sly-secret")
                   :if-does-not-exist nil)
    (and in (read-line in nil ""))))

(defun serve-requests (connection)
  "Read and process all requests on connections."
  (etypecase connection
    (multithreaded-connection
     (spawn-threads-for-connection connection))
    (singlethreaded-connection
     (ecase (connection-communication-style connection)
       ((nil) (simple-serve-requests connection))
       (:sigio (install-sigio-handler connection))
       (:fd-handler (install-fd-handler connection))))))

(defun stop-serving-requests (connection)
  (etypecase connection
    (multithreaded-connection
     (cleanup-connection-threads connection))
    (singlethreaded-connection
     (ecase (connection-communication-style connection)
       ((nil))
       (:sigio (deinstall-sigio-handler connection))
       (:fd-handler (deinstall-fd-handler connection))))))

(defun announce-server-port (file port)
  (with-open-file (s file
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (format s "~S~%" port))
  (simple-announce-function port))

(defun simple-announce-function (port)
  (when *slynk-debug-p*
    (format *log-output* "~&;; Slynk started at port: ~D.~%" port)
    (force-output *log-output*)))


;;;;; Event Decoding/Encoding

(defun decode-message (stream)
  "Read an S-expression from STREAM using the SLY protocol."
  (log-event "decode-message~%")
  (without-sly-interrupts
    (handler-bind ((error #'signal-slynk-error))
      (handler-case (read-message stream *slynk-io-package*)
        (slynk-reader-error (c)
          `(:reader-error ,(slynk-reader-error.packet c)
                          ,(slynk-reader-error.cause c)))))))

(defun encode-message (message stream)
  "Write an S-expression to STREAM using the SLY protocol."
  (log-event "encode-message~%")
  (without-sly-interrupts
    (handler-bind ((error #'signal-slynk-error))
      (write-message message *slynk-io-package* stream))))


;;;;; Event Processing

(defvar *sly-db-quit-restart* nil
  "The restart that will be invoked when the user calls sly-db-quit.")

;; Establish a top-level restart and execute BODY.
;; Execute K if the restart is invoked.
(defmacro with-top-level-restart ((connection k) &body body)
  `(with-connection (,connection)
     (restart-case
         (let ((*sly-db-quit-restart* (find-restart 'abort)))
           ,@body)
       (abort (&optional v)
         :report "Return to SLY's top level."
         (declare (ignore v))
         (force-user-output)
         ,k))))

(defun handle-requests (connection &optional timeout)
  "Read and process :emacs-rex requests.
The processing is done in the extent of the toplevel restart."
  (with-connection (connection)
    (cond (*sly-db-quit-restart*
           (process-requests timeout))
          (t
           (tagbody
            start
              (with-top-level-restart (connection (go start))
                (process-requests timeout)))))))

(defvar-unbound *channel*
  "Current CHANNEL instance used by :EMACS-CHANNEL-SEND messages.")

(defun process-requests (timeout)
  "Read and process requests from Emacs.
TIMEOUT has the same meaning as in WAIT-FOR-EVENT."
  (catch 'stop-processing
    (loop
      (multiple-value-bind (event timed-out-p)
          (wait-for-event `(or (:emacs-rex . _)
                               (:emacs-channel-send . _))
                          timeout)
        (when timed-out-p (return))
        (destructure-case event
          ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
          ((:emacs-channel-send *channel* (selector &rest args))
           (channel-send *channel* selector args)))))))

(defun spawn-channel-thread (connection channel)
  "Spawn a listener thread for CONNECTION and CHANNEL.

The new thread will block waiting for a :SERVE-CHANNEL message, then
process all requests in series until the :TEARDOWN message, at which
point the thread terminates and CHANNEL is closed."
  (slynk-backend:spawn
   (lambda ()
     (with-connection (connection)
       (unwind-protect
            (destructure-case
                (slynk-backend:receive)
              ((:serve-channel c)
               (assert (eq c channel))
               (loop
                 (with-top-level-restart (connection
                                          (drop-unprocessed-events channel))
                   (when (eq (process-requests nil)
                             'listener-teardown)
                     (return))))))
         (close-channel channel))))
   :name (with-slots (id name) channel
           (format nil "sly-channel-~a-~a" id name))))


(defun current-socket-io ()
  (connection-socket-io *emacs-connection*))

(defun close-connection (connection condition backtrace)
  (send-to-sentinel `(:close-connection ,connection ,condition ,backtrace)))

(defun close-connection% (c condition backtrace)
  (let ((*debugger-hook* nil))
    (log-event "close-connection: ~a ...~%" condition)
    (format *log-output* "~&;; slynk:close-connection: ~A~%"
            (escape-non-ascii (safe-condition-message condition)))
    (let ((*emacs-connection* c))
      (format *log-output* "~&;; closing ~a channels~%" (length (connection-channels c)))
      (mapc #'(lambda (c) (close-channel c :force t)) (connection-channels c))
      (format *log-output* "~&;; closing ~a listeners~%" (length (connection-listeners c)))
      (ignore-errors
       (mapc #'close-listener (connection-listeners c))))
    (stop-serving-requests c)
    (close (connection-socket-io c))
    (setf *connections* (remove c *connections*))
    (run-hook *connection-closed-hook* c)
    (when (and condition (not (typep condition 'end-of-file)))
      (finish-output *log-output*)
      (format *log-output* "~&;; Event history start:~%")
      (dump-event-history *log-output*)
      (format *log-output* "~
;; Event history end.~%~
;; Backtrace:~%~{~A~%~}~
;; Connection to Emacs lost. [~%~
;;  condition: ~A~%~
;;  type: ~S~%~
;;  style: ~S]~%"
              (loop for (i f) in backtrace
                    collect
                    (ignore-errors
                     (format nil "~d: ~a" i (escape-non-ascii f))))
              (escape-non-ascii (safe-condition-message condition) )
              (type-of condition)
              (connection-communication-style c)))
    (finish-output *log-output*)
    (log-event "close-connection ~a ... done.~%" condition)))

;;;;;; Thread based communication

(defun read-loop (connection)
  (let ((input-stream (connection-socket-io connection))
        (control-thread (mconn.control-thread connection)))
    (with-slynk-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defun dispatch-loop (connection)
  (let ((*emacs-connection* connection))
    (with-panic-handler (connection)
      (loop (dispatch-event connection (receive))))))

(defgeneric thread-for-evaluation (connection id)
  (:documentation "Find or create a thread to evaluate the next request.")
  (:method ((connection multithreaded-connection) (id (eql t)))
    (spawn-worker-thread connection))
  (:method ((connection multithreaded-connection) (id (eql :find-existing)))
    (car (mconn.active-threads connection)))
  (:method (connection (id integer))
    (declare (ignorable connection))
    (find-thread id))
  (:method ((connection singlethreaded-connection) id)
    (declare (ignorable connection connection id))
    (current-thread)))

(defun interrupt-worker-thread (connection id)
  (let ((thread (thread-for-evaluation connection
                                       (cond ((eq id t) :find-existing)
                                             (t id)))))
    (log-event "interrupt-worker-thread: ~a ~a~%" id thread)
    (if thread
        (etypecase connection
          (multithreaded-connection
           (queue-thread-interrupt thread #'simple-break))
          (singlethreaded-connection
           (simple-break)))
        (encode-message (list :debug-condition (current-thread-id)
                              (format nil "Thread with id ~a not found"
                                      id))
                        (current-socket-io)))))

(defun spawn-worker-thread (connection)
  (spawn (lambda ()
           (with-bindings *default-worker-thread-bindings*
             (with-top-level-restart (connection nil)
               (let ((thread (current-thread)))
                 (unwind-protect
                      (apply #'eval-for-emacs
                             (cdr (wait-for-event `(:emacs-rex . _))))
                   (remove-active-thread connection thread))))))
         :name "slynk-worker"))

(defun add-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection
     (push thread (mconn.active-threads connection)))
    (singlethreaded-connection)))

(defun remove-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection
     (setf (mconn.active-threads connection)
           (delete thread (mconn.active-threads connection) :count 1)))
    (singlethreaded-connection)))

(defun dispatch-event (connection event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "dispatch-event: ~s~%" event)
  (destructure-case event
    ((:emacs-rex form package thread-id id &rest extra-rex-options)
     (let ((thread (thread-for-evaluation connection thread-id)))
       (cond (thread
              (add-active-thread connection thread)
              (send-event thread `(:emacs-rex ,form ,package ,id ,@extra-rex-options)))
             (t
              (encode-message
               (list :invalid-rpc id
                     (format nil "Thread not found: ~s" thread-id))
               (current-socket-io))))))
    ((:return thread &rest args)
     (declare (ignore thread))
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread connection thread-id))
    (((:write-string
       :debug :debug-condition :debug-activate :debug-return :channel-send
       :presentation-start :presentation-end
       :new-package :new-features :ed :indentation-update
       :eval :eval-no-wait :background-message :inspect :ping
       :y-or-n-p :read-from-minibuffer :read-string :read-aborted :test-delay)
      &rest _)
     (declare (ignore _))
     (encode-message event (current-socket-io)))
    (((:emacs-pong :emacs-return :emacs-return-string) thread-id &rest args)
     (send-event (find-thread thread-id) (cons (car event) args)))
    ((:emacs-channel-send channel-id msg)
     (let* ((ch (find-channel channel-id))
            (thread (and ch (find-channel-thread ch))))
       (cond ((and ch thread)
              (send-event thread `(:emacs-channel-send ,ch ,msg)))
             (ch
              (encode-message 
               (list :invalid-channel channel-id
                     "No suitable threads for channel")
               (current-socket-io)))
             (t
              (encode-message 
               (list :invalid-channel channel-id "Channel not found")
               (current-socket-io))))))
    ((:reader-error packet condition)
     (encode-message `(:reader-error ,packet
                                     ,(safe-condition-message condition))
                     (current-socket-io)))))


(defun send-event (thread event)
  (log-event "send-event: ~s ~s~%" thread event)
  (let ((c *emacs-connection*))
    (etypecase c
      (multithreaded-connection
       (send thread event))
      (singlethreaded-connection
       (setf (sconn.event-queue c) (nconc (sconn.event-queue c) (list event)))
       (setf (sconn.events-enqueued c) (mod (1+ (sconn.events-enqueued c))
                                            most-positive-fixnum))))))

(defun send-to-emacs (event)
  "Send EVENT to Emacs."
  ;;(log-event "send-to-emacs: ~a" event)
  (without-sly-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (send (mconn.control-thread c) event))
        (singlethreaded-connection
         (dispatch-event c event)))
      (maybe-slow-down))))

(defun make-thread-bindings-aware-lambda (fn)
  (let ((connection *emacs-connection*)
        (send-counter *send-counter*))
    (lambda (&rest args)
      (let ((*emacs-connection* connection)
            (*send-counter* send-counter))
        (apply fn args)))))


;;;;;; Flow control

;; After sending N (usually 100) messages we slow down and ping Emacs
;; to make sure that everything we have sent so far was received.

(defconstant send-counter-limit 100)

(defun maybe-slow-down ()
  (let ((counter (incf *send-counter*)))
    (when (< send-counter-limit counter)
      (setf *send-counter* 0)
      (ping-pong))))

(defun ping-pong ()
  (let* ((tag (make-tag))
         (pattern `(:emacs-pong ,tag)))
    (send-to-emacs `(:ping ,(current-thread-id) ,tag))
    (wait-for-event pattern)))


(defun wait-for-event (pattern &optional timeout)
  "Scan the event queue for PATTERN and return the event.
If TIMEOUT is NIL wait until a matching event is enqued.
If TIMEOUT is T only scan the queue without waiting.
The second return value is t if the timeout expired before a matching
event was found."
  (log-event "wait-for-event: ~s ~s~%" pattern timeout)
  (without-sly-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (receive-if (lambda (e) (event-match-p e pattern)) timeout))
        (singlethreaded-connection
         (wait-for-event/event-loop c pattern timeout))))))

(defun wait-for-event/event-loop (connection pattern timeout)
  (assert (or (not timeout) (eq timeout t)))
  (loop
   (check-sly-interrupts)
   (let ((event (poll-for-event connection pattern)))
     (when event (return (car event))))
   (let ((events-enqueued (sconn.events-enqueued connection))
         (ready (wait-for-input (list (current-socket-io)) timeout)))
     (cond ((and timeout (not ready))
            (return (values nil t)))
           ((or (/= events-enqueued (sconn.events-enqueued connection))
                (eq ready :interrupt))
            ;; rescan event queue, interrupts may enqueue new events
            )
           (t
            (assert (equal ready (list (current-socket-io))))
            (dispatch-event connection
                            (decode-message (current-socket-io))))))))

(defun poll-for-event (connection pattern)
  (let* ((c connection)
         (tail (member-if (lambda (e) (event-match-p e pattern))
                          (sconn.event-queue c))))
    (when tail
      (setf (sconn.event-queue c)
            (nconc (ldiff (sconn.event-queue c) tail) (cdr tail)))
      tail)))

;;; FIXME: Make this use SLYNK-MATCH.
(defun event-match-p (event pattern)
  (cond ((or (keywordp pattern) (numberp pattern) (stringp pattern)
	     (member pattern '(nil t)))
	 (equal event pattern))
	((symbolp pattern) t)
	((consp pattern)
         (case (car pattern)
           ((or) (some (lambda (p) (event-match-p event p)) (cdr pattern)))
           (t (and (consp event)
                   (and (event-match-p (car event) (car pattern))
                        (event-match-p (cdr event) (cdr pattern)))))))
        (t (error "Invalid pattern: ~S" pattern))))



(defun spawn-threads-for-connection (connection)
  (setf
   (mconn.control-thread connection)
   (spawn
    (lambda ()
      "Spawns a reader and indentation threads, then calls DISPATCH-LOOP."
      (setf (mconn.reader-thread connection) (spawn (lambda () (read-loop connection))
                                                    :name "reader-thread"))
      (setf (mconn.indentation-cache-thread connection)
            (spawn (lambda () (indentation-cache-loop connection))
                   :name "slynk-indentation-cache-thread"))
      (dispatch-loop connection))
    :name "control-thread"))
  connection)

(defun cleanup-connection-threads (connection)
  (let* ((c connection)
         (threads (list (mconn.reader-thread c)
                        (mconn.control-thread c)
                        (mconn.auto-flush-thread c)
                        (mconn.indentation-cache-thread c))))
    (dolist (thread threads)
      (when (and thread
                 (thread-alive-p thread)
                 (not (equal (current-thread) thread)))
        (ignore-errors (kill-thread thread))))))

;;;;;; Signal driven IO

(defun install-sigio-handler (connection)
  (add-sigio-handler (connection-socket-io connection)
                     (lambda () (process-io-interrupt connection)))
  (handle-requests connection t))

(defvar *io-interupt-level* 0)

(defun process-io-interrupt (connection)
  (log-event "process-io-interrupt ~d ...~%" *io-interupt-level*)
  (let ((*io-interupt-level* (1+ *io-interupt-level*)))
    (invoke-or-queue-interrupt
     (lambda () (handle-requests connection t))))
  (log-event "process-io-interrupt ~d ... done ~%" *io-interupt-level*))

(defun deinstall-sigio-handler (connection)
  (log-event "deinstall-sigio-handler...~%")
  (remove-sigio-handlers (connection-socket-io connection))
  (log-event "deinstall-sigio-handler...done~%"))

;;;;;; SERVE-EVENT based IO

(defun install-fd-handler (connection)
  (add-fd-handler (connection-socket-io connection)
                  (lambda () (handle-requests connection t)))
  (setf (sconn.saved-sigint-handler connection)
        (install-sigint-handler
         (lambda ()
           (invoke-or-queue-interrupt
            (lambda () (dispatch-interrupt-event connection))))))
  (handle-requests connection t))

(defun dispatch-interrupt-event (connection)
  (with-connection (connection)
    (dispatch-event connection `(:emacs-interrupt ,(current-thread-id)))))

(defun deinstall-fd-handler (connection)
  (log-event "deinstall-fd-handler~%")
  (remove-fd-handlers (connection-socket-io connection))
  (install-sigint-handler (sconn.saved-sigint-handler connection)))

;;;;;; Simple sequential IO

(defun simple-serve-requests (connection)
  (unwind-protect
       (with-connection (connection)
         (call-with-user-break-handler
          (lambda ()
            (invoke-or-queue-interrupt
             (lambda () (dispatch-interrupt-event connection))))
          (lambda ()
            (with-simple-restart (close-connection "Close SLY connection.")
              (let* ((stdin (real-input-stream *standard-input*))
                     (*standard-input* (make-repl-input-stream connection
                                                               stdin)))
                (tagbody toplevel
                   (with-top-level-restart (connection (go toplevel))
                     (simple-repl))))))))
    (close-connection connection nil (safe-backtrace))))

;; this is signalled when our custom stream thinks the end-of-file is reached.
;; (not when the end-of-file on the socket is reached)
(define-condition end-of-repl-input (end-of-file) ())

(defun simple-repl ()
  (loop
   (format t "~a> " (package-string-for-prompt *package*))
   (force-output)
   (let ((form (handler-case (read)
                 (end-of-repl-input () (return)))))
     (let* ((- form)
            (values (multiple-value-list (eval form))))
       (setq *** **  ** *  * (car values)
             /// //  // /  / values
             +++ ++  ++ +  + form)
       (cond ((null values) (format t "; No values~&"))
             (t (mapc (lambda (v) (format t "~s~&" v)) values)))))))

(defun make-repl-input-stream (connection stdin)
  (make-input-stream
   (lambda () (repl-input-stream-read connection stdin))))

(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection-socket-io connection))
          (inputs (list socket stdin))
          (ready (wait-for-input inputs)))
     (cond ((eq ready :interrupt)
            (check-sly-interrupts))
           ((member socket ready)
            ;; A Sly request from Emacs is pending; make sure to
            ;; redirect IO to the REPL buffer.
            (with-simple-restart (process-input "Continue reading input.")
              (let ((*sly-db-quit-restart* (find-restart 'process-input)))
                (with-default-listener (connection)
                  (handle-requests connection t)))))
           ((member stdin ready)
            ;; User typed something into the  *inferior-lisp* buffer,
            ;; so do not redirect.
            (return (read-non-blocking stdin)))
           (t (assert (null ready)))))))

(defun read-non-blocking (stream)
  (with-output-to-string (str)
    (handler-case
        (loop (let ((c (read-char-no-hang stream)))
                (unless c (return))
                (write-char c str)))
      (end-of-file () (error 'end-of-repl-input :stream stream)))))



(defvar *sly-features* nil
  "The feature list that has been sent to Emacs.")

(defun send-oob-to-emacs (object)
  (send-to-emacs object))

(defun force-user-output ()
  (with-default-listener (*emacs-connection*)
    (force-output *standard-output*)))

(add-hook *pre-reply-hook* 'force-user-output)

(defun clear-user-input  ()
  (with-default-listener (*emacs-connection*)
    (clear-input *standard-input*)))

;; FIXME: not thread safe.
(defvar *tag-counter* 0)

(defun make-tag ()
  (setq *tag-counter* (mod (1+ *tag-counter*) (expt 2 22))))

(defun y-or-n-p-in-emacs (format-string &rest arguments)
  "Like y-or-n-p, but ask in the Emacs minibuffer."
  (let ((tag (make-tag))
        (question (apply #'format nil format-string arguments)))
    (force-output)
    (send-to-emacs `(:y-or-n-p ,(current-thread-id) ,tag ,question))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defun read-from-minibuffer-in-emacs (prompt &optional initial-value)
  "Ask user a question in Emacs' minibuffer. Returns \"\" when user
entered nothing, returns NIL when user pressed C-g."
  (check-type prompt string) (check-type initial-value (or null string))
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:read-from-minibuffer ,(current-thread-id) ,tag
                                           ,prompt ,initial-value))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defun process-form-for-emacs (form)
  "Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."
  (etypecase form
    (string (format nil "~S" form))
    (cons (format nil "(~A . ~A)"
                  (process-form-for-emacs (car form))
                  (process-form-for-emacs (cdr form))))
    (character (format nil "?~C" form))
    (symbol (concatenate 'string (when (eq (symbol-package form)
                                           #.(find-package "KEYWORD"))
                                   ":")
                         (string-downcase (symbol-name form))))
    (number (let ((*print-base* 10))
              (princ-to-string form)))))

(defun eval-in-emacs (form &optional nowait)
  "Eval FORM in Emacs.
`sly-enable-evaluate-in-emacs' should be set to T on the Emacs side."
  (cond (nowait
         (send-to-emacs `(:eval-no-wait ,(process-form-for-emacs form))))
        (t
         (force-output)
         (let ((tag (make-tag)))
	   (send-to-emacs `(:eval ,(current-thread-id) ,tag
				  ,(process-form-for-emacs form)))
	   (let ((value (caddr (wait-for-event `(:emacs-return ,tag result)))))
	     (destructure-case value
	       ((:ok value) value)
               ((:error kind . data) (error "~a: ~{~a~}" kind data))
	       ((:abort) (abort))))))))

(defun sly-version-string ()
  "Return a string identifying the SLY version.
Return nil if nothing appropriate is available."
  (let ((this-file #.(or *compile-file-truename* *load-truename*)))
    (with-open-file (s (make-pathname :name "sly" :type "el"
                                      :directory (butlast
                                                  (pathname-directory this-file)
                                                  1)
                                      :defaults this-file))
      (let ((seq (make-array 200 :element-type 'character :initial-element #\null)))
        (read-sequence seq s :end 200)
        (let* ((beg (search ";; Version:" seq))
               (end (position #\NewLine seq :start beg))
               (middle (position #\Space seq :from-end t :end end)))
          (subseq seq (1+ middle) end))))))

(defvar *slynk-wire-protocol-version* (ignore-errors (sly-version-string))
  "The version of the slynk/sly communication protocol.")

(defslyfun connection-info ()
  "Return a key-value list of the form:
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION PROGRAM)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"
  (let ((c *emacs-connection*))
    (setq *sly-features* *features*)
    `(:pid ,(getpid) :style ,(connection-communication-style c)
      :encoding (:coding-systems
                 ,(loop for cs in '("utf-8-unix" "iso-latin-1-unix")
                        when (find-external-format cs) collect cs))
      :lisp-implementation (:type ,(lisp-implementation-type)
                            :name ,(lisp-implementation-type-name)
                            :version ,(lisp-implementation-version)
                            :program ,(lisp-implementation-program))
      :machine (:instance ,(machine-instance)
               :type ,(machine-type)
               :version ,(machine-version))
      :features ,(features-for-emacs)
      :modules ,*modules*
      :package (:name ,(package-name *package*)
               :prompt ,(package-string-for-prompt *package*))
      :version ,*slynk-wire-protocol-version*)))

(defun debug-on-slynk-error ()
  (assert (eq *debug-on-slynk-protocol-error* *debug-slynk-backend*))
  *debug-on-slynk-protocol-error*)

(defun (setf debug-on-slynk-error) (new-value)
  (setf *debug-on-slynk-protocol-error* new-value)
  (setf *debug-slynk-backend* new-value))

(defslyfun toggle-debug-on-slynk-error ()
  (setf (debug-on-slynk-error) (not (debug-on-slynk-error))))


;;;; Reading and printing

(defvar-unbound *buffer-package*
    "Package corresponding to sly-buffer-package.

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a sly
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(defvar-unbound *buffer-readtable*
    "Readtable associated with the current buffer")

(defmacro with-buffer-syntax ((&optional package-designator
                                         readtable)
                              &body body)
  "Conceptually execute BODY inside a SLY Lisp buffer.

Execute BODY with appropriate *PACKAGE* and *READTABLE* bindings.

PACKAGE-DESIGNATOR, if non-NIL, is anything remotely designating a
package.  READTABLE, if non-NIL, must verify CL:READTABLEP.

READTABLE defaults to *BUFFER-READTABLE* as set by
GUESS-BUFFER-READTABLE, which in turn uses a mapping in
*READTABLE-ALIST* as indexed by *BUFFER-PACKAGE*, and *not*
PACKAGE-DESIGNATOR.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  `(call-with-buffer-syntax ,package-designator ,readtable (lambda () ,@body)))

(defun call-with-buffer-syntax (package readtable fun)
  (let ((*package* (if package
                       (guess-buffer-package package)
                       *buffer-package*))
        (*buffer-readtable* (or (and (readtablep readtable)
                                     readtable)
                                *buffer-readtable*)))
    ;; Don't shadow *readtable* unnecessarily because that prevents
    ;; the user from assigning to it.
    (if (eq *readtable* *buffer-readtable*)
        (call-with-syntax-hooks fun)
        (let ((*readtable* *buffer-readtable*))
          (call-with-syntax-hooks fun)))))

(defmacro without-printing-errors ((&key object stream
                                        (msg "<<error printing object>>"))
                                  &body body)
  ;; JT: Careful when calling this, make sure STREAM, if provided, is
  ;; a symbol that alwyas designates a non-nil stream.  See gh#287.
  "Catches errors during evaluation of BODY and prints MSG instead."
  `(handler-case (progn ,@body)
     (serious-condition ()
       ,(cond ((and stream object)
               (let ((gstream (gensym "STREAM+")))
                 `(let ((,gstream ,stream))
                    (print-unreadable-object (,object ,gstream :type t
                                                      :identity t)
                      (write-string ,msg ,gstream)))))
              (stream
               `(write-string ,msg ,stream))
              (object
               `(with-output-to-string (s)
                  (print-unreadable-object (,object s :type t :identity t)
                    (write-string ,msg  s))))
              (t msg)))))

(defun to-string (object)
  "Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (without-printing-errors (:object object :stream nil)
        (prin1-to-string object)))))

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (with-buffer-syntax ()
    (let ((*read-suppress* nil))
      (values (read-from-string string)))))

(defun parse-string (string package)
  "Read STRING in PACKAGE."
  (with-buffer-syntax (package)
    (let ((*read-suppress* nil))
      (read-from-string string))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun tokenize-symbol-thoroughly (string)
  "This version of TOKENIZE-SYMBOL handles escape characters."
  (let ((package nil)
        (token (make-array (length string) :element-type 'character
                                           :fill-pointer 0))
        (backslash nil)
        (vertical nil)
        (internp nil)
        (caser (char-casifier string)))
    (loop for char across string do
          (cond
            (backslash
             (vector-push-extend char token)
             (setq backslash nil))
            ((char= char #\\) ; Quotes next character, even within |...|
             (setq backslash t))
            ((char= char #\|)
             (setq vertical (not vertical)))
            (vertical
             (vector-push-extend char token))
            ((char= char #\:)
             (cond ((and package internp)
                    (return-from tokenize-symbol-thoroughly))
                   (package
                    (setq internp t))
                   (t
                    (setq package token
                          token (make-array (length string)
                                            :element-type 'character
                                            :fill-pointer 0)))))
            (t
             (vector-push-extend (funcall caser char) token))))
    (unless vertical
          (values token package (or (not package) internp)))))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name) 	symbol-name)
        (internal-p 		(cat package-name "::" symbol-name))
        (t 			(cat package-name ":" symbol-name))))

(defun char-casifier (string)
  "Return a function which converts characters in STRING according to `readtable-case'."
  (ecase (readtable-case *readtable*)
    (:preserve #'identity)
    (:upcase   #'char-upcase)
    (:downcase #'char-downcase)
    ;; :invert only inverts the case if every character of a token is the same
    ;; case, otherwise it acts like :preserve.
    (:invert (let ((upper (count-if #'upper-case-p string)))
               (cond ((= upper 0) #'char-upcase)
                     ((= upper (length string)) #'char-downcase)
                     (t #'identity))))))


(defun find-symbol-with-status (symbol-name status
                                &optional (package *package*))
  (multiple-value-bind (symbol flag) (find-symbol symbol-name package)
    (if (and flag (eq flag status))
        (values symbol flag)
        (values nil nil))))

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."
  (multiple-value-bind (sname pname internalp)
      (tokenize-symbol-thoroughly string)
    (when sname
     (let ((package (cond ((string= pname "") +keyword-package+)
                          (pname              (find-package pname))
                          (t                  package))))
       (if package
           (multiple-value-bind (symbol flag)
               (if internalp
                   (find-symbol sname package)
                   (find-symbol-with-status sname ':external package))
             (values symbol flag sname package))
           (values nil nil nil nil))))))

(defun parse-symbol-or-lose (string &optional (package *package*))
  (multiple-value-bind (symbol status) (parse-symbol string package)
    (if status
        (values symbol status)
        (error "Unknown symbol: ~A [in ~A]" string package))))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (let ((*package* *slynk-io-package*))
                    (read-from-string string)))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

(defvar *readtable-alist* (default-readtable-alist)
  "An alist mapping package names to readtables.")

(defun guess-buffer-readtable (package-name)
  (let ((package (guess-package package-name)))
    (or (and package
             (cdr (assoc (package-name package) *readtable-alist*
                         :test #'string=)))
        *readtable*)))


;;;; Evaluation

(defvar *pending-continuations* '()
  "List of continuations for Emacs. (thread local)")

(defun guess-buffer-package (string)
  "Return a package for STRING.
Fall back to the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defvar *eval-for-emacs-wrappers* nil
  "List of functions for fine-grained control over form evaluation.
Each element must be a function taking an arbitrary number of
arguments, the first of which is a function of no arguments, call it
IN-FUNCTION, while the remaining are bound to the EXTRA-REX-OPTIONS
parameter of EVAL-FOR-EMACS.  Every function *must* return another
function of no arguments, call it OUT-FUNCTION, that, when called,
*must* call IN-FUNCTION in whatever dynamic environment it sees fit.

Slynk will go through the elements of this variable in order, passing
a function that evaluates the form coming from Emacs to the first
element until it collects the result of the last, which is finally
called with no arguments.

Be careful when changing this variable since you may mess very basic
functionality of your Slynk, including the ability to correct any
errors you make.")

(defun eval-for-emacs (form buffer-package id &rest extra-rex-options)
  "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.  Errors are trapped and
invoke our debugger.  EXTRA-REX-OPTIONS are passed to the functions of
*EVAL-FOR-EMACS-WRAPPERS*, which see."
  (let (ok result condition)
    (unwind-protect
         (let ((*buffer-package* (guess-buffer-package buffer-package))
               (*buffer-readtable* (guess-buffer-readtable buffer-package))
               (*pending-continuations* (cons id *pending-continuations*)))
           (check-type *buffer-package* package)
           (check-type *buffer-readtable* readtable)
           (handler-bind ((t (lambda (c) (setf condition c))))
             (setq result (with-sly-interrupts
                            (flet ((eval-it ()
                                     ;; APPLY would be cleaner than EVAL.
                                     ;; (setq result (apply (car form) (cdr form)))
                                     (eval form)))
                              ;; Honour *EVAL-FOR-EMACS-WRAPPERS*
                              ;; 
                              (loop for lambda = #'eval-it then
                                                           (handler-case
                                                               (apply wrapper lambda extra-rex-options)
                                                             (error (e)
                                                               (warn "~s ignoring wrapper ~a (~a)"
                                                                     'eval-for-emacs wrapper e)
                                                               lambda))
                                    for wrapper in *eval-for-emacs-wrappers*
                                    finally (return (funcall lambda)))))))
           (run-hook *pre-reply-hook*)
           (setq ok t))
      (send-to-emacs `(:return ,(current-thread)
                               ,(if ok
                                    `(:ok ,result)
                                    `(:abort ,(prin1-to-string condition)))
                               ,id)))))

(defun format-integer-length (i) (format nil "~a bit~:p" (integer-length i)))
(defun format-integer-as-hex (i)
  (unless (or (minusp i) (> (integer-length i) 64)) (format nil "#x~X" i)))
(defun format-integer-as-octal (i)
  (unless (or (minusp i) (> (integer-length i) 8)) (format nil "#o~O" i)))
(defun format-integer-as-binary (i) -128
  (unless (or (minusp i) (> (integer-length i) 8)) (format nil "#b~B" i)))
(defun format-ratio-as-float (r) (ignore-errors (format nil "~f" r)))
(defun format-as-percentage-maybe (f) (when (< 0 (abs f) 2) (format nil "~2,'0d%" (* f 100))))

(defparameter *echo-number-alist*
  '((integer . (format-integer-length format-integer-as-hex format-integer-as-octal format-integer-as-binary))
    (ratio . (format-ratio-as-float format-as-percentage-maybe))
    (float . (format-as-percentage-maybe)))
  "Alist of functions used for presenting numbers in the echo area.

Each element takes the form (TYPE . FUNCTIONS), where TYPE is a type
designator and FUNCTIONS is a list of function designators for
displaying that number in SLY. Each function takes the number as a
single argument and returns a string, or nil, if that particular
representation is to be disregarded.

Additionally if a given function chooses to return t as its optional
second value, then all the remaining functions following it in the
list are disregarded.")

(defparameter *present-number-alist* nil
  "Alist of functions used for presenting numbers the REPL.

This is an \"override\". If nil the (the alist is empty) the value of
*ECHO-NUMBER-ALIST* is used, otherwise the structure is exactly the
same as that variable.")

(defun present-number-considering-alist (number alist)
  (let* ((functions (cdr (assoc number alist :test #'typep)))
         (extra-presentations
           (loop for fn in functions
                 for (display skip)
                   = (multiple-value-list
                      (handler-case
                          (funcall fn number)
                        (error (e)
                          (declare (ignore e))
                          "<error echoing>")))
                 when display collect it
                   until skip)))
    (if extra-presentations
        (format nil "~A (~{~a~^, ~})"
                number extra-presentations)
        (format nil "~A" number))))

(defun echo-for-emacs (values &optional (fn #'slynk-pprint))
  "Format VALUES in a way suitable to be echoed in the SLY client.
May insert newlines between each of VALUES.  Considers
*ECHO-NUMBER-ALIST*."
  (let ((*print-readably* nil))
    (cond ((null values) "; No value")
          ((and (numberp (car values))
                (null (cdr values)))
           (present-number-considering-alist (car values) *echo-number-alist*))
          (t
           (let ((strings (loop for v in values
                                collect (funcall fn v))))
             (if (some #'(lambda (s) (find #\Newline s))
                       strings)
                 (format nil "~{~a~^~%~}" strings)
                 (format nil "~{~a~^, ~}" strings)))))))

(defun present-for-emacs (value &optional (fn #'slynk-pprint))
  "Format VALUE in a way suitable to be displayed in the SLY client.
FN is only used if value is not a number"
  (if (numberp value)
      (present-number-considering-alist value (or *present-number-alist*
                                                  *echo-number-alist*))
      (funcall fn value)))

(defslyfun interactive-eval (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY interactive evaluation request.")
      (let ((values (multiple-value-list (eval (from-string string)))))
        (finish-output)
        (echo-for-emacs values)))))

(defslyfun eval-and-grab-output (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY evaluation request.")
      (let* ((s (make-string-output-stream))
             (*standard-output* s)
             (values (multiple-value-list (eval (from-string string)))))
        (list (get-output-stream-string s)
              (echo-for-emacs values))))))

(defun eval-region (string)
  "Evaluate STRING.
Return the results of the last form as a list and as secondary value the
last form."
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
         (when (eq form stream)
           (finish-output)
           (return (values values -)))
         (setq - form)
         (setq values (multiple-value-list (eval form)))
         (finish-output))))))

(defslyfun interactive-eval-region (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY interactive evaluation request.")
      (echo-for-emacs (eval-region string)))))

(defslyfun re-evaluate-defvar (form)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY evaluation request.")
      (let ((form (read-from-string form)))
        (destructuring-bind (dv name &optional value doc) form
          (declare (ignore value doc))
          (assert (eq dv 'defvar))
          (makunbound name)
          (prin1-to-string (eval form)))))))

(defvar-unbound *string-elision-length*
  "Maximum length of a sring before elision by SLYNK-PPRINT.")

(defparameter *slynk-pprint-bindings*
  `((*print-pretty*           . t)
    (*print-level*            . nil)
    (*print-length*           . nil)
    (*string-elision-length*  . 200)
    (*print-circle*           . nil)
    (*print-gensym*           . t)
    (*print-readably*         . nil))
  "A list of variables bindings during pretty printing.
Used by pprint-eval.")

(defun slynk-pprint (object &key (stream nil))
  "Pretty print OBJECT to STREAM using *SLYNK-PPRINT-BINDINGS*.
If STREAM is nil, use a string"
  (with-bindings *slynk-pprint-bindings*
    ;; a failsafe for *PRINT-LENGTH* and *PRINT-LEVEL*: if they're NIL
    ;; and *PRINT-CIRCLE* is also nil we could be in trouble printing
    ;; recursive structures.
    ;;
    (let ((*print-length* (or *print-length*
                              (and (not *print-circle*) 512)))
          (*print-level* (or *print-level*
                              (and (not *print-circle*) 20))))
      (flet ((write-it (s)
               (cond ((and *string-elision-length*
                           (stringp object)
                           (> (length object) *string-elision-length*))
                      (format s "\"~a...[sly-elided string of length ~a]\""
                              (subseq object 0 *string-elision-length*)
                              (length object)))
                     (t
                      (write object :stream s :pretty t :escape t)))))
        (if stream
            (without-printing-errors (:object object :stream stream)
              (write-it stream))
            (without-printing-errors (:object object)
              (with-output-to-string (s) (write-it s))))))))

(defun slynk-pprint-values (values &key (stream nil))
  "Pretty print each of VALUES to STREAM using *SLYNK-PPRINT-BINDINGS*.
Separated by a newline. If no values indicate that in a comment.
If STREAM is nil, use a string"
  (labels ((print-one (object s)
             (let ((*slynk-pprint-bindings* nil))
               (slynk-pprint object :stream s)))
           (print-all (s)
             (loop for o in values
                   do (print-one o s)
                      (terpri))))
    (with-bindings *slynk-pprint-bindings*
      (cond ((null values)
             (format stream "; No value"))
            (t
             (if stream
                 (print-all stream)
                 (with-output-to-string (s)
                   (print-all s))))))))

(defun slynk-pprint-to-line (object)
  "Print OBJECT to a single line string and return it."
  (let ((*slynk-pprint-bindings*
          `((*print-lines* . 1)
            (*print-right-margin* . 512)
            ,@*slynk-pprint-bindings*)))
    (substitute #\Space #\Newline (slynk-pprint object :stream nil))))

(defslyfun pprint-eval (string)
  (with-buffer-syntax ()
    (let* ((s (make-string-output-stream))
           (values
            (let ((*standard-output* s)
                  (*trace-output* s))
              (multiple-value-list (eval (read-from-string string))))))
      (cat (get-output-stream-string s)
           (slynk-pprint-values values)))))

(defslyfun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p) nil "Package ~a doesn't exist." name)
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))

(defun cat (&rest strings)
  "Concatenate all arguments and make the result a string."
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string (write-string s out))
        (character (write-char s out))))))

(defun truncate-string (string width &optional ellipsis)
  (let ((len (length string)))
    (cond ((< len width) string)
          (ellipsis (cat (subseq string 0 width) ellipsis))
          (t (subseq string 0 width)))))

(defun call/truncated-output-to-string (length function
                                        &optional (ellipsis ".."))
  "Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."
  (let ((buffer (make-string (+ length (length ellipsis))))
        (fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
               (let* ((free (- length fill-pointer))
                      (count (min free (length string))))
                 (replace buffer string :start1 fill-pointer :end2 count)
                 (incf fill-pointer count)
                 (when (> (length string) free)
                   (replace buffer ellipsis :start1 fill-pointer)
                   (return-from buffer-full buffer)))))
        (let ((stream (make-output-stream #'write-output)))
          (funcall function stream)
          (finish-output stream)
          (subseq buffer 0 fill-pointer))))))

(defmacro with-string-stream ((var &key length bindings)
                              &body body)
  (cond ((and (not bindings) (not length))
         `(with-output-to-string (,var) . ,body))
        ((not bindings)
         `(call/truncated-output-to-string
           ,length (lambda (,var) . ,body)))
        (t
         `(with-bindings ,bindings
            (with-string-stream (,var :length ,length)
              . ,body)))))

(defun escape-string (string stream &key length (map '((#\" . "\\\"")
                                                       (#\\ . "\\\\"))))
  "Write STRING to STREAM surronded by double-quotes.
LENGTH -- if non-nil truncate output after LENGTH chars.
MAP -- rewrite the chars in STRING according to this alist."
  (let ((limit (or length array-dimension-limit)))
    (write-char #\" stream)
    (loop for c across string
          for i from 0 do
          (when (= i limit)
            (write-string "..." stream)
            (return))
          (let ((probe (assoc c map)))
            (cond (probe (write-string (cdr probe) stream))
                  (t (write-char c stream)))))
    (write-char #\" stream)))


;;;; Prompt

;; FIXME: do we really need 45 lines of code just to figure out the
;; prompt?

(defvar *canonical-package-nicknames*
  `((:common-lisp-user . :cl-user))
  "Canonical package names to use instead of shortest name/nickname.")
  
(defvar *auto-abbreviate-dotted-packages* t
  "Abbreviate dotted package names to their last component if T.")

(defun package-string-for-prompt (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (unparse-name
   (or (canonical-package-nickname package)
       (auto-abbreviated-package-name package)
       (shortest-package-nickname package))))

(defun canonical-package-nickname (package)
  "Return the canonical package nickname, if any, of PACKAGE."
  (let ((name (cdr (assoc (package-name package) *canonical-package-nicknames*
                          :test #'string=))))
    (and name (string name))))

(defun auto-abbreviated-package-name (package)
  "Return an abbreviated 'name' for PACKAGE.

N.B. this is not an actual package name or nickname."
  (when *auto-abbreviate-dotted-packages*
    (loop with package-name = (package-name package)
          with offset = nil
          do (let ((last-dot-pos (position #\. package-name :end offset
                                           :from-end t)))
               (unless last-dot-pos
                 (return nil))
               ;; If a dot chunk contains only numbers, that chunk most
               ;; likely represents a version number; so we collect the
               ;; next chunks, too, until we find one with meat.
               (let ((name (subseq package-name (1+ last-dot-pos) offset)))
                 (if (notevery #'digit-char-p name)
                     (return (subseq package-name (1+ last-dot-pos)))
                     (setq offset last-dot-pos)))))))

(defun shortest-package-nickname (package)
  "Return the shortest nickname of PACKAGE."
  (loop for name in (cons (package-name package) (package-nicknames package))
        for shortest = name then (if (< (length name) (length shortest))
                                   name
                                   shortest)
              finally (return shortest)))



(defslyfun ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  A function name (symbol or cons),
  NIL. "
  (flet ((canonicalize-filename (filename)
           (pathname-to-filename (or (probe-file filename) filename))))
    (let ((target
           (etypecase what
             (null nil)
             ((or string pathname)
              `(:filename ,(canonicalize-filename what)))
             ((cons (or string pathname) *)
              `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
             ((or symbol cons)
              `(:function-name ,(prin1-to-string what))))))
      (cond (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
            ((default-connection)
             (with-connection ((default-connection))
               (send-oob-to-emacs `(:ed ,target))))
            (t (error "No connection"))))))

(defslyfun inspect-in-emacs (what &key wait)
  "Inspect WHAT in Emacs. If WAIT is true (default NIL) blocks until the
inspector has been closed in Emacs."
  (flet ((send-it ()
           (let ((tag (when wait (make-tag)))
                 (thread (when wait (current-thread-id))))
             (with-buffer-syntax ()
               (reset-inspector)
               (send-oob-to-emacs `(:inspect ,(inspect-object what)
                                             ,thread
                                             ,tag)))
             (when wait
               (wait-for-event `(:emacs-return ,tag result))))))
    (cond
      (*emacs-connection*
       (send-it))
      ((default-connection)
       (with-connection ((default-connection))
         (send-it))))
    what))

(defslyfun value-for-editing (form)
  "Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."
  ;; FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
  (with-buffer-syntax ()
    (let* ((value (eval (read-from-string form)))
           (*print-length* nil))
      (prin1-to-string value))))

(defslyfun commit-edited-value (form value)
  "Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."
  (with-buffer-syntax ()
    (eval `(setf ,(read-from-string form)
            ,(read-from-string (concatenate 'string "`" value))))
    t))

(defun background-message  (format-string &rest args)
  "Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped if we are too busy with other things."
  (when *emacs-connection*
    (send-to-emacs `(:background-message
                     ,(apply #'format nil format-string args)))))

;; This is only used by the test suite.
(defun sleep-for (seconds)
  "Sleep for at least SECONDS seconds.
This is just like cl:sleep but guarantees to sleep
at least SECONDS."
  (let* ((start (get-internal-real-time))
         (end (+ start
                 (* seconds internal-time-units-per-second))))
    (loop
     (let ((now (get-internal-real-time)))
       (cond ((< end now) (return))
             (t (sleep (/ (- end now)
                          internal-time-units-per-second))))))))


;;;; Debugger

(defun invoke-sly-debugger (condition)
  "Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (without-sly-interrupts
    (cond (*emacs-connection*
           (debug-in-emacs condition))
          ((default-connection)
           (with-connection ((default-connection))
             (debug-in-emacs condition))))))

(define-condition invoke-default-debugger () ())

(defun slynk-debugger-hook (condition hook)
  "Debugger function for binding *DEBUGGER-HOOK*."
  (declare (ignore hook))
  (handler-case
      (call-with-debugger-hook #'slynk-debugger-hook
                               (lambda () (invoke-sly-debugger condition)))
    (invoke-default-debugger ()
      (invoke-default-debugger condition))))

(defun invoke-default-debugger (condition)
  (call-with-debugger-hook nil (lambda () (invoke-debugger condition))))

(defvar *global-debugger* t
  "Non-nil means the Slynk debugger hook will be installed globally.")

(add-hook *new-connection-hook* 'install-debugger)
(defun install-debugger (connection)
  (declare (ignore connection))
  (when *global-debugger*
    (install-debugger-globally #'slynk-debugger-hook)))

;;;;; Debugger loop
;;;
;;; These variables are dynamically bound during debugging.
;;;
(defvar *slynk-debugger-condition* nil
  "The condition being debugged.")

(defvar *sly-db-level* 0
  "The current level of recursive debugging.")

(defvar *sly-db-initial-frames* 20
  "The initial number of backtrace frames to send to Emacs.")

(defvar *sly-db-restarts* nil
  "The list of currenlty active restarts.")

(defvar *sly-db-stepping-p* nil
  "True during execution of a step command.")

(defun debug-in-emacs (condition)
  (let ((*slynk-debugger-condition* condition)
        (*sly-db-restarts* (compute-restarts condition))
        (*sly-db-quit-restart* (and *sly-db-quit-restart*
                                    (find-restart *sly-db-quit-restart*
                                                  condition)))
        (*package* (or (and (boundp '*buffer-package*)
                            (symbol-value '*buffer-package*))
                       *package*))
        (*sly-db-level* (1+ *sly-db-level*))
        (*sly-db-stepping-p* nil))
    (force-user-output)
    (call-with-debugging-environment
     (lambda ()
       (sly-db-loop *sly-db-level*)))))

(defun sly-db-loop (level)
  (unwind-protect
       (loop
        (with-simple-restart (abort "Return to sly-db level ~D." level)
          (send-to-emacs
           (list* :debug (current-thread-id) level
                  (debugger-info-for-emacs 0 *sly-db-initial-frames*)))
          (send-to-emacs
           (list :debug-activate (current-thread-id) level))
          (loop
           (handler-case
               (destructure-case (wait-for-event
                                  `(or (:emacs-rex . _)
                                       (:emacs-channel-send . _)
                                       (:sly-db-return ,(1+ level))))
                 ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
                 ((:emacs-channel-send channel (selector &rest args))
                  (channel-send channel selector args))
                 ((:sly-db-return _) (declare (ignore _)) (return nil)))
             (sly-db-condition (c)
               (handle-sly-db-condition c))))))
    (send-to-emacs `(:debug-return
                     ,(current-thread-id) ,level ,*sly-db-stepping-p*))
    (wait-for-event `(:sly-db-return ,(1+ level)) t) ; clean event-queue
    (when (> level 1)
      (send-event (current-thread) `(:sly-db-return ,level)))))

(defun handle-sly-db-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(current-thread-id)
                                      ,(princ-to-string real-condition)))))

(defun %%condition-message (condition)
  (let ((limit (ash 1 16)))
    (with-string-stream (stream :length limit)
      (handler-case
          (let ((*print-readably* nil)
                (*print-pretty* t)
                (*print-right-margin* 65)
                (*print-circle* t)
                (*print-length* (or *print-length* limit))
                (*print-level* (or *print-level* limit))
                (*print-lines* (or *print-lines* limit)))
            (print-condition condition stream))
        (serious-condition (c)
          (ignore-errors
            (with-standard-io-syntax
              (let ((*print-readably* nil))
                (format stream "~&Error (~a) printing the following condition: " (type-of c))
                (print-unreadable-object (condition stream :type t
                                                    :identity t))))))))))

(defun %condition-message (condition)
  (string-trim #(#\newline #\space #\tab)
               (%%condition-message condition)))

(defvar *sly-db-condition-printer* #'%condition-message
  "Function called to print a condition to an SLY-DB buffer.")

(defun safe-condition-message (condition)
  "Print condition to a string, handling any errors during printing."
  (funcall *sly-db-condition-printer* condition))

(defvar *debugger-extra-options* nil
  ;; JT@15/08/24: FIXME: Actually, with a nice and proper method-combination for
  ;; interfaces (as was once quite bravely attempted by Helmut, this variable
  ;; could go away and contribs could simply add methods to CONDITION-EXTRAS)
  ;; 
  "A property list of extra options describing a condition.
This works much like the CONDITION-EXTRAS interface, but can be
dynamically bound by contribs when invoking the debugger.")

(defun debugger-condition-for-emacs ()
  (list (safe-condition-message *slynk-debugger-condition*)
        (format nil "   [Condition of type ~S]"
                (type-of *slynk-debugger-condition*))
        (append (condition-extras *slynk-debugger-condition*)
                *debugger-extra-options*)))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *slynk-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sly-db-restarts* collect
          (list (format nil "~:[~;*~]~a"
                        (eq restart *sly-db-quit-restart*)
                        (restart-name restart))
                (with-output-to-string (stream)
                  (without-printing-errors (:object restart
                                            :stream stream
                                            :msg "<<error printing restart>>")
                    (princ restart stream)))))))

;;;;; SLY-DB entry points

(defslyfun sly-db-break-with-default-debugger (dont-unwind)
  "Invoke the default debugger."
  (cond (dont-unwind
         (invoke-default-debugger *slynk-debugger-condition*))
        (t
         (signal 'invoke-default-debugger))))

(defslyfun backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (loop for frame in (compute-backtrace start end)
        for i from start collect
        (list* i (frame-to-string frame)
               (ecase (frame-restartable-p frame)
                 ((nil) nil)
                 ((t) `((:restartable t)))))))

(defun frame-to-string (frame)
  (with-string-stream (stream :length (* (or *print-lines* 1)
                                         (or *print-right-margin* 100))
                              :bindings *backtrace-printer-bindings*)
    (handler-case (print-frame frame stream)
      (serious-condition ()
        (format stream "[error printing frame]")))))

(defslyfun debugger-info-for-emacs (start end)
  "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description [plist])
  extra       ::= (:references and other random things)
  cont        ::= continuation
  plist       ::= (:restartable {nil | t | :unknown})

condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continuation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Sly toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
  (4))"
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)
        *pending-continuations*))

(defun nth-restart (index)
  (nth index *sly-db-restarts*))

(defslyfun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (when restart
      (let* ((prompt nil)
             (*query-io*
               (make-two-way-stream
                (make-input-stream
                 (lambda ()
                   (format nil "~a~%"
                           (read-from-minibuffer-in-emacs
                            (format nil "~a" (or prompt
                                                 "[restart prompt] :"))))))
                (make-output-stream
                 #'(lambda (s)
                     (setq prompt
                           (concatenate 'string
                                        (or prompt "")
                                        s)))))))
        (invoke-restart-interactively restart)))))

(defslyfun sly-db-abort ()
  (invoke-restart (find 'abort *sly-db-restarts* :key #'restart-name)))

(defslyfun sly-db-continue ()
  (continue))

(defun coerce-to-condition (datum args)
  (etypecase datum
    (string (make-condition 'simple-error :format-control datum
                            :format-arguments args))
    (symbol (apply #'make-condition datum args))))

(defslyfun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (invoke-sly-debugger (coerce-to-condition datum args))))

;; FIXME: (last (compute-restarts)) looks dubious.
(defslyfun throw-to-toplevel ()
  "Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."
  (let ((restart (or (and *sly-db-quit-restart*
                          (find-restart *sly-db-quit-restart*))
                     (car (last (compute-restarts))))))
    (cond (restart (invoke-restart restart))
          (t (format nil "Restart not active [~s]" *sly-db-quit-restart*)))))

(defslyfun invoke-nth-restart-for-emacs (sly-db-level n)
  "Invoke the Nth available restart.
SLY-DB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."
  (when (= sly-db-level *sly-db-level*)
    (invoke-nth-restart n)))

(defun wrap-sly-db-vars (form)
  `(let ((*sly-db-level* ,*sly-db-level*))
     ,form))

(defun eval-in-frame-aux (frame string package print)
  (let* ((form (wrap-sly-db-vars (parse-string string package)))
         (values (multiple-value-list (eval-in-frame form frame))))
    (with-buffer-syntax (package)
      (funcall print values))))

(defslyfun eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'echo-for-emacs))

(defslyfun pprint-eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'slynk-pprint-values))

(defslyfun frame-package-name (frame)
  (let ((pkg (frame-package frame)))
    (cond (pkg (package-name pkg))
          (t (with-buffer-syntax () (package-name *package*))))))

(defslyfun frame-locals-and-catch-tags (index)
  "Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
LOCALS is a list of the form ((&key NAME ID VALUE) ...).
TAGS has is a list of strings."
  (list (frame-locals-for-emacs index)
        (mapcar #'to-string (frame-catch-tags index))))

(defun frame-locals-for-emacs (index)
  (loop for var in (frame-locals index)
        collect
        (destructuring-bind (&key name id value) var
          (list :name (let ((*package* (or (frame-package index) *package*)))
                        (prin1-to-string name))
                :id id
                :value
                (let ((*slynk-pprint-bindings*
                        (append *slynk-pprint-bindings*
                                *backtrace-printer-bindings*)))
                  (slynk-pprint value))))))

(defslyfun sly-db-disassemble (index)
  (with-output-to-string (*standard-output*)
    (disassemble-frame index)))

(defslyfun sly-db-return-from-frame (index string)
  (let ((form (from-string string)))
    (to-string (multiple-value-list (return-from-frame index form)))))

(defslyfun sly-db-break (name)
  (with-buffer-syntax ()
    (sly-db-break-at-start (read-from-string name))))

(defmacro define-stepper-function (name backend-function-name)
  `(defslyfun ,name (frame)
     (cond ((sly-db-stepper-condition-p *slynk-debugger-condition*)
            (setq *sly-db-stepping-p* t)
            (,backend-function-name))
           ((find-restart 'continue)
            (activate-stepping frame)
            (setq *sly-db-stepping-p* t)
            (continue))
           (t
            (error "Not currently single-stepping, ~
and no continue restart available.")))))

(define-stepper-function sly-db-step sly-db-step-into)
(define-stepper-function sly-db-next sly-db-step-next)
(define-stepper-function sly-db-out  sly-db-step-out)

(defslyfun toggle-break-on-signals ()
  (setq *break-on-signals* (not *break-on-signals*))
  (format nil "*break-on-signals* = ~a" *break-on-signals*))

(defslyfun sdlb-print-condition ()
  (princ-to-string *slynk-debugger-condition*))


;;;; Compilation Commands.

(defstruct (compilation-result (:type list))
  (type :compilation-result)
  notes
  (successp nil :type boolean)
  (duration 0.0 :type float)
  (loadp nil :type boolean)
  (faslfile nil :type (or null string)))

(defun measure-time-interval (fun)
  "Call FUN and return the first return value and the elapsed time.
The time is measured in seconds."
  (declare (type function fun))
  (let ((before (get-internal-real-time))) ;
    (values
     (funcall fun)
     (/ (- (get-internal-real-time) before)
        (coerce internal-time-units-per-second 'float)))))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list* :message (message condition)
         :severity (severity condition)
         :location (location condition)
         :references (references condition)
         (let ((s (source-context condition)))
           (if s (list :source-context s)))))

(defun collect-notes (function)
  (let ((notes '()))
    (multiple-value-bind (result seconds)
        (handler-bind ((compiler-condition
                        (lambda (c) (push (make-compiler-note c) notes))))
          (measure-time-interval
           (lambda ()
             ;; To report location of error-signaling toplevel forms
             ;; for errors in EVAL-WHEN or during macroexpansion.
             (restart-case (multiple-value-list (funcall function))
               (abort () :report "Abort compilation." (list nil))))))
      (destructuring-bind (successp &optional loadp faslfile) result
        (let ((faslfile (etypecase faslfile
                          (null nil)
                          (pathname (pathname-to-filename faslfile)))))
          (make-compilation-result :notes (reverse notes)
                                   :duration seconds
                                   :successp (if successp t)
                                   :loadp (if loadp t)
                                   :faslfile faslfile))))))

(defun slynk-compile-file* (pathname load-p &rest options &key policy
                                                      &allow-other-keys)
  (multiple-value-bind (output-pathname warnings? failure?)
      (slynk-compile-file pathname
                          (fasl-pathname pathname options)
                          nil
                          (or (guess-external-format pathname)
                              :default)
                          :policy policy)
    (declare (ignore warnings?))
    (values t (not failure?) load-p output-pathname)))

(defvar *compile-file-for-emacs-hook* '(slynk-compile-file*))

(defslyfun compile-file-for-emacs (filename load-p &rest options)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (collect-notes
     (lambda ()
       (let ((pathname (filename-to-pathname filename))
             (*compile-print* nil)
             (*compile-verbose* t))
         (loop for hook in *compile-file-for-emacs-hook*
               do
               (multiple-value-bind (tried success load? output-pathname)
                   (apply hook pathname load-p options)
                 (when tried
                   (return (values success load? output-pathname))))))))))

;; FIXME: now that *compile-file-for-emacs-hook* is there this is
;; redundant and confusing.
(defvar *fasl-pathname-function* nil
  "In non-nil, use this function to compute the name for fasl-files.")

(defun pathname-as-directory (pathname)
  (append (pathname-directory pathname)
          (when (pathname-name pathname)
            (list (file-namestring pathname)))))

(defun compile-file-output (file directory)
  (make-pathname :directory (pathname-as-directory directory)
                 :defaults (compile-file-pathname file)))

(defun fasl-pathname (input-file options)
  (cond (*fasl-pathname-function*
         (funcall *fasl-pathname-function* input-file options))
        ((getf options :fasl-directory)
         (let ((dir (getf options :fasl-directory)))
           (assert (char= (aref dir (1- (length dir))) #\/))
           (compile-file-output input-file dir)))
        (t
         (compile-file-pathname input-file))))

(defslyfun compile-string-for-emacs (string buffer position filename policy)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (let* ((offset (cadr (assoc :position position)))
         (line-column (cdr (assoc :line position)))
         (line (first line-column))
         (column (second line-column)))
    (with-buffer-syntax ()
      (collect-notes
       (lambda ()
         (let ((*compile-print* nil)
               (*compile-verbose* nil)
               (*load-verbose* nil))
           (slynk-compile-string string
                                 :buffer buffer
                                 :position offset
                                 :filename filename
                                 :line line
                                 :column column
                                 :policy policy)))))))

(defslyfun compile-multiple-strings-for-emacs (strings policy)
  "Compile STRINGS (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (loop for (string buffer package position filename) in strings collect
        (collect-notes
         (lambda ()
           (with-buffer-syntax (package)
             (let ((*compile-print* t) (*compile-verbose* nil))
               (slynk-compile-string string
                                     :buffer buffer
                                     :position position
                                     :filename filename
                                     :policy policy)))))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

(defslyfun compile-file-if-needed (filename loadp)
  (let ((pathname (filename-to-pathname filename)))
    (cond ((requires-compile-p pathname)
           (compile-file-for-emacs pathname loadp))
          (t
           (collect-notes
            (lambda ()
              (or (not loadp)
                  (load (compile-file-pathname pathname)))))))))


;;;; Loading

(defslyfun load-file (filename)
  (to-string (load (filename-to-pathname filename))))


;;;;; slynk-require

(defvar *module-loading-method* (find-if #'find-package '(:slynk-loader :asdf))
  "Keyword naming the module-loading method.

SLY's own `slynk-loader.lisp' is tried first, then ASDF")

(defvar *asdf-load-in-progress* nil
  "Set to t if inside a \"ASDF:LOAD-SYSTEM\" operation.
Introduced to prevent problematic recursive ASDF loads, but going away
soon once non-ASDF loading is removed. (see github#134)")

(defgeneric require-module (method module)
  (:documentation
   "Use METHOD to load MODULE.
Receives a module name as argument and should return non-nil if it
managed to load it.")
  (:method ((method (eql :slynk-loader)) module)
    (funcall (intern "REQUIRE-MODULE" :slynk-loader) module))
  (:method ((method (eql :asdf)) module)
    (unless *asdf-load-in-progress*
      (let ((*asdf-load-in-progress* t))
        (funcall (intern "LOAD-SYSTEM" :asdf) module)))))

(defun add-to-load-path-1 (path load-path-var)
  (pushnew path (symbol-value load-path-var) :test #'equal))

(defgeneric add-to-load-path (method path)
  (:documentation
   "Using METHOD, consider PATH when searching for modules.")
  (:method ((method (eql :slynk-loader)) path)
    (add-to-load-path-1 path (intern "*LOAD-PATH*" :slynk-loader)))
  (:method ((method (eql :asdf)) path)
    (add-to-load-path-1 path (intern "*CENTRAL-REGISTRY*" :asdf))))

(defvar *slynk-require-hook* '()
  "Functions run after SLYNK-REQUIRE. Called with new modules.")

(defslyfun slynk-require (modules)
  "Load each module in MODULES.

MODULES is a list of strings designators or a single string
designator. Returns a list of all modules available."
  (let ((loaded))
    (dolist (module (ensure-list modules))
      (with-simple-restart (continue "Continue without SLY contrib ~a" module)
        (funcall #'require-module *module-loading-method* module)
        (push module loaded)
        (pushnew (string-upcase module) *modules* :test #'equal))
      (loop for fn in *slynk-require-hook*
            do (funcall fn loaded)))
    (list *modules* loaded)))

(defslyfun slynk-add-load-paths (paths)
  (dolist (path paths)
    (funcall #'add-to-load-path *module-loading-method* (pathname path))))


;;;; Macroexpansion

(defvar *macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-lines* . nil)
    (*print-level* . nil)
    (*print-length* . nil)
    (*print-case* . :downcase))
  "Pretty-pretty bindings to use when expanding macros")

(defun apply-macro-expander (expander string)
  (with-buffer-syntax ()
    (let ((expansion (funcall expander (from-string string))))
      (with-bindings *macroexpand-printer-bindings*
        (prin1-to-string expansion)))))

(defslyfun slynk-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslyfun slynk-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslyfun slynk-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))

(defslyfun slynk-compiler-macroexpand-1 (string)
  (apply-macro-expander #'compiler-macroexpand-1 string))

(defslyfun slynk-compiler-macroexpand (string)
  (apply-macro-expander #'compiler-macroexpand string))

(defslyfun slynk-expand-1 (string)
  (apply-macro-expander #'expand-1 string))

(defslyfun slynk-expand (string)
  (apply-macro-expander #'expand string))

(defun expand-1 (form)
  (multiple-value-bind (expansion expanded?) (macroexpand-1 form)
    (if expanded?
        (values expansion t)
        (compiler-macroexpand-1 form))))

(defun expand (form)
  (expand-repeatedly #'expand-1 form))

(defun expand-repeatedly (expander form)
  (loop
    (multiple-value-bind (expansion expanded?) (funcall expander form)
      (unless expanded? (return expansion))
      (setq form expansion))))

(defslyfun slynk-format-string-expand (string)
  (apply-macro-expander #'format-string-expand string))

(defslyfun disassemble-form (form)
  (with-buffer-syntax ()
    (with-output-to-string (*standard-output*)
      (let ((*print-readably* nil))
        (disassemble (eval (read-from-string form)))))))


;;;; Simple arglist display

(defslyfun operator-arglist (name package)
  (ignore-errors
   (let ((args (arglist (parse-symbol name (guess-buffer-package package)))))
     (cond ((eq args :not-available) nil)
           (t (princ-to-string (cons name args)))))))


;;;; Documentation

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun call-with-describe-settings (fn)
  (let ((*print-readably* nil))
    (funcall fn)))

(defmacro with-describe-settings ((&rest _) &body body)
  (declare (ignore _))
  `(call-with-describe-settings (lambda () ,@body)))

(defun describe-to-string (object)
  (with-describe-settings ()
    (with-output-to-string (*standard-output*)
      (describe object))))

(defslyfun describe-symbol (symbol-name)
  (with-buffer-syntax ()
    (describe-to-string (parse-symbol-or-lose symbol-name))))

(defslyfun describe-function (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol-or-lose name)))
      (describe-to-string (or (macro-function symbol)
                              (symbol-function symbol))))))

(defslyfun describe-definition-for-emacs (name kind)
  (with-buffer-syntax ()
    (with-describe-settings ()
      (with-output-to-string (*standard-output*)
        (describe-definition (parse-symbol-or-lose name) kind)))))

(defslyfun documentation-symbol (symbol-name)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (with-output-to-string (string)
              (format string "Documentation for the symbol ~a:~2%" sym)
              (unless (or vdoc fdoc)
                (format string "Not documented." ))
              (when vdoc
                (format string "Variable:~% ~a~2%" vdoc))
              (when fdoc
                (format string "Function:~% Arglist: ~a~2% ~a"
                        (slynk-backend:arglist sym)
                        fdoc))))
          (format nil "No such symbol, ~a." symbol-name)))))


;;;; Package Commands

(defslyfun list-all-package-names (&optional nicknames)
  "Return a list of all package names.
Include the nicknames if NICKNAMES is true."
  (mapcar #'unparse-name
          (if nicknames
              (mapcan #'package-names (list-all-packages))
              (mapcar #'package-name  (list-all-packages)))))


;;;; Tracing

;; Use eval for the sake of portability...
(defun tracedp (fspec)
  (member fspec (eval '(trace))))

(defvar *after-toggle-trace-hook* nil
  "Hook called whenever a SPEC is traced or untraced.

If non-nil, called with two arguments SPEC and TRACED-P." )
(defslyfun slynk-toggle-trace (spec-string)
  (let* ((spec (from-string spec-string))
         (retval (cond ((consp spec) ; handle complicated cases in the backend
                        (toggle-trace spec))
                       ((tracedp spec)
                        (eval `(untrace ,spec))
                        (format nil "~S is now untraced." spec))
                       (t
                        (eval `(trace ,spec))
                        (format nil "~S is now traced." spec))))
         (traced-p (let* ((tosearch "is now traced.")
                          (start (- (length retval)
                                    (length tosearch)))
                          (end (+ start (length tosearch))))
                     (search tosearch (subseq retval start end))))
         (hook-msg (when *after-toggle-trace-hook*
                     (funcall *after-toggle-trace-hook*
                              spec
                              traced-p))))
    (if hook-msg
        (format nil "~a~%(also ~a)" retval hook-msg)
        retval)))

(defslyfun untrace-all ()
  (untrace))


;;;; Undefing

(defslyfun undefine-function (fname-string)
  (let ((fname (from-string fname-string)))
    (format nil "~S" (fmakunbound fname))))

(defun read-as-function (name)
  (eval (from-string (format nil "(function ~A)" name))))

(defslyfun remove-method-by-name (generic-name qualifiers specializers)
  "Remove GENERIC-NAME's method with QUALIFIERS and SPECIALIZERS."
  (let* ((generic-function (read-as-function generic-name))
         (qualifiers (mapcar #'from-string qualifiers))
         (specializers (mapcar #'from-string specializers))
         (method (find-method generic-function qualifiers specializers)))
    (remove-method generic-function method)
    t))

(defslyfun generic-method-specs (generic-name)
  "Compute ((QUALIFIERS SPECIALIZERS)...) for methods of GENERIC-NAME's gf.
QUALIFIERS and SPECIALIZERS are lists of strings."
  (mapcar
   (lambda (method)
     (list (mapcar #'prin1-to-string (slynk-mop:method-qualifiers method))
           (mapcar (lambda (specializer)
                     (if (typep specializer 'slynk-mop:eql-specializer)
                         (format nil "(eql ~A)"
                                 (slynk-mop:eql-specializer-object specializer))
                         (prin1-to-string (class-name specializer))))
                   (slynk-mop:method-specializers method))))
   (slynk-mop:generic-function-methods (read-as-function generic-name))))

(defslyfun unintern-symbol (name package)
  (let ((pkg (guess-package package)))
    (cond ((not pkg) (format nil "No such package: ~s" package))
          (t
           (multiple-value-bind (sym found) (parse-symbol name pkg)
             (case found
               ((nil) (format nil "~s not in package ~s" name package))
               (t
                (unintern sym pkg)
                (format nil "Uninterned symbol: ~s" sym))))))))

(defslyfun slynk-delete-package (package-name)
  (let ((pkg (or (guess-package package-name)
                 (error "No such package: ~s" package-name))))
    (delete-package pkg)
    nil))

;;;; Source Locations

(defslyfun find-definition-for-thing (thing)
  (find-source-location thing))

(defslyfun find-source-location-for-emacs (spec)
  (find-source-location (value-spec-ref spec)))

(defun value-spec-ref (spec)
  (destructure-case spec
    ((:string string package)
     (with-buffer-syntax (package)
       (eval (read-from-string string))))
    ((:inspector part)
     (inspector-nth-part part))
    ((:sly-db frame var)
     (frame-var-value frame var))))

(defvar *find-definitions-right-trim* ",:.>")
(defvar *find-definitions-left-trim* "#:<")

(defun find-definitions-find-symbol-or-package (name)
  (flet ((do-find (name)
           (multiple-value-bind (symbol found name)
               (with-buffer-syntax ()
                 (parse-symbol name))
             (cond (found
                    (return-from find-definitions-find-symbol-or-package
                      (values symbol found)))
                   ;; Packages are not named by symbols, so
                   ;; not-interned symbols can refer to packages
                   ((find-package name)
                    (return-from find-definitions-find-symbol-or-package
                      (values (make-symbol name) t)))))))
    (do-find name)
    (do-find (string-right-trim *find-definitions-right-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim*
                               (string-right-trim
                                *find-definitions-right-trim* name)))
    ;; Not exactly robust
    (when (and (eql (search "(setf " name :test #'char-equal) 0)
               (char= (char name (1- (length name))) #\)))
      (multiple-value-bind (symbol found)
          (with-buffer-syntax ()
            (parse-symbol (subseq name (length "(setf ")
                                  (1- (length name)))))
        (when found
          (values `(setf ,symbol) t))))))

(defslyfun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (multiple-value-bind (symbol found)
      (find-definitions-find-symbol-or-package name)
    (when found
      (mapcar #'xref>elisp (find-definitions symbol)))))

;;; Generic function so contribs can extend it.
(defgeneric xref-doit (type thing)
  (:method (type thing)
    (declare (ignore type thing))
    :not-implemented))

(macrolet ((define-xref-action (xref-type handler)
             `(defmethod xref-doit ((type (eql ,xref-type)) thing)
                (declare (ignorable type))
                (funcall ,handler thing))))
  (define-xref-action :calls        #'who-calls)
  (define-xref-action :calls-who    #'calls-who)
  (define-xref-action :references   #'who-references)
  (define-xref-action :binds        #'who-binds)
  (define-xref-action :sets         #'who-sets)
  (define-xref-action :macroexpands #'who-macroexpands)
  (define-xref-action :specializes  #'who-specializes)
  (define-xref-action :callers      #'list-callers)
  (define-xref-action :callees      #'list-callees))

(defslyfun xref (type name)
  (multiple-value-bind (sexp error) (ignore-errors (from-string name))
    (unless error
      (let ((xrefs  (xref-doit type sexp)))
        (if (eq xrefs :not-implemented)
            :not-implemented
            (mapcar #'xref>elisp xrefs))))))

(defslyfun xrefs (types name)
  (loop for type in types
        for xrefs = (xref type name)
        when (and (not (eq :not-implemented xrefs))
                  (not (null xrefs)))
          collect (cons type xrefs)))

(defun xref>elisp (xref)
  (destructuring-bind (name loc) xref
    (list (to-string name) loc)))


;;;;; Lazy lists

(defstruct (lcons (:constructor %lcons (car %cdr))
                  (:predicate lcons?))
  car
  (%cdr nil :type (or null lcons function))
  (forced? nil))

(defmacro lcons (car cdr)
  `(%lcons ,car (lambda () ,cdr)))

(defmacro lcons* (car cdr &rest more)
  (cond ((null more) `(lcons ,car ,cdr))
        (t `(lcons ,car (lcons* ,cdr ,@more)))))

(defun lcons-cdr (lcons)
  (let ((cdr (lcons-%cdr lcons)))
    (cond ((lcons-forced? lcons) cdr)
          (t
           (let ((value (funcall cdr)))
             (setf (lcons-forced? lcons) t
                   (lcons-%cdr lcons) value))))))

(defun llist-range (llist start end)
  (llist-take (llist-skip llist start) (- end start)))

(defun llist-skip (lcons index)
  (do ((i 0 (1+ i))
       (l lcons (lcons-cdr l)))
      ((or (= i index) (null l))
       l)))

(defun llist-take (lcons count)
  (let ((result '()))
    (do ((i 0 (1+ i))
         (l lcons (lcons-cdr l)))
        ((or (= i count)
             (null l)))
      (push (lcons-car l) result))
    (nreverse result)))

(defun iline (label value)
  `(:line ,label ,value))


;;;; Inspecting
(defvar-unbound *current-inspector*
    "Current inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defvar-unbound *target-inspector*
    "Target inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defun current-inspector ()
  (or (and (boundp '*current-inspector*)
           *current-inspector*)
      (find-inspector "default")
      (make-instance 'inspector :name "default")))

(defun target-inspector ()
  (or (and (boundp '*target-inspector*)
           *target-inspector*)
      (current-inspector)))


(defvar *inspector-printer-bindings*
  '((*print-lines*        . 1)
    (*print-right-margin* . 75)
    (*print-pretty*       . t)
    (*print-readably*     . nil)))

(defvar *inspector-verbose-printer-bindings*
  '((*print-escape* . t)
    (*print-circle* . t)
    (*print-array*  . nil)))

(defclass inspector ()
  ((verbose-p :initform nil :accessor inspector-verbose-p)
   (history :initform (make-array 10 :adjustable t :fill-pointer 0) :accessor inspector-%history)
   (name :initarg :name :initform (error "Name this INSPECTOR!") :accessor inspector-name)))

(defmethod print-object ((i inspector) s)
  (print-unreadable-object (i s :type t) 
    (format s "~a/~a" (inspector-name i) (length (inspector-%history i)))))

(defmethod initialize-instance :after ((i inspector) &key name)
  (assert (not (find-inspector name)) nil "Already have an inspector named ~a" name)
  (push i (connection-inspectors *emacs-connection*)))

(defun find-inspector (name)
  (find name (connection-inspectors *emacs-connection*)
        :key #'inspector-name :test #'string=))

(defstruct inspector-state)
(defstruct (istate (:conc-name istate.) (:include inspector-state))
  object
  (parts (make-array 10 :adjustable t :fill-pointer 0))
  (actions (make-array 10 :adjustable t :fill-pointer 0))
  metadata
  content
  serial)

(defun ensure-istate-metadata (o indicator default)
  (with-struct (istate. object metadata) (current-istate)
    (assert (eq object o))
    (let ((data (getf metadata indicator default)))
      (setf (getf metadata indicator) data)
      data)))

(defun current-istate (&optional (inspector (current-inspector)))
  (let* ((history (inspector-%history inspector)))
    (and (plusp (length history))
         (aref history (1- (length history))))))

(defun reset-inspector (&optional (inspector (current-inspector)))
  #+sbcl
  ;; FIXME: On SBCL, for some silly reason, this is needed to lose the
  ;; references to the history's objects (github##568)
  (loop with hist = (inspector-%history inspector)
        for i from 0 below (array-dimension hist 0)
        do (setf (aref hist i) nil))
  (setf (inspector-%history inspector)
        (make-array 10 :adjustable t :fill-pointer 0)))

(defslyfun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (inspect-object (eval (read-from-string string))))))

(defun inspect-object (o)
  (let* ((inspector (target-inspector))
         (history (inspector-%history inspector))
         (istate (make-istate :object o)))
    (vector-push-extend istate history)
    (let ((*current-inspector* inspector))
      ;; HACK! because EMACS-INSPECT may call ENSURE-ISTATE-METADATA
      ;; which expects its object to be the current istate's objects.
      (setf (istate.content istate)
            (emacs-inspect o)))
    (vector-push-extend :break-history history)
    (decf (fill-pointer history))
    (istate>elisp istate)))

(defun istate>elisp (istate)
  (list :title (prepare-title istate)
        :id (assign-index (istate.object istate) (istate.parts istate))
        :content (prepare-range istate 0 500)
        ;; :serial (istate.serial istate)
        ))

(defun prepare-title (istate)
  (if (inspector-verbose-p (current-inspector))
      (with-bindings *inspector-verbose-printer-bindings*
        (to-string (istate.object istate)))
      (with-string-stream (stream :length 200
                                  :bindings *inspector-printer-bindings*)
        (print-unreadable-object
            ((istate.object istate) stream :type t :identity t)))))

(defun prepare-range (istate start end)
  (let* ((range (content-range (istate.content istate) start end))
         (ps (loop for part in range append (prepare-part part istate))))
    (list ps
          (if (< (length ps) (- end start))
              (+ start (length ps))
              (+ end 1000))
          start end)))

(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (destructure-case part
              ((:newline) (list newline))
              ((:value obj &optional str)
               (list (value-part obj str (istate.parts istate))))
              ((:label &rest strs)
               (list (list :label (apply #'cat (mapcar #'string strs)))))
              ((:action label lambda &key (refreshp t))
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value)
               (list (princ-to-string label) ": "
                     (value-part value nil (istate.parts istate))
                     newline)))))))

(defun value-part (object string parts)
  (list :value
        (or string (print-part-to-string object))
        (assign-index object parts)))

(defun action-part (label lambda refreshp actions)
  (list :action label (assign-index (list lambda refreshp) actions)))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun print-part-to-string (value)
  (let* ((*print-readably* nil)
         (string (slynk-pprint-to-line value))
         (pos (position value
                        (inspector-%history (current-inspector))
                        :key #'istate.object)))
    (if pos
        (format nil "@~D=~A" pos string)
        string)))

(defun content-range (list start end)
  (typecase list
    (list (let ((len (length list)))
            (subseq list start (min len end))))
    (lcons (llist-range list start end))))

(defslyfun inspector-nth-part (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (let* ((parts (istate.parts (current-istate)))
         (foundp (< index (length parts))))
    (values (and foundp (aref parts index))
            foundp)))

(defslyfun inspector-nth-part-or-lose (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (multiple-value-bind (part foundp)
      (inspector-nth-part index)
    (if foundp part (error "No part with index ~a" index))))

(defslyfun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslyfun inspector-range (from to)
  (prepare-range (current-istate) from to))

(defslyfun inspector-call-nth-action (index &rest args)
  (destructuring-bind (fun refreshp) (aref (istate.actions (current-istate)) index)
    (apply fun args)
    (if refreshp
        (inspector-reinspect)
        ;; tell emacs that we don't want to refresh the inspector buffer
        nil)))

(defslyfun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (> (length history) 1)
        (decf (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-next ()
  "Inspect the next element in the history of inspected objects.."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (and (< (fill-pointer history)
                    (array-dimension history 0))
                 (istate-p (aref history (fill-pointer history))))
        (incf (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-reinspect ()
  (let ((istate (current-istate)))
    (setf (istate.content istate)
          (emacs-inspect (istate.object istate)))
    (istate>elisp istate)))

(defslyfun inspector-toggle-verbose ()
  "Toggle verbosity of inspected object."
  (setf (inspector-verbose-p (current-inspector))
        (not (inspector-verbose-p (current-inspector))))
  (istate>elisp (current-istate)))

(defslyfun inspector-eval (string)
  (let* ((obj (istate.object (current-istate)))
         (context (eval-context obj))
         (form (with-buffer-syntax ((cdr (assoc '*package* context)))
                 (read-from-string string)))
         (ignorable (remove-if #'boundp (mapcar #'car context))))
    (to-string (eval `(let ((* ',obj) (- ',form)
                            . ,(loop for (var . val) in context
                                     unless (constantp var) collect
                                     `(,var ',val)))
                        (declare (ignorable . ,ignorable))
                        ,form)))))

(defslyfun inspector-history ()
  (slynk-pprint-to-line (inspector-%history (current-inspector))))

(defslyfun quit-inspector ()
  (reset-inspector)
  nil)

(defslyfun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (istate.object (current-istate)))))

(defslyfun describe-inspector-part (index)
  "Describe part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (inspector-nth-part index))))

(defslyfun pprint-inspector-part (index)
  "Pretty-print part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (slynk-pprint (inspector-nth-part index))))

(defslyfun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (reset-inspector)
      (inspect-object (eval-in-frame (from-string string) index)))))

(defslyfun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *slynk-debugger-condition*)))

(defslyfun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))

(defslyfun pprint-frame-var (frame var)
  (with-buffer-syntax ()
    (slynk-pprint (frame-var-value frame var))))

(defslyfun describe-frame-var (frame var)
  (with-buffer-syntax ()
    (describe-to-string (frame-var-value frame var))))

(defslyfun eval-for-inspector (current
                               target
                               slave-slyfun &rest args)
  "Call SLAVE-SLYFUN with ARGS in CURRENT inspector, open in TARGET."
  (let ((*current-inspector* (and current
                                  (or (find-inspector current)
                                      (make-instance 'inspector :name current))))
        (*target-inspector* (and target
                                 (or (find-inspector target)
                                     (make-instance 'inspector :name target)))))
    (apply slave-slyfun args)))

;;;;; Lists

(defmethod emacs-inspect ((o cons))
  (if (listp (cdr o))
      (inspect-list o)
      (inspect-cons o)))

(defun inspect-cons (cons)
  (label-value-line*
   ('car (car cons))
   ('cdr (cdr cons))))

(defun inspect-list (list)
  (multiple-value-bind (length tail) (safe-length list)
    (flet ((frob (title list)
             (list* title '(:newline) (inspect-list-aux list))))
      (cond ((not length)
             (frob "A circular list:"
                   (cons (car list)
                         (ldiff (cdr list) list))))
            ((not tail)
             (frob "A proper list:" list))
            (t
             (frob "An improper list:" list))))))

(defun inspect-list-aux (list)
  (loop for i from 0  for rest on list  while (consp rest)  append
        (if (listp (cdr rest))
            (label-value-line i (car rest))
            (label-value-line* (i (car rest)) (:tail (cdr rest))))))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Return NIL if LIST is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;;;;; Hashtables

(defun hash-table-to-alist (ht)
  (let ((result '()))
    (maphash (lambda (key value)
               (setq result (acons key value result)))
             ht)
    result))

(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]"
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (let ((content (hash-table-to-alist ht)))
     (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
            (setf content (sort content 'string< :key #'first)))
           ((every (lambda (x) (typep (first x) 'real)) content)
            (setf content (sort content '< :key #'first))))
     (loop for (key . value) in content appending
           `((:value ,key) " = " (:value ,value)
             " " (:action "[remove entry]"
                          ,(let ((key key))
                                (lambda () (remhash key ht))))
             (:newline))))))

;;;;; Arrays

(defmethod emacs-inspect ((array array))
  (lcons*
   (iline "Dimensions" (array-dimensions array))
   (iline "Element type" (array-element-type array))
   (iline "Total size" (array-total-size array))
   (iline "Adjustable" (adjustable-array-p array))
   (iline "Fill pointer" (if (array-has-fill-pointer-p array)
                             (fill-pointer array)))
   "Contents:" '(:newline)
   (labels ((k (i max)
              (cond ((= i max) '())
                    (t (lcons (iline i (row-major-aref array i))
                              (k (1+ i) max))))))
     (k 0 (array-total-size array)))))

;;;;; Chars

(defmethod emacs-inspect :around (object)
  (declare (ignore object))
  (with-bindings (if (inspector-verbose-p (current-inspector))
                     *inspector-verbose-printer-bindings*
                     *inspector-printer-bindings*)
    (call-next-method)))

(defmethod emacs-inspect ((char character))
  (append
   (label-value-line*
    ("Char code" (char-code char))
    ("Lower cased" (char-downcase char))
    ("Upper cased" (char-upcase char)))
   (if (get-macro-character char)
       `("In the current readtable ("
         (:value ,*readtable*) ") it is a macro character: "
         (:value ,(get-macro-character char))))))

;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslyfun list-threads ()
  "Return a list (LABELS (ID NAME STATUS ATTRS ...) ...).
LABELS is a list of attribute names and the remaining lists are the
corresponding attribute values per thread.
Example:
  ((:id :name :status :priority)
   (6 \"slynk-indentation-cache-thread\" \"Semaphore timed wait\" 0)
   (5 \"reader-thread\" \"Active\" 0)
   (4 \"control-thread\" \"Semaphore timed wait\" 0)
   (2 \"Slynk Sentinel\" \"Semaphore timed wait\" 0)
   (1 \"listener\" \"Active\" 0)
   (0 \"Initial\" \"Sleep\" 0))"
  (setq *thread-list* (all-threads))
  (when (and *emacs-connection*
             (use-threads-p)
             ;; FIXME: hardcoded thread name
             (equalp (thread-name (current-thread)) "slynk-worker")) 
    (setf *thread-list* (delete (current-thread) *thread-list*)))
  (let* ((plist (thread-attributes (car *thread-list*)))
         (labels (loop for (key) on plist by #'cddr
                       collect key)))
    `((:id :name :status ,@labels)
      ,@(loop for thread in *thread-list*
              for name = (thread-name thread)
              for attributes = (thread-attributes thread)
              collect (list* (thread-id thread)
                             (string name)
                             (thread-status thread)
                             (loop for label in labels
                                   collect (getf attributes label)))))))

(defslyfun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslyfun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (queue-thread-interrupt
     (nth-thread index)
     (lambda ()
       (with-connection (connection)
         (simple-break))))))

(defslyfun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslyfun start-slynk-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a slynk server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))

;;;; Class browser

(defun mop-helper (class-name fn)
  (let ((class (find-class class-name nil)))
    (if class
        (mapcar (lambda (x) (to-string (class-name x)))
                (funcall fn class)))))

(defslyfun mop (type symbol-name)
  "Return info about classes using mop.

    When type is:
     :subclasses - return the list of subclasses of class.
     :superclasses - return the list of superclasses of class."
  (let ((symbol (parse-symbol symbol-name *buffer-package*)))
    (ecase type
      (:subclasses
       (mop-helper symbol #'slynk-mop:class-direct-subclasses))
      (:superclasses
       (mop-helper symbol #'slynk-mop:class-direct-superclasses)))))


;;;; Automatically synchronized state
;;;
;;; Here we add hooks to push updates of relevant information to
;;; Emacs.

;;;;; *FEATURES*

(defun sync-features-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  ;; FIXME: *sly-features* should be connection-local
  (unless (eq *sly-features* *features*)
    (setq *sly-features* *features*)
    (send-to-emacs (list :new-features (features-for-emacs)))))

(defun features-for-emacs ()
  "Return `*sly-features*' in a format suitable to send it to Emacs."
  *sly-features*)

(add-hook *pre-reply-hook* 'sync-features-to-emacs)


;;;;; Indentation of macros
;;;
;;; This code decides how macros should be indented (based on their
;;; arglists) and tells Emacs. A per-connection cache is used to avoid
;;; sending redundant information to Emacs -- we just say what's
;;; changed since last time.
;;;
;;; The strategy is to scan all symbols, pick out the macros, and look
;;; for &body-arguments.

(defvar *configure-emacs-indentation* t
  "When true, automatically send indentation information to Emacs
after each command.")

(defslyfun update-indentation-information ()
  (send-to-indentation-cache `(:update-indentation-information))
  nil)

;; This function is for *PRE-REPLY-HOOK*.
(defun sync-indentation-to-emacs ()
  "Send any indentation updates to Emacs via CONNECTION."
  (when *configure-emacs-indentation*
    (send-to-indentation-cache `(:sync-indentation ,*buffer-package*))))

;; Send REQUEST to the cache.  If we are single threaded perform the
;; request right away, otherwise delegate the request to the
;; indentation-cache-thread.
(defun send-to-indentation-cache (request)
  (let ((c *emacs-connection*))
    (etypecase c
      (singlethreaded-connection
       (handle-indentation-cache-request c request))
      (multithreaded-connection
       (without-sly-interrupts
         (send (mconn.indentation-cache-thread c) request)))
      (null t))))

(defun indentation-cache-loop (connection)
  (with-connection (connection)
    (loop
      (restart-case
          (handle-indentation-cache-request connection (receive))
        (abort ()
          :report "Return to the indentation cache request handling loop.")))))

(defun handle-indentation-cache-request (connection request)
  (destructure-case request
    ((:sync-indentation package)
     ;; PACKAGE may have been deleted...
     (when (package-name package)
       (let ((fullp (need-full-indentation-update-p connection)))
         (perform-indentation-update connection fullp package))))
    ((:update-indentation-information)
     (perform-indentation-update connection t nil))))

(defun need-full-indentation-update-p (connection)
  "Return true if the whole indentation cache should be updated.
This is a heuristic to avoid scanning all symbols all the time:
instead, we only do a full scan if the set of packages has changed."
  (set-difference (list-all-packages)
                  (connection-indentation-cache-packages connection)))

(defun perform-indentation-update (connection force package)
  "Update the indentation cache in CONNECTION and update Emacs.
If FORCE is true then start again without considering the old cache."
  (let ((cache (connection-indentation-cache connection)))
    (when force (clrhash cache))
    (let ((delta (update-indentation/delta-for-emacs cache force package)))
      (setf (connection-indentation-cache-packages connection)
            (list-all-packages))
      (unless (null delta)
        (setf (connection-indentation-cache connection) cache)
        (send-to-emacs (list :indentation-update delta))))))

(defun update-indentation/delta-for-emacs (cache force package)
  "Update the cache and return the changes in a (SYMBOL INDENT PACKAGES) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to PACKAGE."
  (let ((alist '()))
    (flet ((consider (symbol)
             (let ((indent (symbol-indentation symbol)))
               (when indent
                 (unless (equal (gethash symbol cache) indent)
                   (setf (gethash symbol cache) indent)
                   (let ((pkgs (mapcar #'package-name
                                       (symbol-packages symbol)))
                         (name (string-downcase symbol)))
                     (push (list name indent pkgs) alist)))))))
      (cond (force
             (do-all-symbols (symbol)
               (consider symbol)))
            ((package-name package) ; don't try to iterate over a
                                    ; deleted package.
             (do-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (consider symbol)))))
      alist)))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))

(defun symbol-packages (symbol)
  "Return the  packages where SYMBOL can be found."
  (let ((string (string symbol)))
    (loop for p in (list-all-packages)
          when (eq symbol (find-symbol string p))
          collect p)))

(defun cl-symbol-p (symbol)
  "Is SYMBOL a symbol in the COMMON-LISP package?"
  (eq (symbol-package symbol) cl-package))

(defun known-to-emacs-p (symbol)
  "Return true if Emacs has special rules for indenting SYMBOL."
  (cl-symbol-p symbol))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `sly-common-lisp-indent-function' property
in Emacs."
  (if (and (macro-function symbol)
           (not (known-to-emacs-p symbol)))
      (let ((arglist (arglist symbol)))
        (etypecase arglist
          ((member :not-available)
           nil)
          (list
           (macro-indentation arglist))))
      nil))

(defun macro-indentation (arglist)
  (if (well-formed-list-p arglist)
      (position '&body (remove '&optional (clean-arglist arglist)))
      nil))

(defun clean-arglist (arglist)
  "Remove &whole, &enviroment, and &aux elements from ARGLIST."
  (cond ((null arglist) '())
        ((member (car arglist) '(&whole &environment))
         (clean-arglist (cddr arglist)))
        ((eq (car arglist) '&aux)
         '())
        (t (cons (car arglist) (clean-arglist (cdr arglist))))))

(defun well-formed-list-p (list)
  "Is LIST a proper list terminated by NIL?"
  (typecase list
    (null t)
    (cons (well-formed-list-p (cdr list)))
    (t    nil)))

(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (symbol-indentation s))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (symbol-indentation s) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))

;;; FIXME: it's too slow on CLASP right now, remove once it's fast enough.
#-clasp
(add-hook *pre-reply-hook* 'sync-indentation-to-emacs)


;;;; Testing

(defslyfun io-speed-test (&optional (n 1000) (m 1))
  (let* ((s *standard-output*)
         (*trace-output* (make-broadcast-stream s *log-output*)))
    (time (progn
            (dotimes (i n)
              (format s "~D abcdefghijklm~%" i)
              (when (zerop (mod n m))
                (finish-output s)))
            (finish-output s)
            (when *emacs-connection*
              (eval-in-emacs '(message "done.")))))
    (terpri *trace-output*)
    (finish-output *trace-output*)
    nil))

(defslyfun flow-control-test (n delay)
  (let ((stream (make-output-stream
                 (let ((conn *emacs-connection*))
                   (lambda (string)
                     (declare (ignore string))
                     (with-connection (conn)
                       (send-to-emacs `(:test-delay ,delay))))))))
    (dotimes (i n)
      (print i stream)
      (force-output stream)
      (background-message "flow-control-test: ~d" i))))


;;;; The "official" API

(defpackage :slynk-api (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((api '(#:*emacs-connection*
               #:*m-x-sly-from-emacs*
               #:default-connection
               ;;
               #:channel
               #:channel-id
               #:channel-thread-id
               #:close-channel
               #:define-channel-method
               #:find-channel
               #:send-to-remote-channel
               #:*channel*
               ;;
               #:listener
               #:with-listener-bindings
               #:saving-listener-bindings
               #:flush-listener-streams
               #:default-listener
               #:close-listener
               ;;
               #:add-hook
               #:*connection-closed-hook*
               #:*after-init-hook*
               #:*new-connection-hook*
               #:*pre-reply-hook*
               #:*after-toggle-trace-hook*
               #:*eval-for-emacs-wrappers*
               #:*debugger-extra-options*
               #:*buffer-readtable*
               ;;
               #:defslyfun
               #:destructure-case
               #:log-event
               #:process-requests
               #:use-threads-p
               #:wait-for-event
               #:with-bindings
               #:with-connection
               #:with-top-level-restart
               #:with-sly-interrupts
               #:with-buffer-syntax
               #:with-retry-restart
               #:*loaded-user-init-file*
               #:load-user-init-file
               #:make-thread-bindings-aware-lambda
               ;;
               #:package-string-for-prompt
               ;;
               #:*slynk-wire-protocol-version*
               ;;
               #:*slynk-require-hook*
               ;;
               #:present-for-emacs
               ;; packages
               ;;
               #:cl-package
               #:+keyword-package+
               #:guess-package
               #:guess-buffer-package
               #:*exclude-symbol-functions*
               #:*buffer-package*
               #:*slynk-io-package*
               #:parse-package
               ;; symbols
               ;;
               #:tokenize-symbol
               #:untokenize-symbol
               #:symbol-external-p
               #:unparse-name
               #:excluded-from-searches-p
               ;;
               ;;
               #:slynk-pprint
               #:slynk-pprint-values
               #:slynk-pprint-to-line
               ;;
               ;;
               #:background-message
               #:map-if)))
    (loop for sym in api
          for slynk-api-sym = (intern (string sym) :slynk-api)
          for slynk-sym = (intern (string sym) :slynk)
          do (unintern slynk-api-sym :slynk-api)
             (import slynk-sym :slynk-api)
             (export slynk-sym :slynk-api))))


;;;; INIT, as called from the slynk-loader.lisp and ASDF's loaders
;;;;
(defvar *loaded-user-init-file* nil
  "User init file actually loaded from user's home, if any.")
(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (find-if (lambda (homedir-file)
             (load (merge-pathnames (user-homedir-pathname)
                                    homedir-file)
                   :if-does-not-exist nil))
           (list (make-pathname :name ".slynk" :type "lisp")
                 (make-pathname :name ".slynkrc"))))

(defun init ()
  (unless (member :slynk *features*)
    (pushnew :slynk *features*))
  (setq *loaded-user-init-file* (load-user-init-file))
  (run-hook *after-init-hook*))

;; Local Variables:
;; sly-load-failed-fasl: ask
;; End:
