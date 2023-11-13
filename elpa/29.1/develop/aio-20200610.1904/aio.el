;;; aio.el --- async/await for Emacs Lisp -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacs-aio
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; `aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python. This
;; package builds upon Emacs 25 generators to provide functions that
;; pause while they wait on asynchronous events. They do not block any
;; thread while paused.

;; The main components of this package are `aio-defun' / `aio-lambda'
;; to define async function, and `aio-await' to pause these functions
;; while they wait on asynchronous events. When an asynchronous
;; function is paused, the main thread is not blocked. It is no more
;; or less powerful than callbacks, but is nicer to use.

;; This is implementation is based on Emacs 25 generators, and
;; asynchronous functions are actually iterators in disguise, operated
;; as stackless, asymmetric coroutines.

;;; Code:

(require 'cl-lib)
(require 'font-lock)
(require 'generator)
(require 'rx)

;; Register new error types
(define-error 'aio-cancel "Promise was canceled")
(define-error 'aio-timeout "Timeout was reached")

(defun aio-promise ()
  "Create a new promise object."
  (record 'aio-promise nil ()))

(defsubst aio-promise-p (object)
  "Return non-nil if OBJECT is a promise."
  (and (eq 'aio-promise (type-of object))
       (= 3 (length object))))

(defsubst aio-result (promise)
  "Return the result of PROMISE, or nil if it is unresolved.

Promise results are wrapped in a function. The result must be
called (e.g. `funcall') in order to retrieve the value."
  (unless (aio-promise-p promise)
    (signal 'wrong-type-argument (list 'aio-promise-p promise)))
  (aref promise 1))

(defun aio-listen (promise callback)
  "Add CALLBACK to PROMISE.

If the promise has already been resolved, the callback will be
scheduled for the next event loop turn."
  (let ((result (aio-result promise)))
    (if result
        (run-at-time 0 nil callback result)
      (push callback (aref promise 2)))))

(defun aio-resolve (promise value-function)
  "Resolve this PROMISE with VALUE-FUNCTION.

A promise can only be resolved once, and any further calls to
`aio-resolve' are silently ignored. The VALUE-FUNCTION must be a
function that takes no arguments and either returns the result
value or rethrows a signal."
  (unless (functionp value-function)
    (signal 'wrong-type-argument (list 'functionp value-function)))
  (unless (aio-result promise)
    (let ((callbacks (nreverse (aref promise 2))))
      (setf (aref promise 1) value-function
            (aref promise 2) ())
      (dolist (callback callbacks)
        (run-at-time 0 nil callback value-function)))))

(defun aio--step (iter promise yield-result)
  "Advance ITER to the next promise.

PROMISE is the return promise of the iterator, which was returned
by the originating async function. YIELD-RESULT is the value
function result directly from the previously yielded promise."
  (condition-case _
      (cl-loop for result = (iter-next iter yield-result)
               then (iter-next iter (lambda () result))
               until (aio-promise-p result)
               finally (aio-listen result
                                   (lambda (value)
                                     (aio--step iter promise value))))
    (iter-end-of-sequence)))

(defmacro aio-with-promise (promise &rest body)
  "Evaluate BODY and resolve PROMISE with the result.

If the body signals an error, this error will be stored in the
promise and rethrown in the promise's listeners."
  (declare (indent defun)
           (debug (form body)))
  (cl-assert (eq lexical-binding t))
  `(aio-resolve ,promise
                (condition-case error
                    (let ((result (progn ,@body)))
                      (lambda () result))
                  (error (lambda ()
                           (signal (car error) (cdr error)))))))

(defmacro aio-await (expr)
  "If EXPR evaluates to a promise, pause until the promise is resolved.

Pausing an async function does not block Emacs' main thread. If
EXPR doesn't evaluate to a promise, the value is returned
immediately and the function is not paused. Since async functions
return promises, async functions can await directly on other
async functions using this macro.

This macro can only be used inside an async function, either
`aio-lambda' or `aio-defun'."
  `(funcall (iter-yield ,expr)))

(defmacro aio-lambda (arglist &rest body)
  "Like `lambda', but defines an async function.

The body of this function may use `aio-await' to wait on
promises. When an async function is called, it immediately
returns a promise that will resolve to the function's return
value, or any uncaught error signal."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define lambda-list lambda-doc
                           [&optional ("interactive" interactive)]
                           &rest sexp)))
  (let ((args (make-symbol "args"))
        (promise (make-symbol "promise"))
        (split-body (macroexp-parse-body body)))
    `(lambda (&rest ,args)
       ,@(car split-body)
       (let* ((,promise (aio-promise))
              (iter (apply (iter-lambda ,arglist
                             (aio-with-promise ,promise
                               ,@(cdr split-body)))
                           ,args)))
         (prog1 ,promise
           (aio--step iter ,promise nil))))))

(defmacro aio-defun (name arglist &rest body)
  "Like `aio-lambda' but gives the function a name like `defun'."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list &rest sexp)))
  `(progn
     (defalias ',name (aio-lambda ,arglist ,@body))
     (function-put ',name 'aio-defun-p t)))

(defun aio-wait-for (promise)
  "Synchronously wait for PROMISE, blocking the current thread."
  (while (null (aio-result promise))
    (accept-process-output))
  (funcall (aio-result promise)))

(defun aio-cancel (promise &optional reason)
  "Attempt to cancel PROMISE, returning non-nil if successful.

All awaiters will receive an aio-cancel signal. The actual
underlying asynchronous operation will not actually be canceled."
  (unless (aio-result promise)
    (aio-resolve promise (lambda () (signal 'aio-cancel reason)))))

(defmacro aio-with-async (&rest body)
  "Evaluate BODY asynchronously as if it was inside `aio-lambda'.

Since BODY is evalued inside an asynchronous lambda, `aio-await'
is available here. This macro evaluates to a promise for BODY's
eventual result.

Beware: Dynamic bindings that are lexically outside
‘aio-with-async’ blocks have no effect.  For example,

  (defvar dynamic-var nil)
  (defun my-func ()
    (let ((dynamic-var 123))
      (aio-with-async dynamic-var)))
  (let ((dynamic-var 456))
    (aio-wait-for (my-func)))
  ⇒ 456

Other global state such as the current buffer behaves likewise."
  (declare (indent 0)
           (debug (&rest sexp)))
  `(let ((promise (funcall (aio-lambda ()
                             (aio-await (aio-sleep 0))
                             ,@body))))
     (prog1 promise
       ;; The is the main feature: Force the final result to be
       ;; realized so that errors are reported.
       (aio-listen promise #'funcall))))

(defmacro aio-chain (expr)
  "`aio-await' on EXPR and replace place EXPR with the next promise.

EXPR must be setf-able. Returns (cdr result). This macro is
intended to be used with `aio-make-callback' in order to follow
a chain of promise-yielding promises."
  (let ((result (make-symbol "result")))
    `(let ((,result (aio-await ,expr)))
       (setf ,expr (car ,result))
       (cdr ,result))))

;; Useful promise-returning functions:

(require 'url)

(aio-defun aio-all (promises)
  "Return a promise that resolves when all PROMISES are resolved."
  (dolist (promise promises)
    (aio-await promise)))

(defun aio-catch (promise)
  "Return a new promise that wraps PROMISE but will never signal.

The promise value is a cons where the car is either :success or
:error. For :success, the cdr will be the result value. For
:error, the cdr will be the error data."
  (let ((result (aio-promise)))
    (cl-flet ((callback (value)
                (aio-resolve result
                             (lambda ()
                               (condition-case error
                                   (cons :success (funcall value))
                                 (error (cons :error error)))))))
      (prog1 result
        (aio-listen promise #'callback)))))

(defun aio-sleep (seconds &optional result)
  "Create a promise that is resolved after SECONDS with RESULT.

The result is a value, not a value function, and it will be
automatically wrapped with a value function (see `aio-resolve')."
  (let ((promise (aio-promise)))
    (prog1 promise
      (run-at-time seconds nil
                   #'aio-resolve promise (lambda () result)))))

(defun aio-idle (seconds &optional result)
  "Create a promise that is resolved after idle SECONDS with RESULT.

The result is a value, not a value function, and it will be
automatically wrapped with a value function (see `aio-resolve')."
  (let ((promise (aio-promise)))
    (prog1 promise
      (run-with-idle-timer seconds nil
                           #'aio-resolve promise (lambda () result)))))

(defun aio-timeout (seconds)
  "Create a promise with a timeout error after SECONDS."
  (let ((timeout (aio-promise)))
    (prog1 timeout
      (run-at-time seconds nil#'aio-resolve timeout
                   (lambda () (signal 'aio-timeout seconds))))))

(defun aio-url-retrieve (url &optional silent inhibit-cookies)
  "Wraps `url-retrieve' in a promise.

This function will never directly signal an error. Instead any
errors will be delivered via the returned promise. The promise
result is a cons of (status . buffer). This buffer is a clone of
the buffer created by `url-retrieve' and should be killed by the
caller."
  (let ((promise (aio-promise)))
    (prog1 promise
      (condition-case error
          (url-retrieve url (lambda (status)
                              (let ((value (cons status (clone-buffer))))
                                (aio-resolve promise (lambda () value))))
                        silent inhibit-cookies)
        (error (aio-resolve promise
                            (lambda ()
                              (signal (car error) (cdr error)))))))))

(cl-defun aio-make-callback (&key tag once)
  "Return a new callback function and its first promise.

Returns a cons (callback . promise) where callback is function
suitable for repeated invocation. This makes it useful for
process filters and sentinels. The promise is the first promise
to be resolved by the callback.

The promise resolves to:
  (next-promise . callback-args)
Or when TAG is supplied:
  (next-promise TAG . callback-args)
Or if ONCE is non-nil:
  callback-args

The callback resolves next-promise on the next invocation. This
creates a chain of promises representing the sequence of calls.
Note: To avoid keeping lots of garbage in memory, avoid holding
onto the first promise (i.e. capturing it in a closure).

The `aio-chain' macro makes it easier to use these promises."
  (let* ((promise (aio-promise))
         (callback
          (if once
              (lambda (&rest args)
                (let ((result (if tag
                                  (cons tag args)
                                args)))
                  (aio-resolve promise (lambda () result))))
            (lambda (&rest args)
              (let* ((next-promise (aio-promise))
                     (result (if tag
                                 (cons next-promise (cons tag args))
                               (cons next-promise args))))
                (aio-resolve promise (lambda () result))
                (setf promise next-promise))))))
    (cons callback promise)))

;; A simple little queue

(defsubst aio--queue-empty-p (queue)
  "Return non-nil if QUEUE is empty.
An empty queue is (nil . nil)."
  (null (caar queue)))

(defsubst aio--queue-get (queue)
  "Get the next item from QUEUE, or nil for empty."
  (let ((head (car queue)))
    (cond ((null head)
           nil)
          ((eq head (cdr queue))
           (prog1 (car head)
             (setf (car queue) nil
                   (cdr queue) nil)))
          ((prog1 (car head)
             (setf (car queue) (cdr head)))))))

(defsubst aio--queue-put (queue element)
  "Append ELEMENT to QUEUE, returning ELEMENT."
  (let ((new (list element)))
    (prog1 element
      (if (null (car queue))
          (setf (car queue) new
                (cdr queue) new)
        (setf (cdr (cdr queue)) new
              (cdr queue) new)))))

;; An efficient select()-like interface for promises

(defun aio-make-select (&optional promises)
  "Create a new `aio-select' object for waiting on multiple promises."
  (let ((select (record 'aio-select
                        ;; Membership table
                        (make-hash-table :test 'eq)
                        ;; "Seen" table (avoid adding multiple callback)
                        (make-hash-table :test 'eq :weakness 'key)
                        ;; Queue of pending resolved promises
                        (cons nil nil)
                        ;; Callback to resolve select's own promise
                        nil)))
    (prog1 select
      (dolist (promise promises)
        (aio-select-add select promise)))))

(defun aio-select-add (select promise)
  "Add PROMISE to the set of promises in SELECT.

SELECT is created with `aio-make-select'. It is valid to add a
promise that was previously removed."
  (let ((members (aref select 1))
        (seen (aref select 2)))
    (prog1 promise
      (unless (gethash promise seen)
        (setf (gethash promise seen) t
              (gethash promise members) t)
        (aio-listen promise
                    (lambda (_)
                      (when (gethash promise members)
                        (aio--queue-put (aref select 3) promise)
                        (remhash promise members)
                        (let ((callback (aref select 4)))
                          (when callback
                            (setf (aref select 4) nil)
                            (funcall callback))))))))))

(defun aio-select-remove (select promise)
  "Remove PROMISE form the set of promises in SELECT.

SELECT is created with `aio-make-select'."
  (remhash promise (aref select 1)))

(defun aio-select-promises (select)
  "Return a list of promises in SELECT.

SELECT is created with `aio-make-select'."
  (cl-loop for key being the hash-keys of (aref select 1)
           collect key))

(defun aio-select (select)
  "Return a promise that resolves when any promise in SELECT resolves.

SELECT is created with `aio-make-select'. This function is
level-triggered: if a promise in SELECT is already resolved, it
returns immediately with that promise. Promises returned by
`aio-select' are automatically removed from SELECT. Use this
function to repeatedly wait on a set of promises.

Note: The promise returned by this function resolves to another
promise, not that promise's result. You will need to `aio-await'
on it, or use `aio-result'."
  (let* ((result (aio-promise))
         (callback (lambda ()
                     (let ((promise (aio--queue-get (aref select 3))))
                       (aio-resolve result (lambda () promise))))))
    (prog1 result
      (if (aio--queue-empty-p (aref select 3))
          (setf (aref select 4) callback)
        (funcall callback)))))

;; Semaphores

(defun aio-sem (init)
  "Create a new semaphore with initial value INIT."
  (record 'aio-sem
          ;; Semaphore value
          init
          ;; Queue of waiting async functions
          (cons nil nil)))

(defun aio-sem-post (sem)
  "Increment the value of SEM.

If asynchronous functions are awaiting on SEM, then one will be
woken up. This function is not awaitable."
  (when (<= (cl-incf (aref sem 1)) 0)
    (let ((waiting (aio--queue-get (aref sem 2))))
      (when waiting
        (aio-resolve waiting (lambda () nil))))))

(defun aio-sem-wait (sem)
  "Decrement the value of SEM.

If SEM is at zero, returns a promise that will resolve when
another asynchronous function uses `aio-sem-post'."
  (when (< (cl-decf (aref sem 1)) 0)
    (aio--queue-put (aref sem 2) (aio-promise))))

;; `emacs-lisp-mode' font lock

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(aio-defun" (+ blank)
         (group (+ (or (syntax word) (syntax symbol)))))
    1 'font-lock-function-name-face)))

(add-hook 'help-fns-describe-function-functions #'aio-describe-function)

(defun aio-describe-function (function)
  "Insert whether FUNCTION is an asynchronous function.
This function is added to ‘help-fns-describe-function-functions’."
  (when (function-get function 'aio-defun-p)
    (insert "  This function is asynchronous; it returns "
            "an ‘aio-promise’ object.\n")))

(provide 'aio)

;;; aio.el ends here
