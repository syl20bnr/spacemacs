# aio: async/await for Emacs Lisp

`aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python. This
package builds upon Emacs 25 generators to provide functions that
pause while they wait on asynchronous events. They do not block any
thread while paused.

Introduction: [An Async / Await Library for Emacs Lisp][post]

Installation is [available through MELPA][melpa]. Since it uses the
`record` built-in, it requires Emacs 26 or later.

## Usage

An async function is defined using `aio-defun` or `aio-lambda`. The
body of such functions can use `aio-await` to pause the function and
wait on a given promise. The function continues with the promise's
resolved value when it's ready. The package provides a number of
functions that return promises, and every async function returns a
promise representing its future return value.

For example:

```el
(aio-defun foo (url)
  (aio-await (aio-sleep 3))
  (message "Done sleeping. Now fetching %s" url)
  (let* ((result (aio-await (aio-url-retrieve url)))
         (contents (with-current-buffer (cdr result)
                     (prog1 (buffer-string)
                       (kill-buffer)))))
    (message "Result: %s" contents)))
```

If an uncaught signal terminates an asynchronous function, that signal
is captured by its return value promise and propagated into any
function that awaits on that function.

```el
(aio-defun divide (a b)
  (aio-await (aio-sleep 1))
  (/ a b))

(aio-defun divide-safe (a b)
  (condition-case error
      (aio-await (divide a b))
    (arith-error :arith-error)))

(aio-wait-for (divide-safe 1.0 2.0))
;; => 0.5

(aio-wait-for (divide-safe 0 0))
;; => :arith-error
```

To convert a callback-based function into an awaitable, async-friendly
function, create a new promise object with `aio-promise`, then
`aio-resolve` that promise in the callback. The helper function,
`aio-make-callback`, makes this easy.

## Utility macros and functions

```el
(aio-wait-for promise)
;; Synchronously wait for PROMISE, blocking the current thread.

(aio-cancel promise)
;; Attempt to cancel PROMISE, returning non-nil if successful.

(aio-with-promise promise &rest body) [macro]
;; Evaluate BODY and resolve PROMISE with the result.

(aio-with-async &rest body) [macro]
;; Evaluate BODY asynchronously as if it was inside `aio-lambda'.

(aio-make-callback &key tag once)
;; Return a new callback function and its first promise.

(aio-chain expr) [macro]
;; `aio-await' on EXPR and replace place EXPR with the next promise.
```

The `aio-make-callback` function is especially useful for callbacks
that are invoked repeatedly, such as process filters and sentinels.
The `aio-chain` macro works in conjunction.

## Awaitable functions

Here are some useful promise-returning — i.e. awaitable — functions
defined by this package.

```el
(aio-sleep seconds &optional result)
;; Return a promise that is resolved after SECONDS with RESULT.

(aio-idle seconds &optional result)
;; Return a promise that is resolved after idle SECONDS with RESULT.

(aio-url-retrieve url &optional silent inhibit-cookies)
;; Wraps `url-retrieve' in a promise.

(aio-all promises)
;; Return a promise that resolves when all PROMISES are resolved."
```

## Select API

This package includes a select()-like, level-triggered API for waiting
on multiple promises at once. Create a "select" object, add promises
to it, and await on it. Resolved and returned promises are
automatically removed, and the "select" object can be reused.

```el
(aio-make-select &optional promises)
;; Create a new `aio-select' object for waiting on multiple promises.

(aio-select-add select promise)
;; Add PROMISE to the set of promises in SELECT.

(aio-select-remove select promise)
;; Remove PROMISE form the set of promises in SELECT.

(aio-select-promises select)
;; Return a list of promises in SELECT.

(aio-select select)
;; Return a promise that resolves when any promise in SELECT resolves.
```

For example, here's an implementation of sleep sort:

```el
(aio-defun sleep-sort (values)
  (let* ((promises (mapcar (lambda (v) (aio-sleep v v)) values))
         (select (aio-make-select promises)))
    (cl-loop repeat (length promises)
             for next = (aio-await (aio-select select))
             collect (aio-await next))))
```

## Semaphore API

Semaphores work just as they would as a thread synchronization
primitive. There's an internal counter that cannot drop below zero,
and `aio-sem-wait` is an awaitable function that may block the
asynchronous function until another asynchronous function calls
`aio-sem-post`. Blocked functions wait in a FIFO queue and are awoken
in the same order that they awaited.

```el
(aio-sem init)
;; Create a new semaphore with initial value INIT.

(aio-sem-post sem)
;; Increment the value of SEM.

(aio-sem-wait sem)
;; Decrement the value of SEM.
```

This can be used to create a work queue. For example, here's a
configurable download queue for `url-retrieve`:

```el
(defun fetch (url-list max-parallel callback)
  (let ((sem (aio-sem max-parallel)))
    (dolist (url url-list)
      (aio-with-async
        (aio-await (aio-sem-wait sem))
        (cl-destructuring-bind (status . buffer)
            (aio-await (aio-url-retrieve url))
          (aio-sem-post sem)
          (funcall callback
                   (with-current-buffer buffer
                     (prog1 (buffer-string)
                       (kill-buffer)))))))))
```


[asyncio]: https://docs.python.org/3/library/asyncio.html
[melpa]: https://melpa.org/#/aio
[post]: https://nullprogram.com/blog/2019/03/10/
