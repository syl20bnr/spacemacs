;;; async.el --- Asynchronous processing in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.

;; Author: John Wiegley <jwiegley@gmail.com>
;; Maintainer: Thierry Volpiatto <thievol@posteo.net>

;; Created: 18 Jun 2012
;; Version: 1.9.8
;; Package-Requires: ((emacs "24.4"))

;; Keywords: async
;; X-URL: https://github.com/jwiegley/emacs-async

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds the ability to call asynchronous functions and process with ease.  See
;; the documentation for `async-start' and `async-start-process'.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar tramp-password-prompt-regexp)

(defgroup async nil
  "Simple asynchronous processing in Emacs"
  :group 'lisp)

(defcustom async-variables-noprops-function #'async--purecopy
  "Default function to remove text properties in variables."
  :type 'function)

(defcustom async-prompt-for-password t
  "Prompt for password in parent Emacs if needed when non nil.
When this is nil child Emacs will hang forever when a user interaction
for password is required unless a password is stored in a \".authinfo\" file."
  :type 'boolean)

(defvar async-debug nil)
(defvar async-send-over-pipe t)
(defvar async-in-child-emacs nil)
(defvar async-callback nil)
(defvar async-callback-for-process nil
  "Non-nil if the subprocess is not Emacs executing a lisp form.")
(defvar async-callback-value nil)
(defvar async-callback-value-set nil)
(defvar async-current-process nil)
(defvar async--procvar nil)
(defvar async-read-marker nil
  "Position from which we read the last message packet.

Message packets are delivered from client line-by-line as base64
encoded strings.")
(defvar async-child-init nil
  "Initialisation file for async child Emacs.

If defined this allows for an init file to setup the child Emacs. It
should not be your normal init.el as that would likely load more
things that you require. It should limit itself to ensuring paths have
been setup so any async code can load libraries you expect.")

;; For emacs<29 (only exists in emacs-29+).
(defvar print-symbols-bare)

(defun async--purecopy (object)
  "Remove text properties in OBJECT.

Argument OBJECT may be a list or a string, if anything else it
is returned unmodified."
  (cond ((stringp object)
         (substring-no-properties object))
        ((consp object)
         (cl-loop for elm in object
                  ;; A string.
                  if (stringp elm)
                  collect (substring-no-properties elm)
                  else
                  ;; Proper lists.
                  if (and (consp elm) (null (cdr (last elm))))
                  collect (async--purecopy elm)
                  else
                  ;; Dotted lists.
                  ;; We handle here only dotted list where car and cdr
                  ;; are atoms i.e. (x . y) and not (x . (x . y)) or
                  ;; (x . (x y)) which should fit most cases.
                  if (and (consp elm) (cdr (last elm)))
                  collect (let ((key (car elm))
                                (val (cdr elm)))
                            (cons (if (stringp key)
                                      (substring-no-properties key)
                                    key)
                                  (if (stringp val)
                                      (substring-no-properties val)
                                    val)))
                  else
                  collect elm))
        (t object)))

(defun async-inject-variables
    (include-regexp &optional predicate exclude-regexp noprops)
  "Return a `setq' form that replicates part of the calling environment.

It sets the value for every variable matching INCLUDE-REGEXP and
also PREDICATE.  It will not perform injection for any variable
matching EXCLUDE-REGEXP (if present) or representing a `syntax-table'
i.e. ending by \"-syntax-table\".
When NOPROPS is non nil it tries to strip out text properties of each
variable's value with `async-variables-noprops-function'.

It is intended to be used as follows:

    (async-start
       \\=`(lambda ()
          (require \\='smtpmail)
          (with-temp-buffer
            (insert ,(buffer-substring-no-properties (point-min) (point-max)))
            ;; Pass in the variable environment for smtpmail
            ,(async-inject-variables \"\\\\=`\\(smtpmail\\|\\(user-\\)?mail\\)-\")
            (smtpmail-send-it)))
       \\='ignore)"
  `(setq
    ,@(let (bindings)
        (mapatoms
         (lambda (sym)
           (let* ((sname (and (boundp sym) (symbol-name sym)))
                  (value (and sname (symbol-value sym))))
             (when (and sname
                        (or (null include-regexp)
                            (string-match include-regexp sname))
                        (or (null exclude-regexp)
                            (not (string-match exclude-regexp sname)))
                        (not (string-match "-syntax-table\\'" sname)))
               (unless (or (stringp value)
                           (memq value '(nil t))
                           (numberp value)
                           (vectorp value))
                 (setq value `(quote ,value)))
               (when noprops
                 (setq value (funcall async-variables-noprops-function
                                      value)))
               (when (or (null predicate)
                         (funcall predicate sym))
                 (setq bindings (cons value bindings)
                       bindings (cons sym bindings)))))))
        bindings)))

(defalias 'async-inject-environment 'async-inject-variables)

(defun async-handle-result (func result buf)
  (if (null func)
      (progn
        (set (make-local-variable 'async-callback-value) result)
        (set (make-local-variable 'async-callback-value-set) t))
    (unwind-protect
        (if (and (listp result)
                 (eq 'async-signal (nth 0 result)))
            (signal (car (nth 1 result))
                    (cdr (nth 1 result)))
          (funcall func result))
      (unless async-debug
        (kill-buffer buf)))))

(defun async-when-done (proc &optional _change)
  "Process sentinel used to retrieve the value from the child process."
  (when (eq 'exit (process-status proc))
    (with-current-buffer (process-buffer proc)
      (let ((async-current-process proc))
        (if (= 0 (process-exit-status proc))
            (if async-callback-for-process
                (if async-callback
                    (prog1
                        (funcall async-callback proc)
                      (unless async-debug
                        ;; we need to check this because theoretically
                        ;; `async-callback' could've killed it already
                        (when (buffer-live-p (process-buffer proc))
                          (kill-buffer (process-buffer proc)))))
                  (set (make-local-variable 'async-callback-value) proc)
                  (set (make-local-variable 'async-callback-value-set) t))
              ;; Maybe strip out unreadable "#"; They are replaced by
              ;; empty string unless they are prefixing a special
              ;; object like a marker. See issue #145.
              (widen)
              (goto-char (point-min))
              (save-excursion
                ;; Transform markers in list like
                ;; (marker (moves after insertion) at 2338 in
                ;; test\.org) so that remap text properties function
                ;; can parse it to restitute marker.
                (while (re-search-forward "#<\\([^>]*\\)>" nil t)
                  (replace-match (concat "(" (match-string 1) ")") t t)))
              (while (re-search-forward "#(" nil t)
                (replace-match "(" t t))
              (goto-char (point-max))
              (backward-sexp)
              (let ((value (read (current-buffer))))
                (async-handle-result async-callback value (current-buffer))))
          (set (make-local-variable 'async-callback-value)
               (list 'error
                     (format "Async process '%s' failed with exit code %d"
                             (process-name proc) (process-exit-status proc))))
          (set (make-local-variable 'async-callback-value-set) t))))))

(defun async-read-from-client (proc string &optional prompt-for-pwd)
  "Process text from client process.

The string chunks usually arrive in maximum of 4096 bytes, so a
long client message might be split into multiple calls of this
function.

We use a marker `async-read-marker' to track the position of the
lasts complete line.  Every time we get new input, we try to look
for newline, and if found, process the entire line and bump the
marker position to the end of this next line.

Argument PROMPT-FOR-PWD allow binding lexically the value of
`async-prompt-for-password', if unspecified its global value
is used."
  (with-current-buffer (process-buffer proc)
    (when (and prompt-for-pwd
               (boundp 'tramp-password-prompt-regexp)
               tramp-password-prompt-regexp
               (string-match tramp-password-prompt-regexp string))
      (process-send-string
       proc (concat (read-passwd (match-string 0 string)) "\n")))
    (goto-char (point-max))
    (save-excursion
      (insert string))

    (while (search-forward "\n" nil t)
      (save-excursion
        (save-restriction
          (widen)
          (narrow-to-region async-read-marker (point))
          (goto-char (point-min))
          (let (msg)
            (condition-case nil
                ;; It is safe to throw errors in the read because we
                ;; send messages always on their own line, and they
                ;; are always a base64 encoded string, so a message
                ;; will always read.  We will also ignore the rest
                ;; of this line since there won't be anything
                ;; interesting.
                (while (setq msg (read (current-buffer)))
                  (let ((msg-decoded (ignore-errors (base64-decode-string msg))))
                    (when msg-decoded
                      (setq msg-decoded (car (read-from-string msg-decoded)))
                      (when (and (listp msg-decoded)
                                 (async-message-p msg-decoded)
                                 async-callback)
                        (funcall async-callback msg-decoded)))))
              ;; This is OK, we reached the end of the chunk subprocess sent
              ;; at this time.
              (invalid-read-syntax t)
              (end-of-file t)))
          (goto-char (point-max))
          (move-marker async-read-marker (point)))))))

(defun async--receive-sexp (&optional stream)
  ;; FIXME: Why use `utf-8-auto' instead of `utf-8-unix'?  This is
  ;; a communication channel over which we have complete control,
  ;; so we get to choose exactly which encoding and EOL we use, isn't
  ;; it?
  ;; UPDATE: We use now `utf-8-emacs-unix' instead of `utf-8-auto' as
  ;; recommended in bug#165.
  (let ((sexp (decode-coding-string (base64-decode-string (read stream))
                                    'utf-8-emacs-unix))
        ;; Parent expects UTF-8 encoded text.
        (coding-system-for-write 'utf-8-emacs-unix))
    (if async-debug
        (message "Received sexp {{{%s}}}" (pp-to-string sexp)))
    (setq sexp (read sexp))
    (if async-debug
        (message "Read sexp {{{%s}}}" (pp-to-string sexp)))
    (eval sexp t)))

(defun async--insert-sexp (sexp)
  (let (print-level
        print-length
        (print-escape-nonascii t)
        (print-circle t)
        ;; Fix bug#153 in emacs-29 with symbol's positions.
        (print-symbols-bare t))
    (prin1 sexp (current-buffer))
    ;; Just in case the string we're sending might contain EOF
    (encode-coding-region (point-min) (point-max) 'utf-8-emacs-unix)
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min)) (insert ?\")
    (goto-char (point-max)) (insert ?\" ?\n)))

(defun async--transmit-sexp (process sexp)
  (with-temp-buffer
    (if async-debug
        (message "Transmitting sexp {{{%s}}}" (pp-to-string sexp)))
    (async--insert-sexp sexp)
    (process-send-region process (point-min) (point-max))))

(defun async-batch-invoke ()
  "Called from the child Emacs process' command line."
  ;; Make sure 'message' and 'prin1' encode stuff in UTF-8, as parent
  ;; process expects.
  (let ((coding-system-for-write 'utf-8-emacs-unix)
        (args-left command-line-args-left))
    (setq async-in-child-emacs t
          debug-on-error async-debug
          command-line-args-left nil)
    (condition-case-unless-debug err
        (let ((ret (funcall
                    (async--receive-sexp (unless async-send-over-pipe
                                           args-left)))))
          ;; The newlines makes client messages more robust and also
          ;; handle some weird line-buffering issues on windows.
          ;; Sometimes, the last "chunk" was not read by the filter,
          ;; so a newline here should force a buffer flush.
          (princ "\n")
          (prin1 ret)
          (princ "\n"))
      (error
       (progn
         (princ "\n")
         (prin1 (list 'async-signal err))
         (princ "\n"))))))

(defun async-ready (future)
  "Query a FUTURE to see if it is ready.

I.e., if no blocking would result from a call to `async-get' on that FUTURE."
  (and (memq (process-status future) '(exit signal))
       (let ((buf (process-buffer future)))
         (if (buffer-live-p buf)
             (with-current-buffer buf
               async-callback-value-set)
           t))))

(defun async-wait (future)
  "Wait for FUTURE to become ready."
  (while (not (async-ready future))
    (sleep-for 0.05)))

(defun async-get (future)
  "Get the value from process FUTURE when it is ready.
FUTURE is returned by `async-start' or `async-start-process' when
its FINISH-FUNC is nil."
  (and future (async-wait future))
  (let ((buf (process-buffer future)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (async-handle-result
         #'identity async-callback-value (current-buffer))))))

(defun async-message-p (value)
  "Return non-nil if VALUE is an async.el message packet."
  (and (listp value)
       (plist-get value :async-message)))

(defun async-send (process-or-key &rest args)
  "Send the given message to the asynchronous child or parent Emacs.

To send messages from the parent to a child, PROCESS-OR-KEY is
the child process object.  ARGS is a plist.  Example:

  (async-send proc :operation :load-file :file \"this file\")

To send messages from the child to the parent, PROCESS-OR-KEY is
the first key of the plist, ARGS is a value followed by
optionally more key-value pairs.  Example:

  (async-send :status \"finished\" :file-size 123)"
  (let ((args (append args '(:async-message t))))
    (if async-in-child-emacs
        ;; `princ' because async--insert-sexp already quotes everything.
        (princ
         (with-temp-buffer
           (async--insert-sexp (cons process-or-key args))
           ;; always make sure that one message package has its own
           ;; line as there can be any random debug garbage printed
           ;; above it.
           (concat "\n" (buffer-string))))
      (async--transmit-sexp process-or-key (list 'quote args)))))

(defun async-receive ()
  "Receive message from parent Emacs.

The child process blocks until a message is received.

Message is a plist with one key :async-message set to t always
automatically added to signify this plist is an async message.

You can use `async-message-p' to test if the payload was a
message.

Use

   (let ((msg (async-receive))) ...)

to read and process a message."
  (async--receive-sexp))

;;;###autoload
(defun async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (buf-err (generate-new-buffer (concat "*" name ":err*")))
         (prt-for-pwd async-prompt-for-password)
         (proc (let ((process-connection-type nil))
                 (make-process
                  :name name
                  :buffer buf
                  :stderr buf-err
                  :command (cons program program-args)))))
    (set-process-sentinel
     (get-buffer-process buf-err)
     (lambda (proc _change)
       (unless (or async-debug (process-live-p proc))
         (kill-buffer (process-buffer proc)))))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set (make-local-variable 'async-read-marker)
           (set-marker (make-marker) (point-min) buf))
      (set-marker-insertion-type async-read-marker nil)
      (set-process-sentinel proc #'async-when-done)
      ;; Pass the value of `async-prompt-for-password' to the process
      ;; filter fn through the lexical local var prt-for-pwd (Issue#182).
      (set-process-filter proc (lambda (proc string)
                                 (async-read-from-client
                                  proc string prt-for-pwd)))
      (unless (string= name "emacs")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

(defvar async-quiet-switch "-Q"
  "The Emacs parameter to use to call emacs without config.
Can be one of \"-Q\" or \"-q\".
Default is \"-Q\" but it is sometimes useful to use \"-q\" to have a
enhanced config or some more variables loaded.")

(defun async--emacs-program-args (&optional sexp)
  "Return a list of arguments for invoking the child Emacs."
  ;; Using `locate-library' ensure we use the right file
  ;; when the .elc have been deleted.
  (let ((args (list async-quiet-switch "-l" (locate-library "async"))))
    (when async-child-init
      (setq args (append args (list "-l" async-child-init))))
    (append args (list "-batch" "-f" "async-batch-invoke"
                       (if sexp
                           (with-temp-buffer
                             (async--insert-sexp (list 'quote sexp))
                             (buffer-string))
                           "<none>")))))

;;;###autoload
(defun async-start (start-func &optional finish-func)
  "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If you call `async-send' from a child process, the message will
be also passed to the FINISH-FUNC.  You can test RESULT to see if
it is a message by using `async-message-p'.  If nil, it means
this is the final result.  Example of the FINISH-FUNC:

    (lambda (result)
      (if (async-message-p result)
          (message \"Received a message from child process: %s\" result)
        (message \"Async process done, result: %s\" result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     \\='ignore)

Special case:
If the output of START-FUNC is a string with properties
e.g. (buffer-string) RESULT will be transformed in a list where the
car is the string itself (without props) and the cdr the rest of
properties, this allows using in FINISH-FUNC the string without
properties and then apply the properties in cdr to this string (if
needed).
Properties handling special objects like markers are returned as
list to allow restoring them later.
See <https://github.com/jwiegley/emacs-async/issues/145> for more infos.

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'."
  (let ((sexp start-func)
        ;; Subordinate Emacs will send text encoded in UTF-8.
        (coding-system-for-read 'utf-8-emacs-unix))
    (setq async--procvar
          (apply 'async-start-process
                 "emacs" (file-truename
                          (expand-file-name invocation-name
                                            invocation-directory))
                 finish-func
                 (async--emacs-program-args (if (not async-send-over-pipe) sexp))))

    (if async-send-over-pipe
        (async--transmit-sexp async--procvar (list 'quote sexp)))
    async--procvar))

(defmacro async-sandbox(func)
  "Evaluate FUNC in a separate Emacs process, synchronously."
  `(async-get (async-start ,func)))

(defun async--fold-left (fn forms bindings)
  (let ((res forms))
    (dolist (binding bindings)
      (setq res (funcall fn res
                         (if (listp binding)
                             binding
                           (list binding)))))
    res))

(defmacro async-let (bindings &rest forms)
  "Implements `let', but each binding is established asynchronously.
For example:

  (async-let ((x (foo))
              (y (bar)))
     (message \"%s %s\" x y))

    expands to ==>

  (async-start (foo)
   (lambda (x)
     (async-start (bar)
      (lambda (y)
        (message \"%s %s\" x y)))))"
  (declare (indent 1))
  (async--fold-left
   (lambda (acc binding)
     (let ((fun (pcase (cadr binding)
                  ((and (pred functionp) f) f)
                  (f `(lambda () ,f)))))
       `(async-start ,fun
                     (lambda (,(car binding))
                       ,acc))))
   `(progn ,@forms)
   (reverse bindings)))

(provide 'async)

;;; async.el ends here
