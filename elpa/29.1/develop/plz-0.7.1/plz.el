;;; plz.el --- HTTP library                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/plz.el
;; Version: 0.7.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: comm, network, http

;; This file is part of GNU Emacs.

;;; License:

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
;;
;; An HTTP library that uses curl as a backend.  Inspired by, and some
;; code copied from, Christopher Wellons's library, elfeed-curl.el.
;;
;; Why this package?
;;
;; 1.  `url' works well for many things, but it has some issues.
;; 2.  `request' works well for many things, but it has some issues.
;; 3.  Chris Wellons doesn't have time to factor his excellent
;;     elfeed-curl.el library out of Elfeed.  This will have to do.
;;
;; Why is it called `plz'?
;;
;; 1.  There's already a package called `http'.
;; 2.  There's already a package called `request'.
;; 3.  Naming things is hard.

;;;; Usage:

;; FIXME(v0.8): Remove the following note.

;; NOTE: In v0.8 of plz, only one error will be signaled: `plz-error'.
;; The existing errors, `plz-curl-error' and `plz-http-error', inherit
;; from `plz-error' to allow applications to update their code while
;; using v0.7 (i.e. any `condition-case' forms should now handle only
;; `plz-error', not the other two).

;; Call function `plz' to make an HTTP request.  Its docstring
;; explains its arguments.  `plz' also supports other HTTP methods,
;; uploading and downloading binary files, sending URL parameters and
;; HTTP headers, configurable timeouts, error-handling "else" and
;; always-called "finally" functions, and more.

;; Basic usage is simple.  For example, to make a synchronous request
;; and return the HTTP response body as a string:
;;
;;   (plz 'get "https://httpbin.org/get")
;;
;; Which returns the JSON object as a string:
;;
;;   "{
;;     \"args\": {},
;;     \"headers\": {
;;       \"Accept\": \"*/*\",
;;       \"Accept-Encoding\": \"deflate, gzip\",
;;       \"Host\": \"httpbin.org\",
;;       \"User-Agent\": \"curl/7.35.0\"
;;     },
;;     \"origin\": \"xxx.xxx.xxx.xxx\",
;;     \"url\": \"https://httpbin.org/get\"
;;   }"
;;
;; To make the same request asynchronously, decoding the JSON and
;; printing a message with a value from it:
;;
;;   (plz 'get "https://httpbin.org/get" :as #'json-read
;;     :then (lambda (alist) (message "URL: %s" (alist-get 'url alist))))
;;
;; Which, after the request returns, prints:
;;
;;   URL: https://httpbin.org/get

;;;; Credits:

;; Thanks to Chris Wellons for inspiration, encouragement, and advice.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

;;;; Errors

(define-error 'plz-error "plz error")
(define-error 'plz-curl-error "plz: Curl error" 'plz-error)
(define-error 'plz-http-error "plz: HTTP error" 'plz-error)

;;;; Structs

(cl-defstruct plz-response
  version status headers body)

(cl-defstruct plz-error
  curl-error response message)

;;;; Constants

(defconst plz-http-response-status-line-regexp
  (rx "HTTP/" (group (or "1.0" "1.1" "2")) " "
      ;; Status code
      (group (1+ digit)) " "
      ;; Reason phrase
      (optional (group (1+ (not (any "\r\n")))))
      (or
       ;; HTTP 1
       "\r\n"
       ;; HTTP 2
       "\n"))
  "Regular expression matching HTTP response status line.")

(defconst plz-http-end-of-headers-regexp
  (rx (or "\r\n\r\n" "\n\n"))
  "Regular expression matching the end of HTTP headers.
This must work with both HTTP/1 (using CRLF) and HTTP/2 (using
only LF).")

(defconst plz-curl-errors
  ;; Copied from elfeed-curl.el.
  '((1 . "Unsupported protocol.")
    (2 . "Failed to initialize.")
    (3 . "URL malformed. The syntax was not correct.")
    (4 . "A feature or option that was needed to perform the desired request was not enabled or was explicitly disabled at build-time.")
    (5 . "Couldn't resolve proxy. The given proxy host could not be resolved.")
    (6 . "Couldn't resolve host. The given remote host was not resolved.")
    (7 . "Failed to connect to host.")
    (8 . "FTP weird server reply. The server sent data curl couldn't parse.")
    (9 . "FTP access denied.")
    (11 . "FTP weird PASS reply.")
    (13 . "FTP weird PASV reply.")
    (14 . "FTP weird 227 format.")
    (15 . "FTP can't get host.")
    (17 . "FTP couldn't set binary.")
    (18 . "Partial file. Only a part of the file was transferred.")
    (19 . "FTP couldn't download/access the given file, the RETR (or similar) command failed.")
    (21 . "FTP quote error. A quote command returned error from the server.")
    (22 . "HTTP page not retrieved.")
    (23 . "Write error.")
    (25 . "FTP couldn't STOR file.")
    (26 . "Read error. Various reading problems.")
    (27 . "Out of memory. A memory allocation request failed.")
    (28 . "Operation timeout.")
    (30 . "FTP PORT failed.")
    (31 . "FTP couldn't use REST.")
    (33 . "HTTP range error. The range \"command\" didn't work.")
    (34 . "HTTP post error. Internal post-request generation error.")
    (35 . "SSL connect error. The SSL handshaking failed.")
    (36 . "FTP bad download resume.")
    (37 . "FILE couldn't read file.")
    (38 . "LDAP bind operation failed.")
    (39 . "LDAP search failed.")
    (41 . "Function not found. A required LDAP function was not found.")
    (42 . "Aborted by callback.")
    (43 . "Internal error. A function was called with a bad parameter.")
    (45 . "Interface error. A specified outgoing interface could not be used.")
    (47 . "Too many redirects.")
    (48 . "Unknown option specified to libcurl.")
    (49 . "Malformed telnet option.")
    (51 . "The peer's SSL certificate or SSH MD5 fingerprint was not OK.")
    (52 . "The server didn't reply anything, which here is considered an error.")
    (53 . "SSL crypto engine not found.")
    (54 . "Cannot set SSL crypto engine as default.")
    (55 . "Failed sending network data.")
    (56 . "Failure in receiving network data.")
    (58 . "Problem with the local certificate.")
    (59 . "Couldn't use specified SSL cipher.")
    (60 . "Peer certificate cannot be authenticated with known CA certificates.")
    (61 . "Unrecognized transfer encoding.")
    (62 . "Invalid LDAP URL.")
    (63 . "Maximum file size exceeded.")
    (64 . "Requested FTP SSL level failed.")
    (65 . "Sending the data requires a rewind that failed.")
    (66 . "Failed to initialise SSL Engine.")
    (67 . "The user name, password, or similar was not accepted and curl failed to log in.")
    (68 . "File not found on TFTP server.")
    (69 . "Permission problem on TFTP server.")
    (70 . "Out of disk space on TFTP server.")
    (71 . "Illegal TFTP operation.")
    (72 . "Unknown TFTP transfer ID.")
    (73 . "File already exists (TFTP).")
    (74 . "No such user (TFTP).")
    (75 . "Character conversion failed.")
    (76 . "Character conversion functions required.")
    (77 . "Problem with reading the SSL CA cert (path? access rights?).")
    (78 . "The resource referenced in the URL does not exist.")
    (79 . "An unspecified error occurred during the SSH session.")
    (80 . "Failed to shut down the SSL connection.")
    (82 . "Could not load CRL file, missing or wrong format (added in 7.19.0).")
    (83 . "Issuer check failed (added in 7.19.0).")
    (84 . "The FTP PRET command failed")
    (85 . "RTSP: mismatch of CSeq numbers")
    (86 . "RTSP: mismatch of Session Identifiers")
    (87 . "unable to parse FTP file list")
    (88 . "FTP chunk callback reported error")
    (89 . "No connection available, the session will be queued")
    (90 . "SSL public key does not matched pinned public key"))
  "Alist mapping curl error code integers to helpful error messages.")

;;;; Customization

(defgroup plz nil
  "Options for `plz'."
  :group 'network
  :link '(url-link "https://github.com/alphapapa/plz.el"))

(defcustom plz-curl-program "curl"
  "Name of curl program to call."
  :type 'string)

(defcustom plz-curl-default-args
  '("--silent"
    "--compressed"
    "--location")
  "Default arguments to curl.
Note that these arguments are passed on the command line, which
may be visible to other users on the local system."
  :type '(repeat string))

(defcustom plz-connect-timeout 5
  "Default connection timeout in seconds.
This limits how long the connection phase may last (the
\"--connect-timeout\" argument to curl)."
  :type 'number)

(defcustom plz-timeout 60
  "Default request timeout in seconds.
This limits how long an entire request may take, including the
connection phase and waiting to receive the response (the
\"--max-time\" argument to curl)."
  :type 'number)

;;;; Functions

;;;;; Public

(cl-defun plz (method url &rest rest &key headers body else finally noquery
                      (as 'string) (then 'sync)
                      (body-type 'text) (decode t decode-s)
                      (connect-timeout plz-connect-timeout) (timeout plz-timeout))
  "Request METHOD from URL with curl.
Return the curl process object or, for a synchronous request, the
selected result.

HEADERS may be an alist of extra headers to send with the
request.

BODY may be a string, a buffer, or a list like `(file FILENAME)'
to upload a file from disk.

BODY-TYPE may be `text' to send BODY as text, or `binary' to send
it as binary.

AS selects the kind of result to pass to the callback function
THEN, or the kind of result to return for synchronous requests.
It may be:

- `buffer' to pass the response buffer, which will be narrowed to
  the response body and decoded according to DECODE.

- `binary' to pass the response body as an un-decoded string.

- `string' to pass the response body as a decoded string.

- `response' to pass a `plz-response' structure.

- `file' to pass a temporary filename to which the response body
  has been saved without decoding.

- `(file FILENAME)' to pass FILENAME after having saved the
  response body to it without decoding.  FILENAME must be a
  non-existent file; if it exists, it will not be overwritten,
  and an error will be signaled.

- A function, which is called in the response buffer with it
  narrowed to the response body (suitable for, e.g. `json-read').

If DECODE is non-nil, the response body is decoded automatically.
For binary content, it should be nil.  When AS is `binary',
DECODE is automatically set to nil.

THEN is a callback function, whose sole argument is selected
above with AS; if the request fails and no ELSE function is
given (see below), the argument will be a `plz-error' structure
describing the error.  Or THEN may be `sync' to make a
synchronous request, in which case the result is returned
directly from this function.

ELSE is an optional callback function called when the request
fails (i.e. if curl fails, or if the HTTP response has a non-2xx
status code).  It is called with one argument, a `plz-error'
structure.  If ELSE is nil, a `plz-curl-error' or
`plz-http-error' is signaled when the request fails, with a
`plz-error' structure as the error data.  For synchronous
requests, this argument is ignored.

NOTE: In v0.8 of `plz', only one error will be signaled:
`plz-error'.  The existing errors, `plz-curl-error' and
`plz-http-error', inherit from `plz-error' to allow applications
to update their code while using v0.7 (i.e. any `condition-case'
forms should now handle only `plz-error', not the other two).

FINALLY is an optional function called without argument after
THEN or ELSE, as appropriate.  For synchronous requests, this
argument is ignored.

CONNECT-TIMEOUT and TIMEOUT are a number of seconds that limit
how long it takes to connect to a host and to receive a response
from a host, respectively.

NOQUERY is passed to `make-process', which see.

\(To silence checkdoc, we mention the internal argument REST.)"
  ;; FIXME(v0.8): Remove the note about error changes from the docstring.
  ;; FIXME(v0.8): Update error signals in docstring.
  (declare (indent defun))
  (setf decode (if (and decode-s (not decode))
                   nil decode))
  ;; NOTE: By default, for PUT requests and POST requests >1KB, curl sends an
  ;; "Expect:" header, which causes servers to send a "100 Continue" response, which
  ;; we don't want to have to deal with, so we disable it by setting the header to
  ;; the empty string.  See <https://gms.tf/when-curl-sends-100-continue.html>.
  ;; TODO: Handle "100 Continue" responses and remove this workaround.
  (push (cons "Expect" "") headers)
  (let* ((data-arg (pcase-exhaustive body-type
                     ('binary "--data-binary")
                     ('text "--data")))
         (curl-command-line-args (append plz-curl-default-args
                                         (list "--config" "-")))
         (curl-config-header-args (cl-loop for (key . value) in headers
                                           collect (cons "--header" (format "%s: %s" key value))))
         (curl-config-args (append curl-config-header-args
                                   (list (cons "--url" url))
                                   (when connect-timeout
                                     (list (cons "--connect-timeout"
                                                 (number-to-string connect-timeout))))
                                   (when timeout
                                     (list (cons "--max-time" (number-to-string timeout))))
                                   ;; NOTE: To make a HEAD request
                                   ;; requires using the "--head"
                                   ;; option rather than "--request
                                   ;; HEAD", and doing so with
                                   ;; "--dump-header" duplicates the
                                   ;; headers, so we must instead
                                   ;; specify that for each other
                                   ;; method.
                                   (pcase method
                                     ('get
                                      (list (cons "--dump-header" "-")))
                                     ((or 'put 'post)
                                      (list (cons "--dump-header" "-")
                                            (cons "--request" (upcase (symbol-name method)))
                                            ;; It appears that this must be the last argument
                                            ;; in order to pass data on the rest of STDIN.
                                            (pcase body
                                              (`(file ,filename)
                                               ;; Use `expand-file-name' because curl doesn't
                                               ;; expand, e.g. "~" into "/home/...".
                                               (cons "--upload-file" (expand-file-name filename)))
                                              (_ (cons data-arg "@-")))))
                                     ('delete
                                      (list (cons "--dump-header" "-")
                                            (cons "--request" (upcase (symbol-name method)))))
                                     ('head
                                      (list (cons "--head" "")
                                            (cons "--request" "HEAD"))))))
         (curl-config (cl-loop for (key . value) in curl-config-args
                               concat (format "%s \"%s\"\n" key value)))
         (decode (pcase as
                   ('binary nil)
                   (_ decode)))
         (default-directory
           ;; Avoid making process in a nonexistent directory (in case the current
           ;; default-directory has since been removed).  It's unclear what the best
           ;; directory is, but this seems to make sense, and it should still exist.
           temporary-file-directory)
         (process-buffer (generate-new-buffer " *plz-request-curl*"))
         (stderr-process (make-pipe-process :name "plz-request-curl-stderr"
                                            :buffer (generate-new-buffer " *plz-request-curl-stderr*")
                                            :noquery t
                                            :sentinel #'plz--stderr-sentinel))
         (process (make-process :name "plz-request-curl"
                                :buffer process-buffer
                                :coding 'binary
                                :command (append (list plz-curl-program) curl-command-line-args)
                                :connection-type 'pipe
                                :sentinel #'plz--sentinel
                                :stderr stderr-process
                                :noquery noquery))
         sync-p)
    (when (eq 'sync then)
      (setf sync-p t
            then (lambda (result)
                   (process-put process :plz-result result))
            else nil))
    (setf
     ;; Set the callbacks, etc. as process properties.
     (process-get process :plz-then)
     (pcase-exhaustive as
       ((or 'binary 'string)
        (lambda ()
          (let ((coding-system (or (plz--coding-system) 'utf-8)))
            (pcase as
              ('binary (set-buffer-multibyte nil)))
            (plz--narrow-to-body)
            (when decode
              (decode-coding-region (point) (point-max) coding-system))
            (funcall then (or (buffer-string)
                              (make-plz-error :message (format "buffer-string is nil in buffer:%S" process-buffer)))))))
       ('buffer (progn
                  (setf (process-get process :plz-as) 'buffer)
                  (lambda ()
                    (let ((coding-system (or (plz--coding-system) 'utf-8)))
                      (pcase as
                        ('binary (set-buffer-multibyte nil)))
                      (plz--narrow-to-body)
                      (when decode
                        (decode-coding-region (point) (point-max) coding-system)))
                    (funcall then (current-buffer)))))
       ('response (lambda ()
                    (funcall then (or (plz--response :decode-p decode)
                                      (make-plz-error :message (format "response is nil for buffer:%S  buffer-string:%S"
                                                                       process-buffer (buffer-string)))))))
       ('file (lambda ()
                (set-buffer-multibyte nil)
                (plz--narrow-to-body)
                (let ((filename (make-temp-file "plz-")))
                  (condition-case err
                      (progn
                        (write-region (point-min) (point-max) filename)
                        (funcall then filename))
                    ;; In case of an error writing to the file, delete the temp file
                    ;; and signal the error.  Ignore any errors encountered while
                    ;; deleting the file, which would obscure the original error.
                    (error (ignore-errors
                             (delete-file filename))
                           (funcall then (make-plz-error :message (format "error while writing to file %S: %S" filename err))))))))
       (`(file ,(and (pred stringp) filename))
        (lambda ()
          (set-buffer-multibyte nil)
          (plz--narrow-to-body)
          (condition-case err
              (progn
                (write-region (point-min) (point-max) filename nil nil nil 'excl)
                (funcall then filename))
            ;; Since we are creating the file, it seems sensible to delete it in case of an
            ;; error while writing to it (e.g. a disk-full error).  And we ignore any errors
            ;; encountered while deleting the file, which would obscure the original error.
            (error (ignore-errors
                     (when (file-exists-p filename)
                       (delete-file filename)))
                   (funcall then (make-plz-error :message (format "error while writing to file %S: %S" filename err)))))))
       ((pred functionp) (lambda ()
                           (let ((coding-system (or (plz--coding-system) 'utf-8)))
                             (plz--narrow-to-body)
                             (when decode
                               (decode-coding-region (point) (point-max) coding-system))
                             (funcall then (funcall as))))))
     (process-get process :plz-else) else
     (process-get process :plz-finally) finally
     (process-get process :plz-sync) sync-p
     ;; Record list of arguments for debugging purposes (e.g. when
     ;; using Edebug in a process buffer, this allows determining
     ;; which request the buffer is for).
     (process-get process :plz-args) (apply #'list method url rest)
     ;; HACK: We set the result to a sentinel value so that any other
     ;; value, even nil, means that the response was processed, and
     ;; the sentinel does not need to be called again (see below).
     (process-get process :plz-result) :plz-result)
    ;; Send --config arguments.
    (process-send-string process curl-config)
    (when body
      (cl-typecase body
        (string (process-send-string process body))
        (buffer (with-current-buffer body
                  (process-send-region process (point-min) (point-max))))))
    (process-send-eof process)
    (if sync-p
        (unwind-protect
            (with-local-quit
              ;; See Info node `(elisp)Accepting Output'.
              (unless (and process stderr-process)
                (error "Process unexpectedly nil"))
              (while (accept-process-output process))
              (while (accept-process-output stderr-process))
              (when (eq :plz-result (process-get process :plz-result))
                ;; HACK: Sentinel seems to not have been called: call it again.  (Although
                ;; this is a hack, it seems to be a necessary one due to Emacs's process
                ;; handling.)  See <https://github.com/alphapapa/plz.el/issues/3> and
                ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50166>.
                (plz--sentinel process "finished\n")
                (when (eq :plz-result (process-get process :plz-result))
                  (error "Plz: NO RESULT FROM PROCESS:%S  ARGS:%S"
                         process rest)))
              ;; Sentinel seems to have been called: check the result.
              (pcase (process-get process :plz-result)
                ((and (pred plz-error-p) data)
                 ;; The AS function signaled an error, which was collected
                 ;; into a `plz-error' struct: re-signal the error here,
                 ;; outside of the sentinel.
                 (if (plz-error-response data)
                     ;; FIXME(v0.8): Signal only plz-error.
                     (signal 'plz-http-error (list "HTTP error" data))
                   (signal 'plz-curl-error (list "Curl error" data))))
                (else
                 ;; The AS function returned a value: return it.
                 else)))
          (unless (eq as 'buffer)
            (kill-buffer process-buffer))
          (kill-buffer (process-buffer stderr-process)))
      ;; Async request: return the process object.
      process)))

;;;;; Queue

;; A simple queue system.

(cl-defstruct plz-queued-request
  "Structure representing a queued `plz' HTTP request.
For more details on these slots, see arguments to the function
`plz'."
  method url headers body else finally noquery
  as then body-type decode
  connect-timeout timeout
  next previous process)

(cl-defstruct plz-queue
  "Structure forming a queue for `plz' requests.
The queue may be appended to (the default) and pre-pended to, and
items may be removed from the front of the queue (i.e. by
default, it's FIFO).  Use functions `plz-queue', `plz-run', and
`plz-clear' to queue, run, and clear requests, respectively."
  (limit 1
         :documentation "Number of simultaneous requests.")
  (active nil
          :documentation "Active requests.")
  (requests nil
            :documentation "Queued requests.")
  (canceled-p nil
              :documentation "Non-nil when queue has been canceled.")
  first-active last-active
  first-request last-request
  (finally nil
           :documentation "Function called with no arguments after queue has been emptied or canceled."))

(defun plz-queue (queue &rest args)
  "Queue request for ARGS on QUEUE and return QUEUE.
To pre-pend to QUEUE rather than append, it may be a list of the
form (`prepend' QUEUE).  QUEUE is a `plz-request' queue.  ARGS
are those passed to `plz', which see.  Use `plz-run' to start
making QUEUE's requests."
  (declare (indent defun))
  (cl-assert (not (equal 'sync (plist-get (cddr args) :then))) nil
             "Only async requests may be queued")
  (pcase-let* ((`(,method ,url . ,rest) args)
               (args `(:method ,method :url ,url ,@rest))
               (request (apply #'make-plz-queued-request args)))
    (pcase queue
      (`(prepend ,queue) (plz--queue-prepend request queue))
      (_ (plz--queue-append request queue))))
  queue)

(defun plz--queue-append (request queue)
  "Add REQUEST to end of QUEUE and return QUEUE."
  (cl-check-type request plz-queued-request
                 "REQUEST must be a `plz-queued-request' structure.")
  (cl-check-type queue plz-queue
                 "QUEUE must be a `plz-queue' structure.")
  (when (plz-queue-last-request queue)
    (setf (plz-queued-request-next (plz-queue-last-request queue)) request))
  (setf (plz-queued-request-previous request) (plz-queue-last-request queue)
        (plz-queue-last-request queue) request)
  (unless (plz-queue-first-request queue)
    (setf (plz-queue-first-request queue) request))
  (unless (plz-queue-last-request queue)
    (setf (plz-queue-last-request queue) request))
  (push request (plz-queue-requests queue))
  queue)

(defun plz--queue-prepend (request queue)
  "Add REQUEST to front of QUEUE and return QUEUE."
  (cl-check-type request plz-queued-request
                 "REQUEST must be a `plz-queued-request' structure.")
  (cl-check-type queue plz-queue
                 "QUEUE must be a `plz-queue' structure.")
  (when (plz-queue-requests queue)
    (setf (plz-queued-request-next request) (car (plz-queue-requests queue))
          (plz-queued-request-previous (plz-queued-request-next request)) request))
  (setf (plz-queue-first-request queue) request)
  (unless (plz-queue-first-request queue)
    (setf (plz-queue-first-request queue) request))
  (unless (plz-queue-last-request queue)
    (setf (plz-queue-last-request queue) request))
  (push request (plz-queue-requests queue))
  queue)

(defun plz--queue-pop (queue)
  "Return the first queued request on QUEUE and remove it from QUEUE."
  (let* ((request (plz-queue-first-request queue))
         (next (plz-queued-request-next request)))
    (when next
      (setf (plz-queued-request-previous next) nil))
    (setf (plz-queue-first-request queue) next
          (plz-queue-requests queue) (delq request (plz-queue-requests queue)))
    (when (eq request (plz-queue-last-request queue))
      (setf (plz-queue-last-request queue) nil))
    request))

(defun plz-run (queue)
  "Process requests in QUEUE and return QUEUE.
Return when QUEUE is at limit or has no more queued requests.

QUEUE should be a `plz-queue' structure."
  (cl-labels ((readyp
               (queue) (and (not (plz-queue-canceled-p queue))
                            (plz-queue-requests queue)
                            ;; With apologies to skeeto...
                            (< (length (plz-queue-active queue)) (plz-queue-limit queue)))))
    (while (readyp queue)
      (pcase-let* ((request (plz--queue-pop queue))
                   ((cl-struct plz-queued-request method url
                               headers body finally noquery as body-type decode connect-timeout timeout
                               (else orig-else) (then orig-then))
                    request)
                   (then (lambda (response)
                           (unwind-protect
                               ;; Ensure any errors in the THEN function don't abort the queue.
                               (funcall orig-then response)
                             (setf (plz-queue-active queue) (delq request (plz-queue-active queue)))
                             (plz-run queue))))
                   (else (lambda (arg)
                           ;; FIXME(v0.8): This should be done in `plz-queue' because
                           ;; `plz-clear' will call the second queued-request's ELSE
                           ;; before it can be set by `plz-run'.
                           (unwind-protect
                               ;; Ensure any errors in the THEN function don't abort the queue.
                               (when orig-else
                                 (funcall orig-else arg))
                             (setf (plz-queue-active queue) (delq request (plz-queue-active queue)))
                             (plz-run queue))))
                   (args (list method url
                               ;; Omit arguments for which `plz' has defaults so as not to nil them.
                               :headers headers :body body :finally finally :noquery noquery
                               :connect-timeout connect-timeout :timeout timeout)))
        ;; Add arguments which override defaults.
        (when as
          (setf args (plist-put args :as as)))
        (when else
          (setf args (plist-put args :else else)))
        (when then
          (setf args (plist-put args :then then)))
        (when decode
          (setf args (plist-put args :decode decode)))
        (when body-type
          (setf args (plist-put args :body-type body-type)))
        (when connect-timeout
          (setf args (plist-put args :connect-timeout connect-timeout)))
        (when timeout
          (setf args (plist-put args :timeout timeout)))
        (setf (plz-queued-request-process request) (apply #'plz args))
        (push request (plz-queue-active queue))))
    (when (and (plz-queue-finally queue)
               (zerop (length (plz-queue-active queue)))
               (zerop (length (plz-queue-requests queue))))
      (funcall (plz-queue-finally queue)))
    queue))

(defun plz-clear (queue)
  "Clear QUEUE and return it.
Cancels any active or pending requests and calls the queue's
FINALLY function.  For pending requests, their ELSE functions
will be called with a `plz-error' structure with the message,
\"`plz' queue cleared; request canceled.\"; active requests will
have their curl processes killed and their ELSE functions called
with the corresponding data."
  (setf (plz-queue-canceled-p queue) t)
  (dolist (request (plz-queue-active queue))
    (when (process-live-p (plz-queued-request-process request))
      (kill-process (plz-queued-request-process request)))
    (setf (plz-queue-active queue) (delq request (plz-queue-active queue))))
  (dolist (request (plz-queue-requests queue))
    (funcall (plz-queued-request-else request)
             (make-plz-error :message "`plz' queue cleared; request canceled."))
    (setf (plz-queue-requests queue) (delq request (plz-queue-requests queue))))
  (when (plz-queue-finally queue)
    (funcall (plz-queue-finally queue)))
  (setf (plz-queue-first-active queue) nil
        (plz-queue-last-active queue) nil
        (plz-queue-first-request queue) nil
        (plz-queue-last-request queue) nil
        (plz-queue-canceled-p queue) nil)
  queue)

(defun plz-length (queue)
  "Return number of of QUEUE's outstanding requests.
Includes active and queued requests."
  (+ (length (plz-queue-active queue))
     (length (plz-queue-requests queue))))

;;;;; Private

(defun plz--sentinel (process status)
  "Sentinel for curl PROCESS.
STATUS should be the process's event string (see info
node `(elisp) Sentinels').  Calls `plz--respond' to process the
HTTP response (directly for synchronous requests, or from a timer
for asynchronous ones)."
  (pcase status
    ((or "finished\n" "killed\n" "interrupt\n"
         (pred numberp)
         (rx "exited abnormally with code " (group (1+ digit))))
     (let ((buffer (process-buffer process)))
       (if (process-get process :plz-sync)
           (plz--respond process buffer status)
         (run-at-time 0 nil #'plz--respond process buffer status))))))

(defun plz--respond (process buffer status)
  "Respond to HTTP response from PROCESS in BUFFER.
Parses the response and calls the THEN/ELSE callbacks
accordingly.  To be called from `plz--sentinel'.  STATUS is the
argument passed to `plz--sentinel', which see."
  ;; Is it silly to call this function "please respond"?  Perhaps, but
  ;; naming things is hard.  The term "process" has another meaning in
  ;; this context, and the old standby, "handle," is much overused.
  ;; "Respond" also means "to react to something," which is what this
  ;; does--react to receiving the HTTP response--and it's an internal
  ;; name, so why not.
  (unwind-protect
      (with-current-buffer buffer
        (pcase-exhaustive status
          ((or 0 "finished\n")
           ;; Curl exited normally: check HTTP status code.
           (goto-char (point-min))
           (plz--skip-proxy-headers)
           (while (plz--skip-redirect-headers))
           (pcase (plz--http-status)
             ((and status (guard (<= 200 status 299)))
              ;; Any 2xx response is considered successful.
              (ignore status) ; Byte-compiling in Emacs <28 complains without this.
              (funcall (process-get process :plz-then)))
             (_
              ;; TODO: If using ":as 'response", the HTTP response
              ;; should be passed to the THEN function, regardless
              ;; of the status code.  Only for curl errors should
              ;; the ELSE function be called.  (Maybe in v0.8.)

              ;; Any other status code is considered unsuccessful
              ;; (for now, anyway).
              (let ((err (make-plz-error :response (plz--response))))
                (pcase-exhaustive (process-get process :plz-else)
                  (`nil (process-put process :plz-result err))
                  ((and (pred functionp) fn) (funcall fn err)))))))

          ((or (and (pred numberp) code)
               (rx "exited abnormally with code " (let code (group (1+ digit)))))
           ;; Curl error.
           (let* ((curl-exit-code (cl-typecase code
                                    (string (string-to-number code))
                                    (number code)))
                  (curl-error-message (alist-get curl-exit-code plz-curl-errors))
                  (err (make-plz-error :curl-error (cons curl-exit-code curl-error-message))))
             (pcase-exhaustive (process-get process :plz-else)
               (`nil (process-put process :plz-result err))
               ((and (pred functionp) fn) (funcall fn err)))))

          ((and (or "killed\n" "interrupt\n") status)
           ;; Curl process killed or interrupted.
           (let* ((message (pcase status
                             ("killed\n" "curl process killed")
                             ("interrupt\n" "curl process interrupted")))
                  (err (make-plz-error :message message)))
             (pcase-exhaustive (process-get process :plz-else)
               (`nil (process-put process :plz-result err))
               ((and (pred functionp) fn) (funcall fn err)))))))
    (when-let ((finally (process-get process :plz-finally)))
      (funcall finally))
    (unless (or (process-get process :plz-sync)
                (eq 'buffer (process-get process :plz-as)))
      (kill-buffer buffer))))

(defun plz--stderr-sentinel (process status)
  "Sentinel for STDERR buffer.
Arguments are PROCESS and STATUS (ok, checkdoc?)."
  (pcase status
    ((or "finished\n" "killed\n" "interrupt\n"
         (pred numberp)
         (rx "exited abnormally with code " (1+ digit)))
     (kill-buffer (process-buffer process)))))

;;;;;; HTTP Responses

;; Functions for parsing HTTP responses.

(defun plz--skip-proxy-headers ()
  "Skip proxy headers in current buffer."
  (when (looking-at plz-http-response-status-line-regexp)
    (let* ((status-code (string-to-number (match-string 2)))
           (reason-phrase (match-string 3)))
      (when (and (equal 200 status-code)
                 (equal "Connection established" reason-phrase))
        ;; Skip proxy headers (curl apparently offers no way to omit
        ;; them).
        (unless (re-search-forward "\r\n\r\n" nil t)
          (signal 'plz-http-error '("plz--response: End of proxy headers not found")))))))

(defun plz--skip-redirect-headers ()
  "Skip HTTP redirect headers in current buffer."
  (when (and (looking-at plz-http-response-status-line-regexp)
             (member (string-to-number (match-string 2)) '(301 302 303 307 308)))
    ;; Skip redirect headers ("--dump-header" forces redirect headers to be included
    ;; even when used with "--location").
    (or (re-search-forward "\r\n\r\n" nil t)
        (signal 'plz-http-error '("plz--response: End of redirect headers not found")))))

(cl-defun plz--response (&key (decode-p t))
  "Return response structure for HTTP response in current buffer.
When DECODE-P is non-nil, decode the response body automatically
according to the apparent coding system.

Assumes that point is at beginning of HTTP response."
  (save-excursion
    ;; Parse HTTP version and status code.
    (unless (looking-at plz-http-response-status-line-regexp)
      (signal 'plz-http-error
              (list "plz--response: Unable to parse HTTP response status line"
                    (buffer-substring (point) (line-end-position)))))
    (let* ((http-version (string-to-number (match-string 1)))
           (status-code (string-to-number (match-string 2)))
           (headers (plz--headers))
           (coding-system (or (plz--coding-system headers) 'utf-8)))
      (plz--narrow-to-body)
      (when decode-p
        (decode-coding-region (point) (point-max) coding-system))
      (make-plz-response
       :version http-version
       :status status-code
       :headers headers
       :body (buffer-string)))))

(defun plz--coding-system (&optional headers)
  "Return coding system for HTTP response in current buffer.
HEADERS may optionally be an alist of parsed HTTP headers to
refer to rather than the current buffer's un-parsed headers."
  (let* ((headers (or headers (plz--headers)))
         (content-type (alist-get 'content-type headers)))
    (when content-type
      (coding-system-from-name content-type))))

(defun plz--http-status ()
  "Return HTTP status code for HTTP response in current buffer.
Assumes point is at start of HTTP response."
  (when (looking-at plz-http-response-status-line-regexp)
    (string-to-number (match-string 2))))

(defun plz--headers ()
  "Return headers alist for HTTP response in current buffer.
Assumes point is at start of HTTP response."
  (save-excursion
    (forward-line 1)
    (let ((limit (save-excursion
                   (re-search-forward plz-http-end-of-headers-regexp nil)
                   (point))))
      (cl-loop while (re-search-forward (rx bol (group (1+ (not (in ":")))) ":" (1+ blank)
                                            (group (1+ (not (in "\r\n")))))
                                        limit t)
               ;; NOTE: Some HTTP servers send all-lowercase header keys, which means an alist
               ;; lookup with `equal' or `string=' fails when the case differs.  We don't want
               ;; users to have to worry about this, so for consistency, we downcase the
               ;; header name.  And while we're at it, we might as well intern it so we can
               ;; use `alist-get' without having to add "nil nil #'equal" every time.
               collect (cons (intern (downcase (match-string 1))) (match-string 2))))))

(defun plz--narrow-to-body ()
  "Narrow to body of HTTP response in current buffer.
Assumes point is at start of HTTP response."
  (unless (re-search-forward plz-http-end-of-headers-regexp nil t)
    (signal 'plz-http-error '("plz--narrow-to-body: Unable to find end of headers")))
  (narrow-to-region (point) (point-max)))

;;;; Footer

(provide 'plz)

;;; plz.el ends here
