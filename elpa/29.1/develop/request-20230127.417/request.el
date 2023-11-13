;;; request.el --- Compatible layer for URL request  -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Takafumi Arakaki
;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
;;   Free Software Foundation, Inc.

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; URL: https://github.com/tkf/emacs-request
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.3.3

;; This file is NOT part of GNU Emacs.

;; request.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Uses ``curl`` as its backend or Emacs's native ``url.el`` library if
;; ``curl`` is not found.
;;
;; The default encoding for requests is ``utf-8``.  Please explicitly specify
;; ``:encoding 'binary`` for binary data.

;;; Code:

(eval-when-compile
  (defvar url-http-method)
  (defvar url-http-response-status))

(require 'cl-lib)
(require 'url)
(require 'mail-utils)
(require 'auth-source)
(require 'mailheader)

(defgroup request nil
  "Compatible layer for URL request in Emacs."
  :group 'comm
  :prefix "request-")

(defconst request-version "0.3.3")

(defcustom request-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "request")
  "Directory to store data related to request.el."
  :type 'directory)

(defcustom request-curl "curl"
  "Executable for curl command."
  :type 'string)

(defcustom request-curl-options nil
  "List of curl command options.

List of strings that will be passed to every curl invocation.
You can pass extra options here, like setting the proxy."
  :type '(repeat string))

(defcustom request-backend (if (executable-find request-curl)
                               'curl
                             'url-retrieve)
  "Backend to be used for HTTP request.
Automatically set to `curl' if curl command is found."
  :type '(choice (const :tag "cURL backend" curl)
                 (const :tag "url-retrieve backend" url-retrieve)))

(defcustom request-timeout nil
  "Default request timeout in second.
nil means no timeout."
  :type '(choice (integer :tag "Request timeout seconds")
                 (boolean :tag "No timeout" nil)))

(make-obsolete-variable 'request-temp-prefix nil "0.3.3")

(defcustom request-log-level -1
  "Logging level for request.
One of `error'/`warn'/`info'/`verbose'/`debug'/`trace'/`blather'.
-1 means no logging."
  :type '(choice (integer :tag "No logging" -1)
                 (const :tag "Level error" error)
                 (const :tag "Level warn" warn)
                 (const :tag "Level info" info)
                 (const :tag "Level Verbose" verbose)
                 (const :tag "Level DEBUG" debug)
                 (const :tag "Level TRACE" trace)
                 (const :tag "Level BLATHER" blather)))

(defcustom request-message-level 'warn
  "Logging level for request.
See `request-log-level'."
  :type '(choice (integer :tag "No logging" -1)
                 (const :tag "Level error" error)
                 (const :tag "Level warn" warn)
                 (const :tag "Level info" info)
                 (const :tag "Level Verbose" verbose)
                 (const :tag "Level DEBUG" debug)
                 (const :tag "Level TRACE" trace)
                 (const :tag "Level BLATHER" blather)))

(defmacro request--document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for defstruct accessor etc."
  (declare (indent defun)
           (doc-string 2))
  `(put ',function 'function-documentation ,docstring))

(defconst request--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defvar request-log-buffer-name " *request-log*")

(defmacro request-log (level fmt &rest args)
  "Main logging function at warning LEVEL in FMT with ARGS."
  (declare (indent 1))
  `(cl-flet ((log-level-as-int
              (level)
              (if (integerp level)
                  level
                (or (cdr (assq level request--log-level-def)) 0))))
     (let ((level (log-level-as-int ,level))
           (log-level (log-level-as-int request-log-level))
           (msg-level (log-level-as-int request-message-level)))
       (when (<= level (max log-level msg-level))
         (let ((msg (format "[%s] %s" ,level (format ,fmt ,@args))))
           (when (<= level log-level)
             (with-current-buffer (get-buffer-create request-log-buffer-name)
               (setq buffer-read-only t)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert msg "\n"))))
           (when (<= level msg-level)
             (message "%s" msg)))))))

(defconst request--url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?~)
  "`url-unreserved-chars' copied from Emacs 24.3 release candidate.
This is used for making `request--urlencode-alist' RFC 3986 compliant
for older Emacs versions.")

(defun request--urlencode-alist (alist)
  "Hexify ALIST fields according to RFC3986."
  (let ((url-unreserved-chars request--url-unreserved-chars))
    (cl-loop for sep = "" then "&"
             for (k . v) in alist
             concat sep
             concat (url-hexify-string (format "%s" k))
             concat "="
             concat (url-hexify-string (format "%s" v)))))


(defun request--parse-response-at-point ()
  "Parse the first header line such as \"HTTP/1.1 200 OK\"."
  (when (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)" nil t)
    (list :version (match-string 1)
          :code (string-to-number (match-string 2)))))

(defun request--goto-next-body (&optional noerror)
  "Scan forward to next blank line allowing NOERROR if missing."
  (re-search-forward "^\r\n" nil noerror))

(cl-defstruct request-response
  "A structure holding all relevant information of a request."
  status-code history data error-thrown symbol-status url
  done-p settings
  ;; internal variables
  -buffer -raw-header -timer -backend)

(defmacro request--document-response (function docstring)
  "Append to FUNCTION's DOCSTRING some more canned verbiage."
  (declare (indent defun) (doc-string 2))
  `(request--document-function ,function ,(concat docstring "

.. This is an accessor for `request-response' object.

\(fn RESPONSE)")))

(request--document-response request-response-status-code
  "Integer HTTP response code (e.g., 200).")

(request--document-response request-response-history
  "Redirection history (a list of response object).
The first element is the oldest redirection.

You can use restricted portion of functions for the response
objects in the history slot.  It also depends on backend.  Here
is the table showing what functions you can use for the response
objects in the history slot.

==================================== ============== ==============
Slots                                          Backends
------------------------------------ -----------------------------
\\                                    curl           url-retrieve
==================================== ============== ==============
request-response-url                  yes            yes
request-response-header               yes            no
other functions                       no             no
==================================== ============== ==============")

(request--document-response request-response-data
  "Response parsed by the given parser.")

(request--document-response request-response-error-thrown
  "Error thrown during request.
It takes the form of ``(ERROR-SYMBOL . DATA)``, which can be
re-raised (`signal'ed) by ``(signal ERROR-SYMBOL DATA)``.")

(request--document-response request-response-symbol-status
  "A symbol representing the status of request (not HTTP response code).
One of success/error/timeout/abort/parse-error.")

(request--document-response request-response-url
  "Final URL location of response.")

(request--document-response request-response-done-p
  "Return t when the request is finished or aborted.")

(request--document-response request-response-settings
  "Keyword arguments passed to `request' function.
Some arguments such as HEADERS is changed to the one actually
passed to the backend.  Also, it has additional keywords such
as URL which is the requested URL.")

;;;###autoload
(defun request-response-header (response field-name)
  "Fetch the values of RESPONSE header field named FIELD-NAME.

It returns comma separated values when the header has multiple
field with the same name, as :RFC:`2616` specifies.

Examples::

  (request-response-header response
                           \"content-type\") ; => \"text/html; charset=utf-8\"
  (request-response-header response
                           \"unknown-field\") ; => nil"
  (let ((raw-header (request-response--raw-header response)))
    (when raw-header
      (with-temp-buffer
        (erase-buffer)
        (insert raw-header)
        ;; ALL=t to fetch all fields with the same name to get comma
        ;; separated value [#rfc2616-sec4]_.
        (mail-fetch-field field-name nil t)))))
;; .. [#rfc2616-sec4] RFC2616 says this is the right thing to do
;;    (see https://tools.ietf.org/html/rfc2616.html#section-4.2).
;;    Python's requests module does this too.

;;;###autoload
(defun request-response-headers (response)
  "Return RESPONSE headers as an alist.
I would have chosen a function name that wasn't so suggestive that
`headers` is a member of the `request-response` struct, but
as there's already precedent with `request-response-header', I
hew to consistency."
  (let ((raw-header (request-response--raw-header response)))
    (when raw-header
      (with-temp-buffer
        (save-excursion (insert raw-header))
        (when (save-excursion (request--parse-response-at-point))
          (forward-line))
        (mail-header-extract-no-properties)))))

(defconst request--backend-alist
  '((url-retrieve
     . ((request             . request--url-retrieve)
        (request-sync        . request--url-retrieve-sync)
        (terminate-process   . delete-process)
        (get-cookies         . request--url-retrieve-get-cookies)))
    (curl
     . ((request             . request--curl)
        (request-sync        . request--curl-sync)
        (terminate-process   . interrupt-process)
        (get-cookies         . request--curl-get-cookies))))
  "Map backend and method name to actual method (symbol).

It's alist of alist, of the following form::

    ((BACKEND . ((METHOD . FUNCTION) ...)) ...)

It would be nicer if I can use EIEIO.  But as CEDET is included
in Emacs by 23.2, using EIEIO means abandon older Emacs versions.
It is probably necessary if I need to support more backends.  But
let's stick to manual dispatch for now.")
;; See: (view-emacs-news "23.2")

(defun request--choose-backend (method)
  "Return `fucall'able object for METHOD of current `request-backend'."
  (assoc-default
   method
   (or (assoc-default request-backend request--backend-alist)
       (error "%S is not valid `request-backend'" request-backend))))


(defun request-cookie-string (host &optional localpart secure)
  "Lookup HOST LOCALPART SECURE in cookie jar as`document.cookie` string.
Example::

  (request-cookie-string \"127.0.0.1\" \"/\")  ; => \"key=value; key2=value2\""
  (mapconcat (lambda (nv) (concat (car nv) "=" (cdr nv)))
             (request-cookie-alist host localpart secure)
             "; "))

(defun request-cookie-alist (host &optional localpart secure)
  "Lookup HOST LOCALPART SECURE in cookie jar as alist.

Example::

  (request-cookie-alist \"127.0.0.1\" \"/\")  ; => ((\"key\" . \"value\") ...)"
  (funcall (request--choose-backend 'get-cookies) host localpart secure))

;;;###autoload
(cl-defun request (url &rest settings
                       &key
                       (params nil)
                       (data nil)
                       (headers nil)
                       (encoding 'utf-8)
                       (error nil)
                       (sync nil)
                       (response (make-request-response))
                       &allow-other-keys)
  "Main entry requesting URL with property list SETTINGS as follow.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
DATA    (string/alist)   data to be sent to the server
FILES          (alist)   files to be sent to the server (see below)
PARSER        (symbol)   a function that reads current buffer and return data
HEADERS        (alist)   additional headers to send with the request
ENCODING      (symbol)   encoding for request body (utf-8 by default)
SUCCESS     (function)   called on success
ERROR       (function)   called on error
COMPLETE    (function)   called on both success and error
TIMEOUT       (number)   timeout in second
STATUS-CODE    (alist)   map status code (int) to callback
SYNC            (bool)   If non-nil, wait until request is done. Default is nil.
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr\\='s in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it\\='s better to use `&allow-other-keys\\=' [#]_).::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success/error/timeout/abort/parse-error
     :response      response       ; request-response object
     ...)

.. [#] `&allow-other-keys\\=' is a special \"markers\" available in macros
   in the CL library for function definition such as `cl-defun\\=' and
   `cl-function\\='.  Without this marker, you need to specify all arguments
   to be passed.  This becomes problem when request.el adds new arguments
   when calling callback functions.  If you use `&allow-other-keys\\='
   (or manually ignore other arguments), your code is free from this
   problem.  See info node `(cl) Argument Lists\\=' for more information.

Arguments data, error-thrown, symbol-status can be accessed by
`request-response-data\\=', `request-response-error-thrown\\=',
`request-response-symbol-status\\=' accessors, i.e.::

    (request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following accessors:
`request-response-status-code\\=',
`request-response-url\\=' and
`request-response-settings\\='

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME &key PATH BUFFER STRING MIME-TYPE)

FILE-N can also be a string (path to the file) or a buffer object.
In that case, FILENAME is set to the file name or buffer name.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password.txt\" :file \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch.txt\"  :buffer ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\"     :data \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: https://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response body.  The current position in the HTTP
response buffer is at the beginning of the buffer.  As the HTTP
header is stripped off, the cursor is actually at the beginning
of the response body.  So, for example, you can pass `json-read\\='
to parse JSON object in the buffer.  To fetch whole response as a
string, pass `buffer-string\\='.

When using `json-read\\=', it is useful to know that the returned
type can be modified by `json-object-type\\=', `json-array-type\\=',
`json-key-type\\=', `json-false\\=' and `json-null\\='.  See docstring of
each function for what it does.  For example, to convert JSON
objects to plist instead of alist, wrap `json-read\\=' by `lambda\\='
like this.::

    (request
     \"https://...\"
     :parser (lambda ()
               (let ((json-object-type \\='plist))
                 (json-read)))
     ...)

This is analogous to the `dataType\\=' argument of jQuery.ajax_.
Only this function can access to the process buffer, which
is killed immediately after the execution of this function.

* SYNC

Synchronous request is functional, but *please* don\\='t use it
other than testing or debugging.  Emacs users have better things
to do rather than waiting for HTTP request.  If you want a better
way to write callback chains, use `request-deferred\\='.

If you can\\='t avoid using it (e.g., you are inside of some hook
which must return some value), make sure to set TIMEOUT to
relatively small value.

Due to limitation of `url-retrieve-synchronously\\=', response slots
`request-response-error-thrown\\=', `request-response-history\\=' and
`request-response-url\\=' are unknown (always nil) when using
synchronous request with `url-retrieve\\=' backend.

* Note

API of `request\\=' is somewhat mixture of jQuery.ajax_ (Javascript)
and requests.request_ (Python).

.. _jQuery.ajax: https://api.jquery.com/jQuery.ajax/
.. _requests.request: https://docs.python-requests.org"
  (declare (indent defun))
  ;; FIXME: support CACHE argument (if possible)
  ;; (unless cache
  ;;   (setq url (request--url-no-cache url)))
  (unless error
    (setq error (cl-function
                 (lambda (&rest args &key symbol-status &allow-other-keys)
                   (request-log 'error
                     "request-default-error-callback: %s %s"
                     url symbol-status))))
    (setq settings (plist-put settings :error error)))
  (when (and (consp data)
             (not (assoc-string "Content-Type" headers t)))
    (setq data (request--urlencode-alist data))
    (setq settings (plist-put settings :data data)))
  (when params
    (cl-assert (listp params) nil "PARAMS must be an alist.  Given: %S" params)
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (request--urlencode-alist params))))
  (setq settings (plist-put settings :url url))
  (setq settings (plist-put settings :response response))
  (setq settings (plist-put settings :encoding encoding))
  (setf (request-response-settings response) settings)
  (setf (request-response-url      response) url)
  (setf (request-response--backend response) request-backend)
  ;; Call `request--url-retrieve'(`-sync') or `request--curl'(`-sync').
  (apply (if sync
             (request--choose-backend 'request-sync)
           (request--choose-backend 'request))
         url settings)
  response)

(defun request--clean-header (response)
  "Strip off carriage return in the header of RESPONSE."
  (let* ((buffer (request-response--buffer response))
         (backend (request-response--backend response))
         ;; FIXME: a workaround when `url-http-clean-headers' fails...
         (sep-regexp (if (eq backend 'url-retrieve) "^\r?$" "^\r$")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (and (re-search-forward sep-regexp nil t)
                   (not (equal (match-string 0) "")))
          (request-log 'trace "request--clean-header: cleaning\n%s"
                       (buffer-substring (save-excursion
                                           (forward-line -1)
                                           (line-beginning-position))
                                         (save-excursion
                                           (forward-line 1)
                                           (line-end-position))))
          (while (re-search-backward "\r$" (point-min) t)
            (replace-match "")))))))

(defun request--cut-header (response)
  "Move the header to the raw-header slot of RESPONSE object."
  (let ((buffer (request-response--buffer response)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t)
          (setf (request-response--raw-header response)
                (buffer-substring (point-min) (point)))
          (request-log 'trace "request--cut-header: cutting\n%s"
                       (buffer-substring (point-min) (min (1+ (point)) (point-max))))
          (delete-region (point-min) (min (1+ (point)) (point-max))))))))

;;;###autoload
(defun request-untrampify-filename (file)
  "Return FILE as the local file name."
  (or (file-remote-p file 'localname) file))

(defun request--parse-data (response encoding parser)
  "In RESPONSE buffer, decode via ENCODING, then send to PARSER."
  (let ((buffer (request-response--buffer response)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (request-log 'trace "request--parse-data: %s" (buffer-string))
        (unless (eq (request-response-status-code response) 204)
          (recode-region (point-min) (point-max) encoding 'no-conversion)
          (goto-char (point-min))
          (setf (request-response-data response)
                (if parser (funcall parser) (buffer-string))))))))

(defsubst request-url-file-p (url)
  "Return non-nil if URL looks like a file URL."
  (let ((scheme (and (stringp url) (url-type (url-generic-parse-url url)))))
    (and (stringp scheme)
         (not (string-match-p "^http" scheme)))))

(cl-defun request--callback (buffer
                             &key
                             parser success error complete
                             status-code response
                             encoding
                             &allow-other-keys)
  "Parse BUFFER according to PARSER.
Delegate to callbacks SUCCESS, ERROR, and COMPLETE the STATUS-CODE of
RESPONSE via ENCODING."
  (when (buffer-live-p buffer)
    ;; questionable whether BUFFER should override RESPONSE.
    (setf (request-response--buffer response) buffer))
  (request-log 'debug "request--callback: UNPARSED\n%s"
               (when (buffer-live-p (request-response--buffer response))
                 (with-current-buffer (request-response--buffer response)
                   (buffer-string))))
  (cl-symbol-macrolet ((timer (request-response--timer response)))
    (when timer
      (cancel-timer timer)
      (setq timer nil)))
  (cl-symbol-macrolet
      ((error-thrown (request-response-error-thrown response))
       (symbol-status (request-response-symbol-status response))
       (data (request-response-data response))
       (done-p (request-response-done-p response)))
    (let* ((response-url (request-response-url response))
           (curl-file-p (and (eq (request-response--backend response) 'curl)
                             (request-url-file-p response-url))))
      (unless curl-file-p
        (request--clean-header response)
        (request--cut-header response)))

    ;; Parse response even if `error-thrown' is set, e.g., timeout
    (condition-case err
        (request--parse-data response encoding parser)
      (error (unless error-thrown (setq error-thrown err))
             (unless symbol-status (setq symbol-status 'parse-error))))
    (kill-buffer (request-response--buffer response))

    ;; Ensuring `symbol-status' and `error-thrown' are consistent
    ;; is why we should get rid of `symbol-status'
    ;; (but downstream apps might ill-advisedly rely on it).
    (if error-thrown
        (progn
          (request-log 'error "request--callback: %s"
                       (error-message-string error-thrown))
          (unless symbol-status (setq symbol-status 'error)))
      (unless symbol-status (setq symbol-status 'success))
      (request-log 'debug "request--callback: PARSED\n%s" data))

    (let ((args (list :data data
                      :symbol-status symbol-status
                      :error-thrown error-thrown
                      :response response)))
      (let* ((success-p (eq symbol-status 'success))
             (cb (if success-p success error))
             (name (if success-p "success" "error")))
        (when cb
          (request-log 'debug "request--callback: executing %s" name)
          (apply cb args)))
      (let ((cb (cdr (assq (request-response-status-code response)
                           status-code))))
        (when cb
          (request-log 'debug "request--callback: executing status-code")
          (apply cb args)))
      (when complete
        (request-log 'debug "request--callback: executing complete")
        (apply complete args)))

    (setq done-p t)))

(cl-defun request-response--timeout-callback (response)
  "If RESPONSE times out, ensure `request--callback' gets called."
  (setf (request-response-symbol-status response) 'timeout)
  (setf (request-response-error-thrown response)  '(error . ("Timeout")))
  (let* ((buffer (request-response--buffer response))
         (proc (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (if proc
        ;; This implicitly calls `request--callback'!
        (funcall (request--choose-backend 'terminate-process) proc)
      (cl-symbol-macrolet ((done-p (request-response-done-p response)))
        (unless done-p
          (when (buffer-live-p buffer)
            (cl-destructuring-bind (&key code &allow-other-keys)
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (request--parse-response-at-point))
              (setf (request-response-status-code response) code)))
          (apply #'request--callback
                 buffer
                 (request-response-settings response))
          (setq done-p t))))))

;;;###autoload
(defun request-abort (response)
  "Abort request for RESPONSE (the object returned by `request').
Note that this function invoke ERROR and COMPLETE callbacks.
Callbacks may not be called immediately but called later when
associated process is exited."
  (cl-symbol-macrolet ((buffer (request-response--buffer response))
                       (symbol-status (request-response-symbol-status response))
                       (done-p (request-response-done-p response)))
    (let ((process (get-buffer-process buffer)))
      (unless symbol-status             ; should I use done-p here?
        (setq symbol-status 'abort)
        (setq done-p t)
        (when (process-live-p process)
          (funcall (request--choose-backend 'terminate-process) process))))))


(cl-defun request--url-retrieve-preprocess-settings
    (&rest settings &key type data files headers &allow-other-keys)
  "Augment SETTINGS with properties TYPE DATA FILES HEADERS."
  (when files
    (error "`url-retrieve' backend does not support FILES"))
  (when (and (equal type "POST")
             data
             (not (assoc-string "Content-Type" headers t)))
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  settings)

(cl-defun request--url-retrieve (url &rest settings
                                     &key type data timeout response
                                     &allow-other-keys
                                     &aux headers)
  "Internal workhorse querying URL via curl.
SETTINGS is a property list with keys (some optional) such as GET or POST TYPE,
DATA for posting fields, TIMEOUT in seconds, RESPONSE a mandatory struct.
HEADERS needs to be assigned after SETTINGS is preprocessed."
  (setq settings (apply #'request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'request--url-retrieve-callback
                               (nconc (list :response response) settings) t))
         (proc (get-buffer-process buffer)))
    (request--install-timeout timeout response)
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (set-process-query-on-exit-flag proc nil)))

(cl-defun request--url-retrieve-callback (status &rest settings
                                                 &key response url
                                                 &allow-other-keys)
  "Ensure `request--callback' gets called for STATUS.
SETTINGS should include RESPONSE and URL properties which
inform any necessary redirect or history recording logic."
  (when (featurep 'url-http)
    (setf (request-response-status-code response) url-http-response-status))
  (let ((redirect (plist-get status :redirect)))
    (when redirect
      (setf (request-response-url response) redirect)))
  ;; Construct history slot
  (cl-loop for v in
           (cl-loop with first = t
                    with l = nil
                    for (k v) on status by 'cddr
                    when (eq k :redirect)
                    if first
                    do (setq first nil)
                    else
                    do (push v l)
                    finally do (cons url l))
           do (let ((r (make-request-response :-backend 'url-retrieve)))
                (setf (request-response-url r) v)
                (push r (request-response-history response))))
  (cl-symbol-macrolet ((error-thrown (request-response-error-thrown response))
                       (status-error (plist-get status :error)))
    (when status-error
      (request-log 'warn "request--url-retrieve-callback: %s" status-error)
      (unless error-thrown
        (setq error-thrown status-error))))
  (apply #'request--callback (current-buffer) settings))

(cl-defun request--url-retrieve-sync (url &rest settings
                                          &key type data timeout response
                                          &allow-other-keys
                                          &aux headers)
  "Internal synchronous retrieve of URL.
SETTINGS include typical TYPE DATA TIMEOUT RESPONSE properties.
HEADERS needs to be assigned after SETTINGS is preprocessed."
  (setq settings (apply #'request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf (request-response-symbol-status response)
                                'timeout)
                          (setf (request-response-done-p response) t)
                          nil)
                       (url-retrieve-synchronously url t))
                   (url-retrieve-synchronously url t))))
    (setf (request-response--buffer response) buffer)
    ;; It seems there is no way to get redirects and URL here...
    (when buffer
      ;; Fetch HTTP response code
      (with-current-buffer buffer
        (goto-char (point-min))
        (cl-destructuring-bind (&key code &allow-other-keys)
            (request--parse-response-at-point)
          (setf (request-response-status-code response) code)))
      ;; Parse response body, etc.
      (apply #'request--callback buffer settings)))
  response)

(defun request--url-retrieve-get-cookies (host localpart secure)
  "Retrieve cookies corresponding to HOST LOCALPART SECURE."
  (mapcar
   (lambda (c) (cons (url-cookie-name c) (url-cookie-value c)))
   (url-cookie-retrieve host localpart secure)))

(defvar request--curl-cookie-jar nil
  "Override what the function `request--curl-cookie-jar' returns.
Currently it is used only for testing.")

(defun request--curl-cookie-jar ()
  "Cookie storage for curl backend."
  (or request--curl-cookie-jar
      (expand-file-name "curl-cookie-jar" request-storage-directory)))

(defvar request--curl-capabilities-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Used to avoid invoking curl more than once for version info.  By skeeto/elfeed.")

(defun request--curl-capabilities ()
  "Return capabilities plist for curl.  By skeeto/elfeed.
:version     -- cURL's version string
:compression -- non-nil if --compressed is supported."
  (let ((cache-value (gethash request-curl request--curl-capabilities-cache)))
    (if cache-value
        cache-value
      (with-temp-buffer
        (call-process request-curl nil t nil "--version")
        (let ((version
               (progn
                 (goto-char (point-min))
                 (when (re-search-forward "[.0-9]+" nil t)
                   (match-string 0))))
              (compression
               (progn
                 (goto-char (point-min))
                 (not (null (re-search-forward "libz\\>" nil t))))))
          (setf (gethash request-curl request--curl-capabilities-cache)
                `(:version ,version :compression ,compression)))))))

(defconst request--curl-write-out-template
  (if (eq system-type 'windows-nt)
      "\\n(:num-redirects %{num_redirects} :url-effective %{url_effective})"
    "\\n(:num-redirects %{num_redirects} :url-effective \"%{url_effective}\")"))

(defun request--curl-stdin-config (&rest args)
  "Split ARGS such that we \"Only write one option per physical line\".
Fragile.  Some escaping will be necessary for special characters
in user `request-curl-options'."
  (let (result)
    (dolist (arg args (mapconcat #'identity (reverse (cons "" result)) "\n"))
      (if (or (not result)
              (string-prefix-p "-" arg))
          (push arg result)
        (setcar result (format "%s %s" (car result)
                               (if (cl-search " " arg)
                                   (format "\"%s\""
                                           (replace-regexp-in-string
                                            "\""
                                            (regexp-quote "\\\"") arg))
                                 arg)))))))

(defun request--curl-command ()
  "Stub for test-request.el to override."
  (list request-curl "--config" "-"))

(cl-defun request--curl-command-args
    (url &key type data headers files unix-socket auth
         &allow-other-keys
         &aux (cookie-jar (convert-standard-filename
                           (expand-file-name (request--curl-cookie-jar)))))
  "Internal command cobbler for curl to URL.
TYPE, DATA, HEADERS, FILES, UNIX-SOCKET, AUTH are as described in `request'.
COOKIE-JAR is the file location for the netscape cookie jar, usually
in the request subdirectory of `user-emacs-directory'.

BUG: Simultaneous requests are a known cause of cookie-jar corruption."
  (append
   (list "--silent" "--location"
         "--cookie" cookie-jar "--cookie-jar" cookie-jar)
   (when auth
     (let* ((host (url-host (url-generic-parse-url url)))
            (auth-source-creation-prompts `((user . ,(format "%s user: " host))
                                            (secret . "Password for %u: ")))
            (cred (car (auth-source-search
                        :host host :require '(:user :secret) :create t :max 1))))
       (split-string (format "--%s --user %s:%s"
                             auth
                             (plist-get cred :user)
                             (let ((secret (plist-get cred :secret)))
                               (if (functionp secret)
                                   (funcall secret)
                                 secret))))))
   (unless (request-url-file-p url)
     (list "--include" "--write-out" request--curl-write-out-template))
   request-curl-options
   (when (plist-get (request--curl-capabilities) :compression) (list "--compressed"))
   (when unix-socket (list "--unix-socket" unix-socket))
   (when type (if (equal "head" (downcase type))
		  (list "--head")
		(list "--request" type)))
   (cl-loop for (k . v) in headers
            collect "--header"
            collect (format "%s: %s" k v))
   (list "--url" url)
   (when data
     (split-string "--data-binary @-"))
   (cl-loop with stdin-p = data
            for (name . item) in files
            collect "--form"
            collect
            (apply #'format "%s=%s%s;filename=%s%s"
                   (cond ((stringp item)
                          (list name "@" item (file-name-nondirectory item) ""))
                         ((bufferp item)
                          (if stdin-p
                              (error (concat "request--curl-command-args: "
                                             "only one buffer or data entry permitted"))
                            (setq stdin-p t))
                          (list name "@" "-" (buffer-name item) ""))
                         ((listp item)
                          (unless (plist-get (cdr item) :file)
                            (if stdin-p
                                (error (concat "request--curl-command-args: "
                                               "only one buffer or data entry permitted"))
                              (setq stdin-p t)))
                          (list name
                                (if (plist-get (cdr item) :use-contents) "<" "@")
                                (or (plist-get (cdr item) :file) "-")
                                (car item)
                                (if (plist-get (cdr item) :mime-type)
                                    (format ";type=%s" (plist-get (cdr item) :mime-type))
                                  "")))
                         (t (error (concat "request--curl-command-args: "
                                           "%S not string, buffer, or list")
                                   item)))))))

(defun request--install-timeout (timeout response)
  "Out-of-band trigger after TIMEOUT seconds to forestall a hung RESPONSE."
  (when (numberp timeout)
    (setf (request-response--timer response)
          (run-at-time timeout nil
                       #'request-response--timeout-callback response))))

(defun request--curl-occlude-secret (command)
  "Simple regex filter on anything looking like a secret in COMMAND."
  (let ((matched
         (string-match (concat (regexp-quote "--user") "\\s-*\\(\\S-+\\)") command)))
    (if matched
        (replace-match "elided" nil nil command 1)
      command)))

(cl-defun request--curl (url &rest settings
                             &key data files timeout response encoding semaphore
                             &allow-other-keys)
  "Internal workhorse querying URL via curl.

SETTINGS is a property list with keys (some optional) such as DATA for
posting fields, FILES containing one or more lists of the form
  (NAME . FILENAME)
  (NAME . BUFFER)
  (NAME . (FILENAME :buffer BUFFER))
  (NAME . (FILENAME :data DATA))
  (NAME . (FILENAME :file FILE :use-contents t))
with NAME and FILENAME defined by curl(1)'s overwrought `--form` switch format,
TIMEOUT in seconds, RESPONSE a mandatory struct, ENCODING, and SEMAPHORE,
an internal semaphore.  Adding `:use-contents t` sends a text field
with the file's contents as opposed to attaching a file as described
in curl(1).

Redirection handling strategy
-----------------------------

curl follows redirection when --location is given.  However,
all headers are printed when it is used with --include option.
Number of redirects is printed out sexp-based message using
--write-out option (see `request--curl-write-out-template').
This number is used for removing extra headers and parse
location header from the last redirection header.

Sexp at the end of buffer and extra headers for redirects are
removed from the buffer before it is shown to the parser function."
  (ignore-errors
    (make-directory (file-name-directory (request--curl-cookie-jar)) t))
  (let* (process-connection-type ;; pipe, not pty, else curl hangs
         (home-directory (or (file-remote-p default-directory) "~/"))
         (default-directory (expand-file-name home-directory))
         (buffer (generate-new-buffer " *request curl*"))
         (file-items (mapcar #'cdr files))
         (file-buffer (or (cl-some (lambda (item)
                                     (when (bufferp item) item))
                                   file-items)
                          (cl-some (lambda (item)
                                     (and (listp item)
                                          (plist-get (cdr item) :buffer)))
                                   file-items)))
         (file-data (cl-some (lambda (item)
                               (and (listp item)
                                    (plist-get (cdr item) :data)))
                             file-items))
         (command-args (apply #'request--curl-command-args url settings))
         (stdin-config (apply #'request--curl-stdin-config command-args))
         (command (request--curl-command))
         (proc (apply #'start-process "request curl" buffer command)))
    (request--install-timeout timeout response)
    (request-log 'debug "request--curl: %s"
                 (request--curl-occlude-secret (mapconcat #'identity command-args " ")))
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (set-process-coding-system proc 'no-conversion 'no-conversion)
    (set-process-query-on-exit-flag proc nil)
    (process-send-string proc stdin-config)
    (when (or data file-buffer file-data)
      ;; We dynamic-let the global `buffer-file-coding-system' to `no-conversion'
      ;; in case the user-configured `encoding' doesn't fly.
      ;; If we do not dynamic-let the global, `select-safe-coding-system' would
      ;; plunge us into an undesirable interactive dialogue.
      (let* ((buffer-file-coding-system-orig
              (default-value 'buffer-file-coding-system))
             (select-safe-coding-system-accept-default-p
              (lambda (&rest _) t)))
        (unwind-protect
            (progn
              (setf (default-value 'buffer-file-coding-system) 'no-conversion)
              (with-temp-buffer
                (setq-local buffer-file-coding-system encoding)
                (insert (or data
                            (when file-buffer
                              (with-current-buffer file-buffer
                                (buffer-substring-no-properties (point-min) (point-max))))
                            file-data))
                (process-send-region proc (point-min) (point-max))))
          (setf (default-value 'buffer-file-coding-system)
                buffer-file-coding-system-orig))))
    (process-send-eof proc)
    (let ((callback-2 (apply-partially #'request--curl-callback url)))
      (if semaphore
          (set-process-sentinel proc (lambda (&rest args)
                                       (apply callback-2 args)
                                       (apply semaphore args)))
        (set-process-sentinel proc callback-2)))))

(defun request--curl-read-and-delete-tail-info ()
  "Read a sexp at the end of buffer and remove it and preceding character.
This function moves the point at the end of buffer by side effect.
See also `request--curl-write-out-template'."
  (let (forward-sexp-function)
    (goto-char (point-max))
    (forward-sexp -1)
    (let ((beg (1- (point))))
      (prog1
          (read (current-buffer))
        (delete-region beg (point-max))))))

(defconst request--cookie-reserved-re
  (mapconcat
   (lambda (x) (concat "\\(^" x "\\'\\)"))
   '("comment" "commenturl" "discard" "domain" "max-age" "path" "port"
     "secure" "version" "expires")
   "\\|")
  "Uninterested keys in cookie.
See \"set-cookie-av\" in https://www.ietf.org/rfc/rfc2965.txt")

(defun request--consume-100-continue ()
  "Remove \"HTTP/* 100 Continue\" header at the point."
  (cl-destructuring-bind (&key code &allow-other-keys)
      (save-excursion (request--parse-response-at-point))
    (when (equal code 100)
      (request-log 'debug "request--consume-100-continue: consuming\n%s"
                   (buffer-substring (point)
                                     (save-excursion
                                       (request--goto-next-body t)
                                       (point))))
      (delete-region (point) (progn (request--goto-next-body) (point)))
      ;; FIXME: Does this make sense?  Is it possible to have multiple 100?
      (request--consume-100-continue))))

(defun request--consume-200-connection-established ()
  "Remove \"HTTP/* 200 Connection established\" header at the point."
  (when (looking-at-p "HTTP/1\\.[0-1] 200 Connect")
    (delete-region (point) (progn (request--goto-next-body) (point)))))

(defun request--curl-preprocess (&optional url)
  "Pre-process current buffer before showing it to user.
Curl switches need to be adjusted if URL is a file://."
  (let (history)
    (cl-destructuring-bind (&key num-redirects url-effective)
        (if (request-url-file-p url)
            `(:num-redirects 0 :url-effective ,url)
          (request--curl-read-and-delete-tail-info))
      (goto-char (point-min))
      (request--consume-100-continue)
      (request--consume-200-connection-established)
      (when (> num-redirects 0)
        (cl-loop with case-fold-search = t
                 repeat num-redirects
                 ;; Do not store code=100 headers:
                 do (request--consume-100-continue)
                 do (let ((response (make-request-response
                                     :-buffer (current-buffer)
                                     :-backend 'curl)))
                      (request--clean-header response)
                      (request--cut-header response)
                      (push response history))))

      (goto-char (point-min))
      (nconc (list :num-redirects num-redirects :url-effective url-effective
                   :history (nreverse history))
             (request--parse-response-at-point)))))

(defun request--curl-absolutify-redirects (start-url redirects)
  "Convert relative paths in REDIRECTS to absolute URLs.
START-URL is the URL requested."
  (cl-loop for prev-url = start-url then url
           for url in redirects
           unless (string-match url-nonrelative-link url)
           do (setq url (url-expand-file-name url prev-url))
           collect url))

(defun request--curl-absolutify-location-history (start-url history)
  "Convert relative paths in HISTORY to absolute URLs.
START-URL is the URL requested."
  (when history
    (setf (request-response-url (car history)) start-url))
  (cl-loop for url in (request--curl-absolutify-redirects
                       start-url
                       (mapcar (lambda (response)
                                 (or (request-response-header response "location")
                                     (request-response-url response)))
                               history))
           for response in (cdr history)
           do (setf (request-response-url response) url)))

(defun request--curl-callback (url proc event)
  "Ensure `request--callback' gets called after curl to URL finishes.
See info entries on sentinels regarding PROC and EVENT."
  (let* ((response (process-get proc :request-response))
         ;; questionable whether (process-buffer proc)
         ;; should override RESPONSE's -buffer member.
         (buffer (or (process-buffer proc)
                     (request-response--buffer response)))
         (settings (request-response-settings response)))
    (request-log 'debug "request--curl-callback: event %s" event)
    (request-log 'trace "request--curl-callback: raw-bytes=\n%s"
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer (buffer-string))))
    (cond
     ((and (memq (process-status proc) '(exit signal))
           (/= (process-exit-status proc) 0))
      (setf (request-response-error-thrown response) (cons 'error event))
      (apply #'request--callback buffer settings))
     ((cl-search "finished" event)
      (cl-destructuring-bind (&key code history error url-effective &allow-other-keys)
          (condition-case err
              (with-current-buffer buffer
                (request--curl-preprocess url))
            ((debug error)
             (list :error err)))
        (request--curl-absolutify-location-history (plist-get settings :url)
                                                   history)
        (setf (request-response-status-code  response) code)
        (setf (request-response-url          response) url-effective)
        (setf (request-response-history      response) history)
        (setf (request-response-error-thrown response)
              (or error (and (numberp code) (>= code 400) `(error . (http ,code)))))
        (apply #'request--callback buffer settings))))))

(cl-defun request--curl-sync (url &rest settings &key response &allow-other-keys)
  "Internal synchronous curl call to URL with SETTINGS bespeaking RESPONSE."
  (let (finished)
    (prog1 (apply #'request--curl url
                  :semaphore (lambda (&rest _) (setq finished t))
                  settings)
      (cl-loop with buf = (request-response--buffer response)
               with interval = 0.05
               with timeout = 5
               with maxiter = (truncate (/ timeout interval))
               with iter = 0
               until (or (>= iter maxiter) finished)
               do (accept-process-output nil interval)
               for proc = (get-buffer-process buf)
               if (or (not proc) (not (process-live-p proc)))
               ;; only run the clock if lollygagging
               ;; (before or after process lifetime)
               do (cl-incf iter)
               end
               finally (when (>= iter maxiter)
                         (let ((m "request--curl-sync: semaphore never called"))
                           (princ (format "%s %S %s\n"
                                          m
                                          buf
                                          (buffer-live-p buf))
                                  #'external-debugging-output)
                           (request-log 'error m)))))))

(defun request--curl-get-cookies (host localpart secure)
  "Return entry for HOST LOCALPART SECURE in cookie jar."
  (request--netscape-get-cookies (request--curl-cookie-jar)
                                 host localpart secure))

(defun request--netscape-cookie-parse ()
  "Parse Netscape/Mozilla cookie format."
  (goto-char (point-min))
  (let ((tsv-re (concat "^\\(#HttpOnly_\\)?"
                        (cl-loop repeat 6 concat "\\([^\t\n]+\\)\t")
                        "\\(.*\\)"))
        cookies)
    (while (not (eobp))
      ;; HttpOnly cookie starts with '#' but its line is not comment line(#60)
      (cond ((and (looking-at-p "^#") (not (looking-at-p "^#HttpOnly_"))) t)
            ((looking-at-p "^$") t)
            ((looking-at tsv-re)
             (let ((cookie (cl-loop for i from 1 to 8 collect (match-string i))))
               (push cookie cookies))))
      (forward-line 1))
    (setq cookies (nreverse cookies))
    (cl-loop for (http-only domain flag path secure expiration name value) in cookies
             collect (list domain
                           (equal flag "TRUE")
                           path
                           (equal secure "TRUE")
                           (null (not http-only))
                           (string-to-number expiration)
                           name
                           value))))

(defun request--netscape-filter-cookies (cookies host localpart secure)
  "Filter COOKIES for entries containing HOST LOCALPART SECURE."
  (cl-loop for (domain _flag path secure-1 _http-only _expiration name value) in cookies
           when (and (equal domain host)
                     (equal path localpart)
                     (or secure (not secure-1)))
           collect (cons name value)))

(defun request--netscape-get-cookies (filename host localpart secure)
  "Get cookies from FILENAME corresponding to HOST LOCALPART SECURE."
  (when (file-readable-p filename)
    (with-temp-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (request--netscape-filter-cookies (request--netscape-cookie-parse)
                                        host localpart secure))))

(provide 'request)

;;; request.el ends here
