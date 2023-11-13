;;; elfeed-curl.el --- curl backend for Elfeed -*- lexical-binding: t; -*-

;;; Comments:

;; An alternative to `url-retrieve' and `url-queue' that fetches URLs
;; using the curl command line program.

;; The API is three functions:

;; * `elfeed-curl-retrieve'
;; * `elfeed-curl-retrieve-synchronously'
;; * `elfeed-curl-enqueue'

;; And has four buffer-local variables for use in callbacks:

;; * `elfeed-curl-headers'
;; * `elfeed-curl-status-code'
;; * `elfeed-curl-error-message'
;; * `elfeed-curl-location'

;; The buffer delivered to callbacks may contain multiple requests. It
;; will be narrowed to the specific content for the current request.
;; It's vitally important that callbacks do not kill the buffer
;; because it may be needed for other callbacks. It also means the
;; buffer won't necessarily be around when the callback returns.
;; Callbacks should also avoid editing the buffer, though this
;; generally shouldn't impact other requests.

;; Sometimes Elfeed asks curl to retrieve multiple requests and
;; deliver them concatenated. Due to the possibility of HTTP/1.0 being
;; involved — and other ambiguous-length protocols — there's no
;; perfectly unambiguous way to split the output. To work around this,
;; I use curl's --write-out to insert a randomly-generated token after
;; each request. It's highly unlikely (1 in ~1e38) that this token
;; will appear in content, so I can use it to identify the end of each
;; request.

;;; Code:

(require 'url)
(require 'cl-lib)
(require 'elfeed-lib)
(require 'elfeed-log)

(defcustom elfeed-curl-program-name "curl"
  "Name/path by which to invoke the curl program."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-curl-max-connections 16
  "Maximum number of concurrent fetches."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-curl-timeout 30
  "Maximum number of seconds a fetch is allowed to take once started."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-curl-extra-arguments ()
  "A list of additional arguments to pass to cURL.
These extra arguments are appended after Elfeed's own arguments,
and care must be taken to not interfere with Elfeed's needs. The
guideline is to avoid arguments that change anything about cURL's
output format."
  :group 'elfeed
  :type '(repeat string))

(defvar elfeed-curl-queue ()
  "List of pending curl requests.")

(defvar elfeed-curl-queue-active 0
  "Number of concurrent requests currently active.")

(defvar-local elfeed-curl-headers nil
  "Alist of HTTP response headers.")

(defvar-local elfeed-curl-status-code nil
  "Numeric HTTP response code, nil for non-HTTP protocols.")

(defvar-local elfeed-curl-error-message nil
  "Human-friendly message describing the error.")

(defvar-local elfeed-curl-location nil
  "Actual URL fetched (after any redirects).")

(defvar-local elfeed-curl--regions ()
  "List of markers bounding separate requests.")

(defvar-local elfeed-curl--requests ()
  "List of URL / callback pairs for the current buffer.")

(defvar-local elfeed-curl--token nil
  "Unique token that splits requests.")

(defvar-local elfeed-curl--refcount nil
  "Number of callbacks waiting on the current buffer.")

(defvar elfeed-curl--error-codes
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
    (16 . "A problem was detected in the HTTP2 framing layer.")
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
    (90 . "SSL public key does not matched pinned public key")))

(defvar elfeed-curl--capabilities-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Used to avoid invoking curl more than once for version info.")

(defun elfeed-curl-get-capabilities ()
  "Return capabilities plist for the curl at `elfeed-curl-program-name'.
:version     -- cURL's version string
:compression -- non-nil if --compressed is supported
:protocols   -- symbol list of supported protocols
:features    -- string list of supported features"
  (let* ((cache elfeed-curl--capabilities-cache)
         (cache-value (gethash elfeed-curl-program-name cache)))
    (if cache-value
        cache-value
      (with-temp-buffer
        (call-process elfeed-curl-program-name nil t nil "--version")
        (let ((version
               (progn
                 (goto-char (point-min))
                 (when (re-search-forward "[.0-9]+" nil t)
                   (match-string 0))))
              (protocols
               (progn
                 (goto-char (point-min))
                 (when (re-search-forward "^Protocols: \\(.*\\)$" nil t)
                   (mapcar #'intern (split-string (match-string 1))))))
              (features
               (progn
                 (goto-char (point-min))
                 (when (re-search-forward "^Features: \\(.*\\)$")
                   (split-string (match-string 1))))))
          (setf (gethash elfeed-curl-program-name cache)
                (list :version version
                      :compression (not (null (member "libz" features)))
                      :protocols protocols
                      :features features)))))))

(defun elfeed-curl-get-version ()
  "Return the version of curl for `elfeed-curl-program-name'."
  (plist-get (elfeed-curl-get-capabilities) :version))
(make-obsolete 'elfeed-curl-get-version 'elfeed-curl-get-capabilities "3.0.1")

(defun elfeed-curl--token ()
  "Return a unique, random string that prints as a symbol without escapes.
This token is used to split requests. The % is excluded since
it's special to --write-out."
  (let* ((token (make-string 22 ?=))
         (set "!$&*+-/0123456789:<>@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_\
abcdefghijklmnopqrstuvwxyz|~"))
    (prog1 token ; workaround bug#16206
      (dotimes (i (- (length token) 2))
        (setf (aref token (1+ i)) (aref set (cl-random (length set))))))))

(defun elfeed-curl--parse-write-out ()
  "Parse curl's write-out (-w) messages into `elfeed-curl--regions'."
  (widen)
  (goto-char (point-max))
  (setf elfeed-curl--regions ())
  (while (> (point) (point-min))
    (search-backward elfeed-curl--token)
    (goto-char (1- (point)))
    (let ((end (point)))
      (cl-destructuring-bind (_ . header) (read (current-buffer))
        (goto-char end)
        ;; Find next sentinel token
        (if (search-backward elfeed-curl--token nil t)
            (search-forward ")" nil t)
          (goto-char (point-min)))
        (let* ((header-start (point))
               (header-end (+ (point) header))
               (content-start (+ (point) header))
               (content-end end)
               (regions (list header-start header-end
                              content-start content-end))
               (markers (cl-loop for p in regions
                                 for marker = (make-marker)
                                 collect (set-marker marker p))))
          (push markers elfeed-curl--regions))))))

(defun elfeed-curl--narrow (kind n)
  "Narrow to Nth region of KIND (:header, :content)."
  (let ((region (nth n elfeed-curl--regions)))
    (cl-destructuring-bind (h-start h-end c-start c-end) region
      (cl-ecase kind
        (:header (narrow-to-region h-start h-end))
        (:content (narrow-to-region c-start c-end))))))

(defun elfeed-curl--parse-http-headers ()
  "Parse the current HTTP response headers into buffer-locals.
Sets `elfeed-curl-headers'and `elfeed-curl-status-code'.
Use `elfeed-curl--narrow' to select a header."
  (when (> (- (point-max) (point-min)) 0)
    (goto-char (point-max))
    (re-search-backward "HTTP/[.0-9]+ +\\([0-9]+\\)")
    (setf elfeed-curl-status-code (string-to-number (match-string 1)))
    (cl-loop initially (goto-char (point-max))
             while (re-search-backward "^\\([^:]+\\): +\\([^\r\n]+\\)" nil t)
             for key = (downcase (match-string 1))
             for value = (match-string 2)
             collect (cons key value) into headers
             finally (setf elfeed-curl-headers headers))))

(defun elfeed-curl--decode ()
  "Try to decode the buffer based on the headers."
  (let ((content-type (cdr (assoc "Content-Type" elfeed-curl-headers))))
    (if (and content-type (string-match "charset=\\(.+\\)" content-type))
        (decode-coding-region (point-min) (point-max)
                              (coding-system-from-name
                               (match-string 1 content-type)))
      (decode-coding-region (point-min) (point-max) 'utf-8))))

(defun elfeed-curl--final-location (location headers)
  "Given start LOCATION and HEADERS, find the final location."
  (cl-loop for (key . value) in headers
           when (equal key "location")
           do (setf location (elfeed-update-location location value))
           finally return location))

(defun elfeed-curl--args (url token &optional headers method data)
  "Build an argument list for curl for URL.
URL can be a string or a list of URL strings."
  (let* ((args ())
         (capabilities (elfeed-curl-get-capabilities)))
    (push "--disable" args)
    (when (plist-get capabilities :compression)
      (push "--compressed" args))
    (push "--silent" args)
    (push "--location" args)
    (push (format "-w(%s . %%{size_header})" token) args)
    (push (format "-m%s" elfeed-curl-timeout) args)
    (push "-D-" args)
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (push (format "-H%s: %s" key value) args)))
    (when method (push (format "-X%s" method) args))
    (when data (push (format "-d%s" data) args))
    (setf args (nconc (reverse elfeed-curl-extra-arguments) args))
    (if (listp url)
        (nconc (nreverse args) url)
      (nreverse (cons url args)))))

(defun elfeed-curl--prepare-response (url n protocol)
  "Prepare response N for delivery to user."
  (elfeed-curl--narrow :header n)
  (when (eq protocol 'http)
    (elfeed-curl--parse-http-headers))
  (setf elfeed-curl-location
        (elfeed-curl--final-location url elfeed-curl-headers))
  (elfeed-curl--narrow :content n)
  (elfeed-curl--decode)
  (current-buffer))

(cl-defun elfeed-curl-retrieve-synchronously (url &key headers method data)
  "Retrieve the contents for URL and return a new buffer with them.

HEADERS is an alist of additional headers to add to the HTTP request.
METHOD is the HTTP method to use.
DATA is the content to include in the request."
  (with-current-buffer (generate-new-buffer " *curl*")
    (setf elfeed-curl--token (elfeed-curl--token))
    (let ((args (elfeed-curl--args url elfeed-curl--token headers method data))
          (coding-system-for-read 'binary))
      (apply #'call-process elfeed-curl-program-name nil t nil args))
    (elfeed-curl--parse-write-out)
    (elfeed-curl--prepare-response url 0 (elfeed-curl--protocol-type url))))

(defun elfeed-curl--protocol-type (url)
  (let ((scheme (intern (or (url-type (url-generic-parse-url url)) "nil"))))
    (cl-case scheme
      ((https nil) 'http)
      (otherwise scheme))))

(defun elfeed-curl--call-callback (buffer n url cb)
  "Prepare the buffer for callback N and call it."
  (let ((result nil)
        (protocol (elfeed-curl--protocol-type url)))
    (with-current-buffer buffer
      (setf elfeed-curl-error-message "unable to parse curl response")
      (unwind-protect
          (progn
            (elfeed-curl--prepare-response url n protocol)
            (cond ((eq protocol 'file)
                   ;; No status code is returned by curl for file:// urls
                   (setf result t
                         elfeed-curl-error-message nil))
                  ((eq protocol 'gopher)
                   (setf result t
                         elfeed-curl-error-message nil
                         elfeed-curl-status-code nil))
                  ((and (>= elfeed-curl-status-code 400)
                        (<= elfeed-curl-status-code 599))
                   (setf elfeed-curl-error-message
                         (format "HTTP %d" elfeed-curl-status-code)))
                  (t
                   (setf result t
                         elfeed-curl-error-message nil)))
            ;; Always call callback
            (unwind-protect
                (funcall cb result)
              ;; Always clean up
              (when (zerop (cl-decf elfeed-curl--refcount))
                (kill-buffer))))))))

(defun elfeed-curl--fail-callback (buffer cb)
  "Inform the callback the request failed."
  (with-current-buffer buffer
    (unwind-protect
        (funcall cb nil)
      (when (zerop (cl-decf elfeed-curl--refcount))
        (kill-buffer)))))

(defun elfeed-curl--sentinel (process status)
  "Manage the end of a curl process' life."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      ;; Fire off callbacks in separate interpreter turns so they can
      ;; each fail in isolation from each other.
      (if (equal status "finished\n")
          (cl-loop with handler = #'elfeed-curl--call-callback
                   initially do (elfeed-curl--parse-write-out)
                   for (url . cb) in elfeed-curl--requests
                   for n upfrom 0
                   do (run-at-time 0 nil handler buffer n url cb))
        (if (string-match "exited abnormally with code \\([0-9]+\\)" status)
            (let* ((code (string-to-number (match-string 1 status)))
                   (message (cdr (assoc code elfeed-curl--error-codes))))
              (setf elfeed-curl-error-message
                    (format "(%d) %s" code
                            (or message "Unknown curl error!"))))
          (setf elfeed-curl-error-message status))
        (cl-loop with handler = #'elfeed-curl--fail-callback
                 for (_ . cb) in elfeed-curl--requests
                 do (run-at-time 0 nil handler buffer cb))))))

(cl-defun elfeed-curl-retrieve (url cb &key headers method data)
  "Retrieve URL contents asynchronously, calling CB with one status argument.

The callback must *not* kill the buffer!

The destination buffer is set at the current buffer for the
callback.

HEADERS is an alist of additional headers to add to HTTP requests.
METHOD is the HTTP method to use.
DATA is the content to include in the request.

URL can be a list of URLs, which will fetch them all in the same
curl process. In this case, CB can also be either a list of the
same length, or just a single function to be called once for each
URL in the list. Headers will be common to all requests. A TCP or
DNS failure in one will cause all to fail, but 4xx and 5xx
results will not."
  (with-current-buffer (generate-new-buffer " *curl*")
    (setf elfeed-curl--token (elfeed-curl--token))
    (let* ((coding-system-for-read 'binary)
           (process-connection-type nil)
           (args (elfeed-curl--args url elfeed-curl--token headers method data))
           (process (apply #'start-process "elfeed-curl" (current-buffer)
                           elfeed-curl-program-name args)))
      (prog1 process
        (if (listp url)
            (progn
              (when (functionp cb)
                (setf cb (make-list (length url) cb)))
              (setf elfeed-curl--requests (cl-mapcar #'cons url cb)
                    elfeed-curl--refcount (length url)))
          (push (cons url cb) elfeed-curl--requests)
          (setf elfeed-curl--refcount 1))
        (set-process-query-on-exit-flag process nil)
        (setf (process-sentinel process) #'elfeed-curl--sentinel)))))

(defun elfeed-curl--request-key (url headers method data)
  "Try to fetch URLs with matching keys at the same time."
  (unless (listp url)
    (let* ((urlobj (url-generic-parse-url url)))
      (list (url-type urlobj)
            (url-host urlobj)
            (url-portspec urlobj)
            headers
            method
            data))))

(defun elfeed-curl--queue-consolidate (queue-in)
  "Group compatible requests together and return a new queue.
Compatible means the requests have the same protocol, domain,
port, headers, method, and body, allowing them to be used safely
in the same curl invocation."
  (let ((table (make-hash-table :test 'equal))
        (keys ())
        (queue-out ()))
    (dolist (entry queue-in)
      (cl-destructuring-bind (url _ headers method data) entry
        (let* ((key (elfeed-curl--request-key url headers method data)))
          (push key keys)
          (push entry (gethash key table nil)))))
    (dolist (key (nreverse keys))
      (let ((entry (gethash key table)))
        (when entry
          (let ((rotated (list (nreverse (cl-mapcar #'car entry))
                               (nreverse (cl-mapcar #'cadr entry))
                               (cl-caddar entry)
                               (elt (car entry) 3)
                               (elt (car entry) 4))))
            (push rotated queue-out)
            (setf (gethash key table) nil)))))
    (nreverse queue-out)))

(defun elfeed-curl--queue-wrap (cb)
  "Wrap the curl CB so that it operates the queue."
  (lambda (status)
    (cl-decf elfeed-curl-queue-active)
    (elfeed-curl--run-queue)
    (funcall cb status)))

(defvar elfeed-curl--run-queue-queued nil
  "Non-nil if run-queue has already been queued for the next turn.")

(defun elfeed-curl--run-queue ()
  "Possibly fire off some new requests."
  (when elfeed-curl--run-queue-queued
    (setf elfeed-curl--run-queue-queued nil
          ;; Try to consolidate the new requests.
          elfeed-curl-queue
          (elfeed-curl--queue-consolidate elfeed-curl-queue)))
  (while (and (< elfeed-curl-queue-active elfeed-curl-max-connections)
              (> (length elfeed-curl-queue) 0))
    (cl-destructuring-bind (url cb headers method data) (pop elfeed-curl-queue)
      (elfeed-log 'debug "retrieve %s" url)
      (cl-incf elfeed-curl-queue-active 1)
      (elfeed-curl-retrieve
       url
       (if (functionp cb)
           (elfeed-curl--queue-wrap cb)
         (cons (elfeed-curl--queue-wrap (car cb))
               (cdr cb)))
       :headers headers
       :method method
       :data data))))

(cl-defun elfeed-curl-enqueue (url cb &key headers method data)
  "Just like `elfeed-curl-retrieve', but restricts concurrent fetches."
  (unless (or (stringp url)
              (and (listp url) (cl-every #'stringp url)))
    ;; Signal error synchronously instead of asynchronously in the timer
    (signal 'wrong-type-argument (list 'string-p-or-string-list-p url)))
  (let ((entry (list url cb headers method data)))
    (setf elfeed-curl-queue (nconc elfeed-curl-queue (list entry)))
    (unless elfeed-curl--run-queue-queued
      (run-at-time 0 nil #'elfeed-curl--run-queue)
      (setf elfeed-curl--run-queue-queued t))))

(provide 'elfeed-curl)

;;; elfeed-curl.el ends here
