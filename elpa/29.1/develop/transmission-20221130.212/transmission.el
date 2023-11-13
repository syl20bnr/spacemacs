;;; transmission.el --- Interface to a Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.12.2
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.5"))
;; Keywords: comm, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to a Transmission session.

;; Entry points are the `transmission' and `transmission-add'
;; commands.  A variety of commands are available for manipulating
;; torrents and their contents, many of which can be applied over
;; multiple items by selecting them with marks or within a region.
;; The menus for each context provide good exposure.

;; "M-x transmission RET" pops up a torrent list.  One can add,
;; start/stop, verify, remove torrents, set speed limits, ratio
;; limits, bandwidth priorities, trackers, etc.  Also, one can
;; navigate to the corresponding file list, torrent info, or peer info
;; contexts.  In the file list, individual files can be toggled for
;; download, and their priorities set.

;; Customize-able are: the session address components, RPC
;; credentials, the display format of dates, file sizes and transfer
;; rates, pieces display, automatic refreshing of the torrent
;; list, etc.  See the `transmission' customization group.

;; The design draws from a number of sources, including the command
;; line utility transmission-remote(1), the ncurses interface
;; transmission-remote-cli(1), and the rtorrent(1) client.  These can
;; be found respectively at the following:
;; <https://github.com/transmission/transmission/blob/master/utils/remote.c>
;; <https://github.com/fagga/transmission-remote-cli>
;; <https://rakshasa.github.io/rtorrent/>

;; Originally based on the JSON RPC library written by Christopher
;; Wellons, available online at <https://github.com/skeeto/elisp-json-rpc>.

;;; Code:

(require 'auth-source)
(require 'calc-bin)
(require 'calc-ext)
(require 'color)
(require 'diary-lib)
(require 'json)
(require 'mailcap)
(require 'tabulated-list)
(require 'url-util)

(eval-when-compile
  (cl-declaim (optimize (speed 3)))
  (require 'cl-lib)
  (require 'let-alist)
  (require 'subr-x))

(declare-function dired-goto-file "dired" (file))

(defgroup transmission nil
  "Interface to a Transmission session."
  :link '(url-link "https://github.com/transmission/transmission")
  :link '(url-link "https://transmissionbt.com/")
  :group 'external)

(defcustom transmission-host "localhost"
  "Host name, IP address, or socket address of the Transmission session."
  :type 'string)

(defcustom transmission-service 9091
  "Port or name of the service for the Transmission session."
  :type '(choice (const :tag "Default" 9091)
                 (string :tag "Service")
                 (integer :tag "Port"))
  :link '(function-link make-network-process))

(defcustom transmission-rpc-path "/transmission/rpc"
  "Path to the Transmission session RPC interface."
  :type '(choice (const :tag "Default" "/transmission/rpc")
                 (string :tag "Other path")))

(defcustom transmission-rpc-auth nil
  "Authentication (username, password, etc.) for the RPC interface.
Its value is a specification of the type used in `auth-source-search'.
If no password is set, `auth-sources' is searched using the
username, `transmission-host', and `transmission-service'."
  :type '(choice (const :tag "None" nil)
                 (plist :tag "Username/password"
                        :options ((:username string)
                                  (:password string))))
  :link '(info-link "(auth) Help for users")
  :link '(function-link auth-source-search))

(defcustom transmission-digit-delimiter ","
  "String used to delimit digits in numbers.
The variable `calc-group-char' is bound to this in `transmission-group-digits'."
  :type '(choice (const :tag "Comma" ",")
                 (const :tag "Full Stop" ".")
                 (const :tag "None" nil)
                 (string :tag "Other char"))
  :link '(variable-link calc-group-char)
  :link '(function-link transmission-group-digits))

(defcustom transmission-pieces-function #'transmission-format-pieces
  "Function used to show pieces of incomplete torrents.
The function takes a string (bitfield) representing the torrent
pieces and the number of pieces as arguments, and should return a string."
  :type '(radio (const :tag "None" nil)
                (function-item transmission-format-pieces)
                (function-item transmission-format-pieces-brief)
                (function :tag "Function")))

(defcustom transmission-trackers '()
  "List of tracker URLs.
These are used for completion in `transmission-trackers-add' and
`transmission-trackers-replace'."
  :type '(repeat (string :tag "URL")))

(defcustom transmission-units nil
  "The flavor of units used to display file sizes.
See `file-size-human-readable'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "SI" si)
                 (const :tag "IEC" iec))
  :link '(function-link file-size-human-readable))

(defcustom transmission-refresh-modes '()
  "List of major modes in which to refresh the buffer automatically."
  :type 'hook
  :options '(transmission-mode
             transmission-files-mode
             transmission-info-mode
             transmission-peers-mode))

(defcustom transmission-refresh-interval 2
  "Period in seconds of the refresh timer."
  :type '(number :validate (lambda (w)
                             (when (<= (widget-value w) 0)
                               (widget-put w :error "Value must be positive")
                               w))))

(defcustom transmission-time-format "%a %b %e %T %Y %z"
  "Format string used to display dates.
See `format-time-string'."
  :type 'string
  :link '(function-link format-time-string))

(defcustom transmission-time-zone nil
  "Time zone of formatted dates.
See `format-time-string'."
  :type '(choice (const :tag "Local time" nil)
                 (const :tag "Universal Time (UTC)" t)
                 (const :tag "System Wall Clock" wall)
                 (string :tag "Time Zone Identifier"))
  :link '(info-link "(libc) TZ Variable")
  :link '(function-link format-time-string))

(defcustom transmission-add-history-variable 'transmission-add-history
  "History list to use for interactive prompts of `transmission-add'.
Consider adding the value (`transmission-add-history' by default)
to `savehist-additional-variables'."
  :type 'variable
  :link '(emacs-commentary-link "savehist"))

(defcustom transmission-tracker-history-variable 'transmission-tracker-history
  "History list to use for interactive prompts of tracker commands.
Consider adding the value (`transmission-tracker-history' by default)
to `savehist-additional-variables'."
  :type 'variable
  :link '(emacs-commentary-link "savehist"))

(defcustom transmission-torrent-functions
  '(transmission-ffap transmission-ffap-selection transmission-ffap-last-killed)
  "List of functions to use for guessing torrents for `transmission-add'.
Each function should accept no arguments, and return a string or nil."
  :type 'hook
  :options '(transmission-ffap
             transmission-ffap-selection
             transmission-ffap-last-killed))

(defcustom transmission-files-command-functions '(mailcap-file-default-commands)
  "List of functions to use for guessing default applications.
Each function should accept one argument, a list of file names,
and return a list of strings or nil."
  :type 'hook
  :options '(mailcap-file-default-commands))

(defcustom transmission-geoip-function nil
  "Function used to translate an IP address into a location name.
The function should accept an IP address and return a string or nil."
  :type '(radio (const :tag "None" nil)
                (function-item transmission-geoiplookup)
                (function :tag "Function")))

(defcustom transmission-geoip-use-cache nil
  "Whether to cache IP address/location name associations.
If non-nil, associations are stored in `transmission-geoip-table'.
Useful if `transmission-geoip-function' does not have its own
caching built in or is otherwise slow."
  :type 'boolean)

(defcustom transmission-turtle-lighter " turtle"
  "Lighter for `transmission-turtle-mode'."
  :type '(choice (const :tag "Default" " turtle")
                 (const :tag "ASCII" " ,=,e")
                 (const :tag "Emoji" " \U0001f422")
                 (string :tag "Some string"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'transmission-turtle-poll) (transmission-turtle-poll)))
  :link '(info-link "(elisp) Defining Minor Modes"))

(defconst transmission-schedules
  (eval-when-compile
    (pcase-let*
        ((`(,sun ,mon ,tues ,wed ,thurs ,fri ,sat)
          (cl-loop for x below 7 collect (ash 1 x)))
         (weekday (logior mon tues wed thurs fri))
         (weekend (logior sat sun))
         (all (logior weekday weekend)))
      `((sun . ,sun)
        (mon . ,mon)
        (tues . ,tues)
        (wed . ,wed)
        (thurs . ,thurs)
        (fri . ,fri)
        (sat . ,sat)
        (weekday . ,weekday)
        (weekend . ,weekend)
        (all . ,all))))
  "Alist of Transmission turtle mode schedules.")

(defconst transmission-mode-alist
  '((session . 0)
    (torrent . 1)
    (unlimited . 2))
  "Alist of threshold mode enumerations.")

(defconst transmission-priority-alist
  '((low . -1)
    (normal . 0)
    (high . 1))
  "Alist of names to priority values.")

(defconst transmission-status-names
  ["stopped"
   "verifywait"
   "verifying"
   "downwait"
   "downloading"
   "seedwait"
   "seeding"]
  "Array of possible Transmission torrent statuses.")

(defconst transmission-draw-torrents-keys
  ["hashString" "name" "status" "eta" "error" "labels"
   "rateDownload" "rateUpload"
   "percentDone" "sizeWhenDone" "metadataPercentComplete"
   "uploadRatio"])

(defconst transmission-draw-files-keys
  ["name" "files" "downloadDir" "wanted" "priorities"])

(defconst transmission-draw-info-keys
  ["id" "name" "hashString" "magnetLink" "labels" "activityDate" "addedDate"
   "dateCreated" "doneDate" "peers" "pieces" "pieceCount"
   "pieceSize" "trackerStats" "peersConnected" "peersGettingFromUs" "peersFrom"
   "peersSendingToUs" "sizeWhenDone" "error" "errorString" "uploadRatio"
   "downloadedEver" "corruptEver" "haveValid" "totalSize" "percentDone"
   "seedRatioLimit" "seedRatioMode" "bandwidthPriority" "downloadDir"
   "uploadLimit" "uploadLimited" "downloadLimit" "downloadLimited"
   "honorsSessionLimits" "rateDownload" "rateUpload" "queuePosition"])

(defconst transmission-file-symbols
  '(:files-wanted :files-unwanted :priority-high :priority-low :priority-normal)
  "List of \"torrent-set\" method arguments for operating on files.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(defvar transmission-add-history nil
  "Default history list for `transmission-add'.")

(defvar transmission-tracker-history nil
  "Default history list for `transmission-trackers-add' and others.")

(defvar-local transmission-torrent-vector nil
  "Vector of Transmission torrent data.")

(defvar-local transmission-torrent-id nil
  "The SHA-1 torrent info hash.")

(define-error 'transmission-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"")

(define-error 'transmission-unauthorized
  "Unauthorized user.  Check `transmission-rpc-auth'")

(define-error 'transmission-wrong-rpc-path
  "Bad RPC path.  Check `transmission-rpc-path'")

(define-error 'transmission-failure "RPC Failure")

(define-error 'transmission-misdirected
  "Unrecognized hostname.  Check \"rpc-host-whitelist\"")

(defvar transmission-timer nil
  "Timer for repeating `revert-buffer' in a visible Transmission buffer.")

(defvar transmission-geoip-table (make-hash-table :test 'equal)
  "Table for storing associations between IP addresses and location names.")

(defvar-local transmission-marked-ids nil
  "List of identifiers of the currently marked items.")

(defvar transmission-network-process-pool nil
  "List of network processes connected to Transmission.")


;; JSON RPC

(defun transmission--move-to-content ()
  "Move the point to beginning of content after the headers."
  (goto-char (point-min))
  (re-search-forward "^\r?\n" nil t))

(defun transmission--content-finished-p ()
  "Return non-nil if all of the content has arrived."
  (goto-char (point-min))
  (when (search-forward "Content-Length: " nil t)
    (let ((length (read (current-buffer))))
      (and (transmission--move-to-content)
           (<= length (- (position-bytes (point-max))
                         (position-bytes (point))))))))

(defun transmission--status ()
  "Check the HTTP status code.
A 409 response from a Transmission session includes the
\"X-Transmission-Session-Id\" header.  If a 409 is received,
update `transmission-session-id' and signal the error."
  (goto-char (point-min))
  (forward-char 5) ; skip "HTTP/"
  (skip-chars-forward "0-9.")
  (let* ((buffer (current-buffer))
         (status (read buffer)))
    (pcase status
      (200 (let (result)
             (when (and (transmission--move-to-content)
                        (search-forward "\"result\":" nil t)
                        (not (equal "success" (setq result (json-read)))))
               (signal 'transmission-failure (list result)))))
      ((or 301 404 405) (signal 'transmission-wrong-rpc-path (list status)))
      (401 (signal 'transmission-unauthorized (list status)))
      (403 (signal 'transmission-failure (list status)))
      (409 (when (search-forward "X-Transmission-Session-Id: ")
             (setq transmission-session-id (read buffer))
             (signal 'transmission-conflict (list status))))
      (421 (signal 'transmission-misdirected (list transmission-host))))))

(defun transmission--auth-source-secret (user)
  "Return the secret for USER at found in `auth-sources'.
Unless otherwise specified in `transmission-rpc-auth', the host
and port default to `transmission-host' and
`transmission-service', respectively."
  (let ((spec (copy-sequence transmission-rpc-auth)))
    (unless (plist-get spec :host) (plist-put spec :host transmission-host))
    (unless (plist-get spec :port) (plist-put spec :port transmission-service))
    (apply #'auth-source-pick-first-password (nconc `(:user ,user) spec))))

(defun transmission--auth-string ()
  "HTTP \"Authorization\" header value if `transmission-rpc-auth' is populated."
  (when transmission-rpc-auth
    (let* ((user (plist-get transmission-rpc-auth :username))
           (pass (and user (or (plist-get transmission-rpc-auth :password)
                               (transmission--auth-source-secret user)))))
      (concat "Basic " (base64-encode-string (concat user ":" pass) t)))))

(defun transmission-http-post (process content)
  "Send to PROCESS an HTTP POST request containing CONTENT."
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((headers (list (cons "X-Transmission-Session-Id" transmission-session-id)
                       (cons "Host" transmission-host) ; CVE-2018-5702
                       (cons "Content-length" (string-bytes content)))))
    (let ((auth (transmission--auth-string)))
      (when auth (push (cons "Authorization" auth) headers)))
    (with-temp-buffer
      (insert (concat "POST " transmission-rpc-path " HTTP/1.1\r\n"))
      (dolist (elt headers)
        (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
      (insert "\r\n" content)
      (process-send-region process (point-min) (point-max)))))

(defun transmission-wait (process)
  "Wait to receive HTTP response from PROCESS.
Return JSON object parsed from content."
  (with-current-buffer (process-buffer process)
    (while (and (not (transmission--content-finished-p))
                (process-live-p process))
      (accept-process-output process 1))
    (transmission--status)
    (transmission--move-to-content)
    (when (search-forward "\"arguments\":" nil t)
      (json-read))))

(defun transmission-send (process content)
  "Send PROCESS string CONTENT and wait for response synchronously."
  (transmission-http-post process content)
  (transmission-wait process))

(defun transmission-process-sentinel (process _message)
  "Sentinel for PROCESS made by `transmission-make-network-process'."
  (setq transmission-network-process-pool
        (delq process transmission-network-process-pool))
  (when (buffer-live-p (process-buffer process))
    (kill-buffer (process-buffer process))))

(defun transmission-make-network-process ()
  "Return a network client process connected to a Transmission daemon.
When creating a new connection, the address is determined by the
custom variables `transmission-host' and `transmission-service'."
  (let ((socket (when (file-name-absolute-p transmission-host)
                  (expand-file-name transmission-host)))
        buffer process)
    (unwind-protect
        (prog1
            (setq buffer (generate-new-buffer " *transmission*")
                  process
                  (make-network-process
                   :name "transmission" :buffer buffer
                   :host (when (null socket) transmission-host)
                   :service (or socket transmission-service)
                   :family (when socket 'local) :noquery t
                   :coding 'binary :filter-multibyte nil))
          (setq buffer nil process nil))
      (when (process-live-p process) (kill-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun transmission-get-network-process ()
  "Return a network client process connected to a Transmission daemon.
Returns a stopped process in `transmission-network-process-pool'
or, if none is found, establishes a new connection and adds it to
the pool."
  (or (cl-loop for process in transmission-network-process-pool
               when (process-command process) return (continue-process process))
      (let ((process (transmission-make-network-process)))
        (push process transmission-network-process-pool)
        process)))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission and return a JSON object.
The JSON is the \"arguments\" object decoded Transmission's response.

METHOD is a string.
ARGUMENTS is a plist having keys corresponding to METHOD.
TAG is an integer and ignored.

Details regarding the Transmission RPC can be found here:
<https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt>"
  (let ((process (transmission-get-network-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (set-process-plist process nil)
    (set-process-filter process nil)
    (set-process-sentinel process nil)
    (unwind-protect
        (condition-case err
            (transmission-send process content)
          (transmission-conflict
           (transmission-send process content))
          (transmission-failure
           (message "%s" (cdr err))))
      (if (process-live-p process) (stop-process process)
        (setq transmission-network-process-pool
              (delq process transmission-network-process-pool))
        (kill-buffer (process-buffer process))))))


;; Asynchronous calls

(defun transmission-process-callback (process)
  "Call PROCESS's callback if it has one."
  (let ((callback (process-get process :callback)))
    (when callback
      (transmission--move-to-content)
      (when (search-forward "\"arguments\":" nil t)
        (run-at-time 0 nil callback (json-read))))))

(defun transmission-process-filter (process text)
  "Handle PROCESS's output TEXT and trigger handlers."
  (internal-default-process-filter process text)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (transmission--content-finished-p)
        (condition-case e
            (progn (transmission--status)
                   (transmission-process-callback process)
                   (stop-process process))
          (transmission-conflict
           (transmission-http-post process (process-get process :request)))
          (transmission-failure
           (message "%s" (cdr e)))
          (error
           (stop-process process)
           (signal (car e) (cdr e))))))))

(defun transmission-request-async (callback method &optional arguments tag)
  "Send a request to Transmission asynchronously.

CALLBACK accepts one argument, the response \"arguments\" JSON object.
METHOD, ARGUMENTS, and TAG are the same as in `transmission-request'."
  (let ((process (transmission-get-network-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (set-process-filter process #'transmission-process-filter)
    (set-process-sentinel process #'transmission-process-sentinel)
    (process-put process :request content)
    (process-put process :callback callback)
    (transmission-http-post process content)
    process))


;; Response destructuring

(defun transmission-torrents (response)
  "Return the \"torrents\" array in RESPONSE, otherwise nil."
  (let ((torrents (cdr (assq 'torrents response))))
    (and (< 0 (length torrents)) torrents)))

(defun transmission-unique-labels (torrents)
  "Return a list of unique labels from TORRENTS."
  (let (labels res)
    (dotimes (i (length torrents))
      (dotimes (j (length (setq labels (cdr (assq 'labels (aref torrents i))))))
        (cl-pushnew (aref labels j) res :test #'equal)))
    res))


;; Timer management

(defun transmission-timer-revert ()
  "Revert the buffer or cancel `transmission-timer'."
  (if (and (memq major-mode transmission-refresh-modes)
           (not (or (bound-and-true-p isearch-mode)
                    (buffer-narrowed-p)
                    (use-region-p))))
      (revert-buffer)
    (cancel-timer transmission-timer)))

(defun transmission-timer-run ()
  "Run the timer `transmission-timer'."
  (when transmission-timer (cancel-timer transmission-timer))
  (setq
   transmission-timer
   (run-at-time t transmission-refresh-interval #'transmission-timer-revert)))

(defun transmission-timer-check ()
  "Check if current buffer should run a refresh timer."
  (when (memq major-mode transmission-refresh-modes)
    (transmission-timer-run)))


;; Other

(defun transmission-refs (sequence key)
  "Return a list of the values of KEY in each element of SEQUENCE."
  (mapcar (lambda (x) (cdr (assq key x))) sequence))

(defun transmission-size (bytes)
  "Return string showing size BYTES in human-readable form."
  (file-size-human-readable bytes transmission-units))

(defun transmission-percent (have total)
  "Return the percentage of HAVE by TOTAL."
  (if (zerop total) 0 (/ (* 100.0 have) total)))

(defun transmission-slice (str k)
  "Slice STRING into K strings of somewhat equal size.
The result can have no more elements than STRING.
\n(fn STRING K)"
  (let ((len (length str)))
    (let ((quotient (/ len k))
          (remainder (% len k))
          (i 0)
          slice result)
      (while (and (/= 0 (setq len (length str))) (< i k))
        (setq slice (if (< i remainder) (1+ quotient) quotient))
        (push (substring str 0 (min slice len)) result)
        (setq str (substring str (min slice len) len))
        (cl-incf i))
      (nreverse result))))

(defun transmission-text-property-all (beg end prop)
  "Return a list of non-nil values of a text property PROP between BEG and END.
If none are found, return nil."
  (let (res pos)
    (save-excursion
      (goto-char beg)
      (while (> end (point))
        (push (get-text-property (point) prop) res)
        (setq pos (text-property-not-all (point) end prop (car-safe res)))
        (goto-char (or pos end))))
    (nreverse (delq nil res))))

(defun transmission-eta (seconds percent)
  "Return a string showing SECONDS in human-readable form;
otherwise some other estimate indicated by SECONDS and PERCENT."
  (if (<= seconds 0)
      (if (= percent 1) "Done"
        (if (char-displayable-p #x221e) "\u221e" "Inf"))
    (let* ((minute 60.0)
           (hour 3600.0)
           (day 86400.0)
           (month (* 29.53 day))
           (year (* 365.25 day)))
      (apply #'format "%.0f%s"
             (cond
              ((> minute seconds) (list seconds "s"))
              ((> hour seconds) (list (/ seconds minute) "m"))
              ((> day seconds) (list (/ seconds hour) "h"))
              ((> month seconds) (list (/ seconds day) "d"))
              ((> year seconds) (list (/ seconds month) "mo"))
              (t (list (/ seconds year) "y")))))))

(defun transmission-when (seconds)
  "The `transmission-eta' of time between `current-time' and SECONDS."
  (if (<= seconds 0) "never"
    (let ((secs (- seconds (float-time (current-time)))))
      (format (if (< secs 0) "%s ago" "in %s")
              (transmission-eta (abs secs) nil)))))

(defun transmission-rate (bytes)
  "Return a rate in units kilobytes per second.
The rate is calculated from BYTES according to `transmission-units'."
  (/ bytes (if (eq 'iec transmission-units) 1024 1000)))

(defun transmission-throttle-torrent (ids limit n)
  "Set transfer speed limit for IDS.
LIMIT is a keyword; either :uploadLimit or :downloadLimit.
N is the desired threshold.  A negative value of N means to disable the limit."
  (cl-assert (memq limit '(:uploadLimit :downloadLimit)))
  (let ((arguments `(:ids ,ids ,(pcase limit
                                  (:uploadLimit :uploadLimited)
                                  (:downloadLimit :downloadLimited))
                     ,@(if (< n 0) '(:json-false) `(t ,limit ,n)))))
    (transmission-request-async nil "torrent-set" arguments)))

(defun transmission-torrent-honors-speed-limits-p ()
  "Return non-nil if torrent honors session speed limits, otherwise nil."
  (eq t (cdr (assq 'honorsSessionLimits (elt transmission-torrent-vector 0)))))

(defun transmission-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.
If UPLOAD is non-nil, make a prompt for upload rate, otherwise
for download rate."
  (let ((args '(:fields ["speed-limit-up" "speed-limit-down"
                         "speed-limit-up-enabled" "speed-limit-down-enabled"])))
    (let-alist (transmission-request "session-get" args)
      (let ((limit (if upload .speed-limit-up .speed-limit-down))
            (enabled (eq t (if upload .speed-limit-up-enabled
                             .speed-limit-down-enabled))))
        (list (read-number (concat "Set global " (if upload "up" "down") "load limit ("
                                   (if enabled (format "%d kB/s" limit) "disabled")
                                   "): ")))))))

(defun transmission-prompt-ratio-limit ()
  "Make a prompt to set global seed ratio limit."
  (let ((arguments '(:fields ["seedRatioLimit" "seedRatioLimited"])))
    (let-alist (transmission-request "session-get" arguments)
      (let ((limit .seedRatioLimit)
            (enabled (eq t .seedRatioLimited)))
        (list (read-number (concat "Set global seed ratio limit ("
                                   (if enabled (format "%.1f" limit) "disabled")
                                   "): ")))))))

(defun transmission-read-strings (prompt &optional collection history filter)
  "Read strings until an input is blank, with optional completion.
PROMPT, COLLECTION, and HISTORY are the same as in `completing-read'.
FILTER is a predicate that prevents adding failing input to HISTORY.
Returns a list of non-blank inputs."
  (let ((history-add-new-input (null history))
        res entry)
    (while (and (setq entry (if (not collection) (read-string prompt nil history)
                              (completing-read prompt collection nil nil nil history)))
                (not (string-empty-p entry))
                (not (string-blank-p entry)))
      (when (and history (or (null filter) (funcall filter entry)))
        (add-to-history history entry))
      (push entry res)
      (when (consp collection)
        (setq collection (delete entry collection))))
    (nreverse res)))

(defun transmission-read-time (prompt)
  "Read an expression for time, prompting with string PROMPT.
Uses `diary-entry-time' to parse user input.
Returns minutes from midnight, otherwise nil."
  (let ((hhmm (diary-entry-time (read-string prompt))))
    (when (>= hhmm 0) (+ (% hhmm 100) (* 60 (/ hhmm 100))))))

(defun transmission-format-minutes (minutes)
  "Return a formatted string from MINUTES from midnight."
  (format-time-string "%H:%M" (seconds-to-time (* 60 (+ 300 minutes)))))

(defun transmission-n->days (n)
  "Return days corresponding to bitfield N.
Days are the keys of `transmission-schedules'."
  (cond
   ((let ((cell (rassq n transmission-schedules)))
      (when cell (list (car cell)))))
   ((let (res)
      (pcase-dolist (`(,k . ,v) transmission-schedules)
        (unless (zerop (logand n v))
          (push k res)
          (cl-decf n v)))
      (nreverse res)))))

(defun transmission-levi-civita (a b c)
  "Return Levi-Civita symbol value for three numbers A, B, C."
  (cond
   ((or (< a b c) (< b c a) (< c a b)) 1)
   ((or (< c b a) (< a c b) (< b a c)) -1)
   ((or (= a b) (= b c) (= c a)) 0)))

(defun transmission-turtle-when (beg end &optional now)
  "Calculate the time in seconds until the next schedule change.
BEG END are minutes after midnight of schedules start and end.
NOW is a time, defaulting to `current-time'."
  (let* ((time (or now (current-time)))
         (hours (string-to-number (format-time-string "%H" time)))
         (minutes (+ (* 60 hours)
                     (string-to-number (format-time-string "%M" time)))))
    (pcase (transmission-levi-civita minutes beg end)
      (1 (* 60 (if (> beg minutes) (- beg minutes) (+ beg minutes))))
      (-1 (* 60 (if (> end minutes) (- end minutes) (+ end minutes))))
      ;; FIXME this should probably just return 0 because of inaccuracy
      (0 (* 60 (or (and (= minutes beg) end) (and (= minutes end) beg)))))))

(defun transmission-tracker-url-p (str)
  "Return non-nil if STR is not just a number."
  (let ((match (string-match "[^[:blank:]]" str)))
    (when match (null (<= ?0 (aref str match) ?9)))))

(defun transmission-tracker-stats (id)
  "Return the \"trackerStats\" array for torrent id ID."
  (let* ((arguments `(:ids ,id :fields ["trackerStats"]))
         (response (transmission-request "torrent-get" arguments)))
    (cdr (assq 'trackerStats (elt (transmission-torrents response) 0)))))

(defun transmission-unique-announce-urls ()
  "Return a list of unique announce URLs from all current torrents."
  (let ((response (transmission-request "torrent-get" '(:fields ["trackers"])))
        torrents trackers res)
    (dotimes (i (length (setq torrents (transmission-torrents response))))
      (dotimes (j (length (setq trackers (cdr (assq 'trackers (aref torrents i))))))
        (cl-pushnew (cdr (assq 'announce (aref trackers j))) res :test #'equal)))
    res))

(defun transmission-btih-p (string)
  "Return STRING if it is a BitTorrent info hash, otherwise nil."
  (and string (string-match (rx bos (= 40 xdigit) eos) string) string))

(defun transmission-directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (last ?.))
    (if (> len 0) (setq last (aref name (1- len))))
    (or (= last ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= last ?\\)))))

(defun transmission-ffap ()
  "Return a file name, URL, or info hash at point, otherwise nil."
  (or (get-text-property (point) 'shr-url)
      (get-text-property (point) :nt-link)
      (let ((fn (run-hook-with-args-until-success 'file-name-at-point-functions)))
        (unless (transmission-directory-name-p fn) fn))
      (url-get-url-at-point)
      (transmission-btih-p (thing-at-point 'word))))

(defun transmission-ffap-string (string)
  "Apply `transmission-ffap' to the beginning of STRING."
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (transmission-ffap))))

(defun transmission-ffap-last-killed ()
  "Apply `transmission-ffap' to the most recent `kill-ring' entry."
  (transmission-ffap-string (car kill-ring)))

(defun transmission-ffap-selection ()
  "Apply `transmission-ffap' to the graphical selection."
  (transmission-ffap-string (with-no-warnings (x-get-selection))))

(defun transmission-files-do (action)
  "Apply ACTION to files in `transmission-files-mode' buffers."
  (cl-assert (memq action transmission-file-symbols))
  (let ((id transmission-torrent-id)
        (prop 'tabulated-list-id)
        indices)
    (setq indices
          (or transmission-marked-ids
              (if (null (use-region-p))
                  (list (cdr (assq 'index (get-text-property (point) prop))))
                (transmission-refs (transmission-text-property-all
                                    (region-beginning) (region-end) prop)
                                   'index))))
    (if (and id indices)
        (let ((arguments (list :ids id action indices)))
          (transmission-request-async nil "torrent-set" arguments))
      (user-error "No files selected or at point"))))

(defun transmission-files-file-at-point ()
  "Return the absolute path of the torrent file at point, or nil.
If the file named \"foo\" does not exist, try \"foo.part\" before returning."
  (let* ((dir (cdr (assq 'downloadDir (elt transmission-torrent-vector 0))))
         (base (or (and dir (cdr (assq 'name (tabulated-list-get-id))))
                   (user-error "No file at point")))
         (filename (and base (expand-file-name base dir))))
    (or (file-exists-p filename)
        (let ((part (concat filename ".part")))
          (and (file-exists-p part) (setq filename part))))
    (if filename (abbreviate-file-name filename)
      (user-error "File does not exist"))))

(defun transmission-files-index (torrent)
  "Return an array containing file data from TORRENT."
  (let-alist torrent
    (let* ((n (length .files))
           (res (make-vector n 0)))
      (dotimes (i n)
        (aset res i (append (aref .files i)
                            (list (cons 'wanted (aref .wanted i))
                                  (cons 'priority (aref .priorities i))
                                  (cons 'index i)))))
      res)))

(defun transmission-files-prefix (files)
  "Return a directory name that is a prefix of every path in FILES, otherwise nil."
  (when (> (length files) 0)
    (let ((ref (cdr (assq 'name (aref files 0))))
          (start 0)
          end)
      (setq files (substring files 1))
      (while (and (prog1 (string-match "/" ref start)
                    (setq end (match-end 0)))
                  (cl-loop for file across files
                           always (eq t (compare-strings
                                         ref start end (cdr (assq 'name file)) start end))))
        (setq start end))
      (substring ref 0 start))))

(defun transmission-geoiplookup (ip)
  "Return country name associated with IP using geoiplookup(1)."
  (let ((program (if (string-match-p ":" ip) "geoiplookup6" "geoiplookup")))
    (when (executable-find program)
      (with-temp-buffer
        (call-process program nil t nil ip)
        (car (last (split-string (buffer-string) ": " t "[ \t\r\n]*")))))))

(defun transmission-geoip-retrieve (ip)
  "Retrieve value of IP in `transmission-geoip-table'.
If IP is not a key, add it with the value from `transmission-geoip-function'.
If `transmission-geoip-function' has changed, reset `transmission-geoip-table'."
  (let ((fun transmission-geoip-function)
        (cache transmission-geoip-table))
    (when (functionp fun)
      (if (not transmission-geoip-use-cache)
          (funcall fun ip)
        (if (eq fun (get 'transmission-geoip-table :fn))
            (or (gethash ip cache)
                (setf (gethash ip cache) (funcall fun ip)))
          (setq cache (make-hash-table :test 'equal))
          (put 'transmission-geoip-table :fn fun)
          (setf (gethash ip cache) (funcall fun ip)))))))

(defun transmission-time (seconds)
  "Format a time string, given SECONDS from the epoch."
  (if (= 0 seconds) "Never"
    (format-time-string transmission-time-format (seconds-to-time seconds)
                        transmission-time-zone)))

(defun transmission-hamming-weight (byte)
  "Calculate the Hamming weight of BYTE."
  (setq byte (- byte (logand (ash byte -1) #x55555555)))
  (setq byte (+ (logand byte #x33333333) (logand (ash byte -2) #x33333333)))
  (ash (* (logand (+ byte (ash byte -4)) #x0f0f0f0f) #x01010101) -24))

(defun transmission-count-bits (bytearray)
  "Calculate sum of Hamming weight of each byte in BYTEARRAY."
  (cl-loop for x across bytearray sum (transmission-hamming-weight x)))

(defun transmission-byte->string (byte)
  "Format integer BYTE into a string."
  (let* ((calc-number-radix 2)
         (string (math-format-binary byte)))
    (concat (make-string (- 8 (length string)) ?0) string)))

(defun transmission-ratio->glyph (ratio)
  "Return a single-char string representing RATIO."
  (cond
   ((= 0 ratio) " ")
   ((< ratio 0.333) "\u2591")
   ((< ratio 0.667) "\u2592")
   ((< ratio 1) "\u2593")
   ((= 1 ratio) "\u2588")))

(defun transmission-ratio->256 (ratio)
  "Return a grey font-locked single-space string according to RATIO.
Uses color names for the 256 color palette."
  (let ((n (if (= 1 ratio) 231 (+ 236 (* 19 ratio)))))
    (propertize " " 'font-lock-face `(:background ,(format "color-%d" n)))))

(defun transmission-ratio->grey (ratio)
  "Return a grey font-locked single-space string according to RATIO."
  (let ((l (+ 0.2 (* 0.8 ratio))))
    (propertize " " 'font-lock-face `(:background ,(color-rgb-to-hex l l l))
                'help-echo (format "%.2f" ratio))))

(defun transmission-group-digits (n)
  "Group digits of positive number N with `transmission-digit-delimiter'."
  (if (< n 10000) (number-to-string n)
    (let ((calc-group-char transmission-digit-delimiter))
      (math-group-float (number-to-string n)))))

(defun transmission-plural (n s)
  "Return a pluralized string expressing quantity N of thing S.
Done in the spirit of `dired-plural-s'."
  (let ((m (if (= -1 n) 0 n)))
    (concat (transmission-group-digits m) " " s (when (/= m 1) "s"))))

(defun transmission-format-size (bytes)
  "Format size BYTES into a more readable string."
  (format "%s (%s bytes)" (transmission-size bytes)
          (transmission-group-digits bytes)))

(defun transmission-toggle-mark-at-point ()
  "Toggle mark of item at point.
Registers the change in `transmission-marked-ids'."
  (let* ((eid (tabulated-list-get-id))
         (id (cdr (or (assq 'hashString eid) (assq 'index eid)))))
    (if (member id transmission-marked-ids)
        (progn
          (setq transmission-marked-ids (delete id transmission-marked-ids))
          (tabulated-list-put-tag " "))
      (push id transmission-marked-ids)
      (tabulated-list-put-tag ">"))
    (set-buffer-modified-p nil)))

(defun transmission-move-to-file-name ()
  "Move to the beginning of the filename on the current line."
  (let* ((eol (line-end-position))
         (change (next-single-property-change (point) 'transmission-name nil eol)))
    (when (and change (< change eol))
      (goto-char change))))

(defun transmission-file-name-matcher (limit)
  (let ((beg (next-single-property-change (point) 'transmission-name nil limit)))
    (when (and beg (< beg limit))
      (goto-char beg)
      (let ((end (next-single-property-change (point) 'transmission-name nil limit)))
        (when (and end (<= end limit))
          (set-match-data (list beg end))
          (goto-char end))))))

(defmacro transmission-interactive (&rest spec)
  "Specify interactive use of a function.
The symbol `ids' is bound to a list of torrent IDs marked, at
point or in region, otherwise a `user-error' is signalled."
  (declare (debug t))
  (let ((region (make-symbol "region"))
        (marked (make-symbol "marked"))
        (torrent (make-symbol "torrent")))
    `(interactive
      (let ((,torrent transmission-torrent-id) ,marked ,region ids)
        (setq ids (or (and ,torrent (list ,torrent))
                      (setq ,marked transmission-marked-ids)))
        (when (null ids)
          (if (setq ,region (use-region-p))
              (setq ids
                    (cl-loop for x in
                     (transmission-text-property-all
                      (region-beginning) (region-end) 'tabulated-list-id)
                     collect (cdr (assq 'hashString x))))
            (let ((value (tabulated-list-get-id (point))))
              (when value (setq ids (list (cdr (assq 'hashString value))))))))
        (if (null ids) (user-error "No torrent selected")
          ,@(cl-labels
                ((expand (form x)
                   (cond
                    ((atom form) form)
                    ((and (listp form)
                          (memq (car form)
                                '(read-number y-or-n-p yes-or-no-p
                                  completing-read transmission-read-strings)))
                     (pcase form
                       (`(read-number ,prompt . ,rest)
                        `(read-number (concat ,prompt ,x) ,@rest))
                       (`(y-or-n-p ,prompt)
                        `(y-or-n-p (concat ,prompt ,x)))
                       (`(yes-or-no-p ,prompt)
                        `(yes-or-no-p (concat ,prompt ,x)))
                       (`(completing-read ,prompt . ,rest)
                        `(completing-read (concat ,prompt ,x) ,@rest))
                       (`(transmission-read-strings ,prompt . ,rest)
                        `(transmission-read-strings (concat ,prompt ,x) ,@rest))))
                    ((or (listp form) (null form))
                     (mapcar (lambda (subexp) (expand subexp x)) form))
                    (t (error "Bad syntax: %S" form)))))
              (expand spec
                      `(cond
                        (,marked (format "[%d marked] " (length ,marked)))
                        (,region (format "[%d in region] " (length ids)))))))))))

(defun transmission-collect-hook (hook &rest args)
  "Run HOOK with ARGS and return a list of non-nil results from its elements."
  (let (res)
    (cl-flet
        ((collect (fun &rest args)
           (let ((val (apply fun args)))
             (when val (cl-pushnew val res :test #'equal))
             nil)))
      (apply #'run-hook-wrapped hook #'collect args)
      (nreverse res))))

(defmacro transmission-with-window-maybe (window &rest body)
  "If WINDOW is non-nil, execute BODY with WINDOW current.
Otherwise, just execute BODY."
  (declare (indent 1) (debug t))
  `(if (null ,window) (progn ,@body)
     (with-selected-window ,window
       ,@body)))

(defun transmission-window->state (window)
  "Return a list containing some state of WINDOW.
A simplification of `window-state-get', the list associates
WINDOW with `window-start' and the line/column coordinates of `point'."
  (transmission-with-window-maybe window
    (save-restriction
      (widen)
      (list window (window-start) (line-number-at-pos) (current-column)))))

(defun transmission-restore-state (state)
  "Set `window-start' and `window-point' according to STATE."
  (pcase-let ((`(,window ,start ,line ,column) state))
    (transmission-with-window-maybe window
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (setf (window-start) start))))

(defmacro transmission-with-saved-state (&rest body)
  "Execute BODY, restoring window position, point, and mark."
  (declare (indent 0) (debug t))
  (let ((old-states (make-symbol "old-states"))
        (old-mark (make-symbol "old-mark"))
        (old-mark-active (make-symbol "old-mark-active")))
    `(let* ((,old-states (or (mapcar #'transmission-window->state
                                     (get-buffer-window-list nil nil t))
                             (list (transmission-window->state nil))))
            (,old-mark (if (not (region-active-p)) (mark)
                         (let ((beg (region-beginning)))
                           (if (= (window-point) beg) (region-end) beg))))
            (,old-mark-active mark-active))
       ,@body
       (mapc #'transmission-restore-state ,old-states)
       (and ,old-mark (set-mark ,old-mark))
       (unless ,old-mark-active (deactivate-mark)))))


;; Interactive

;;;###autoload
(defun transmission-add (torrent &optional directory)
  "Add TORRENT by filename, URL, magnet link, or info hash.
When called with a prefix, prompt for DIRECTORY."
  (interactive
   (let* ((f (transmission-collect-hook 'transmission-torrent-functions))
          (def (mapcar #'file-relative-name f))
          (prompt (concat "Add torrent" (if def (format " [%s]" (car def))) ": "))
          (history-add-new-input nil)
          (file-name-history (symbol-value transmission-add-history-variable))
          (input (read-file-name prompt nil def)))
     (add-to-history transmission-add-history-variable input)
     (list input
           (if current-prefix-arg
               (read-directory-name "Target directory: ")))))
  (transmission-request-async
   (lambda (response)
     (let-alist response
       (or (and .torrent-added.name
                (message "Added %s" .torrent-added.name))
           (and .torrent-duplicate.name
                (message "Already added %s" .torrent-duplicate.name)))))
   "torrent-add"
   (append (if (and (file-readable-p torrent) (not (file-directory-p torrent)))
               `(:metainfo ,(with-temp-buffer
                              (insert-file-contents-literally torrent)
                              (base64-encode-string (buffer-string) t)))
             (setq torrent (string-trim torrent))
             `(:filename ,(if (transmission-btih-p torrent)
                              (concat "magnet:?xt=urn:btih:" torrent)
                            torrent)))
           (when directory (list :download-dir (expand-file-name directory))))))

(defun transmission-free (directory)
  "Show in the echo area how much free space is in DIRECTORY."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (transmission-request-async
   (lambda (response)
     (let-alist response
       (message "%s free in %s" (transmission-format-size .size-bytes)
                (abbreviate-file-name .path))))
   "free-space" (list :path (expand-file-name directory))))

(defun transmission-stats ()
  "Message some information about the session."
  (interactive)
  (transmission-request-async
   (lambda (response)
     (let-alist response
       (message (concat "%d kB/s down, %d kB/s up; %d/%d torrents active; "
                        "%s received, %s sent; uptime %s")
                (transmission-rate .downloadSpeed)
                (transmission-rate .uploadSpeed)
                .activeTorrentCount .torrentCount
                (transmission-size .current-stats.downloadedBytes)
                (transmission-size .current-stats.uploadedBytes)
                (transmission-eta .current-stats.secondsActive nil))))
   "session-stats"))

(defun transmission-move (ids location)
  "Move torrent at point, marked, or in region to a new LOCATION."
  (transmission-interactive
   (let* ((dir (read-directory-name "New directory: "))
          (prompt (format "Move torrent%s to %s? " (if (cdr ids) "s" "") dir)))
     (if (y-or-n-p prompt) (list ids dir) '(nil nil))))
  (when ids
    (let ((arguments (list :ids ids :move t :location (expand-file-name location))))
      (transmission-request-async nil "torrent-set-location" arguments))))

(defun transmission-reannounce (ids)
  "Reannounce torrent at point, marked, or in region."
  (transmission-interactive (list ids))
  (when ids
    (transmission-request-async nil "torrent-reannounce" (list :ids ids))))

(defun transmission-remove (ids &optional unlink)
  "Prompt to remove torrent at point or torrents marked or in region.
When called with a prefix UNLINK, also unlink torrent data on disk."
  (transmission-interactive
   (if (yes-or-no-p (concat "Remove " (and current-prefix-arg "and unlink ")
                            "torrent" (and (cdr ids) "s") "? "))
       (progn (setq deactivate-mark t transmission-marked-ids nil)
              (list ids current-prefix-arg))
     '(nil nil)))
  (when ids
    (let ((arguments `(:ids ,ids :delete-local-data ,(and unlink t))))
      (transmission-request-async nil "torrent-remove" arguments))))

(defun transmission-delete (ids)
  "Prompt to delete (unlink) torrent at point or torrents marked or in region."
  (transmission-interactive
   (list
    (and (yes-or-no-p (concat "Delete torrent" (and (cdr ids) "s") "? "))
         (setq transmission-marked-ids nil deactivate-mark t)
         ids)))
  (when ids
    (transmission-request-async nil "torrent-remove" `(:ids ,ids :delete-local-data t))))

(defun transmission-set-bandwidth-priority (ids priority)
  "Set bandwidth priority of torrent(s) at point, in region, or marked."
  (transmission-interactive
   (let* ((prompt "Set bandwidth priority: ")
          (priority (completing-read prompt transmission-priority-alist nil t))
          (number (cdr (assoc-string priority transmission-priority-alist))))
     (list (when number ids) number)))
  (when ids
    (let ((arguments `(:ids ,ids :bandwidthPriority ,priority)))
      (transmission-request-async nil "torrent-set" arguments))))

(defun transmission-set-download (limit)
  "Set global download speed LIMIT in kB/s."
  (interactive (transmission-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-upload (limit)
  "Set global upload speed LIMIT in kB/s."
  (interactive (transmission-prompt-speed-limit t))
  (let ((arguments (if (< limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-ratio (limit)
  "Set global seed ratio LIMIT."
  (interactive (transmission-prompt-ratio-limit))
  (let ((arguments (if (< limit 0) '(:seedRatioLimited :json-false)
                     `(:seedRatioLimited t :seedRatioLimit ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-torrent-download (ids)
  "Set download limit of selected torrent(s) in kB/s."
  (transmission-interactive (list ids))
  (if (cdr ids)
      (let ((prompt "Set torrents' download limit: "))
        (transmission-throttle-torrent ids :downloadLimit (read-number prompt)))
    (transmission-request-async
     (lambda (response)
       (let-alist (elt (transmission-torrents response) 0)
         (let* ((s (if (eq t .downloadLimited) (format "%d kB/s" .downloadLimit) "disabled"))
                (prompt (concat "Set torrent's download limit (" s "): ")))
           (transmission-throttle-torrent ids :downloadLimit (read-number prompt)))))
     "torrent-get" `(:ids ,ids :fields ["downloadLimit" "downloadLimited"]))))

(defun transmission-set-torrent-upload (ids)
  "Set upload limit of selected torrent(s) in kB/s."
  (transmission-interactive (list ids))
  (if (cdr ids)
      (let ((prompt "Set torrents' upload limit: "))
        (transmission-throttle-torrent ids :uploadLimit (read-number prompt)))
    (transmission-request-async
     (lambda (response)
       (let-alist (elt (transmission-torrents response) 0)
         (let* ((s (if (eq t .uploadLimited) (format "%d kB/s" .uploadLimit) "disabled"))
                (prompt (concat "Set torrent's upload limit (" s "): ")))
           (transmission-throttle-torrent ids :uploadLimit (read-number prompt)))))
     "torrent-get" `(:ids ,ids :fields ["uploadLimit" "uploadLimited"]))))

(defun transmission-set-torrent-ratio (ids mode limit)
  "Set seed ratio limit of selected torrent(s)."
  (transmission-interactive
   (let* ((prompt (concat "Set torrent" (if (cdr ids) "s'" "'s") " ratio mode: "))
          (mode (completing-read prompt transmission-mode-alist nil t))
          (n (cdr (assoc-string mode transmission-mode-alist))))
     (list ids n (when (= n 1) (read-number "Set torrent ratio limit: ")))))
  (when ids
    (let ((arguments `(:ids ,ids :seedRatioMode ,mode
                       ,@(when limit `(:seedRatioLimit ,limit)))))
      (transmission-request-async nil "torrent-set" arguments))))

(defun transmission-toggle-limits (ids)
  "Toggle whether selected torrent(s) honor session speed limits."
  (transmission-interactive (list ids))
  (when ids
    (transmission-request-async
     (lambda (response)
       (let* ((torrents (transmission-torrents response))
              (honor (pcase (cdr (assq 'honorsSessionLimits (elt torrents 0)))
                       (:json-false t) (_ :json-false))))
         (transmission-request-async nil "torrent-set"
                                     `(:ids ,ids :honorsSessionLimits ,honor))))
     "torrent-get" `(:ids ,ids :fields ["honorsSessionLimits"]))))

(defun transmission-toggle (ids)
  "Toggle selected torrent(s) between started and stopped."
  (transmission-interactive (list ids))
  (when ids
    (transmission-request-async
     (lambda (response)
       (let* ((torrents (transmission-torrents response))
              (status (and torrents (cdr (assq 'status (elt torrents 0)))))
              (method (and status
                           (if (zerop status) "torrent-start" "torrent-stop"))))
         (when method (transmission-request-async nil method (list :ids ids)))))
     "torrent-get" (list :ids ids :fields ["status"]))))

(defun transmission-label (ids labels)
  "Set labels for selected torrent(s)."
  (transmission-interactive
   (let* ((response (transmission-request "torrent-get" '(:fields ["labels"])))
          (torrents (transmission-torrents response)))
     (list ids (transmission-read-strings "Labels: " (transmission-unique-labels torrents)))))
  (transmission-request-async
   nil "torrent-set" (list :ids ids :labels (vconcat labels))))

(defun transmission-trackers-add (ids urls)
  "Add announce URLs to selected torrent or torrents."
  (transmission-interactive
   (let* ((trackers (transmission-refs (transmission-tracker-stats ids) 'announce))
          (urls (or (transmission-read-strings
                     "Add announce URLs: "
                     (cl-loop for url in
                              (append transmission-trackers
                                      (transmission-unique-announce-urls))
                              unless (member url trackers) collect url)
                     transmission-tracker-history-variable
                     #'transmission-tracker-url-p)
                    (user-error "No trackers to add"))))
     (list ids
           ;; Don't add trackers that are already there
           (cl-loop for url in urls
                    unless (member url trackers) collect url))))
  (transmission-request-async
   (lambda (_) (message "Added %s" (mapconcat #'identity urls ", ")))
   "torrent-set" (list :ids ids :trackerAdd urls)))

(defun transmission-trackers-remove ()
  "Remove trackers from torrent at point by ID or announce URL."
  (interactive)
  (let* ((id (or transmission-torrent-id (user-error "No torrent selected")))
         (array (or (transmission-tracker-stats id)
                    (user-error "No trackers to remove")))
         (prompt (format "Remove tracker (%d trackers): " (length array)))
         (trackers (cl-loop for x across array
                            collect (cons (cdr (assq 'announce x))
                                          (cdr (assq 'id x)))))
         (completion-extra-properties
          `(:annotation-function
            (lambda (x) (format " ID# %d" (cdr (assoc x ',trackers))))))
         (urls (or (transmission-read-strings
                    prompt trackers transmission-tracker-history-variable
                    #'transmission-tracker-url-p)
                   (user-error "No trackers selected for removal")))
         (tids (cl-loop for alist across array
                        if (or (member (cdr (assq 'announce alist)) urls)
                               (member (number-to-string (cdr (assq 'id alist))) urls))
                        collect (cdr (assq 'id alist)))))
    (transmission-request-async
     (lambda (_) (message "Removed %s" (mapconcat #'identity urls ", ")))
     "torrent-set" (list :ids id :trackerRemove tids))))

(defun transmission-trackers-replace ()
  "Replace tracker by ID or announce URL."
  (interactive)
  (let* ((id (or transmission-torrent-id (user-error "No torrent selected")))
         (trackers (or (cl-loop for x across (transmission-tracker-stats id)
                                collect (cons (cdr (assq 'announce x))
                                              (cdr (assq 'id x))))
                       (user-error "No trackers to replace")))
         (prompt (format "Replace tracker (%d trackers): " (length trackers)))
         (tid (or (let* ((completion-extra-properties
                          `(:annotation-function
                            (lambda (x)
                              (format " ID# %d" (cdr (assoc x ',trackers))))))
                         (tracker (completing-read prompt trackers)))
                    (cl-loop for cell in trackers
                             if (member tracker (list (car cell)
                                                      (number-to-string (cdr cell))))
                             return (cdr cell)))
                  (user-error "No tracker selected for substitution")))
         (replacement
          (completing-read "Replacement tracker? "
                           (append transmission-trackers
                                   (transmission-unique-announce-urls))
                           nil nil nil
                           transmission-tracker-history-variable)))
    (transmission-request-async
     (lambda (_) (message "Replaced #%d with %s" tid replacement))
     "torrent-set" (list :ids id :trackerReplace (vector tid replacement)))))

(defun transmission-turtle-set-days (days &optional disable)
  "Set DAYS on which turtle mode will be active.
DAYS is a bitfield, the associations of which are in `transmission-schedules'.
Empty input or non-positive DAYS makes no change to the schedule.
With a prefix argument, disable turtle mode schedule."
  (interactive
   (let ((arguments '(:fields ["alt-speed-time-day" "alt-speed-time-enabled"])))
     (let-alist (transmission-request "session-get" arguments)
       (let* ((alist transmission-schedules)
              (prompt
               (format "Days %s%s: "
                       (or (transmission-n->days .alt-speed-time-day) "(none)")
                       (if (eq t .alt-speed-time-enabled) "" " [disabled]")))
              (names (transmission-read-strings prompt alist))
              (bits 0))
         (dolist (name names)
           (cl-callf logior bits (cdr (assq (intern name) alist))))
         (list bits current-prefix-arg)))))
  (let ((arguments
         (append `(:alt-speed-time-enabled ,(if disable json-false t))
                 (when (> days 0) `(:alt-speed-time-day ,days)))))
    (transmission-request-async #'transmission-turtle-poll "session-set" arguments)))

(defun transmission-turtle-set-times (begin end)
  "Set BEGIN and END times for turtle mode.
See `transmission-read-time' for details on time input."
  (interactive
   (let ((arguments '(:fields ["alt-speed-time-begin" "alt-speed-time-end"])))
     (let-alist (transmission-request "session-get" arguments)
       (let* ((begs (transmission-format-minutes .alt-speed-time-begin))
              (ends (transmission-format-minutes .alt-speed-time-end))
              (start (or (transmission-read-time (format "Begin (%s): " begs))
                         .alt-speed-time-begin))
              (stop (or (transmission-read-time (format "End (%s): " ends))
                        .alt-speed-time-end)))
         (when (and (= start .alt-speed-time-begin) (= stop .alt-speed-time-end))
           (user-error "No change in schedule"))
         (if (y-or-n-p (format "Set active time from %s to %s? "
                               (transmission-format-minutes start)
                               (transmission-format-minutes stop)))
             (list start stop) '(nil nil))))))
  (when (or begin end)
    (let ((arguments
           (append (when begin (list :alt-speed-time-begin begin))
                   (when end (list :alt-speed-time-end end)))))
      (transmission-request-async #'transmission-turtle-poll "session-set" arguments))))

(defun transmission-turtle-set-speeds (up down)
  "Set UP and DOWN speed limits (kB/s) for turtle mode."
  (interactive
   (let-alist (transmission-request "session-get" '(:fields ["alt-speed-up" "alt-speed-down"]))
     (let ((p1 (format "Set turtle upload limit (%d kB/s): " .alt-speed-up))
           (p2 (format "Set turtle download limit (%d kB/s): " .alt-speed-down)))
       (list (read-number p1) (read-number p2)))))
  (let ((arguments
         (append (when down (list :alt-speed-down down))
                 (when up (list :alt-speed-up up)))))
    (transmission-request-async #'transmission-turtle-poll "session-set" arguments)))

(defun transmission-turtle-status ()
  "Message details about turtle mode configuration."
  (interactive)
  (transmission-request-async
   (lambda (response)
     (let-alist response
       (message
        "%sabled; %d kB/s down, %d kB/s up; schedule %sabled, %s-%s, %s"
        (if (eq .alt-speed-enabled t) "En" "Dis") .alt-speed-down .alt-speed-up
        (if (eq .alt-speed-time-enabled t) "en" "dis")
        (transmission-format-minutes .alt-speed-time-begin)
        (transmission-format-minutes .alt-speed-time-end)
        (let ((bits (transmission-n->days .alt-speed-time-day)))
          (if (null bits) "never" (mapconcat #'symbol-name bits " "))))))
   "session-get"
   '(:fields ["alt-speed-enabled" "alt-speed-down" "alt-speed-up" "alt-speed-time-day"
              "alt-speed-time-enabled" "alt-speed-time-begin" "alt-speed-time-end"])))

(defun transmission-verify (ids)
  "Verify torrent at point, in region, or marked."
  (transmission-interactive
   (if (y-or-n-p (concat "Verify torrent" (when (cdr ids) "s") "? "))
       (list ids) '(nil)))
  (when ids (transmission-request-async nil "torrent-verify" (list :ids ids))))

(defun transmission-queue-move-top (ids)
  "Move torrent(s)--at point, in region, or marked--to the top of the queue."
  (transmission-interactive
   (if (y-or-n-p (concat "Queue torrent" (when (cdr ids) "s") " first? "))
       (list ids) '(nil)))
  (when ids
    (transmission-request-async nil "queue-move-top" (list :ids ids))))

(defun transmission-queue-move-bottom (ids)
  "Move torrent(s)--at point, in region, or marked--to the bottom of the queue."
  (transmission-interactive
   (if (y-or-n-p (concat "Queue torrent" (when (cdr ids) "s") " last? "))
       (list ids) '(nil)))
  (when ids
    (transmission-request-async nil "queue-move-bottom" (list :ids ids))))

(defun transmission-queue-move-up (ids)
  "Move torrent(s)--at point, in region, or marked--up in the queue."
  (transmission-interactive
   (if (y-or-n-p (concat "Raise torrent" (when (cdr ids) "s") " in the queue? "))
       (list ids) '(nil)))
  (when ids
    (transmission-request-async nil "queue-move-up" (list :ids ids))))

(defun transmission-queue-move-down (ids)
  "Move torrent(s)--at point, in region, or marked--down in the queue."
  (transmission-interactive
   (if (y-or-n-p (concat "Lower torrent" (when (cdr ids) "s") " in the queue? "))
       (list ids) '(nil)))
  (when ids
    (transmission-request-async nil "queue-move-down" (list :ids ids))))

(defun transmission-quit ()
  "Quit and bury the buffer."
  (interactive)
  (if (let ((cur (current-buffer)))
        (cl-loop for list in (window-prev-buffers) never (eq cur (car list))))
      (quit-window)
    (if (one-window-p)
        (bury-buffer)
      (delete-window))))

(defun transmission-files-unwant ()
  "Mark file(s)--at point, in region, or marked--as unwanted."
  (interactive)
  (transmission-files-do :files-unwanted))

(defun transmission-files-want ()
  "Mark file(s)--at point, in region, or marked--as wanted."
  (interactive)
  (transmission-files-do :files-wanted))

(defun transmission-files-priority (priority)
  "Set bandwidth PRIORITY on file(s) at point, in region, or marked."
  (interactive
   (list (completing-read "Set priority: " transmission-priority-alist nil t)))
  (transmission-files-do (intern (concat ":priority-" priority))))

(defun transmission-files-command (command file)
  "Run a command COMMAND on the FILE at point."
  (interactive
   (let* ((fap (run-hook-with-args-until-success 'file-name-at-point-functions))
          (fn (replace-regexp-in-string "\\.part\\'" "" fap))
          (def (let ((lists (transmission-collect-hook
                             'transmission-files-command-functions (list fn))))
                 (delete-dups (apply #'append lists))))
          (prompt (and fap (concat "! on " (file-name-nondirectory fap)
                                   (when def (format " (default %s)" (car def)))
                                   ": ")))
          (input (read-shell-command prompt nil nil def t)))
     (if fap (list (if (string-empty-p input) (or (car def) "") input) fap)
       (user-error "File does not exist"))))
  (let ((args (nconc (split-string-and-unquote command)
                     (list (expand-file-name file)))))
    (apply #'start-process (car args) nil args)))

(defun transmission-copy-file (file newname &optional ok-if-already-exists)
  "Copy the file at point to another location.
FILE, NEWNAME, and OK-IF-ALREADY-EXISTS are the same as in `copy-file'."
  (interactive
   (let* ((f (transmission-files-file-at-point))
          (prompt (format "Copy %s to: " (file-name-nondirectory f)))
          (def (when (bound-and-true-p dired-dwim-target)
                 (buffer-local-value 'default-directory
                                     (window-buffer (next-window)))))
          (new (read-file-name prompt nil def)))
     (list f new 0)))
  (copy-file file newname ok-if-already-exists t t t)
  (message "Copied %s" (file-name-nondirectory file)))

(defun transmission-find-file ()
  "Visit the file at point with `find-file-read-only'."
  (interactive)
  (find-file-read-only (transmission-files-file-at-point)))

(defun transmission-find-file-other-window ()
  "Visit the file at point in another window."
  (interactive)
  (find-file-read-only-other-window (transmission-files-file-at-point)))

(defun transmission-display-file ()
  "Display the file at point in another window."
  (interactive)
  (let ((buf (find-file-noselect (transmission-files-file-at-point))))
    (with-current-buffer buf
      (read-only-mode 1))
    (display-buffer buf t)))

(defun transmission-view-file ()
  "Examine the file at point in view mode."
  (interactive)
  (view-file (transmission-files-file-at-point)))

(defun transmission-browse-url-of-file ()
  "Browse file at point in a WWW browser."
  (interactive)
  (browse-url-of-file (expand-file-name (transmission-files-file-at-point))))

(defun transmission-dired-file ()
  "Show file at point in DirEd."
  (interactive)
  (let* ((f (transmission-files-file-at-point))
         (dir (file-name-directory f)))
    (if (file-directory-p dir)
        (with-current-buffer (dired dir)
          (dired-goto-file (expand-file-name f)))
      (message "Directory '%s' does not exist."
               (file-name-base (substring dir 0 -1))))))

(defun transmission-copy-filename-as-kill (&optional arg)
  "Copy name of file at point into the kill ring.
With a prefix argument, use the absolute file name."
  (interactive "P")
  (let* ((fn (transmission-files-file-at-point))
         (str (if arg fn (file-name-nondirectory fn))))
    (if (eq last-command 'kill-region)
        (kill-append str nil)
      (kill-new str))
    (message "%S" str)))

(defun transmission-copy-magnet ()
  "Copy magnet link of current torrent."
  (interactive)
  (let ((magnet (cdr (assq 'magnetLink (elt transmission-torrent-vector 0)))))
    (when magnet
      (kill-new magnet)
      (message "Copied %s" magnet))))

(defun transmission-toggle-mark (arg)
  "Toggle mark of item(s) at point.
If the region is active, toggle the mark on all items in the region.
Otherwise, with a prefix arg, mark files on the next ARG lines."
  (interactive "p")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (not (eobp))
            (transmission-toggle-mark-at-point)
            (forward-line))))
    (while (and (> arg 0) (not (eobp)))
      (cl-decf arg)
      (transmission-toggle-mark-at-point)
      (forward-line 1))
    (while (and (< arg 0) (not (bobp)))
      (cl-incf arg)
      (forward-line -1)
      (transmission-toggle-mark-at-point))))

(defun transmission-unmark-all ()
  "Remove mark from all items."
  (interactive)
  (let ((inhibit-read-only t) (n 0))
    (when transmission-marked-ids
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (when (= (following-char) ?>)
              (save-excursion
                (forward-char)
                (insert-and-inherit ?\s))
              (delete-region (point) (1+ (point)))
              (cl-incf n))
            (forward-line))))
      (setq transmission-marked-ids nil)
      (set-buffer-modified-p nil)
      (message "%s removed" (transmission-plural n "mark")))))

(defun transmission-invert-marks ()
  "Toggle mark on all items."
  (interactive)
  (let ((inhibit-read-only t) ids tag key)
    (when (setq key (cl-ecase major-mode
                      (transmission-mode 'hashString)
                      (transmission-files-mode 'index)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (when (setq tag (car (memq (following-char) '(?> ?\s))))
              (save-excursion
                (forward-char)
                (insert-and-inherit (if (= tag ?>) ?\s ?>)))
              (delete-region (point) (1+ (point)))
              (when (= tag ?\s)
                (push (cdr (assq key (tabulated-list-get-id))) ids)))
            (forward-line))))
      (setq transmission-marked-ids ids)
      (set-buffer-modified-p nil))))


;; Turtle mode

(defvar transmission-turtle-poll-callback
  (let (timer enabled next lighter)
    (lambda (response)
      (let-alist response
        (setq enabled (eq t .alt-speed-enabled))
        (setq next (transmission-turtle-when .alt-speed-time-begin
                                             .alt-speed-time-end))
        (set-default 'transmission-turtle-mode enabled)
        (setq lighter
              (if enabled
                  (concat transmission-turtle-lighter
                          (format ":%d/%d" .alt-speed-down .alt-speed-up))
                nil))
        (transmission-register-turtle-mode lighter)
        (when timer (cancel-timer timer))
        (setq timer (run-at-time next nil #'transmission-turtle-poll)))))
  "Closure checking turtle mode status and marshaling a timer.")

(defun transmission-turtle-poll (&rest _args)
  "Initiate `transmission-turtle-poll-callback' timer function."
  (transmission-request-async
   transmission-turtle-poll-callback "session-get"
   '(:fields ["alt-speed-enabled" "alt-speed-down" "alt-speed-up"
              "alt-speed-time-begin" "alt-speed-time-end"])))

(defvar transmission-turtle-mode-lighter nil
  "Lighter for `transmission-turtle-mode'.")

(define-minor-mode transmission-turtle-mode
  "Toggle alternative speed limits (turtle mode).
Indicates on the mode-line the down/up speed limits in kB/s."
  :group 'transmission
  :global t
  :lighter transmission-turtle-mode-lighter
  (transmission-request-async
   #'transmission-turtle-poll
   "session-set" `(:alt-speed-enabled ,(or transmission-turtle-mode json-false))))

(defun transmission-register-turtle-mode (lighter)
  "Add LIGHTER to buffers with a transmission-* major mode."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p "transmission" (symbol-name major-mode))
        (setq-local transmission-turtle-mode-lighter lighter)))))


;; Formatting

(defun transmission-format-status (status up down)
  "Return a propertized string describing torrent status.
STATUS is the index of `transmission-status-names'.  UP and DOWN are
transmission rates."
  (let ((state (aref transmission-status-names status))
        (idle (propertize "idle" 'font-lock-face 'shadow))
        (uploading
         (propertize "uploading" 'font-lock-face 'font-lock-constant-face)))
    (pcase status
      (0 (propertize state 'font-lock-face 'warning))
      ((or 1 3 5) (propertize state 'font-lock-face '(bold shadow)))
      (2 (propertize state 'font-lock-face 'font-lock-function-name-face))
      (4 (if (> down 0) (propertize state 'font-lock-face 'highlight)
           (if (> up 0) uploading idle)))
      (6 (if (> up 0) (propertize state 'font-lock-face 'success) idle))
      (_ state))))

(defun transmission-format-pieces (pieces count)
  "Format into a string the bitfield PIECES holding COUNT boolean flags."
  (let* ((bytes (base64-decode-string pieces))
         (bits (mapconcat #'transmission-byte->string bytes "")))
    (cl-flet ((string-partition (s n)
                (let (res middle last)
                  (while (not (zerop (setq last (length s))))
                    (setq middle (min n last))
                    (push (substring s 0 middle) res)
                    (setq s (substring s middle last)))
                  (nreverse res))))
      (string-join (string-partition (substring bits 0 count) 72) "\n"))))

(defun transmission-format-pieces-brief (pieces count)
  "Format pieces into a one-line greyscale representation.
PIECES and COUNT are the same as in `transmission-format-pieces'."
  (let* ((bytes (base64-decode-string pieces))
         (slices (transmission-slice bytes 72))
         (ratios
          (cl-loop for bv in slices with div = nil
                   do (cl-decf count (setq div (min count (* 8 (length bv)))))
                   collect (/ (transmission-count-bits bv) (float div)))))
    (mapconcat (pcase (display-color-cells)
                 ((pred (< 256)) #'transmission-ratio->grey)
                 (256 #'transmission-ratio->256)
                 (_ #'transmission-ratio->glyph))
               ratios "")))

(defun transmission-format-pieces-internal (pieces count size)
  "Format piece data into a string.
PIECES and COUNT are the same as in `transmission-format-pieces'.
SIZE is the file size in bytes of a single piece."
  (let ((have (cl-loop for b across (base64-decode-string pieces)
                       sum (transmission-hamming-weight b))))
    (concat
     "Piece count: " (transmission-group-digits have)
     " / " (transmission-group-digits count)
     " (" (format "%.1f" (transmission-percent have count)) "%) * "
     (transmission-format-size size) " each"
     (when (and (functionp transmission-pieces-function)
                (/= have 0) (< have count))
       (let ((str (funcall transmission-pieces-function pieces count)))
         (concat "\nPieces:\n\n" str))))))

(defun transmission-format-ratio (ratio mode limit)
  "String showing a torrent's seed ratio limit.
MODE is which seed ratio to use; LIMIT is the torrent-level limit."
  (concat "Ratio: " (pcase ratio
                      (-2 (if (char-displayable-p #x221e) "\u221e" "Inf"))
                      (-1 "n/a")
                      (_ (format "%.3f" ratio)))
          " / " (pcase mode
                  (0 "session limit")
                  (1 (format "%.2f (torrent-specific limit)" limit))
                  (2 "unlimited"))))

(defun transmission-format-peers (peers origins connected sending receiving)
  "Format peer information into a string.
PEERS is an array of peer-specific data.
ORIGINS is an alist giving counts of peers from different swarms.
CONNECTED, SENDING, RECEIVING are numbers."
  (cl-macrolet ((accumulate (array key)
                  `(cl-loop for alist across ,array
                            count (eq t (cdr (assq ,key alist))))))
    (if (zerop connected) "Peers: none connected\n"
      (concat
       (format "Peers: %d connected, uploading to %d, downloading from %d"
               connected sending receiving)
       (format " (%d unchoked, %d interested)\n"
               (- connected (accumulate peers 'clientIsChoked))
               (accumulate peers 'peerIsInterested))
       (format
        "Peer origins: %s\n"
        (string-join
         (cl-loop with x = 0 for cell in origins for src across
                  ["cache" "DHT" "incoming" "LPD" "LTEP" "PEX" "tracker(s)"]
                  if (not (zerop (setq x (cdr cell))))
                  collect (format "%d from %s" x src))
         ", "))))))

(defun transmission-format-tracker (tracker)
  "Format alist TRACKER into a string of tracker info."
  (let-alist tracker
    (let* ((label (format "Tracker %d" .id))
           (col (length label))
           (fill (propertize (make-string col ?\s) 'display `(space :align-to ,col)))
           (result (unless (member .lastAnnounceResult '("Success" ""))
                     (concat "\n" fill ": "
                             (propertize .lastAnnounceResult 'font-lock-face 'warning)))))
      (format
       (concat label ": %s (Tier %d)\n"
               fill ": %s %s. Announcing %s\n"
               fill ": %s, %s, %s %s. Scraping %s"
               result)
       .announce .tier
       (transmission-plural .lastAnnouncePeerCount "peer")
       (transmission-when .lastAnnounceTime) (transmission-when .nextAnnounceTime)
       (transmission-plural .seederCount "seeder")
       (transmission-plural .leecherCount "leecher")
       (transmission-plural .downloadCount "download")
       (transmission-when .lastScrapeTime) (transmission-when .nextScrapeTime)))))

(defun transmission-format-trackers (trackers)
  "Format tracker information into a string.
TRACKERS should be the \"trackerStats\" array."
  (if (zerop (length trackers)) "Trackers: none\n"
    (concat (mapconcat #'transmission-format-tracker trackers "\n") "\n")))

(defun transmission-format-speed-limit (speed limit limited)
  "Format speed limit data into a string.
SPEED and LIMIT are rates in bytes per second.  LIMITED, if t,
indicates that the speed limit is enabled."
  (cond
   ((not (eq limited t)) (format "%d kB/s" (transmission-rate speed)))
   (t (format "%d / %d kB/s" (transmission-rate speed) limit))))

(defun transmission-format-limits (session rx tx rx-lim tx-lim rx-thr tx-thr)
  "Format download and upload rate and limits into a string."
  (concat (transmission-format-speed-limit rx rx-lim rx-thr) " down, "
          (transmission-format-speed-limit tx tx-lim tx-thr) " up"
          (when (eq session t) ", session limited")))


;; Drawing

(defun transmission-tabulated-list-format (&optional _arg _noconfirm)
  "Initialize tabulated-list header or update `tabulated-list-format'."
  (let ((idx (cl-loop for format across tabulated-list-format
                      if (plist-get (nthcdr 3 format) :transmission-size)
                      return format))
        (col (if (eq 'iec transmission-units) 9 7)))
    (if (= (cadr idx) col)
        (or header-line-format (tabulated-list-init-header))
      (setf (cadr idx) col)
      (tabulated-list-init-header))))

(defmacro transmission-do-entries (seq &rest body)
  "Map over SEQ to generate a new value of `tabulated-list-entries'.
Each form in BODY is a column descriptor."
  (declare (indent 1) (debug t))
  (let ((res (make-symbol "res")))
    `(let (,res)
       (mapc (lambda (x) (let-alist x (push (list x (vector ,@body)) ,res)))
             ,seq)
       (setq tabulated-list-entries (nreverse ,res)))))

(defun transmission-draw-torrents (_id)
  (let* ((arguments `(:fields ,transmission-draw-torrents-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (transmission-do-entries transmission-torrent-vector
    (transmission-eta .eta .percentDone)
    (transmission-size .sizeWhenDone)
    (format "%d%%" (* 100 (if (= 1 .metadataPercentComplete)
                              .percentDone .metadataPercentComplete)))
    (format "%d" (transmission-rate .rateDownload))
    (format "%d" (transmission-rate .rateUpload))
    (format "%.1f" (if (> .uploadRatio 0) .uploadRatio 0))
    (if (not (zerop .error)) (propertize "error" 'font-lock-face 'error)
      (transmission-format-status .status .rateUpload .rateDownload))
    (concat
     (propertize .name 'transmission-name t)
     (mapconcat (lambda (l)
                  (concat " " (propertize l 'font-lock-face 'font-lock-constant-face)))
                .labels "")))
  (tabulated-list-print))

(defun transmission-draw-files (id)
  (let* ((arguments `(:ids ,id :fields ,transmission-draw-files-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (let* ((files (transmission-files-index (elt transmission-torrent-vector 0)))
         (prefix (transmission-files-prefix files)))
    (transmission-do-entries files
      (format "%d%%" (transmission-percent .bytesCompleted .length))
      (symbol-name (car (rassq .priority transmission-priority-alist)))
      (if (zerop .wanted) "no" "yes")
      (transmission-size .length)
      (propertize (if prefix (string-remove-prefix prefix .name) .name)
                  'transmission-name t)))
  (tabulated-list-print))

(defmacro transmission-insert-each-when (&rest body)
  "Insert each non-nil form in BODY sequentially on its own line."
  (declare (indent 0) (debug t))
  (let ((tmp (make-symbol "tmp")))
    (cl-loop for form in body
             collect `(when (setq ,tmp ,form) (insert ,tmp "\n")) into res
             finally return `(let (,tmp) ,@res))))

(defun transmission-draw-info (id)
  (let* ((arguments `(:ids ,id :fields ,transmission-draw-info-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (erase-buffer)
  (let-alist (elt transmission-torrent-vector 0)
    (transmission-insert-each-when
      (format "ID: %d" .id)
      (concat "Name: " .name)
      (concat "Hash: " id)
      (concat "Magnet: " (propertize .magnetLink 'font-lock-face 'link))
      (if (zerop (length .labels)) ""
        (concat "Labels: " (mapconcat #'identity .labels ", ") "\n"))
      (concat "Location: " (abbreviate-file-name .downloadDir))
      (let* ((percent (* 100 .percentDone))
             (fmt (if (zerop (mod percent 1)) "%d" "%.2f")))
        (concat "Percent done: " (format fmt percent) "%"))
      (format "Bandwidth priority: %s"
              (car (rassq .bandwidthPriority transmission-priority-alist)))
      (format "Queue position: %d" .queuePosition)
      (concat "Speed: "
              (transmission-format-limits
               .honorsSessionLimits .rateDownload .rateUpload
               .downloadLimit .uploadLimit .downloadLimited .uploadLimited))
      (transmission-format-ratio .uploadRatio .seedRatioMode .seedRatioLimit)
      (pcase .error
        ((or 2 3) (concat "Error: " (propertize .errorString 'font-lock-face 'error)))
        (1 (concat "Warning: " (propertize .errorString 'font-lock-face 'warning))))
      (transmission-format-peers .peers .peersFrom .peersConnected
                                 .peersGettingFromUs .peersSendingToUs)
      (concat "Date created:    " (transmission-time .dateCreated))
      (concat "Date added:      " (transmission-time .addedDate))
      (concat "Date finished:   " (transmission-time .doneDate))
      (concat "Latest Activity: " (transmission-time .activityDate) "\n")
      (transmission-format-trackers .trackerStats)
      (concat "Wanted: " (transmission-format-size .sizeWhenDone))
      (concat "Downloaded: " (transmission-format-size .downloadedEver))
      (concat "Verified: " (transmission-format-size .haveValid))
      (unless (zerop .corruptEver)
        (concat "Corrupt: " (transmission-format-size .corruptEver)))
      (concat "Total size: " (transmission-format-size .totalSize))
      (transmission-format-pieces-internal .pieces .pieceCount .pieceSize))))

(defun transmission-draw-peers (id)
  (let* ((arguments `(:ids ,id :fields ["peers"]))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (transmission-do-entries (cdr (assq 'peers (elt transmission-torrent-vector 0)))
    .address
    .flagStr
    (format "%d%%" (transmission-percent .progress 1.0))
    (format "%d" (transmission-rate .rateToClient))
    (format "%d" (transmission-rate .rateToPeer))
    .clientName
    (or (transmission-geoip-retrieve .address) ""))
  (tabulated-list-print))

(defmacro define-transmission-refresher (name)
  "Define a function `transmission-refresh-NAME' that refreshes a context buffer.
The defined function takes no arguments and expects
`transmission-draw-NAME' to exist.
Window position, point, and mark are restored, and the timer
object `transmission-timer' is run."
  (declare (indent 1) (debug (symbolp)))
  (let ((thing (symbol-name name)))
    `(defun ,(intern (concat "transmission-refresh-" thing)) (_arg _noconfirm)
       (transmission-with-saved-state
         (run-hooks 'before-revert-hook)
         (with-silent-modifications
           (,(intern (concat "transmission-draw-" thing)) transmission-torrent-id))
         (run-hooks 'after-revert-hook))
       (transmission-timer-check))))

(define-transmission-refresher torrents)
(define-transmission-refresher files)
(define-transmission-refresher info)
(define-transmission-refresher peers)

(defmacro transmission-context (mode)
  "Switch to a context buffer of major mode MODE."
  (declare (debug (symbolp)))
  (cl-assert (string-suffix-p "-mode" (symbol-name mode)))
  (let ((name (make-symbol "name")))
    `(let ((id (or transmission-torrent-id
                   (cdr (assq 'hashString (tabulated-list-get-id)))))
           (,name ,(format "*%s*" (string-remove-suffix "-mode" (symbol-name mode)))))
       (if (not id) (user-error "No torrent selected")
         (let ((buffer (or (get-buffer ,name)
                           (generate-new-buffer ,name))))
           (transmission-turtle-poll)
           (with-current-buffer buffer
             (let ((old-id (or transmission-torrent-id
                               (cdr (assq 'hashString (tabulated-list-get-id))))))
               (unless (eq major-mode ',mode)
                 (funcall #',mode))
               (if (and old-id (eq old-id id))
                   (revert-buffer)
                 (setq transmission-torrent-id id)
                 (setq transmission-marked-ids nil)
                 (revert-buffer)
                 (goto-char (point-min)))))
           (pop-to-buffer-same-window buffer))))))

(defun transmission-print-torrent (id cols)
  "Insert a torrent entry at point using `tabulated-list-print-entry'.
Put the mark tag in the padding area of the current line if the current
torrent is marked.
ID is a Lisp object identifying the entry to print, and COLS is a vector
of column descriptors."
  (tabulated-list-print-entry id cols)
  (let* ((key (cl-ecase major-mode
                (transmission-mode 'hashString)
                (transmission-files-mode 'index)))
         (item-id (cdr (assq key id))))
    (when (member item-id transmission-marked-ids)
      (save-excursion
        (forward-line -1)
        (tabulated-list-put-tag ">")))))


;; Major mode definitions

(defmacro define-transmission-predicate (name test &rest body)
  "Define transmission-NAME as a function.
The function is to be used as a `sort' predicate for `tabulated-list-format'.
The definition is (lambda (a b) (TEST ...)) where the body
is constructed from TEST, BODY and the `tabulated-list-id' tagged as `<>'."
  (declare (indent 2) (debug (symbolp function-form body)))
  (let ((a (make-symbol "a"))
        (b (make-symbol "b")))
    (cl-labels
        ((cut (form x)
           (cond
            ((eq form '<>) (list 'car x))
            ((atom form) form)
            ((or (listp form) (null form))
             (mapcar (lambda (subexp) (cut subexp x)) form)))))
      `(defun ,(intern (concat "transmission-" (symbol-name name))) (,a ,b)
         (,test ,(cut (macroexp-progn body) a)
                ,(cut (macroexp-progn body) b))))))

(define-transmission-predicate download>? > (cdr (assq 'rateToClient <>)))
(define-transmission-predicate upload>? > (cdr (assq 'rateToPeer <>)))
(define-transmission-predicate size>? > (cdr (assq 'length <>)))
(define-transmission-predicate size-when-done>? > (cdr (assq 'sizeWhenDone <>)))
(define-transmission-predicate percent-done>? > (cdr (assq 'percentDone <>)))
(define-transmission-predicate ratio>? > (cdr (assq 'uploadRatio <>)))
(define-transmission-predicate progress>? > (cdr (assq 'progress <>)))
(define-transmission-predicate file-want? > (cdr (assq 'wanted <>)))

(define-transmission-predicate eta>=? >=
  (let-alist <>
    (if (>= .eta 0) .eta
      (- 1.0 .percentDone))))

(define-transmission-predicate file-have>? >
  (let-alist <>
    (/ (* 1.0 .bytesCompleted) .length)))

(defvar transmission-peers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'transmission-info)
    map)
  "Keymap used in `transmission-peers-mode' buffers.")

(easy-menu-define transmission-peers-mode-menu transmission-peers-mode-map
  "Menu used in `transmission-peers-mode' buffers."
  '("Transmission-Peers"
    ["View Torrent Files" transmission-files]
    ["View Torrent Info" transmission-info]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-peers-mode tabulated-list-mode "Transmission-Peers"
  "Major mode for viewing peer information.
See the \"--peer-info\" option in transmission-remote(1) or
https://github.com/transmission/transmission/blob/main/docs/Peer-Status-Text.md
for explanation of the peer flags."
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        [("Address" 15 nil)
         ("Flags" 6 t)
         ("Has" 4 transmission-progress>? :right-align t)
         ("Down" 4 transmission-download>? :right-align t)
         ("Up" 3 transmission-upload>? :right-align t :pad-right 2)
         ("Client" 20 t)
         ("Location" 0 t)])
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (setq-local revert-buffer-function #'transmission-refresh-peers))

(defun transmission-peers ()
  "Open a `transmission-peers-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-peers-mode))

(defvar transmission-info-font-lock-keywords
  (eval-when-compile
    `((,(rx bol (group (*? nonl) ":") (* blank) (group (* nonl)) eol)
       (1 'font-lock-type-face)
       (2 'font-lock-keyword-face))))
  "Default expressions to highlight in `transmission-info-mode' buffers.")

(defvar transmission-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'transmission-files)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "a" 'transmission-trackers-add)
    (define-key map "c" 'transmission-copy-magnet)
    (define-key map "d" 'transmission-set-torrent-download)
    (define-key map "e" 'transmission-peers)
    (define-key map "L" 'transmission-label)
    (define-key map "l" 'transmission-set-torrent-ratio)
    (define-key map "r" 'transmission-trackers-remove)
    (define-key map "u" 'transmission-set-torrent-upload)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-info-mode' buffers.")

(easy-menu-define transmission-info-mode-menu transmission-info-mode-map
  "Menu used in `transmission-info-mode' buffers."
  '("Transmission-Info"
    ["Add Tracker URLs" transmission-trackers-add]
    ["Remove Trackers" transmission-trackers-remove]
    ["Replace Tracker" transmission-trackers-replace]
    ["Copy Magnet Link" transmission-copy-magnet]
    ["Move Torrent" transmission-move]
    ["Reannounce Torrent" transmission-reannounce]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ("Set Torrent Limits"
     ["Honor Session Speed Limits" transmission-toggle-limits
      :help "Toggle whether torrent honors session limits."
      :style toggle :selected (transmission-torrent-honors-speed-limits-p)]
     ["Set Torrent Download Limit" transmission-set-torrent-download]
     ["Set Torrent Upload Limit" transmission-set-torrent-upload]
     ["Set Torrent Seed Ratio Limit" transmission-set-torrent-ratio])
    ("Set Torrent Queue Position"
     ["Move To Top" transmission-queue-move-top]
     ["Move To Bottom" transmission-queue-move-bottom]
     ["Move Up" transmission-queue-move-up]
     ["Move Down" transmission-queue-move-down])
    ["Set Torrent Labels" transmission-label]
    ["Verify Torrent" transmission-verify]
    "--"
    ["View Torrent Files" transmission-files]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-info-mode special-mode "Transmission-Info"
  "Major mode for viewing and manipulating torrent attributes."
  :group 'transmission
  (setq buffer-undo-list t)
  (setq font-lock-defaults '(transmission-info-font-lock-keywords t))
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (setq-local revert-buffer-function #'transmission-refresh-info))

(defun transmission-info ()
  "Open a `transmission-info-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-info-mode))

(defvar transmission-files-font-lock-keywords
  '(("^[>]" (".+" (transmission-move-to-file-name) nil (0 'warning))))
  "Default expressions to highlight in `transmission-files-mode'.")

(defvar transmission-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'transmission-find-file)
    (define-key map "o" 'transmission-find-file-other-window)
    (define-key map (kbd "C-o") 'transmission-display-file)
    (define-key map "^" 'quit-window)
    (define-key map "!" 'transmission-files-command)
    (define-key map "&" 'transmission-files-command)
    (define-key map "X" 'transmission-files-command)
    (define-key map "W" 'transmission-browse-url-of-file)
    (define-key map "C" 'transmission-copy-file)
    (define-key map "d" 'transmission-dired-file)
    (define-key map "e" 'transmission-peers)
    (define-key map "i" 'transmission-info)
    (define-key map "m" 'transmission-toggle-mark)
    (define-key map "t" 'transmission-invert-marks)
    (define-key map "u" 'transmission-files-unwant)
    (define-key map "U" 'transmission-unmark-all)
    (define-key map "v" 'transmission-view-file)
    (define-key map "w" 'transmission-files-want)
    (define-key map "y" 'transmission-files-priority)
    map)
  "Keymap used in `transmission-files-mode' buffers.")

(easy-menu-define transmission-files-mode-menu transmission-files-mode-map
  "Menu used in `transmission-files-mode' buffers."
  '("Transmission-Files"
    ["Run Command On File" transmission-files-command]
    ["Visit File" transmission-find-file
     "Switch to a read-only buffer visiting file at point"]
    ["Visit File In Other Window" transmission-find-file-other-window]
    ["Display File" transmission-display-file
     "Display a read-only buffer visiting file at point"]
    ["Visit File In View Mode" transmission-view-file]
    ["Open File In WWW Browser" transmission-browse-url-of-file]
    ["Show File In DirEd" transmission-dired-file]
    ["Copy File Name" transmission-copy-filename-as-kill]
    "--"
    ["Unwant Files" transmission-files-unwant
     :help "Tell Transmission not to download files at point or in region"]
    ["Want Files" transmission-files-want
     :help "Tell Transmission to download files at point or in region"]
    ["Set Files' Bandwidth Priority" transmission-files-priority]
    "--"
    ["Toggle Mark" transmission-toggle-mark]
    ["Unmark All" transmission-unmark-all]
    ["Invert Marks" transmission-invert-marks]
    "--"
    ["View Torrent Info" transmission-info]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-files-mode tabulated-list-mode "Transmission-Files"
  "Major mode for a torrent's file list."
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        [("Have" 4 transmission-file-have>? :right-align t)
         ("Priority" 8 t)
         ("Want" 4 transmission-file-want? :right-align t)
         ("Size" 9 transmission-size>? :right-align t :transmission-size t)
         ("Name" 0 t)])
  (setq tabulated-list-padding 1)
  (transmission-tabulated-list-format)
  (setq-local file-name-at-point-functions '(transmission-files-file-at-point))
  (setq tabulated-list-printer #'transmission-print-torrent)
  (setq-local revert-buffer-function #'transmission-refresh-files)
  (setq-local font-lock-defaults '(transmission-files-font-lock-keywords t))
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (add-hook 'before-revert-hook #'transmission-tabulated-list-format nil t))

(defun transmission-files ()
  "Open a `transmission-files-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-files-mode))

(defvar transmission-font-lock-keywords
  '(("^[>]" (transmission-file-name-matcher nil nil (0 'warning))))
  "Default expressions to highlight in `transmission-mode'.")

(defvar transmission-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry #x221e "w" table)
    table)
  "Syntax table used in `transmission-mode' buffers.")

(defvar transmission-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'transmission-files)
    (define-key map (kbd "RET") 'transmission-files)
    (define-key map "a" 'transmission-add)
    (define-key map "d" 'transmission-set-download)
    (define-key map "e" 'transmission-peers)
    (define-key map "i" 'transmission-info)
    (define-key map "k" 'transmission-trackers-add)
    (define-key map "L" 'transmission-label)
    (define-key map "l" 'transmission-set-ratio)
    (define-key map "m" 'transmission-toggle-mark)
    (define-key map "r" 'transmission-remove)
    (define-key map "D" 'transmission-delete)
    (define-key map "s" 'transmission-toggle)
    (define-key map "t" 'transmission-invert-marks)
    (define-key map "u" 'transmission-set-upload)
    (define-key map "v" 'transmission-verify)
    (define-key map "q" 'transmission-quit)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    (define-key map "U" 'transmission-unmark-all)
    map)
  "Keymap used in `transmission-mode' buffers.")

(easy-menu-define transmission-mode-menu transmission-mode-map
  "Menu used in `transmission-mode' buffers."
  '("Transmission"
    ["Add Torrent" transmission-add]
    ["Start/Stop Torrent" transmission-toggle
     :help "Toggle pause on torrents at point or in region"]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ["Add Tracker URLs" transmission-trackers-add]
    ("Set Global/Session Limits"
     ["Set Global Download Limit" transmission-set-download]
     ["Set Global Upload Limit" transmission-set-upload]
     ["Set Global Seed Ratio Limit" transmission-set-ratio])
    ("Set Torrent Limits"
     ["Toggle Torrent Speed Limits" transmission-toggle-limits
      :help "Toggle whether torrent honors session limits."]
     ["Set Torrent Download Limit" transmission-set-torrent-download]
     ["Set Torrent Upload Limit" transmission-set-torrent-upload]
     ["Set Torrent Seed Ratio Limit" transmission-set-torrent-ratio])
    ["Move Torrent" transmission-move]
    ["Remove Torrent" transmission-remove]
    ["Delete Torrent" transmission-delete
     :help "Delete torrent contents from disk."]
    ["Reannounce Torrent" transmission-reannounce]
    ("Set Torrent Queue Position"
     ["Move To Top" transmission-queue-move-top]
     ["Move To Bottom" transmission-queue-move-bottom]
     ["Move Up" transmission-queue-move-up]
     ["Move Down" transmission-queue-move-down])
    ["Set Torrent Labels" transmission-label]
    ["Verify Torrent" transmission-verify]
    "--"
    ["Toggle Mark" transmission-toggle-mark]
    ["Unmark All" transmission-unmark-all]
    ["Invert Marks" transmission-invert-marks
     :help "Toggle mark on all items"]
    "--"
    ["Query Free Space" transmission-free]
    ["Session Statistics" transmission-stats]
    ("Turtle Mode" :help "Set and schedule alternative speed limits"
     ["Turtle Mode Status" transmission-turtle-status]
     ["Toggle Turtle Mode" transmission-turtle-mode]
     ["Set Active Days" transmission-turtle-set-days]
     ["Set Active Time Span" transmission-turtle-set-times]
     ["Set Turtle Speed Limits" transmission-turtle-set-speeds])
    "--"
    ["View Torrent Files" transmission-files]
    ["View Torrent Info" transmission-info]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" transmission-quit]))

(define-derived-mode transmission-mode tabulated-list-mode "Transmission"
  "Major mode for the list of torrents in a Transmission session.
See https://github.com/transmission/transmission for more information about
Transmission."
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        [("ETA" 4 transmission-eta>=? :right-align t)
         ("Size" 9 transmission-size-when-done>?
          :right-align t :transmission-size t)
         ("Have" 4 transmission-percent-done>? :right-align t)
         ("Down" 4 nil :right-align t)
         ("Up" 3 nil :right-align t)
         ("Ratio" 5 transmission-ratio>? :right-align t)
         ("Status" 11 t)
         ("Name" 0 t)])
  (setq tabulated-list-padding 1)
  (transmission-tabulated-list-format)
  (setq tabulated-list-printer #'transmission-print-torrent)
  (setq-local revert-buffer-function #'transmission-refresh-torrents)
  (setq-local font-lock-defaults '(transmission-font-lock-keywords t))
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (add-hook 'before-revert-hook #'transmission-tabulated-list-format nil t))

;;;###autoload
(defun transmission ()
  "Open a `transmission-mode' buffer."
  (interactive)
  (let* ((name "*transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (transmission-turtle-poll)
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (unless (eq major-mode 'transmission-mode)
          (condition-case e
              (progn
                (transmission-mode)
                (revert-buffer)
                (goto-char (point-min)))
            (error
             (kill-buffer buffer)
             (signal (car e) (cdr e))))))
      (switch-to-buffer-other-window buffer))))

(provide 'transmission)

;;; transmission.el ends here
