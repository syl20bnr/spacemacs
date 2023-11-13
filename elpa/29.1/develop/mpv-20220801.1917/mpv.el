;;; mpv.el --- control mpv for easy note-taking  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Johann Klähn

;; Author: Johann Klähn <johann@jklaehn.de>
;; URL: https://github.com/kljohann/mpv.el
;; Version: 0.2.0
;; Keywords: tools, multimedia
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a potpourri of helper functions to control a mpv
;; process via its IPC interface.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-timer)
(require 'tq)
(require 'url-parse)

(defgroup mpv nil
  "Customization group for mpv."
  :prefix "mpv-"
  :group 'external)

(defcustom mpv-executable "mpv"
  "Name or path to the mpv executable."
  :type 'file
  :group 'mpv)

(defcustom mpv-default-options nil
  "List of default options to be passed to mpv."
  :type '(repeat string)
  :group 'mpv)

(defcustom mpv-start-timeout 0.5
  "Maximum time in seconds that `mpv-start' blocks while waiting for mpv."
  :type 'number
  :group 'mpv)

(defcustom mpv-speed-step 1.10
  "Scale factor used when adjusting playback speed."
  :type 'number
  :group 'mpv)

(defcustom mpv-volume-step 1.50
  "Scale factor used when adjusting volume."
  :type 'number
  :group 'mpv)

(defcustom mpv-seek-step 5
  "Step size in seconds used when seeking."
  :type 'number
  :group 'mpv)

(defcustom mpv-entry-with-offset-format "%t [%o]"
  "The format of the entries for mpv listing operations.

The following %-escapes will be expanded using `format-spec':

%t      The entry's title.
%o      The entry's time offset in `[HH:]MM:SS' format."
  :type 'string
  :group 'mpv)

(defcustom mpv-current-indicator " *"
  "The indicator to use for the currently-playing entry."
  :type 'string
  :group 'mpv)

(defcustom mpv-loop-indicator " R"
  "The indicator to use for a looped entry."
  :type 'string
  :group 'mpv)

(defcustom mpv-on-event-hook nil
  "Hook to run when an event message is received.
The hook will be called with the parsed JSON message as its only an
argument.  See \"List of events\" in the mpv man page."
  :type 'hook
  :group 'mpv)

(defcustom mpv-on-start-hook nil
  "Hook to run when a new mpv process is started.
The hook will be called with the arguments passed to `mpv-start'."
  :type 'hook
  :group 'mpv)

(defcustom mpv-on-exit-hook nil
  "Hook to run when the mpv process dies."
  :type 'hook
  :group 'mpv)

(defvar mpv--process nil)
(defvar mpv--queue nil)

(defun mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and mpv--process (eq (process-status mpv--process) 'run)))

(defun mpv-start (&rest args)
  "Start an mpv process with the specified ARGS.

If there already is an mpv process controlled by this Emacs instance,
it will be killed.  Options specified in `mpv-default-options' will be
prepended to ARGS."
  (mpv-kill)
  (let ((socket (make-temp-name
                 (expand-file-name "mpv-" temporary-file-directory))))
    (setq mpv--process
          (apply #'start-process "mpv-player" nil mpv-executable
                 "--no-terminal"
                 (concat "--input-ipc-server=" socket)
                 (append mpv-default-options args)))
    (set-process-query-on-exit-flag mpv--process nil)
    (set-process-sentinel
     mpv--process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (mpv-kill)
         (when (file-exists-p socket)
           (with-demoted-errors (delete-file socket)))
         (run-hooks 'mpv-on-exit-hook))))
    (with-timeout
        (mpv-start-timeout (mpv-kill)
                           (error "Failed to connect to mpv"))
      (while (not (file-exists-p socket))
        (sleep-for 0.05)))
    (setq mpv--queue (tq-create
                      (make-network-process :name "mpv-socket"
                                            :family 'local
                                            :service socket)))
    (set-process-filter
     (tq-process mpv--queue)
     (lambda (_proc string)
       (mpv--tq-filter mpv--queue string)))
    (run-hook-with-args 'mpv-on-start-hook args)
    t))

(defun mpv--as-strings (command)
  "Convert COMMAND to a list of strings."
  (mapcar (lambda (arg)
            (if (numberp arg)
                (number-to-string arg)
              arg))
          command))

(defun mpv--enqueue (command fn &optional delay-command)
  "Add COMMAND to the transaction queue.

FN will be called with the corresponding answer.
If DELAY-COMMAND is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes.

Note that we do not use the regexp and closure arguments of
`tq-enqueue', see our custom implementation of `tq-process-buffer'
below."
  (when (mpv-live-p)
    (tq-enqueue
     mpv--queue
     (concat (json-encode `((command . ,(mpv--as-strings command)))) "\n")
     "" nil fn delay-command)
    t))

(defun mpv-run-command (command &rest arguments)
  "Send a COMMAND to mpv, passing the remaining ARGUMENTS.
Block while waiting for the response."
  (when (mpv-live-p)
    (let* ((response
            (cl-block mpv-run-command-wait-for-response
              (mpv--enqueue
               (cons command arguments)
               (lambda (response)
                 (cl-return-from mpv-run-command-wait-for-response
                   response)))
              (while (mpv-live-p)
                (sleep-for 0.05))))
           (status (alist-get 'error response))
           (data (alist-get 'data response)))
      (unless (string-equal status "success")
        (error "`%s' failed: %s" command status))
      data)))

(defun mpv--tq-filter (tq string)
  "Append to the queue's buffer and process the new data.

TQ is a transaction queue created by `tq-create'.
STRING is the data fragment received from the process.

This is a verbatim copy of `tq-filter' that uses
`mpv--tq-process-buffer' instead of `tq-process-buffer'."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (mpv--tq-process-buffer tq)))))

(defun mpv--tq-process-buffer (tq)
  "Check TQ's buffer for a JSON response.

Replacement for `tq-process-buffer' that ignores regular expressions
\(answers are always passed to the first handler in the queue) and
passes unsolicited event messages to `mpv-on-event-hook'."
  (goto-char (point-min))
  (skip-chars-forward "^{")
  (let ((answer (ignore-errors (json-read))))
    (when answer
      (delete-region (point-min) (point))
      ;; event messages have form {"event": ...}
      ;; answers have form {"error": ..., "data": ...}
      (cond
       ((assoc 'event answer)
        (run-hook-with-args 'mpv-on-event-hook answer))
       ((not (tq-queue-empty tq))
        (unwind-protect
            (funcall (tq-queue-head-fn tq) answer)
          (tq-queue-pop tq))))
      ;; Recurse to check for further JSON messages.
      (mpv--tq-process-buffer tq))))

(defmacro mpv--with-json (&rest body)
  "Decode JSON result appropriately from BODY."
  `(let* ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false 'false))
     ,@body))

(defun mpv-toggle-loop (&optional playlist)
  "Cycle between infinite and no looping for the current mpv file.

With prefix argument PLAYLIST, cycle looping of the current playlist instead."
  (interactive "P")
  (let ((prop (if playlist
                  "loop-playlist"
                "loop-file")))
    (mpv-run-command "cycle-values" prop "inf" "no")
    (message "Loop [%s]: %s"
             (cadr (split-string prop "-"))
             (mpv--with-json
              (mpv-get-property prop)))))

(defun mpv-toggle-video ()
  "Cycle video playback state for the current mpv file."
  (interactive)
  (mpv-cycle-property "video"))

(defun mpv--completing-read-playlist-entry-index ()
  "Read a playlist entry with completion and return its index in the playlist."
  (let* ((choices (seq-map-indexed 'cons (mpv--get-formatted-playlist)))
         (choice
          (completing-read "Playlist entries: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   (category . mpv-file)
                                   (display-sort-function . ,#'identity))
                               (complete-with-action action choices string pred)))
                           nil 'require-match)))
    (cdr (assoc choice choices))))

(defun mpv--completing-read-chapter-index ()
  "Read a chapter with completion and return its index."
  (let* ((choices (seq-map-indexed 'cons (mpv--get-formatted-chapters)))
         (choice
          (completing-read "Chapters: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   (category . mpv-chapter)
                                   (display-sort-function . ,#'identity))
                               (complete-with-action action choices string pred)))
                           nil 'require-match)))
    (cdr (assoc choice choices))))

(defun mpv-jump-to-chapter (chapter)
  "Jump to chapter CHAPTER.

When called interactively, the chapter is read from the
minibuffer with completion."
  (interactive (list (mpv--completing-read-chapter-index)))
  (mpv-set-property "chapter" chapter))

(defun mpv-jump-to-playlist-entry (index)
  "Jump to entry INDEX of the mpv playlist.

When called interactively, the playlist entry is read from the
minibuffer with completion."
  (interactive (list (mpv--completing-read-playlist-entry-index)))
  (mpv-run-command "playlist-play-index" index))

(defun mpv-remove-playlist-entry (index)
  "Remove entry INDEX from the mpv playlist.

When called interactively, the playlist entry is read from the
minibuffer with completion."
  (interactive (list (mpv--completing-read-playlist-entry-index)))
  (mpv-run-command "playlist-remove" index))

(defun mpv-set-chapter-ab-loop (chapter)
  "Toggle an A-B loop for the timestamps between where CHAPTER is bound.

When called interactively, the chapter is read from the
minibuffer with completion."
  (interactive (list (mpv--completing-read-chapter-index)))
  (let* ((current-chapter (nth chapter
                               (mpv--with-json
                                (mpv-get-property "chapter-list"))))
         (current-timestamp (alist-get 'time current-chapter))
         (title (mpv-get-property (format "chapter-list/%s/title" chapter))))
    (if (eql (mpv-get-property "ab-loop-a") current-timestamp)
        (progn
          (mpv-set-property "ab-loop-a" "no")
          (mpv-set-property "ab-loop-b" "no")
          (message "Removed A-B loop from chapter `%s'" title))
      (progn
        (mpv-set-property "ab-loop-a" current-timestamp)
        (if (eql (mpv-get-property "chapters") (1+ chapter))
            (mpv-set-property "ab-loop-b" (mpv-get-property "duration"))
          (thread-last
            (1+ chapter)
            (format "chapter-list/%s/time")
            (mpv-get-property)
            (mpv-set-property "ab-loop-b")))
        (message "Chapter `%s' set to A-B loop" title)))))

(defun mpv-set-ab-loop ()
  "Set an A-B loop point to the current playback position.

The first invocation sets the A point of the loop to the current
playback position, the second sets the B point to the current
playback position.  A third invocation can be used to remove the
A-B loop."
  (interactive)
  (mpv-run-command "ab-loop")
  (cl-flet ((ab-loop-p
             (point)
             (or (numberp (mpv-get-property point))
                 (not (string= (mpv-get-property point) "no")))))
    (cond
     ((and (not (ab-loop-p "ab-loop-a"))
           (not (ab-loop-p "ab-loop-b")))
      (message "Removed A-B loop"))
     ((and (ab-loop-p "ab-loop-a")
           (ab-loop-p "ab-loop-b"))
      (message "Set point B for A-B loop"))
     ((ab-loop-p "ab-loop-a")
      (message "Set point A for A-B loop")))))

(defun mpv-chapter-next ()
  "Jump to the next chapter in the current playback."
  (interactive)
  (if (mpv--with-json
       (mpv-get-property "chapter-list"))
      (progn
        (mpv-run-command "add" "chapter" "1")
        (run-at-time 1 nil (lambda ()
                             (thread-last
                               (mpv-get-property "chapter")
                               (format "chapter-list/%d/title")
                               (mpv-get-property)
                               (message "%s")))))
    (user-error "No chapters available")))

(defun mpv-chapter-prev ()
  "Jump to the previous chapter in the current playback."
  (interactive)
  (if (mpv--with-json
       (mpv-get-property "chapter-list"))
      (progn
        (mpv-run-command "add" "chapter" "-1")
        (run-at-time 1 nil (lambda ()
                             (thread-last
                               (mpv-get-property "chapter")
                               (format "chapter-list/%d/title")
                               (mpv-get-property)
                               (message "%s")))))
    (user-error "No chapters available")))

(cl-defun mpv--format-entry (title &optional offset &key (current nil) (looping nil))
  "Format TITLE for minibuffer display, optionally showing a time OFFSET value.

When an offset is provided, `mpv-entry-with-offset-format' is
used to format the result.

Markers are appended to the formatted title, in order to indicate
the CURRENT item, and if an entry is LOOPING.
See `mpv-current-indicator' and `mpv-loop-indicator' respectively."
  (concat
   (if (numberp offset)
       (format-spec
        mpv-entry-with-offset-format
        `((?t . ,title)
          (?o . ,(format-time-string
                  (if (< 3600 offset) "%T" "%M:%S")
                  offset t))))
     title)
   (and current mpv-current-indicator)
   (and looping mpv-loop-indicator)))

(defun mpv--get-formatted-chapters ()
  "Return a formatted list of the available chapters in the current mpv playback."
  (if-let* ((chapters (mpv--with-json
                       (mpv-get-property "chapter-list")))
            (formatted-chapters
             (cl-loop with counter = 0
                      for chapter in chapters
                      collect (let ((time (alist-get 'time chapter))
                                    (title (alist-get 'title chapter)))
                                (cond
                                 ((and (= counter (mpv-get-property "chapter"))
                                       (eql (mpv-get-property "ab-loop-a") time))
                                  (mpv--format-entry title time :current t :looping t))
                                 ((= counter (mpv-get-property "chapter"))
                                  (mpv--format-entry title time :current t))
                                 ((eql (mpv-get-property "ab-loop-a") time)
                                  (mpv--format-entry title time :looping t))
                                 (t
                                  (mpv--format-entry title time))))
                      do (cl-incf counter))))
      formatted-chapters
    (user-error "No chapters available")))

(defun mpv--get-formatted-playlist ()
  "Return a formatted list of the current playlist entries."
  (if-let* ((entries (mpv--with-json
                      (mpv-get-property "playlist")))
            (formatted-entries
             (cl-loop for entry in entries
                      collect (let* ((title (or (alist-get 'title entry)
                                                (alist-get 'filename entry))))
                                (if (alist-get 'current entry)
                                    (mpv--format-entry title nil :current t)
                                  (mpv--format-entry title nil))))))
      formatted-entries
    (user-error "No entries in playlist")))

(defun mpv--url-p (url)
  "Return if URL is an HTTP(S) URL."
  (member (url-type (url-generic-parse-url url)) '("http" "https")))

;;;###autoload
(defun mpv-play (path)
  "Start an mpv process playing the file at PATH.

You can use this with `org-add-link-type' or `org-file-apps'.
See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options."
  (interactive "fFile: ")
  (mpv-start (expand-file-name path)))

;;;###autoload
(defun mpv-play-url (url)
  "Start an mpv process playing the video stream at URL.

See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options."
  (interactive "sURL: ")
  (unless (mpv--url-p url)
    (user-error "Invalid argument: `%s' (must be a valid URL)" url))
  (mpv-start url))

;;;###autoload
(defun mpv--playlist-append (thing &rest args)
  "Append THING to the current mpv playlist.

If ARGS are provided, they are passed as per-file options to mpv."
  (mpv-run-command "loadfile" thing "append"
                   (string-join
                    (mapcar (lambda (arg)
                              (string-trim-left arg "--"))
                            args)
                    ","))
  (when-let* ((count (mpv-get-property "playlist-count"))
              (index (1- count))
              (filename (mpv-get-property (format "playlist/%d/filename" index))))
    (message "Added `%s' to the current playlist" filename)))

(defun mpv-playlist-append (path &rest args)
  "Append the file at PATH to the current mpv playlist.

If ARGS are provided, they are passed as per-file options to mpv."
  (interactive "fFile: ")
  (apply 'mpv--playlist-append (expand-file-name path) args))

(defun mpv-playlist-append-url (url &rest args)
  "Append URL to the current mpv playlist.

If ARGS are provided, they are passed as per-file options to mpv."
  (interactive "sURL: ")
  (unless (mpv--url-p url)
    (user-error "Invalid argument: `%s' (must be a valid URL)" url))
  (apply 'mpv--playlist-append url args))

;;;###autoload
(defun mpv-quit (watch-later)
  "Exit the current mpv process.

If WATCH-LATER is non-nil, tell mpv store the current playback
position for later.  When called interactively, prompt whether to
do so."
  (interactive
   (list (y-or-n-p "Save to watch later?")))
  (if watch-later
      (mpv-run-command "quit-watch-later")
    (mpv-kill)))

;;;###autoload
(defun mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when mpv--queue
    (tq-close mpv--queue))
  (when (mpv-live-p)
    (kill-process mpv--process))
  (with-timeout
      (0.5 (error "Failed to kill mpv"))
    (while (mpv-live-p)
      (sleep-for 0.05)))
  (setq mpv--process nil)
  (setq mpv--queue nil))

;;;###autoload
(defun mpv-pause ()
  "Pause or unpause playback."
  (interactive)
  (mpv--enqueue '("cycle" "pause") #'ignore))

(defun mpv-get-property (property)
  "Return the value of the given PROPERTY."
  (mpv-run-command "get_property" property))

(defun mpv-set-property (property value)
  "Set the given PROPERTY to VALUE."
  (mpv-run-command "set_property" property value))

(defun mpv-cycle-property (property)
  "Cycle the given PROPERTY."
  (mpv-run-command "cycle" property))

(defun mpv-get-playback-position ()
  "Return the current playback position in seconds."
  (mpv-get-property "playback-time"))

(defun mpv-get-duration ()
  "Return the estimated total duration of the current file in seconds."
  (mpv-get-property "duration"))

;;;###autoload
(defun mpv-insert-playback-position (&optional arg)
  "Insert the current playback position at point.

When called with a non-nil ARG, insert a timer list item like `org-timer-item'."
  (interactive "P")
  (let ((time (mpv-get-playback-position)))
    (funcall
     (if arg #'mpv--position-insert-as-org-item #'insert)
     (org-timer-secs-to-hms (round time)))))

(defun mpv--position-insert-as-org-item (time-string)
  "Insert a description-type item with the playback position TIME-STRING.

See `org-timer-item' which this is based on."
  (cl-letf (((symbol-function 'org-timer)
             (lambda (&optional _restart no-insert)
               (funcall
                (if no-insert #'identity #'insert)
                (concat time-string " ")))))
    (org-timer-item)))

;;;###autoload
(defun mpv-seek-to-position-at-point ()
  "Jump to playback position as inserted by `mpv-insert-playback-position'.

This can be used with the `org-open-at-point-functions' hook."
  (interactive)
  (save-excursion
    (skip-chars-backward ":[:digit:]" (point-at-bol))
    (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}")
      (let ((secs (org-timer-hms-to-secs (match-string 0))))
        (when (>= secs 0)
          (mpv-seek secs))))))

;;;###autoload
(defun mpv-speed-set (factor)
  "Set playback speed to FACTOR."
  (interactive "nFactor: ")
  (mpv--enqueue `("set" "speed" ,(abs factor)) #'ignore))

;;;###autoload
(defun mpv-speed-increase (steps)
  "Increase playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (let ((factor (if (>= steps 0)
                    (* steps mpv-speed-step)
                  (/ 1 (* (- steps) mpv-speed-step)))))
    (mpv--enqueue `("multiply" "speed" ,factor) #'ignore)))

;;;###autoload
(defun mpv-speed-decrease (steps)
  "Decrease playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (mpv-speed-increase (- steps)))

;;;###autoload
(defun mpv-volume-set (factor)
  "Set playback volume to FACTOR."
  (interactive "nFactor: ")
  (mpv--enqueue `("set" "volume" ,(abs factor)) #'ignore))

;;;###autoload
(defun mpv-volume-increase (steps)
  "Increase playback volume by STEPS factors of `mpv-volume-step'."
  (interactive "p")
  (let ((factor (if (>= steps 0)
                    (* steps mpv-volume-step)
                  (/ 1 (* (- steps) mpv-volume-step)))))
    (mpv--enqueue `("multiply" "volume" ,factor) #'ignore)))

;;;###autoload
(defun mpv-volume-decrease (steps)
  "Decrease playback volume by STEPS factors of `mpv-volume-step'."
  (interactive "p")
  (mpv-volume-increase (- steps)))

(defun mpv--raw-prefix-to-seconds (arg)
  "Convert raw prefix argument ARG to seconds using `mpv-seek-step'.
Numeric arguments will be treated as seconds, repeated use
\\[universal-argument] will be multiplied with `mpv-seek-step'."
  (if (numberp arg)
      arg
    (* mpv-seek-step
       (cl-signum (or (car arg) 1))
       (log (abs (or (car arg) 4)) 4))))

;;;###autoload
(defun mpv-seek (seconds)
  "Seek to the given (absolute) time in SECONDS.
A negative value is interpreted relative to the end of the file."
  (interactive "nPosition in seconds: ")
  (mpv--enqueue `("seek" ,seconds "absolute") #'ignore))

;;;###autoload
(defun mpv-seek-forward (arg)
  "Seek forward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (mpv--enqueue `("seek" ,(mpv--raw-prefix-to-seconds arg) "relative") #'ignore))

;;;###autoload
(defun mpv-seek-backward (arg)
  "Seek backward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (mpv-seek-forward (- (mpv--raw-prefix-to-seconds arg))))

;;;###autoload
(defun mpv-revert-seek ()
  "Undo the previous seek command."
  (interactive)
  (mpv--enqueue '("revert-seek") #'ignore))

;;;###autoload
(defun mpv-playlist-next ()
  "Go to the next entry on the playlist."
  (interactive)
  (mpv--enqueue '("playlist-next") #'ignore))

;;;###autoload
(defun mpv-playlist-prev ()
  "Go to the previous entry on the playlist."
  (interactive)
  (mpv--enqueue '("playlist-prev") #'ignore))

;;;###autoload
(defun mpv-version ()
  "Return the mpv version string.
When called interactively, also show a more verbose version in
the echo area."
  (interactive)
  (let ((version (cadr (split-string (car (process-lines mpv-executable "--version"))))))
    (prog1 version
      (if (called-interactively-p 'interactive)
          (message "mpv %s" version)))))

(provide 'mpv)
;;; mpv.el ends here
