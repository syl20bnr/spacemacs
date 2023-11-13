;;; greader.el --- gnamù reader, send buffer contents to a speech engine. -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2023  Free Software Foundation, Inc.

;; package-requires: ((emacs "25"))
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; Keywords: tools, accessibility

;; Version: 0.8.0

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

;; Greader reads aloud the current buffer continuously, moving the point while
;; reading proceeds.

;; It can be used in conjunction with emacspeak or speechd-el packages.
;; it doesn't substitute those packages, but integrates them, providing a
;; functionality that those lacks.
;; In addition to reading the buffer, Greader provides a timer for reading,
;; and a "sleep mode" when you are tired and you want simply relax yourself.
;; for further details, please see the "README" file.

;; To start using greader, you have to install espeak and/or speech-dispatcher,
;; and make sure those packages work correctly.

;; In order to read a buffer:
;; 'M-x greader-mode RET'
;; 'C-r SPC'

;;; Code:
(require 'view)
(defvar-local greader-timer-flag nil)

(defvar greader-auto-tired-timer nil)
(defvar greader-auto-tired-end-timer)
(defvar greader-last-point nil)
(defvar greader-tired-timer nil)
(defvar greader-timer-enabled-interactively nil)
(defvar greader-stop-timer 0)
(defvar greader-elapsed-timer 0)
(defvar greader-elapsed-time 0)
(defvar greader-timer-flag nil)
(defvar greader-tired-flag nil)
(defvar greader-filter-enabled nil)
(defvar greader-debug-buffer "spd-output"
  "Contains the buffer name for debugging purposes.")
(defvar greader-backend-action #'greader--default-action)
(defvar greader-status 'paused)
(defvar greader-synth-process nil)
(require 'seq)

(define-obsolete-variable-alias 'greader-before-get-sentence-functions
  'greader-before-get-sentence-hook "2023")
(defvar greader-before-get-sentence-hook nil
  "Hook run before getting a sentence.
Functions in this variable don't receive arguments.")

(defvar greader-after-get-sentence-functions nil
  "Hook run after getting a sentence.
Functions in this hook take a string as argument, and should modify
  that string that contains the sentence that will be read.
the function should return modified sentence, or nil if no operation
  was needed.")

(defun greader--call-functions-after-get-of-sentence (sentence)
  "Call functions in `greader-after-get-sentence-functions'.
Return SENTENCE, eventually modified by the functions."
  (let ((result sentence))
    (run-hook-wrapped 'greader-after-get-sentence-functions
                      (lambda (func)
                        (when (fboundp func)
	                  (setq result (or (funcall func result)
	                                   sentence)))))
    result))

(defvar greader-before-read-hook nil
  "Code to execute just before start of reading.")

(defvar greader-after-read-hook nil
  "Execute code just after reading a sentence.")

(define-obsolete-variable-alias 'greader-before-finish-hook
  'greader-before-finish-functions "2023")
(defvar greader-before-finish-functions nil
  "Code executed just after finishing reading of buffer.
Functions in this hook should return non-nil if at least one function
  returns non-nil, meaning that reading of buffer continues.
If all the functions called return nil, reading finishes normally.")

(defun greader--call-before-finish-functions ()
  "Return t if at least one of the function return t.
If all the functions in the hook return nil, this function return nil."
  (run-hook-with-args-until-success 'greader-before-finish-functions))

(defvar greader-after-stop-hook nil
  "Hook run just after tts is stopped.")

(defgroup
  greader
  nil
  "Greader customization."
  :group 'convenience)
(defcustom greader-hyphen-regex "[-‐]\n+[[:blank:]]*"
  "Regex to use when dehyphenation is needed."
  :type 'string
  :tag "greader hyphen regex")

(defcustom
  greader-backends
  '(greader-espeak greader-speechd)
  "A list of functions that are back-ends for greader."
  :tag "greader back-ends"
  :type '(repeat function))

(defcustom
  greader-current-backend
  #'greader-espeak
  "Greader back-end to use."
  :tag "greader current back-end"
  :type
  `(radio
    ,@(mapcar
       (lambda (backend)
	 `(function-item ,backend))
       greader-backends)))

(defcustom
  greader-auto-tired-mode-time
  "22"
  "Specifies the hour when tired mode will be activated automatically."
  :tag "greader-tired-mode start time"
  :type 'string)

(defcustom
  greader-auto-tired-time-end
  "07"
  "Specifies when auto tired mode should be disabled.
For more information on syntax, see documentation of
`greader-auto-tired-mode-time'."
  :tag "greader auto tired end time"
  :type 'string)

(defcustom
  greader-auto-tired-mode
  nil
  "Enable or disable`auto-tired-mode'.
If t, auto-tired-mode is enabled, and
tired mode will be turned on at a time specified with
`greader-auto-tired-mode-time' and disabled at
`greader-auto-tired-mode-end-time' automatically."
  :tag "greader auto tired mode"
  :type 'boolean)

(defcustom
  greader-tired-time
  60
  "Auto tired mode.
Sets when, in seconds after a timer stop, the point must be moved
to the last position have you called command `greader-read'."
  :type 'integer
  :tag "greader seconds for tired mode")

(defcustom
  greader-soft-timer
  t
  "If enabled, reading of text will end not exactly at time expiration.
Instead, the sentence will be read completely."
  :tag "greader soft timer"
  :type 'boolean)

(defcustom
  greader-timer
  10
  "Minutes for timer to stop reading."
  :type 'integer
  :tag "time to stop reading")

(defcustom
  greader-debug
  nil
  "Enables debug information."
  :tag "enable debug"
  :type 'boolean)

(defcustom   greader-hook nil ;; FIXME: Can't see where it's run!
  "Hook run after mode activation.
Through this hook you can
customize your key definitions for greader, for example."
  :tag "greader-mode hook"
  :type 'hook)

(defcustom greader-move-to-next-chunk
  #'greader-forward-sentence
  "The function that moves the cursor for the next chunk of text.
For example if you have specified `sentence-at-point' function to get
the current chunk, you should specify `forward-sentence' for this
variable."
  :tag "greader move to next chunk function"
  :type 'function)

(defcustom greader-read-chunk-of-text
  #'greader-sentence-at-point
  "The function used to get the portion of text to read.
The variable `greader-move-to-next-chunk' must be set to a function that
moves the cursor to the same amount of text that is set in this
variable.  For example, if you specify a function that gets a
sentence, you should specify a function that moves to the next one."
  :type 'function
  :tag "greader get chunk of text function")
(defcustom greader-use-prefix t
  "Toggle on or off for use register feature.
if set to t, when you call function `greader-read', that function sets a
  register that points to the actual position in buffer.
  when you call again function `greader-read' with a prefix argument, the point
  is set at register position then reading starts from there."
  :type 'boolean
  :tag "use register")

(defun greader-set-reading-keymap ()
  "Set greader's keymap when reading."
  (greader-mode -1)
  (greader-reading-mode t))

(defun greader-set-greader-keymap ()
  "Set greader's keymap when not reading."

  (greader-mode t)
  (greader-reading-mode -1))

(define-obsolete-variable-alias 'greader-map 'greader-mode-map "2022")
(defvar greader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r s")   #'greader-toggle-tired-mode)
    (define-key map (kbd "C-r r")   #'isearch-backward)
    (define-key map (kbd "C-r SPC") #'greader-read)
    (define-key map (kbd "C-r l")   #'greader-set-language)
    (define-key map (kbd "C-r t")   #'greader-toggle-timer)
    (define-key map (kbd "C-r f")   #'greader-get-attributes)
    (define-key map (kbd "C-r b")   #'greader-change-backend)
    (define-key map (kbd "C-r c") #'greader-compile-at-point)
    map))

(defvar greader-reading-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'greader-stop)
    (define-key map (kbd "p")   #'greader-toggle-punctuation)
    (define-key map (kbd ".")   #'greader-stop-with-timer)
    (define-key map (kbd "+")   #'greader-inc-rate)
    (define-key map (kbd "-")   #'greader-dec-rate)
    (define-key map (kbd "<left>")   #'greader-backward)
    (define-key map (kbd "<right>")   #'greader-forward)
    map))

;;;###autoload
(define-minor-mode greader-mode
  nil
  :lighter " greader"
  :group 'greader
  (cond
   (greader-mode
    (when greader-queue-mode
      (greader-queue-mode -1))
    (greader-load-backends))))
;;;###autoload
(define-minor-mode greader-reading-mode
  nil
  :interactive nil
  :keymap greader-reading-map
  :lighter " reading...")

(defun set-bookmark-for-greader ()
  "Imposta il segnalibro ad ogni interruzione della lettura."
  (when buffer-file-name
    (let ((inhibit-message t))
      (bookmark-set (buffer-name)))))
;;;###autoload
(define-minor-mode greader-auto-bookmark-mode
  "Enable automatic bookmarking.
Each time the reading of the buffer is stopped a bookmark is saved
when the buffer is visiting a file."
  :lighter " bk"
  :global t
  (if greader-auto-bookmark-mode
      (add-hook 'greader-after-stop-hook #'set-bookmark-for-greader)
    (remove-hook 'greader-after-stop-hook #'set-bookmark-for-greader)))
;; greader-region-mode is a non-interactive minor mode that deals with
;; read the active region instead of the entire buffer.
;; The current implementation of greader probably dictates that the
;; buffer needs to be temporarily narrowed when the region is
;; active, so that the functions that deal with obtaining the sentences
;; to read and move the point "believe" that that is all the
;; buffer to read.
(defvar greader-start-region nil
  "start of region.")
(defvar greader-end-region nil
  "end of region.")

(defun greader--active-region-p ()
  "Return t if the region in the current buffer is active.
Active in this context means that the variables
  `greader-start-region' and `greader-end-region' are set appropriately."
  (if (and greader-start-region greader-end-region)
      t
    nil))

(defun greader-narrow ()
  "Narrow current buffer if region is active."
  (unless (buffer-narrowed-p)
    (narrow-to-region greader-start-region greader-end-region)))

;; This function widens the buffer, and is added to the
;; `greader-after-stop-hook' hook by `greader-region-mode'.
(defun greader-widen ()
  "Widen buffer and set greader-region variables to nil."
  (setq greader-start-region nil)
  (setq greader-end-region nil)
  (greader-region-mode -1)  
  (widen))

;; This function places the point at the beginning of the active region.
(defun greader-set-point-to-start-of-region ()
  "set the point to the beginning of the active region.
This only happens if the variables `greader-start-region' and
`greader-end-region' are set."
  (when (and greader-start-region greader-end-region)
    (goto-char greader-start-region)))

(define-minor-mode greader-region-mode
  "This mode activates when the region is active."
  :interactive nil
  (if greader-region-mode
      (progn
	(setq greader-start-region (region-beginning))
	(setq greader-end-region (region-end))
	(greader-narrow)
	(add-hook 'greader-after-stop-hook #'greader-widen)
	(add-hook 'greader-before-finish-functions #'greader-widen)
	(greader-set-point-to-start-of-region))
    (remove-hook 'greader-before-finish-functions #'greader-widen)
    (remove-hook 'greader-after-stop-hook #'greader-widen)))



(defun greader-set-register ()
  "Set the `?G' register to the point in current buffer."
  (when greader-use-prefix
    (point-to-register ?G)))

(defun greader-jump-to-register ()
  "Jump to register `?G' if `greader-use-prefix' is enabled."
  (when greader-use-prefix
    (jump-to-register ?G)))

(defun greader--get-backends ()
  "Return actual available back-ends, as a list of strings."
  (let (b)
    (dolist (greader-elem greader-backends nil)
      (setq b (append b `(,(get greader-elem 'greader-backend-name)))))
    b))

(defun greader-call-backend (command &optional arg)
  "Call backend passing it COMMAND and ARG.
\(internal use!\)." ;; FIXME: Use "--" in the name, then.

  (if arg
      (funcall greader-current-backend command arg)
    (funcall greader-current-backend command)))
(defvar
  greader-backend-filename
  (greader-call-backend 'executable))
(defvar greader-backend `(,greader-backend-filename))
(defvar greader-orig-buffer nil)
(defvar greader-dissoc-buffer "*Dissociation*")
(defvar greader-temp-function nil)
(defun greader-change-backend (&optional backend)
  "Change backend used for actually read the buffer.
If BACKEND is non-nil, it changes to BACKEND, else it cycles through
available backends."
  (interactive
   (list
    (if current-prefix-arg
	(completing-read "Back-end: " (greader--get-backends)))))
  (setq-local greader-current-backend
              (cond
	       ((functionp backend)
	        (if (memq backend greader-backends)
	            backend
	          (error "Not a greader's back-end: %S" backend)))
	       ((stringp backend)
                (let ((result nil))
                  (dolist (elem greader-backends result)
	            (if
	                (equal
	                 (get elem 'greader-backend-name) backend)
	                (setq result elem)))
	          (or result
	              (user-error "Not a greader's back-end: %S" backend))))
	       (backend
	        (error "backend should be a string or a function: %S" backend))
	       (t
	        (car (or (cdr (memq greader-current-backend greader-backends))
	                 greader-backends)))))
  (message "Current back-end is %s"
           (get greader-current-backend 'greader-backend-name)))

(defun greader-load-backends ()
  "Load backends taken from `greader-backends'."
  (mapcar #'require greader-backends))

(defun greader-read-asynchronous (txt)
  "Read the text given in TXT."
  (if greader-debug
      (greader-debug "greader-read-asynchronous entered\n"))
  (run-hooks 'greader-before-read-hook)
  (greader-build-args)
  (if (and txt (greader-sentence-needs-dehyphenation txt))
      (setq txt (greader-dehyphenate txt)))
  (let* ((txt (concat " " txt))
         (backend (append greader-backend `(,txt))))
    (and (stringp txt)
	 (setq-local greader-synth-process (make-process
				            :name "greader-backend"
				            :sentinel #'greader-action
				            :filter #'greader-process-filter
				            :command backend)))
    (if greader-debug
	(progn
	  (set-process-buffer greader-synth-process greader-debug-buffer)
	  (greader-debug (message "greader-read-asynchronous: %S" backend))))))

(defun greader-get-status ()
  "Return greader status."
  greader-status)

(defun greader-change-status (arg)
  "Change status of greader using ARG."
  (if greader-debug
      (greader-debug "greader-change-status entered"))
  (if (symbolp arg)
      (setq greader-status arg)
    (error "Status must be a symbol!")))

(defun greader-action (process event)
  "Sentinel for greader processes using PROCESS and EVENT."
  (if greader-debug
      (progn
	(greader-debug "greader-action entered.\n")
	(greader-debug (format "event: %S\n" event))))
  (funcall greader-backend-action process event))

(defun greader-tts-stop ()
  "Stop reading of current buffer."
  (set-process-sentinel greader-synth-process #'greader--default-action)
  (if
      (not
       (eq
	(greader-call-backend 'stop) 'not-implemented))
      (greader-call-backend 'stop))
  (delete-process greader-synth-process)
  (setq-local greader-backend-action #'greader--default-action))

(defun greader--default-action (&optional _process event)
  "Internal use.
Optional argument PROCESS
Optional argument EVENT ."
  (if greader-debug
      (greader-debug (format "greader--default-action entered.\nevent: %S\n" event)))
  (cond
   ((and (greader-timer-flag-p) (timerp greader-stop-timer))
    (greader-cancel-elapsed-timer)
    (greader-cancel-stop-timer)
    (greader-reset-elapsed-time)
    (setq-local greader-stop-timer 0)
    (greader-set-greader-keymap))))

(defun greader-build-args ()
  "Build the string that will be passed to the back-end."
  (greader-reset)
  (let (args arg)
    (setq arg
	  (greader-call-backend 'rate))
    (setq args (append `(,arg) args))
    (cond ((greader-call-backend 'lang)
	   (setq arg
		 (greader-call-backend 'lang))
	   (setq args (append `(,arg) args))))
    (cond ((greader-call-backend 'punctuation)
	   (setq arg (greader-call-backend 'punctuation))
	   (setq args (append `(,arg) args))))
    (setq greader-backend (greader-call-backend 'executable))
    (cond
     (
      (not
       (eq
	(greader-call-backend 'extra)
	'not-implemented))
      (setq arg (greader-call-backend 'extra))
      (setq args (append `(,arg) args))))
    (setq greader-backend (append `(,greader-backend) args))))

(defun greader-reset ()
  "Reset greader."
  (setq greader-backend `(,(greader-call-backend 'executable))))

(defun greader-next-action (_process event)
  "Perform next action when reading.
Argument PROCESS .
Argument EVENT ."
  (if greader-debug
      (greader-debug (format "greader-next-action: %s" event)))
  (run-hooks 'greader-after-read-hook)
  (funcall greader-move-to-next-chunk)
  (greader-read))

(defun greader-response-for-dissociate (&optional _prompt)
  "Return t to the caller until a condition is reached.
This function will be locally bound to `y.or-n-p' until
`dissociated-press' does the job.
Optional argument PROMPT variable not used."
  (with-current-buffer greader-orig-buffer
    (if (< (buffer-size greader-dissoc-buffer) 100000)
	t
      nil)))

(defun greader-read-dissociated ()
  "Use `dissociated-press to read a text dissociately.
\(Helpful for
mindfullness!)."
  (interactive)
  (setq greader-orig-buffer (current-buffer))
  (setq greader-dissoc-buffer (get-buffer-create "*Dissociation*"))
  (unwind-protect
      (progn
	(fset 'greader-temp-function (symbol-function 'y-or-n-p))
	(fset 'y-or-n-p (symbol-function
			 'greader-response-for-dissociate))
	(let ((arg (random 10)))
	  (while (equal arg 0)
	    (setq arg (random 10)))
	  (dissociated-press arg))
	(switch-to-buffer greader-dissoc-buffer)
	(goto-char (point-min))

	(greader-mode 1)
	(greader-read))
    (fset 'y-or-n-p (symbol-function 'greader-temp-function))))

(defun greader-read (&optional goto-marker)
  "Start reading of current buffer.
if `GOTO-MARKER' is t and if you pass a prefix to this
  function, point jumps at the last position you called command `greader-read'."

  (interactive "P")
  (when goto-marker
    (greader-jump-to-register))
  (when (called-interactively-p 'any)
    (greader-set-register))

  (if (and greader-tired-flag (= greader-elapsed-time 0))
      (progn
	(if greader-tired-timer
	    (cancel-timer greader-tired-timer))
	(setq-local greader-last-point (point))))

  (cond
   ((and (greader-timer-flag-p) (not (timerp greader-stop-timer)))
    (greader-setup-timers)))
  (when (region-active-p)
    (cond
     ((and (not greader-region-mode) (not (greader--active-region-p)))
      (greader-region-mode 1))))
  (run-hooks greader-before-get-sentence-hook)
  (let ((chunk (funcall greader-read-chunk-of-text)))
    (if chunk
	(progn
	  (setq chunk (greader--call-functions-after-get-of-sentence chunk))
	  ;; This extra verification is necessary because espeak has a bug that,
	  ;; when we pass a string containing a vocal plus only 2 .. it reads
	  ;; garbage.
	  (if (string-suffix-p ".." chunk)
	      (setq chunk (concat chunk ".")))
	  (greader-set-reading-keymap)
	  (setq-local greader-backend-action #'greader-next-action)
	  (greader-read-asynchronous chunk))
      (progn
	(setq-local greader-backend-action #'greader--default-action)
	(greader-set-greader-keymap)
	(unless (greader--call-before-finish-functions)
	  (greader-read-asynchronous ". end"))))))

(defun greader-stop ()
  "Stops reading of document."
  (interactive)
  (cond
   ((and (> greader-elapsed-time 0) greader-timer-flag)
    (greader-cancel-elapsed-timer)
    (greader-cancel-stop-timer)
    (if (>= greader-elapsed-time (1- (greader-convert-mins-to-secs greader-timer)))
	(greader-reset-elapsed-time))
    (setq-local greader-stop-timer 0)))
  (greader-set-greader-keymap)
  (greader-tts-stop)
  (run-hooks 'greader-after-stop-hook))

(defun greader-debug (arg)
  "Used to get some fast debugging.
Argument ARG is not used."
  (save-current-buffer
    (get-buffer-create greader-debug-buffer)
    (set-buffer greader-debug-buffer)
    (insert arg)))

(defun greader-forward-sentence ()
  "Move the point to next sentence."
  (let ((result (greader-call-backend 'next-text)))
    (if (not (equal result 'not-implemented))
	result
      (forward-sentence))))

(defun greader-get-sentence ()
  "Get current sentence.
Before returning sentence, this function runs
`greader-before-get-sentence-hook'
If at end of buffer, nil is returned."
  (let ((result (greader-call-backend 'get-text)))
    (if (stringp result)
	result
      (let ((sentence-start (make-marker)))
	(setq sentence-start (point))
	(save-excursion
	  (when (not (eobp))
	    (forward-sentence))
	  (if (> (point) sentence-start)
	      (string-trim (buffer-substring sentence-start (point)) "[ \t\n\r]+")
	    nil))))))

(defun greader-sentence-at-point ()
  "Get sentence starting from point."
  (greader-get-sentence))

(defun greader-process-filter (_process string)
  "Process filter.
Optional argument STRING contains the string passed to
`greader-read-asynchronous'."
  (if greader-filter-enabled
      (message string)))

(defun greader-set-language (lang)
  "Set language of tts.
LANG must be in ISO code, for example `en' for English or `fr' for
French.  This function sets the language of tts local for current
buffer, so if you want to set it globally, please use
`M-x customize-option RET greader-language RET'."
  (interactive
   (list
    (let ((result (greader-call-backend 'set-voice nil)))
      (if (equal result 'not-implemented)
	  (read-string "Set language to: ")
	result))))
  (greader-call-backend 'lang lang))
(defun greader-set-punctuation (flag)
  "Set punctuation to FLAG."
  (greader-call-backend 'punctuation flag))

(defun greader--get-local-language ()
  "Returns the language code from the system's locale."
  (let ((locale (or (getenv "LANG") ; First try with the LANG environment variable
                    (getenv "LC_ALL") ; Then with LC_ALL
                    "en"))) ; Default to "en" if nothing is found
    ;; Extracts the language code from the locale (e.g., "en_US.UTF-8" becomes "en")
    (if (string-match "\\([a-z]+\\)_" locale)
        (match-string 1 locale)
      "en"))) ; Default to "en" if the locale format is unrecognized

(defun greader-get-language ()
  "return language set in current back-end.
if `current-backend' does not implement `get-language' command, try to
get the language from the environment."
  (let ((lang nil))
    (if (equal (greader-call-backend 'get-language)
	       'not-implemented)
	(setq lang (greader--get-local-language))
      (setq lang (greader-call-backend 'get-language)))
    lang))

(defun greader-toggle-punctuation ()
  "Toggle punctuation locally for current buffer."
  (interactive)
  (if (not (greader-call-backend 'punctuation))
      (progn
	(greader-tts-stop)
	(greader-set-punctuation 'yes)
	(message "punctuation enabled in current buffer")
	(greader-read))
    (progn
      (greader-tts-stop)
      (greader-set-punctuation 'no)
      (message "punctuation disabled in current buffer")
      (greader-read))))

(defun greader-toggle-timer-flag ()
  "Not yet documented."
  (cond
   (greader-timer-flag
    (setq-local greader-timer-flag nil)
    (greader-reset-elapsed-time)
    (if (not (equal greader-elapsed-timer 0))
	(greader-cancel-elapsed-timer))
    (if (and greader-auto-tired-mode greader-tired-flag)
	(greader-toggle-tired-mode)))
   ((not greader-timer-flag)
    (setq-local greader-timer-flag t))))

(defun greader-toggle-timer ()
  "Toggle on or off timer when reading.
To configure the timer \(in minutes\) call `M-x greader-set-timer' or
  `C-r t'."
  (interactive)
  (greader-toggle-timer-flag)
  (if greader-timer-flag
      (progn
	(setq-local greader-timer-enabled-interactively t)
	(message "timer enabled in current buffer"))
    (progn
      (setq-local greader-timer-enabled-interactively nil)
      (if greader-tired-flag
	  (greader-toggle-tired-flag))
      (message "timer disabled in current buffer"))))

(defun greader-set-timer (&optional timer-in-mins)
  "Set timer for reading expressed in minutes.
This command should be
used only if you want to set locally a timer different of that you set
via customize, that is considered the default value for this
variable.
Optional argument TIMER-IN-MINS timer in minutes (integer)."

  (interactive "Nset timer for:")
  (if (not (greader-timer-flag-p))
      (greader-toggle-timer))
  (setq-local greader-timer timer-in-mins))

(defun greader-timer-flag-p ()
  "Not yet documented."
  greader-timer-flag)

(defun greader-setup-timers ()
  "Set up timers, that is, call `run-at-time' using settings you have specified."
  (catch 'timer-is-nil
    (cond
     ((greader-timer-flag-p)
      (setq-local greader-stop-timer (run-at-time (- (greader-convert-mins-to-secs greader-timer) greader-elapsed-time) nil #'greader-stop-timer-callback))
      (setq-local greader-elapsed-timer
                  (run-at-time 1 1 #'greader-elapsed-time)))
     ((not (greader-timer-flag-p))
      (throw 'timer-is-nil nil))))
  t)

(defun greader-elapsed-time ()
  "Not documented (internal use)."
  (setq-local greader-elapsed-time (1+ greader-elapsed-time)))

(defun greader-convert-mins-to-secs (mins)
  "Convert MINS in seconds."
  (* mins 60))

(defun greader-cancel-stop-timer ()
  "Not documented, internal use."
  (cancel-timer greader-stop-timer))

(defun greader-cancel-elapsed-timer ()
  "Not documented, internal use."
  (cancel-timer greader-elapsed-timer))

(defun greader-reset-elapsed-time ()
  "Not documented, internal use."
  (setq-local greader-elapsed-time 0))

(defun greader-stop-with-timer ()
  "Stop reading of buffer and also reset timer.
If you use this
command, next reading will start timer at its current value.  If you
stop normally with `greader-stop', next reading will continue from the
time elapsed before you stopped."
  (interactive)
  (if (greader-timer-flag-p)
      (progn
	(greader-cancel-elapsed-timer)
	(greader-cancel-stop-timer)
	(setq-local greader-stop-timer 0)
	(greader-reset-elapsed-time)))
  (greader-stop))

(defun greader-stop-timer-callback ()
  "Function called when timer expires."
  (if greader-tired-flag
      (greader-setup-tired-timer))
  (cond
   ((greader-soft-timer-p)
    (setq-local greader-backend-action #'greader--default-action))
   ((not greader-soft-timer)
    (greader-stop))))

(defun greader-soft-timer-p ()
  "Return t if soft-timer is enabled.
With soft timer, greader will stop reading at the end of sentence is
  actually reading.
If it is disabled, greader will stop reading immediately after timer expiration."
  (if greader-soft-timer
      t
    nil))

(defun greader-toggle-tired-flag ()
  "Not documented, internal use."
  (if greader-tired-flag
      (progn
	(if greader-timer-flag
	    (greader-toggle-timer))
	(setq-local greader-tired-flag nil))
    (progn
      (if (not greader-timer-flag)
	  (greader-toggle-timer)))
    (setq-local greader-tired-flag t)))

(defun greader-toggle-tired-mode ()
  "Toggle tired mode.
if tired mode is enabled, when a timer expires, greader will wait an
  amount of time customizable, and if Emacs remains iddle for that
  time, point in buffer will be placed at last position where you
  called command `greader-read'.
Enabling tired mode implicitly enables timer also."
  (interactive)
  (if (not greader-tired-flag)
      (progn
	(greader-toggle-tired-flag)
	(if (not (greader-timer-flag-p))
	    (greader-toggle-timer-flag))
	(message "tired mode enabled in current buffer."))
    (progn
      (if (not greader-timer-enabled-interactively)
	  (greader-toggle-timer-flag))

      (greader-toggle-tired-flag)
      (message "tired mode disabled in current buffer"))))

(defun greader-setup-tired-timer ()
  "Not documented, internal use."
  (if greader-tired-flag
      (run-with-idle-timer
       (time-add
	(current-idle-time)
	(seconds-to-time
	 greader-tired-time))
       nil #'greader-tired-mode-callback)))

(defun greader-tired-mode-callback ()
  "Not documented, internal use."
  (if (equal last-command #'greader-read)
      (greader-move-to-last-point)))

(defun greader-move-to-last-point ()
  "Not documented, internal use."
  (goto-char greader-last-point))

(defun greader-auto-tired-mode-setup ()
  "Not documented, internal use."
  (if greader-auto-tired-mode
      (progn
	(if (not greader-tired-flag)
	    (greader-toggle-tired-mode))
	(setq-local greader-auto-tired-timer
	            (run-at-time nil 1 #'greader-auto-tired-callback)))
    (progn
      (if greader-tired-flag
	  (greader-toggle-tired-mode))
      (setq-local greader-auto-tired-timer
                  (cancel-timer greader-auto-tired-timer)))))

(defun greader-toggle-auto-tired-mode-flag ()
  "Not documented, internal use."
  (if greader-auto-tired-mode
      (progn
	(setq-local greader-auto-tired-mode nil)
	(if greader-auto-tired-timer
	    (cancel-timer greader-auto-tired-timer)))
    (progn
      (setq-local greader-auto-tired-mode t)
      (greader-auto-tired-mode-setup))))

(defun greader-toggle-auto-tired-mode () "Enable auto tired mode.
In this mode, greader will enter in tired mode at a customizable time
  and will exit from it at another time.  The default is 22:00 for
  entering and 08:00 for exiting."  (interactive)
  (greader-toggle-auto-tired-mode-flag) (if greader-auto-tired-mode
					    (message "auto-tired mode enabled in current buffer") (message
												   "auto-tired mode disabled in current buffer.")))

(defun greader-current-time ()
  "Not documented, internal use."
  (string-to-number (format-time-string "%H")))

(defun greader-convert-time (time)
  "Not documented, internal use."
  (let ((current-t (decode-time))
	(i (nth 2 (decode-time)))
	(counter (nth 2 (decode-time))))
    (if (stringp time)
	(setq time (string-to-number time)))
    (catch 'done
      (while t
	(if (= i time)
	    (throw 'done nil))
	(cl-incf i)
	(cl-incf counter)
	(if (= i 24)
	    (setq i 0))))
    (setcar (cdr (cdr current-t)) counter)
    (setcar current-t 0)
    (setcar (cdr current-t) 0)
    (apply #'encode-time current-t)))

(defun greader-current-time-in-interval-p (time1 time2)
  "Not documented, internal use."
  (let
      ((current-t (current-time)))
    (if
	(and (time-less-p time1 current-t) (time-less-p current-t time2))
	t
      nil)))

(defun greader-auto-tired-callback ()
  "Not documented, internal use."
  (if
      (stringp greader-auto-tired-mode-time)
      (setq-local greader-auto-tired-mode-time (greader-convert-time greader-auto-tired-mode-time)))
  (if
      (stringp greader-auto-tired-time-end)
      (setq-local greader-auto-tired-time-end (greader-convert-time greader-auto-tired-time-end)))
  (if
      (and
       (greader-current-time-in-interval-p greader-auto-tired-mode-time greader-auto-tired-time-end)
       (not greader-tired-flag))
      (greader-toggle-tired-mode))
  (if
      (and
       (not (greader-current-time-in-interval-p greader-auto-tired-mode-time greader-auto-tired-time-end))
       greader-tired-flag)
      (progn
	(setq-local greader-auto-tired-mode-time (number-to-string (nth 2 (decode-time greader-auto-tired-mode-time))))
	(setq-local greader-auto-tired-time-end (number-to-string (nth 2 (decode-time greader-auto-tired-time-end))))
	(greader-toggle-tired-mode))))

(defun greader-set-rate (n)
  "Set rate in current buffer to tthe specified value in N.
rate is expressed in words per minute.  For maximum value, see `man espeak'."
  (greader-call-backend 'rate n))

(defun greader-inc-rate (&optional n)
  "Increment rate of speech by N units.
If prefix, it will be used to increment by that.  Default is N=10."
  (interactive "P")
  (if (not n)
      (setq n 10))
  (greader-tts-stop)
  (greader-set-rate (+ (greader-call-backend 'rate 'value) n))
  (greader-read))

(defun greader-dec-rate (&optional n)
  "Decrements rate of speech by units specified in N.
If prefix, it will be used to decrement  rate."
  (interactive "P")
  (if (not n)
      (setq n 10))
  (greader-tts-stop)
  (greader-set-rate (- (greader-call-backend 'rate 'value) n))
  (greader-read))

(defun greader-sentence-needs-dehyphenation (str)
  "Return t if there are lines broken by hyphens in STR, nil otherwise."
  (string-match greader-hyphen-regex str))

(defun greader-dehyphenate (sentence)
  "Join lines broken by hyphens in SENTENCE.
It is possible to customize what this function considers to be an
  hyphen, by setting `greader-hyphen-regex'."
  (replace-regexp-in-string greader-hyphen-regex "" sentence))

(defun greader-get-attributes ()
  "Print text properties associated with current char."
  (interactive)
  (print (text-properties-at (point))))

(defcustom greader-compile-command "--compile="
  "Espeak-ng parameter to compile a lang."
  :tag "greader compile command"
  :type 'string)

(defcustom greader-compile-extra-parameters nil
  "Extra parameters to pass to espeak-ng.
In general you should specify an alternative path for espeak voice
  data."
  :tag "greader compile extra parameters"
  :type '(repeat :tag "extra parameter" string))

;;;###autoload
(defcustom greader-compile-dictsource nil
  "Location of espeak dictionary source data.
You must configure this variable in order to use
  `greader-compile-mode'."
  :tag "greader compile source dictionary directory"
  :type '(repeat :tag "directory:" string))

(define-minor-mode greader-compile-mode
  "Espeak voice definition and compilation mode.
This global minor mode of greader allows saving of an
espeak-ng dictionary file and subsequent correspondent voice compilation
in one shot.
In some cases, the directory where espeak-ng keeps its data
is not writable by the normal user, in this case, when
saving the file, you will be asked to enter your password
administrator."
  :global t

  (if greader-compile-mode
      (progn
	(unless greader-compile-dictsource
	  (error "Please set or customize `greader-compile-dictsource'
    to define espeak-ng dictionary source location"))
	(add-hook 'after-save-hook #'greader-check-visited-file)
	;; FIXME: AFAIK `define-minor-mode' will emit pretty much
	;; this exact message if we don't.
	(message "greader-compile minor mode enabled"))
    (when (member #'greader-check-visited-file after-save-hook)
      ;; FIXME: AFAIK `define-minor-mode' will emit pretty much
      ;; this exact message if we don't.
      (message "greader-compile mode disabled"))
    ;; Call `remove-hook' even if the `member' test failed:
    ;; it's safer to do so anyway (e.g. the `member' test may
    ;; actually give the wrong answer).
    (remove-hook 'after-save-hook #'greader-check-visited-file)))

(defvar greader-compile-history nil)
(defun greader-compile (&optional lang)
  "Compile espeak voice for a given LANG.
when called interactively, compile the
espeak-ng definitions for a given language.
By default, greader-compile infers the language from the first two
letters of the file you are visiting.
If its LANG parameter is `non-nil', the function will ask for
specifying the language to compile, ignoring the current file name
but proposing it as default.
In case the specified language does not seem compatible with
buffer environment, this function will ask for confirmation before
proceed.
Important notice: The parameter is only for an interactive call
since the function uses the minibuffer to get
the language.
If it is not called interactively, pass no arguments to it, the
function is specifically designed to be executed by a hook."

  (interactive "P")

  (if lang
      (progn

	(setq lang (read-from-minibuffer "Language (2 letters):"
					 nil
					 nil
					 nil
					 nil
					 nil
					 (greader-compile-guess-lang))))

    (setq lang (greader-compile-guess-lang))

    (let (data-is-writable (command (append '("espeak") (list (concat greader-compile-command lang)) greader-compile-extra-parameters)))

      (with-temp-buffer
	(call-process "espeak" nil t t "--version")
	(goto-char (point-min))
	(search-forward "/")
	(setq data-is-writable (file-writable-p (thing-at-point
						 'filename))))

      (if (not data-is-writable)
	  (setq command (append '("sudo")command)))

      (make-process
       :name "greader-espeak"
       :filter #'greader-compile--filter
       :command command))))

(defun greader-compile--filter (&optional process str)
  "Filter process for sudo."
  (when (string-match "password" str)
    (process-send-string process (concat (read-passwd str) "\n")))
  (when (string-match "error" str)
    (error "%s" str)))

(defun greader-compile-guess-lang ()
  "Internal use.
This function return t if the file associated with current buffer
  seems to be an espeak dictionary definition file."

  (when (buffer-file-name)
    (let*
	((lang (buffer-file-name))

	 (start (string-match "/[[:alpha:]][[:alpha:]]_" lang))

	 end)
      (if start
	  (progn
	    (setq start (1+ start))
	    (setq end (string-match "_" lang start))
	    (substring lang start end))
	nil))))

(defun greader-check-visited-file ()
  "Internal use.
Hook for `after-save-hook'."
  (and
   (member default-directory greader-compile-dictsource)
   (greader-compile-guess-lang)
   (greader-compile)))

(defcustom greader-compile-default-source "extra"
  "Dict source file suffix to use when `greader-compile-at-point' is called.
If nil, you can not use `greader-compile-at-point'."
  :tag "greader compile default dictionary source file"
  :type 'string)

(defvar greader-espeak-language)
(defun greader-compile-at-point (&optional src dst)
  "Add a word to the espeak dictionary definition.
When called interactively and point is on a word, this function asks
  for the definition, assuming you want to define current word.
If you want to be asked about the word to define, call this command
  with prefix.
When called from a function, you should specify SRC and DST, even if
  SRC and DST are declared as optional."
  (interactive "P")
  (unless greader-compile-default-source
    (error "You must set or customize `greader-compile-default-source'"))

  (unless src
    (setq src (thing-at-point 'word t))
    (unless src
      (setq src (read-string "Word to add:"))))

  (when (listp src)
    (setq src (read-string "word to add:"))
    (if (equal src "")
	(setq src (thing-at-point 'word t))))
  (unless dst
    (setq dst (read-string (concat "Redefine " src " to: ") nil
			   'greader-compile-history)))

  (let ((lang-file
	 (if (string-prefix-p "/" greader-compile-default-source)
	     greader-compile-default-source
	   (concat (car greader-compile-dictsource) (substring greader-espeak-language 0 2) "_" greader-compile-default-source))))
    (with-current-buffer (find-file-noselect lang-file)
      (goto-char (point-max))
      (insert (concat src " " dst "\n"))
      (save-buffer)
      (unless greader-compile-mode
	(greader-compile)))))

(defun greader-compile-goto-source ()
  "Visit default dictsource currently used by `greader-compile-at-point'."
  (interactive)
  (if (string-match "/" greader-compile-default-source)
      (find-file greader-compile-default-source)
    (find-file (concat (car greader-compile-dictsource)
		       greader-espeak-language "_" greader-compile-default-source))))

(defcustom greader-backward-acoustic-feedback nil
  "If t, when point returns to the end of sentence, plays a beep."
  :tag "greader backward acoustic feedback"
  :type 'boolean)

(defcustom greader-backward-seconds 5
  "Number of seconds to wait before returning at the end of sentence."
  :tag "greader-backward seconds"
  :type 'float)

(defvar greader--marker-backward (make-marker))

(defvar greader--timer-backward nil)

(defun greader--forward ()
  (when (equal
	 (point) greader--marker-backward)
    (forward-sentence)
    (backward-char 2)
    (when greader-backward-acoustic-feedback
      (beep))))

(defun greader--set-forward-timer ()
  (setq greader--timer-backward(run-with-idle-timer greader-backward-seconds nil #'greader--forward)))

(defun greader-backward ()
  "Restart reading from start of sentence.
When at start of sentence, this function sets a timer for
  `greader--set-forward-timer' seconds, and, if the cursor is yet at
  start of sentence, puts the point at the end.
So you can use this command like a player, if you press <left> you
  will ear the start of last read sentence, and if you press again
  while timer is in effect, you go back by one sentence."
  (interactive)
  (when (bobp)
    (signal 'beginning-of-buffer ()))
  (when greader--timer-backward
    (cancel-timer greader--timer-backward)
    (setq greader--timer-backward nil))
  (greader-tts-stop)
  (backward-sentence)
  (greader-set-register)
  (setq greader--marker-backward (point))
  (greader--set-forward-timer)
  (greader-read))

(defun greader-forward ()
  "Move point to next sentence and start reading from there."
  (interactive)
  (when (eobp)
    (signal 'end-of-buffer nil))
  (greader-tts-stop)
  (greader-forward-sentence)
  (greader-set-register)
  (greader-read))

;; greader-queue-mode
;; This minor mode enables an alternative reading mode, which
;; works with blocks of text instead of the buffer in a linear fashion.

(defvar-local greader-queue nil
  "This is the variable that contains the queue.
each element is formed by a cons, whose form is 'start-position
. end-position'.")

(defvar-local greader-queue-current-element nil
  "Represent the current item in the queue (as index).")

;; Questa funzione aggiunge un elemento alla coda.
(defun greader-queue-add-element (&optional start end)
  "Add an item to the queue.
If START and END are nil, check if the region is active.
If the region is not active either, then it throws an error."
  (interactive)
  (unless (and start end)
    (if (region-active-p)
	(progn
	  (setq start (region-beginning) end (region-end))
	  (deactivate-mark))
      (user-error "Nothing to add")))
  (let ((result (greader-queue-region-overlap start end)))
    (if result
	(greader-queue-modify-element result start end)
      (add-to-list 'greader-queue (cons start end) t)))
  (unless greader-queue-current-element
    (setq greader-queue-current-element 0)))

;; This function determines whether the region overlaps with one of the
;; items in the queue.
(defun greader-queue-region-overlap (&optional start end)
  "Return the index of the element that overlaps the region.
If the region does not overlap any of the items in the queue,
then return nil."
  (unless (and start end)
    (if (region-active-p)
	(setq start (region-beginning) end (region-end))
      (user-error "Please specify a region")))
  (if greader-queue
      (let ((result nil) (counter 0))
	(dolist (item greader-queue)
	  (when (or
		 (and (>= start (car item)) (<= start (cdr item)))
		 (and (>= end (car item)) (<= end (cdr item))))
	    (setq result counter))
	  (setq counter (1+ counter)))
	result)
    nil))

;; This function modifies an item in the queue.
(defun greader-queue-modify-element (index start end)
  "Modify the item specified in INDEX with the new START and END.
This function modifies the `greader-queue' variable."
  (let ((result nil) (counter 0))
    (dolist (item greader-queue)
      (if (equal counter index)
	  (progn
	    (push (cons start end) result)
	    (setq counter (1+ counter)))
	(push item result)
	(setq counter (1+ counter))))
    (setq greader-queue (reverse result))))

;;this function resets the queue.
(defun greader-queue-reset-queue ()
  "Reset the queue and relative variables."
  (interactive)
  (setq greader-queue nil)
  (setq greader-queue-current-element nil))

;; This function removes an item from the queue.
(defun greader-queue-remove-element (&optional index)
  "Remove the item in INDEX.
If INDEX is nil, use the `greader-queue-current-element' variable."
  (interactive)
  (unless index
    (setq index greader-queue-current-element))
  (when (>= index (length greader-queue))
    (user-error "Index out of range"))
  (setq greader-queue (seq-remove-at-position greader-queue index)))

;; This function gets the text linked to an element of the
;; queue.
(defun greader-queue-get-element (&optional index)
  "return the text associated with a queue item or nil if INDEX is beyond
the limit.
If INDEX is nil, use `greader-queue-current-element'."
  (unless index
    (setq index greader-queue-current-element))
  (when (equal (length greader-queue) 0)
    (user-error "Queue is emty"))
  (let ((result nil))
    (when (< greader-queue-current-element (length greader-queue))
      (setq result (buffer-substring (car (elt greader-queue index)) (cdr (elt
									   greader-queue
									   index)))))
    result))

;; This function returns the next item in the queue. If
;; the current element is the last one, then it returns nil.
(defun greader-queue-get-next-element ()
  "return the next item in the queue.
If the current element is the last one, then return nil."
  (unless greader-queue-current-element
    (setq greader-queue-current-element 0))
  (if (equal (1+ greader-queue-current-element) (length
						 greader-queue))
      nil
    (setq greader-queue-current-element (1+ greader-queue-current-element))
    (greader-queue-get-element)))

;; This function returns the previous item in the queue, if it is
;; the first one, returns it.
(defun greader-queue-get-prev-element ()
  "Return the previous item in the queue.
If the current element is the first, it returns it."
  (unless greader-queue-current-element
    (setq greader-queue-current-element 0))
  (if (> greader-queue-current-element 0)
      (setq greader-queue-current-element (1-
					   greader-queue-current-element)))
  (greader-queue-get-element))

;; This function will act as a sentinel for the tts when invoked
;; by the `greader-queue-read' function.
(defun greader-queue-next-action (&optional process string)
  "Sentinel process for greader-queue-mode."
  (setq greader-queue-current-element (1+
				       greader-queue-current-element))
  (greader-queue-read))

;; This function is a reimplementation in salsa queue's
;; reader-read. The idea is to map it right in place of
;; greader-read in the `greader-queue-mode' keymap.
(defun greader-queue-read ()
  "Entry point for `greader-queue-mode'."
  (interactive)
  (let ((text (greader-queue-get-element)))
    (if text
	(progn
	  (setq-local greader-backend-action
		      #'greader-queue-next-action)
	  (greader-read-asynchronous text))
      (setq-local greader-backend-action #'greader--default-action)
      (greader-read-asynchronous "end of queue."))))

;; This function stops reading of queue.
(defun greader-queue-stop ()
  "stop reading of queue."
  (interactive)
  (setq greader-backend-action #'greader--default-action)
  (greader-tts-stop))
;; This function moves the point to the beginning of the previous element
;; in the queue and start reading.
(defun greader-queue-backward ()
  "Move the point to the beginning of the item in the queue and start the
reading.
If we are already at the first element, it makes a ding and does
nothing else."
  (interactive)
  (if (equal greader-queue-current-element 0)
      (ding)
    (greader-tts-stop)
    (setq greader-queue-current-element (1-
					 greader-queue-current-element))
    (greader-queue-read)))

;; This function moves the point to the next item in the queue and
;; reads it.
(defun greader-queue-forward ()
  "Move the point to the next item in the queue and read it.
If it is already the last item in the queue it beeps and does nothing
else."
  (if (equal greader-queue-current-element (- 1 (length
						 greader-queue)))
      (ding)
    (greader-tts-stop)
    (setq greader-queue-current-element (1+
					 greader-queue-current-element))
    (greader-queue-read)))

;; this is the keymap for greader-queue-mode.
(defvar-keymap greader-queue-mode-map
  :doc "greader-queue-mode map."
  "C-r RET" #'greader-queue-add-element
  "C-r SPC" #'greader-queue-read
  "C-r <left>" #'greader-queue-backward
  "C-r <right>" #'greader-queue-forward
  "C-r ." #'greader-queue-stop)
(defvar-local greader-greader-mode-was-active nil
  "This variable becomes t if `greader-mode' is active when
`greader-queue-mode' is enabled.
This is to avoid reactivating `greader-mode' if it wasn't active
before.")

;;;###autoload
(define-minor-mode greader-queue-mode
  "In this mode, text reading occurs via blocks.
normally greader reads the text of a buffer sequentially; in queue-mode you add blocks of text to a queue, so you can choose only certain parts of the buffer.
As reading progresses, further blocks can be added,
or you can add the blocks and then start reading."
  :lighter " greader-q"
  (cond
   (greader-queue-mode
    (when greader-mode
      (setq greader-greader-mode-was-active t)
      (greader-mode -1)))
   ((not greader-queue-mode)
    (when greader-greader-mode-was-active
      (greader-mode 1)))))

(defcustom greader-enriched-tag "link: "
  "Message to be spoken when clicking elements are encountered.
If the value is a string, that string will be used.
If it is a function, it must return a string."
  :type '(choice (string :tag "String")
                 (function :tag "function")))

(defun greader--get-enriched-tag ()
  "Return the value of `greader-enriched-tag'."
  (pcase greader-enriched-tag
    ((pred functionp)
     (funcall #'greader-enriched-tag))
    ((pred stringp)
     greader-enriched-tag)))

(defun greader-scrap-links (input-string)
  "Modify the INPUT-STRING string to precede each link with 'link: '"
  (let ((pos 0)
        (modified nil)
        (result (copy-sequence input-string)))
    ;; Search for the location of the next link.
    (while (setq pos (next-single-property-change pos 'mouse-face result))
      ;; Check if the location has the 'mouse-face' property set.
      (when (get-text-property pos 'mouse-face result)
        (setq modified t)
	;; Add 'link:' before the location.
        (setq result (concat (substring result 0 pos)
                             (greader--get-enriched-tag)
                             (substring result pos)))
	;; Update the search position taking into account the "link: " string we just entered.
        (setq pos (+ pos 6))))
    ;; Return the modified string if links were found.
    (if modified result nil)))

;;;###autoload
(define-minor-mode greader-enriched-mode
  "This mode causes greader to announce clickable objects.
To configure the message with which it must be announced
the element, configure the `greader-enriched-tag' variable."
  :lighter "rich"
  (if greader-enriched-mode
      (progn
	(unless greader-mode
	  (greader-mode 1))
	(add-hook 'greader-after-get-sentence-functions
		  #'greader-scrap-links))
    (remove-hook 'greader-after-get-sentence-functions
		 #'greader-scrap-links)))

;;greader-continuous-mode
;; In this mode, greader will try to "guess" the function in the
;; current major mode that allows you to scroll the page
;; next one.
;;To do this, it assumes that, usually, the command to switch to
;; next page or section in a buffer is associated with the space key.
;; However, it will be possible to specify via the variable
;; customizable `greader-continuous-modes' functions associated with
;; particular major modes.
(defcustom greader-continuous-modes
  (if (package-installed-p 'nov)
      (progn
	(require 'nov)
	'((nov-mode . nov-next-document)))
    ())
  "Alist mapping major modes to functions for greader-continuous
guessing."
  :type '(alist :key-type (symbol :tag "mode-name") 
                :value-type (function :tag "Scroll function")))

(defcustom greader-continuous-excluded-modes ()
  "Alist mapping major modes to functions for greader-continuous guessing."
  :type '(list (symbol :tag "modes")))

(defcustom greader-continuous-key "SPC"
  "The key or key sequence from which the scroll function is to be derived."
  :type 'string)

(defun greader-continuous-guess-function ()
  "Guess the function for greader-continuous mode based on GREADER-CONTINUOUS-KEY.
If GREADER-CONTINUOUS-KEY is nil, checks against `greader-continuous-excluded-modes'
and `greader-continuous-modes' to determine the appropriate function."
  (cond
   ((member major-mode greader-continuous-excluded-modes)
    ;; If the current major mode is excluded, return nil.
    nil)
   ((assoc major-mode greader-continuous-modes)
    ;; If the current major mode is included, return the associated function.
    (cdr (assoc major-mode greader-continuous-modes)))
   ;; If a key sequence is provided, get the corresponding command.
   (greader-continuous-key
    (let ((command (key-binding (kbd greader-continuous-key))))
      ;; Return nil if the command is `self-insert-command',
      ;; otherwise return the command itself.
      (unless (eq command 'self-insert-command)
        command)))
   (t
    ;; If the major mode is not mentioned in any list, return nil.
    nil)))

;; The following is the function that will be added to
;; `greader-before-finish-functions', just in case
;; `greader-continuous-mode' is enabled.
(defun greader-continuous-call-function ()
  "Call the function returned by `greader-continuous-guess-key'.
If the `greader-continuous-guess-key` function returns nil, then
this function will return nil, otherwise it returns t. It also handles
any errors that occur during the execution of the command or
the reading process, returning nil in such cases."
  (condition-case nil
      ;; The condition-case block here is to handle any errors
      ;; that might occur in any part of the following code.
      (let ((command (greader-continuous-guess-function)))
        ;; Check if a valid command is returned.
        (when command
          ;; Calls the function returned by greader-continuous-guess-function.
	  ;; We check that the point is not at the end of the buffer.
	  (when (eobp)
	    (goto-char (- (point-max) 1)))
          (funcall command)
          (greader-read))
        t)  ; Returns t if no error occurs.
    ((debug error) nil)))  ; Returns nil in case of an error.

;; greader-continuous-mode
;; This minor-mode will take care of adding to the hook
;; `greader-before-finish-functions' the function
;; `greader-continuous-call-function'.
;; If minor-mode is deactivated, the function will be removed
;; from the hook.
(define-minor-mode greader-continuous-mode
  "This minor-mode enables autoscrolling in buffers that support it
require.
Examples are `info-mode', `nov-mode', to some extent `man-mode' etc...
When this minor-mode is active, say in `info-mode', it will be
called the `info-scroll-up' function instead of finishing reading."
  :lighter " continuous"
  (if greader-continuous-mode
      (progn
	(greader-study-mode -1)
	(unless (greader-continuous-guess-function)
	  (let ((error-string
		 (concat
		  "I can't determine the function for
scroll in " (symbol-name major-mode) ".\nPlease add
this major mode to the variable `greader-continuous-modes'")))
	    (greader-continuous-mode -1)
	    (user-error "%s" error-string)))
	(add-hook 'greader-before-finish-functions
		  #'greader-continuous-call-function 0 t))
    (remove-hook 'greader-before-finish-functions
		 #'greader-continuous-call-function t)))
;; greader-study-mode
;; This is the complementary mode of "greader-continuous-mode", and
;; is used to repeatedly read the contents of a buffer or part of
;; it.

;; greader-study-restart is the function that will be added to the hook
;; `greader-before-finish-functions'.
;; The function simply returns to the beginning of the buffer or the
;; active region and runs `greader-read'.
(defun greader-study-restart ()
  "restart reading of buffer or region."
  (if greader-region-mode
      (goto-char (region-beginning))
    (goto-char (point-min)))
  (greader-read) t)

;; greader-study-mode
(define-minor-mode greader-study-mode
  "In study-mode, reading of buffer will restart continuously.
If `greader-region-mode' is enabled, restart will behave accordingly."
  :lighter " study"
  (if greader-study-mode
      (progn
	(greader-continuous-mode -1)
	(add-hook 'greader-before-finish-functions
		  #'greader-study-restart 0 t))
    (remove-hook 'greader-before-finish-functions
		 #'greader-study-restart t)))

(provide 'greader)
;;; greader.el ends here
