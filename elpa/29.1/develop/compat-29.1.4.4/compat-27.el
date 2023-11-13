;;; compat-27.el --- Functionality added in Emacs 27.1 -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

;; Functionality added in Emacs 27.1, needed by older Emacs versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-require compat-26 "26.1")

(compat-version "27.1")

;;;; Defined in fns.c

(compat-defun proper-list-p (object) ;; <compat-tests:proper-list-p>
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  (if (eval-when-compile (< emacs-major-version 26))
      ;; On older Emacs than 26.1 use Tortoise and Hare algorithm
      (when (listp object)
        (catch 'cycle
          (let ((hare object) (tortoise object)
                (max 2) (q 2))
            (while (consp hare)
              (setq hare (cdr hare))
              (when (and (or (/= 0 (setq q (1- q)))
                             (ignore
                              (setq max (ash max 1)
                                    q max
                                    tortoise hare)))
                         (eq hare tortoise))
                (throw 'cycle nil)))
            (and (null hare) (length object)))))
    ;; Errors on 26.1 and newer
    (and (listp object) (ignore-errors (length object)))))

(compat-defun string-distance (string1 string2 &optional bytecompare) ;; <compat-tests:string-distance>
  "Return Levenshtein distance between STRING1 and STRING2.
The distance is the number of deletions, insertions, and substitutions
required to transform STRING1 into STRING2.
If BYTECOMPARE is nil or omitted, compute distance in terms of characters.
If BYTECOMPARE is non-nil, compute distance in terms of bytes.
Letter-case is significant, but text properties are ignored."
  ;; https://en.wikipedia.org/wiki/Levenshtein_distance
  (let ((s1 (if bytecompare
                (encode-coding-string string1 'raw-text)
              (concat string1 "")))
        (s2 (if bytecompare
                (encode-coding-string string2 'raw-text)
              string2)))
    (let* ((len1 (length s1))
           (len2 (length s2))
           (column (make-vector (1+ len1) 0)))
      (dotimes (y len1)
        (setf (aref column (1+ y)) y))
      (dotimes (x len2)
        (setf (aref column 0) (1+ x))
        (let ((lastdiag x) olddiag)
          (dotimes (y len1)
            (setf olddiag (aref column (1+ y))
                  (aref column (1+ y))
                  (min (+ (if (= (aref s1 y) (aref s2 x)) 0 1)
                          lastdiag)
                       (1+ (aref column (1+ y)))
                       (1+ (aref column y)))
                  lastdiag olddiag))))
      (aref column len1))))

;;;; Defined in window.c

(compat-defun recenter (&optional arg redisplay) ;; <compat-tests:recenter>
  "Handle optional argument REDISPLAY."
  :extended t
  (recenter arg)
  (when (and redisplay recenter-redisplay)
    (redisplay)))

;;;; Defined in keymap.c

(compat-defun lookup-key (keymap key &optional accept-default) ;; <compat-tests:lookup-key>
  "Allow for KEYMAP to be a list of keymaps."
  :extended t
  (cond
   ((keymapp keymap)
    (lookup-key keymap key accept-default))
   ((listp keymap)
    (catch 'found
      (dolist (map keymap)
        (when-let ((fn (lookup-key map key accept-default)))
          (throw 'found fn)))))
   ((signal 'wrong-type-argument (list 'keymapp keymap)))))

;;;; Defined in timefns.c

(compat-defun time-equal-p (t1 t2) ;; <compat-tests:time-equal-p>
  "Return non-nil if time value T1 is equal to time value T2.
A nil value for either argument stands for the current time.

NOTE: This function is not as accurate as the actual `time-equal-p'."
  (cond
   ((eq t1 t2))
   ((and (consp t1) (consp t2))
    (equal t1 t2))
   (t
    ;; Due to inaccuracies and the relatively slow evaluating of
    ;; Emacs Lisp compared to C, we allow for slight inaccuracies
    ;; (less than a millisecond) when comparing time values.
    (< (abs (- (float-time t1) (float-time t2)))
       (if (and t1 t2) 1e-6 1e-5)))))

;;;; Defined in subr.el

(compat-defalias fixnump integerp) ;; <compat-tests:fixnump>
(compat-defalias bignump ignore) ;; <compat-tests:bignump>

(compat-defmacro setq-local (&rest pairs) ;; <compat-tests:setq-local>
  "Handle multiple assignments."
  :extended t
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let (body)
    (while pairs
      (let* ((sym (pop pairs))
             (val (pop pairs)))
        (unless (symbolp sym)
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        (push `(set (make-local-variable ',sym) ,val)
              body)))
    (cons 'progn (nreverse body))))

(compat-defmacro ignore-error (condition &rest body) ;; <compat-tests:ignore-error>
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

(compat-defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body) ;; <compat-tests:dolist-with-progress-reporter>
  "Loop over a list and report progress in the echo area.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((prep (make-symbol "--dolist-progress-reporter--"))
        (count (make-symbol "--dolist-count--"))
        (list (make-symbol "--dolist-list--")))
    `(let ((,prep ,reporter-or-message)
           (,count 0)
           (,list ,(cadr spec)))
       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 (length ,list))))
       (dolist (,(car spec) ,list)
         ,@body
         (progress-reporter-update ,prep (setq ,count (1+ ,count))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))

(compat-defun flatten-tree (tree) ;; <compat-tests:flatten-tree>
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(compat-defun xor (cond1 cond2) ;; <compat-tests:xor>
  "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
  (declare (pure t) (side-effect-free error-free))
  (cond ((not cond1) cond2)
        ((not cond2) cond1)))

(compat-defvar regexp-unmatchable "\\`a\\`" ;; <compat-tests:regexp-unmatchable>
  "Standard regexp guaranteed not to match any string at all."
  :constant t)

(compat-defun assoc-delete-all (key alist &optional test) ;; <compat-tests:assoc-delete-all>
  "Handle optional argument TEST."
  :extended "26.2"
  (unless test (setq test #'equal))
  (while (and (consp (car alist))
              (funcall test (caar alist) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (funcall test (caar tail-cdr) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(compat-defvar major-mode--suspended nil ;; <compat-tests:major-mode-suspend>
  "Suspended major mode."
  :local permanent)

(compat-defun major-mode-suspend () ;; <compat-tests:major-mode-suspend>
  "Exit current major mode, remembering it."
  (let* ((prev-major-mode (or major-mode--suspended
                              (unless (eq major-mode 'fundamental-mode)
                                major-mode))))
    (kill-all-local-variables)
    (setq-local major-mode--suspended prev-major-mode)))

(compat-defun major-mode-restore (&optional avoided-modes) ;; <compat-tests:major-mode-suspend>
  "Restore major mode earlier suspended with `major-mode-suspend'.
If there was no earlier suspended major mode, then fallback to `normal-mode',
though trying to avoid AVOIDED-MODES."
  (if major-mode--suspended
      (funcall (prog1 major-mode--suspended
                 (kill-local-variable 'major-mode--suspended)))
    (let ((auto-mode-alist
           (let ((alist (copy-sequence auto-mode-alist)))
             (dolist (mode avoided-modes)
               (setq alist (rassq-delete-all mode alist)))
             alist))
          (magic-fallback-mode-alist
           (let ((alist (copy-sequence magic-fallback-mode-alist)))
             (dolist (mode avoided-modes)
               (setq alist (rassq-delete-all mode alist)))
             alist)))
      (normal-mode))))

(compat-defun read-char-from-minibuffer-insert-char () ;; <compat-tests:read-char-from-minibuffer>
  "Insert the character you type into the minibuffer and exit minibuffer.
Discard all previous input before inserting and exiting the minibuffer."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert last-command-event)
    (exit-minibuffer)))

(compat-defun read-char-from-minibuffer-insert-other () ;; <compat-tests:read-char-from-minibuffer>
  "Reject a disallowed character typed into the minibuffer.
This command is intended to be bound to keys that users are not
allowed to type into the minibuffer.  When the user types any
such key, this command discard all minibuffer input and displays
an error message."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (ding)
    (discard-input)
    (minibuffer-message "Wrong answer")
    (sit-for 2)))

(compat-defvar read-char-history nil ;; <compat-tests:read-char-from-minibuffer>
  "The default history for the `read-char-from-minibuffer' function.")

(compat-defvar read-char-from-minibuffer-map ;; <compat-tests:read-char-from-minibuffer>
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [remap self-insert-command] #'read-char-from-minibuffer-insert-char)
    (define-key map [remap exit-minibuffer] #'read-char-from-minibuffer-insert-other)
    map)
  "Keymap for the `read-char-from-minibuffer' function.")

(compat-defvar read-char-from-minibuffer-map-hash  ;; <compat-tests:read-char-from-minibuffer>
  (make-hash-table :test 'equal)
  "Hash table of keymaps used by `read-char-from-minibuffer'."
  :constant t)

(compat-defun read-char-from-minibuffer (prompt &optional chars history) ;; <compat-tests:read-char-from-minibuffer>
  "Read a character from the minibuffer, prompting for it with PROMPT.
Like `read-char', but uses the minibuffer to read and return a character.
Optional argument CHARS, if non-nil, should be a list of characters;
the function will ignore any input that is not one of CHARS.
Optional argument HISTORY, if non-nil, should be a symbol that
specifies the history list variable to use for navigating in input
history using \\`M-p' and \\`M-n', with \\`RET' to select a character from
history.
If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result.
There is no need to explicitly add `help-char' to CHARS;
`help-char' is bound automatically to `help-form-show'."
  (let* ((map (if (consp chars)
                  (or (gethash (list help-form (cons help-char chars))
                               read-char-from-minibuffer-map-hash)
                      (let ((map (make-sparse-keymap))
                            (msg help-form))
                        (set-keymap-parent map read-char-from-minibuffer-map)
                        ;; If we have a dynamically bound `help-form'
                        ;; here, then the `C-h' (i.e., `help-char')
                        ;; character should output that instead of
                        ;; being a command char.
                        (when help-form
                          (define-key map (vector help-char)
                            (lambda ()
                              (interactive)
                              (let ((help-form msg)) ; lexically bound msg
                                (help-form-show)))))
                        (dolist (char chars)
                          (define-key map (vector char)
                            #'read-char-from-minibuffer-insert-char))
                        (define-key map [remap self-insert-command]
                          #'read-char-from-minibuffer-insert-other)
                        (puthash (list help-form (cons help-char chars))
                                 map read-char-from-minibuffer-map-hash)
                        map))
                read-char-from-minibuffer-map))
         ;; Protect this-command when called from pre-command-hook (bug#45029)
         (this-command this-command)
         (result (read-from-minibuffer prompt nil map nil (or history t)))
         (char
          (if (> (length result) 0)
              ;; We have a string (with one character), so return the first one.
              (elt result 0)
            ;; The default value is RET.
            (when history (push "\r" (symbol-value history)))
            ?\r)))
    ;; Display the question with the answer.
    (message "%s%s" prompt (char-to-string char))
    char))

;;;; Defined in simple.el

(compat-guard (not (fboundp 'decoded-time-second)) ;; <compat-tests:decoded-time>
  (cl-defstruct (decoded-time
                 (:constructor nil)
                 (:copier nil)
                 (:type list))
    (second nil :documentation "\
This is an integer or a Lisp timestamp (TICKS . HZ) representing a nonnegative
number of seconds less than 61.  (If not less than 60, it is a leap second,
which only some operating systems support.)")
    (minute nil :documentation "This is an integer between 0 and 59 (inclusive).")
    (hour nil :documentation "This is an integer between 0 and 23 (inclusive).")
    (day nil :documentation "This is an integer between 1 and 31 (inclusive).")
    (month nil :documentation "\
This is an integer between 1 and 12 (inclusive).  January is 1.")
    (year nil :documentation "This is a four digit integer.")
    (weekday nil :documentation "\
This is a number between 0 and 6, and 0 is Sunday.")
    (dst -1 :documentation "\
This is t if daylight saving time is in effect, nil if it is not
in effect, and -1 if daylight saving information is not available.
Also see `decoded-time-dst'.")
    (zone nil :documentation "\
This is an integer indicating the UTC offset in seconds, i.e.,
the number of seconds east of Greenwich.")))

(compat-defun minibuffer-history-value () ;; <compat-tests:minibuffer-history-value>
  "Return the value of the minibuffer input history list.
If `minibuffer-history-variable' points to a buffer-local variable and
the minibuffer is active, return the buffer-local value for the buffer
that was current when the minibuffer was activated."
  (buffer-local-value minibuffer-history-variable
                      (window-buffer (minibuffer-selected-window))))

;;;; Defined in minibuffer.el

(compat-defmacro with-minibuffer-selected-window (&rest body) ;; <compat-tests:with-minibuffer-selected-window>
  "Execute the forms in BODY from the minibuffer in its original window.
When used in a minibuffer window, select the window selected just before
the minibuffer was activated, and execute the forms."
  (declare (indent 0) (debug t))
  `(when-let ((window (minibuffer-selected-window)))
     (with-selected-window window
       ,@body)))

;;;; Defined in byte-run.el

(compat-defmacro with-suppressed-warnings (_warnings &rest body) ;; <compat-tests:with-suppressed-warnings>
  "Like `progn', but prevents compiler WARNINGS in BODY.
NOTE: The compatibility version behaves like `with-no-warnings'."
  `(with-no-warnings ,@body))

;;;; Defined in image.el

(compat-defun image--set-property (image property value) ;; <compat-tests:image-property>
  "Set PROPERTY in IMAGE to VALUE, internal use only."
  :extended "26.1"
  :feature image
  (if (null value)
      (while (cdr image)
        (if (eq (cadr image) property)
            (setcdr image (cdddr image))
          (setq image (cddr image))))
    (setcdr image (plist-put (cdr image) property value)))
  value)

;; HACK: image--set-property was broken with an off-by-one error on Emacs 26.
;; The bug was fixed in a4ad7bed187493c1c230f223b52c71f5c34f7c89. Therefore we
;; override the gv expander until Emacs 27.1.
(compat-guard ;; <compat-tests:image-property>
    (or (= emacs-major-version 26) (not (get 'image-property 'gv-expander)))
  :feature image
  (gv-define-setter image-property (value image prop)
    `(,(if (< emacs-major-version 26) 'image--set-property 'compat--image--set-property)
      ,image ,prop ,value)))

;;;; Defined in files.el

(compat-defun file-name-quoted-p (name &optional top) ;; <compat-tests:file-name-quoted-p>
  "Handle optional argument TOP."
  :extended "26.1"
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (string-prefix-p "/:" (file-local-name name))))

(compat-defun file-name-quote (name &optional top) ;; <compat-tests:file-name-quote>
  "Handle optional argument TOP."
  :extended "26.1"
  (let* ((file-name-handler-alist (unless top file-name-handler-alist))
         (localname (file-local-name name)))
    (if (string-prefix-p "/:" localname)
        name
      (concat (file-remote-p name) "/:" localname))))

(compat-defun file-name-unquote (name &optional top) ;; <compat-tests:file-name-unquote>
  "Handle optional argument TOP."
  :extended "26.1"
  (let* ((file-name-handler-alist (unless top file-name-handler-alist))
         (localname (file-local-name name)))
    (when (string-prefix-p "/:" localname)
      (setq localname (if (= (length localname) 2) "/" (substring localname 2))))
    (concat (file-remote-p name) localname)))

(compat-defun file-size-human-readable (file-size &optional flavor space unit) ;; <compat-tests:file-size-human-readable>
  "Handle the optional arguments SPACE and UNIT."
  :extended t
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                 1000.0))
        (prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr prefixes))
      (setq file-size (/ file-size power)
            prefixes (cdr prefixes)))
    (let* ((prefix (car prefixes))
           (prefixed-unit (if (eq flavor 'iec)
                              (concat
                               (if (string= prefix "k") "K" prefix)
                               (if (string= prefix "") "" "i")
                               (or unit "B"))
                            (concat prefix unit))))
      (format (if (and (>= (mod file-size 1.0) 0.05)
                       (< (mod file-size 1.0) 0.95))
                  "%.1f%s%s"
                "%.0f%s%s")
              file-size
              (if (string= prefixed-unit "") "" (or space ""))
              prefixed-unit))))

(compat-defun file-size-human-readable-iec (size) ;; <compat-tests:file-size-human-readable-iec>
  "Human-readable string for SIZE bytes, using IEC prefixes."
  (compat--file-size-human-readable size 'iec " "))

(compat-defun exec-path () ;; <compat-tests:exec-path>
  "Return list of directories to search programs to run in remote subprocesses.
The remote host is identified by `default-directory'.  For remote
hosts that do not support subprocesses, this returns nil.
If `default-directory' is a local directory, this function returns
the value of the variable `exec-path'."
  (let ((handler (find-file-name-handler default-directory 'exec-path)))
    ;; NOTE: The handler may fail since it was added in 27.1.
    (or (and handler (ignore-errors (funcall handler 'exec-path)))
        (if (file-remote-p default-directory)
            ;; FIXME: Just return some standard path on remote
            '("/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin")
          exec-path))))

(compat-defun executable-find (command &optional remote) ;; <compat-tests:executable-find>
  "Handle optional argument REMOTE."
  :extended t
  (if (and remote (file-remote-p default-directory))
      (let ((res (locate-file
                  command
                  (mapcar
                   (apply-partially
                    #'concat (file-remote-p default-directory))
                   (exec-path))
                  exec-suffixes 'file-executable-p)))
        (when (stringp res) (file-local-name res)))
    (executable-find command)))

(compat-defun make-empty-file (filename &optional parents) ;; <compat-tests:make-empty-file>
  "Create an empty file FILENAME.
Optional arg PARENTS, if non-nil then creates parent dirs as needed."
  (when (and (file-exists-p filename) (null parents))
    (signal 'file-already-exists (list "File exists" filename)))
  (let ((paren-dir (file-name-directory filename)))
    (when (and paren-dir (not (file-exists-p paren-dir)))
      (make-directory paren-dir parents)))
  (write-region "" nil filename nil 0))

;;;; Defined in regexp-opt.el

(compat-defun regexp-opt (strings &optional paren) ;; <compat-tests:regexp-opt>
  "Handle an empty list of STRINGS."
  :extended t
  (if (null strings)
      (let ((re "\\`a\\`"))
        (cond ((null paren)
               (concat "\\(?:" re "\\)"))
              ((stringp paren)
               (concat paren re "\\)"))
              ((eq paren 'words)
               (concat "\\<\\(" re "\\)\\>"))
              ((eq paren 'symbols)
               (concat "\\_\\(<" re "\\)\\_>"))
              ((concat "\\(" re "\\)"))))
    (regexp-opt strings paren)))

;;;; Defined in package.el

(declare-function lm-header "lisp-mnt")
(declare-function macroexp-file-name nil)

(compat-defun package-get-version () ;; <compat-tests:package-get-version>
  "Return the version number of the package in which this is used.
Assumes it is used from an Elisp file placed inside the top-level directory
of an installed ELPA package.
The return value is a string (or nil in case we can’t find it)."
  ;; No :feature since the function is autoloaded.
  ;; In a sense, this is a lie, but it does just what we want: precompute
  ;; the version at compile time and hardcodes it into the .elc file!
  (declare (pure t))
  ;; Hack alert!
  (let ((file (or (macroexp-file-name) buffer-file-name)))
    (cond
     ((null file) nil)
     ;; Packages are normally installed into directories named "<pkg>-<vers>",
     ;; so get the version number from there.
     ((string-match
       "/[^/]+-\\([0-9]\\(?:[0-9.]\\|pre\\|beta\\|alpha\\|snapshot\\)+\\)/[^/]+\\'"
       file)
      (match-string 1 file))
     ;; For packages run straight from the an elpa.git clone, there's no
     ;; "-<vers>" in the directory name, so we have to fetch the version
     ;; the hard way.
     ((let* ((pkgdir (file-name-directory file))
             (pkgname (file-name-nondirectory (directory-file-name pkgdir)))
             (mainfile (expand-file-name (concat pkgname ".el") pkgdir)))
        (when (file-readable-p mainfile)
          (require 'lisp-mnt)
          (with-temp-buffer
            (insert-file-contents mainfile)
            (or (lm-header "package-version")
                (lm-header "version")))))))))

;;;; Defined in time-date.el

(compat-defun make-decoded-time ;; <compat-tests:make-decoded-time>
              (&key second minute hour day month year (dst -1) zone)
  "Return a `decoded-time' structure with only the keywords given filled out."
  :feature time-date
  (list second minute hour day month year nil dst zone))

(compat-defun date-days-in-month (year month) ;; <compat-tests:date-days-in-month>
  "The number of days in MONTH in YEAR."
  :feature time-date
  (unless (and (numberp month)
               (<= 1 month)
               (<= month 12))
    (error "Month %s is invalid" month))
  (if (= month 2)
      (if (date-leap-year-p year)
          29
        28)
    (if (memq month '(1 3 5 7 8 10 12))
        31
      30)))

(compat-defun date-ordinal-to-time (year ordinal) ;; <compat-tests:date-ordinal-to-time>
  "Convert a YEAR/ORDINAL to the equivalent `decoded-time' structure.
ORDINAL is the number of days since the start of the year, with
January 1st being 1."
  (let ((month 1))
    (while (> ordinal (date-days-in-month year month))
      (setq ordinal (- ordinal (date-days-in-month year month))
            month (1+ month)))
    (list nil nil nil ordinal month year nil nil nil)))

;;;; Defined in text-property-search.el

(declare-function make-prop-match nil)
(compat-guard (not (fboundp 'make-prop-match)) ;; <compat-tests:prop-match>
  (cl-defstruct (prop-match) beginning end value))

(compat-defun text-property-search-forward ;; <compat-tests:text-property-search-forward>
    (property &optional value predicate not-current)
  "Search for the next region of text where PREDICATE is true.
PREDICATE is used to decide whether a value of PROPERTY should be
considered as matching VALUE.

If PREDICATE is a function, it will be called with two arguments:
VALUE and the value of PROPERTY.  The function should return
non-nil if these two values are to be considered a match.

Two special values of PREDICATE can also be used:
If PREDICATE is t, that means a value must `equal' VALUE to be
considered a match.
If PREDICATE is nil (which is the default value), a value will
match if is not `equal' to VALUE.  Furthermore, a nil PREDICATE
means that the match region is ended if the value changes.  For
instance, this means that if you loop with

  (while (setq prop (text-property-search-forward \\='face))
    ...)

you will get all distinct regions with non-nil `face' values in
the buffer, and the `prop' object will have the details about the
match.  See the manual for more details and examples about how
VALUE and PREDICATE interact.

If NOT-CURRENT is non-nil, the function will search for the first
region that doesn't include point and has a value of PROPERTY
that matches VALUE.

If no matches can be found, return nil and don't move point.
If found, move point to the end of the region and return a
`prop-match' object describing the match.  To access the details
of the match, use `prop-match-beginning' and `prop-match-end' for
the buffer positions that limit the region, and
`prop-match-value' for the value of PROPERTY in the region."
  (let* ((match-p
          (lambda (prop-value)
            (funcall
             (cond
              ((eq predicate t)
               #'equal)
              ((eq predicate nil)
               (lambda (val p-val)
                 (not (equal val p-val))))
              (predicate))
             value prop-value)))
         (find-end
          (lambda (start)
            (let (end)
              (if (and value
                       (null predicate))
                  ;; This is the normal case: We're looking for areas where the
                  ;; values aren't, so we aren't interested in sub-areas where the
                  ;; property has different values, all non-matching value.
                  (let ((ended nil))
                    (while (not ended)
                      (setq end (next-single-property-change (point) property))
                      (if (not end)
                          (progn
                            (goto-char (point-max))
                            (setq end (point)
                                  ended t))
                        (goto-char end)
                        (unless (funcall match-p (get-text-property (point) property))
                          (setq ended t)))))
                ;; End this at the first place the property changes value.
                (setq end (next-single-property-change (point) property nil (point-max)))
                (goto-char end))
              (make-prop-match
               :beginning start
               :end end
               :value (get-text-property start property))))))
    (cond
     ;; No matches at the end of the buffer.
     ((eobp)
      nil)
     ;; We're standing in the property we're looking for, so find the
     ;; end.
     ((and (funcall match-p (get-text-property (point) property))
           (not not-current))
      (funcall find-end (point)))
     (t
      (let ((origin (point))
            (ended nil)
            pos)
        ;; Find the next candidate.
        (while (not ended)
          (setq pos (next-single-property-change (point) property))
          (if (not pos)
              (progn
                (goto-char origin)
                (setq ended t))
            (goto-char pos)
            (if (funcall match-p (get-text-property (point) property))
                (setq ended (funcall find-end (point)))
              ;; Skip past this section of non-matches.
              (setq pos (next-single-property-change (point) property))
              (unless pos
                (goto-char origin)
                (setq ended t)))))
        (and (not (eq ended t))
             ended))))))

(compat-defun text-property-search-backward ;; <compat-tests:text-property-search-backward>
    (property &optional value predicate not-current)
  "Search for the previous region of text whose PROPERTY matches VALUE.

Like `text-property-search-forward', which see, but searches backward,
and if a matching region is found, place point at the start of the region."
  (let* ((match-p
          (lambda (prop-value)
            (funcall
             (cond
              ((eq predicate t)
               #'equal)
              ((eq predicate nil)
               (lambda (val p-val)
                 (not (equal val p-val))))
              (predicate))
             value prop-value)))
         (find-end
          (lambda (start)
            (let (end)
              (if (and value
                       (null predicate))
                  ;; This is the normal case: We're looking for areas where the
                  ;; values aren't, so we aren't interested in sub-areas where the
                  ;; property has different values, all non-matching value.
                  (let ((ended nil))
                    (while (not ended)
                      (setq end (previous-single-property-change (point) property))
                      (if (not end)
                          (progn
                            (goto-char (point-min))
                            (setq end (point)
                                  ended t))
                        (goto-char (1- end))
                        (unless (funcall match-p (get-text-property (point) property))
                          (goto-char end)
                          (setq ended t)))))
                ;; End this at the first place the property changes value.
                (setq end (previous-single-property-change
                           (point) property nil (point-min)))
                (goto-char end))
              (make-prop-match
               :beginning end
               :end (1+ start)
               :value (get-text-property end property))))))
    (cond
     ;; We're at the start of the buffer; no previous matches.
     ((bobp)
      nil)
     ;; We're standing in the property we're looking for, so find the
     ;; end.
     ((funcall match-p (get-text-property (1- (point)) property))
      (let ((origin (point))
            (match (funcall find-end (1- (point)) property value predicate)))
        ;; When we want to ignore the current element, then repeat the
        ;; search if we haven't moved out of it yet.
        (if (and not-current
                 (equal (get-text-property (point) property)
                        (get-text-property origin property)))
            (text-property-search-backward property value predicate)
          match)))
     (t
      (let ((origin (point))
            (ended nil)
            pos)
        ;; Find the previous candidate.
        (while (not ended)
          (setq pos (previous-single-property-change (point) property))
          (if (not pos)
              (progn
                (goto-char origin)
                (setq ended t))
            (goto-char (1- pos))
            (if (funcall match-p (get-text-property (point) property))
                (setq ended
                      (funcall find-end (point)))
              ;; Skip past this section of non-matches.
              (setq pos (previous-single-property-change (point) property))
              (unless pos
                (goto-char origin)
                (setq ended t)))))
        (and (not (eq ended t))
             ended))))))

;;;; Defined in ring.el

(compat-defun ring-resize (ring size) ;; <compat-tests:ring-resize>
  "Set the size of RING to SIZE.
If the new size is smaller, then the oldest items in the ring are
discarded."
  :feature ring
  (when (integerp size)
    (let ((length (ring-length ring))
          (new-vec (make-vector size nil)))
      (if (= length 0)
          (setcdr ring (cons 0 new-vec))
        (let* ((hd (car ring))
               (old-size (ring-size ring))
               (old-vec (cddr ring))
               (copy-length (min size length))
               (copy-hd (mod (+ hd (- length copy-length)) length)))
          (setcdr ring (cons copy-length new-vec))
          ;; If the ring is wrapped, the existing elements must be written
          ;; out in the right order.
          (dotimes (j copy-length)
            (aset new-vec j (aref old-vec (mod (+ copy-hd j) old-size))))
          (setcar ring 0))))))

;;;; Defined in map-ynp.el

(compat-version "26.2")

(compat-defvar read-answer-short 'auto ;; <compat-tests:read-answer>
  "If non-nil, the `read-answer' function accepts single-character answers.
If t, accept short (single key-press) answers to the question.
If nil, require long answers.  If `auto', accept short answers if
`use-short-answers' is non-nil, or the function cell of `yes-or-no-p'
is set to `y-or-n-p'.

Note that this variable does not affect calls to the more
commonly-used `yes-or-no-p' function; it only affects calls to
the `read-answer' function.  To control whether `yes-or-no-p'
requires a long or a short answer, see the `use-short-answers'
variable.")

(compat-defun read-answer (question answers) ;; <compat-tests:read-answer>
  "Read an answer either as a complete word or its character abbreviation.
Ask user a question and accept an answer from the list of possible answers.

QUESTION should end in a space; this function adds a list of answers to it.

ANSWERS is an alist with elements in the following format:
  (LONG-ANSWER SHORT-ANSWER HELP-MESSAGE)
where
  LONG-ANSWER is a complete answer,
  SHORT-ANSWER is an abbreviated one-character answer,
  HELP-MESSAGE is a string describing the meaning of the answer.

SHORT-ANSWER is not necessarily a single character answer.  It can be
also a function key like F1, a character event such as C-M-h, or
a control character like C-h.

Example:
  \\='((\"yes\"  ?y \"perform the action\")
    (\"no\"   ?n \"skip to the next\")
    (\"all\"  ?! \"accept all remaining without more questions\")
    (\"help\" ?h \"show help\")
    (\"quit\" ?q \"exit\"))

When `read-answer-short' is non-nil, accept short answers.

Return a long answer even in case of accepting short ones.

When `use-dialog-box' is t, pop up a dialog window to get user input."
  ;; NOTE: For simplicity we provide a primitive implementation based on
  ;; `read-multiple-choice', which does neither support long answers nor the the
  ;; gui dialog box.
  (cadr (read-multiple-choice
         (string-trim-right question)
         (delq nil
               (mapcar (lambda (x) (unless (equal "help" (car x))
                                     (list (cadr x) (car x) (caddr x))))
                       answers)))))

(provide 'compat-27)
;;; compat-27.el ends here
