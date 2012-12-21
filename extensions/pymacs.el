;;; pymacs.el --- Interface between Emacs Lisp and Python

;; Copyright © 2001, 2002, 2003, 2012 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; Created: 2001
;; Version: 0.25
;; Keywords: Python interface protocol

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary:

;; Pymacs is a powerful tool which, once started from Emacs, allows
;; both-way communication between Emacs Lisp and Python.  Pymacs aims
;; Python as an extension language for Emacs rather than the other way
;; around.  Visit http://pymacs.progiciels-bpi.ca to read its manual,
;; which also contains installation instructions.

;;; Code:

;; The code is organized into pages, grouping declarations by topic.
;; Such pages are introduced by a form feed and a topic description.

;;; Portability stunts.

(defvar pymacs-use-hash-tables
  (and (fboundp 'make-hash-table) (fboundp 'gethash) (fboundp 'puthash))
  "Set to t if hash tables are available.")

(eval-and-compile

  ;; pymacs-cancel-timer
  (defalias 'pymacs-cancel-timer
    (cond ((fboundp 'cancel-timer) 'cancel-timer)
          ;; XEmacs case - yet having post-gc-hook, this is unused.
          ((fboundp 'delete-itimer) 'delete-itimer)
          (t 'ignore)))

  ;; pymacs-kill-without-query
  (if (fboundp 'set-process-query-on-exit-flag)
      (defun pymacs-kill-without-query (process)
        "Tell recent Emacs how to quickly destroy PROCESS while exiting."
        (set-process-query-on-exit-flag process nil))
    (defalias 'pymacs-kill-without-query
      (if (fboundp 'process-kill-without-query-process)
          'process-kill-without-query-process
        'ignore)))

  ;; pymacs-multibyte-string-p
  (cond ((fboundp 'multibyte-string-p)
         (defalias 'pymacs-multibyte-string-p 'multibyte-string-p))
        ((fboundp 'find-charset-string)
         (defun pymacs-multibyte-string-p (string)
           "Tell XEmacs if STRING should be handled as multibyte."
           (not (member (find-charset-string string) '(nil (ascii))))))
        (t
         ;; Tell XEmacs that STRING is unibyte, when Mule is not around!
         (defalias 'pymacs-multibyte-string-p 'ignore)))

  ;; pymacs-report-error
  (defalias 'pymacs-report-error (symbol-function 'error))

  ;; pymacs-set-buffer-multibyte
  (if (fboundp 'set-buffer-multibyte)
      (defalias 'pymacs-set-buffer-multibyte 'set-buffer-multibyte)
    (defun pymacs-set-buffer-multibyte (flag)
      "For use in Emacs 20.2 or earlier.  Under XEmacs: no operation."
      (setq enable-multibyte-characters flag)))

  ;; pymacs-timerp
  (defalias 'pymacs-timerp
    (cond ((fboundp 'timerp) 'timerp)
          ;; XEmacs case - yet having post-gc-hook, this is unused.
          ((fboundp 'itimerp) 'itimerp)
          (t 'ignore)))

  )

;;; Published variables and functions.

(defvar pymacs-python-command "python"
  "Shell command used to start Python interpreter.")

(defvar pymacs-load-path nil
  "List of additional directories to search for Python modules.
The directories listed will be searched first, in the order given.")

(defvar pymacs-trace-transit '(5000 . 30000)
  "Keep the communication buffer growing, for debugging.
When this variable is nil, the `*Pymacs*' communication buffer gets erased
before each communication round-trip.  Setting it to `t' guarantees that
the full communication is saved, which is useful for debugging.
It could also be given as (KEEP . LIMIT): whenever the buffer exceeds LIMIT
bytes, it is reduced to approximately KEEP bytes.")

(defvar pymacs-forget-mutability nil
  "Transmit copies to Python instead of Lisp handles, as much as possible.
When this variable is nil, most mutable objects are transmitted as handles.
This variable is meant to be temporarily rebound to force copies.")

(defvar pymacs-mutable-strings nil
  "Prefer transmitting Lisp strings to Python as handles.
When this variable is nil, strings are transmitted as copies, and the
Python side thus has no way for modifying the original Lisp strings.
This variable is ignored whenever `forget-mutability' is set.")

(defvar pymacs-timeout-at-start 30
  "Maximum reasonable time, in seconds, for starting the Pymacs helper.
A machine should be pretty loaded before one needs to increment this.")

(defvar pymacs-timeout-at-reply 5
  "Expected maximum time, in seconds, to get the first line of a reply.
The status of the Pymacs helper is checked at every such timeout.")

(defvar pymacs-timeout-at-line 2
  "Expected maximum time, in seconds, to get another line of a reply.
The status of the Pymacs helper is checked at every such timeout.")

(defvar pymacs-auto-restart 'ask
  "Should the Pymacs helper be restarted whenever it dies?
Possible values are nil, t or ask.")

(defvar pymacs-dreadful-zombies nil
  "If zombies should trigger hard errors, whenever they get called.
If `nil', calling a zombie will merely produce a diagnostic message.")

;;;###autoload
(defun pymacs-load (module &optional prefix noerror)
  "Import the Python module named MODULE into Emacs.
Each function in the Python module is made available as an Emacs function.
The Lisp name of each function is the concatenation of PREFIX with
the Python name, in which underlines are replaced by dashes.  If PREFIX is
not given, it defaults to MODULE followed by a dash.
If NOERROR is not nil, do not raise error when the module is not found."
  (interactive
   (let* ((module (read-string "Python module? "))
          (default (concat (car (last (split-string module "\\."))) "-"))
          (prefix (read-string (format "Prefix? [%s] " default)
                               nil nil default)))
     (list module prefix)))
  (message "Pymacs loading %s..." module)
  (let ((lisp-code (pymacs-call "pymacs_load_helper" module prefix)))
    (cond (lisp-code (let ((result (eval lisp-code)))
                       (message "Pymacs loading %s...done" module)
                       result))
          (noerror (message "Pymacs loading %s...failed" module) nil)
          (t (pymacs-report-error "Pymacs loading %s...failed" module)))))

;;;###autoload
(defun pymacs-autoload (function module &optional prefix docstring interactive)
  "Pymacs's equivalent of the standard emacs facility `autoload'.
Define FUNCTION to autoload from Python MODULE using PREFIX.
If PREFIX is not given, it defaults to MODULE followed by a dash.
Optional DOCSTRING documents FUNCTION until it gets loaded.
INTERACTIVE is normally the argument to the function `interactive',
t means `interactive' without arguments, nil means not interactive,
which is the default."
  (unless (symbolp function)
    (error "`%s' should be a symbol" function))
  (unless (stringp module)
    (error "`%s' should be a string" module))
  (unless (pymacs-python-reference function)

    (defalias function
      `(lambda (&rest args)
         ,(or docstring
              (format "Function `%s' to be loaded from Python module `%s'"
                      function module))
         ,(cond ((eq interactive t) '(interactive))
                (interactive `(interactive ,interactive)))
         (pymacs-load ,module ,prefix)
         (unless (pymacs-python-reference ',function)
           (error "Pymacs autoload failed to define function %s" ',function))
         (apply ',function args)))))

;;;###autoload
(defun pymacs-eval (text)
  "Compile TEXT as a Python expression, and return its value."
  (interactive "sPython expression? ")
  (let ((value (pymacs-serve-until-reply "eval" `(princ ,text))))
    (when (interactive-p)
      (message "%S" value))
    value))

;;;###autoload
(defun pymacs-exec (text)
  "Compile and execute TEXT as a sequence of Python statements.
This functionality is experimental, and does not appear to be useful."
  (interactive "sPython statements? ")
  (let ((value (pymacs-serve-until-reply "exec" `(princ ,text))))
    (when (interactive-p)
      (message "%S" value))
    value))

;;;###autoload
(defun pymacs-call (function &rest arguments)
  "Return the result of calling a Python function FUNCTION over ARGUMENTS.
FUNCTION is a string denoting the Python function, ARGUMENTS are separate
Lisp expressions, one per argument.  Immutable Lisp constants are converted
to Python equivalents, other structures are converted into Lisp handles."
  (pymacs-serve-until-reply
   "eval" `(pymacs-print-for-apply ',function ',arguments)))

;;;###autoload
(defun pymacs-apply (function arguments)
  "Return the result of calling a Python function FUNCTION over ARGUMENTS.
FUNCTION is a string denoting the Python function, ARGUMENTS is a list of
Lisp expressions.  Immutable Lisp constants are converted to Python
equivalents, other structures are converted into Lisp handles."
  (pymacs-serve-until-reply
   "eval" `(pymacs-print-for-apply ',function ',arguments)))

;;; Integration details.

;; This page tries to increase the integration seamlessness of Pymacs
;; with the reminder of Emacs.

;; Module "desktop" savagely kills `*Pymacs*' in some circumstances.
;; Let's avoid such damage.

(eval-after-load 'desktop
  '(push "\\*Pymacs\\*" desktop-clear-preserve-buffers))

;; Python functions and modules should ideally look like Lisp
;; functions and modules.

(when t

  (defadvice documentation (around pymacs-ad-documentation activate)
    ;; Integration of doc-strings.
    (let* ((reference (pymacs-python-reference function))
           (python-doc (when reference
                         (pymacs-eval (format "doc_string(%s)" reference)))))
      (if (or reference python-doc)
          (setq ad-return-value
                (concat
                 "It interfaces to a Python function.\n\n"
                 (when python-doc
                   (if raw python-doc (substitute-command-keys python-doc)))))
        ad-do-it))))

(defun pymacs-python-reference (object)
  ;; Return the text reference of a Python object if possible, else nil.
  (when (functionp object)
    (let* ((definition (indirect-function object))
           (body (and (pymacs-proper-list-p definition)
                      (> (length definition) 2)
                      (eq (car definition) 'lambda)
                      (cddr definition))))
      (when (and body (listp (car body)) (eq (caar body) 'interactive))
        ;; Skip the interactive specification of a function.
        (setq body (cdr body)))
      (when (and body
                 ;; Advised functions start with a string.
                 (not (stringp (car body)))
                 ;; Python trampolines hold exactly one expression.
                 (= (length body) 1))
        (let ((expression (car body)))
          ;; EXPRESSION might now hold something like:
          ;;    (pymacs-apply (quote (pymacs-python . N)) ARGUMENT-LIST)
          (when (and (pymacs-proper-list-p expression)
                     (= (length expression) 3)
                     (eq (car expression) 'pymacs-apply)
                     (eq (car (cadr expression)) 'quote))
            (setq object (cadr (cadr expression))))))))
  (when (eq (car-safe object) 'pymacs-python)
    (format "python[%d]" (cdr object))))

;; The following functions are experimental -- they are not satisfactory yet.

(defun pymacs-file-handler (operation &rest arguments)
  ;; Integration of load-file, autoload, etc.
  ;; Emacs might want the contents of some `MODULE.el' which does not exist,
  ;; while there is a `MODULE.py' or `MODULE.pyc' file in the same directory.
  ;; The goal is to generate a virtual contents for this `MODULE.el' file, as
  ;; a set of Lisp trampoline functions to the Python module functions.
  ;; Python modules can then be loaded or autoloaded as if they were Lisp.
  (cond ((and (eq operation 'file-readable-p)
              (let ((module (substring (car arguments) 0 -3)))
                (or (pymacs-file-force operation arguments)
                    (file-readable-p (concat module ".py"))
                    (file-readable-p (concat module ".pyc"))))))
        ((and (eq operation 'load)
              (not (pymacs-file-force
                    'file-readable-p (list (car arguments))))
              (file-readable-p (car arguments)))
         (let ((lisp-code (pymacs-call "pymacs_load_helper"
                                       (substring (car arguments) 0 -3)
                                       nil)))
           (unless lisp-code
             (pymacs-report-error "Python import error"))
           (eval lisp-code)))
        ((and (eq operation 'insert-file-contents)
              (not (pymacs-file-force
                    'file-readable-p (list (car arguments))))
              (file-readable-p (car arguments)))
         (let ((lisp-code (pymacs-call "pymacs_load_helper"
                                       (substring (car arguments) 0 -3)
                                       nil)))
           (unless lisp-code
             (pymacs-report-error "Python import error"))
           (insert (prin1-to-string lisp-code))))
        (t (pymacs-file-force operation arguments))))

(defun pymacs-file-force (operation arguments)
  ;; Bypass the file handler.
  (let ((inhibit-file-name-handlers
         (cons 'pymacs-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation arguments)))

;;(add-to-list 'file-name-handler-alist '("\\.el\\'" . pymacs-file-handler))

;;; Gargabe collection of Python IDs.

;; Python objects which have no Lisp representation are allocated on the
;; Python side as `python[INDEX]', and INDEX is transmitted to Emacs, with
;; the value to use on the Lisp side for it.  Whenever Lisp does not need a
;; Python object anymore, it should be freed on the Python side.  The
;; following variables and functions are meant to fill this duty.

(defvar pymacs-used-ids nil
  "List of received IDs, currently allocated on the Python side.")

;; This is set whenever the Pymacs helper successfully starts, and is
;; also used to later detect the death of a previous helper.  If
;; pymacs-use-hash-tables is unset, this variable receives `t' when
;; the helper starts, so the detection works nevertheless.
(defvar pymacs-weak-hash nil
  "Weak hash table, meant to find out which IDs are still needed.")

(defvar pymacs-gc-wanted nil
  "Flag that it is desirable to clean up unused IDs on the Python side.")

(defvar pymacs-gc-inhibit nil
  "Flag that a new Pymacs garbage collection should just not run now.")

(defvar pymacs-gc-timer nil
  "Timer to trigger Pymacs garbage collection at regular time intervals.
The timer is used only if `post-gc-hook' is not available.")

(defun pymacs-schedule-gc (&optional xemacs-list)
  (unless pymacs-gc-inhibit
    (setq pymacs-gc-wanted t)))

(defun pymacs-garbage-collect ()
  ;; Clean up unused IDs on the Python side.
  (when (and pymacs-use-hash-tables (not pymacs-gc-inhibit))
    (let ((pymacs-gc-inhibit t)
          (pymacs-forget-mutability t)
          (ids pymacs-used-ids)
          used-ids unused-ids)
      (while ids
        (let ((id (car ids)))
          (setq ids (cdr ids))
          (if (gethash id pymacs-weak-hash)
              (setq used-ids (cons id used-ids))
            (setq unused-ids (cons id unused-ids)))))
      (setq pymacs-used-ids used-ids
            pymacs-gc-wanted nil)
      (when unused-ids
        (let ((pymacs-forget-mutability t))
          (pymacs-call "free_python" unused-ids))))))

(defun pymacs-defuns (arguments)
  ;; Take one argument, a list holding a number of items divisible by 3.  The
  ;; first argument is an INDEX, the second is a NAME, the third is the
  ;; INTERACTION specification, and so forth.  Register Python INDEX with a
  ;; function with that NAME and INTERACTION on the Lisp side.  The strange
  ;; calling convention is to minimise quoting at call time.
  (while (>= (length arguments) 3)
    (let ((index (nth 0 arguments))
          (name (nth 1 arguments))
          (interaction (nth 2 arguments)))
      (fset name (pymacs-defun index interaction))
      (setq arguments (nthcdr 3 arguments)))))

(defun pymacs-defun (index interaction)
  ;; Register INDEX on the Lisp side with a Python object that is a function,
  ;; and return a lambda form calling that function.  If the INTERACTION
  ;; specification is nil, the function is not interactive.  Otherwise, the
  ;; function is interactive, INTERACTION is then either a string, or the
  ;; index of an argument-less Python function returning the argument list.
  (let ((object (pymacs-python index)))
    (cond ((null interaction)
           `(lambda (&rest arguments)
              (pymacs-apply ',object arguments)))
          ((stringp interaction)
           `(lambda (&rest arguments)
              (interactive ,interaction)
              (pymacs-apply ',object arguments)))
          (t `(lambda (&rest arguments)
                (interactive (pymacs-call ',(pymacs-python interaction)))
                (pymacs-apply ',object arguments))))))

(defun pymacs-python (index)
  ;; Register on the Lisp side a Python object having INDEX, and return it.
  ;; The result is meant to be recognised specially by `print-for-eval', and
  ;; in the function position by `print-for-apply'.
  (let ((object (cons 'pymacs-python index)))
    (when pymacs-use-hash-tables
      (puthash index object pymacs-weak-hash)
      (setq pymacs-used-ids (cons index pymacs-used-ids)))
    object))

;;; Generating Python code.

;; Many Lisp expressions cannot fully be represented in Python, at least
;; because the object is mutable on the Lisp side.  Such objects are allocated
;; somewhere into a vector of handles, and the handle index is used for
;; communication instead of the expression itself.

(defvar pymacs-lisp nil
  "Vector of handles to hold transmitted expressions.")

(defvar pymacs-freed-list nil
  "List of unallocated indices in Lisp.")

;; When the Python GC is done with a Lisp object, a communication occurs so to
;; free the object on the Lisp side as well.

(defun pymacs-allocate-lisp (expression)
  ;; This function allocates some handle for an EXPRESSION, and return its
  ;; index.
  (unless pymacs-freed-list
    (let* ((previous pymacs-lisp)
           (old-size (length previous))
           (new-size (if (zerop old-size) 100 (+ old-size (/ old-size 2))))
           (counter new-size))
      (setq pymacs-lisp (make-vector new-size nil))
      (while (> counter 0)
        (setq counter (1- counter))
        (if (< counter old-size)
            (aset pymacs-lisp counter (aref previous counter))
          (setq pymacs-freed-list (cons counter pymacs-freed-list))))))
  (let ((index (car pymacs-freed-list)))
    (setq pymacs-freed-list (cdr pymacs-freed-list))
    (aset pymacs-lisp index expression)
    index))

(defun pymacs-free-lisp (indices)
  ;; This function is triggered from Python side for Lisp handles which lost
  ;; their last reference.  These references should be cut on the Lisp side as
  ;; well, or else, the objects will never be garbage-collected.
  (while indices
    (let ((index (car indices)))
      (aset pymacs-lisp index nil)
      (setq pymacs-freed-list (cons index pymacs-freed-list)
            indices (cdr indices)))))

(defun pymacs-print-for-apply (function arguments)
  ;; This function prints a Python expression calling FUNCTION, which is a
  ;; string naming a Python function, or a Python reference, over all its
  ;; ARGUMENTS, which are Lisp expressions.
  (let ((separator "")
        argument)
    (if (eq (car-safe function) 'pymacs-python)
        (princ (format "python[%d]" (cdr function)))
      (princ function))
    (princ "(")
    (while arguments
      (setq argument (car arguments)
            arguments (cdr arguments))
      (princ separator)
      (setq separator ", ")
      (pymacs-print-for-eval argument))
    (princ ")")))

(defun pymacs-print-for-eval (expression)
  ;; This function prints a Python expression out of a Lisp EXPRESSION.
  (let (done)
    (cond ((not expression)
           (princ "None")
           (setq done t))
          ((eq expression t)
           (princ "True")
           (setq done t))
          ((numberp expression)
           (princ expression)
           (setq done t))
          ((stringp expression)
           (when (or pymacs-forget-mutability
                     (not pymacs-mutable-strings))
             (let* ((multibyte (pymacs-multibyte-string-p expression))
                    (text (if multibyte
                              (encode-coding-string expression 'utf-8)
                            (copy-sequence expression))))
               (set-text-properties 0 (length text) nil text)
               (princ (mapconcat 'identity
                                 (split-string (prin1-to-string text) "\n")
                                 "\\n"))
               (when multibyte
                 (princ ".decode('UTF-8')")))
             (setq done t)))
          ((symbolp expression)
           (let ((name (symbol-name expression)))
             ;; The symbol can only be transmitted when in the main oblist.
             (when (eq expression (intern-soft name))
               (princ "lisp[")
               (prin1 name)
               (princ "]")
               (setq done t))))
          ((vectorp expression)
           (when pymacs-forget-mutability
             (let ((limit (length expression))
                   (counter 0))
               (princ "(")
               (while (< counter limit)
                 (unless (zerop counter)
                   (princ ", "))
                 (pymacs-print-for-eval (aref expression counter))
                 (setq counter (1+ counter)))
               (when (= limit 1)
                 (princ ","))
               (princ ")")
               (setq done t))))
          ((eq (car-safe expression) 'pymacs-python)
           (princ "python[")
           (princ (cdr expression))
           (princ "]")
           (setq done t))
          ((pymacs-proper-list-p expression)
           (when pymacs-forget-mutability
             (princ "[")
             (pymacs-print-for-eval (car expression))
             (while (setq expression (cdr expression))
               (princ ", ")
               (pymacs-print-for-eval (car expression)))
             (princ "]")
             (setq done t))))
    (unless done
      (let ((class (cond ((vectorp expression) "Vector")
                         ((and pymacs-use-hash-tables
                               (hash-table-p expression))
                          "Table")
                         ((bufferp expression) "Buffer")
                         ((pymacs-proper-list-p expression) "List")
                         (t "Lisp"))))
        (princ class)
        (princ "(")
        (princ (pymacs-allocate-lisp expression))
        (princ ")")))))

;;; Communication protocol.

(defvar pymacs-transit-buffer nil
  "Communication buffer between Emacs and Python.")

;; The principle behind the communication protocol is that it is easier to
;; generate than parse, and that each language already has its own parser.
;; So, the Emacs side generates Python text for the Python side to interpret,
;; while the Python side generates Lisp text for the Lisp side to interpret.
;; About nothing but expressions are transmitted, which are evaluated on
;; arrival.  The pseudo `reply' function is meant to signal the final result
;; of a series of exchanges following a request, while the pseudo `error'
;; function is meant to explain why an exchange could not have been completed.

;; The protocol itself is rather simple, and contains human readable text
;; only.  A message starts at the beginning of a line in the communication
;; buffer, either with `>' for the Lisp to Python direction, or `<' for the
;; Python to Lisp direction.  This is followed by a decimal number giving the
;; length of the message text, a TAB character, and the message text itself.
;; Message direction alternates systematically between messages, it never
;; occurs that two successive messages are sent in the same direction.  The
;; first message is received from the Python side, it is `(version VERSION)'.

(defun pymacs-start-services ()
  ;; This function gets called automatically, as needed.
  (let ((buffer (get-buffer-create "*Pymacs*")))
    (with-current-buffer buffer
      ;; Erase the buffer in case some previous incarnation of the
      ;; Pymacs helper died.  Otherwise, the "(goto-char (point-min))"
      ;; below might not find the proper synchronising reply and later
      ;; trigger a spurious "Protocol error" diagnostic.
      (erase-buffer)
      (buffer-disable-undo)
      (pymacs-set-buffer-multibyte nil)
      (set-buffer-file-coding-system 'raw-text)
      (save-match-data
        ;; Launch the Pymacs helper.
        (let ((process
               (apply 'start-process "pymacs" buffer
                      (let ((python (getenv "PYMACS_PYTHON")))
                        (if (or (null python) (equal python ""))
                            pymacs-python-command
                          python))
                      "-c" (concat "import sys;"
                                   " from Pymacs import main;"
                                   " main(*sys.argv[1:])")
                      (append
                       (and (>= emacs-major-version 24) '("-f"))
                       (mapcar 'expand-file-name pymacs-load-path)))))
          (pymacs-kill-without-query process)
          ;; Receive the synchronising reply.
          (while (progn
                   (goto-char (point-min))
                   (not (re-search-forward "<\\([0-9]+\\)\t" nil t)))
            (unless (accept-process-output process pymacs-timeout-at-start)
              (pymacs-report-error
               "Pymacs helper did not start within %d seconds"
               pymacs-timeout-at-start)))
          (let ((marker (process-mark process))
                (limit-position (+ (match-end 0)
                                   (string-to-number (match-string 1)))))
            (while (< (marker-position marker) limit-position)
              (unless (accept-process-output process pymacs-timeout-at-start)
                (pymacs-report-error
                 "Pymacs helper probably was interrupted at start")))))
        ;; Check that synchronisation occurred.
        (goto-char (match-end 0))
        (let ((reply (read (current-buffer))))
          (if (and (pymacs-proper-list-p reply)
                   (= (length reply) 2)
                   (eq (car reply) 'version))
              (unless (string-equal (cadr reply) "0.25")
                (pymacs-report-error
                 "Pymacs Lisp version is 0.25, Python is %s"
                 (cadr reply)))
            (pymacs-report-error "Pymacs got an invalid initial reply")))))
    (if (not pymacs-use-hash-tables)
        (setq pymacs-weak-hash t)
      (when pymacs-used-ids
        ;; A previous Pymacs session occurred in this Emacs session,
        ;; some IDs hang around which do not correspond to anything on
        ;; the Python side.  Python should not recycle such IDs for
        ;; new objects.
        (let ((pymacs-transit-buffer buffer)
              (pymacs-forget-mutability t)
              (pymacs-gc-inhibit t))
          (pymacs-call "zombie_python" pymacs-used-ids))
        (setq pymacs-used-ids nil))
      (setq pymacs-weak-hash (make-hash-table :weakness 'value))
      (if (boundp 'post-gc-hook)
          (add-hook 'post-gc-hook 'pymacs-schedule-gc)
        (setq pymacs-gc-timer (run-at-time 20 20 'pymacs-schedule-gc))))
    ;; If nothing failed, only then declare that Pymacs has started!
    (setq pymacs-transit-buffer buffer)))

(defun pymacs-terminate-services ()
  ;; This function is mainly provided for documentation purposes.
  (interactive)
  (garbage-collect)
  (pymacs-garbage-collect)
  (when (or (not pymacs-used-ids)
            (yes-or-no-p "\
Killing the Pymacs helper might create zombie objects.  Kill? "))
    (cond ((boundp 'post-gc-hook)
           (remove-hook 'post-gc-hook 'pymacs-schedule-gc))
          ((pymacs-timerp pymacs-gc-timer)
           (pymacs-cancel-timer pymacs-gc-timer)))
    (when pymacs-transit-buffer
      (kill-buffer pymacs-transit-buffer))
    (setq pymacs-gc-inhibit nil
          pymacs-gc-timer nil
          pymacs-transit-buffer nil
          pymacs-lisp nil
          pymacs-freed-list nil)))

(defun pymacs-serve-until-reply (action inserter)
  ;; This function builds a Python request by printing ACTION and
  ;; evaluating INSERTER, which itself prints an argument.  It then
  ;; sends the request to the Pymacs helper, and serves all
  ;; sub-requests coming from the Python side, until either a reply or
  ;; an error is finally received.
  (unless (and pymacs-transit-buffer
               (buffer-name pymacs-transit-buffer)
               (get-buffer-process pymacs-transit-buffer))
    (when pymacs-weak-hash
      (unless (or (eq pymacs-auto-restart t)
                  (and (eq pymacs-auto-restart 'ask)
                       (yes-or-no-p "The Pymacs helper died.  Restart it? ")))
        (pymacs-report-error "There is no Pymacs helper!")))
    (pymacs-start-services))
  (when pymacs-gc-wanted
    (pymacs-garbage-collect))
  (let ((inhibit-quit t)
        done value)
    (while (not done)
      (let ((form (pymacs-round-trip action inserter)))
        (setq action (car form))
        (when (eq action 'free)
          (pymacs-free-lisp (cadr form))
          (setq form (cddr form)
                action (car form)))
        (let* ((pair (pymacs-interruptible-eval (cadr form)))
               (success (cdr pair)))
          (setq value (car pair))
          (cond ((eq action 'eval)
                 (if success
                     (setq action "return"
                           inserter `(pymacs-print-for-eval ',value))
                   (setq action "raise"
                         inserter `(let ((pymacs-forget-mutability t))
                                     (pymacs-print-for-eval ,value)))))
                ((eq action 'expand)
                 (if success
                     (setq action "return"
                           inserter `(let ((pymacs-forget-mutability t))
                                       (pymacs-print-for-eval ,value)))
                   (setq action "raise"
                         inserter `(let ((pymacs-forget-mutability t))
                                     (pymacs-print-for-eval ,value)))))
                ((eq action 'return)
                 (if success
                     (setq done t)
                   (pymacs-report-error "%s" value)))
                ((eq action 'raise)
                 (if success
                     (pymacs-report-error "Python: %s" value)
                   (pymacs-report-error "%s" value)))
                (t (pymacs-report-error "Protocol error: %s" form))))))
    value))

(defun pymacs-round-trip (action inserter)
  ;; This function produces a Python request by printing and
  ;; evaluating INSERTER, which itself prints an argument.  It sends
  ;; the request to the Pymacs helper, awaits for any kind of reply,
  ;; and returns it.
  (with-current-buffer pymacs-transit-buffer
    ;; Possibly trim the beginning of the transit buffer.
    (cond ((not pymacs-trace-transit)
           (erase-buffer))
          ((consp pymacs-trace-transit)
           (when (> (buffer-size) (cdr pymacs-trace-transit))
             (let ((cut (- (buffer-size) (car pymacs-trace-transit))))
               (when (> cut 0)
                 (save-excursion
                   (goto-char cut)
                   (unless (memq (preceding-char) '(0 ?\n))
                     (forward-line 1))
                   (delete-region (point-min) (point))))))))
    ;; Send the request, wait for a reply, and process it.
    (let* ((process (get-buffer-process pymacs-transit-buffer))
           (status (process-status process))
           (marker (process-mark process))
           (moving (= (point) marker))
           send-position reply-position reply)
      (save-excursion
        (save-match-data
          ;; Encode request.
          (setq send-position (marker-position marker))
          (let ((standard-output marker))
            (princ action)
            (princ " ")
            (eval inserter))
          (goto-char marker)
          (unless (= (preceding-char) ?\n)
            (princ "\n" marker))
          ;; Send request text.
          (goto-char send-position)
          (insert (format ">%d\t" (- marker send-position)))
          (setq reply-position (marker-position marker))
          (process-send-region process send-position marker)
          ;; Receive reply text.
          (while (and (eq status 'run)
                      (progn
                        (goto-char reply-position)
                        (not (re-search-forward "<\\([0-9]+\\)\t" nil t))))
            (unless (accept-process-output process pymacs-timeout-at-reply)
              (setq status (process-status process))))
          (when (eq status 'run)
            (let ((limit-position (+ (match-end 0)
                                     (string-to-number (match-string 1)))))
              (while (and (eq status 'run)
                          (< (marker-position marker) limit-position))
                (unless (accept-process-output process pymacs-timeout-at-line)
                  (setq status (process-status process))))))
          ;; Decode reply.
          (if (not (eq status 'run))
              (pymacs-report-error "Pymacs helper status is `%S'" status)
            (goto-char (match-end 0))
            (setq reply (read (current-buffer))))))
      (when (and moving (not pymacs-trace-transit))
        (goto-char marker))
      reply)))

(defun pymacs-interruptible-eval (expression)
  ;; This function produces a pair (VALUE . SUCCESS) for EXPRESSION.
  ;; A cautious evaluation of EXPRESSION is attempted, and any
  ;; error while evaluating is caught, including Emacs quit (C-g).
  ;; Any Emacs quit also gets forward as a SIGINT to the Pymacs handler.
  ;; With SUCCESS being true, VALUE is the expression value.
  ;; With SUCCESS being false, VALUE is an interruption diagnostic.
  (condition-case info
      (cons (let ((inhibit-quit nil)) (eval expression)) t)
    (quit (setq quit-flag t)
          (interrupt-process pymacs-transit-buffer)
          (cons "*Interrupted!*" nil))
    (error (cons (prin1-to-string info) nil))))

(defun pymacs-proper-list-p (expression)
  ;; Tell if a list is proper, id est, that it is `nil' or ends with `nil'.
  (cond ((not expression))
        ((consp expression) (not (cdr (last expression))))))

(provide 'pymacs)
;;; pymacs.el ends here
