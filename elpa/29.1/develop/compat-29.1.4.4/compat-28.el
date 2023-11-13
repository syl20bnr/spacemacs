;;; compat-28.el --- Functionality added in Emacs 28.1 -*- lexical-binding: t; -*-

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

;; Functionality added in Emacs 28.1, needed by older Emacs versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-require compat-27 "27.1")

(compat-version "28.1")

;;;; Defined in comp.c

(compat-defalias native-comp-available-p ignore) ;; <compat-tests:native-comp-available-p>

;;;; Defined in fns.c

;; FIXME Should handle multibyte regular expressions
(compat-defun string-search (needle haystack &optional start-pos) ;; <compat-tests:string-search>
  "Search for the string NEEDLE in the string HAYSTACK.

The return value is the position of the first occurrence of
NEEDLE in HAYSTACK, or nil if no match was found.

The optional START-POS argument says where to start searching in
HAYSTACK and defaults to zero (start at the beginning).
It must be between zero and the length of HAYSTACK, inclusive.

Case is always significant and text properties are ignored.

NOTE: Prior to Emacs 27 `string-match' has issues handling
multibyte regular expressions.  As the compatibility function
for `string-search' is implemented via `string-match', these
issues are inherited."
  (when (and start-pos (or (< (length haystack) start-pos)
                           (< start-pos 0)))
    (signal 'args-out-of-range (list start-pos)))
  (let (case-fold-search)
    (string-match-p (regexp-quote needle) haystack start-pos)))

(compat-defun length= (sequence length) ;; [[compat-tests:length=]]
  "Returns non-nil if SEQUENCE has a length equal to LENGTH."
  (cond
   ((null sequence) (zerop length))
   ((consp sequence)
    (and (null (nthcdr length sequence))
         (nthcdr (1- length) sequence)
         t))
   ((arrayp sequence)
    (= (length sequence) length))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(compat-defun length< (sequence length) ;; [[compat-tests:length<]]
  "Returns non-nil if SEQUENCE is shorter than LENGTH."
  (cond
   ((null sequence) (not (zerop length)))
   ((listp sequence)
    (null (nthcdr (1- length) sequence)))
   ((arrayp sequence)
    (< (length sequence) length))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(compat-defun length> (sequence length) ;; [[compat-tests:length>]]
  "Returns non-nil if SEQUENCE is longer than LENGTH."
  (cond
   ((listp sequence)
    (and (nthcdr length sequence) t))
   ((arrayp sequence)
    (> (length sequence) length))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

;;;; Defined in fileio.c

(compat-defun file-name-concat (directory &rest components) ;; <compat-tests:file-name-concat>
  "Append COMPONENTS to DIRECTORY and return the resulting string.
Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don’t end with a slash, a slash will be
inserted before contatenating."
  (let ((separator (eval-when-compile
                     (if (memq system-type '(ms-dos windows-nt cygwin))
                         "\\" "/")))
        (components (delq nil
                          (mapcar (lambda (x) (and (not (equal "" x)) x))
                                  (cons directory components))))
        (result ""))
    (while components
      (let ((c (pop components)))
        (setq result (concat result c
                             (and components
                                  (not (string-suffix-p separator c))
                                  separator)))))
    result))

;;;; Defined in alloc.c

(compat-defalias garbage-collect-maybe ignore) ;; <compat-tests:garbage-collect-maybe>

;;;; Defined in characters.c

(compat-defun string-width (string &optional from to) ;; <compat-tests:string-width>
  "Handle optional arguments FROM and TO."
  :extended t
  (let* ((len (length string))
         (from (or from 0))
         (to (or to len)))
    (if (and (= from 0) (= to len))
        (string-width string)
      (string-width (substring string from to)))))

;;;; Defined in dired.c

(compat-defun directory-files (directory &optional full match nosort count) ;; <compat-tests:directory-files>
  "Handle additional optional argument COUNT."
  :extended t
  (let ((files (directory-files directory full match nosort)))
    (when (natnump count)
      (setf (nthcdr count files) nil))
    files))

(compat-defun directory-files-and-attributes (directory &optional full match nosort id-format count) ;; <compat-tests:directory-files-and-attributes>
  "Handle additional optional argument COUNT."
  :extended t
  (let ((files (directory-files-and-attributes directory full match nosort id-format)))
    (when (natnump count)
      (setf (nthcdr count files) nil))
    files))

;;;; xfaces.c

(compat-defun color-values-from-color-spec (spec) ;; <compat-tests:color-values-from-color-spec>
  "Parse color SPEC as a numeric color and return (RED GREEN BLUE).
This function recognises the following formats for SPEC:

 #RGB, where R, G and B are hex numbers of equal length, 1-4 digits each.
 rgb:R/G/B, where R, G, and B are hex numbers, 1-4 digits each.
 rgbi:R/G/B, where R, G and B are floating-point numbers in [0,1].

If SPEC is not in one of the above forms, return nil.

Each of the 3 integer members of the resulting list, RED, GREEN,
and BLUE, is normalized to have its value in [0,65535]."
  (let ((case-fold-search nil))
    (save-match-data
      (cond
       ((string-match
         ;; (rx bos "#"
         ;;     (or (: (group-n 1 (= 1 hex)) (group-n 2 (= 1 hex)) (group-n 3 (= 1 hex)))
         ;;         (: (group-n 1 (= 2 hex)) (group-n 2 (= 2 hex)) (group-n 3 (= 2 hex)))
         ;;         (: (group-n 1 (= 3 hex)) (group-n 2 (= 3 hex)) (group-n 3 (= 3 hex)))
         ;;         (: (group-n 1 (= 4 hex)) (group-n 2 (= 4 hex)) (group-n 3 (= 4 hex))))
         ;;     eos)
         "\\`#\\(?:\\(?1:[[:xdigit:]]\\{1\\}\\)\\(?2:[[:xdigit:]]\\{1\\}\\)\\(?3:[[:xdigit:]]\\{1\\}\\)\\|\\(?1:[[:xdigit:]]\\{2\\}\\)\\(?2:[[:xdigit:]]\\{2\\}\\)\\(?3:[[:xdigit:]]\\{2\\}\\)\\|\\(?1:[[:xdigit:]]\\{3\\}\\)\\(?2:[[:xdigit:]]\\{3\\}\\)\\(?3:[[:xdigit:]]\\{3\\}\\)\\|\\(?1:[[:xdigit:]]\\{4\\}\\)\\(?2:[[:xdigit:]]\\{4\\}\\)\\(?3:[[:xdigit:]]\\{4\\}\\)\\)\\'"
         spec)
        (let ((max (1- (ash 1 (* (- (match-end 1) (match-beginning 1)) 4)))))
          (list (/ (* (string-to-number (match-string 1 spec) 16) 65535) max)
                (/ (* (string-to-number (match-string 2 spec) 16) 65535) max)
                (/ (* (string-to-number (match-string 3 spec) 16) 65535) max))))
       ((string-match
         ;; (rx bos "rgb:"
         ;;     (group (** 1 4 hex)) "/"
         ;;     (group (** 1 4 hex)) "/"
         ;;     (group (** 1 4 hex))
         ;;     eos)
         "\\`rgb:\\([[:xdigit:]]\\{1,4\\}\\)/\\([[:xdigit:]]\\{1,4\\}\\)/\\([[:xdigit:]]\\{1,4\\}\\)\\'"
         spec)
        (list (/ (* (string-to-number (match-string 1 spec) 16) 65535)
                 (1- (ash 1 (* (- (match-end 1) (match-beginning 1)) 4))))
              (/ (* (string-to-number (match-string 2 spec) 16) 65535)
                 (1- (ash 1 (* (- (match-end 2) (match-beginning 2)) 4))))
              (/ (* (string-to-number (match-string 3 spec) 16) 65535)
                 (1- (ash 1 (* (- (match-end 3) (match-beginning 3)) 4))))))
       ;; The "RGBi" (RGB Intensity) specification is defined by
       ;; XCMS[0], see [1] for the implementation in Xlib.
       ;;
       ;; [0] http://www.nic.funet.fi/pub/X11/X11R4/DOCS/color/Xcms.text
       ;; [1] https://gitlab.freedesktop.org/xorg/lib/libx11/-/blob/master/src/xcms/LRGB.c#L1392
       ((string-match
         ;; (rx bos "rgbi:" (* space)
         ;;     (group (? (or "-" "+"))
         ;;            (or (: (+ digit) (? "." (* digit)))
         ;;                (: "." (+ digit)))
         ;;            (? "e" (? (or "-" "+")) (+ digit)))
         ;;     "/" (* space)
         ;;     (group (? (or "-" "+"))
         ;;            (or (: (+ digit) (? "." (* digit)))
         ;;                (: "." (+ digit)))
         ;;            (? "e" (? (or "-" "+")) (+ digit)))
         ;;     "/" (* space)
         ;;     (group (? (or "-" "+"))
         ;;            (or (: (+ digit) (? "." (* digit)))
         ;;                (: "." (+ digit)))
         ;;            (? "e" (? (or "-" "+")) (+ digit)))
         ;;     eos)
         "\\`rgbi:[[:space:]]*\\([+-]?\\(?:[[:digit:]]+\\(?:\\.[[:digit:]]*\\)?\\|\\.[[:digit:]]+\\)\\(?:e[+-]?[[:digit:]]+\\)?\\)/[[:space:]]*\\([+-]?\\(?:[[:digit:]]+\\(?:\\.[[:digit:]]*\\)?\\|\\.[[:digit:]]+\\)\\(?:e[+-]?[[:digit:]]+\\)?\\)/[[:space:]]*\\([+-]?\\(?:[[:digit:]]+\\(?:\\.[[:digit:]]*\\)?\\|\\.[[:digit:]]+\\)\\(?:e[+-]?[[:digit:]]+\\)?\\)\\'"
         spec)
        (let ((r (round (* (string-to-number (match-string 1 spec)) 65535)))
              (g (round (* (string-to-number (match-string 2 spec)) 65535)))
              (b (round (* (string-to-number (match-string 3 spec)) 65535))))
          (when (and (<= 0 r) (<= r 65535)
                     (<= 0 g) (<= g 65535)
                     (<= 0 b) (<= b 65535))
            (list r g b))))))))

;;;; Defined in simple.el

(compat-defun make-separator-line (&optional length) ;; <compat-tests:make-separator-line>
  "Make a string appropriate for usage as a visual separator line.
If LENGTH is nil, use the window width."
  (if (display-graphic-p)
      (if length
          (concat (propertize (make-string length ?\s) 'face '(:underline t)) "\n")
        (propertize "\n" 'face '(:extend t :height 0.1 :inverse-video t)))
    (concat (make-string (or length (1- (window-width))) ?-) "\n")))

;;;; Defined in subr.el

(compat-defun process-lines-handling-status (program status-handler &rest args) ;; <compat-tests:process-lines-handling-status>
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
If STATUS-HANDLER is non-nil, it must be a function with one
argument, which will be called with the exit status of the
program before the output is collected.  If STATUS-HANDLER is
nil, an error is signaled if the program returns with a non-zero
exit status."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil (current-buffer) nil args)))
      (if status-handler
          (funcall status-handler status)
        (unless (eq status 0)
          (error "%s exited with status %s" program status)))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (setq lines (cons (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            lines))
          (forward-line 1))
        (nreverse lines)))))

(compat-defun process-lines-ignore-status (program &rest args) ;; <compat-tests:process-lines-ignore-status>
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
The exit status of the program is ignored.
Also see `process-lines'."
  (apply 'process-lines-handling-status program #'ignore args))

;; FIXME Should handle multibyte regular expressions
(compat-defun string-replace (fromstring tostring instring) ;; <compat-tests:string-replace>
  "Replace FROMSTRING with TOSTRING in INSTRING each time it occurs."
  (when (equal fromstring "")
    (signal 'wrong-length-argument '(0)))
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     (regexp-quote fromstring)
     tostring instring
     t t)))

(compat-defun always (&rest _arguments) ;; <compat-tests:always>
  "Do nothing and return t.
This function accepts any number of ARGUMENTS, but ignores them.
Also see `ignore'."
  t)

(compat-defun insert-into-buffer (buffer &optional start end) ;; <compat-tests:insert-into-buffer>
  "Insert the contents of the current buffer into BUFFER.
If START/END, only insert that region from the current buffer.
Point in BUFFER will be placed after the inserted text."
  (let ((current (current-buffer)))
    (with-current-buffer buffer
      (insert-buffer-substring current start end))))

(compat-defun replace-string-in-region (string replacement &optional start end) ;; <compat-tests:replace-string-in-region>
  "Replace STRING with REPLACEMENT in the region from START to END.
The number of replaced occurrences are returned, or nil if STRING
doesn't exist in the region.

If START is nil, use the current point.  If END is nil, use `point-max'.

Comparisons and replacements are done with fixed case."
  (if start
      (when (< start (point-min))
        (error "Start before start of buffer"))
    (setq start (point)))
  (if end
      (when (> end (point-max))
        (error "End after end of buffer"))
    (setq end (point-max)))
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (let ((matches 0)
            (case-fold-search nil))
        (while (search-forward string nil t)
          (delete-region (match-beginning 0) (match-end 0))
          (insert replacement)
          (setq matches (1+ matches)))
        (and (not (zerop matches))
             matches)))))

(compat-defun replace-regexp-in-region (regexp replacement &optional start end) ;; <compat-tests:replace-regexp-in-region>
  "Replace REGEXP with REPLACEMENT in the region from START to END.
The number of replaced occurrences are returned, or nil if REGEXP
doesn't exist in the region.

If START is nil, use the current point.  If END is nil, use `point-max'.

Comparisons and replacements are done with fixed case.

REPLACEMENT can use the following special elements:

  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\?' is treated literally."
  (if start
      (when (< start (point-min))
        (error "Start before start of buffer"))
    (setq start (point)))
  (if end
      (when (> end (point-max))
        (error "End after end of buffer"))
    (setq end (point-max)))
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (let ((matches 0)
            (case-fold-search nil))
          (while (re-search-forward regexp nil t)
          (replace-match replacement t)
          (setq matches (1+ matches)))
        (and (not (zerop matches))
             matches)))))

(compat-defun buffer-local-boundp (symbol buffer) ;; <compat-tests:buffer-local-boundp>
  "Return non-nil if SYMBOL is bound in BUFFER.
Also see `local-variable-p'."
  (condition-case nil
      (progn (buffer-local-value symbol buffer)
             t)
    (void-variable nil)))

(compat-defmacro with-existing-directory (&rest body) ;; <compat-tests:with-existing-directory>
  "Execute BODY with `default-directory' bound to an existing directory.
If `default-directory' is already an existing directory, it's not changed."
  (declare (indent 0) (debug t))
  `(let ((default-directory
          (or (catch 'quit
                (dolist (dir (list default-directory
                                   (expand-file-name "~/")
                                   temporary-file-directory
                                   (getenv "TMPDIR")
                                   "/tmp/"))
                  (when (and dir (file-exists-p dir))
                    (throw 'quit dir))))
              "/")))
     ,@body))

(compat-defmacro dlet (binders &rest body) ;; <compat-tests:dlet>
  "Like `let' but using dynamic scoping."
  (declare (indent 1) (debug let))
  `(let (_)
     ,@(mapcar (lambda (binder)
                 `(defvar ,(if (consp binder) (car binder) binder)))
               binders)
     (let ,binders ,@body)))

(compat-defun ensure-list (object) ;; <compat-tests:ensure-list>
  "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (if (listp object)
      object
    (list object)))

(compat-defalias subr-primitive-p subrp) ;; <compat-tests:subr-primitive-p>

;;;; Defined in data.c

(compat-defalias subr-native-elisp-p ignore) ;; <compat-tests:subr-native-elisp-p>

;;;; Defined in subr-x.el

(compat-defun string-clean-whitespace (string) ;; <compat-tests:string-clean-whitespace>
  "Clean up whitespace in STRING.
All sequences of whitespaces in STRING are collapsed into a
single space character, and leading/trailing whitespace is
removed."
  (let ((blank "[[:blank:]\r\n]+"))
    (replace-regexp-in-string
     "^[[:blank:]\r\n]+\\|[[:blank:]\r\n]+$"
     ""
     (replace-regexp-in-string
      blank " " string))))

(compat-defun string-fill (string length) ;; <compat-tests:string-fill>
  "Clean up whitespace in STRING.
All sequences of whitespaces in STRING are collapsed into a
single space character, and leading/trailing whitespace is
removed."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((fill-column length)
          (adaptive-fill-mode nil))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(compat-defun string-pad (string length &optional padding start) ;; <compat-tests:string-pad>
  "Pad STRING to LENGTH using PADDING.
If PADDING is nil, the space character is used.  If not nil, it
should be a character.

If STRING is longer than the absolute value of LENGTH, no padding
is done.

If START is nil (or not present), the padding is done to the end
of the string, and if non-nil, padding is done to the start of
the string."
  (unless (natnump length)
    (signal 'wrong-type-argument (list 'natnump length)))
  (let ((pad-length (- length (length string))))
    (if (< pad-length 0)
        string
      (concat (and start
                   (make-string pad-length (or padding ?\s)))
              string
              (and (not start)
                   (make-string pad-length (or padding ?\s)))))))

(compat-defun string-chop-newline (string) ;; <compat-tests:string-chop-newline>
  "Remove the final newline (if any) from STRING."
  (if (and (>= (length string) 1) (= (aref string (1- (length string))) ?\n))
      (substring string 0 -1)
    string))

(compat-defmacro named-let (name bindings &rest body) ;; <compat-tests:named-let>
  "Looping construct taken from Scheme.
Like `let', bind variables in BINDINGS and then evaluate BODY,
but with the twist that BODY can evaluate itself recursively by
calling NAME, where the arguments passed to NAME are used
as the new values of the bound variables in the recursive invocation."
  (declare (indent 2) (debug (symbolp (&rest (symbolp form)) body)))
  (let ((fargs (mapcar (lambda (b)
                         (let ((var (if (consp b) (car b) b)))
                           (make-symbol (symbol-name var))))
                       bindings))
        (aargs (mapcar (lambda (b) (if (consp b) (cadr b))) bindings))
        rargs)
    (dotimes (i (length bindings))
      (let ((b (nth i bindings)))
        (push (list (if (consp b) (car b) b) (nth i fargs))
              rargs)
        (setf (if (consp b) (car b) b)
              (nth i fargs))))
    (letrec
        ((quit (make-symbol "quit")) (self (make-symbol "self"))
         (total-tco t)
         (macro (lambda (&rest args)
                  (setq total-tco nil)
                  `(funcall ,self . ,args)))
         ;; Based on `cl--self-tco':
         (tco-progn (lambda (exprs)
                      (append
                       (butlast exprs)
                       (list (funcall tco (car (last exprs)))))))
         (tco (lambda (expr)
                (cond
                 ((eq (car-safe expr) 'if)
                  (append (list 'if
                                (cadr expr)
                                (funcall tco (nth 2 expr)))
                          (funcall tco-progn (nthcdr 3 expr))))
                 ((eq (car-safe expr) 'cond)
                  (let ((conds (cdr expr)) body)
                    (while conds
                      (let ((branch (pop conds)))
                        (push (cond
                               ((cdr branch) ;has tail
                                (funcall tco-progn branch))
                               ((null conds) ;last element
                                (list t (funcall tco (car branch))))
                               ((progn
                                  branch)))
                              body)))
                    (cons 'cond (nreverse body))))
                 ((eq (car-safe expr) 'or)
                  (if (cddr expr)
                      (let ((var (make-symbol "var")))
                        `(let ((,var ,(cadr expr)))
                           (if ,var ,(funcall tco var)
                             ,(funcall tco (cons 'or (cddr expr))))))
                    (funcall tco (cadr expr))))
                 ((eq (car-safe expr) 'condition-case)
                  (append (list 'condition-case (cadr expr) (nth 2 expr))
                          (mapcar
                           (lambda (handler)
                             (cons (car handler)
                                   (funcall tco-progn (cdr handler))))
                           (nthcdr 3 expr))))
                 ((memq (car-safe expr) '(and progn))
                  (cons (car expr) (funcall tco-progn (cdr expr))))
                 ((memq (car-safe expr) '(let let*))
                  (append (list (car expr) (cadr expr))
                          (funcall tco-progn (cddr expr))))
                 ((eq (car-safe expr) name)
                  (let (sets (args (cdr expr)))
                    (dolist (farg fargs)
                      (push (list farg (pop args))
                            sets))
                    (cons 'setq (apply #'nconc (nreverse sets)))))
                 (`(throw ',quit ,expr))))))
      (when-let ((tco-body (funcall tco (macroexpand-all (macroexp-progn body)))))
        (setq body `((catch ',quit
                       (while t (let ,rargs ,@(macroexp-unprogn tco-body)))))))
      (let ((expand (macroexpand-all (macroexp-progn body) (list (cons name macro)))))
        (if total-tco
            `(let ,bindings ,expand)
          `(funcall
            (letrec ((,self (lambda ,fargs ,expand))) ,self)
            ,@aargs))))))

;;;; Defined in files.el

(compat-defun file-name-with-extension (filename extension) ;; <compat-tests:file-name-with-extension>
  "Set the EXTENSION of a FILENAME.
The extension (in a file name) is the part that begins with the last \".\".

Trims a leading dot from the EXTENSION so that either \"foo\" or
\".foo\" can be given.

Errors if the FILENAME or EXTENSION are empty, or if the given
FILENAME has the format of a directory.

See also `file-name-sans-extension'."
  (let ((extn (string-remove-prefix "." extension)))
    (cond
     ((string= filename "")
      (error "Empty filename"))
     ((string= extn "")
      (error "Malformed extension: %s" extension))
     ((directory-name-p filename)
      (error "Filename is a directory: %s" filename))
     (t
      (concat (file-name-sans-extension filename) "." extn)))))

(compat-defun directory-empty-p (dir) ;; <compat-tests:directory-empty-p>
  "Return t if DIR names an existing directory containing no other files.
Return nil if DIR does not name a directory, or if there was
trouble determining whether DIR is a directory or empty.

Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp t))))

(compat-defun file-modes-number-to-symbolic (mode &optional filetype) ;; <compat-tests:file-modes-number-to-symbolic>
  "Return a string describing a file's MODE.
For instance, if MODE is #o700, then it produces `-rwx------'.
FILETYPE if provided should be a character denoting the type of file,
such as `?d' for a directory, or `?l' for a symbolic link and will override
the leading `-' char."
  (string
   (or filetype
       (pcase (ash mode -12)
         ;; POSIX specifies that the file type is included in st_mode
         ;; and provides names for the file types but values only for
         ;; the permissions (e.g., S_IWOTH=2).

         ;; (#o017 ??) ;; #define S_IFMT  00170000
         (#o014 ?s)    ;; #define S_IFSOCK 0140000
         (#o012 ?l)    ;; #define S_IFLNK  0120000
         ;; (8  ??)    ;; #define S_IFREG  0100000
         (#o006  ?b)   ;; #define S_IFBLK  0060000
         (#o004  ?d)   ;; #define S_IFDIR  0040000
         (#o002  ?c)   ;; #define S_IFCHR  0020000
         (#o001  ?p)   ;; #define S_IFIFO  0010000
         (_ ?-)))
   (if (zerop (logand   256 mode)) ?- ?r)
   (if (zerop (logand   128 mode)) ?- ?w)
   (if (zerop (logand    64 mode))
       (if (zerop (logand  2048 mode)) ?- ?S)
     (if (zerop (logand  2048 mode)) ?x ?s))
   (if (zerop (logand    32 mode)) ?- ?r)
   (if (zerop (logand    16 mode)) ?- ?w)
   (if (zerop (logand     8 mode))
       (if (zerop (logand  1024 mode)) ?- ?S)
     (if (zerop (logand  1024 mode)) ?x ?s))
   (if (zerop (logand     4 mode)) ?- ?r)
   (if (zerop (logand     2 mode)) ?- ?w)
   (if (zerop (logand 512 mode))
       (if (zerop (logand   1 mode)) ?- ?x)
     (if (zerop (logand   1 mode)) ?T ?t))))

(compat-defun file-backup-file-names (filename) ;; <compat-tests:file-backup-file-names>
  "Return a list of backup files for FILENAME.
The list will be sorted by modification time so that the most
recent files are first."
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
                    (make-backup-file-name (expand-file-name filename))))
         (dir (file-name-directory filename))
         files)
    (dolist (file (file-name-all-completions
                   (file-name-nondirectory filename) dir))
      (let ((candidate (concat dir file)))
        (when (and (backup-file-name-p candidate)
                   (string= (file-name-sans-versions candidate) filename))
          (push candidate files))))
    (sort files #'file-newer-than-file-p)))

(compat-defun make-lock-file-name (filename) ;; <compat-tests:make-lock-file-name>
  "Make a lock file name for FILENAME.
This prepends \".#\" to the non-directory part of FILENAME, and
doesn't respect `lock-file-name-transforms', as Emacs 28.1 and
onwards does."
  (expand-file-name
   (concat
    ".#" (file-name-nondirectory filename))
   (file-name-directory filename)))

;;;; Defined in minibuffer.el

(compat-defun format-prompt (prompt default &rest format-args) ;; <compat-tests:format-prompt>
  "Format PROMPT with DEFAULT.
If FORMAT-ARGS is nil, PROMPT is used as a plain string.  If
FORMAT-ARGS is non-nil, PROMPT is used as a format control
string, and FORMAT-ARGS are the arguments to be substituted into
it.  See `format' for details.

If DEFAULT is a list, the first element is used as the default.
If not, the element is used as is.

If DEFAULT is nil or an empty string, no \"default value\" string
is included in the return value."
  (concat
   (if (null format-args)
       prompt
     (apply #'format prompt format-args))
   (and default
        (or (not (stringp default))
            (> (length default) 0))
        (format " (default %s)"
                (if (consp default)
                    (car default)
                  default)))
   ": "))

;;;; Defined in faces.el

(compat-defvar color-luminance-dark-limit 0.325 ;; <compat-tests:color-dark-p>
  "The relative luminance below which a color is considered \"dark\".
A \"dark\" color in this sense provides better contrast with white
than with black; see `color-dark-p'.
This value was determined experimentally."
  :constant t)

(compat-defun color-dark-p (rgb) ;; <compat-tests:color-dark-p>
  "Whether RGB is more readable against white than black.
RGB is a 3-element list (R G B), each component in the range [0,1].
This predicate can be used both for determining a suitable (black or white)
contrast color with RGB as background and as foreground."
  (unless (<= 0 (apply #'min rgb) (apply #'max rgb) 1)
    (error "RGB components %S not in [0,1]" rgb))
  ;; Compute the relative luminance after gamma-correcting (assuming sRGB),
  ;; and compare to a cut-off value determined experimentally.
  ;; See https://en.wikipedia.org/wiki/Relative_luminance for details.
  (let* ((sr (nth 0 rgb))
         (sg (nth 1 rgb))
         (sb (nth 2 rgb))
         ;; Gamma-correct the RGB components to linear values.
         ;; Use the power 2.2 as an approximation to sRGB gamma;
         ;; it should be good enough for the purpose of this function.
         (r (expt sr 2.2))
         (g (expt sg 2.2))
         (b (expt sb 2.2))
         (y (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))))
    (< y color-luminance-dark-limit)))

;;;; Defined in window.el

(compat-defmacro with-window-non-dedicated (window &rest body) ;; <compat-tests:with-window-non-dedicated>
  "Evaluate BODY with WINDOW temporarily made non-dedicated.
If WINDOW is nil, use the selected window.  Return the value of
the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((window-dedicated-sym (gensym))
        (window-sym (gensym)))
    `(let* ((,window-sym (window-normalize-window ,window t))
            (,window-dedicated-sym (window-dedicated-p ,window-sym)))
       (set-window-dedicated-p ,window-sym nil)
       (unwind-protect
           (progn ,@body)
         (set-window-dedicated-p ,window-sym ,window-dedicated-sym)))))

(compat-defun count-windows (&optional minibuf all-frames) ;; <compat-tests:count-windows>
  "Handle optional argument ALL-FRAMES."
  :extended t
  (if all-frames
      (let ((sum 0))
        (dolist (frame (frame-list))
          (with-selected-frame frame
            (setq sum (+ (count-windows minibuf) sum))))
        sum)
    (count-windows minibuf)))

;;;; Defined in thingatpt.el

(compat-defun thing-at-mouse (event thing &optional no-properties) ;; <compat-tests:thing-at-mouse>
  "Return the THING at mouse click.
Like `thing-at-point', but tries to use the event
where the mouse button is clicked to find a thing nearby."
  ;; No :feature specified, since the function is autoloaded.
  (save-excursion
    (mouse-set-point event)
    (thing-at-point thing no-properties)))

(compat-defun bounds-of-thing-at-mouse (event thing) ;; <compat-tests:thing-at-mouse>
  "Determine start and end locations for THING at mouse click given by EVENT.
Like `bounds-of-thing-at-point', but tries to use the position in EVENT
where the mouse button is clicked to find the thing nearby."
  ;; No :feature specified, since the function is autoloaded.
  (save-excursion
    (mouse-set-point event)
    (bounds-of-thing-at-point thing)))

;;;; Defined in mouse.el

(compat-defun mark-thing-at-mouse (click thing) ;; <compat-tests:thing-at-mouse>
  "Activate the region around THING found near the mouse CLICK."
  (when-let ((bounds (bounds-of-thing-at-mouse click thing)))
    (goto-char (if mouse-select-region-move-to-beginning
                   (car bounds) (cdr bounds)))
    (push-mark (if mouse-select-region-move-to-beginning
                   (cdr bounds) (car bounds))
               t 'activate)))

;;;; Defined in macroexp.el

(compat-defun macroexp-warn-and-return (msg form &optional _category _compile-only _arg) ;; <compat-tests:macroexp-warn-and-return>
  "Return code equivalent to FORM labeled with warning MSG.
CATEGORY is the category of the warning, like the categories that
can appear in `byte-compile-warnings'.
COMPILE-ONLY non-nil means no warning should be emitted if the code
is executed without being compiled first.
ARG is a symbol (or a form) giving the source code position for the message.
It should normally be a symbol with position and it defaults to FORM."
  (macroexp--warn-and-return msg form))

(compat-defun macroexp-file-name () ;; <compat-tests:macroexp-file-name>
  "Return the name of the file from which the code comes.
Returns nil when we do not know.
A non-nil result is expected to be reliable when called from a macro in order
to find the file in which the macro's call was found, and it should be
reliable as well when used at the top-level of a file.
Other uses risk returning non-nil value that point to the wrong file."
  (let ((file (car (last current-load-list))))
    (or (if (stringp file) file)
        (bound-and-true-p byte-compile-current-file))))

;;;; Defined in env.el

(compat-defmacro with-environment-variables (variables &rest body) ;; <compat-tests:with-environment-variables>
  "Set VARIABLES in the environent and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be be restored upon exit."
  (declare (indent 1) (debug (sexp body)))
  (unless (consp variables)
    (error "Invalid VARIABLES: %s" variables))
  `(let ((process-environment (copy-sequence process-environment)))
     ,@(mapcar (lambda (elem)
                 `(setenv ,(car elem) ,(cadr elem)))
               variables)
     ,@body))

;;;; Defined in time-data.el

(compat-defun decoded-time-period (time) ;; <compat-tests:decoded-time-period>
  "Interpret DECODED as a period and return its length in seconds.
For computational purposes, years are 365 days long and months
are 30 days long."
  :feature time-date
  (+ (if (consp (decoded-time-second time))
         (/ (float (car (decoded-time-second time)))
            (cdr (decoded-time-second time)))
       (or (decoded-time-second time) 0))
     (* (or (decoded-time-minute time) 0) 60)
     (* (or (decoded-time-hour time) 0) 60 60)
     (* (or (decoded-time-day time) 0) 60 60 24)
     (* (or (decoded-time-month time) 0) 60 60 24 30)
     (* (or (decoded-time-year time) 0) 60 60 24 365)))

;;;; Defined in doc.c

(compat-defun text-quoting-style () ;; <compat-tests:text-quoting-style>
  "Return the current effective text quoting style.
If the variable `text-quoting-style' is `grave', `straight' or
`curve', just return that value.  If it is nil (the default), return
`grave' if curved quotes cannot be displayed (for instance, on a
terminal with no support for these characters), otherwise return
`quote'.  Any other value is treated as `grave'.

Note that in contrast to the variable `text-quoting-style', this
function will never return nil."
  (cond
   ((memq text-quoting-style '(grave straight curve))
    text-quoting-style)
   ((not text-quoting-style) 'grave)
   (t 'curve)))

;;;; Defined in button.el

;; Obsolete Alias since 29
(compat-defalias button-buttonize buttonize :obsolete t) ;; <compat-tests:button-buttonize>

;;;; Defined in wid-edit.el

(compat-guard t ;; <compat-tests:widget-natnum>
  :feature wid-edit
  (define-widget 'natnum 'restricted-sexp
    "A nonnegative integer."
    :tag "Integer (positive)"
    :value 0
    :type-error "This field should contain a nonnegative integer"
    :match-alternatives '(natnump)))

(provide 'compat-28)
;;; compat-28.el ends here
