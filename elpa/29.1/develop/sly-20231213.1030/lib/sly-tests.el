;;; sly-tests.el --- Automated tests for sly.el -*- lexical-binding: t; -*-
;;
;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;     Copyright (C) 2013
;;
;;     For a detailed list of contributors, see the manual.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;;;; Tests
(require 'sly)
(require 'ert nil t)
(require 'ert "lib/ert" t) ;; look for bundled version for Emacs 23
(require 'cl-lib)
(require 'bytecomp) ; byte-compile-current-file

(defun sly-shuffle-list (list)
  (let* ((len (length list))
         (taken (make-vector len nil))
         (result (make-vector len nil)))
    (dolist (e list)
      (while (let ((i (random len)))
               (cond ((aref taken i))
                     (t (aset taken i t)
                        (aset result i e)
                        nil)))))
    (append result '())))

(defun sly-batch-test (&optional test-name randomize)
  "Run the test suite in batch-mode.
Exits Emacs when finished. The exit code is the number of failed tests."
  (interactive)
  (let ((ert-debug-on-error nil)
        (timeout 30))
    (sly)
    ;; Block until we are up and running.
    (let (timed-out)
      (run-with-timer timeout nil
                      (lambda () (setq timed-out t)))
      (while (not (sly-connected-p))
        (sit-for 1)
        (when timed-out
          (when noninteractive
            (kill-emacs 252)))))
    (sly-sync-to-top-level 5)
    (let* ((selector (if randomize
                         `(member ,@(sly-shuffle-list
                                     (ert-select-tests (or test-name t) t)))
                       (or test-name t)))
           (ert-fun (if noninteractive
                        'ert-run-tests-batch
                      'ert)))
      (let ((stats (funcall ert-fun selector)))
        (if noninteractive
            (kill-emacs (ert-stats-completed-unexpected stats)))))))

(defun sly-skip-test (message)
  ;; ERT for Emacs 23 and earlier doesn't have `ert-skip'
  (if (fboundp 'ert-skip)
      (ert-skip message)
    (message (concat "SKIPPING: " message))
    (ert-pass)))

(defun sly-tests--undefine-all ()
  (dolist (test (ert-select-tests t t))
    (let ((sym (ert-test-name test)))
      (cl-assert (eq (get sym 'ert--test) test))
      (cl-remprop sym 'ert--test))))

(sly-tests--undefine-all)

(eval-and-compile
  (defun sly-tests-auto-tags ()
    (append '(sly)
            (let ((file-name (or load-file-name
                                 byte-compile-current-file)))
              (if (and (stringp file-name)
                       (string-match "test/sly-\\(.*\\)\.elc?$" file-name))
                  (list 'contrib (intern (match-string 1 file-name)))
                '(core)))))
  
  (defmacro define-sly-ert-test (name &rest args)
    "Like `ert-deftest', but set tags automatically.
Also don't error if `ert.el' is missing."
    (declare (debug (&define name sexp sexp &rest def-form)))
    (let* ((docstring (and (stringp (cl-second args))
                           (cl-second args)))
           (args (if docstring
                     (cddr args)
                   (cdr args)))
           (tags (sly-tests-auto-tags)))
      `(ert-deftest ,name () ,(or docstring "No docstring for this test.")
                    :tags ',tags
                    ,@args)))

  (defun sly-test-ert-test-for (name input i doc _body fails-for style fname)
    `(define-sly-ert-test
       ,(intern (format "%s-%d" name i)) ()
       ,(format "For input %s, %s" (truncate-string-to-width
                                    (format "%s" input)
                                    15 nil nil 'ellipsis)
                (replace-regexp-in-string "^.??\\(\\w+\\)"
                                          (lambda (s) (downcase s))
                                          doc
                                          t))
       ,@(if fails-for
             `(:expected-result
               '(satisfies
                 (lambda (result)
                   (ert-test-result-type-p
                    result
                    (if (cl-find-if
                         (lambda (impl)
                           (unless (listp impl)
                             (setq impl (list impl #'(lambda (&rest _ign) t))))
                           (and (equal (car impl) (sly-lisp-implementation-name))
                                (funcall
                                 (cadr impl)
                                 ;; Appease `version-to-list' for
                                 ;; SBCL.  `version-regexp-alist'
                                 ;; doesn't work here.
                                 (replace-regexp-in-string
                                  "[-._+ ]?[[:alnum:]]\\{7,9\\}$"
                                  "-snapshot"
                                  (sly-lisp-implementation-version))
                                 (caddr impl))))
                         ',fails-for)
                        :failed
                      :passed))))))

       ,@(when style
           `((let ((style (sly-communication-style)))
               (when (not (member style ',style))
                 (sly-skip-test (format "test not applicable for style %s"
                                        style))))))
       (apply #',fname ',input))))

(defmacro def-sly-test (name args doc inputs &rest body)
  "Define a test case.
NAME    ::= SYMBOL | (SYMBOL OPTION*) is a symbol naming the test.
OPTION  ::= (:fails-for IMPLEMENTATION*) | (:style COMMUNICATION-STYLE*)
ARGS is a lambda-list.
DOC is a docstring.
INPUTS is a list of argument lists, each tested separately.
BODY is the test case. The body can use `sly-check' to test
conditions (assertions)."
  (declare (debug (&define name sexp sexp sexp &rest def-form))
           (indent 4))
  (if (not (featurep 'ert))
      (warn "No ert.el found: not defining test %s"
            name)
    `(progn
       ,@(cl-destructuring-bind (name &rest options)
             (if (listp name) name (list name))
           (let ((fname (intern (format "sly-test-%s" name))))
             (cons `(defun ,fname ,args
                      (sly-sync-to-top-level 0.3)
                      ,@body
                      (sly-sync-to-top-level 0.3))
                   (cl-loop for input in (eval inputs)
                            for i from 1
                            with fails-for = (cdr (assoc :fails-for options))
                            with style = (cdr (assoc :style options))
                            collect (sly-test-ert-test-for name
                                                             input
                                                             i
                                                             doc
                                                             body
                                                             fails-for
                                                             style
                                                             fname))))))))

(defmacro sly-check (check &rest body)
  (declare (indent defun))
  `(unless (progn ,@body)
     (ert-fail ,(cl-etypecase check
                  (cons `(concat "Ooops, " ,(cons 'format check)))
                  (string `(concat "Check failed: " ,check))
                  (symbol `(concat "Check failed: " ,(symbol-name check)))))))


;;;;; Test case definitions
(defun sly-check-top-level () ;(&optional _test-name)
  (accept-process-output nil 0.001)
  (sly-check "At the top level (no debugging or pending RPCs)"
    (sly-at-top-level-p)))

(defun sly-at-top-level-p ()
  (and (not (sly-db-get-default-buffer))
       (null (sly-rex-continuations))))

(defun sly-wait-condition (name predicate timeout &optional cleanup)
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (while (not (funcall predicate))
      (sly-message "waiting for condition: %s [%s]" name
                   (format-time-string "%H:%M:%S.%6N"))
      (cond ((time-less-p end (current-time))
             (unwind-protect
                 (error "Timeout waiting for condition: %S" name)
               (funcall cleanup)))
            (t
             ;; XXX if a process-filter enters a recursive-edit, we
             ;; hang forever
             (accept-process-output nil 0.1))))))

(defun sly-sync-to-top-level (timeout)
  (sly-wait-condition "top-level" #'sly-at-top-level-p timeout
                      (lambda ()
                        (let ((sly-db-buffer
                               (sly-db-get-default-buffer)))
                          (when (bufferp sly-db-buffer)
                            (with-current-buffer sly-db-buffer
                              (sly-db-quit)))))))

;; XXX: unused function
(defun sly-check-sly-db-level (expected)
  (let ((sly-db-level (let ((sly-db (sly-db-get-default-buffer)))
		      (if sly-db
			  (with-current-buffer sly-db
			    sly-db-level)))))
    (sly-check ("SLY-DB level (%S) is %S" expected sly-db-level)
      (equal expected sly-db-level))))

(defun sly-test-expect (_name expected actual &optional test)
  (when (stringp expected) (setq expected (substring-no-properties expected)))
  (when (stringp actual)   (setq actual (substring-no-properties actual)))
  (if test
      (should (funcall test expected actual))
    (should (equal expected actual))))

(defun sly-db-level ()
  (let ((sly-db (sly-db-get-default-buffer)))
    (if sly-db
	(with-current-buffer sly-db
	  sly-db-level))))

(defun sly-sly-db-level= (level)
  (equal level (sly-db-level)))

(eval-when-compile
 (defvar sly-test-symbols
   '(("foobar") ("foo@bar") ("@foobar") ("foobar@") ("\\@foobar")
     ("|asdf||foo||bar|")
     ("\\#<Foo@Bar>")
     ("\\(setf\\ car\\)"))))

(defun sly-check-symbol-at-point (prefix symbol suffix)
  ;; We test that `sly-symbol-at-point' works at every
  ;; character of the symbol name.
  (with-temp-buffer
    (lisp-mode)
    (insert prefix)
    (let ((start (point)))
      (insert symbol suffix)
      (dotimes (i (length symbol))
        (goto-char (+ start i))
        (sly-test-expect (format "Check `%s' (at %d)..."
                                   (buffer-string) (point))
                           symbol
                           (sly-symbol-at-point)
                           #'equal)))))



(def-sly-test symbol-at-point.2 (sym)
  "fancy symbol-name _not_ at BOB/EOB"
  sly-test-symbols
  (sly-check-symbol-at-point "(foo " sym " bar)"))

(def-sly-test symbol-at-point.3 (sym)
  "fancy symbol-name with leading ,"
  (cl-remove-if (lambda (s) (eq (aref (car s) 0) ?@)) sly-test-symbols)
  (sly-check-symbol-at-point "," sym ""))

(def-sly-test symbol-at-point.4 (sym)
  "fancy symbol-name with leading ,@"
  sly-test-symbols
  (sly-check-symbol-at-point ",@" sym ""))

(def-sly-test symbol-at-point.5 (sym)
  "fancy symbol-name with leading `"
  sly-test-symbols
  (sly-check-symbol-at-point "`" sym ""))

(def-sly-test symbol-at-point.6 (sym)
  "fancy symbol-name wrapped in ()"
  sly-test-symbols
  (sly-check-symbol-at-point "(" sym ")"))

(def-sly-test symbol-at-point.7 (sym)
  "fancy symbol-name wrapped in #< {DEADBEEF}>"
  sly-test-symbols
  (sly-check-symbol-at-point "#<" sym " {DEADBEEF}>"))

;;(def-sly-test symbol-at-point.8 (sym)
;;  "fancy symbol-name wrapped in #<>"
;;  sly-test-symbols
;;  (sly-check-symbol-at-point "#<" sym ">"))

(def-sly-test symbol-at-point.9 (sym)
  "fancy symbol-name wrapped in #| ... |#"
  sly-test-symbols
  (sly-check-symbol-at-point "#|\n" sym "\n|#"))

(def-sly-test symbol-at-point.10 (sym)
  "fancy symbol-name after #| )))(( |# (1)"
  sly-test-symbols
  (sly-check-symbol-at-point "#| )))(( #|\n" sym ""))

(def-sly-test symbol-at-point.11 (sym)
  "fancy symbol-name after #| )))(( |# (2)"
  sly-test-symbols
  (sly-check-symbol-at-point "#| )))(( #|" sym ""))

(def-sly-test symbol-at-point.12 (sym)
  "fancy symbol-name wrapped in \"...\""
  sly-test-symbols
  (sly-check-symbol-at-point "\"\n" sym "\"\n"))

(def-sly-test symbol-at-point.13 (sym)
  "fancy symbol-name wrapped in \" )))(( \" (1)"
  sly-test-symbols
  (sly-check-symbol-at-point "\" )))(( \"\n" sym ""))

(def-sly-test symbol-at-point.14 (sym)
  "fancy symbol-name wrapped in \" )))(( \" (1)"
  sly-test-symbols
  (sly-check-symbol-at-point "\" )))(( \"" sym ""))

(def-sly-test symbol-at-point.15 (sym)
  "symbol-at-point after #."
  sly-test-symbols
  (sly-check-symbol-at-point "#." sym ""))

(def-sly-test symbol-at-point.16 (sym)
  "symbol-at-point after #+"
  sly-test-symbols
  (sly-check-symbol-at-point "#+" sym ""))


(def-sly-test sexp-at-point.1 (string)
  "symbol-at-point after #'"
  '(("foo")
    ("#:foo")
    ("#'foo")
    ("#'(lambda (x) x)")
    ("()"))
  (with-temp-buffer
    (lisp-mode)
    (insert string)
    (goto-char (point-min))
    (sly-test-expect (format "Check sexp `%s' (at %d)..."
                               (buffer-string) (point))
                       string
                       (sly-sexp-at-point)
                       #'equal)))

(def-sly-test narrowing ()
    "Check that narrowing is properly sustained."
    '()
  (sly-check-top-level)
  (let ((random-buffer-name (symbol-name (cl-gensym)))
        (defun-pos) (tmpbuffer))
    (with-temp-buffer
      (dotimes (i 100) (insert (format ";;; %d. line\n" i)))
      (setq tmpbuffer (current-buffer))
      (setq defun-pos (point))
      (insert (concat "(defun __foo__ (x y)" "\n"
                      "  'nothing)"          "\n"))
      (dotimes (i 100) (insert (format ";;; %d. line\n" (+ 100 i))))
      (sly-check "Checking that newly created buffer is not narrowed."
        (not (buffer-narrowed-p)))

      (goto-char defun-pos)
      (narrow-to-defun)
      (sly-check "Checking that narrowing succeeded."
       (buffer-narrowed-p))

      (sly-with-popup-buffer (random-buffer-name)
        (sly-check ("Checking that we're in Sly's temp buffer `%s'"
                      random-buffer-name)
          (equal (buffer-name (current-buffer)) random-buffer-name)))
      (with-current-buffer random-buffer-name
        ;; Notice that we cannot quit the buffer within the extent
        ;; of sly-with-output-to-temp-buffer.
        (quit-window t))
      (sly-check ("Checking that we've got back from `%s'"
                    random-buffer-name)
        (and (eq (current-buffer) tmpbuffer)
             (= (point) defun-pos)))

      (sly-check "Checking that narrowing sustained \
after quitting Sly's temp buffer."
        (buffer-narrowed-p))

      (let ((sly-buffer-package "SLYNK")
            (symbol '*buffer-package*))
        (sly-edit-definition (symbol-name symbol))
        (sly-check ("Checking that we've got M-. into slynk.lisp. %S" symbol)
          (string= (file-name-nondirectory (buffer-file-name))
                   "slynk.lisp"))
        (sly-pop-find-definition-stack)
        (sly-check ("Checking that we've got back.")
          (and (eq (current-buffer) tmpbuffer)
               (= (point) defun-pos)))

        (sly-check "Checking that narrowing sustained after M-,"
          (buffer-narrowed-p)))
      ))
  (sly-check-top-level))

(defun sly-test--pos-at-line (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-beginning-position)))

(def-sly-test recenter
    (pos-line target expected-window-start)
    "Test `sly-recenter'."
    ;; numbers are actually lines numbers
    '(;; region visible, point in region
      (2 4 1)
      ;; end not visible
      (2 (+ wh 2) 2)
      ;; end and start not visible
      ((+ wh 2) (+ wh 500) (+ wh 2)))
  (when noninteractive
    (sly-skip-test "Can't test sly-recenter in batch mode"))
  (with-temp-buffer
    (cl-loop for i from 1 upto 1000
             do (insert (format "%09d\n" i)))
    (let* ((win (display-buffer (current-buffer))))
      (cl-flet ((eval-with-wh (form)
                              (eval `(let ((wh ,(window-text-height win)))
                                       ,form))))
        (with-selected-window win
          (goto-char (sly-test--pos-at-line (eval-with-wh pos-line)))
          (sly-recenter (sly-test--pos-at-line (eval-with-wh target)))
          (redisplay)
          (should (= (eval-with-wh expected-window-start)
                     (line-number-at-pos (window-start)))))))))

(def-sly-test find-definition
    (name buffer-package snippet)
    "Find the definition of a function or macro in slynk.lisp."
    '(("start-server" "SLYNK" "(defun start-server ")
      ("slynk::start-server" "CL-USER" "(defun start-server ")
      ("slynk:start-server" "CL-USER" "(defun start-server ")
      ("slynk::connection" "CL-USER" "(defstruct (connection")
      ("slynk::*emacs-connection*" "CL-USER" "(defvar \\*emacs-connection\\*")
      )
  (switch-to-buffer "*scratch*")        ; not buffer of definition
  (sly-check-top-level)
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))
        (enable-local-variables nil)    ; don't get stuck on -*- eval: -*-
        (sly-buffer-package buffer-package))
    (sly-edit-definition name)
    ;; Postconditions
    (sly-check ("Definition of `%S' is in slynk.lisp." name)
      (string= (file-name-nondirectory (buffer-file-name)) "slynk.lisp"))
    (sly-check ("Looking at '%s'." snippet) (looking-at snippet))
    (sly-pop-find-definition-stack)
    (sly-check "Returning from definition restores original buffer/position."
      (and (eq orig-buffer (current-buffer))
           (= orig-pos (point)))))
    (sly-check-top-level))

(def-sly-test (find-definition.2 (:fails-for "allegro" "lispworks"))
    (buffer-content buffer-package snippet)
    "Check that we're able to find definitions even when
confronted with nasty #.-fu."
    '(("#.(prog1 nil (defvar *foobar* 42))

       (defun .foo. (x)
         (+ x #.*foobar*))

       #.(prog1 nil (makunbound '*foobar*))
       "
       "SLYNK"
       "[ \t]*(defun .foo. "
       )
      ("#.(prog1 nil (defvar *foobar* 42))

       ;; some comment
       (defun .foo. (x)
         (+ x #.*foobar*))

       #.(prog1 nil (makunbound '*foobar*))
       "
       "SLYNK"
       "[ \t]*(defun .foo. "
       )
      ("(in-package slynk)
(eval-when (:compile-toplevel) (defparameter *bar* 456))
(eval-when (:load-toplevel :execute) (makunbound '*bar*))
(defun bar () #.*bar*)
(defun .foo. () 123)"
"SLYNK"
"[ \t]*(defun .foo. () 123)"))
  (let ((sly-buffer-package buffer-package))
    (with-temp-buffer
      (insert buffer-content)
      (sly-check-top-level)
      (sly-eval
       `(slynk:compile-string-for-emacs
         ,buffer-content
         ,(buffer-name)
         '((:position 0) (:line 1 1))
         ,nil
         ,nil))
      (let ((bufname (buffer-name)))
        (sly-edit-definition ".foo.")
        (sly-check ("Definition of `.foo.' is in buffer `%s'." bufname)
          (string= (buffer-name) bufname))
        (sly-check "Definition now at point." (looking-at snippet)))
      )))

(def-sly-test (find-definition.3
                 (:fails-for "abcl" "allegro" "clisp" "lispworks"
                             ("sbcl" version< "1.3.0")
                             "ecl"))
    (name source regexp)
    "Extra tests for defstruct."
    '(("slynk::foo-struct"
       "(progn
  (defun foo-fun ())
  (defstruct (foo-struct (:constructor nil) (:predicate nil)))
)"
       "(defstruct (foo-struct"))
  (switch-to-buffer "*scratch*")
    (with-temp-buffer
      (insert source)
      (let ((sly-buffer-package "SLYNK"))
        (sly-eval
         `(slynk:compile-string-for-emacs
           ,source
           ,(buffer-name)
           '((:position 0) (:line 1 1))
           ,nil
           ,nil)))
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer "*scratch*"
          (sly-edit-definition name)
          (sly-check ("Definition of %S is in buffer `%s'."
                        name temp-buffer)
            (eq (current-buffer) temp-buffer))
          (sly-check "Definition now at point." (looking-at regexp)))
      )))

(def-sly-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p"
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" (nil ""))
      ("slynk::compile-file" (("slynk::compile-file"
                               "slynk::compile-file-for-emacs"
                               "slynk::compile-file-if-needed"
                               "slynk::compile-file-output"
                               "slynk::compile-file-pathname")
                              "slynk::compile-file"))
      ("cl:m-v-l" (nil "")))
  (let ((completions (sly-simple-completions prefix)))
    (sly-test-expect "Completion set" expected-completions completions)))

(def-sly-test flex-complete-symbol
    (prefix expectations)
    "Find the flex completions of a symbol-name prefix."
    '(("m-v-b" (("multiple-value-bind" 1)))
      ("mvbind" (("multiple-value-bind" 1)))
      ("mvcall" (("multiple-value-call" 1)))
      ("mvlist" (("multiple-value-list" 3)))
      ("echonumberlist" (("slynk:*echo-number-alist*" 1))))
  (let* ((sly-buffer-package "COMMON-LISP")
         (completions (car (sly-flex-completions prefix))))
    (cl-loop for (completion before-or-at) in expectations
             for pos = (cl-position completion completions :test #'string=)
             unless pos
             do (ert-fail (format "Didn't find %s in the completions for %s" completion prefix))
             unless (< pos before-or-at)
             do (ert-fail (format "Expected to find %s in the first %s completions for %s, but it came in %s
=> %s"
                                  completion before-or-at prefix (1+ pos)
                                  (cl-subseq completions 0 (1+ pos)))))))

(def-sly-test basic-completion
  (input-keys expected-result)
  "Test `sly-read-from-minibuffer' with INPUT-KEYS as events."
  '(("( r e v e TAB TAB SPC ' ( 1 SPC 2 SPC 3 ) ) RET"
     "(reverse '(1 2 3))")
    ("( c l : c o n TAB s t a n t l TAB TAB SPC 4 2 ) RET"
     "(cl:constantly 42)"))
  (when noninteractive
    (sly-skip-test "Can't use unread-command-events in batch mode"))
  (setq unread-command-events (listify-key-sequence (kbd input-keys)))
  (let ((actual-result (sly-read-from-minibuffer "Test: ")))
    (sly-test-expect "Completed string" expected-result actual-result)))

(def-sly-test arglist
    ;; N.B. Allegro apparently doesn't return the default values of
    ;; optional parameters. Thus the regexp in the start-server
    ;; expected value. In a perfect world we'd find a way to smooth
    ;; over this difference between implementations--perhaps by
    ;; convincing Franz to provide a function that does what we want.
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("slynk::operator-arglist" "(slynk::operator-arglist name package)")
      ("slynk::compute-backtrace" "(slynk::compute-backtrace start end)")
      ("slynk::emacs-connected" "(slynk::emacs-connected)")
      ("slynk::compile-string-for-emacs"
       "(slynk::compile-string-for-emacs \
string buffer position filename policy)")
      ("slynk::connection-socket-io"
       "(slynk::connection-socket-io \
\\(struct\\(ure\\)?\\|object\\|instance\\|x\\|connection\\))")
      ("cl:lisp-implementation-type" "(cl:lisp-implementation-type)")
      ("cl:class-name"
       "(cl:class-name \\(class\\|object\\|instance\\|structure\\))"))
  (let ((arglist (sly-eval `(slynk:operator-arglist ,function-name
                                                      "slynk"))))
    (sly-test-expect "Argument list is as expected"
                       expected-arglist (and arglist (downcase arglist))
                       (lambda (pattern arglist)
                         (and arglist (string-match pattern arglist))))))

(defun sly-test--compile-defun (program subform)
  (sly-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert program)
    (let ((font-lock-verbose nil))
      (setq sly-buffer-package ":slynk")
      (sly-compile-string (buffer-string) 1)
      (setq sly-buffer-package ":cl-user")
      (sly-sync-to-top-level 5)
      (goto-char (point-max))
      (call-interactively 'sly-previous-note)
      (sly-check error-location-correct
        (equal (read (current-buffer)) subform))))
  (sly-check-top-level))

(def-sly-test (compile-defun (:fails-for "allegro" "lispworks" "clisp"))
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that the EXPECTED subform is correctly located."
    '(("(defun cl-user::foo () (cl-user::bar))" (cl-user::bar))
      ("(defun cl-user::foo ()
          #\\space
          ;;Sdf
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
             #+(or)skipped
             #| #||#
                #||# |#
             (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          \"\\\" bla bla \\\"\"
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          #.*log-events*
          (cl-user::bar))"
       (cl-user::bar))
      ("#.'(defun x () (/ 1 0))
        (defun foo ()
           (cl-user::bar))

        "
       (cl-user::bar)))
  (sly-test--compile-defun program subform))

;; This test ideally would be collapsed into the previous
;; compile-defun test, but only 1 case fails for ccl--and that's here
(def-sly-test (compile-defun-with-reader-conditionals
               (:fails-for "allegro" "lispworks" "clisp" "ccl"))
    (program expected)
    "Compile PROGRAM containing errors.
Confirm that the EXPECTED subform is correctly located."
    '(("(defun foo ()
          #+#.'(:and) (/ 1 0))"
       (/ 1 0)))
  (sly-test--compile-defun program expected))

;; SBCL used to pass this one but since 1.2.2 the backquote/unquote
;; reader was changed. See
;; https://bugs.launchpad.net/sbcl/+bug/1361502
(def-sly-test (compile-defun-with-backquote
               (:fails-for "sbcl" "allegro" "lispworks" "clisp"))
  (program subform)
  "Compile PROGRAM containing errors.
Confirm that SUBFORM is correctly located."
  '(("(defun cl-user::foo ()
           (list `(1 ,(random 10) 2 ,@(make-list (random 10)) 3
                     ,(cl-user::bar))))"
     (cl-user::bar)))
  (sly-test--compile-defun program subform))

(def-sly-test (compile-file (:fails-for "allegro" "lispworks" "clisp"))
    (string)
    "Insert STRING in a file, and compile it."
    `((,(pp-to-string '(defun foo () nil))))
  (let ((filename "/tmp/sly-tmp-file.lisp"))
    (with-temp-file filename
      (insert string))
    (let ((cell (cons nil nil)))
      (sly-eval-async
       `(slynk:compile-file-for-emacs ,filename nil)
       (sly-rcurry (lambda (result cell)
                       (setcar cell t)
                       (setcdr cell result))
                     cell))
      (sly-wait-condition "Compilation finished" (lambda () (car cell))
                            0.5)
      (let ((result (cdr cell)))
        (sly-check "Compilation successfull"
          (eq (sly-compilation-result.successp result) t))))))

(def-sly-test utf-8-source
    (input output)
    "Source code containing utf-8 should work"
    (list (let*  ((bytes "\343\201\212\343\201\257\343\202\210\343\201\206")
                  ;;(encode-coding-string (string #x304a #x306f #x3088 #x3046)
                  ;;                      'utf-8)
                  (string (decode-coding-string bytes 'utf-8-unix)))
            (cl-assert (equal bytes (encode-coding-string string 'utf-8-unix)))
            (list (concat "(defun cl-user::foo () \"" string "\")")
                  string)))
  (sly-eval `(cl:eval (cl:read-from-string ,input)))
  (sly-test-expect "Eval result correct"
                     output (sly-eval '(cl-user::foo)))
  (let ((cell (cons nil nil)))
    (let ((hook (sly-curry (lambda (cell &rest _) (setcar cell t)) cell)))
      (add-hook 'sly-compilation-finished-hook hook)
      (unwind-protect
          (progn
            (sly-compile-string input 0)
            (sly-wait-condition "Compilation finished"
                                  (lambda () (car cell))
                                  0.5)
            (sly-test-expect "Compile-string result correct"
                               output (sly-eval '(cl-user::foo))))
        (remove-hook 'sly-compilation-finished-hook hook))
      (let ((filename "/tmp/sly-tmp-file.lisp"))
        (setcar cell nil)
        (add-hook 'sly-compilation-finished-hook hook)
        (unwind-protect
            (with-temp-buffer
              (when (fboundp 'set-buffer-multibyte)
                (set-buffer-multibyte t))
              (setq buffer-file-coding-system 'utf-8-unix)
              (setq buffer-file-name filename)
              (insert ";; -*- coding: utf-8-unix -*- \n")
              (insert input)
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region nil nil filename nil t))
              (let ((sly-load-failed-fasl 'always))
                (sly-compile-and-load-file)
                (sly-wait-condition "Compilation finished"
                                      (lambda () (car cell))
                                      0.5))
              (sly-test-expect "Compile-file result correct"
                                 output (sly-eval '(cl-user::foo))))
          (remove-hook 'sly-compilation-finished-hook hook)
          (ignore-errors (delete-file filename)))))))

(def-sly-test async-eval-debugging (depth)
  "Test recursive debugging of asynchronous evaluation requests."
  '((1) (2) (3))
  (let ((depth depth)
        (debug-hook-max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sly-db-get-default-buffer)
               (when (> sly-db-level debug-hook-max-depth)
                 (setq debug-hook-max-depth sly-db-level)
                 (if (= sly-db-level depth)
                     ;; We're at maximum recursion - time to unwind
                     (sly-db-quit)
                   ;; Going down - enter another recursive debug
                   ;; Recursively debug.
                   (sly-eval-async '(error))))))))
      (let ((sly-db-hook (cons debug-hook sly-db-hook)))
        (sly-eval-async '(error))
        (sly-sync-to-top-level 5)
        (sly-check ("Maximum depth reached (%S) is %S."
                      debug-hook-max-depth depth)
          (= debug-hook-max-depth depth))))))

(def-sly-test unwind-to-previous-sly-db-level (level2 level1)
  "Test recursive debugging and returning to lower SLY-DB levels."
  '((2 1) (4 2))
  (sly-check-top-level)
  (let ((level2 level2)
        (level1 level1)
        (state 'enter)
        (max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sly-db-get-default-buffer)
               (setq max-depth (max sly-db-level max-depth))
               (cl-ecase state
                 (enter
                  (cond ((= sly-db-level level2)
                         (setq state 'leave)
                         (sly-db-invoke-restart (sly-db-first-abort-restart)))
                        (t
                         (sly-eval-async `(cl:aref cl:nil ,sly-db-level)))))
                 (leave
                  (cond ((= sly-db-level level1)
                         (setq state 'ok)
                         (sly-db-quit))
                        (t
                         (sly-db-invoke-restart (sly-db-first-abort-restart))
                         ))))))))
      (let ((sly-db-hook (cons debug-hook sly-db-hook)))
        (sly-eval-async `(cl:aref cl:nil 0))
        (sly-sync-to-top-level 15)
        (sly-check-top-level)
        (sly-check ("Maximum depth reached (%S) is %S." max-depth level2)
          (= max-depth level2))
        (sly-check ("Final state reached.")
          (eq state 'ok))))))

(defun sly-db-first-abort-restart ()
  (let ((case-fold-search t))
    (cl-position-if (lambda (x) (string-match "abort" (car x))) sly-db-restarts)))

(def-sly-test loop-interrupt-quit
    ()
    "Test interrupting a loop."
    '(())
  (sly-check-top-level)
  (sly-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (accept-process-output nil 1)
  (sly-check "In eval state." (sly-busy-p))
  (sly-interrupt)
  (sly-wait-condition "First interrupt" (lambda () (sly-sly-db-level= 1)) 5)
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-quit))
  (sly-sync-to-top-level 5)
  (sly-check-top-level))

(def-sly-test loop-interrupt-continue-interrupt-quit
    ()
    "Test interrupting a previously interrupted but continued loop."
    '(())
  (sly-check-top-level)
  (sly-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (sleep-for 1)
  (sly-wait-condition "running" #'sly-busy-p 5)
  (sly-interrupt)
  (sly-wait-condition "First interrupt" (lambda () (sly-sly-db-level= 1)) 5)
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-continue))
  (sly-wait-condition "running" (lambda ()
                                    (and (sly-busy-p)
                                         (not (sly-db-get-default-buffer)))) 5)
  (sly-interrupt)
  (sly-wait-condition "Second interrupt" (lambda () (sly-sly-db-level= 1)) 5)
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-quit))
  (sly-sync-to-top-level 5)
  (sly-check-top-level))

(def-sly-test interactive-eval
    ()
    "Test interactive eval and continuing from the debugger."
    '(())
  (sly-check-top-level)
  (let ((sly-db-hook (lambda ()
                       (sly-db-continue))))
    (sly-interactive-eval
     "(progn\
 (cerror \"foo\" \"restart\")\
 (cerror \"bar\" \"restart\")\
 (+ 1 2))")
    (sly-sync-to-top-level 5)
    (current-message))
  (unless noninteractive
    (should (equal "=> 3 (2 bits, #x3, #o3, #b11)"
                   (current-message)))))

(def-sly-test report-condition-with-circular-list
    (format-control format-argument)
    "Test conditions involving circular lists."
    '(("~a" "(let ((x (cons nil nil))) (setf (cdr x) x))")
      ("~a" "(let ((x (cons nil nil))) (setf (car x) x))")
      ("~a" "(let ((x (cons (make-string 100000 :initial-element #\\X) nil)))\
                (setf (cdr x) x))"))
  (sly-check-top-level)
  (let ((done nil))
    (let ((sly-db-hook (lambda () (sly-db-continue) (setq done t))))
      (sly-interactive-eval
       (format "(with-standard-io-syntax (cerror \"foo\" \"%s\" %s) (+ 1 2))"
               format-control format-argument))
      (while (not done) (accept-process-output))
      (sly-sync-to-top-level 5)
      (sly-check-top-level)
      (unless noninteractive
        (let ((message (current-message)))
          (sly-check "Minibuffer contains: \"3\""
            (equal "=> 3 (2 bits, #x3, #o3, #b11)" message)))))))

(def-sly-test interrupt-bubbling-idiot
    ()
    "Test interrupting a loop that sends a lot of output to Emacs."
    '(())
  (accept-process-output nil 1)
  (sly-check-top-level)
  (sly-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i)
                                                           (cl:finish-output)))
                    (lambda (_) )
                    "CL-USER")
  (sleep-for 1)
  (sly-interrupt)
  (sly-wait-condition "Debugger visible"
                        (lambda ()
                          (and (sly-sly-db-level= 1)
                               (get-buffer-window (sly-db-get-default-buffer))))
                        30)
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-quit))
  (sly-sync-to-top-level 5))

(def-sly-test (interrupt-encode-message (:style :sigio))
    ()
    "Test interrupt processing during slynk::encode-message"
    '(())
  (sly-eval-async '(cl:loop :for i :from 0
                              :do (slynk::background-message "foo ~d" i)))
  (sleep-for 1)
  (sly-eval-async '(cl:/ 1 0))
  (sly-wait-condition "Debugger visible"
                        (lambda ()
                          (and (sly-sly-db-level= 1)
                               (get-buffer-window (sly-db-get-default-buffer))))
                        30)
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-quit))
  (sly-sync-to-top-level 5))

(def-sly-test inspector
    (exp)
    "Test basic inspector workingness."
    '(((let ((h (make-hash-table)))
         (loop for i below 10 do (setf (gethash i h) i))
         h))
      ((make-array 10))
      ((make-list 10))
      ('cons)
      (#'cons))
  (sly-inspect (prin1-to-string exp))
  (cl-assert (not (sly-inspector-visible-p)))
  (sly-wait-condition "Inspector visible" #'sly-inspector-visible-p 5)
  (with-current-buffer (window-buffer (selected-window))
    (sly-inspector-quit))
  (sly-wait-condition "Inspector closed"
                        (lambda () (not (sly-inspector-visible-p)))
                        5)
  (sly-sync-to-top-level 1))

(defun sly-buffer-visible-p (name)
  (let ((buffer (window-buffer (selected-window))))
    (string-match name (buffer-name buffer))))

(defun sly-inspector-visible-p ()
  (sly-buffer-visible-p (sly-buffer-name :inspector :connection t)))

(defun sly-execute-as-command (name)
  "Execute `name' as if it was done by the user through the
Command Loop. Similiar to `call-interactively' but also pushes on
the buffer's undo-list."
  (undo-boundary)
  (call-interactively name))

(def-sly-test macroexpand
    (macro-defs bufcontent expansion1 search-str expansion2)
    "foo"
    '((("(defmacro qwertz (&body body) `(list :qwertz ',body))"
        "(defmacro yxcv (&body body) `(list :yxcv (qwertz ,@body)))")
       "(yxcv :A :B :C)"
       "(list :yxcv (qwertz :a :b :c))"
       "(qwertz"
       "(list :yxcv (list :qwertz '(:a :b :c)))"))
  (sly-check-top-level)
  (setq sly-buffer-package ":slynk")
  (with-temp-buffer
    (lisp-mode)
    (dolist (def macro-defs)
      (sly-compile-string def 0)
      (sly-sync-to-top-level 5))
    (insert bufcontent)
    (goto-char (point-min))
    (sly-execute-as-command 'sly-macroexpand-1)
    (sly-wait-condition "Macroexpansion buffer visible"
                          (lambda ()
                            (sly-buffer-visible-p
                             (sly-buffer-name :macroexpansion)))
                          5)
    (with-current-buffer (get-buffer (sly-buffer-name :macroexpansion))
      (sly-test-expect "Initial macroexpansion is correct"
                         expansion1
                         (downcase (buffer-string))
                         #'sly-test-macroexpansion=)
      (search-forward search-str)
      (backward-up-list)
      (sly-execute-as-command 'sly-macroexpand-1-inplace)
      (sly-sync-to-top-level 3)
      (sly-test-expect "In-place macroexpansion is correct"
                         expansion2
                         (downcase (buffer-string))
                         #'sly-test-macroexpansion=)
      (sly-execute-as-command 'sly-macroexpand-undo)
      (sly-test-expect "Expansion after undo is correct"
                         expansion1
                         (downcase (buffer-string))
                         #'sly-test-macroexpansion=)))
    (setq sly-buffer-package ":cl-user"))

(defun sly-test-macroexpansion= (string1 string2 &optional ignore-case)
  (let ((string1 (replace-regexp-in-string " *\n *" " " string1))
        (string2 (replace-regexp-in-string " *\n *" " " string2)))
    (compare-strings string1 nil nil
                     string2 nil nil
                     ignore-case)))

(def-sly-test indentation (buffer-content point-markers)
        "Check indentation update to work correctly."
    '(("
\(in-package :slynk)

\(defmacro with-lolipop (&body body)
  `(progn ,@body))

\(defmacro lolipop (&body body)
  `(progn ,@body))

\(with-lolipop
  1
  2
  42)

\(lolipop
  1
  2
  23)
"
       ("23" "42")))
  (with-temp-buffer
    (lisp-mode)
    (sly-editing-mode 1)
    (insert buffer-content)
    (sly-compile-region (point-min) (point-max))
    (sly-sync-to-top-level 3)
    (sly-update-indentation)
    (sly-sync-to-top-level 3)
    (dolist (marker point-markers)
      (search-backward marker)
      (beginning-of-defun)
      (indent-region (point) (progn (end-of-defun) (point))))
    (sly-test-expect "Correct buffer content"
                       buffer-content
                       (substring-no-properties (buffer-string)))))

(def-sly-test break
    (times exp)
    "Test whether BREAK invokes SLY-DB."
    (let ((exp1 '(break)))
      `((1 ,exp1) (2 ,exp1) (3 ,exp1)))
  (accept-process-output nil 0.2)
  (sly-check-top-level)
  (sly-eval-async
   `(cl:eval (cl:read-from-string
              ,(prin1-to-string `(dotimes (i ,times)
                                   (unless (= i 0)
                                     (slynk::sleep-for 1))
                                   ,exp)))))
  (dotimes (_i times)
    (sly-wait-condition "Debugger visible"
                          (lambda ()
                            (and (sly-sly-db-level= 1)
                                 (get-buffer-window
                                  (sly-db-get-default-buffer))))
                          3)
    (with-current-buffer (sly-db-get-default-buffer)
      (sly-db-continue))
    (sly-wait-condition "sly-db closed"
                          (lambda () (not (sly-db-get-default-buffer)))
                          0.5))
  (sly-sync-to-top-level 1))

(def-sly-test (break2 (:fails-for "cmucl" "allegro"))
    (times exp)
    "Backends should arguably make sure that BREAK does not depend
on *DEBUGGER-HOOK*."
    (let ((exp2
           '(block outta
              (let ((*debugger-hook* (lambda (c h) (return-from outta 42))))
                (break)))))
      `((1 ,exp2) (2 ,exp2) (3 ,exp2)))
  (sly-test-break times exp))

(def-sly-test locally-bound-debugger-hook
    ()
    "Test that binding *DEBUGGER-HOOK* locally works properly."
    '(())
  (accept-process-output nil 1)
  (sly-check-top-level)
  (sly-compile-string
   (prin1-to-string `(defun cl-user::quux ()
                       (block outta
                         (let ((*debugger-hook*
                                (lambda (c hook)
                                  (declare (ignore c hook))
                                  (return-from outta 42))))
                           (error "FOO")))))
   0)
  (sly-sync-to-top-level 2)
  (sly-eval-async '(cl-user::quux))
  ;; FIXME: sly-wait-condition returns immediately if the test returns true
  (sly-wait-condition "Checking that Debugger does not popup"
                        (lambda ()
                          (not (sly-db-get-default-buffer)))
                        3)
  (sly-sync-to-top-level 5))

(def-sly-test end-of-file
    (expr)
    "Signalling END-OF-FILE should invoke the debugger."
    '(((cl:read-from-string ""))
      ((cl:error 'cl:end-of-file)))
  (let ((value (sly-eval
                `(cl:let ((condition nil))
                         (cl:with-simple-restart
                          (cl:continue "continue")
                          (cl:let ((cl:*debugger-hook*
                                    (cl:lambda (c h)
                                               (cl:setq condition c)
                                               (cl:continue))))
                                  ,expr))
                         (cl:and (cl:typep condition 'cl:end-of-file))))))
    (sly-test-expect "Debugger invoked" t value)))

(def-sly-test interrupt-at-toplevel
    ()
    "Let's see what happens if we send a user interrupt at toplevel."
    '(())
  (sly-check-top-level)
  (unless (and (eq (sly-communication-style) :spawn)
               (not (featurep 'sly-repl)))
    (sly-interrupt)
    (sly-wait-condition
     "Debugger visible"
     (lambda ()
       (and (sly-sly-db-level= 1)
            (get-buffer-window (sly-db-get-default-buffer))))
     5)
    (with-current-buffer (sly-db-get-default-buffer)
      (sly-db-quit))
    (sly-sync-to-top-level 5)))

(def-sly-test interrupt-in-debugger (interrupts continues)
    "Let's see what happens if we interrupt the debugger.
INTERRUPTS ... number of nested interrupts
CONTINUES  ... how often the continue restart should be invoked"
    '((1 0) (2 1) (4 2))
  (sly-check "No debugger" (not (sly-db-get-default-buffer)))
  (when (and (eq (sly-communication-style) :spawn)
             (not (featurep 'sly-repl)))
    (sly-eval-async '(slynk::without-sly-interrupts
                        (slynk::receive)))
    (sit-for 0.2))
  (dotimes (i interrupts)
    (sly-interrupt)
    (let ((level (1+ i)))
      (sly-wait-condition (format "Debug level %d reachend" level)
                            (lambda () (equal (sly-db-level) level))
                            2)))
  (dotimes (i continues)
    (with-current-buffer (sly-db-get-default-buffer)
      (sly-db-continue))
    (let ((level (- interrupts (1+ i))))
      (sly-wait-condition (format "Return to debug level %d" level)
                            (lambda () (equal (sly-db-level) level))
                            2)))
  (with-current-buffer (sly-db-get-default-buffer)
    (sly-db-quit))
  (sly-sync-to-top-level 1))

(def-sly-test flow-control
    (n delay interrupts)
    "Let Lisp produce output faster than Emacs can consume it."
    `((300 0.03 3))
  (when noninteractive
    (sly-skip-test "test is currently unstable"))
  (sly-check "No debugger" (not (sly-db-get-default-buffer)))
  (sly-eval-async `(slynk:flow-control-test ,n ,delay))
  (sleep-for 0.2)
  (dotimes (_i interrupts)
    (sly-interrupt)
    (sly-wait-condition "In debugger" (lambda () (sly-sly-db-level= 1)) 5)
    (sly-check "In debugger" (sly-sly-db-level= 1))
    (with-current-buffer (sly-db-get-default-buffer)
      (sly-db-continue))
    (sly-wait-condition "No debugger" (lambda () (sly-sly-db-level= nil)) 3)
    (sly-check "Debugger closed" (sly-sly-db-level= nil)))
  (sly-sync-to-top-level 10))

(def-sly-test sbcl-world-lock
    (n delay)
    "Print something from *MACROEXPAND-HOOK*.
In SBCL, the compiler grabs a lock which can be problematic because
no method dispatch code can be generated for other threads.
This test will fail more likely before dispatch caches are warmed up."
    '((10 0.03)
      ;;((cl:+ slynk::send-counter-limit 10) 0.03)
      )
  (sly-test-expect "no error"
		     123
		     (sly-eval
		      `(cl:let ((cl:*macroexpand-hook*
				 (cl:lambda (fun form env)
					    (slynk:flow-control-test ,n ,delay)
					    (cl:funcall fun form env))))
			       (cl:eval '(cl:macrolet ((foo () 123))
					   (foo)))))))

(def-sly-test (disconnect-one-connection (:style :spawn)) ()
    "`sly-disconnect' should disconnect only the current connection"
    '(())
  (let ((connection-count (length sly-net-processes))
        (old-connection sly-default-connection)
        (sly-connected-hook nil))
    (unwind-protect
         (let ((sly-dispatching-connection
                (sly-connect "localhost"
                               ;; Here we assume that the request will
                               ;; be evaluated in its own thread.
                               (sly-eval `(slynk:create-server
                                             :port 0 ; use random port
                                             :style :spawn
                                             :dont-close nil)))))
           (sly-sync-to-top-level 3)
           (sly-disconnect)
           (sly-test-expect "Number of connections must remane the same"
                              connection-count
                              (length sly-net-processes)))
      (sly-select-connection old-connection))))

(def-sly-test disconnect-and-reconnect
    ()
    "Close the connetion.
Confirm that the subprocess continues gracefully.
Reconnect afterwards."
    '(())
  (sly-check-top-level)
  (let* ((c (sly-connection))
         (p (sly-inferior-process c)))
    (with-current-buffer (process-buffer p)
      (erase-buffer))
    (delete-process c)
    (cl-assert (equal (process-status c) 'closed) nil "Connection not closed")
    (accept-process-output nil 0.1)
    (cl-assert (equal (process-status p) 'run) nil "Subprocess not running")
    (with-current-buffer (process-buffer p)
      (cl-assert (< (buffer-size) 500) nil "Unusual output"))
    (sly-inferior-connect p (sly-inferior-lisp-args p))
    (let ((hook nil) (p p))
      (setq hook (lambda ()
                   (sly-test-expect
                    "We are connected again" p (sly-inferior-process))
                   (remove-hook 'sly-connected-hook hook)))
      (add-hook 'sly-connected-hook hook)
      (sly-wait-condition "Lisp restarted"
                            (lambda ()
                              (not (member hook sly-connected-hook)))
                            5))))


;;;; SLY-loading tests that launch separate Emacsen
;;;;
(defvar sly-test-check-repl-forms
  `((unless (and (featurep 'sly-mrepl)
                 (assq 'slynk/mrepl sly-contrib--required-slynk-modules))
      (die "`sly-repl' contrib not properly setup"))
    (let ((mrepl-buffer (sly-mrepl--find-buffer)))
      (unless mrepl-buffer
        (die "MREPL buffer not setup!"))
      (with-current-buffer mrepl-buffer
        ;; FIXME: suboptimal: wait one second for the lisp
        ;; to reply.
        (sit-for 1) 
        (unless (and (string-match "^; +SLY" (buffer-string))
                     (string-match "CL-USER> *$" (buffer-string)))
          (die (format "MREPL prompt: %s" (buffer-string))))))))

(defvar sly-test-check-asdf-loader-forms
  `((when (sly-eval '(cl:and (cl:find-package :slynk-loader) t))
      (die "Didn't expect SLY to be loaded with slynk-loader.lisp"))))

(cl-defun sly-test-recipe-test-for
    (&key preflight
          (takeoff   `((call-interactively 'sly)))
          (landing   (append sly-test-check-repl-forms
                             sly-test-check-asdf-loader-forms)))
  (let ((success nil)
        (test-file (make-temp-file "sly-recipe-" nil ".el"))
        (test-forms
         `((require 'cl)
           (labels
               ((die (reason &optional more)
                     (princ reason)
                     (terpri)
                     (and more (pp more))
                     (kill-emacs 254)))
             (condition-case err
                 (progn ,@preflight
                        ,@takeoff
                        ,(when (null landing) '(kill-emacs 0))
                        (add-hook
                         'sly-connected-hook
                         #'(lambda ()
                             (condition-case err
                                 (progn
                                   ,@landing
                                   (kill-emacs 0))
                               (error
                                (die "Unexpected error running landing forms"
                                     err))))
                         t))
               (error
                (die "Unexpected error running preflight/takeoff forms" err)))
             (with-timeout
                 (30
                  (die "Timeout waiting for recipe test to finish."))
               (while t (sit-for 1)))))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (mapc #'insert (mapcar #'pp-to-string test-forms))
            (write-file test-file))
          (with-temp-buffer
            (let ((retval
                   (call-process (concat invocation-directory invocation-name)
                                 nil (list t nil) nil
                                 "-Q" "--batch"
                                 "-l" test-file)))
              (unless (= 0 retval)
                (ert-fail (buffer-string)))))
          (setq success t))
    (if success (delete-file test-file)
      (message "Test failed: keeping %s for inspection" test-file)))))

(define-sly-ert-test readme-recipe ()
  "Test the README.md's autoload recipe."
  (sly-test-recipe-test-for
   :preflight `((add-to-list 'load-path ,sly-path)
                (setq inferior-lisp-program ,inferior-lisp-program)
                (require 'sly-autoloads))))

(define-sly-ert-test traditional-recipe ()
  "Test the README.md's traditional recipe."
  (sly-test-recipe-test-for
   :preflight `((add-to-list 'load-path ,sly-path)
                (setq inferior-lisp-program ,inferior-lisp-program)
                (require 'sly)
                (sly-setup '(sly-fancy)))))

(define-sly-ert-test slynk-loader-fallback ()
  "Test `sly-init-using-slynk-loader'"
  ;; TODO: another useful test would be to test
  ;; `sly-init-using-asdf's fallback to slynk-loader.lisp."
  (sly-test-recipe-test-for
   :preflight `((add-to-list 'load-path ,sly-path)
                (setq inferior-lisp-program ,inferior-lisp-program)
                (require 'sly-autoloads)
                (setq sly-contribs '(sly-fancy))
                (setq sly-init-function 'sly-init-using-slynk-loader)
                (sly-setup '(sly-fancy)))
   :landing `((unless (sly-eval '(cl:and (cl:find-package :slynk-loader) t))
                (die "Expected SLY to be loaded with slynk-loader.lisp"))
              ,@sly-test-check-repl-forms)))


;;; xref recompilation
;;;
(defun sly-test--eval-now (string)
  (cl-second (sly-eval `(slynk:eval-and-grab-output ,string))))

(def-sly-test (sly-recompile-all-xrefs (:fails-for "cmucl")) ()
  "Test recompilation of all references within an xref buffer."
  '(())
  (let* ((cell (cons nil nil))
         (hook (sly-curry (lambda (cell &rest _) (setcar cell t)) cell))
         (filename (make-temp-file "sly-recompile-all-xrefs" nil ".lisp"))
         (xref-buffer))
    (add-hook 'sly-compilation-finished-hook hook)
    (unwind-protect
        (with-temp-file filename
          (set-visited-file-name filename)
          (sly-test--eval-now "(defparameter slynk::*.var.* nil)")
          (insert "(in-package :slynk)
                    (defun .fn1. ())
                    (defun .fn2. () (.fn1.) #.*.var.*)
                    (defun .fn3. () (.fn1.) #.*.var.*)")
          (save-buffer)
          (sly-compile-and-load-file)
          (sly-wait-condition "Compilation finished"
                              (lambda () (car cell))
                              0.5)
          (sly-test--eval-now "(setq *.var.* t)")
          (setcar cell nil)
          (sly-xref :calls ".fn1."
                    (lambda (&rest args)
                      (setq xref-buffer (apply #'sly-xref--show-results args))
                      (setcar cell t)))
          (sly-wait-condition "Xrefs computed and displayed"
                              (lambda () (car cell))
                              0.5)
          (setcar cell nil)
          (should (cl-equalp (list (sly-test--eval-now "(.fn2.)")
                                   (sly-test--eval-now "(.fn3.)"))
                             '("nil" "nil")))
          ;; Recompile now
          ;; 
          (with-current-buffer xref-buffer
            (sly-recompile-all-xrefs)
            (sly-wait-condition "Compilation finished"
                                (lambda () (car cell))
                                0.5))
          (should (cl-equalp (list (sly-test--eval-now "(.fn2.)")
                                   (sly-test--eval-now "(.fn3.)"))
                             '("T" "T"))))
      (remove-hook 'sly-compilation-finished-hook hook)
      (when xref-buffer
        (kill-buffer xref-buffer)))))


;;; window management after M-.
;;;
(cl-defmacro sly-test--with-find-definition-window-checker (fn
                                                            (window-splits
                                                             total-windows
                                                             starting-buffer-sym
                                                             starting-window-sym)
                                                            &rest body)
  (declare (indent 2))
  (let ((temp-frame-sym (cl-gensym "temp-frame-")))
    `(progn
       (sly-check-top-level)
       (let ((,temp-frame-sym nil))
         (unwind-protect
             (progn
               (setq ,temp-frame-sym (if noninteractive
                                         (selected-frame)
                                       (make-frame)))
               ;; too large a frame will exhibit slightly different
               ;; window-popping behaviour
               (set-frame-width ,temp-frame-sym 100)
               (set-frame-height ,temp-frame-sym 40)
               (with-selected-frame ,temp-frame-sym
                 (with-temp-buffer
                   (delete-other-windows)
                   (switch-to-buffer (current-buffer))
                   (let ((,starting-window-sym (selected-window))
                         (,starting-buffer-sym (current-buffer)))
                     (dotimes (_i ,window-splits)
                       (split-window))
                     (funcall ,fn "cl:print-object")
                     (should (= ,total-windows (length (window-list ,temp-frame-sym))))
                     (with-current-buffer
                         (window-buffer (selected-window))
                       (should (eq major-mode 'sly-xref-mode))
                       (forward-line 1)
                       (sly-xref-goto))
                     ,@body))))
           (unless noninteractive
             (delete-frame ,temp-frame-sym t)))))))

(def-sly-test find-definition-same-window (window-splits total-windows)
  "Test `sly-edit-definition' windows"
  '((0 2)
    (1 2)
    (2 3))
  (sly-test--with-find-definition-window-checker
      'sly-edit-definition
      (window-splits
       total-windows
       temp-buffer
       original-window)
    (with-current-buffer
        (window-buffer (selected-window))
      (should-not (eq temp-buffer (current-buffer)))
      (should (eq (selected-window) original-window)))
    (should (= (if (zerop window-splits)
                   1
                 total-windows)
               (length (window-list (selected-frame)))))))

(def-sly-test find-definition-other-window (window-splits total-windows)
  "Test `sly-edit-definition-other-window' windows"
  '((0 2)
    (1 2)
    (2 3))
  (sly-test--with-find-definition-window-checker
      'sly-edit-definition-other-window
      (window-splits
       total-windows
       temp-buffer
       original-window)
    (with-current-buffer
        (window-buffer (selected-window))
      (should (window-live-p original-window))
      (should (eq temp-buffer (window-buffer original-window)))
      (should-not (eq (selected-window) original-window)))
    (should (= total-windows
               (length (window-list (selected-frame)))))))



(provide 'sly-tests)
