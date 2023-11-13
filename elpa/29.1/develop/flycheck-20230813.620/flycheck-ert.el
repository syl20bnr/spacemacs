;;; flycheck-ert.el --- Flycheck: ERT extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Flycheck contributors
;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;; URL: https://github.com/flycheck/flycheck

;; This file is not part of GNU Emacs.

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

;; Unit testing library for Flycheck, the modern on-the-fly syntax checking
;; extension for GNU Emacs.

;; Provide various utility functions and unit test helpers to test Flycheck and
;; Flycheck extensions.

;;; Code:

(require 'flycheck)
(require 'ert)
(require 'macroexp)                     ; For macro utilities


;;; Compatibility

(eval-and-compile
  ;; Provide `ert-skip' and friends for Emacs 24.3
  (defconst flycheck-ert-ert-can-skip (fboundp 'ert-skip)
    "Whether ERT supports test skipping.")

  (unless (fboundp 'define-error)
    ;; from Emacs `subr.el'
    (defun define-error (name message &optional parent)
      "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
      (unless parent (setq parent 'error))
      (let ((conditions
             (if (consp parent)
                 (apply #'append
                        (mapcar
                         (lambda (parent)
                           (cons parent
                                 (or (get parent 'error-conditions)
                                     (error "Unknown signal `%s'" parent))))
                         parent))
               (cons parent (get parent 'error-conditions)))))
        (put name 'error-conditions
             (delete-dups (copy-sequence (cons name conditions))))
        (when message (put name 'error-message message)))))

  (unless flycheck-ert-ert-can-skip
    ;; Fake skipping

    (define-error 'flycheck-ert-skipped "Test skipped")

    (defun ert-skip (data)
      (signal 'flycheck-ert-skipped data))

    (defmacro skip-unless (form)
      `(unless (ignore-errors ,form)
         (signal 'flycheck-ert-skipped ',form)))

    (defun ert-test-skipped-p (result)
      (and (ert-test-failed-p result)
           (eq (car (ert-test-failed-condition result))
               'flycheck-ert-skipped)))))


;;; Internal variables

(defvar flycheck-ert--resource-directory nil
  "The directory to get resources from in this test suite.")


;;; Resource management macros

(defmacro flycheck-ert-with-temp-buffer (&rest body)
  "Eval BODY within a temporary buffer.

Like `with-temp-buffer', but resets the modification state of the
temporary buffer to make sure that it is properly killed even if
it has a backing file and is modified."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (unwind-protect
         ,(macroexp-progn body)
       ;; Reset modification state of the buffer, and unlink it from its backing
       ;; file, if any, because Emacs refuses to kill modified buffers with
       ;; backing files, even if they are temporary.
       (set-buffer-modified-p nil)
       (set-visited-file-name nil 'no-query))))

(defmacro flycheck-ert-with-file-buffer (file-name &rest body)
  "Create a buffer from FILE-NAME and eval BODY.

BODY is evaluated with `current-buffer' being a buffer with the
contents FILE-NAME."
  (declare (indent 1) (debug t))
  `(let ((file-name ,file-name))
     (unless (file-exists-p file-name)
       (error "%s does not exist" file-name))
     (flycheck-ert-with-temp-buffer
       (insert-file-contents file-name 'visit)
       (set-visited-file-name file-name 'no-query)
       (cd (file-name-directory file-name))
       ;; Mark the buffer as not modified, because we just loaded the file up to
       ;; now.
       (set-buffer-modified-p nil)
       ,@body)))

(defmacro flycheck-ert-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards.

Use this macro to test functions that create a Help buffer."
  (declare (indent 0))
  `(unwind-protect
       ,(macroexp-progn body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))

(defmacro flycheck-ert-with-global-mode (&rest body)
  "Execute BODY with Global Flycheck Mode enabled.

After BODY, restore the old state of Global Flycheck Mode."
  (declare (indent 0))
  `(let ((old-state global-flycheck-mode))
     (unwind-protect
         (progn
           (global-flycheck-mode 1)
           ,@body)
       (global-flycheck-mode (if old-state 1 -1)))))

(defmacro flycheck-ert-with-env (env &rest body)
  "Add ENV to `process-environment' in BODY.

Execute BODY with a `process-environment' which contains all
variables from ENV added.

ENV is an alist, where each cons cell `(VAR . VALUE)' is a
environment variable VAR to be added to `process-environment'
with VALUE."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (pcase-dolist (`(,var . ,value) ,env)
       (setenv var value))
     ,@body))


;;; Test resources
(defun flycheck-ert-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-ert--resource-directory'."
  (expand-file-name resource-file flycheck-ert--resource-directory))

(defmacro flycheck-ert-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-ert-resource-filename'."
  (declare (indent 1))
  `(flycheck-ert-with-file-buffer
       (flycheck-ert-resource-filename ,resource-file)
     ,@body))


;;; Test suite initialization

(defun flycheck-ert-initialize (resource-dir)
  "Initialize a test suite with RESOURCE-DIR.

RESOURCE-DIR is the directory, `flycheck-ert-resource-filename'
should use to lookup resource files."
  (when flycheck-ert--resource-directory
    (error "Test suite already initialized"))
  (let ((tests (ert-select-tests t t)))
    ;; Select all tests
    (unless tests
      (error "No tests defined.  \
Call `flycheck-ert-initialize' after defining all tests!"))

    (setq flycheck-ert--resource-directory resource-dir)

    ;; Emacs 24.3 don't support skipped tests, so we add poor man's test
    ;; skipping: We mark skipped tests as expected failures by adjusting the
    ;; expected result of all test cases. Not particularly pretty, but works :)
    (unless flycheck-ert-ert-can-skip
      (dolist (test tests)
        (let ((result (ert-test-expected-result-type test)))
          (setf (ert-test-expected-result-type test)
                `(or ,result (satisfies ert-test-skipped-p))))))))


;;; Test case definitions
(defmacro flycheck-ert-def-checker-test (checker language name
                                                 &rest keys-and-body)
  "Define a test case for a syntax CHECKER for LANGUAGE.

CHECKER is a symbol or a list of symbols denoting syntax checkers
being tested by the test.  The test case is skipped, if any of
these checkers cannot be used.  LANGUAGE is a symbol or a list of
symbols denoting the programming languages supported by the
syntax checkers.  This is currently only used for tagging the
test appropriately.

NAME is a symbol denoting the local name of the test.  The test
itself is ultimately named
`flycheck-define-checker/CHECKER/NAME'.  If CHECKER is a list,
the first checker in the list is used for naming the test.

Optionally, the keyword arguments `:tags' and `:expected-result'
may be given.  They have the same meaning as in `ert-deftest.',
and are added to the tags and result expectations set up by this
macro.

The remaining forms KEYS-AND-BODY denote the body of the test
case, including assertions and setup code."
  (declare (indent 3))
  (unless checker
    (error "No syntax checkers specified"))
  (unless language
    (error "No languages specified"))
  (let* ((checkers (if (symbolp checker) (list checker) checker))
         (checker (car checkers))
         (languages (if (symbolp language) (list language) language))
         (language-tags (mapcar (lambda (l) (intern (format "language-%s" l)))
                                languages))
         (checker-tags (mapcar (lambda (c) (intern (format "checker-%s" c)))
                               checkers))
         (local-name (or name 'default))
         (full-name (intern (format "flycheck-define-checker/%s/%s"
                                    checker local-name)))
         (keys-and-body (ert--parse-keys-and-body keys-and-body))
         (body (cadr keys-and-body))
         (keys (car keys-and-body))
         (default-tags '(syntax-checker external-tool)))
    `(ert-deftest ,full-name ()
       :expected-result ,(or (plist-get keys :expected-result) :passed)
       :tags (append ',(append default-tags language-tags checker-tags)
                     ,(plist-get keys :tags))
       ,@(mapcar (lambda (c)
                   `(skip-unless
                     ;; Ignore non-command checkers
                     (or (not (flycheck-checker-get ',c 'command))
                         (executable-find (flycheck-checker-executable ',c)))))
                 checkers)
       ,@body)))


;;; Test case results

(defun flycheck-ert-syntax-check-timed-out-p (result)
  "Whether RESULT denotes a timed-out test.

RESULT is an ERT test result object."
  (and (ert-test-failed-p result)
       (eq (car (ert-test-failed-condition result))
           'flycheck-ert-syntax-check-timed-out)))


;;; Syntax checking in tests

(defvar-local flycheck-ert-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-ert-syntax-checker-finished t)))

(defconst flycheck-ert-checker-wait-time 10
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(define-error 'flycheck-ert-syntax-check-timed-out "Syntax check timed out.")

(defun flycheck-ert-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (let ((starttime (float-time)))
    (while (and (not flycheck-ert-syntax-checker-finished)
                (< (- (float-time) starttime) flycheck-ert-checker-wait-time))
      (accept-process-output nil 0.02))
    (unless (< (- (float-time) starttime) flycheck-ert-checker-wait-time)
      (flycheck-stop)
      (signal 'flycheck-ert-syntax-check-timed-out nil)))
  (setq flycheck-ert-syntax-checker-finished nil))

(defun flycheck-ert-buffer-sync ()
  "Like `flycheck-buffer', but synchronously."
  (setq flycheck-ert-syntax-checker-finished nil)
  (should (not (flycheck-running-p)))
  (flycheck-mode) ;; This will only start a deferred check,
  (should (flycheck-get-checker-for-buffer))
  (flycheck-buffer) ;; …so we need an explicit manual check
  ;; After starting the check, the checker should either be running now, or
  ;; already be finished (if it was fast).
  (should (or flycheck-current-syntax-check
              flycheck-ert-syntax-checker-finished))
  ;; Also there should be no deferred check pending anymore
  (should-not (flycheck-deferred-check-p))
  (flycheck-ert-wait-for-syntax-checker))

(defun flycheck-ert-ensure-clear ()
  "Clear the current buffer.

Raise an assertion error if the buffer is not clear afterwards."
  (flycheck-clear)
  (should (not flycheck-current-errors))
  (should (not (-any? (lambda (ov) (overlay-get ov 'flycheck-overlay))
                      (overlays-in (point-min) (point-max))))))


;;; Test assertions

(defun flycheck-error-without-group (err)
  "Return a copy ERR with the `group' property set to nil."
  (let ((copy (copy-flycheck-error err)))
    (setf (flycheck-error-group copy) nil)
    copy))

(defun flycheck-ert-should-overlay (error)
  "Test that ERROR has a proper overlay in the current buffer.

ERROR is a Flycheck error object."
  (let* ((overlay (-first (lambda (ov)
                            (equal (flycheck-error-without-group
                                    (overlay-get ov 'flycheck-error))
                                   (flycheck-error-without-group error)))
                          (flycheck-overlays-in 0 (+ 1 (buffer-size)))))
         (region
          ;; Overlays of errors from other files are on the first line
          (if (flycheck-relevant-error-other-file-p error)
              (cons (point-min)
                    (save-excursion (goto-char (point-min))
                                    (line-end-position)))
            (flycheck-error-region-for-mode error 'symbols)))
         (level (flycheck-error-level error))
         (category (flycheck-error-level-overlay-category level))
         (face (get category 'face))
         (fringe-bitmap (flycheck-error-level-fringe-bitmap level))
         (fringe-face (flycheck-error-level-fringe-face level))
         (fringe-icon (list 'left-fringe fringe-bitmap fringe-face)))
    (should overlay)
    (should (overlay-get overlay 'flycheck-overlay))
    (should (= (overlay-start overlay) (car region)))
    (should (= (overlay-end overlay) (cdr region)))
    (should (eq (overlay-get overlay 'face) face))
    (should (equal (get-char-property 0 'display
                                      (overlay-get overlay 'before-string))
                   fringe-icon))
    (should (eq (overlay-get overlay 'category) category))
    (should (equal (flycheck-error-without-group (overlay-get overlay
                                                              'flycheck-error))
                   (flycheck-error-without-group error)))))

(defun flycheck-ert-sort-errors (errors)
  "Sort ERRORS by `flycheck-error-<'."
  (seq-sort #'flycheck-error-< errors))

(defun flycheck-ert-should-errors (&rest errors)
  "Test that the current buffers has ERRORS.

ERRORS is a list of errors expected to be present in the current
buffer.  Each error is given as a list of arguments to
`flycheck-error-new-at'.

If ERRORS are omitted, test that there are no errors at all in
the current buffer.

With ERRORS, test that each error in ERRORS is present in the
current buffer, and that the number of errors in the current
buffer is equal to the number of given ERRORS.  In other words,
check that the buffer has all ERRORS, and no other errors."
  (let ((expected (flycheck-ert-sort-errors
                   (mapcar (apply-partially #'apply #'flycheck-error-new-at)
                           errors)))
        (current (flycheck-ert-sort-errors flycheck-current-errors)))
    (should (equal (mapcar #'flycheck-error-without-group expected)
                   (mapcar #'flycheck-error-without-group current)))
    ;; Check that related errors are the same
    (cl-mapcar
     (lambda (err1 err2)
       (should (equal (flycheck-ert-sort-errors
                       (mapcar #'flycheck-error-without-group
                               (flycheck-related-errors err1 expected)))
                      (flycheck-ert-sort-errors
                       (mapcar #'flycheck-error-without-group
                               (flycheck-related-errors err2))))))
     expected current)
    (mapc #'flycheck-ert-should-overlay expected))
  (should (= (length errors)
             (length (flycheck-overlays-in (point-min) (point-max))))))

(define-error 'flycheck-ert-suspicious-checker "Suspicious state from checker")

(defun flycheck-ert-should-syntax-check-in-buffer (&rest errors)
  "Test a syntax check in BUFFER, expecting ERRORS.

This is like `flycheck-ert-should-syntax-check', but with a
buffer in the right mode instead of a file."
  ;; Load safe file-local variables because some tests depend on them
  (let ((enable-local-variables :safe)
        ;; Disable all hooks at this place, to prevent 3rd party packages
        ;; from interfering
        (hack-local-variables-hook))
    (hack-local-variables))
  ;; Configure config file locating for unit tests
  (let ((process-hook-called 0)
        (suspicious nil))
    (add-hook 'flycheck-process-error-functions
              (lambda (_err)
                (setq process-hook-called (1+ process-hook-called))
                nil)
              nil :local)
    (add-hook 'flycheck-status-changed-functions
              (lambda (status)
                (when (eq status 'suspicious)
                  (setq suspicious t)))
              nil :local)
    (flycheck-ert-buffer-sync)
    (when suspicious
      (signal 'flycheck-ert-suspicious-checker nil))
    (apply #'flycheck-ert-should-errors errors)
    (should (= process-hook-called (length errors))))
  (flycheck-ert-ensure-clear))

(defun flycheck-ert-should-syntax-check (resource-file modes &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  If more than one major mode is specified, the test
is run for each mode separately, so if you give three major
modes, the entire test will run three times.  ERRORS is the list
of expected errors, as in `flycheck-ert-should-errors'.  If
omitted, the syntax check must not emit any errors.  The errors
are cleared after each test.

The syntax checker is selected via standard syntax checker
selection.  To test a specific checker, you need to set
`flycheck-checker' or `flycheck-disabled-checkers' accordingly
before using this predicate, depending on whether you want to use
manual or automatic checker selection.

During the syntax check, configuration files of syntax checkers
are also searched in the `config-files' sub-directory of the
resource directory."
  (when (symbolp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (unless (fboundp mode)
      (ert-skip (format "%S missing" mode)))
    (flycheck-ert-with-resource-buffer resource-file
      (funcall mode)
      (apply #'flycheck-ert-should-syntax-check-in-buffer errors))))

(defun flycheck-ert-at-nth-error (n)
  "Determine whether point is at the N'th Flycheck error.

Return non-nil if the point is at the N'th Flycheck error in the
current buffer.  Otherwise return nil."
  (let* ((error (nth (1- n) flycheck-current-errors))
         (mode flycheck-highlighting-mode)
         (region (flycheck-error-region-for-mode error mode)))
    (and (member error (flycheck-overlay-errors-at (point)))
         (= (point) (car region)))))

(defun flycheck-ert-explain--at-nth-error (n)
  "Explain a failed at-nth-error predicate at N."
  (let ((errors (flycheck-overlay-errors-at (point))))
    (if (null errors)
        (format "Expected to be at error %s, but no error at point %s"
                n (point))
      (let ((pos (cl-position (car errors) flycheck-current-errors)))
        (format "Expected to be at point %s and error %s, \
but point %s is at error %s"
                (car (flycheck-error-region-for-mode
                      (nth (1- n) flycheck-current-errors)
                      flycheck-highlighting-mode))
                n (point) (1+ pos))))))

(put 'flycheck-ert-at-nth-error 'ert-explainer
     'flycheck-ert-explain--at-nth-error)

(provide 'flycheck-ert)

;;; flycheck-ert.el ends here
