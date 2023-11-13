;;; test-plz.el --- Tests for plz          -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

;; This file is part of GNU Emacs.

;;; License:

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

;; NOTE: NOTE: NOTE: NOTE: Yes, four NOTEs, because this is important:
;; As of this moment, all of the tests pass when run with makem.sh.
;; And when running them in an interactive Emacs with ERT, one test at
;; a time, individual tests pass, or almost always do (depending on
;; whether the httpbin.org server is overloaded).  But when running
;; multiple tests in ERT at one time,
;; i.e. (ert-run-tests-interactively "plz-"), multiple, if not most,
;; tests fail, but not the same ones every time.

;; I have now spent hours trying to figure out why, inserting many
;; debug statements in many functions, and come up with nothing.  I
;; tried changing the way `accept-process-output' is called, like
;; using timeouts or JUST-THIS-ONE, but it made no difference.  I
;; tried calling it extra times, nope.  I tried calling the sentinel
;; extra times when it seemed that it hadn't run the THEN function,
;; nope.  Nothing seems to make a difference.

;; I even checked out an earlier commit, before the commit that
;; rewrote/merged the synchronous request code into the `plz'
;; function, thinking that surely I broke something--but, nope, they
;; apparently failed the same way back then: passing with makem.sh,
;; passing individually, but failing when run in close succession by
;; ERT.

;; After inserting enough debug statements, I noticed that the process
;; sentinel sometimes seemed to run for the last time after the ERT
;; test had returned, which suggests that ERT might be doing something
;; weird, or somehow its instrumentation interferes with the
;; process-handling code.  But if that's not the cause, then I'm out
;; of ideas.

;; So then I tried rewriting the synchronous request code to use
;; `call-process-region', instead of calling `accept-process-output'
;; in a loop to block on the curl process (which is how the Elisp
;; manual says to do it), but that still made no difference: even the
;; async requests fail in the same way with ERT.  So that doesn't
;; appear to be the problem, either.

;; So is there some kind of fundamental flaw in the `plz' design?
;; Maybe.  Is there a simple, logical oversight in its code that only
;; manifests under certain conditions?  Maybe.  Is ERT doing something
;; weird that's interfering with process-related code?  Maybe.  Is
;; Emacs's own process-handling code still broken in some mysterious
;; way?  Maybe.

;; But despite all of that, when using `plz' "in anger", in `ement',
;; it seems to work reliably for me.  I did get one report from one
;; user that sounded like the same kind of problem I'm seeing with ERT
;; here, but then he tried `ement-connect' again, and it worked.  And
;; I'm sitting here watching `ement' constantly using `plz' to talk to
;; the matrix.org server, and I haven't had a single error or failure,
;; even after hours of being connected.  It *seems* to *actually*
;; work.

;; So, if you're reading this, and you're wondering whether you should
;; use `plz': Well, please do, and please let me know if you have any
;; problems; I do need to know whether it's working for other users.
;; And if you think you might know what's going wrong when running the
;; tests in ERT, please let me know, because I'm out of ideas: as far
;; as I can tell, when it comes to process-handling in Emacs, "there
;; be dragons."

;;; Code:

;;;; Requirements

(require 'ert)
(require 'json)
(require 'let-alist)
(require 'map)

(require 'plz)

;;;; Variables

(defvar plz-test-uri-prefix
  ;; "https://httpbin.org"
  "http://localhost"
  "URI prefix for HTTP requests, without trailing slash.
If running httpbin locally, set to \"http://localhost\".")

;;;; Customization


;;;; Commands


;;;; Macros

(cl-defun plz-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  (when process
    ;; Sometimes it seems that the process is killed, the THEN
    ;; function called by its sentinel, and its buffer killed, all
    ;; before this function gets called with the process argument;
    ;; when that happens, tests that use this can fail.  Testing
    ;; whether PROCESS is non-nil seems to fix it, but it's possible
    ;; that something funny is going on...
    (cl-loop for i upto times ;; 10 seconds
             while (equal 'run (process-status process))
             do (sleep-for seconds))))

(cl-defmacro plz-deftest (name () &body docstring-keys-and-body)
  "Like `ert-deftest', but defines tests for both HTTP/1.1 and HTTP/2."
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(progn
     ,@(cl-loop for http-version in '("1.1" "2")
                collect (let ((name (intern (format "%s-http%s" name http-version))))
                          `(ert-deftest ,name ()
                             (let ((plz-curl-default-args
                                    ',(append plz-curl-default-args (list (format "--http%s" http-version)))))
                               ,@docstring-keys-and-body))))))

;;;; Functions

(defun plz-test-url (url-part)
  "Return URL-PART appended to `plz-test-uri-prefix'.
Also, any instance of \"URI-PREFIX\" in URL-PART is replaced with
`plz-test-uri-prefix' in URL-encoded form."
  (setf url-part (replace-regexp-in-string "URI-PREFIX" (url-hexify-string plz-test-uri-prefix)
                                           url-part t t))
  (concat plz-test-uri-prefix url-part))

(defmacro plz-test-get-response (response)
  "Test parts of RESPONSE with `should'."
  `(progn
     (should (plz-response-p ,response))
     (should (numberp (plz-response-version ,response)))
     (should (eq 200 (plz-response-status ,response)))
     (should (equal "application/json" (alist-get 'content-type (plz-response-headers ,response))))
     (should (string-match "curl"
                           (map-nested-elt (json-read-from-string (plz-response-body ,response))
                                           '(headers User-Agent))))))

;;;; Tests

;;;;; Async

(plz-deftest plz-get-string nil
  (let* ((test-string)
         (process (plz 'get (plz-test-url "/get")
                    :as 'string
                    :then (lambda (string)
                            (setf test-string string)))))
    (plz-test-wait process)
    (should (string-match "curl" test-string))))

(plz-deftest plz-get-buffer nil
  (let* ((result-buffer)
         (process (plz 'get (plz-test-url "/get")
                    :as 'buffer :then (lambda (buffer)
                                        (setf result-buffer buffer)))))
    (unwind-protect
        (progn
          (plz-test-wait process)
          (should (buffer-live-p result-buffer))
          (with-current-buffer result-buffer
            (should-not (looking-at-p plz-http-response-status-line-regexp))
            (should (string-match "curl" (buffer-string)))))
      (kill-buffer result-buffer)
      (should-not (buffer-live-p result-buffer)))))

(plz-deftest plz-get-response nil
  (let* ((test-response)
         (process (plz 'get (plz-test-url "/get")
                    :as 'response
                    :then (lambda (response)
                            (setf test-response response)))))
    (plz-test-wait process)
    (plz-test-get-response test-response)))

(plz-deftest plz-get-json nil
  (let* ((test-json)
         (process (plz 'get (plz-test-url "/get")
                    :as #'json-read
                    :then (lambda (json)
                            (setf test-json json)))))
    (plz-test-wait process)
    (let-alist test-json
      (should (string-match "curl" .headers.User-Agent)))))

(plz-deftest plz-post-json-string nil
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (response-json)
         (process (plz 'post (plz-test-url "/post")
                    :headers '(("Content-Type" . "application/json"))
                    :body json-string
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

(plz-deftest plz-post-jpeg-string nil
  (let* ((jpeg-to-upload (plz 'get (plz-test-url "/image/jpeg")
                           :as 'binary :then 'sync))
         (_ (unless jpeg-to-upload
              (error "jpeg-to-upload is nil")))
         (response-json)
         (response-jpeg)
         (process (plz 'post (plz-test-url "/post")
                    :headers '(("Content-Type" . "image/jpeg"))
                    :body jpeg-to-upload :body-type 'binary
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json
                                  response-jpeg
                                  (base64-decode-string
                                   (string-remove-prefix "data:application/octet-stream;base64,"
                                                         (alist-get 'data json))))))))
    (should (equal 'jpeg (image-type-from-data jpeg-to-upload)))
    (plz-test-wait process)
    (should response-json)
    (should (equal 'jpeg (image-type-from-data response-jpeg)))
    (should (equal (length jpeg-to-upload) (length response-jpeg)))
    (should (equal jpeg-to-upload response-jpeg))))

;; TODO: POST JSON buffer.

(plz-deftest plz-put-json-string nil
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (response-json)
         (process (plz 'put (plz-test-url "/put")
                    :headers '(("Content-Type" . "application/json"))
                    :body json-string
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

;; TODO: Put JSON buffer.

;;;;; Sync

(plz-deftest plz-get-string-sync nil
  (let-alist (json-read-from-string (plz 'get (plz-test-url "/get")
                                      :as 'string :then 'sync))
    (should (equal (plz-test-url "/get") .url))))

(plz-deftest plz-get-response-sync nil
  (plz-test-get-response (plz 'get (plz-test-url "/get")
                           :as 'response :then 'sync)))

(plz-deftest plz-get-json-sync nil
  (let-alist (plz 'get (plz-test-url "/get")
               :as #'json-read :then 'sync)
    (should (string-match "curl" .headers.User-Agent))))

(plz-deftest plz-get-buffer-sync nil
  (let ((buffer (plz 'get (plz-test-url "/get")
                  :as 'buffer :then 'sync)))
    (unwind-protect
        (should (buffer-live-p buffer))
      (kill-buffer buffer))))

;;;;; Headers

;; These tests were added when plz--curl was changed to send headers
;; with "--config" rather than on the command line.

(plz-deftest plz-get-with-headers ()
  (let* ((response-json)
         (process (plz 'get (plz-test-url "/get")
                    :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header)))))

(plz-deftest plz-post-with-headers ()
  (let* ((alist (list (cons "key" "value")))
         (response-json)
         (process (plz 'post (plz-test-url "/post")
                    :headers '(("Content-Type" . "application/json")
                               ("X-Plz-Test-Header" . "plz-test-header-value"))
                    :body (json-encode alist)
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))
      (should (equal "value" (alist-get 'key (json-read-from-string .data)))))))

(plz-deftest plz-get-json-with-headers-sync ()
  (let-alist (plz 'get (plz-test-url "/get")
               :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
               :as #'json-read :then 'sync)
    (should (string-match "curl" .headers.User-Agent))
    (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))))

;;;;; HEAD requests

;; NOTE: httpbin.org doesn't appear to support a "/head" endpoint,
;; so we'll use "/get".

(plz-deftest plz-head-without-headers ()
  ;; I'm not sure how useful it may be to make a HEAD request without
  ;; caring about the headers, but perhaps it could be useful as a
  ;; lightweight way to test a server's presence, so we should
  ;; probably support it.  This merely tests that no error is
  ;; signaled, which should mean that the HEAD request succeeded.
  (should (plz 'head (plz-test-url "/get"))))

(plz-deftest plz-head-as-response ()
  (let ((response (plz 'head (plz-test-url "/get")
                    :as 'response)))
    (should (equal "application/json"
                   (alist-get 'content-type
                              (plz-response-headers response))))))

;;;;; POST requests

(plz-deftest plz-post-empty-body ()
  (should (equal ""
                 (alist-get 'data
                            (json-read-from-string
                             (plz 'post (plz-test-url "/post"))))))
  (should (equal "application/json"
                 (alist-get 'content-type
                            (plz-response-headers
                             (plz 'post (plz-test-url "/post") :as 'response))))))

;;;;; Status codes

(plz-deftest plz-201-succeeds ()
  ;; This merely tests that a 201 response does not signal an error.
  (should (plz 'get (plz-test-url "/status/201"))))

(plz-deftest plz-400-errors ()
  (should-error (plz 'get (plz-test-url "/status/400"))))

(plz-deftest plz-500-errors ()
  (should-error (plz 'get (plz-test-url "/status/500"))))

;;;;; Redirects

(plz-deftest plz-301-redirects ()
  (plz-test-get-response
   (plz 'get (plz-test-url "/redirect-to?url=URI-PREFIX%2Fget&status_code=301")
     :as 'response :then 'sync)))

(plz-deftest plz-302-redirects ()
  (plz-test-get-response
   (plz 'get (plz-test-url "/redirect-to?url=URI-PREFIX%2Fget&status_code=302")
     :as 'response :then 'sync)))

(plz-deftest plz-307-redirects ()
  (plz-test-get-response
   (plz 'get (plz-test-url "/redirect-to?url=URI-PREFIX%2Fget&status_code=307")
     :as 'response :then 'sync)))

(plz-deftest plz-308-redirects ()
  (plz-test-get-response
   (plz 'get (plz-test-url "/redirect-to?url=URI-PREFIX%2Fget&status_code=308")
     :as 'response :then 'sync)))

;;;;; Errors

;; TODO: Sync requests with ":as 'response" should return response for errors rather than signaling.

(plz-deftest plz-get-curl-error-async nil
  ;; Async.
  (let* ((err)
         (process (plz 'get "https://httpbinnnnnn.org/get/status/404"
                    :as 'string :then #'ignore
                    :else (lambda (e)
                            (setf err e)))))
    (plz-test-wait process)
    (should (plz-error-p err))
    (should (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                   (plz-error-curl-error err)))))

;; FIXME: This test works interactively but not in batch mode: it
;; stalls the Emacs process indefinitely, using either sleep-for or
;; sit-for.

;; (plz-deftest plz-get-killed-error nil
;;   ;; Async.
;;   (let* ((err)
;;          (process (plz 'get "https://httpbinnnnnn.org/get/status/404"
;;                     :as 'string
;;                     :else (lambda (e)
;;                             (setf err e)))))
;;     (sit-for 0.01)
;;     (delete-process process)
;;     (should (not (process-live-p process)))
;;     (should (plz-error-p err))
;;     (should (equal "curl process killed"
;;                    (plz-error-message err)))))

(plz-deftest plz-get-curl-error-sync nil
  ;; Sync.
  (pcase-let ((`(,_signal . (,_message ,data))
	       (should-error (plz 'get "https://httpbinnnnnn.org/get/status/404"
                               :as 'string :then 'sync)
                             :type 'plz-error)))
    (should (plz-error-p data))
    (should (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                   (plz-error-curl-error data)))))

(plz-deftest plz-get-404-error-sync  nil
  (pcase-let ((`(,_signal . (,_message ,data))
	       (should-error (plz 'get (plz-test-url "/get/status/404")
			       :as 'string :then 'sync)
                             :type 'plz-error)))
    (should (plz-error-p data))
    (should (plz-response-p (plz-error-response data)))
    (should (eq 404 (plz-response-status (plz-error-response data))))))

(plz-deftest plz-get-404-error-async nil
  (let* ((err)
         (process (plz 'get (plz-test-url "/get/status/404")
                    :as 'string :then #'ignore
                    :else (lambda (e)
                            (setf err e)))))
    (plz-test-wait process)
    (should (plz-error-p err))
    (should (plz-response-p (plz-error-response err)))
    (should (eq 404 (plz-response-status (plz-error-response err))))))

(plz-deftest plz-get-timeout-error-sync nil
  (pcase-let* ((start-time (current-time))
               (`(,_signal . (,_message ,(cl-struct plz-error (curl-error `(,code . ,message)))))
		(should-error (plz 'get (plz-test-url "/delay/5")
				:as 'string :then 'sync :timeout 1)
			      :type 'plz-error))
               (end-time (current-time)))
    (should (eq 28 code))
    (should (equal "Operation timeout." message))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1))))

(plz-deftest plz-get-timeout-error-async nil
  (let* ((start-time (current-time))
         (end-time)
         (plz-error)
         (process (plz 'get (plz-test-url "/delay/5")
                    :as 'response :timeout 1 :then #'ignore
                    :else (lambda (e)
                            (setf end-time (current-time)
                                  plz-error e)))))
    (plz-test-wait process)
    (should (eq 28 (car (plz-error-curl-error plz-error))))
    (should (equal "Operation timeout." (cdr (plz-error-curl-error plz-error))))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1))))

;;;;; Finally

(plz-deftest plz-get-finally nil
  (let* ((finally-null t)
         (process (plz 'get (plz-test-url "/get")
                    :as 'string
                    :then #'ignore
                    :finally (lambda ()
                               (setf finally-null nil)))))
    (plz-test-wait process)
    (should-not finally-null)))

;;;;; Binary

(plz-deftest plz-get-jpeg ()
  (let* ((test-jpeg)
         (process (plz 'get (plz-test-url "/image/jpeg")
                    :as 'binary
                    :then (lambda (string)
                            (setf test-jpeg string)))))
    (plz-test-wait process)
    (should (equal 'jpeg (image-type-from-data test-jpeg)))))

(plz-deftest plz-get-jpeg-sync ()
  (let ((jpeg (plz 'get (plz-test-url "/image/jpeg")
                :as 'binary :then 'sync)))
    (should (equal 'jpeg (image-type-from-data jpeg)))))

;;;;; Downloading to files

(plz-deftest plz-get-temp-file ()
  (let ((filename (plz 'get (plz-test-url "/image/jpeg")
                    :as 'file :then 'sync)))
    (unwind-protect
        (let ((jpeg-data (with-temp-buffer
                           (insert-file-contents filename)
                           (buffer-string))))
          (should (equal 'jpeg (image-type-from-data jpeg-data))))
      ;; It's a temp file, so it should always be deleted.
      (delete-file filename))))

(plz-deftest plz-get-named-file ()
  (let ((filename (make-temp-file "plz-")))
    ;; HACK: Delete the temp file and reuse its name, because
    ;; `make-temp-name' is less convenient to use.
    (delete-file filename)
    (unwind-protect
        (progn
          (plz 'get (plz-test-url "/image/jpeg")
            :as `(file ,filename) :then 'sync)
          (let ((jpeg-data (with-temp-buffer
                             (insert-file-contents filename)
                             (buffer-string))))
            (should (equal 'jpeg (image-type-from-data jpeg-data)))))
      ;; It's a temp file, so it should always be deleted.
      (when (file-exists-p filename)
        (delete-file filename)))))

(plz-deftest plz-upload-file-by-name ()
  (let ((filename (make-temp-file "plz-"))
        response-json process)
    (unwind-protect
        (progn
          (with-temp-file filename
            (insert "deadbeef"))
          (setf process
                (plz 'put (plz-test-url "/put")
                  :body `(file ,filename)
                  :as #'json-read
                  :then (lambda (json)
                          (setf response-json json))))
          (plz-test-wait process)
          (should (equal "deadbeef" (alist-get 'data response-json)))
          (should-not (alist-get 'files response-json)))
      (delete-file filename))))

;;;;; Queue

;; TODO: Test that limit is enforced (though it seems to work fine).

(plz-deftest plz-queue-with-finally ()
  "Ensure that a queue with a FINALLY function calls it correctly.
That is, that the function is called after the queue is emptied,
and only called once."
  (let* ((finally-called-at nil)
         (finally-called-times 0)
         (queue (make-plz-queue :limit 2
                                :finally (lambda ()
                                           (setf finally-called-at (current-time))
                                           (cl-incf finally-called-times))))
         (urls (list (plz-test-url "/delay/2")))
         completed-urls queue-started-at)
    (dolist (url urls)
      (plz-queue queue
        'get url :then (lambda (_)
                         (push url completed-urls))))
    (setf queue-started-at (current-time))
    (plz-run queue)
    (cl-loop with waits = 0
             while (and (plz-queue-active queue) (< waits 60))
             do (progn
                  (sleep-for 0.1)
                  (cl-incf waits)))
    (should (seq-set-equal-p urls completed-urls))
    (should (zerop (plz-length queue)))
    (should (= 1 finally-called-times))
    (should (>= (float-time (time-subtract finally-called-at queue-started-at))
                2))))

(plz-deftest plz-queue-without-finally ()
  "Ensure that a queue without a FINALLY function doesn't signal an error."
  (let* ((queue (make-plz-queue :limit 2))
         (urls (list (plz-test-url "/get?foo=0")
                     (plz-test-url "/get?foo=1")))
         completed-urls)
    (dolist (url urls)
      (plz-queue queue
        'get url :then (lambda (_)
                         (push url completed-urls))))
    (plz-run queue)
    (cl-loop with waits = 0
             while (and (plz-queue-active queue) (< waits 20))
             do (progn
                  (sleep-for 0.1)
                  (cl-incf waits)))
    (should (seq-set-equal-p urls completed-urls))
    (should (zerop (plz-length queue)))))

;; TODO: Add test for canceling queue.

;;;; Footer

(provide 'test-plz)

;;; test-plz.el ends here
