;;; org-cliplink-transport.el --- insert org-mode links from the clipboard -*- lexical-binding: t -*-

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/org-cliplink
;; Version: 0.2

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'url-parse)

(require 'org-cliplink-string)

(defvar org-cliplink-block-authorization nil
  "Flag whether to block url.el's usual interactive authorisation procedure")

(defadvice url-http-handle-authentication (around org-cliplink-fix)
  (unless org-cliplink-block-authorization
    ad-do-it))
(ad-activate 'url-http-handle-authentication)

(defun org-cliplink-credentials-to-basic-auth (username password)
  (concat "Basic " (base64-encode-string
                    (concat username ":" password))))

(defun org-cliplink-shadow-basic-auth-credentials (basic-auth-credentials)
  (when basic-auth-credentials
    (list :username "***"
          :password "***")))

(defun org-cliplink-curl-prepare-response-buffer-name (url)
  (format " *curl-%s-%x*"
          (url-host (url-generic-parse-url url))
          (random)))

(defun org-cliplink-build-curl-arguments (url basic-auth-credentials extra-curl-arguments)
  (append extra-curl-arguments
          (list "--include"
                "--silent"
                "--show-error"
                "-X"
                "GET")
          (when basic-auth-credentials
            (let ((username (plist-get basic-auth-credentials :username))
                  (password (plist-get basic-auth-credentials :password)))
              (list "--user"
                    (format "%s:%s" username password))))
          (list url)))

(defun org-cliplink-build-curl-sentinel (response-buffer-name callback)
  (lambda (process event)
    (ignore event)
    (when (not (process-live-p process))
      (if (zerop (process-exit-status process))
          (when callback
            (with-current-buffer response-buffer-name
              (funcall callback nil)))
        (with-current-buffer response-buffer-name
          (error (buffer-string)))))))

(defun org-cliplink-start-curl-process (response-buffer-name curl-arguments)
  (let ((curl-executable (executable-find "curl")))
    (apply #'start-process
           "curl"
           response-buffer-name
           curl-executable
           curl-arguments)))

(defun org-cliplink-log-curl-command (url basic-auth-credentials extra-curl-arguments)
  (message "curl %s"
           (org-cliplink-join-string
            (org-cliplink-build-curl-arguments
             url
             (org-cliplink-shadow-basic-auth-credentials basic-auth-credentials)
             extra-curl-arguments))))

(defun org-cliplink-http-get-request--curl (url callback &optional basic-auth-credentials extra-curl-arguments)
  (let* ((response-buffer-name (org-cliplink-curl-prepare-response-buffer-name url))
         (curl-arguments (org-cliplink-build-curl-arguments url
                                                            basic-auth-credentials
                                                            extra-curl-arguments)))
    (org-cliplink-log-curl-command url basic-auth-credentials
                                   extra-curl-arguments)
    (set-process-sentinel
     (org-cliplink-start-curl-process response-buffer-name
                                      curl-arguments)
     (org-cliplink-build-curl-sentinel response-buffer-name
                                       callback))))

(defun org-cliplink-http-get-request--url-el (url callback &optional basic-auth-credentials)
  (let* (;; Sometimes url-retrieve invokes the callback multiple
         ;; times. Looks like it is a bug in url.el. For more
         ;; information see
         ;; https://github.com/rexim/org-cliplink/issues/34
         (block-title-callback-invocation nil)
         (url-retrieve-callback
          (lambda (status)
            (when (not block-title-callback-invocation)
              (setq block-title-callback-invocation t)
              (funcall callback status)))))
    (if basic-auth-credentials
        (let* ((org-cliplink-block-authorization t)
               (username (plist-get basic-auth-credentials :username))
               (password (plist-get basic-auth-credentials :password))
               (url-request-extra-headers
                `(("Authorization" . ,(org-cliplink-credentials-to-basic-auth
                                       username password)))))
          (url-retrieve url url-retrieve-callback))
      (url-retrieve url url-retrieve-callback nil nil t))))

(provide 'org-cliplink-transport)

;;; org-cliplink-transport.el ends here
