;;; ement-api.el --- Matrix API library              -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

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

;;

;;; Code:

;;;; Debugging

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `ement-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

(require 'json)
(require 'url-parse)
(require 'url-util)

(require 'plz)

(require 'ement-macros)
(require 'ement-structs)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(cl-defun ement-api (session endpoint
                             &key then data params queue
                             (content-type "application/json")
                             (data-type 'text)
                             (else #'ement-api-error) (method 'get)
                             ;; FIXME: What's the right term for the URL part after "/_matrix/"?
                             (endpoint-category "client")
                             (json-read-fn #'json-read)
                             ;; NOTE: Hard to say what the default timeouts
                             ;; should be.  Sometimes the matrix.org homeserver
                             ;; can get slow and respond a minute or two later.
                             (connect-timeout 10) (timeout 60)
                             (version "r0"))
  "Make API request on SESSION to ENDPOINT.
The request automatically uses SESSION's server, URI prefix, and
access token.

These keyword arguments are passed to `plz', which see: THEN,
DATA (passed as BODY), QUEUE (passed to `plz-queue', which see),
DATA-TYPE (passed as BODY-TYPE), ELSE, METHOD,
JSON-READ-FN (passed as AS), CONNECT-TIMEOUT, TIMEOUT.

Other arguments include PARAMS (used as the URL's query
parameters), ENDPOINT-CATEGORY (added to the endpoint URL), and
VERSION (added to the endpoint URL).

Note that most Matrix requests expect JSON-encoded data, so
usually the DATA argument should be passed through
`json-encode'."
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-session server token) session)
               ((cl-struct ement-server uri-prefix) server)
               ((cl-struct url type host portspec) (url-generic-parse-url uri-prefix))
               (path (format "/_matrix/%s/%s/%s" endpoint-category version endpoint))
               (query (url-build-query-string params))
               (filename (concat path "?" query))
               (url (url-recreate-url
                     (url-parse-make-urlobj type nil nil host portspec filename nil data t)))
               (headers (ement-alist "Content-Type" content-type))
               (plz-args))
    (when token
      ;; Almost every request will require a token (only a few, like checking login flows, don't),
      ;; so we simplify the API by using the token automatically when the session has one.
      (push (cons "Authorization" (concat "Bearer " token)) headers))
    (setf plz-args (list method url :headers headers :body data :body-type data-type
                         :as json-read-fn :then then :else else
                         :connect-timeout connect-timeout :timeout timeout :noquery t))
    ;; Omit `then' from debugging because if it's a partially applied
    ;; function on the session object, which may be very large, it
    ;; will take a very long time to print into the warnings buffer.
    ;;  (ement-debug (current-time) method url headers)
    (if queue
        (plz-run
         (apply #'plz-queue queue plz-args))
      (apply #'plz plz-args))))

(define-error 'ement-api-error "Ement API error" 'error)

(defun ement-api-error (plz-error)
  "Signal an Ement API error for PLZ-ERROR."
  ;; This feels a little messy, but it seems to be reasonable.
  (pcase-let* (((cl-struct plz-error response
                           (message plz-message) (curl-error `(,curl-exit-code . ,curl-message)))
                plz-error)
               (status (when (plz-response-p response)
                         (plz-response-status response)))
               (body (when (plz-response-p response)
                       (plz-response-body response)))
               (json-object (when body
                              (ignore-errors
                                (json-read-from-string body))))
               (error-message (format "%S: %s"
                                      (or curl-exit-code status)
                                      (or (when json-object
                                            (alist-get 'error json-object))
                                          curl-message
                                          plz-message))))

    (signal 'ement-api-error (list error-message))))

;;;; Footer

(provide 'ement-api)

;;; ement-api.el ends here
