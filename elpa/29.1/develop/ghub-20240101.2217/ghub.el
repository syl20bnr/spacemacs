;;; ghub.el --- Client libraries for Git forge APIs  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Ghub provides basic support for using the APIs of various Git forges
;; from Emacs packages.  Originally it only supported the Github REST
;; API, but now it also supports the Github GraphQL API as well as the
;; REST APIs of Gitlab, Gitea, Gogs and Bitbucket.

;; Ghub abstracts access to API resources using only a handful of basic
;; functions such as `ghub-get'.  These are convenience wrappers around
;; `ghub-request'.  Additional forge-specific wrappers like `glab-put',
;; `gtea-put', `gogs-post' and `buck-delete' are also available.  Ghub
;; does not provide any resource-specific functions, with the exception
;; of `FORGE-repository-id'.

;; When accessing Github, then Ghub handles the creation and storage of
;; access tokens using a setup wizard to make it easier for users to get
;; started.  The tokens for other forges have to be created manually.

;; Ghub is intentionally limited to only provide these two essential
;; features — basic request functions and guided setup — to avoid being
;; too opinionated, which would hinder wide adoption.  It is assumed that
;; wide adoption would make life easier for users and maintainers alike,
;; because then all packages that talk to forge APIs could be configured
;; the same way.

;; Please consult the manual (info "ghub") for more information.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'compat)
(require 'gnutls)
(require 'let-alist)
(require 'url)
(require 'url-auth)
(require 'url-http)

(eval-when-compile (require 'subr-x))

;; Needed for Emacs < 27.
(eval-when-compile (require 'json))
(declare-function json-read-from-string "json" (string))
(declare-function json-encode "json" (object))

(declare-function glab-repository-id "glab" (owner name &key username auth host))
(declare-function gtea-repository-id "gtea" (owner name &key username auth host))
(declare-function gogs-repository-id "gogs" (owner name &key username auth host))
(declare-function buck-repository-id "buck" (owner name &key username auth host))

(defvar url-callback-arguments)
(defvar url-http-end-of-headers)
(defvar url-http-extra-headers)
(defvar url-http-response-status)

;;; Settings

(defconst ghub-default-host "api.github.com"
  "The default host that is used if `ghub.host' is not set.")

(defvar ghub-github-token-scopes '(repo)
  "The Github API scopes that your private tools need.

You have to manually create or update the token at
https://github.com/settings/tokens.  This variable
only serves as documentation.")

(defvar ghub-insecure-hosts nil
  "List of hosts that use http instead of https.")

(defvar ghub-json-use-jansson nil
  "Whether to use the Jansson library, if available.
This is experimental.  Only let-bind this but do not enable it
globally because doing that is likely to break other packages
that use `ghub'.  As a user also do not enable this yet.
See https://github.com/magit/ghub/pull/149.")

(defvar ghub-json-object-type 'alist
  "The object type that is used for json payload decoding.")

(defvar ghub-json-array-type 'list
  "The array type that is used for json payload decoding.")

;;; Request
;;;; Object

(cl-defstruct (ghub--req
               (:constructor ghub--make-req)
               (:copier nil))
  (url        nil :read-only nil)
  (forge      nil :read-only t)
  (silent     nil :read-only t)
  (method     nil :read-only t)
  (headers    nil :read-only t)
  (handler    nil :read-only t)
  (unpaginate nil :read-only nil)
  (noerror    nil :read-only t)
  (reader     nil :read-only t)
  (callback   nil :read-only t)
  (errorback  nil :read-only t)
  (value      nil :read-only nil)
  (extra      nil :read-only nil))

(defalias 'ghub-req-extra #'ghub--req-extra)

;;;; API

(define-error 'ghub-error "Ghub/Url Error" 'error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)

(defvar ghub-response-headers nil
  "The headers returned in response to the last request.
`ghub-request' returns the response body and stores the
response headers in this variable.")

(cl-defun ghub-head (resource
                     &optional params
                     &key query payload headers
                     silent unpaginate noerror reader
                     username auth host
                     callback errorback extra)
  "Make a `HEAD' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"HEAD\" as METHOD."
  (ghub-request "HEAD" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-get (resource
                    &optional params
                    &key query payload headers
                    silent unpaginate noerror reader
                    username auth host
                    callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD."
  (ghub-request "GET" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-put (resource
                    &optional params
                    &key query payload headers
                    silent unpaginate noerror reader
                    username auth host
                    callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD."
  (ghub-request "PUT" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-post (resource
                     &optional params
                     &key query payload headers
                     silent unpaginate noerror reader
                     username auth host
                     callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD."
  (ghub-request "POST" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-patch (resource
                      &optional params
                      &key query payload headers
                      silent unpaginate noerror reader
                      username auth host
                      callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD."
  (ghub-request "PATCH" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-delete (resource
                       &optional params
                       &key query payload headers
                       silent unpaginate noerror reader
                       username auth host
                       callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD."
  (ghub-request "DELETE" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-request ( method resource
                         &optional params
                         &key query payload headers
                         silent unpaginate noerror reader
                         username auth host forge
                         callback errorback value extra)
  "Make a request for RESOURCE and return the response body.

Also place the response headers in `ghub-response-headers'.

METHOD is the HTTP method, given as a string.
RESOURCE is the resource to access, given as a string beginning
  with a slash.

PARAMS, QUERY, PAYLOAD and HEADERS are alists used to specify
  data.  The Github API documentation is vague on how data has
  to be transmitted and for a particular resource usually just
  talks about \"parameters\".  Generally speaking when the METHOD
  is \"HEAD\" or \"GET\", then they have to be transmitted as a
  query, otherwise as a payload.
Use PARAMS to automatically transmit like QUERY or PAYLOAD would
  depending on METHOD.
Use QUERY to explicitly transmit data as a query.
Use PAYLOAD to explicitly transmit data as a payload.
  Instead of an alist, PAYLOAD may also be a string, in which
  case it gets encoded as UTF-8 but is otherwise transmitted as-is.
Use HEADERS for those rare resources that require that the data
  is transmitted as headers instead of as a query or payload.
  When that is the case, then the API documentation usually
  mentions it explicitly.

If SILENT is non-nil, then don't message progress reports and
  the like.

If UNPAGINATE is t, then make as many requests as necessary to
  get all values.  If UNPAGINATE is a natural number, then get
  at most that many pages.  For any other non-nil value raise
  an error.
If NOERROR is non-nil, then do not raise an error if the request
  fails and return nil instead.  If NOERROR is `return', then
  return the error payload instead of nil.
If READER is non-nil, then it is used to read and return from the
  response buffer.  The default is `ghub--read-json-payload'.
  For the very few resources that do not return JSON, you might
  want to use `ghub--decode-payload'.

If USERNAME is non-nil, then make a request on behalf of that
  user.  It is better to specify the user using the Git variable
  `github.user' for \"api.github.com\", or `github.HOST.user' if
  connecting to a Github Enterprise instance.

Each package that uses `ghub' should use its own token.  If AUTH
  is nil, then the generic `ghub' token is used instead.  This
  is only acceptable for personal utilities.  A packages that
  is distributed to other users should always use this argument
  to identify itself, using a symbol matching its name.

  Package authors who find this inconvenient should write a
  wrapper around this function and possibly for the
  method-specific functions as well.

  Some symbols have a special meaning.  `none' means to make an
  unauthorized request.  `basic' means to make a password based
  request.  If the value is a string, then it is assumed to be
  a valid token.  `basic' and an explicit token string are only
  intended for internal and debugging uses.

If HOST is non-nil, then connect to that Github instance.  This
  defaults to \"api.github.com\".  When a repository is connected
  to a Github Enterprise instance, then it is better to specify
  that using the Git variable `github.host' instead of using this
  argument.

If FORGE is `gitlab', then connect to Gitlab.com or, depending
  on HOST, to another Gitlab instance.  This is only intended for
  internal use.  Instead of using this argument you should use
  function `glab-request' and other `glab-*' functions.

If CALLBACK and/or ERRORBACK is non-nil, then make one or more
  asynchronous requests and call CALLBACK or ERRORBACK when
  finished.  If no error occurred, then call CALLBACK, unless
  that is nil.

  If an error occurred, then call ERRORBACK, or if that is nil,
  then CALLBACK.  ERRORBACK can also be t, in which case an error
  is signaled instead.  NOERROR is ignored for all asynchronous
  requests.

Both callbacks are called with four arguments.
  1. For CALLBACK, the combined value of the retrieved pages.
     For ERRORBACK, the error that occurred when retrieving the
     last page.
  2. The headers of the last page as an alist.
  3. Status information provided by `url-retrieve'. Its `:error'
     property holds the same information as ERRORBACK's first
     argument.
  4. A `ghub--req' struct, which can be passed to `ghub-continue'
     (which see) to retrieve the next page, if any."
  (cl-assert (or (booleanp unpaginate) (natnump unpaginate)))
  (unless (string-prefix-p "/" resource)
    (setq resource (concat "/" resource)))
  (unless host
    (setq host (ghub--host forge)))
  (unless (or username (stringp auth) (eq auth 'none))
    (setq username (ghub--username host forge)))
  (cond ((not params))
        ((member method '("GET" "HEAD"))
         (when query
           (error "PARAMS and QUERY are mutually exclusive for METHOD %S"
                  method))
         (setq query params))
        (t
         (when payload
           (error "PARAMS and PAYLOAD are mutually exclusive for METHOD %S"
                  method))
         (setq payload params)))
  (when (or callback errorback)
    (setq noerror t))
  (ghub--retrieve
   (ghub--encode-payload payload)
   (ghub--make-req
    :url (url-generic-parse-url
          (concat (if (member host ghub-insecure-hosts) "http://" "https://")
                  (cond ((and (equal resource "/graphql")
                              (string-suffix-p "/v3" host))
                         ;; Needed for some Github Enterprise instances.
                         (substring host 0 -3))
                        ((and (equal resource "/api/graphql")
                              (string-suffix-p "/api/v4" host))
                         ;; Needed for all Gitlab instances.
                         (substring host 0 -7))
                        (host))
                  resource
                  (and query (concat "?" (ghub--url-encode-params query)))))
    :forge forge
    :silent silent
    ;; Encode in case caller used (symbol-name 'GET). #35
    :method     (encode-coding-string method 'utf-8)
    :headers    (ghub--headers headers host auth username forge)
    :handler    #'ghub--handle-response
    :unpaginate unpaginate
    :noerror    noerror
    :reader     reader
    :callback   callback
    :errorback  errorback
    :value      value
    :extra      extra)))

(defun ghub-continue (req)
  "If there is a next page, then retrieve that.

This function is only intended to be called from callbacks.  If
there is a next page, then retrieve that and return the buffer
that the result will be loaded into, or t if the process has
already completed.  If there is no next page, then return nil.

Callbacks are called with four arguments (see `ghub-request').
The forth argument is a `ghub--req' struct, intended to be passed
to this function.  A callback may use the struct's `extra' slot
to pass additional information to the callback that will be
called after the next request has finished.  Use the function
`ghub-req-extra' to get and set the value of this slot."
  (and (assq 'next (ghub-response-link-relations req))
       (or (ghub--retrieve nil req) t)))

(cl-defun ghub-wait (resource
                     &optional duration
                     &key username auth host forge)
  "Busy-wait up to DURATION seconds for RESOURCE to become available.

DURATION specifies how many seconds to wait at most.  It defaults
to 64 seconds.  The first attempt is made immediately, the second
after two seconds, and each subsequent attempt is made after
waiting as long again as we already waited between all preceding
attempts combined.

See `ghub-request' for information about the other arguments."
  (unless duration
    (setq duration 64))
  (with-local-quit
    (let ((total 0))
      (while (not (ghub-request "GET" resource nil
                                :noerror t
                                :username username
                                :auth auth
                                :host host
                                :forge forge))
        (message "Waited (%3ss of %ss) for %s..." total duration resource)
        (if (= total duration)
            (error "%s is taking too long to create %s"
                   (if forge (capitalize (symbol-name forge)) "Github")
                   resource)
          (if (> total 0)
              (let ((wait (min total (- duration total))))
                (sit-for wait)
                (cl-incf total wait))
            (sit-for (setq total 2))))))))

(defun ghub-response-link-relations (req &optional headers payload)
  "Return an alist of link relations in HEADERS.
If optional HEADERS is nil, then return those that were
previously stored in the variable `ghub-response-headers'.

When accessing a Bitbucket instance then the link relations
are in PAYLOAD instead of HEADERS, making their API merely
RESTish and forcing this function to append those relations
to the value of `ghub-response-headers', for later use when
this function is called with nil for PAYLOAD."
  (if (eq (ghub--req-forge req) 'bitbucket)
      (if payload
          (let* ((page (cl-mapcan (lambda (key)
                                    (and-let* ((elt (assq key payload)))
                                      (list elt)))
                                  '(size page pagelen next previous)))
                 (headers (cons (cons 'link-alist page) headers)))
            (if (and req (or (ghub--req-callback req)
                             (ghub--req-errorback req)))
                (setq-local ghub-response-headers headers)
              (setq-default ghub-response-headers headers))
            page)
        (cdr (assq 'link-alist ghub-response-headers)))
    (and-let* ((headers (or headers ghub-response-headers))
               (rels (cdr (or (assoc "Link" headers)
                              (assoc "link" headers)))))
      (mapcar (lambda (elt)
                (pcase-let ((`(,url ,rel) (split-string elt "; ")))
                  (cons (intern (substring rel 5 -1))
                        (substring url 1 -1))))
              (split-string rels ", ")))))

(cl-defun ghub-repository-id (owner name &key username auth host forge noerror)
  "Return the id of the specified repository.
Signal an error if the id cannot be determined."
  (let ((fn (cl-case forge
              ((nil ghub github) #'ghub--repository-id)
              (gitlab            #'glab-repository-id)
              (gitea             #'gtea-repository-id)
              (gogs              #'gogs-repository-id)
              (bitbucket         #'buck-repository-id)
              (t (intern (format "%s-repository-id" forge))))))
    (unless (fboundp fn)
      (error "ghub-repository-id: Forge type/abbreviation `%s' is unknown"
             forge))
    (or (funcall fn owner name :username username :auth auth :host host)
        (and (not noerror)
             (error "Repository %S does not exist on %S.\n%s%S?"
                    (concat owner "/" name)
                    (or host (ghub--host forge))
                    "Maybe it was renamed and you have to update "
                    "remote.<remote>.url")))))

;;;; Internal

(defvar ghub-use-workaround-for-emacs-bug t
  "Whether to work around Emacs bug debbugs#34341.

First see https://github.com/magit/ghub/wiki/Known-Issues,
for information about this bug and another related bug.

Because our understanding of these bugs evolved over time,
the possible values of this variable are a bit odd: If t,
enable workaround if necessary (i.e., if Emacs < 26.3 and
GnuTLS >= 3.6.3 are used).  If `force', enable workaround
even if that is believed to be unnecessary.  If nil, do
not enable the workaround.  The default is t.")

(defvar ghub-use-workaround-for-emacs-bug-54989 t
  "Whether to work around Emacs bug debbugs#54989.

First see https://github.com/magit/ghub/wiki/Known-Issues,
for information about this bug and another related bug.

If t, work around the bug if necessary (i.e., if not using Emacs'
\"master\" branch).  If nil, then don't work around the bug.  The
default is t.  Setting this variable only has an effect if it is
done before `ghub' is loaded.")

(cl-defun ghub--retrieve (payload req)
  (let ((url-request-extra-headers
         (let ((headers (ghub--req-headers req)))
           (if (functionp headers) (funcall headers) headers)))
        (url-request-method (ghub--req-method req))
        (url-request-data payload)
        (url-show-status nil)
        (url     (ghub--req-url req))
        (handler (ghub--req-handler req))
        (silent  (ghub--req-silent req))
        (gnutls-algorithm-priority
         (if (and ghub-use-workaround-for-emacs-bug
                  (or (eq ghub-use-workaround-for-emacs-bug 'force)
                      (and (not gnutls-algorithm-priority)
                           (>= libgnutls-version 30603)
                           (version< emacs-version "26.3")
                           ;; (memq (ghub--req-forge req) '(github nil))
                           )))
             "NORMAL:-VERS-TLS1.3"
           gnutls-algorithm-priority)))
    (if (or (ghub--req-callback  req)
            (ghub--req-errorback req))
        (url-retrieve url handler (list req) silent)
      (with-current-buffer
          (url-retrieve-synchronously url silent)
        (funcall handler (car url-callback-arguments) req)))))

(defun ghub--handle-response (status req)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((unpaginate (ghub--req-unpaginate req))
                 (headers    (ghub--handle-response-headers status req))
                 (payload    (ghub--handle-response-payload req))
                 (payload    (ghub--handle-response-error status payload req))
                 (value      (ghub--handle-response-value payload req))
                 (prev       (ghub--req-url req))
                 (next       (cdr (assq 'next (ghub-response-link-relations
                                               req headers payload)))))
            (when (numberp unpaginate)
              (cl-decf unpaginate))
            (setf (ghub--req-url req)
                  (url-generic-parse-url next))
            (setf (ghub--req-unpaginate req) unpaginate)
            (or (and next
                     unpaginate
                     (or (eq unpaginate t)
                         (>  unpaginate 0))
                     (ghub-continue req))
                (let ((callback  (ghub--req-callback req))
                      (errorback (ghub--req-errorback req))
                      (err       (plist-get status :error)))
                  (cond ((and err errorback)
                         (setf (ghub--req-url req) prev)
                         (funcall (if (eq errorback t)
                                      'ghub--errorback
                                    errorback)
                                  err headers status req))
                        (callback
                         (funcall callback value headers status req))
                        (t value))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--handle-response-headers (_status req)
  (goto-char (point-min))
  (forward-line 1)
  (let (headers)
    (when (memq url-http-end-of-headers '(nil 0))
      (setq url-debug t)
      (error "BUG: missing headers; but there's a patch for that \
see https://github.com/magit/ghub/wiki/Known-Issues"))
    (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                              url-http-end-of-headers t)
      (push (cons (match-string 1)
                  (match-string 2))
            headers))
    (setq headers (nreverse headers))
    (goto-char (1+ url-http-end-of-headers))
    (if (and req (or (ghub--req-callback req)
                     (ghub--req-errorback req)))
        (setq-local ghub-response-headers headers)
      (setq-default ghub-response-headers headers))
    headers))

(defun ghub--handle-response-error (status payload req)
  (let ((noerror (ghub--req-noerror req))
        (err (plist-get status :error)))
    (if err
        (if noerror
            (if (eq noerror 'return)
                payload
              (setcdr (last err) (list payload))
              nil)
          (ghub--signal-error err payload req))
      payload)))

(defun ghub--signal-error (err &optional payload req)
  (pcase-let ((`(,symb . ,data) err))
    (if (eq symb 'error)
        (if (eq (car-safe data) 'http)
            (signal 'ghub-http-error
                    (let ((code (car (cdr-safe data))))
                      (list code
                            (nth 2 (assq code url-http-codes))
                            (and req (url-filename (ghub--req-url req)))
                            payload)))
          (signal 'ghub-error data))
      (signal symb data))))

(defun ghub--errorback (err _headers _status req)
  (ghub--signal-error err (nth 3 err) req))

(defun ghub--handle-response-value (payload req)
  (setf (ghub--req-value req)
        (nconc (ghub--req-value req)
               (if-let ((nested (and (eq (ghub--req-forge req) 'bitbucket)
                                     (assq 'values payload))))
                   (cdr nested)
                 payload))))

(defun ghub--handle-response-payload (req)
  (funcall (or (ghub--req-reader req)
               'ghub--read-json-payload)
           url-http-response-status))

(defun ghub--read-json-payload (_status)
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case nil
             (if (and ghub-json-use-jansson
                      (fboundp 'json-parse-string))
                 (json-parse-string
                  raw
                  :object-type  ghub-json-object-type
                  :array-type   ghub-json-array-type
                  :false-object nil
                  :null-object  nil)
               (require 'json)
               (let ((json-object-type ghub-json-object-type)
                     (json-array-type  ghub-json-array-type)
                     (json-false       nil)
                     (json-null        nil))
                 (json-read-from-string raw)))
           ((json-parse-error json-readtable-error)
            `((message
               . ,(if (looking-at "<!DOCTYPE html>")
                      (if (re-search-forward
                           "<p>\\(?:<strong>\\)?\\([^<]+\\)" nil t)
                          (match-string 1)
                        "error description missing")
                    (string-trim (buffer-substring (point) (point-max)))))
              (documentation_url
               . "https://github.com/magit/ghub/wiki/Github-Errors")))))))

(defun ghub--decode-payload (&optional _status)
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point) (point-max))
        'utf-8)))

(defun ghub--encode-payload (payload)
  (and payload
       (progn
         (unless (stringp payload)
           (setq payload
                 (if (and ghub-json-use-jansson
                          (fboundp 'json-serialize))
                     (json-serialize payload
                                     ;; :object-type and :array-type
                                     ;; are not supported here.
                                     :false-object nil
                                     :null-object  :null)
                   (require 'json)
                   (let ((json-object-type ghub-json-object-type)
                         (json-array-type  ghub-json-array-type)
                         (json-false       nil)
                         (json-null        :null))
                     ;; Unfortunately `json-encode' may modify the input.
                     ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40693.
                     ;; and https://github.com/magit/forge/issues/267
                     (json-encode (copy-tree payload))))))
         (encode-coding-string payload 'utf-8))))

(defun ghub--url-encode-params (params)
  (mapconcat (lambda (param)
               (pcase-let ((`(,key . ,val) param))
                 (concat (url-hexify-string (symbol-name key)) "="
                         (cl-typecase val
                           (integer (number-to-string val))
                           (boolean (if val "true" "false"))
                           (t (url-hexify-string val))))))
             params "&"))

;;; Authentication
;;;; API

;;;###autoload
(defun ghub-clear-caches ()
  "Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'."
  (interactive)
  (setq url-http-real-basic-auth-storage nil)
  (auth-source-forget+))

;;;; Internal

(defun ghub--headers (headers host auth username forge)
  (push (cons "Content-Type" "application/json") headers)
  (if (eq auth 'none)
      headers
    (unless (or username (stringp auth))
      (setq username (ghub--username host forge)))
    (lambda ()
      (if (eq auth 'basic)
          (cons (cons "Authorization" (ghub--basic-auth host username))
                headers)
        (cons (ghub--auth host auth username forge) headers)))))

(cl-defgeneric ghub--auth (host auth &optional username forge)
  (unless username
    (setq username (ghub--username host forge)))
  (if (eq auth 'basic)
      (cl-ecase forge
        ((nil gitea gogs bitbucket)
         (cons "Authorization" (ghub--basic-auth host username)))
        ((github gitlab)
         (error "%s does not support basic authentication"
                (capitalize (symbol-name forge)))))
    (cons (cl-ecase forge
            ((nil github gitea gogs bitbucket)
             "Authorization")
            (gitlab
             "Private-Token"))
          (if (eq forge 'bitbucket)
              ;; For some undocumented reason Bitbucket supports
              ;; values of the form "token <token>" only for GET
              ;; requests.  For PUT requests we have to use basic
              ;; authentication.  Note that the secret is a token
              ;; (aka "app password"), not the actual password.
              ;; The documentation fails to mention this little
              ;; detail.  See #97.
              (concat "Basic "
                      (base64-encode-string
                       (concat username ":"
                               (ghub--token host username auth nil forge))
                       t))
            (concat
             (and (not (eq forge 'gitlab)) "token ")
             (encode-coding-string
              (cl-typecase auth
                (string auth)
                (null   (ghub--token host username 'ghub nil forge))
                (symbol (ghub--token host username auth  nil forge))
                (t (signal 'wrong-type-argument
                           `((or stringp symbolp) ,auth))))
              'utf-8))))))

(defun ghub--basic-auth (host username)
  (let ((url (url-generic-parse-url
              (if (member host ghub-insecure-hosts) "http://" "https://"))))
    (setf (url-user url) username)
    (url-basic-auth url t)))

(defun ghub--token (host username package &optional nocreate forge)
  (let* ((user (ghub--ident username package))
         (token
          (or (car (ghub--auth-source-get (list :secret)
                     :host host :user user))
              (progn
                ;; Auth-Source caches the information that there is no
                ;; value, but in our case that is a situation that needs
                ;; fixing so we want to keep trying by invalidating that
                ;; information.
                ;; The (:max 1) is needed and has to be placed at the
                ;; end for Emacs releases before 26.1.  #24 #64 #72
                (auth-source-forget (list :host host :user user :max 1))
                (and (not nocreate)
                     (error "\
Required %s token (\"%s\" for \"%s\") does not exist.
See https://magit.vc/manual/ghub/Getting-Started.html
or (info \"(ghub)Getting Started\") for instructions.
\(The setup wizard no longer exists.)"
                            (capitalize (symbol-name (or forge 'github)))
                            user host))))))
    (if (functionp token) (funcall token) token)))

(cl-defgeneric ghub--host (&optional forge)
  (cl-ecase forge
    ((nil github)
     (or (ignore-errors (car (process-lines "git" "config" "github.host")))
         ghub-default-host))
    (gitlab
     (or (ignore-errors (car (process-lines "git" "config" "gitlab.host")))
         (bound-and-true-p glab-default-host)))
    (gitea
     (or (ignore-errors (car (process-lines "git" "config" "gitea.host")))
         (bound-and-true-p gtea-default-host)))
    (gogs
     (or (ignore-errors (car (process-lines "git" "config" "gogs.host")))
         (bound-and-true-p gogs-default-host)))
    (bitbucket
     (or (ignore-errors (car (process-lines "git" "config" "bitbucket.host")))
         (bound-and-true-p buck-default-host)))))

(cl-defgeneric ghub--username (host &optional forge)
  (let ((var
         (cl-ecase forge
           ((nil github)
            (if (equal host ghub-default-host)
                "github.user"
              (format "github.%s.user" host)))
           (gitlab
            (if (equal host "gitlab.com/api/v4")
                "gitlab.user"
              (format "gitlab.%s.user" host)))
           (bitbucket
            (if (equal host "api.bitbucket.org/2.0")
                "bitbucket.user"
              (format "bitbucket.%s.user" host)))
           (gitea
            (when (zerop (call-process "git" nil nil nil "config" "gitea.host"))
              (error "gitea.host is set but always ignored"))
            (format "gitea.%s.user" host))
           (gogs
            (when (zerop (call-process "git" nil nil nil "config" "gogs.host"))
              (error "gogs.host is set but always ignored"))
            (format "gogs.%s.user"  host)))))
    (condition-case nil
        (car (process-lines "git" "config" var))
      (error
       (let ((user (read-string
                    (format "Git variable `%s' is unset.  Set to: " var))))
         (if (equal user "")
             (user-error "The empty string is not a valid username")
           (call-process
            "git" nil nil nil "config"
            (if (eq (read-char-choice
                     (format "Set %s=%s [g]lobally (recommended) or [l]ocally? "
                             var user)
                     (list ?g ?l))
                    ?g)
                "--global"
              "--local")
            var user)
           user))))))

(defun ghub--ident (username package)
  (format "%s^%s" username package))

(defun ghub--auth-source-get (keys &rest spec)
  (declare (indent 1))
  (let ((plist (car (apply #'auth-source-search
                           (append spec (list :max 1))))))
    (mapcar (lambda (k)
              (plist-get plist k))
            keys)))

(when (version< emacs-version "26.2")
  ;; Fixed by Emacs commit 60ff8101449eea3a5ca4961299501efd83d011bd.
  (advice-add 'auth-source-netrc-parse-next-interesting :around
              'auth-source-netrc-parse-next-interesting@save-match-data)
  (defun auth-source-netrc-parse-next-interesting@save-match-data (fn)
    "Save match-data for the benefit of caller `auth-source-netrc-parse-one'.
Without wrapping this function in `save-match-data' the caller
won't see the secret from a line that is followed by a commented
line."
    (save-match-data (funcall fn))))

(when (< emacs-major-version 28)
  ;; Fixed by Emacs commit 0b98ea5fbe276c67206896dca111c000f984ee0f.
  (advice-add 'url-http-handle-authentication :around
              'url-http-handle-authentication@unauthorized-bugfix)
  (defun url-http-handle-authentication@unauthorized-bugfix (fn proxy)
    "If authorization failed then don't try again but fail properly.
For Emacs 27.1 prevent a useful `http' error from being replaced
by a generic one that omits all useful information.  For earlier
releases prevent a new request from being made, which would
either result in an infinite loop or (e.g., in the case of `ghub')
the user being asked for their name."
    (if (assoc "Authorization" url-http-extra-headers)
        t ; Return "success", here also known as "successfully failed".
      (funcall fn proxy))))

(when (and (< emacs-major-version 29)
           ghub-use-workaround-for-emacs-bug-54989)
  ;; Fixed in Emacs commit 0829c6836eff14dda0cf8b3047376967f7b000f4.
  ;; Cleanup from 26faa2b943675107e1664b2fea7174137c473475 is not
  ;; included in this copy because doing that would require changes
  ;; to more functions.  This function has seen a few other changes
  ;; since Emacs 25.1, the oldest version we still support.  Of these
  ;; only 4f1df40db36b221e7842bd75d6281922dcb268ee makes a functional
  ;; change, fixing debbug#35658.  The first release to contain that
  ;; commit is 27.1.  That commit either fixes a related bug or it
  ;; deals with the same bug but only partially fixes it.
  (advice-add 'url-http-chunked-encoding-after-change-function :override
              'url-http-chunked-encoding-after-change-function@54989-backport)
  (defvar url-http-chunked-last-crlf-missing nil)
  (defvar url-http-content-type)
  (defvar url-http-chunked-start)
  (defvar url-http-chunked-length)
  (defvar url-http-chunked-counter)
  (defun url-http-chunked-encoding-after-change-function@54989-backport
      (st nd length)
    "Backport bugfix from https://debbugs.gnu.org/cgi/bugreport.cgi?bug=54989."
    (if url-http-chunked-last-crlf-missing
        (progn
          (goto-char url-http-chunked-last-crlf-missing)
          (if (not (looking-at "\r\n"))
              (url-http-debug
               "Still spinning for the terminator of last chunk...")
            (url-http-debug "Saw the last CRLF.")
            (delete-region (match-beginning 0) (match-end 0))
            (when (url-http-parse-headers)
              (url-http-activate-callback))))
      (save-excursion
        (goto-char st)
        (let ((read-next-chunk t)
              (case-fold-search t)
              (regexp nil)
              (no-initial-crlf nil))
          ;; We need to loop thru looking for more chunks even within
          ;; one after-change-function call.
          (while read-next-chunk
            (setq no-initial-crlf (= 0 url-http-chunked-counter))
            (with-no-warnings
              (if url-http-content-type
                  (url-display-percentage nil
                                          "Reading [%s]... chunk #%d"
                                          url-http-content-type url-http-chunked-counter)
                (url-display-percentage nil
                                        "Reading... chunk #%d"
                                        url-http-chunked-counter)))
            (url-http-debug "Reading chunk %d (%d %d %d)"
                            url-http-chunked-counter st nd length)
            (setq regexp (if no-initial-crlf
                             "\\([0-9a-z]+\\).*\r?\n"
                           "\r?\n\\([0-9a-z]+\\).*\r?\n"))

            (if url-http-chunked-start
                ;; We know how long the chunk is supposed to be, skip over
                ;; leading crap if possible.
                (if (> nd (+ url-http-chunked-start url-http-chunked-length))
                    (progn
                      (url-http-debug "Got to the end of chunk #%d!"
                                      url-http-chunked-counter)
                      (goto-char (+ url-http-chunked-start
                                    url-http-chunked-length)))
                  (url-http-debug "Still need %d bytes to hit end of chunk"
                                  (- (+ url-http-chunked-start
                                        url-http-chunked-length)
                                     nd))
                  (setq read-next-chunk nil)))
            (if (not read-next-chunk)
                (url-http-debug "Still spinning for next chunk...")
              (if no-initial-crlf (skip-chars-forward "\r\n"))
              (if (not (looking-at regexp))
                  (progn
                    ;; Must not have received the entirety of the chunk header,
                    ;; need to spin some more.
                    (url-http-debug "Did not see start of chunk @ %d!" (point))
                    (setq read-next-chunk nil))
                ;; The data we got may have started in the middle of the
                ;; initial chunk header, so move back to the start of the
                ;; line and re-compute.
                (when (= url-http-chunked-counter 0)
                  (beginning-of-line)
                  (looking-at regexp))
                (add-text-properties (match-beginning 0) (match-end 0)
                                     (list 'chunked-encoding t
                                           'face 'cursor
                                           'invisible t))
                (setq url-http-chunked-length
                      (string-to-number (buffer-substring (match-beginning 1)
                                                          (match-end 1))
                                        16)
                      url-http-chunked-counter (1+ url-http-chunked-counter)
                      url-http-chunked-start (set-marker
                                              (or url-http-chunked-start
                                                  (make-marker))
                                              (match-end 0)))
                (delete-region (match-beginning 0) (match-end 0))
                (url-http-debug "Saw start of chunk %d (length=%d, start=%d"
                                url-http-chunked-counter url-http-chunked-length
                                (marker-position url-http-chunked-start))
                (if (= 0 url-http-chunked-length)
                    (progn
                      ;; Found the end of the document!  Wheee!
                      (url-http-debug "Saw end of stream chunk!")
                      (setq read-next-chunk nil)
                      (with-no-warnings
                        (url-display-percentage nil nil))
                      ;; Every chunk, even the last 0-length one, is
                      ;; terminated by CRLF.  Skip it.
                      (if (not (looking-at "\r?\n"))
                          (progn
                            (url-http-debug
                             "Spinning for the terminator of last chunk...")
                            (setq-local url-http-chunked-last-crlf-missing
                                        (point)))
                        (url-http-debug "Removing terminator of last chunk")
                        (delete-region (match-beginning 0) (match-end 0))
                        (when (re-search-forward "^\r?\n" nil t)
                          (url-http-debug "Saw end of trailers..."))
                        (when (url-http-parse-headers)
                          (url-http-activate-callback)))))))))))))

;;; _
(provide 'ghub)
(require 'ghub-graphql)
;;; ghub.el ends here
