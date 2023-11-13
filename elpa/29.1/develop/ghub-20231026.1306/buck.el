;;; buck.el --- Client library for the Bitbucket API  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 Jonas Bernoulli

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

;; Buck is a library that provides basic support for using the Bitbucket API
;; from Emacs packages.  It abstracts access to API resources using only
;; a handful of functions that are not resource-specific.

;; This library is implemented on top of Ghub.  Unlike Ghub, Buck does
;; not support the guided creation of tokens because Bitbucket lacks the
;; features that would be necessary to implement that.  Users have to
;; create tokens through the web interface.

;;; Code:

(require 'ghub)

(defconst buck-default-host "api.bitbucket.org/2.0"
  "The default host that is used if `buck.host' is not set.")

;; HEAD and PATCH are not supported according to
;; https://developer.atlassian.com/bitbucket/api/2/reference/meta/uri-uuid

(cl-defun buck-get (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD
and `bitbucket' as FORGE."
  (ghub-request "GET" resource params :forge 'bitbucket
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun buck-put (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD
and `bitbucket' as FORGE."
  (ghub-request "PUT" resource params :forge 'bitbucket
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun buck-post (resource &optional params
                              &key query payload headers
                              silent unpaginate noerror reader
                              username auth host
                              callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD
and `bitbucket' as FORGE."
  (ghub-request "POST" resource params :forge 'bitbucket
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun buck-delete (resource &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                username auth host
                                callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD
and `bitbucket' as FORGE."
  (ghub-request "DELETE" resource params :forge 'bitbucket
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun buck-request (method resource &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username auth host
                               callback errorback extra)
  "Make a request for RESOURCE and return the response body.
Like calling `ghub-request' (which see) with `bitbucket' as FORGE."
  (ghub-request method resource params :forge 'bitbucket
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun buck-repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (substring (cdr (assq 'uuid
                        (buck-get (format "/repositories/%s/%s" owner name)
                                  nil
                                  :username username :auth auth :host host)))
             1 -1))

;;; _
(provide 'buck)
;;; buck.el ends here
