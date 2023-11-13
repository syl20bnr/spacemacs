;;; gtea.el --- Client library for the Gitea API  -*- lexical-binding:t -*-

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

;; Gtea is a library that provides basic support for using the Gitea API
;; from Emacs packages.  It abstracts access to API resources using only
;; a handful of functions that are not resource-specific.

;; This library is implemented on top of Ghub.  Unlike Ghub, Gtea does
;; not support the guided creation of tokens because Gitea lacks the
;; features that would be necessary to implement that.  Users have to
;; create tokens through the web interface.

;;; Code:

(require 'ghub)

(defconst gtea-default-host "localhost:3000/api/v1"
  "The default Gitea host.")

;; HEAD does not appear to be supported.

(cl-defun gtea-get (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD
and `gitea' as FORGE."
  (ghub-request "GET" resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-put (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD
and `gitea' as FORGE."
  (ghub-request "PUT" resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-post (resource &optional params
                              &key query payload headers
                              silent unpaginate noerror reader
                              username auth host
                              callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD
and `gitea' as FORGE."
  (ghub-request "POST" resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-patch (resource &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username auth host
                               callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD
and `gitea' as FORGE."
  (ghub-request "PATCH" resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-delete (resource &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                username auth host
                                callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD
and `gitea' as FORGE."
  (ghub-request "DELETE" resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-request (method resource &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username auth host
                               callback errorback extra)
  "Make a request for RESOURCE and return the response body.
Like calling `ghub-request' (which see) with `gitea' as FORGE."
  (ghub-request method resource params :forge 'gitea
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun gtea-repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (number-to-string
   (cdr (assq 'id (gtea-get (format "/repos/%s/%s" owner name)
                            nil :username username :auth auth :host host)))))

;;; _
(provide 'gtea)
;;; gtea.el ends here
