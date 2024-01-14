;;; magit-bookmark.el --- Bookmark support for Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Inspired by an earlier implementation by Yuri Khan.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for bookmarks for most Magit buffers.

;;; Code:

(require 'magit)

;;; Diff
;;;; Diff

(put 'magit-diff-mode 'magit-bookmark-variables
     '(magit-buffer-range-hashed
       magit-buffer-typearg
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-diff-mode))
  (format "magit-diff(%s%s)"
          (pcase (magit-diff-type)
            ('staged "staged")
            ('unstaged "unstaged")
            ('committed magit-buffer-range)
            ('undefined
             (delq nil (list magit-buffer-typearg magit-buffer-range-hashed))))
          (if magit-buffer-diff-files
              (concat " -- " (mapconcat #'identity magit-buffer-diff-files " "))
            "")))

;;;; Revision

(put 'magit-revision-mode 'magit-bookmark-variables
     '(magit-buffer-revision-hash
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-revision-mode))
  (format "magit-revision(%s %s)"
          (magit-rev-abbrev magit-buffer-revision)
          (if magit-buffer-diff-files
              (mapconcat #'identity magit-buffer-diff-files " ")
            (magit-rev-format "%s" magit-buffer-revision))))

;;;; Stash

(put 'magit-stash-mode 'magit-bookmark-variables
     '(magit-buffer-revision-hash
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-stash-mode))
  (format "magit-stash(%s %s)"
          (magit-rev-abbrev magit-buffer-revision)
          (if magit-buffer-diff-files
              (mapconcat #'identity magit-buffer-diff-files " ")
            (magit-rev-format "%s" magit-buffer-revision))))

;;; Log
;;;; Log

(put 'magit-log-mode 'magit-bookmark-variables
     '(magit-buffer-revisions
       magit-buffer-log-args
       magit-buffer-log-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-log-mode))
  (format "magit-log(%s%s)"
          (mapconcat #'identity magit-buffer-revisions " ")
          (if magit-buffer-log-files
              (concat " -- " (mapconcat #'identity magit-buffer-log-files " "))
            "")))

;;;; Cherry

(put 'magit-cherry-mode 'magit-bookmark-variables
     '(magit-buffer-refname
       magit-buffer-upstream))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-cherry-mode))
  (format "magit-cherry(%s > %s)"
          magit-buffer-refname
          magit-buffer-upstream))

;;;; Reflog

(put 'magit-reflog-mode 'magit-bookmark-variables
     '(magit-buffer-refname))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-reflog-mode))
  (format "magit-reflog(%s)" magit-buffer-refname))

;;; Misc

(put 'magit-status-mode 'magit-bookmark-variables nil)

(put 'magit-refs-mode 'magit-bookmark-variables
     '(magit-buffer-upstream
       magit-buffer-arguments))

(put 'magit-stashes-mode 'magit-bookmark-variables nil)

(cl-defmethod magit-bookmark-name (&context (major-mode magit-stashes-mode))
  (format "magit-states(%s)" magit-buffer-refname))

;;; _
(provide 'magit-bookmark)
;;; magit-bookmark.el ends here
