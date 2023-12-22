;;; orgit-forge.el --- Org links to Forge issue buffers  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/orgit-forge
;; Keywords: hypermedia vc

;; Package-Version: 0.1.4
;; Package-Requires: (
;;     (emacs "25.1")
;;     (compat "29.1.4.1")
;;     (forge "0.3")
;;     (magit "3.3")
;;     (org "9.6")
;;     (orgit "1.9"))

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

;; This package defines the Org link typ `orgit-topic', which can be
;; used to link to Forge topic buffers.

;;; Code:

(require 'compat)

(require 'forge)
(require 'orgit)

(defcustom orgit-topic-description-format "%S %T"
  "Format used for `orgit-topic' links.
%o Owner of repository.
%n Name of repository.
%T Title of topic.
%S Slug of topic.
   Example: \"#123\".  Same as %P%N.
These two are preserved for backward compatibly:
%P Type prefix of topic.
%N Number of topic."
  :package-version '(orgit-forge . "0.1.0")
  :group 'orgit
  :type 'string)

;;;###autoload
(with-eval-after-load "org"
  (org-link-set-parameters "orgit-topic"
                           :store              #'orgit-topic-store
                           :follow             #'orgit-topic-open
                           :export             #'orgit-topic-export
                           :complete           #'orgit-topic-complete-link
                           :insert-description #'orgit-topic-describe-link))

;;;###autoload
(defun orgit-topic-store ()
  "Store a link to a Forge-Topic mode buffer.

When the region selects a topic, then store a link to the
Forge-Topic mode buffer for that topic."
  (cond ((derived-mode-p 'forge-topic-mode)
         (orgit-topic-store-1 forge-buffer-topic))
        ((derived-mode-p 'magit-mode)
         (and-let* ((sections (or (magit-region-sections 'issue)
                                  (magit-region-sections 'pullreq))))
           (progn ; Work around debbugs#31840.
             (dolist (section sections)
               (orgit-topic-store-1 (oref section value)))
             t)))
        ((derived-mode-p 'forge-topic-list-mode)
         (orgit-topic-store-1 (forge-get-topic (tabulated-list-get-id))))))

(defun orgit-topic-store-1 (topic)
  (org-link-store-props
   :type "orgit-topic"
   :link (format "orgit-topic:%s" (oref topic id))
   :description (orgit--topic-format-description topic)))

(defun orgit--topic-format-description (topic)
  (let ((repo (forge-get-repository topic)))
    (format-spec orgit-topic-description-format
                 `((?o . ,(oref repo owner))
                   (?n . ,(oref repo name))
                   (?T . ,(oref topic title))
                   (?S . ,(oref topic slug))
                   (?P . ,(substring (oref topic slug) 0 1))
                   (?N . ,(oref topic number))))))

;;;###autoload
(defun orgit-topic-open (id)
  (forge-topic-setup-buffer (forge-get-topic id)))

;;;###autoload
(defun orgit-topic-export (id desc format)
  (orgit--format-export (forge-get-url (forge-get-topic id))
                        desc
                        format))

;;;###autoload
(defun orgit-topic-complete-link (&optional arg)
  (format "orgit-topic:%s"
          (let ((default-directory (magit-read-repository arg)))
            (oref (forge-get-topic (forge-read-topic "Topic")) id))))

;;;###autoload
(defun orgit-topic-describe-link (link default)
  (or default
      (orgit--topic-format-description
       (forge-get-topic (string-remove-prefix "orgit-topic:" link)))))

;;; _
(provide 'orgit-forge)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orgit-forge.el ends here
