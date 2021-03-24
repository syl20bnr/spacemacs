;;; config.el --- Org configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Dumper

(defun org/pre-dump ()
  (spacemacs/dump-modes '(org-mode)))

;; Variables

(defvar org-want-todo-bindings nil
  "If non-nil, evil-org's todo bindings are activated.")

(defvar org-enable-bootstrap-support nil
  "If non-nil Twitter Bootstrap related packages are configured.")

(defvar org-enable-github-support nil
  "If non-nil GitHub related packages are configured.")

(defvar org-enable-reveal-js-support nil
  "If non-nil, enable export to reveal.js.")

(defvar org-projectile-file "TODOs.org"
  "The file to store project TODOs in. If this is a relative
path, one file per project is used (and the path is relative to
the project root). If it an absolute path, one global file is
used.")

(defvar org-enable-notifications nil
  "If non-nil org-wild-notifier is configured.")

(defvar org-start-notification-daemon-on-startup nil
  "If non-nil start the notification daemon on startup.")

(defvar org-enable-org-contacts-support nil
  "If non-nil org-contacts is configured.")

(defvar org-enable-org-journal-support nil
  "If non-nil org-journal is configured.")

(defvar org-enable-sticky-header nil
  "If non-nil org-sticky-header is configured.")

(defvar org-enable-hugo-support nil
  "If non-nil, Hugo (https://gohugo.io) related packages are configured.")

(defvar org-enable-trello-support nil
  "If non-nil org-trello is configured")

(defvar org-enable-epub-support nil
  "If non-nil org-epub is configured")

(defvar org-enable-jira-support nil
  "If non-nil, Jira (https://www.atlassian.com/software/jira) related packages
are configured.")

(defvar org-enable-verb-support nil
  "If non-nil, Verb (https://github.com/federicotdn/verb) is configured.")

(defvar org-enable-roam-support nil
  "If non-nil, org-roam (https://www.orgroam.com/) is configured")

(defvar org-persp-startup-org-file nil
  "If non-nil, opens the specified file instead of the first in org-agenda-files")

(defvar org-persp-startup-with-agenda nil
  "If non-nil, opens the specified agenda custom view")

(defvar org-enable-valign nil
  "If non-nil, enable valign-mode in org-mode buffers.
ATTENTION: `valign-mode' will be laggy working with tables contain more than 100 lines.")

(defvar org-enable-appear-support nil
  "If non-nil, enable org-appear in org-mode buffers.")
