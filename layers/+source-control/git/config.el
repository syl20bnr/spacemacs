;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


;; Variables

(defvar git-enable-magit-delta-plugin nil
  "If non nil, enable `magit-delta' plugin.")

(defvar git-enable-magit-gitflow-plugin nil
  "If non nil, enable `magit-gitflow' plugin.")

(defvar git-enable-magit-svn-plugin nil
  "If non nil, enable `magit-svn' plugin.")

(defvar git-enable-magit-todos-plugin nil
  "If non nil, enable `magit-todos' plugin.")

(defvar git-magit-status-fullscreen nil
  "If non nil magit-status buffer is displayed in fullscreen.")

(defvar spacemacs--git-blame-ts-full-hint-toggle nil
  "Display git blame transient state documentation.")
