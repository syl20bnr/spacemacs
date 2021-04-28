;;; config.el --- Treemacs Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
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


(defvar treemacs-use-follow-mode t
  "When non-nil use `treemacs-follow-mode'.")

(defvar treemacs-use-filewatch-mode t
  "When non-nil use `treemacs-filewatch-mode'.")

(defvar treemacs-use-scope-type 'Frames
  "Determines the scope of treemacs buffers and workspaces.
Possible values are:
 - `Frames' - to scope treemacs to the current frame
 - `Perspectives' - to scope treemacs in conjunction with `persp-mode'.")

(defvar treemacs-use-git-mode nil
  "Type of git integration for `treemacs-git-mode'.
There are 3 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python
3) deferred, which is the same is extended, but delays highlighting for improved
   performance")

(defvar treemacs-lock-width nil
  "When non-nil the treemacs window will not be manually resizable by default.")

(defvar treemacs-use-icons-dired t
  "When non-nil use `treemacs-icons-dired'")

(defvar treemacs-use-all-the-icons-theme nil
  "Enable the treemacs supported `all-the-icons' theme")
