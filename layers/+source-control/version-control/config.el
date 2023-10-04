;;; config.el --- Version Control configuration File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defvar spacemacs--smerge-ts-full-hint-toggle nil
  "Display smerge transient-state documentation.")

(defvar version-control-global-margin t
  "If non-nil, will show diff margins globally.")

(defvar version-control-diff-tool 'diff-hl
  "Options are `git-gutter', `git-gutter+', and `diff-hl' to show
version-control markers.")

(defvar version-control-diff-side 'right
  "Side on which to show version-control markers.
Options are `left' and `right'.")

;; unchanged face
(defface git-gutter+-unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)
(defface git-gutter:unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)

;; change face
(defface git-gutter+-modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface git-gutter:modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface diff-hl-change
  '((default :foreground "blue3")
    (((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "#333355"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

;; added face
(defface git-gutter+-added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface git-gutter:added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface diff-hl-insert
  '((default :inherit diff-added)
    (((class color)) :foreground "green4"))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

;; deleted face
(defface git-gutter+-deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface git-gutter:deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface diff-hl-delete
  '((default :inherit diff-removed)
    (((class color)) :foreground "red3"))
  "Face used to highlight deleted lines."
  :group 'diff-hl)
