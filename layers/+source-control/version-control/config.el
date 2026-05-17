;;; config.el --- Version Control configuration File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

(spacemacs|defc version-control-margin t
  "Whether to display diff indicators in the margin or fringe.

The git-gutter diff tool only supports showing indicators in the margin.

The diff-hl diff tool shows indicators in the fringe for graphical
frames and in the margin for TTY frames (which do not support the
fringe)."
  '(choice (const auto) (const global) boolean))

(spacemacs|defc version-control-diff-tool 'diff-hl
  "Options are `diff-hl' (the preferred choice) or `git-gutter' to show
version-control markers, `nil' to disable this feature."
  '(choice (const diff-hl) (const git-gutter) (const nil)))

(spacemacs|defc version-control-diff-side 'right
  "Side on which to show version-control markers.
Options are `left' and `right'."
  '(choice (const left) (const right)))
