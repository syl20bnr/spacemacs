;;; config.el --- notmuch Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
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


;; TODO see if we can replace this layer variable with `notmuch-tagging-keys'
(defvar notmuch-message-deleted-tags '("+deleted" "-inbox" "-unread")
  "Tags applied when deleting a message.")

(defvar notmuch-spacemacs-layout-name "@Notmuch"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar notmuch-spacemacs-layout-binding "n"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar notmuch-modes
  '(notmuch-hello-mode
    notmuch-message-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode)
  "Modes that are associated with notmuch buffers.")
