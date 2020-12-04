;;; config.el --- notmuch Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; TODO see if we can replace this layer variable with `notmuch-tagging-keys'
(defvar notmuch-message-deleted-tags '("+deleted" "-inbox" "-unread")
  "Tags applied when deleting a message.")

(defvar notmuch-space-macs-layout-name "@Notmuch"
  "Name used in the setup for `space-macs-layouts' micro-state")

(defvar notmuch-space-macs-layout-binding "n"
  "Binding used in the setup for `space-macs-layouts' micro-state")

(defvar notmuch-modes
  '(notmuch-hello-mode
    notmuch-message-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode)
  "Modes that are associated with notmuch buffers.")


