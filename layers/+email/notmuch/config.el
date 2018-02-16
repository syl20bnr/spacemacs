;;; config.el --- notmuch Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
