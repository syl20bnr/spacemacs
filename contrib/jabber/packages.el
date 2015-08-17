;;; packages.el --- jabber Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Tosh Lyons <tosh.lyons@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq jabber-packages '(jabber))

(defun jabber/init-jabber ()
  (use-package jabber
    :defer t
    :init (evil-leader/set-key "aj" 'jabber-connect-all)
    :config (evil-leader/set-key-for-mode 'jabber-roster-mode
              "ma" 'jabber-send-presence
              "mb" 'jabber-get-browse
              "md" 'jabber-disconnect
              "me" 'jabber-roster-edit-action-at-point
              "mg" 'jabber-display-roster
              "mi" 'jabber-get-disco-items
              "mj" 'jabber-muc-join
              "mq" 'bury-buffer
              "mr" 'jabber-roster-toggle-offline-display
              "ms" 'jabber-send-subscription-request
              "mv" 'jabber-get-version
              "m RET" 'jabber-roster-ret-action-at-point)))
