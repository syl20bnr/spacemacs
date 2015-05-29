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

(defvar jabber-packages
  '(
    jabber
    ))

;; List of packages to exclude.
(defvar jabber-excluded-packages '())

(defun jabber/init-jabber ()
  "Initialize jabber"
  (use-package jabber
    :defer t
    :init
    (evil-leader/set-key
      "aj" 'jabber-connect-all)
    :config
    (progn
      (evil-leader/set-key-for-mode 'jabber-roster-mode
        "Ja" 'jabber-send-presence
        "Jb" 'jabber-get-browse
        "Jd" 'jabber-disconnect
        "Je" 'jabber-roster-edit-action-at-point
        "Jg" 'jabber-display-roster
        "Ji" 'jabber-get-disco-items
        "Jj" 'jabber-muc-join
        "Jo" 'jabber-roster-toggle-offline-display
        "Jq" 'bury-buffer
        "Js" 'jabber-send-subscription-request
        "Jv" 'jabber-get-version
        "J RET" 'jabber-roster-ret-action-at-point))))
