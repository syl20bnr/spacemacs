;;; packages.el --- jabber Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Tosh Lyons <tosh.lyons@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst jabber-packages
  '(
    jabber
    window-purpose))

(defun jabber/init-jabber ()
  (use-package jabber
    :defer t
    :init
    (progn
      (add-hook 'jabber-post-connect-hooks 'spacemacs/jabber-connect-hook)
      (spacemacs/set-leader-keys "acj" 'jabber-connect-all))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'jabber-roster-mode
        "a" 'jabber-send-presence
        "b" 'jabber-get-browse
        "d" 'jabber-disconnect
        "e" 'jabber-roster-edit-action-at-point
        "g" 'jabber-display-roster
        "i" 'jabber-get-disco-items
        "j" 'jabber-muc-join
        "q" 'bury-buffer
        "r" 'jabber-roster-toggle-offline-display
        "s" 'jabber-send-subscription-request
        "v" 'jabber-get-version
        "RET" 'jabber-roster-ret-action-at-point)
      (evilified-state-evilify jabber-roster-mode jabber-roster-mode-map
        "j" 'jabber-go-to-next-roster-item
        "k" 'jabber-go-to-previous-roster-item))))

(defun jabber/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :jabber-layer
   (purpose-conf :mode-purposes '((jabber-browse-mode . chat)
                                  (jabber-chat-mode . chat)
                                  (jabber-console-mode . chat)
                                  (jabber-roster-mode . chat)))))
