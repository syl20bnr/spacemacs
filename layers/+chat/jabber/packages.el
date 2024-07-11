;;; packages.el --- jabber Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Tosh Lyons <tosh.lyons@gmail.com>
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


(defconst jabber-packages
  '(
    jabber
    window-purpose))

(defun jabber/init-jabber ()
  (use-package jabber
    :defer t
    :init
    (add-hook 'jabber-post-connect-hooks 'spacemacs/jabber-connect-hook)
    (spacemacs/set-leader-keys "acj" 'jabber-connect-all)
    :config
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
    (evilified-state-evilify-map jabber-roster-mode-map
      :mode jabber-roster-mode
      :bindings
      "j" 'jabber-go-to-next-roster-item
      "k" 'jabber-go-to-previous-roster-item)))

(defun jabber/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :jabber-layer
   (purpose-conf :mode-purposes '((jabber-browse-mode . chat)
                                  (jabber-chat-mode . chat)
                                  (jabber-console-mode . chat)
                                  (jabber-roster-mode . chat)))))
