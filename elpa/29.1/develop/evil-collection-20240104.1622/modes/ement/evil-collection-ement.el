;;; evil-collection-ement.el --- Evil integration for Ement.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords: convenience
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil bindings for Ement.el

;;; Code:
(require 'evil-collection)
(require 'ement nil t)
(require 'ement-room-list nil t)

(defgroup evil-collection-ement nil
  "Evil bindings for Ement.el."
  :group 'evil-collection)

(defcustom evil-collection-ement-want-auto-retro nil
  "When non-nil, automatically run `ement-room-retro' when scrolled up at `bob'."
  :type 'boolean)

(defconst evil-collection-ement-maps '(ement-describe-room-mode-map
                                       ement-directory-mode-map
                                       ement-room-list-mode-map
                                       ement-room-mode-map))

(declare-function ement-room-retro "ement-room")
(defun evil-collection-ement--auto-retro ()
  "Call `ement-room-retro' scroll requested from beginning of buffer hit."
  (and (= (line-beginning-position) (point-min))
       (not (eq evil-state 'emacs))
       (member this-command '(previous-line evil-scroll-up))
       (call-interactively #'ement-room-retro)
       (setq this-command 'ement-room-retro)))

(defun evil-collection-ement--install-auto-retro ()
  "Install `evil-collection-ement--auto-retro'."
  (add-hook 'post-command-hook #'evil-collection-ement--auto-retro nil t))

(defun evil-collection-ement-setup ()
  "Set up `evil' bindings for Ement.el."
  (evil-collection-define-key 'normal 'ement-describe-room-mode-map
    (kbd "q") 'quit-window)

  (evil-collection-define-key 'normal 'ement-directory-mode-map
    (kbd "RET")      'ement-directory-RET
    (kbd "<return>") 'ement-directory-RET
    (kbd "s")        'ement-directory-search
    (kbd "q")        'quit-window)

  (evil-collection-define-key 'normal 'ement-room-list-mode-map
    (kbd "d")     'ement-room-list-kill-buffer
    (kbd "x")     'ement-room-leave
    (kbd "X")     'ement-forget-room
    (kbd "RET")   'ement-room-list-RET
    (kbd "<return>") 'ement-room-list-RET)

  (evil-collection-define-key '(normal motion) 'ement-room-mode-map
    (kbd "<")  'ement-room-transient
    (kbd "<return>")   'ement-room-send-message
    (kbd "RET")        'ement-room-send-message
    (kbd "M-RET")      'ement-room-compose-message
    (kbd "<M-return>") 'ement-room-compose-message
    (kbd "a")  'ement-room-send-message
    (kbd "A")  'ement-room-compose-message
    (kbd "d")  'ement-room-delete-message
    (kbd "D")  'ement-room-delete-message
    (kbd "J")  'ement-room-write-reply
    (kbd "]]") 'ement-room-goto-next
    (kbd "[[") 'ement-room-goto-prev
    (kbd "gr") 'ement-room-sync
    (kbd "gu") 'ement-room-goto-fully-read-marker
    (kbd "gm") 'ement-room-mark-read
    (kbd "gv") 'ement-room-view-event
    (kbd "i")  'ement-room-edit-message
    (kbd "q")  'quit-window
    ;; Room prefix bindings
    (kbd "r/") 'ement-room-occur
    (kbd "rd") 'ement-room-describe
    (kbd "rf") 'ement-room-set-message-format
    (kbd "rl") 'ement-room-list
    (kbd "rm") 'ement-list-members
    (kbd "rn") 'ement-room-override-name
    (kbd "rN") 'ement-room-set-notification-state
    (kbd "rt") 'ement-room-set-topic
    (kbd "rT") 'ement-tag-room
    ;; Room membership
    (kbd "Rc") 'ement-create-room
    (kbd "Rj") 'ement-room-join
    (kbd "Rl") 'ement-room-leave
    (kbd "RF") 'ement-room-forget
    (kbd "Rn") 'ement-room-set-display-name
    (kbd "Rs") 'ement-room-toggle-space
    ;; Send prefix bindings
    (kbd "se") 'ement-room-send-emote
    (kbd "sf") 'ement-room-send-file
    (kbd "si") 'ement-room-send-image
    (kbd "sr") 'ement-room-send-reaction
    ;; User prefix bindings
    (kbd "ui") 'ement-invite-user
    (kbd "uI") 'ement-ignore-user
    (kbd "u RET")      'ement-send-direct-message
    (kbd "u <return>") 'ement-send-direct-message)

  (when evil-collection-ement-want-auto-retro
    (add-hook 'ement-room-mode-hook  #'evil-collection-ement--install-auto-retro))
  nil)

(provide 'evil-collection-ement)
;;; evil-collection-ement.el ends here
