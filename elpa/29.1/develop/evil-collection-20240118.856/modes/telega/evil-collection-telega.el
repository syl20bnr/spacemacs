;;; evil-collection-telega.el --- Evil bindings for telega -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ruoyu Feng

;; Author: Ruoyu Feng <mail@vonfry.name>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.6
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools, telegram, telega

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for telega.

;;; Code:
(require 'telega nil t)
(require 'evil-collection)

(defvar telega-prefix-map)
(defvar telega-filter-map)
(defvar telega-sort-map)
(defvar telega-chatbuf-fastnav-map)
(defvar telega-describe-map)
(defvar telega-folder-map)
(defvar telega-voip-map)
(defvar telega-root-view-map)
(defvar telega-root-fastnav-map)

(defconst evil-collection-telega-maps '(telega-root-mode-map
                                        telega-chat-mode-map
                                        telega-image-mode-map
                                        telega-webpage-mode-map
                                        telega-user-button-map
                                        telega-msg-button-map
                                        telega-chat-button-map
                                        telega-sticker-button-map))

(defconst evil-collection-telega-modes '(telega-root-mode
                                         telega-chat-mode
                                         telega-image-mode
                                         telega-webpage-mode))

;;;###autoload
(defun evil-collection-telega-setup ()
  "Set up `evil' bindings for `telega'."
  (evil-collection-set-readonly-bindings 'telega-root-mode-map)

  (dolist (mode evil-collection-telega-modes)
    (evil-set-initial-state mode 'normal))

  (evil-collection-define-key 'normal 'telega-root-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line

    "ga" telega-prefix-map

    (kbd "<tab>") 'telega-button-forward
    (kbd "<backtab>") 'telega-button-backward

    "S" telega-sort-map
    "s" telega-filter-map
    "_" 'telega-filter-undo
    "-" 'telega-filter-redo

    "c" 'telega-chat-join-by-link
    "C" 'telega-chat-create
    "D" 'telega-chats-filtered-kill-chatbuf
    "R" 'telega-chats-filtered-toggle-read

    "q" 'bury-buffer
    "Q" 'telega-kill

    "g?" telega-describe-map
    "gO" telega-folder-map
    "gC" telega-voip-map
    "gV" telega-root-view-map
    "J" telega-root-fastnav-map

    "gs" 'telega-view-search)

  (evil-collection-define-key 'normal 'telega-chat-mode-map
    "ga" telega-prefix-map
    "gA" telega-chatbuf-fastnav-map

    "zz" 'telega-chatbuf-recenter-1

    "^" 'telega-chatbuf-beginning-of-thing

    "g?" 'telega-describe-chat

    "Za" 'telega-chatbuf-attach
    "Zf" 'telega-chatbuf-attach-media
    "Zv" 'telega-chatbuf-attach-clipboard

    "s" 'telega-chatbuf-filter
    "_" 'telega-chatbuf-filter-cancel
    "S" 'telega-chatbuf-filter-search

    (kbd "<tab>") 'telega-chatbuf-complete-or-next-link
    (kbd "<backtab>") 'telega-chatbuf-prev-link

    (kbd "RET") 'telega-chatbuf-newline-or-input-send

    "q" 'bury-buffer)

  (evil-collection-define-key 'normal 'telega-image-mode-map
    "ga" telega-prefix-map

    "}" 'telega-image-next
    "{" 'telega-image-prev
    "q" 'telega-image-quit)

  (evil-collection-define-key 'normal 'telega-webpage-mode-map
    "ga" telega-prefix-map

    "gx" 'telega-webpage-browse-url
    "yy" 'telega-webpage-copy-url
    "(" 'telega-webpage-history-prev
    ")" 'telega-webpage-history-next
    (kbd "<tab>") 'telega-button-forward
    (kbd "<backtab>") 'telega-button-backward)

  (evil-collection-set-readonly-bindings 'telega-user-button-map)
  ; We have to set keybinds for emacs instead of normal state because normal
  ; states takes no effects on buttons and remove the default bindings.
  ; ref: emacs-evil/evil#1477
  (evil-collection-define-key nil 'telega-user-button-map
    "B" nil
    "K" nil
    (kbd "DEL") nil

    "g?" 'telega-describe-user
    "m" 'telega-user-chat-with
    "D" 'telega-user-block)

  (evil-collection-set-readonly-bindings 'telega-msg-button-map)
  (evil-collection-define-key nil 'telega-msg-button-map
    "c" nil
    "d" nil
    "e" nil
    "f" nil
    "k" nil
    "l" nil
    "m" nil
    "n" nil
    "p" nil
    "^" nil
    (kbd "DEL") nil
    "*" nil

    "D" 'telega-msg-delete-marked-or-at-point
    "dd" 'telega-msg-delete-marked-or-at-point
    "i" 'telega-msg-edit
    "g?" 'telega-describe-message
    "a" 'telega-msg-mark-toggle
    (kbd "<tab>") 'telega-button-forward
    (kbd "<backtab>") 'telega-button-backward
    "R" 'telega-msg-forward-marked-or-at-point
    "r" 'telega-msg-reply
    "gr" 'telega-msg-open-thread

    "Zy" 'telega-msg-copy-text
    "Zl" 'telega-msg-copy-link
    "ds" 'telega-msg-ban-sender
    "ZL" 'telega-msg-redisplay
    "P" 'telega-msg-pin-toggle
    "ZR" 'telega-msg-resend
    "S" 'telega-msg-save
    "u" 'telega-msg-unmark
    "U" 'telega-chatbuf-msg-marks-toggle
    "=" 'telega-msg-diff-edits
    "s" 'telega-msg-favorite-toggle)

  (evil-collection-set-readonly-bindings 'telega-chat-button-map)
  (evil-collection-define-key nil 'telega-chat-button-map
    "i" nil
    "d" nil
    "^" nil
    (kbd "DEL") nil

    "g?" 'telega-describe-chat
    "a" 'telega-chat-add-member
    "o" 'telega-chat-set-custom-order
    "r" 'telega-chat-toggle-read
    "P" 'telega-chat-toggle-pin
    "C" 'telega-chat-call
    "D" 'telega-chat-delete)

  (evil-collection-set-readonly-bindings 'telega-sticker-button-map)
  (evil-collection-define-key nil 'telega-sticker-button-map
    "f" nil
    "*" nil
    "i" nil
    "h" nil

    "t" 'telega-sticker-toggle-favorite
    "g?" 'telega-sticker-help))

(provide 'evil-collection-telega)
;;; evil-collection-telega.el ends here
