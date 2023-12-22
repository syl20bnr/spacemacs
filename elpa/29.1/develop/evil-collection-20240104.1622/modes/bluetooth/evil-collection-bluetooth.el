;;; evil-collection-bluetooth.el --- Bindings for `bluetooth' -*- lexical-binding: t -*-

;; Copyright (C) 2022 Arian Dehghani

;; Author: Arian Dehghani <arianxdehghani@gmail.com>
;; Maintainer: Arian Dehghani <arianxdehghani@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, tools, blutooth

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `bluetooth'.

;;; Code:
(require 'evil-collection)
(require 'bluetooth nil t)

(defvar bluetooth-mode-map)

(defconst evil-collection-bluetooth-maps '(bluetooth-mode-map))

;;;###autoload
(defun evil-collection-bluetooth-setup ()
  "Set up `evil' bindings for `bluetooth'."
  (evil-collection-set-readonly-bindings 'bluetooth-mode-map)
  (evil-collection-define-key 'normal 'bluetooth-mode-map
    "c" 'bluetooth-connect
    "d" 'bluetooth-disconnect
    "b" 'bluetooth-toggle-blocked
    "t" 'bluetooth-toggle-trusted
    "a" 'bluetooth-set-alias
    "r" 'bluetooth-start-discovery
    "R" 'bluetooth-stop-discovery
    "s" 'bluetooth-toggle-powered
    "P" 'bluetooth-pair
    "D" 'bluetooth-toggle-discoverable
    "x" 'bluetooth-toggle-pairable
    "i" 'bluetooth-show-device-info
    "A" 'bluetooth-show-adapter-info
    "K" 'bluetooth-remove-device
    "<" 'bluetooth-beginning-of-list
    ">" 'bluetooth-end-of-list))

(provide 'evil-collection-bluetooth)
;;; evil-collection-bluetooth.el ends here
