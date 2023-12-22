;;; evil-collection-free-keys.el --- Bindings for `free-keys' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for `free-keys'.
(require 'evil-collection)
(require 'free-keys nil t)

;;; Code:
(defvar free-keys-mode-map)

(defconst evil-collection-free-keys-maps '(free-keys-mode-map))

(defun evil-collection-free-keys-set-header-line-format ()
  "Tweak `header-line-format' locally for `free-keys'."
  (setq-local header-line-format
              "Help: (c) change buffer (p) change prefix (q) quit"))

;;;###autoload
(defun evil-collection-free-keys-setup ()
  "Set up `evil' bindings for `free-keys'."
  (add-hook 'free-keys-mode-hook
            #'evil-collection-free-keys-set-header-line-format)
  (evil-set-initial-state 'free-keys-mode 'normal)
  (evil-collection-define-key 'normal 'free-keys-mode-map
    "c" 'free-keys-change-buffer
    "p" 'free-keys-set-prefix
    "q" 'quit-window))

(provide 'evil-collection-free-keys)
;;; evil-collection-free-keys.el ends here
