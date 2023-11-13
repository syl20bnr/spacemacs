;;; evil-collection-apropos.el --- Evil bindings for apropos-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, help, tools

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
;; Evil bindings for `apropos-mode'.

;;; Code:
(require 'evil-collection)
(require 'apropos)

(defconst evil-collection-apropos-maps '(apropos-mode-map))

;;;###autoload
(defun evil-collection-apropos-setup ()
  "Set up `evil' bindings for `apropos'."
  (evil-set-initial-state 'apropos-mode 'normal)
  (evil-collection-define-key 'normal 'apropos-mode-map
    (kbd "RET") 'apropos-follow
    (kbd "TAB") 'forward-button
    (kbd "S-TAB") 'backward-button
    "g?" 'describe-mode
    "gr" 'revert-buffer

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-collection-apropos)
;;; evil-collection-apropos.el ends here
