;;; evil-collection-tabulated-list.el --- Bindings for `tabulated-list' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Morgan Smith <Morgan.J.Smith@outlook.com>

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Maintainer: Morgan Smith <Morgan.J.Smith@outlook.com>
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
;; Bindings for `tabulated-list'

;;; Code:
(require 'evil-collection)
(require 'tabulated-list)

(defvar tabulated-list-mode-map)

(defconst evil-collection-tabulated-list-maps '(tabulated-list-mode-map))

;;;###autoload
(defun evil-collection-tabulated-list-setup ()
  "Set up `evil' bindings for `tabulated-list'."

  (evil-collection-define-key nil 'tabulated-list-mode-map
    "n" nil
    "p" nil)

  (evil-set-initial-state 'tabulated-list-mode 'normal)
  (evil-collection-define-key 'normal 'tabulated-list-mode-map
    "S" 'tabulated-list-sort
    "{" 'tabulated-list-narrow-current-column
    "}" 'tabulated-list-widen-current-column
    "gl" 'tabulated-list-next-column
    "gh" 'tabulated-list-previous-column
    "q" 'quit-window))

(provide 'evil-collection-tabulated-list)
;;; evil-collection-tabulated-list.el ends here
