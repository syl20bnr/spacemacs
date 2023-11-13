;;; evil-collection-imenu-list.el --- Bindings for `imenu-list' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
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
;; Bindings for `imenu-list'.

;;; Code:
(require 'evil-collection)
(require 'imenu-list nil t)

(defconst evil-collection-imenu-list-maps '(imenu-list-major-mode-map))

;;;###autoload
(defun evil-collection-imenu-list-setup ()
  "Set up `evil' bindings for `imenu-list'."
  (evil-collection-define-key 'normal 'imenu-list-major-mode-map
    (kbd "RET") 'imenu-list-goto-entry
    (kbd "TAB") 'hs-toggle-hiding
    "d" 'imenu-list-display-entry
    "gr" 'imenu-list-refresh
    "q" 'imenu-list-quit-window))


(provide 'evil-collection-imenu-list)
;;; evil-collection-imenu-list.el ends here
