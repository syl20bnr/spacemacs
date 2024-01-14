;;; evil-collection-lsp-ui-imenu.el --- Bindings for `lsp-ui-imenu-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: Felix Dick <felix.dick@web.de>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, lsp-ui-imenu, convenience

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
;;; Bindings for `lsp-ui-imenu-mode'.

;;; Code:
(require 'lsp-ui-imenu nil t)
(require 'evil-collection)

(defconst evil-collection-lsp-ui-imenu-mode-maps '(lsp-ui-imenu-mode-map))

;;;###autoload
(defun evil-collection-lsp-ui-imenu-setup ()
  "Set up `evil' bindings for `lsp-ui-imenu'."
  (evil-set-initial-state 'lsp-ui-imenu-mode 'normal)
  (evil-collection-define-key 'normal 'lsp-ui-imenu-mode-map
    (kbd "C-k") 'lsp-ui-imenu--prev-kind
    (kbd "C-j") 'lsp-ui-imenu--next-kind
    (kbd "q") 'lsp-ui-imenu--kill
    (kbd "RET") 'lsp-ui-imenu--view
    (kbd "<M-return>") 'lsp-ui-imenu--visit))

(provide 'evil-collection-lsp-ui-imenu)
;;; evil-collection-lsp-ui-imenu.el ends here
