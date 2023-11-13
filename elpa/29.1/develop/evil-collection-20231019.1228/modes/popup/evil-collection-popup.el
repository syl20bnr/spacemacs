;;; evil-collection-popup.el --- Bindings for `popup' -*- lexical-binding: t -*-

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
;; Bindings for `popup'.

;;; Code:
(require 'evil-collection)
(require 'popup nil t)

(defconst evil-collection-popup-maps '(popup-menu-keymap))

;;;###autoload
(defun evil-collection-popup-setup ()
  "Set up `evil' bindings for `popup'."
  (defvar popup-menu-keymap)
  (evil-collection-define-key nil 'popup-menu-keymap
    (kbd "C-j") 'popup-next
    (kbd "C-k") 'popup-previous))

(provide 'evil-collection-popup)
;;; evil-collection-popup.el ends here
