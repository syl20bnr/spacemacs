;;; evil-collection-imenu.el --- Evil integration for `imenu' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Jacob First

;; Author: Jacob First <jacob.first@member.fsf.org>
;; Maintainer: James Nguyen <james@jojojames.com>
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
;; Evil conveniences for `imenu'.

;;; Code:
(require 'imenu)
(require 'evil-collection)

;;;###autoload
(defun evil-collection-imenu-setup ()
  "Set up Evil integration for `imenu'."
  (evil-declare-not-repeat 'imenu)
  (evil-add-command-properties 'imenu :jump t))

(provide 'evil-collection-imenu)
;;; evil-collection-imenu.el ends here
