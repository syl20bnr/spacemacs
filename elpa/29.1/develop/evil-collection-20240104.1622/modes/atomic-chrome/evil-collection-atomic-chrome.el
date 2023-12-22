;;; evil-collection-atomic-chrome.el --- Evil bindings for atomic-chrome -*- lexical-binding: t -*-

;; Copyright (C) 2022 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Evil bindings for atomic-chrome.

;;; Code:
(require 'evil-collection)
(require 'atomic-chrome nil t)

(defconst evil-collection-atomic-chrome-maps nil)

;;;###autoload
(defun evil-collection-atomic-chrome-setup ()
  "Set up `evil' bindings for `atomic-chrome'."
  ;; The browser is in "inserted" state, makes it consistent.
  (evil-set-initial-state 'atomic-chrome-edit-mode 'insert))

(provide 'evil-collection-atomic-chrome)
;;; evil-collection-atomic-chrome.el ends here
