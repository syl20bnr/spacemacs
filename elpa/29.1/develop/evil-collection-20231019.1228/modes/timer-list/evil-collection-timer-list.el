;;; evil-collection-timer-list.el --- Evil bindings for `timer-list' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

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
;; Evil bindings for `timer-list'.

;;; Code:
(require 'evil-collection)
(require 'timer-list)

(defconst evil-collection-timer-list-maps '(timer-list-mode-map))

;;;###autoload
(defun evil-collection-timer-list-setup ()
  "Set up `evil' bindings for `timer-list'."
  (evil-set-initial-state 'timer-list-mode 'normal)
  (evil-collection-define-key 'normal 'timer-list-mode-map
    "C" 'timer-list-cancel))

(provide 'evil-collection-timer-list)
;;; evil-collection-timer-list.el ends here
