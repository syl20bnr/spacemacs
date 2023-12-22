;;; evil-collection-thread.el --- Evil bindings for `thread' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
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
;; Evil bindings for `thread'.

;;; Code:
(require 'evil-collection)
(require 'thread nil t)

(defconst evil-collection-thread-maps '(thread-list-mode-map))

;;;###autoload
(defun evil-collection-thread-setup ()
  "Set up `evil' bindings for `thread'."
  (evil-set-initial-state 'thread-list-mode 'normal)
  (evil-collection-define-key 'normal 'thread-list-mode-map
    "B" 'thread-list-pop-to-backtrace
    "Q" 'thread-list-send-quit-signal
    "E" 'thread-list-send-error-signal))

(provide 'evil-collection-thread)
;;; evil-collection-thread.el ends here
