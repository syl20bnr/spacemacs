;;; evil-collection-explain-pause-mode.el --- Evil bindings for explain-pause-mode -*- lexical-binding: t -*-

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
;; Evil bindings for explain-pause-mode.

;;; Code:
(require 'evil-collection)
(require 'explain-pause-mode nil t)

(defconst evil-collection-explain-pause-mode-maps '(explain-pause-top-mode-map))

;;;###autoload
(defun evil-collection-explain-pause-mode-setup ()
  "Set up `evil' bindings for `explain-pause-mode'."
  (evil-set-initial-state 'explain-pause-top-mode 'normal)
  (evil-collection-define-key 'normal 'explain-pause-top-mode-map
    "a" 'explain-pause-top-auto-refresh
    "c" 'explain-pause-top-clear
    "o" 'explain-pause-top-sort

    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window))

(provide 'evil-collection-explain-pause-mode)

;;; evil-collection-explain-pause-mode.el ends here
