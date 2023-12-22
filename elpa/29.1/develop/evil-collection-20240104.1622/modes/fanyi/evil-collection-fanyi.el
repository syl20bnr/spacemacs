;;; evil-collection-fanyi.el --- Evil bindings for fanyi -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

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
;; Evil bindings for fanyi.

;;; Code:
(require 'evil-collection)
(require 'fanyi nil t)

(defconst evil-collection-fanyi-maps '(fanyi-mode-map))

;;;###autoload
(defun evil-collection-fanyi-setup ()
  "Set up `evil' bindings for `fanyi'."
  (evil-collection-set-readonly-bindings 'fanyi-mode-map)
  (evil-collection-define-key 'normal 'fanyi-mode-map
    ;; tab
    [tab]     'forward-button
    [backtab] 'backward-button

    "s" 'fanyi-dwim))

(provide 'evil-collection-fanyi)
;;; evil-collection-fanyi.el ends here
