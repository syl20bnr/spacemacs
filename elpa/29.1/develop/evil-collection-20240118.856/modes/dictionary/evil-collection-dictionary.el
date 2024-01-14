;;; evil-collection-dictionary.el --- Evil bindings for dictionary-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, dictionary, tools

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
;; Evil bindings for `dictionary'.

;;; Code:
(require 'evil-collection)
(require 'dictionary nil t)

(defconst evil-collection-dictionary-maps '(dictionary-mode-map))

;;;###autoload
(defun evil-collection-dictionary-setup ()
  "Set up `evil' bindings for `dictionary'."
  (evil-set-initial-state 'dictionary-mode 'normal)
  (evil-collection-set-readonly-bindings 'dictionary-mode-map)
  (evil-collection-define-key 'normal 'dictionary-mode-map
    ;; button
    [tab] 'forward-button
    [backtab] 'backward-button
    ;; mouse
    [mouse-1] 'link-selected
    ;; misc
    "g?" 'dictionary-help ; normally under `h` which is rebound here
    (kbd "C-o") 'dictionary-previous)) ; normally under `l` which is rebound here

(provide 'evil-collection-dictionary)
;;; evil-collection-dictionary.el ends here
