;;; evil-collection-osx-dictionary.el --- Evil bindings for osx-dictionary -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, osx-dictionary, tools

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
;; Evil bindings for osx-dictionary.

;;; Code:
(require 'evil-collection)
(require 'osx-dictionary nil t)

(defconst evil-collection-osx-dictionary-maps '(osx-dictionary-mode-map))

;;;###autoload
(defun evil-collection-osx-dictionary-setup ()
  "Set up `evil' bindings for `osx-dictionary'."
  (evil-set-initial-state 'osx-dictionary-mode 'normal)

  (evil-collection-define-key 'normal 'osx-dictionary-mode-map
    "q" 'osx-dictionary-quit
    "s" 'osx-dictionary-search-input
    "o" 'osx-dictionary-open-dictionary.app
    "r" 'osx-dictionary-read-word))

(provide 'evil-collection-osx-dictionary)
;;; evil-collection-osx-dictionary.el ends here
