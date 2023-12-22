;;; evil-collection-finder.el --- Evil bindings for finder -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, finder, tools

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
;; Evil bindings for finder.

;;; Code:
(require 'evil-collection)
(require 'finder)

(defconst evil-collection-finder-maps '(finder-mode-map))

;;;###autoload
(defun evil-collection-finder-setup ()
  "Set up `evil' bindings for `finder'."
  (evil-set-initial-state 'finder-mode 'normal)

  (evil-collection-define-key 'normal 'finder-mode-map
    (kbd "RET") 'finder-select
    (kbd "SPC") 'finder-select

    "d" 'finder-list-keywords
    "s" 'finder-summary
    "q" 'finder-exit))

(provide 'evil-collection-finder)
;;; evil-collection-finder.el ends here
