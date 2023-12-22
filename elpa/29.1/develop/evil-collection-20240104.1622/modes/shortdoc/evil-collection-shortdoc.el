;;; evil-collection-shortdoc.el --- Evil bindings for shortdoc -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; TODO: Update to emacs 28.1 when it's out
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, lisp, help

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
;; Evil bindings for shortdoc.

;;; Code:
(require 'evil-collection)
(require 'shortdoc nil t)

(defconst evil-collection-shortdoc-maps '(shortdoc-mode-map))

;;;###autoload
(defun evil-collection-shortdoc-setup ()
  "Set up `evil' bindings for `shortdoc'."
  (evil-set-initial-state 'shortdoc-mode 'normal)
  (evil-collection-define-key 'normal 'shortdoc-mode-map
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button
    (kbd "C-k") 'shortdoc-previous
    (kbd "C-j") 'shortdoc-next
    "[[" 'shortdoc-previous-section
    "]]" 'shortdoc-next-section))

(provide 'evil-collection-shortdoc)
;;; evil-collection-shortdoc.el ends here
