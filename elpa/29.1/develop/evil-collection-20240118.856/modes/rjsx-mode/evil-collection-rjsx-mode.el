;;; evil-collection-rjsx-mode.el --- Bindings for `rjsx-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, rjsx, tools

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
;;; Bindings for `rjsx-mode'.

;;; Code:
(require 'evil-collection)
(require 'rjsx-mode nil t)

(defvar rjsx-mode-map)

(defconst evil-collection-rjsx-maps '(rjsx-mode-map))

;;;###autoload
(defun evil-collection-rjsx-mode-setup ()
  "Set up `evil' bindings for `rjsx-mode'."
  (when evil-want-C-d-scroll
    (evil-collection-define-key 'insert 'rjsx-mode-map
      (kbd "C-d") 'rjsx-delete-creates-full-tag)
    (evil-collection-define-key 'normal 'rjsx-mode-map
      (kbd "C-d") 'evil-scroll-down)))

(provide 'evil-collection-rjsx-mode)
;;; evil-collection-rjsx-mode.el ends here
