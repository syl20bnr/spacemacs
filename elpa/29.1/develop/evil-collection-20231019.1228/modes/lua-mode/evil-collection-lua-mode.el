;;; evil-collection-lua-mode.el --- Bindings for `lua-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for `lua-mode'.

;;; Code:
(require 'evil-collection)
(require 'lua-mode nil t)

(defvar lua-indent-level)
(defconst evil-collection-lua-mode-maps '(lua-mode-map))

(defun evil-collection-lua-mode-set-evil-shift-width ()
  "Set `evil-shift-width' according to `lua-indent-level'."
  (setq-local evil-shift-width lua-indent-level))

;;;###autoload
(defun evil-collection-lua-mode-setup ()
  "Set up `evil' bindings for `lua-mode'."
  (add-hook 'lua-mode-hook #'evil-collection-lua-mode-set-evil-shift-width)

  (evil-collection-define-key 'normal 'lua-mode-map
    "K" 'lua-search-documentation
    "gz" 'run-lua))

(provide 'evil-collection-lua-mode)
;;; evil-collection-lua-mode.el ends here
