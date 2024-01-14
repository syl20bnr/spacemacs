;;; evil-collection-js2-mode.el --- Bindings for `js2-mode' -*- lexical-binding: t -*-

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
;;; Bindings for `js2-mode'

;;; Code:
(require 'evil-collection)
(require 'js2-mode nil t)

(defvar js-indent-level)

(defun evil-collection-js2-set-evil-shift-width ()
  "Set `evil-shift-width' according to `js-indent-level'."
  (setq-local evil-shift-width js-indent-level))

;;;###autoload
(defun evil-collection-js2-mode-setup ()
  "Set up `evil' bindings for `js2-mode'."
  (add-hook 'js2-mode-hook #'evil-collection-js2-set-evil-shift-width)
  (add-hook 'js2-jsx-mode-hook #'evil-collection-js2-set-evil-shift-width))

(provide 'evil-collection-js2-mode)
;;; evil-collection-js2-mode.el ends here
