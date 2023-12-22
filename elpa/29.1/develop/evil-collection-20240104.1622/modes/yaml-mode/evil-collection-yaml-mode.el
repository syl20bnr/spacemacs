;;; evil-collection-yaml-mode.el --- Bindings for `yaml-mode' -*- lexical-binding: t -*-

;; Author: Frédéric Giquel
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, tools, yaml

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
;;; Bindings for `yaml-mode'.

;;; Code:
(require 'evil-collection)
(require 'yaml-mode nil t)

(defconst evil-collection-yaml-mode-maps nil)

(defvar yaml-indent-offset)
(defun evil-collection-yaml-mode-set-evil-shift-width ()
  "Set `evil-shift-width' according to `yaml-indent-offset'."
  (setq-local evil-shift-width yaml-indent-offset))

;;;###autoload
(defun evil-collection-yaml-mode-setup ()
  "Set up `evil' bindings for `yaml'."
  (add-hook 'yaml-mode-hook #'evil-collection-yaml-mode-set-evil-shift-width))

(provide 'evil-collection-yaml-mode)
;;; evil-collection-yaml-mode.el ends here
