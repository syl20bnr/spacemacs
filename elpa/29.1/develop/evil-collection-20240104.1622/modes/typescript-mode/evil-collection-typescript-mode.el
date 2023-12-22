;;; evil-collection-typescript-mode.el --- Bindings for `typescript-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, evil, tools, typescript

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
;;; Bindings for `typescript-mode'.

;;; Code:
(require 'evil-collection)
(require 'typescript-mode nil t)

(defvar typescript-indent-level)

(defun evil-collection-typescript-mode-set-evil-shift-width ()
  "Set `evil-shift-width' according to `typescript-indent-level'."
  (setq-local evil-shift-width typescript-indent-level))

;;;###autoload
(defun evil-collection-typescript-mode-setup ()
  "Set up `evil' bindings for `typescript-mode'."
  (add-hook 'typescript-mode-hook
            #'evil-collection-typescript-mode-set-evil-shift-width))

(provide 'evil-collection-typescript-mode)
;;; evil-collection-typescript-mode.el ends here
