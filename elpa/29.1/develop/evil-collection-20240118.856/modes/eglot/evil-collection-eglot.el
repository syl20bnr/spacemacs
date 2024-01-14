;;; evil-collection-eglot.el --- Bindings for `eglot' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, eglot, tools

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
;;; Bindings for `eglot'.

;;; Code:
(require 'eglot nil t)
(require 'evil-collection)

(defconst evil-collection-eglot-maps '(eglot-mode-map))

;;;###autoload
(defun evil-collection-eglot-setup ()
  "Set up `evil' bindings for `eglot'."
  (add-hook 'eglot-managed-mode-hook 'evil-normalize-keymaps)
  (evil-collection-define-key 'normal 'eglot-mode-map
    "gd" 'xref-find-definitions
    "gD" 'xref-find-definitions-other-window
    "g5" 'xref-find-definitions-other-frame
    (kbd "C-t") 'xref-pop-marker-stack
    "K" 'eldoc-doc-buffer)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'eglot-mode-map
      "gr" 'xref-find-references)))

(provide 'evil-collection-eglot)
;;; evil-collection-eglot.el ends here
