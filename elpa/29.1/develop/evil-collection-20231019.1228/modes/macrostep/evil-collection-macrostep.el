;;; evil-collection-macrostep.el --- Evil Integration for Macrostep -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, macrostep, tools

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
;; Evil bindings for `macrostep-mode'.

;;; Code:
(require 'evil-collection)
(require 'macrostep nil t)

(defconst evil-collection-macrostep-maps '(macrostep-keymap))

;;;###autoload
(defun evil-collection-macrostep-setup ()
  "Set up `evil' bindings for `macrostep'."
  ;; Keymaps don't seem to be populated on first try.
  ;; Force `evil' to normalize keymaps.
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'macrostep-keymap
    "q" 'macrostep-collapse-all
    "e" 'macrostep-expand
    "u" 'macrostep-collapse
    "gj" 'macrostep-next-macro
    "gk" 'macrostep-prev-macro
    (kbd "C-j") 'macrostep-next-macro
    (kbd "C-k") 'macrostep-prev-macro))

(provide 'evil-collection-macrostep)
;;; evil-collection-macrostep.el ends here
