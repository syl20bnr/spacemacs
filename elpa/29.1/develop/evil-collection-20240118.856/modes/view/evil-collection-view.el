;;; evil-collection-view.el --- Evil bindings for view -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, bindings, tools

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
;; Evil bindings for View.

;;; Code:
(require 'evil-collection)
(require 'view)

(defconst evil-collection-view-maps '(view-mode-map))

;;;###autoload
(defun evil-collection-view-setup ()
  "Set up `evil' bindings for `view'."
  (add-hook 'view-mode-hook 'evil-normalize-keymaps)
  (evil-set-initial-state 'view-mode 'normal)
  (evil-collection-define-key 'normal 'view-mode-map
    "q" 'quit-window
    (kbd "SPC") 'View-scroll-page-forward
    (kbd "S-SPC") 'View-scroll-page-backward

    ;; zoom
    "+" 'text-scale-increase
    "=" 'text-scale-increase
    "0" 'text-scale-adjust              ; TODO: Conflicts with `evil-beginning-of-line'.
    "-" 'text-scale-decrease

    ;; refresh
    (kbd "gr") 'revert-buffer))

(provide 'evil-collection-view)
;;; evil-collection-view.el ends here
