;;; evil-collection-flycheck.el --- Evil bindings for flycheck -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, flycheck, tools

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
;; Evil bindings for `flycheck-mode'.

;;; Code:
(require 'evil-collection)
(require 'flycheck nil t)

(defvar flycheck-error-list-mode-map)

(defconst evil-collection-flycheck-maps '(flycheck-error-list-mode-map))

;;;###autoload
(defun evil-collection-flycheck-setup ()
  "Set up `evil' bindings for `flycheck'."
  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-collection-define-key 'normal 'flycheck-error-list-mode-map
    "gj" 'flycheck-error-list-next-error
    "gk" 'flycheck-error-list-previous-error
    (kbd "C-j") 'flycheck-error-list-next-error
    (kbd "C-k") 'flycheck-error-list-previous-error
    "]]" 'flycheck-error-list-next-error
    "[[" 'flycheck-error-list-previous-error
    "gr" 'flycheck-error-list-check-source
    "s" 'flycheck-error-list-set-filter
    "S" 'flycheck-error-list-reset-filter
    "x" 'flycheck-error-list-explain-error
    (kbd "RET") 'flycheck-error-list-goto-error
    "q" 'quit-window))

(provide 'evil-collection-flycheck)
;;; evil-collection-flycheck.el ends here
