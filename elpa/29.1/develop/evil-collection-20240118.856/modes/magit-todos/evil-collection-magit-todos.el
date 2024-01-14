;;; evil-collection-magit-todos.el --- Bindings for `magit-todos' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
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
;; Bindings for `magit-todos'.

;;; Code:
(require 'evil-collection)
(require 'magit-todos nil t)

(defconst evil-collection-magit-todos-maps '(magit-todos-section-map
                                             magit-todos-item-section-map
                                             magit-status-mode-map))

(defun evil-collection-magit-todos-setup-jump-key ()
  "Add keybinding to jump to todos section."
  (evil-collection-define-key 'normal 'magit-status-mode-map
    "gT" (and (bound-and-true-p magit-todos-mode) 'magit-todos-jump-to-todos)))

(defun evil-collection-magit-todos-supress-warning (func &rest args)
  "Supress the jT keybinding warning."
  (cl-letf* (((symbol-function 'message) #'ignore))
    (apply func args)))

;;;###autoload
(defun evil-collection-magit-todos-setup ()
  "Set up `evil' bindings for `magit-todos'."
  ;; magit-todos binds jT which prevents evil users from stepping into the section
  (evil-collection-define-key nil 'magit-todos-section-map
    "j" nil)

  (evil-collection-define-key nil 'magit-todos-item-section-map
    "j" nil)

  ;; No need to tell me that jT isn't bound
  (advice-add 'magit-todos-mode :around 'evil-collection-magit-todos-supress-warning)

  (add-hook 'magit-todos-mode-hook 'evil-collection-magit-todos-setup-jump-key))


(provide 'evil-collection-magit-todos)
;;; evil-collection-magit-todos.el ends here
