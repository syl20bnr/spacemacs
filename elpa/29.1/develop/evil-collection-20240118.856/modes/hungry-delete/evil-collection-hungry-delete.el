;;; evil-collection-hungry-delete.el --- Bindings for `hungry-delete' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, hungry-delete, tools

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
;;; Bindings for `hungry-delete'.

;;; Code:
(require 'evil-collection)
(require 'hungry-delete nil t)

(defconst evil-collection-hungry-delete-maps '(hungry-delete-mode-map))

(defun evil-collection-hungry-delete (f &rest args)
  "Wrapper function to run `hungry-delete-backward' if
`hungry-delete-mode' is on."
  (if (and (bound-and-true-p hungry-delete-mode)
           (fboundp 'hungry-delete-backward))
      (hungry-delete-backward 1)
    (apply f args)))

(defun evil-collection-hungry-delete-for-join (f &rest args)
  "Wrapper function to run `hungry-delete-backward' if
`hungry-delete-mode' is on."
  (if (and (bound-and-true-p hungry-delete-mode)
           (fboundp 'hungry-delete-backward))
      (hungry-delete-backward 1)
    (funcall f args)))

;;;###autoload
(defun evil-collection-hungry-delete-setup ()
  "Set up `evil' bindings for `hungry-delete'."
  (advice-add 'evil-delete-backward-char
              :around #'evil-collection-hungry-delete)
  (advice-add 'evil-delete-backward-char-and-join
              :around #'evil-collection-hungry-delete-for-join))

(provide 'evil-collection-hungry-delete)
;;; evil-collection-hungry-delete.el ends here
