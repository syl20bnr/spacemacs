;;; evil-collection-helpful.el --- Evil bindings for helpful-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, help, tools

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
;; Evil bindings for `helpful-mode'.

;;; Code:
(require 'evil-collection)
(require 'helpful nil t)

(defconst evil-collection-helpful-maps '(helpful-mode-map))

;;;###autoload
(defun evil-collection-helpful-setup ()
  "Set up `evil' bindings for `helpful'."
  (evil-collection-set-readonly-bindings 'helpful-mode-map)
  (evil-collection-define-key 'normal 'helpful-mode-map
    ;; motion
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button
    (kbd "RET") 'helpful-visit-reference

    ;; The following bindings don't do what they are supposed to. "go" should open
    ;; in the same window and "gO" should open in a different one.
    "go" 'push-button
    "gO" 'push-button

    "g?" 'describe-mode
    "gr" 'helpful-update))

(provide 'evil-collection-helpful)
;;; evil-collection-helpful.el ends here
