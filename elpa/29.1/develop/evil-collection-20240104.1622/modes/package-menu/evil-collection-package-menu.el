;;; evil-collection-package-menu.el --- Evil bindings for package-menu -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, package-menu, tools

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
;; Evil integration for `package-menu-mode'.

;;; Code:
(require 'evil-collection)
(require 'package)

(defconst evil-collection-package-menu-maps '(package-menu-mode-map))

;;;###autoload
(defun evil-collection-package-menu-setup ()
  "Set up `evil' bindings for `package-menu'."
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-collection-set-readonly-bindings 'package-menu-mode-map)
  (evil-collection-define-key 'normal 'package-menu-mode-map
    "i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "d" 'package-menu-mark-delete
    "gr" 'revert-buffer

    ;; undo
    "u" 'package-menu-mark-unmark

    ;; execute
    "x" 'package-menu-execute

    "g?" 'package-menu-describe-package)

  ;; It's introduced since Emacs 28.
  (when (fboundp 'package-browse-url)
    (evil-collection-define-key 'normal 'package-menu-mode-map
      "B" 'package-browse-url)))

(provide 'evil-collection-package-menu)
;;; evil-collection-package-menu.el ends here
