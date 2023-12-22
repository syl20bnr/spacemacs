;;; evil-collection-auto-package-update.el --- Evil bindings for auto-package-update -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, auto-package-update, tools

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
;; Evil bindings for auto-package-update.

;;; Code:
(require 'evil-collection)
(require 'auto-package-update nil t)

(defconst evil-collection-auto-package-update-maps '(auto-package-update-minor-mode-map))

;;;###autoload
(defun evil-collection-auto-package-update-setup ()
  "Set up `evil' bindings for `auto-package-update'."
  (add-hook 'auto-package-update-minor-mode-hook 'evil-normalize-keymaps)
  (evil-collection-define-key 'normal 'auto-package-update-minor-mode-map
    "q" 'quit-window))

(provide 'evil-collection-auto-package-update)

;;; evil-collection-auto-package-update.el ends here
