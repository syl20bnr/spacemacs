;;; evil-collection-bufler.el --- Evil bindings for mpdel -*- lexical-binding: t -*-

;; Copyright (C) 2023 Ruoyu Feng

;; Author: Ruoyu Feng <mail@vonfry.name>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, tools, bufler, buffer list

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for bufler.

;;; Code:

(require 'bufler nil t)
(require 'evil-collection)

(defvar bufler-list-mode-map)

(defconst evil-collection-bufler-maps '(bufler-list-mode-map))

(defconst evil-collection-bufler-modes '(bufler-list-mode))

;;;###autoload
(defun evil-collection-bufler-setup ()
  "Set up `evil' bindings for `bufler'."

  (evil-collection-set-readonly-bindings 'bufler-list-mode-map)
  (dolist (mode evil-collection-bufler-modes)
    (evil-set-initial-state mode 'normal))
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    "?"   'hydra:bufler/body
    "B"   'bufler
    "f"   'bufler-list-group-frame
    "F"   'bufler-list-group-make-frame
    "r"   'bufler-list-buffer-name-workspace
    "D"   'bufler-list-buffer-kill
    "w"   'bufler-list-buffer-save
    (kbd "RET") 'bufler-list-buffer-switch
    "J"   'bufler-list-buffer-peek))

(provide 'evil-collection-bufler)
;;; evil-collection-bufler.el ends here
