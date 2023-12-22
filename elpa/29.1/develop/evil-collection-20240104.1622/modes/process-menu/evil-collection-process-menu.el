;;; evil-collection-process-menu.el --- Evil bindings for process-menu-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, process, tools

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
;; Evil bindings for process-menu-mode (M-x list-processes).

;;; Code:
(require 'evil-collection)

(defconst evil-collection-process-menu-maps '(process-menu-mode-map))

;;;###autoload
(defun evil-collection-process-menu-setup ()
  "Set up `evil' bindings for `list-processes'."
  (evil-collection-set-readonly-bindings 'process-menu-mode-map)
  (evil-collection-define-key 'normal 'process-menu-mode-map
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    "S" 'tabulated-list-sort
    "D" 'process-menu-delete-process

    ;; motion
    ;; TODO: Implement beginning-of-buffer / end-of-buffer.
    (kbd "SPC") 'evil-scroll-down
    (kbd "S-SPC") 'evil-scroll-up

    ;; refresh
    "gr" 'revert-buffer))

(provide 'evil-collection-process-menu)
;;; evil-collection-process-menu.el ends here
