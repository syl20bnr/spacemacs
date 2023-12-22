;;; evil-collection-rtags.el --- Evil bindings for `rtags' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, rtags, tools

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
;;; Evil bindings for `rtags'.

;;; Code:
(require 'evil-collection)
(require 'rtags nil t)

(defvar rtags-mode-map)
(defvar rtags-dependency-tree-mode-map)
(defvar rtags-references-tree-mode-map)
(defvar rtags-location-stack-visualize-mode-map)

(defconst evil-collection-rtags-maps '(rtags-mode-map
                                       rtags-dependency-tree-mode-map
                                       rtags-references-tree-mode-map
                                       rtags-location-stack-visualize-mode-map))

;;;###autoload
(defun evil-collection-rtags-setup ()
  "Set up `evil' bindings for `rtags'."
  (evil-set-initial-state 'rtags-mode 'normal)
  (evil-set-initial-state 'rtags-dependency-tree-mode 'normal)
  (evil-set-initial-state 'rtags-references-tree-mode 'normal)
  (evil-set-initial-state 'rtags-location-stack-visualize-mode 'normal)

  (evil-collection-define-key 'normal 'rtags-mode-map
    ;; open
    (kbd "RET") 'rtags-select
    (kbd "S-<return>") 'rtags-select-other-window
    (kbd "M-<return>") 'rtags-show-in-other-window
    (kbd "go") 'rtags-select-other-window
    (kbd "gO") 'rtags-show-in-other-window
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window

    "c" 'rtags-select-caller
    "C" 'rtags-select-caller-other-window
    "x" 'rtags-select-and-remove-rtags-buffer
    "q" 'rtags-call-bury-or-delete)

  (evil-collection-define-key 'normal 'rtags-dependency-tree-mode-map
    (kbd "<tab>") 'rtags-dependency-tree-toggle-current-expanded
    "E" 'rtags-dependency-tree-expand-all
    "c" 'rtags-dependency-tree-collapse-all
    "-" 'rtags-dependency-tree-collapse-current
    "+" 'rtags-dependency-tree-expand-current
    "P" 'rtags-dependency-tree-find-path
    "gf" 'rtags-dependency-tree-find-path

    "gj" 'rtags-dependency-tree-next-level
    "gk" 'rtags-dependency-tree-previous-level

    (kbd "C-j") 'rtags-dependency-tree-next-level
    (kbd "C-k") 'rtags-dependency-tree-previous-level
    "]]" 'rtags-dependency-tree-next-level
    "[[" 'rtags-dependency-tree-previous-level

    ;; open
    (kbd "RET") 'rtags-select
    (kbd "S-<return>") 'rtags-select-other-window
    (kbd "M-<return>") 'rtags-show-in-other-window
    "go" 'rtags-select-other-window
    "gO" 'rtags-show-in-other-window
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer
    "q" 'rtags-call-bury-or-delete)

  (evil-collection-define-key 'normal 'rtags-references-tree-mode-map
    (kbd "<tab>") 'rtags-references-tree-toggle-current-expanded

    "E" 'rtags-references-tree-expand-all
    "c" 'rtags-references-tree-collapse-all
    "-" 'rtags-references-tree-collapse-current
    "+" 'rtags-references-tree-expand-current

    "gj" 'rtags-references-tree-next-level
    "gk" 'rtags-references-tree-previous-level

    (kbd "C-j") 'rtags-references-tree-next-level
    (kbd "C-k") 'rtags-references-tree-previous-level

    "]]" 'rtags-references-tree-next-level
    "[[" 'rtags-references-tree-previous-level

    ;; open
    (kbd "RET") 'rtags-select
    (kbd "S-<return>") 'rtags-select-other-window
    (kbd "M-<return>") 'rtags-show-in-other-window
    "go" 'rtags-select-other-window
    "gO" 'rtags-show-in-other-window
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer
    "q" 'rtags-call-bury-or-delete)

  (evil-collection-define-key 'normal 'rtags-location-stack-visualize-mode-map
    ;; open
    (kbd "RET") 'rtags-select
    (kbd "S-<return>") 'rtags-select-other-window
    (kbd "M-<return>") 'rtags-show-in-other-window
    (kbd "go") 'rtags-select-other-window
    (kbd "gO") 'rtags-show-in-other-window
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer
    "q" 'rtags-call-bury-or-delete))

(provide 'evil-collection-rtags)
;;; evil-collection-rtags.el ends here
