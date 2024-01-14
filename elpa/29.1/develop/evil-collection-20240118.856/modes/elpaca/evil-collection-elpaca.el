;;; evil-collection-elpaca.el --- Bindings for `elpaca' -*- lexical-binding: t -*-

;; Copyright (C) 2023 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for `elpaca'.

;;; Code:
(require 'evil-collection)
(require 'elpaca nil t)
(require 'elpaca-ui nil t)

(defvar elpaca-info-mode-map)
(defvar elpaca-ui-mode-map)
(declare-function elpaca-ui-search "elpaca-ui")
(declare-function elpaca-ui-visit "elpaca-ui")


(defconst evil-collection-elpaca-maps '(elpaca-info-mode-map
                                        elpaca-ui-mode-map))

(defcustom evil-collection-elpaca-want-movement t
  "When non nil, prioritize movement keys like \"h\", \"l\", \"b\", etc."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-elpaca-want-v t
  "When non nil, use v for visual mode.
When this is true, move the `elpaca-visit' to gv and gd."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-elpaca-want-u-unmark t
  "When non nil, use u to unmark.

If this is t, flip the u/U bindings.
If this is nil, match original `elpaca' behavior."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-elpaca-want-g-filters t
  "When non nil, put `elpaca' filters on g prefix."
  :group 'evil-collection
  :type 'boolean)

(defun evil-collection-elpaca-ui-visit-build-dir ()
  "Visit package's build-dir."
  (interactive)
  (elpaca-ui-visit 'build))

(defmacro evil-collection-elpaca-defsearch (name query)
  "Return search command with NAME for QUERY."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-search-%s" name)) ()
     ,(format "Search for %S" query)
     (interactive)
     (elpaca-ui-search ,query)))

(defun evil-collection-elpaca-setup ()
  "Set up `evil' bindings for elpaca."
  (evil-collection-define-key 'normal 'elpaca-info-mode-map
    (kbd "TAB") 'forward-button
    (kbd "<tab>") 'forward-button
    (kbd "S-TAB") 'backward-button
    (kbd "<backtab>") 'backward-button
    (kbd "i") 'elpaca-info
    (kbd "s") 'elpaca-info)

  (evil-collection-define-key 'normal 'elpaca-ui-mode-map
    (kbd "RET") 'elpaca-ui-info
    (kbd "!") 'elpaca-ui-send-input
    (kbd "R") 'elpaca-ui-search-refresh
    (kbd "gr") 'elpaca-ui-search-refresh
    (kbd "c") 'elpaca-ui-copy
    (kbd "d") 'elpaca-ui-mark-delete
    (kbd "i") 'elpaca-ui-mark-try
    (kbd "m") 'elpaca-manager
    (kbd "r") 'elpaca-ui-mark-rebuild
    (kbd "s") 'elpaca-ui-search
    (kbd "x") 'elpaca-ui-execute-marks
    (kbd "+") 'elpaca-ui-show-hidden-rows
    (kbd "=") 'elpaca-ui-show-hidden-rows
    (kbd "gb") 'evil-collection-elpaca-ui-visit-build-dir)

  (if evil-collection-elpaca-want-u-unmark
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "u") 'elpaca-ui-unmark
        (kbd "U") 'elpaca-ui-mark-update)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "U") 'elpaca-ui-unmark
      (kbd "u") 'elpaca-ui-mark-update))

  (if evil-collection-elpaca-want-v
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "gv") 'elpaca-ui-visit
        (kbd "gd") 'elpaca-ui-visit)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "v") 'elpaca-ui-visit))

  (if evil-collection-elpaca-want-g-filters
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "gI")
        (evil-collection-elpaca-defsearch installed "#unique #installed")
        (kbd "gM")
        (evil-collection-elpaca-defsearch marked   "#unique #marked")
        (kbd "gO")
        (evil-collection-elpaca-defsearch orphaned "#unique #orphan")
        (kbd "gT")
        (evil-collection-elpaca-defsearch tried "#unique #installed !#declared"))
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "I")
      (evil-collection-elpaca-defsearch installed "#unique #installed")
      (kbd "M")
      (evil-collection-elpaca-defsearch marked   "#unique #marked")
      (kbd "O")
      (evil-collection-elpaca-defsearch orphaned "#unique #orphan")
      (kbd "T")
      (evil-collection-elpaca-defsearch tried "#unique #installed !#declared")))

  (if evil-collection-elpaca-want-movement
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "B") 'elpaca-ui-browse-package ;; b -> B
        (kbd "F") 'elpaca-ui-mark-fetch ;; f -> F
        (kbd "L") 'elpaca-log ;; l -> L
        (kbd "gs") 'elpaca-status ;; t -> gs + gS
        (kbd "gS") 'elpaca-status)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "b") 'elpaca-ui-browse-package
      (kbd "f") 'elpaca-ui-mark-fetch
      (kbd "l") 'elpaca-log
      (kbd "t") 'elpaca-status)))

(provide 'evil-collection-elpaca)
;;; evil-collection-elpaca.el ends here
