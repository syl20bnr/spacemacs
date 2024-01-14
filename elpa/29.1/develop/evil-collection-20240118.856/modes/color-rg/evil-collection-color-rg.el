;;; evil-collection-color-rg.el --- Bindings for color-rg -*- lexical-binding: t -*-

;; Copyright (C) 2023 Mumulhl

;; Author: Mumulhl <mumu-lhl@tuta.io>
;; Maintainer: Mumulhl <mumu-lhl@tuta.io>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
;;; Bindings for color-rg.

;;; Code:
(require 'evil-collection)
(require 'color-rg nil t)

(defvar color-rg-mode-map)
(defconst evil-collection-color-rg-maps '(color-rg-mode-map))

;;;###autoload
(defun evil-collection-color-rg-setup ()
  "Set up `evil' bindings for color-rg."
  (evil-collection-define-key 'normal 'color-rg-mode-map
    (kbd "<backtab>") 'color-rg-jump-prev-keyword
    (kbd "<tab>") 'color-rg-jump-next-keyword
    "C" 'color-rg-rerun-toggle-case
    (kbd "C-a") 'color-rg-beginning-of-line
    "D" 'color-rg-remove-line-from-results
    "E" 'color-rg-rerun-change-exclude-files
    "F" 'color-rg-filter-mismatch-results
    "G" 'color-rg-rerun-change-globs
    "I" 'color-rg-rerun-toggle-ignore
    "L" 'color-rg-rerun-literal
    "N" 'color-rg-rerun-toggle-node
    "O" 'color-rg-rerun-in-project
    "R" 'color-rg-rerun-regexp
    (kbd "RET") 'color-rg-open-file-and-stay
    "S" 'color-rg-customized-search
    (kbd "SPC") 'color-rg-open-file
    "X" 'color-rg-filter-mismatch-files
    "e" 'color-rg-switch-to-edit-mode
    "f" 'color-rg-filter-match-results
    "h" 'color-rg-jump-next-file
    "i" 'color-rg-insert-current-line
    "j" 'color-rg-jump-next-keyword
    "k" 'color-rg-jump-prev-keyword
    "l" 'color-rg-jump-prev-file
    "o" 'color-rg-rerun-in-parent-dir
    "q" 'color-rg-quit
    "r" 'color-rg-replace-all-matches
    "s" 'color-rg-rerun-change-dir
    "u" 'color-rg-unfilter
    "x" 'color-rg-filter-match-files))

(provide 'evil-collection-color-rg)
;;; evil-collection-color-rg.el ends here
