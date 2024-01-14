;;; evil-collection-disk-usage.el --- Evil bindings for disk-usage.el -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, files, convenience, tools

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
;; Evil bindings for `disk-usage-mode'.

;;; Code:
(require 'evil-collection)
(require 'disk-usage nil t)

(defconst evil-collection-disk-usage-maps '(disk-usage-mode-map
                                            disk-usage-by-types-mode-map))

;;;###autoload
(defun evil-collection-disk-usage-setup ()
  "Set up `evil' bindings for `disk-usage'."
  (evil-collection-set-readonly-bindings 'disk-usage-mode-map)
  (evil-collection-define-key 'normal 'disk-usage-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    "S" 'tabulated-list-sort
    "^" 'disk-usage-up

    "zh" 'disk-usage-toggle-human-readable
    "zf" 'disk-usage-toggle-full-path
    "zr" 'disk-usage-toggle-recursive

    (kbd "ze") 'disk-usage-eshell-at-point
    (kbd "zs") 'disk-usage-shell-at-point
    (kbd "d") 'disk-usage-dired-at-point

    "m" 'disk-usage-mark
    "u" 'disk-usage-unmark
    "x" 'disk-usage-delete-marked-files

    "a" 'disk-usage-add-filters
    "A" 'disk-usage-remove-filters

    "g?" 'describe-mode
    "gr" 'revert-buffer)

  (evil-collection-set-readonly-bindings 'disk-usage-by-types-mode-map)
  (evil-collection-define-key 'normal 'disk-usage-by-types-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    (kbd "RET") 'disk-usage-files

    "S" 'tabulated-list-sort

    "zh" 'disk-usage-toggle-human-readable

    "g?" 'describe-mode
    "gr" 'revert-buffer

    "a" 'disk-usage-add-filters
    "A" 'disk-usage-remove-filters))

(provide 'evil-collection-disk-usage)
;;; evil-collection-disk-usage.el ends here
