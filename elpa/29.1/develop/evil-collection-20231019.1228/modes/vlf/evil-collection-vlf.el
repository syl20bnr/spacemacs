;;; evil-collection-vlf.el --- Evil bindings for vlf -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, vlf, tools

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
;; Evil bindings for `vlf'.

;;; Code:
(require 'evil-collection)
(require 'vlf nil t)

(defvar vlf-mode-map)
(declare-function vlf-change-batch-size "vlf")

(defconst evil-collection-vlf-maps '(vlf-mode-map))

(defun evil-collection-vlf-decrease-batch-size ()
  "Decrease vlf batch size by factor of 2."
  (interactive)
  (vlf-change-batch-size t))

;;; Code:
;;;###autoload
(defun evil-collection-vlf-setup ()
  "Set up `evil' bindings for `vlf'."
  (evil-set-initial-state 'vlf-mode 'normal)

  (evil-collection-define-key 'normal 'vlf-mode-map
    "gj" 'vlf-next-batch
    "gk" 'vlf-prev-batch
    (kbd "C-j") 'vlf-next-batch
    (kbd "C-k") 'vlf-prev-batch
    "]]" 'vlf-next-batch
    "[[" 'vlf-prev-batch

    "+" 'vlf-change-batch-size
    "-" 'evil-collection-vlf-decrease-batch-size
    "=" 'vlf-next-batch-from-point

    ;; refresh
    "gr" 'vlf-revert

    "s" 'vlf-re-search-forward
    "S" 'vlf-re-search-backward

    "gg" 'vlf-beginning-of-file
    "G" 'vlf-end-of-file
    "J" 'vlf-jump-to-chunk
    "E" 'vlf-ediff-buffers

    "g%" 'vlf-query-replace
    "go" 'vlf-occur
    "L" 'vlf-goto-line
    "F" 'vlf-toggle-follow))

(provide 'evil-collection-vlf)
;;; evil-collection-vlf.el ends here
