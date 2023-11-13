;;; evil-collection-smerge-mode.el --- Bindings for `smerge-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2021 pspiagicw

;; Author: pspiagicw <pspiagicw@gmail.com>
;; Maintainer: pspiagicw  <pspiagicw@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, python, tools, emacs

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
;;; Bindings for `smerge-mode'.
;;; Code:
(require 'evil-collection)
(require 'smerge-mode)
;; 

(defconst evil-collection-smerge-mode-maps '(smerge-mode-map))

;;;###autoload
(defun evil-collection-smerge-mode-setup ()
  "Setup up `evil' bindings for `smerge-mode'."
  (evil-collection-define-key 'normal 'smerge-mode-map
    "gj" 'smerge-next
    "]]" 'smerge-next
    "gk" 'smerge-prev
    "[[" 'smerge-prev
    "ga" 'smerge-keep-all
    "gu" 'smerge-keep-upper
    "gl" 'smerge-keep-lower
    "go" 'smerge-keep-other
    "gm" 'smerge-keep-mine
    "gc" 'smerge-combine-with-next
    "gr" 'smerge-refine
    "gb" 'smerge-keep-base))
(provide 'evil-collection-smerge-mode)
;;; evil-collection-smerge-mode.el ends here
