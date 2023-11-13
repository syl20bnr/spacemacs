;;; evil-collection-embark.el --- Evil bindings for embark -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Evil bindings for embark.

;;; Code:

(require 'evil-collection)
(require 'embark nil t)

(defconst evil-collection-embark-maps '(embark-collect-mode-map))

;;;###autoload
(defun evil-collection-embark-setup ()
  "Set up `evil' bindings for `embark'."
  (evil-set-initial-state 'embark-collect-mode 'normal)
  (evil-collection-define-key 'normal 'embark-collect-mode-map
    ;; embark specific commands. `isearch-forward' isn't defined, use
    ;; `evil-search-forward' instead.
    "a" 'embark-act
    "A" 'embark-act-all
    "E" 'embark-export                   ;; was e
    "m" 'embark-select
    (kbd "M-a") 'embark-collect-direct-action-minor-mode

    "gr" 'revert-buffer))

(provide 'evil-collection-embark)
;;; evil-collection-embark.el ends here
