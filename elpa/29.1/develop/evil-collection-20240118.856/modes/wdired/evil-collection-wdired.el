;;; evil-collection-wdired.el --- Bindings for `wdired' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `wdired'.

;;; Code:
(require 'evil-collection)
(require 'wdired)

(defconst evil-collection-wdired-maps '(wdired-mode-map))

;;;###autoload
(defun evil-collection-wdired-setup ()
  "Set up `evil' bindings for `wdired'."
  (evil-set-initial-state 'wdired-mode 'normal)

  (evil-collection-define-key nil 'wdired-mode-map
    [remap evil-write] 'wdired-finish-edit)

  (evil-collection-define-key 'normal 'wdired-mode-map
    "ZQ" 'wdired-abort-changes
    "ZZ" 'wdired-finish-edit
    (kbd "<escape>") 'wdired-exit))

(provide 'evil-collection-wdired)
;;; evil-collection-wdired.el ends here
