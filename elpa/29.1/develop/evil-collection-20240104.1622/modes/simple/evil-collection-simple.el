;;; evil-collection-simple.el --- Bindings for `simple' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

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
;; Bindings for `simple'

;;; Code:
(require 'evil-collection)
(require 'simple)

(defvar special-mode-map)

(defconst evil-collection-simple-maps '(special-mode-map))

;;;###autoload
(defun evil-collection-simple-setup ()
  "Set up `evil' bindings for `simple'."
  (evil-collection-set-readonly-bindings 'special-mode-map)
  (evil-collection-define-key nil 'special-mode-map
    "g" nil
    "gr" 'revert-buffer
    "h" nil
    "?" nil
    "0" nil))

(provide 'evil-collection-simple)
;;; evil-collection-simple.el ends here
