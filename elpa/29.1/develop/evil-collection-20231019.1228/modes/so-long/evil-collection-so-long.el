;;; evil-collection-so-long.el --- Bindings for `so-long' -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

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
;;; Bindings for so-long.

;;; Code:
(require 'evil-collection)
(require 'so-long nil t)

(defvar so-long-mode-map)
(defconst evil-collection-so-long-maps '(so-long-mode-map))

(defun evil-collection-so-long-setup ()
  "Set up `evil' bindings for so-long."
  (evil-collection-define-key '(normal visual) 'so-long-mode-map
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "gj" #'evil-next-line
    "gk" #'evil-previous-line
    "0" #'evil-beginning-of-visual-line
    "g0" #'evil-beginning-of-visual-line
    "$" #'evil-end-of-visual-line
    "g$" #'evil-end-of-line
    "^" #'evil-first-non-blank-of-visual-line
    "g^" #'evil-first-non-blank
    "G" 'end-of-buffer))

(provide 'evil-collection-so-long)
;;; evil-collection-so-long.el ends here
