;;; evil-collection-robe.el --- Bindings for `robe' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, evil, tools, robe

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
;;; Bindings for `robe'.

;;; Code:
(require 'evil-collection)
(require 'robe nil t)

(defconst evil-collection-robe-maps '(robe-mode-map))

;;;###autoload
(defun evil-collection-robe-setup ()
  "Set up `evil' bindings for `robe'."
  (evil-collection-define-key 'normal 'robe-mode-map
    "gd" 'robe-jump
    (kbd "C-t") 'pop-tag-mark
    "K" 'robe-doc
    "gr" 'robe-rails-refresh))

(provide 'evil-collection-robe)
;;; evil-collection-robe.el ends here
