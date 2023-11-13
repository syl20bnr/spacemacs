;;; evil-collection-go-mode.el --- Bindings for `go-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools, golang

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
;; Bindings for `go-mode'.

;;; Code:
(require 'evil-collection)
(require 'go-mode nil t)

(defconst evil-collection-go-mode-maps '(go-mode-map
                                         godoc-mode-map))

;;;###autoload
(defun evil-collection-go-mode-setup ()
  "Set up `evil' bindings for `go-mode'."
  (evil-collection-define-key 'normal 'go-mode-map
    "gd" 'godef-jump
    "K" 'godef-describe)
  (evil-collection-define-key 'normal 'godoc-mode-map
    "q" 'quit-window
    "g?" 'describe-mode))

(provide 'evil-collection-go-mode)
;;; evil-collection-go-mode.el ends here
