;;; evil-collection-omnisharp.el --- Evil bindings for omnisharp -*- lexical-binding: t -*-

;; Copyright (C) 2019 Omair Majid

;; Author: Omair Majid <omajid@redhat.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, omnisharp, convenience

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
;; Evil bindings for Omnisharp.

;;; Code:
(require 'omnisharp nil t)
(require 'evil-collection)

(defconst evil-collection-omnisharp-maps '(omnisharp-mode-map))

;;;###autoload
(defun evil-collection-omnisharp-setup ()
  "Set up `evil' bindings for `omnisharp'."
  (evil-collection-define-key '(normal visual) 'omnisharp-mode-map
    "gd" 'omnisharp-go-to-definition
    "gR" 'omnisharp-reload-solution
    "K" 'omnisharp-current-type-documentation))

(provide 'evil-collection-omnisharp)
;;; evil-collection-omnisharp.el ends here
