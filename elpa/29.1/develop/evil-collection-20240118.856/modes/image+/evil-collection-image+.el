;;; evil-collection-image+.el --- Evil bindings for image-mode with image+ -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, image, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for image-mode with image+.

;;; Code:
(require 'evil-collection)
(require 'image+ nil t)

(defconst evil-collection-image+-maps '(image-mode-map))

;;;###autoload
(defun evil-collection-image+-setup ()
  "Set up `evil' bindings for `image+'."
  (evil-collection-define-key 'normal 'image-mode-map
    ;; zoom
    "+" 'imagex-sticky-zoom-in
    "=" 'imagex-sticky-zoom-in
    "-" 'imagex-sticky-zoom-out
    "O" 'imagex-sticky-restore-original

    "M" 'imagex-sticky-maximize
    "m" 'imagex-auto-adjust-mode
    "S" 'imagex-sticky-save-image
    "r" 'imagex-sticky-rotate-right
    "R" 'imagex-sticky-rotate-left
    "<" 'imagex-sticky-rotate-left ; like sxiv
    ">" 'imagex-sticky-rotate-right)) ; like sxiv

(provide 'evil-collection-image+)
;;; evil-collection-image+.el ends here
