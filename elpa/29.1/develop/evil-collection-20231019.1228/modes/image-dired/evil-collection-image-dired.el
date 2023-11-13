;;; evil-collection-image-dired.el --- Evil bindings for image-dired-thumbnail-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, dired, image, tools

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
;; Evil bindings for `image-dired-thumbnail-mode'.

;;; Code:
(require 'evil-collection)
(require 'image-dired)

(defconst evil-collection-image-dired-maps '(image-dired-thumbnail-mode-map))

;;;###autoload
(defun evil-collection-image-dired-setup ()
  "Set up `evil' bindings for `image-dired-thumbnail-mode'."
  (evil-collection-define-key 'normal 'image-dired-thumbnail-mode-map
    ;; motion
    "h" 'image-dired-backward-image
    "l" 'image-dired-forward-image
    "j" 'image-dired-next-line
    "k" 'image-dired-previous-line

    (kbd "SPC") 'image-dired-display-next-thumbnail-original
    (kbd "S-SPC") 'image-dired-display-previous-thumbnail-original
    (kbd "RET") 'image-dired-display-thumbnail-original-image

    "c" 'image-dired-comment-thumbnail
    "d" 'image-dired-thumbnail-set-image-description
    "m" 'image-dired-mark-thumb-original-file
    "u" 'image-dired-unmark-thumb-original-file
    "D" 'image-dired-delete-char

    ;; See image+.
    "r" 'image-dired-rotate-thumbnail-right
    "R" 'image-dired-rotate-thumbnail-left
    "<" 'image-dired-rotate-original-left ; like sxiv
    ">" 'image-dired-rotate-original-right ; like sxiv

    "tr" 'image-dired-tag-thumbnail-remove
    "tt" 'image-dired-tag-thumbnail

    "ff" 'image-dired-line-up
    "fd" 'image-dired-line-up-dynamic
    "fi" 'image-dired-line-up-interactive

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-collection-image-dired)
;;; evil-collection-image-dired.el ends here
