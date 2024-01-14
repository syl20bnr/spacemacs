;;; evil-collection-image.el --- Evil bindings for Image -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, bookmark, tools

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
;; Evil bindings for `image-mode'.

;;; Code:
(require 'evil-collection)
(require 'image-mode)

(defconst evil-collection-image-maps '(image-mode-map))

;;;###autoload
(defun evil-collection-image-setup ()
  "Set up `evil' bindings for `image-mode'."
  (evil-set-initial-state 'image-mode 'normal)

  (evil-collection-define-key 'normal 'image-mode-map
    ;; motion
    "gg" 'image-bob
    "G" 'image-eob
    [remap evil-forward-char] 'image-forward-hscroll
    [remap evil-backward-char] 'image-backward-hscroll
    [remap evil-next-line] 'image-next-line
    [remap evil-previous-line] 'image-previous-line
    "0" 'image-bol
    "^" 'image-bol
    "$" 'image-eol
    (kbd "C-d") 'image-scroll-up
    (kbd "SPC") 'image-scroll-up
    (kbd "S-SPC") 'image-scroll-down
    (kbd "<delete>") 'image-scroll-down
    ;; animation
    (kbd "RET") 'image-toggle-animation
    "a0" 'image-reset-speed
    "ar" 'image-reverse-speed
    "F" 'image-goto-frame
    "," 'image-previous-frame ; mplayer/mpv style
    "." 'image-next-frame ; mplayer/mpv style
    ";" 'image-next-frame ; Evil style
    "{" 'image-decrease-speed ; mplayer/mpv style
    "}" 'image-increase-speed ; mplayer/mpv style

    "H" 'image-transform-fit-to-height
    "W" 'image-transform-fit-to-width

    "+" 'image-increase-size
    "=" 'image-increase-size
    "-" 'image-decrease-size

    "[[" 'image-previous-file
    "]]" 'image-next-file
    "gk" 'image-previous-file
    "gj" 'image-next-file
    (kbd "C-k") 'image-previous-file
    (kbd "C-j") 'image-next-file

    (kbd "C-c C-c") 'image-toggle-display

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window)

  ;; TODO: What if the user changes `evil-want-C-u-scroll' after this is run?
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'image-mode-map
      (kbd "C-u") 'image-scroll-down)))

(provide 'evil-collection-image)
;;; evil-collection-image.el ends here
