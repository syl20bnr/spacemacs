;;; evil-collection-man.el --- Evil bindings for Man-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, man, tools

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
;; Evil bindings for `man'.

;;; Code:
(require 'evil-collection)
(require 'man)

(defconst evil-collection-man-maps '(Man-mode-map))

;;;###autoload
(defun evil-collection-man-setup ()
  "Set up `evil' bindings for `man'."
  (evil-set-initial-state 'Man-mode 'normal)
  (evil-collection-define-key 'normal 'Man-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    (kbd "]]") 'Man-next-manpage
    (kbd "[[") 'Man-previous-manpage
    (kbd "gj") 'Man-next-manpage
    (kbd "gk") 'Man-previous-manpage
    (kbd "C-j") 'Man-next-section
    (kbd "C-k") 'Man-previous-section

    ;; goto
    "gm" 'man
    "gd" 'Man-goto-section ; TODO: "gd" does not match the rationale of "go to definition". Change?
    "gR" 'Man-follow-manual-reference ; TODO: Make this match Info-follow-reference?
    "gs" 'Man-goto-see-also-section

    ;; refresh
    "gr" 'Man-update-manpage

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window))

(provide 'evil-collection-man)
;;; evil-collection-man.el ends here
