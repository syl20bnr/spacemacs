;;; evil-collection-doc-view.el --- Evil bindings for docview -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, bindings, files, pdf, ps, dvi

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
;;; Evil bindings for doc-view.

;;; Code:
(require 'evil-collection)
(require 'doc-view)

(defconst evil-collection-doc-view-maps '(doc-view-mode-map))

;;;###autoload
(defun evil-collection-doc-view-setup ()
  "Set up `evil' bindings for `doc-view'."
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-collection-define-key 'normal 'doc-view-mode-map
    "q" 'quit-window
    [remap evil-next-line] 'doc-view-next-line-or-next-page
    [remap evil-previous-line] 'doc-view-previous-line-or-previous-page
    [remap evil-backward-char] 'image-backward-hscroll
    [remap evil-forward-char] 'image-forward-hscroll
    (kbd "C-j") 'doc-view-next-page
    (kbd "C-k") 'doc-view-previous-page
    "gj" 'doc-view-next-page
    "gk" 'doc-view-previous-page
    (kbd "C-d") 'forward-page
    (kbd "SPC") 'doc-view-scroll-up-or-next-page
    (kbd "DEL") 'doc-view-scroll-down-or-previous-page
    (kbd "S-SPC") 'doc-view-scroll-down-or-previous-page
    "gg" 'doc-view-first-page
    "G" 'doc-view-last-page
    "J" 'doc-view-goto-page
    (kbd "RET") 'image-next-line

    ;; zoom
    "+" 'doc-view-enlarge
    "=" 'doc-view-enlarge
    "0" 'doc-view-scale-reset
    "-" 'doc-view-shrink

    "W" 'doc-view-fit-width-to-window ; Like evil-image.
    "H" 'doc-view-fit-height-to-window ; Like evil-image.
    "P" 'doc-view-fit-page-to-window
    "X" 'doc-view-kill-proc

    (kbd "s s") 'doc-view-set-slice
    (kbd "s m") 'doc-view-set-slice-using-mouse
    (kbd "s b") 'doc-view-set-slice-from-bounding-box
    (kbd "s r") 'doc-view-reset-slice

    (kbd "/") 'doc-view-search
    (kbd "?") 'doc-view-search-backward
    (kbd "C-t") 'doc-view-show-tooltip
    (kbd "C-c C-c") 'doc-view-toggle-display
    (kbd "C-c C-t") 'doc-view-open-text

    ;; refresh
    (kbd "gr") 'doc-view-revert-buffer)

  ;; TODO: What if the user changes `evil-want-C-u-scroll' after this is run?
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'doc-view-mode-map
      (kbd "C-u") 'backward-page)))

(provide 'evil-collection-doc-view)
;;; evil-collection-doc-view.el ends here
