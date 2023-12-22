;;; evil-collection-buff-menu.el --- Bindings for `buff-menu' -*- lexical-binding: t -*-

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
;;; Bindings for `buff-menu'.

;;; Code:
(require 'evil-collection)
(require 'tabulated-list)

;; `evil-collection-buff-menu-Buffer-menu-unmark-all'

;; Code taken from emacs-26 repository.

(defconst evil-collection-buff-menu-maps '(Buffer-menu-mode-map))

;; This is for `evil-collection-Buffer-menu-unmark-all-buffers.'
(defsubst evil-collection-buff-menu-tabulated-list-header-overlay-p (&optional pos)
  "Return non-nil if there is a fake header.
Optional arg POS is a buffer position where to look for a fake header;
defaults to `point-min'."
  (overlays-at (or pos (point-min))))

(defun evil-collection-buff-menu-Buffer-menu-unmark-all ()
  "Cancel all requested operations on buffers."
  (interactive)
  (evil-collection-buff-menu-Buffer-menu-unmark-all-buffers ?\r))

(defun evil-collection-buff-menu-Buffer-menu-unmark-all-buffers (mark)
  "Cancel a requested operation on all buffers.
MARK is the character to flag the operation on the buffers.
When called interactively prompt for MARK;  RET remove all marks."
  (interactive "cRemove marks (RET means all):")
  (save-excursion
    (goto-char (point-min))
    (when (evil-collection-buff-menu-tabulated-list-header-overlay-p)
      (forward-line))
    (while (not (eobp))
      (let ((xmarks (list (aref (tabulated-list-get-entry) 0)
                          (aref (tabulated-list-get-entry) 2))))
        (when (or (char-equal mark ?\r)
                  (member (char-to-string mark) xmarks))
          (Buffer-menu--unmark)))
      (forward-line))))

;; `evil-collection-buff-menu-Buffer-menu-unmark-all'

;;;###autoload
(defun evil-collection-buff-menu-setup ()
  "Set up `evil' bindings for `buff-menu'.."
  (evil-set-initial-state 'Buffer-menu-mode 'normal)

  (evil-collection-define-key 'normal 'Buffer-menu-mode-map
    "ZQ" 'evil-quit
    "ZZ" 'quit-window
    "gr" 'revert-buffer
    "go" 'Buffer-menu-this-window
    "gO" 'Buffer-menu-other-window
    "d" 'Buffer-menu-delete
    "s" 'Buffer-menu-save
    [mouse-2] 'Buffer-menu-mouse-select
    [follow-link] 'mouse-face
    "x" 'Buffer-menu-execute
    "o" 'tabulated-list-sort
    "gv" 'Buffer-menu-select
    "gV" 'Buffer-menu-view
    "v" 'evil-visual-char

    ;; mark
    "u" 'Buffer-menu-unmark
    "U" (if (< emacs-major-version 26)
            'evil-collection-buff-menu-Buffer-menu-unmark-all
          'Buffer-menu-unmark-all)
    "m" 'Buffer-menu-mark

    "X" 'Buffer-menu-bury

    ;; Default ones, unchanged. Redundant ones commented
    "2" 'Buffer-menu-2-window
    "1" 'Buffer-menu-1-window
    (kbd "C-m") 'Buffer-menu-this-window
    (kbd "C-k") 'Buffer-menu-delete
    (kbd "C-d") 'Buffer-menu-delete-backwards
    (kbd "<delete>") 'Buffer-menu-backup-unmark
    "~" 'Buffer-menu-not-modified
    "t" 'Buffer-menu-visit-tags-table
    "%" 'Buffer-menu-toggle-read-only
    "T" 'Buffer-menu-toggle-files-only
    (kbd "M-s a C-s") 'Buffer-menu-isearch-buffers
    (kbd "M-s a M-C-s") 'Buffer-menu-isearch-buffers-regexp
    (kbd "M-s a C-o") 'Buffer-menu-multi-occur))

(provide 'evil-collection-buff-menu)
;;; evil-collection-buff-menu.el ends here
