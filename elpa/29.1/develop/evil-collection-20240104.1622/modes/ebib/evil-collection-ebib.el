;;; evil-collection-ebib.el --- Evil bindings for ebib -*- lexical-binding: t -*-

;; Author: Justin Burkett 
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, ebib, tools

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
;; Evil bindings for the ebib.

;;; Code:
(require 'evil-collection)
(require 'ebib nil t)

(defconst evil-collection-ebib-maps '(ebib-index-mode-map
                                      ebib-entry-mode-map))

(defvar ebib-index-mode-map)
(defvar ebib-entry-mode-map)

;;;###autoload
(defun evil-collection-ebib-setup ()
  "Set up `evil' bindings for `ebib'."
  (evil-set-initial-state 'ebib-index-mode 'normal)
  (evil-make-overriding-map ebib-index-mode-map 'normal)
  (evil-collection-inhibit-insert-state 'ebib-index-mode-map)
  (evil-collection-define-key 'normal 'ebib-index-mode-map
    "d" 'ebib-kill-entry
    "D" 'ebib-delete-entry
    "gg" 'ebib-goto-first-entry
    "G" 'ebib-goto-last-entry
    "j" 'ebib-next-entry
    "k" 'ebib-prev-entry
    "n" 'ebib-search-next
    "p" 'ebib-yank-entry
    "gj" 'ebib-jump-to-entry
    "ZQ" 'ebib-quit
    "ZZ" 'ebib-lower
    (kbd "C-u") 'ebib-index-scroll-down
    (kbd "C-d") 'ebib-index-scroll-up
    (kbd "C-b") 'ebib-index-scroll-down
    (kbd "C-f") 'ebib-index-scroll-up)
  (evil-set-initial-state 'ebib-entry-mode 'normal)
  (evil-make-overriding-map ebib-entry-mode-map 'normal)
  (evil-collection-inhibit-insert-state 'ebib-entry-mode-map)
  (evil-collection-define-key 'normal 'ebib-entry-mode-map
    "[[" 'ebib-goto-prev-set
    "]]" 'ebib-goto-next-set
    "d" 'ebib-kill-field-contents
    "D" 'ebib-delete-field-contents
    "gg" 'ebib-goto-first-field
    "gk" 'ebib-goto-prev-set
    "gj" 'ebib-goto-next-set
    "G" 'ebib-goto-last-field
    "j" 'ebib-next-field
    "k" 'ebib-prev-field
    "p" 'ebib-yank-field-contents
    "y" 'ebib-copy-current-field-contents
    "ZZ" 'ebib-quit-entry-buffer
    "ZQ" 'ebib-quit-entry-buffer))

(provide 'evil-collection-ebib)
;;; evil-collection-ebib.el ends here
