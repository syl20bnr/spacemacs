;;; evil-collection-ebuku.el --- Evil bindings for Ebuku -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Alexis

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, ebuku, tools

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

;; Evil bindings for Ebuku.

;;; Code:

(require 'evil-collection)
(require 'ebuku nil t)


;;;###autoload
(defun evil-collection-ebuku-setup ()
  "Set up `evil' bindings for `Ebuku'."
  (evil-collection-define-key 'normal 'ebuku-mode-map
                              "q" 'quit-window
                              "a" 'ebuku-add-bookmark
                              "d" 'ebuku-delete-bookmark
                              "x" 'ebuku-delete-bookmark
                              "e" 'ebuku-edit-bookmark
                              "gr" 'ebuku-refresh
                              "j" 'ebuku-next-bookmark
                              (kbd "C-j") 'ebuku-next-bookmark
                              "k" 'ebuku-previous-bookmark
                              (kbd "C-k") 'ebuku-previous-bookmark
                              "r" 'ebuku-search-on-recent
                              "s" 'ebuku-search
                              "*" 'ebuku-show-all
                              "-" 'ebuku-toggle-results-limit
                              "t" 'ebuku-toggle-results-limit
                              (kbd "RET") 'ebuku-open-url
                              "C" 'ebuku-copy-url
                              "T" 'ebuku-copy-title
                              "I" 'ebuku-copy-index
                              [mouse-1] 'ebuku-open-url
                              [mouse-2] 'ebuku-open-url))


(provide 'evil-collection-ebuku)
;;; evil-collection-ebuku.el ends here
