;;; evil-collection-log-view.el --- Bindings for `log-view' -*- lexical-binding: t -*-

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
;; Bindings for `log-view'.

;;; Code:
(require 'evil-collection)
(require 'log-view)

(defconst evil-collection-log-view-maps '(log-view-mode-map))

;;;###autoload
(defun evil-collection-log-view-setup ()
  "Set up `evil' bindings for `log-view'."

  ;; Currently vc has these various log-view modes
  (evil-set-initial-state 'vc-hg-log-view-mode 'normal)
  (evil-set-initial-state 'vc-git-log-view-mode 'normal)
  (evil-set-initial-state 'vc-svn-log-view-mode 'normal)

  (evil-collection-define-key 'normal 'log-view-mode-map
    "q" 'quit-window
    (kbd "TAB") 'log-view-toggle-entry-display
    "m" 'log-view-toggle-mark-entry
    "c" 'log-view-modify-change-comment
    (kbd "RET") 'log-view-diff
    "d" 'log-view-diff
    "=" 'log-view-diff
    "D" 'log-view-diff-changeset
    "a" 'log-view-annotate-version
    "F" 'log-view-find-revision
    "gj" 'log-view-msg-next
    "gk" 'log-view-msg-prev
    "]]" 'log-view-msg-next
    "[[" 'log-view-msg-prev
    (kbd "C-j") 'log-view-file-next
    (kbd "C-k") 'log-view-file-prev))

(provide 'evil-collection-log-view)
;;; evil-collection-log-view.el ends here
