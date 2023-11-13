;;; evil-collection-ggtags.el --- Evil bindings for ggtags -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, ggtags, tools

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
;; Evil bindings for `ggtags-mode'.

;;; Code:
(require 'evil-collection)
(require 'ggtags nil t)

(defvar ggtags-global-mode-map)
(defvar ggtags-mode-map)
(defvar ggtags-view-search-history-mode-map)
(defvar ggtags-view-tag-history-mode-map)
(defvar ggtags-navigation-map)

(defconst evil-collection-ggtags-maps '(ggtags-mode-map
                                        ggtags-view-search-history-mode-map
                                        ggtags-view-tag-history-mode-map
                                        ggtags-navigation-map))

;;;###autoload
(defun evil-collection-ggtags-setup ()
  "Set up `evil' bindings for `ggtags'."
  (evil-set-initial-state 'ggtags-global-mode 'normal)
  (evil-set-initial-state 'ggtags-view-search-history-mode 'normal)
  (evil-set-initial-state 'ggtags-view-tag-history-mode 'normal)

  ;; `ggtags-navigation-mode' is global and will conflict with other bindings.
  ;; https://github.com/leoliu/ggtags/issues/124
  (when (boundp 'ggtags-enable-navigation-keys)
    (setq ggtags-enable-navigation-keys nil))

  (evil-collection-define-key 'normal 'ggtags-mode-map
    "gd" 'ggtags-find-tag-dwim
    (kbd "C-t") 'ggtags-prev-mark
    "gf" 'ggtags-find-file)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'ggtags-mode-map
      "gr" 'ggtags-find-reference))

  (evil-collection-define-key 'normal 'ggtags-view-search-history-mode-map
    "gj" 'ggtags-view-search-history-next
    "gk" 'ggtags-view-search-history-prev
    (kbd "C-j") 'ggtags-view-search-history-next
    (kbd "C-k") 'ggtags-view-search-history-prev
    "]]" 'ggtags-view-search-history-next
    "[[" 'ggtags-view-search-history-prev
    "x" 'ggtags-view-search-history-kill
    "gr" 'ggtags-view-search-history-update
    "r" 'ggtags-save-to-register
    "R" 'ggtags-view-search-history-action
    "q" 'ggtags-kill-window)

  (evil-collection-define-key 'normal 'ggtags-view-tag-history-mode-map
    "gj" 'next-error-no-select
    (kbd "C-j") 'next-error-no-select
    "]]" 'next-error-no-select
    "gk" 'previous-error-no-select
    (kbd "C-k") 'previous-error-no-select
    (kbd "[[") 'previous-error-no-select
    "q" 'ggtags-kill-window)

  (evil-collection-define-key 'normal 'ggtags-navigation-map
    ;; navigation
    "gj" 'next-error
    "gk" 'previous-error
    (kbd "C-j") 'next-error
    (kbd "C-k") 'previous-error
    "]]" 'ggtags-navigation-next-file
    "[[" 'ggtags-navigation-previous-file

    ;; search
    "s" 'ggtags-navigation-isearch-forward
    "S" 'ggtags-navigation-isearch-forward

    "go" 'ggtags-navigation-visible-mode ;; FIXME: This can be anything.
    (kbd "RET") 'ggtags-navigation-mode-done))

(provide 'evil-collection-ggtags)
;;; evil-collection-ggtags.el ends here
