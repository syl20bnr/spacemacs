;;; evil-collection-magit-section.el --- Bindings for magit-section -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for magit-section.

;;; Code:
(require 'evil-collection)
(require 'magit-section nil t)

(defvar magit-section-mode-map)
(defconst evil-collection-magit-section-maps '(magit-section-mode-map))

(defcustom evil-collection-magit-section-use-z-for-folds nil
  "When non nil, use \"z\" as a prefix for common vim fold commands, such as
  - z1 Reset visibility to level 1 for all sections
  - z2 Reset visibility to level 2 for all sections
  - z3 Reset visibility to level 3 for all sections
  - z4 Reset visibility to level 4 for all sections
  - za Toggle a section
  - zo Show section
  - zO Show sections recursively
  - zc Hide section
  - zC Hide sections recursively
  - zr Same as z4.

When this option is enabled, the stash popup is available on \"Z\"."
  :group 'magit
  :type  'boolean)

(defun evil-collection-magit-section-setup ()
  "Set up `evil' bindings for magit-section."
  (if evil-collection-magit-section-use-z-for-folds
      (evil-collection-define-key 'normal 'magit-section-mode-map
        "z1" 'magit-section-show-level-1-all
        "z2" 'magit-section-show-level-2-all
        "z3" 'magit-section-show-level-3-all
        "z4" 'magit-section-show-level-4-all
        "za" 'magit-section-toggle
        "zc" 'magit-section-hide
        "zC" 'magit-section-hide-children
        "zo" 'magit-section-show
        "zO" 'magit-section-show-children
        "zr" 'magit-section-show-level-4-all)
    (evil-collection-define-key 'normal 'magit-section-mode-map
      (kbd "M-1") 'magit-section-show-level-1-all
      (kbd "M-2") 'magit-section-show-level-2-all
      (kbd "M-3") 'magit-section-show-level-3-all
      (kbd "M-4") 'magit-section-show-level-4-all))

  (evil-collection-define-key 'normal 'magit-section-mode-map
    (kbd "TAB") 'magit-section-toggle
    [C-tab]     'magit-section-cycle
    [M-tab]     'magit-section-cycle
    ;; [backtab] is the most portable binding for Shift+Tab.
    [backtab]   'magit-section-cycle-global
    "gh" 'magit-section-up
    (kbd "C-k") 'magit-section-backward
    (kbd "C-j") 'magit-section-forward
    "gk" 'magit-section-backward-sibling
    "[" 'magit-section-backward-sibling
    "gj" 'magit-section-forward-sibling
    "]" 'magit-section-forward-sibling
    (kbd "1") 'magit-section-show-level-1
    (kbd "2") 'magit-section-show-level-2
    (kbd "3") 'magit-section-show-level-3
    (kbd "4") 'magit-section-show-level-4))

(provide 'evil-collection-magit-section)
;;; evil-collection-magit-section.el ends here
