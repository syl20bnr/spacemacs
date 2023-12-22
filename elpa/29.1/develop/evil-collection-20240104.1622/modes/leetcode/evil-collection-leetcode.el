;;; evil-collection-leetcode.el --- Bindings for `leetcode' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, tools

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
;;; Bindings for `leetcode'.

;;; Code:
(require 'evil-collection)
(require 'leetcode nil t)

(defconst evil-collection-leetcode-maps
  '(leetcode--problems-mode-map
    leetcode--problem-description-mode-map))

;;;###autoload
(defun evil-collection-leetcode-setup ()
  "Set up `evil' bindings for `leetcode'."
  (evil-collection-define-key 'normal 'leetcode--problems-mode-map
    (kbd "RET") 'leetcode-show-current-problem
    "gr" 'leetcode-problems-refresh
    "q" 'quit-window))

(provide 'evil-collection-leetcode)
;;; evil-collection-leetcode.el ends here
