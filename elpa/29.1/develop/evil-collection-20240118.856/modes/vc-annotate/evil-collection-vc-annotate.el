;;; evil-collection-vc-annotate.el --- Bindings for `vc-annotate' -*- lexical-binding: t -*-

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
;; Bindings for `vc-annotate'

;;; Code:
(require 'evil-collection)
(require 'vc-annotate)

(defconst evil-collection-vc-annotate-maps '(vc-annotate-mode-map))

;;;###autoload
(defun evil-collection-vc-annotate-setup ()
  "Set up `evil' bindings for `vc-annotate'."
  (evil-set-initial-state 'vc-annotate-mode 'normal)
  (evil-collection-define-key 'normal 'vc-annotate-mode-map
    "q" 'quit-window
    "a" 'vc-annotate-revision-previous-to-line
    "d" 'vc-annotate-show-diff-revision-at-line
    "=" 'vc-annotate-show-diff-revision-at-line
    "D" 'vc-annotate-show-changeset-diff-revision-at-line
    "F" 'vc-annotate-find-revision-at-line
    "J" 'vc-annotate-revision-at-line
    "L" 'vc-annotate-show-log-revision-at-line
    "gj" 'vc-annotate-next-revision
    "gk" 'vc-annotate-prev-revision
    "]]" 'vc-annotate-next-revision
    "[[" 'vc-annotate-prev-revision
    (kbd "C-j") 'vc-annotate-next-revision
    (kbd "C-k") 'vc-annotate-prev-revision
    "W" 'vc-annotate-working-revision
    "A" 'vc-annotate-toggle-annotation-visibility
    (kbd "RET") 'vc-annotate-goto-line))

(provide 'evil-collection-vc-annotate)
;;; evil-collection-vc-annotate.el ends here
