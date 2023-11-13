;;; evil-collection-org.el --- Evil bindings for org -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, org, tools

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
;; Evil basic bindings for org-mode. It's NOT intended to supersede
;; `evil-org-mode'.
;;

;;; Code:
(require 'evil-collection)

(defconst evil-collection-org-maps '(org-mode-map))

(declare-function org-shifttab "org")
(declare-function org-backward-paragraph "org")
(declare-function org-forward-paragraph "org")
(declare-function org-backward-sentence "org")
(declare-function org-forward-sentence "org")

;;;###autoload
(defun evil-collection-org-setup ()
  "Set up `evil' bindings for `org'."
  (evil-collection-define-key 'normal 'org-mode-map
    [tab] 'org-cycle
    [S-tab] 'org-shifttab)

  (evil-collection-define-key 'motion 'org-mode-map
    "{" 'org-backward-paragraph
    "}" 'org-forward-paragraph
    "(" 'org-backward-sentence
    ")" 'org-forward-sentence))

(provide 'evil-collection-org)
;;; evil-collection-org.el ends here
