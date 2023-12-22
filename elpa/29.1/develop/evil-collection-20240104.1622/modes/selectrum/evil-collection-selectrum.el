;;; evil-collection-selectrum.el --- Evil bindings for Selectrum -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Maintainer: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Evil bindings for Selectrum.

;;; Code:

(require 'evil-collection)
(require 'selectrum nil t)

(defconst evil-collection-selectrum-maps '(selectrum-minibuffer-map))

;;;###autoload
(defun evil-collection-selectrum-setup ()
  "Set up `evil' bindings for `selectrum-mode'."

  (defvar evil-collection-setup-minibuffer)
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key '(insert normal) 'selectrum-minibuffer-map
      (kbd "C-b") 'selectrum-previous-page
      (kbd "C-f") 'selectrum-next-page
      (kbd "M-RET") 'selectrum-submit-exact-input
      (kbd "<return>") 'selectrum-select-current-candidate)

    (evil-collection-define-key 'insert 'selectrum-minibuffer-map
      (kbd "C-j") 'selectrum-next-candidate
      (kbd "C-k") 'selectrum-previous-candidate
      (kbd "C-n") 'selectrum-next-candidate
      (kbd "C-p") 'selectrum-previous-candidate)

    (evil-collection-define-key 'normal 'selectrum-minibuffer-map
      (kbd "j") 'selectrum-next-candidate
      (kbd "k") 'selectrum-previous-candidate
      (kbd "gj") 'selectrum-next-group
      (kbd "gk") 'selectrum-previous-group
      (kbd "G") 'selectrum-goto-end
      (kbd "gg") 'selectrum-goto-beginning
      (kbd "gy") 'selectrum-kill-ring-save)

    (when evil-want-C-u-scroll
      (evil-collection-define-key '(insert normal) 'selectrum-minibuffer-map
        (kbd "C-u") 'selectrum-previous-page))

    (when evil-want-C-d-scroll
      (evil-collection-define-key '(insert normal) 'selectrum-minibuffer-map
        (kbd "C-d") 'selectrum-next-page))))

(provide 'evil-collection-selectrum)
;;; evil-collection-selectrum.el ends here
