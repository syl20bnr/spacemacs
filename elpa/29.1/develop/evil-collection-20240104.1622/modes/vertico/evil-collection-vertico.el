;;; evil-collection-vertico.el --- Evil bindings for vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, vertico, tools

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
;; Evil bindings for `vertico'.

;;; Code:
(require 'evil-collection)
(require 'vertico nil t)

;;;###autoload
(defun evil-collection-vertico-setup ()
  "Set up `evil' bindings for `vertico'."
  (evil-collection-define-key nil 'vertico-map
    (kbd "<escape>") 'abort-recursive-edit)

  (defvar evil-collection-setup-minibuffer)
  (when evil-collection-setup-minibuffer
    (when evil-want-C-u-scroll
      (evil-collection-define-key 'normal 'vertico-map
        (kbd "C-u") 'vertico-scroll-down)
      (unless evil-want-C-u-delete
        (evil-collection-define-key 'insert 'vertico-map
          (kbd "C-u") 'vertico-scroll-down)))
    (when evil-want-C-d-scroll
      (evil-collection-define-key 'normal 'vertico-map
        (kbd "C-d") 'vertico-scroll-up))

    (evil-collection-define-key 'normal 'vertico-map
      "gj" 'vertico-next-group
      "gk" 'vertico-previous-group
      "j" 'vertico-next
      "k" 'vertico-previous)

    (evil-collection-define-key 'insert 'vertico-map
      (kbd "C-n") 'vertico-next
      (kbd "C-p") 'vertico-previous)))

(provide 'evil-collection-vertico)
;;; evil-collection-vertico.el ends here
