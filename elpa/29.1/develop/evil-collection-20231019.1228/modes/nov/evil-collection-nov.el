;;; evil-collection-nov.el --- Bindings for `nov' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools, epub

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
;;; Bindings for`nov'.

;;; Code:
(require 'evil-collection)
(require 'nov nil t)

(defvar nov-mode-map)

(defconst evil-collection-nov-maps '(nov-mode-map))

;;;###autoload
(defun evil-collection-nov-setup ()
  "Set up `evil' bindings for `nov'."
  (evil-collection-define-key 'normal 'nov-mode-map
    "gr" 'nov-render-document
    "s" 'nov-view-source
    "S" 'nov-view-content-source
    "g?" 'nov-display-metadata
    "gj" 'nov-next-document
    (kbd "C-j") 'nov-next-document
    (kbd "M-j") 'nov-next-document
    "]]" 'nov-next-document
    "gk" 'nov-previous-document
    (kbd "C-k") 'nov-previous-document
    (kbd "M-k") 'nov-previous-document
    "[[" 'nov-previous-document

    "t" 'nov-goto-toc
    "i" 'nov-goto-toc
    (kbd "RET") 'nov-browse-url
    (kbd "<follow-link>") 'mouse-face
    (kbd "<mouse-2>") 'nov-browse-url
    (kbd "TAB") 'shr-next-link
    (kbd "M-TAB") 'shr-previous-link
    (kbd "<backtab>") 'shr-previous-link
    (kbd "SPC") 'nov-scroll-up
    (kbd "S-SPC") 'nov-scroll-down
    (kbd "DEL") 'nov-scroll-down))

(provide 'evil-collection-nov)
;;; evil-collection-nov.el ends here
