;;; evil-collection-elisp-slime-nav.el --- Bindings for `elisp-slime-nav' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elisp-slime-nav, tools

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
;;; Bindings for `elisp-slime-nav'.

;;; Code:
(require 'evil-collection)
(require 'elisp-slime-nav nil t)

(defconst evil-collection-elisp-slime-nav-maps '(elisp-slime-nav-mode-map))

;;;###autoload
(defun evil-collection-elisp-slime-nav-setup ()
  "Set up `evil' bindings for `elisp-slime-nav'."
  (evil-collection-define-key 'normal 'elisp-slime-nav-mode-map
    "gd" 'elisp-slime-nav-find-elisp-thing-at-point
    (kbd "C-t") 'pop-tag-mark
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(provide 'evil-collection-elisp-slime-nav)
;;; evil-collection-elisp-slime-nav.el ends here
