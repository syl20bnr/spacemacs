;;; evil-collection-sh-script.el --- Evil bindings for sh-script -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
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
;; Evil bindings for sh-script.

;;; Code:
(require 'evil-collection)
(require 'sh-script)

(defconst evil-collection-sh-script-maps '(sh-mode-map))

(defun evil-collection-sh-script-set-evil-shift-width ()
  "Set `evil-shift-width' according to `sh-basic-offset'."
  (setq-local evil-shift-width sh-basic-offset))

;;;###autoload
(defun evil-collection-sh-script-setup ()
  "Set up `evil' bindings for `sh-script'."
  (add-hook 'sh-mode-hook #'evil-collection-sh-script-set-evil-shift-width)

  (evil-collection-define-key 'normal 'sh-mode-map
    "gz" 'sh-show-shell))

(provide 'evil-collection-sh-script)
;;; evil-collection-sh-script.el ends here
