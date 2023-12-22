;;; evil-collection-indent.el --- Evil bindings for indentation -*- lexical-binding: t -*-

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
;; Evil bindings for indentation.

;;; Commentary:
;;

;;; Code:
(require 'evil-collection)

(defconst evil-collection-indent-maps '(indent-rigidly-map))

;;;###autoload
(defun evil-collection-indent-setup ()
  "Set up `evil' bindings for autoloaded files."
  (evil-collection-define-key nil 'indent-rigidly-map
    "h" 'indent-rigidly-left
    "l" 'indent-rigidly-right
    "H" 'indent-rigidly-left-to-tab-stop
    "L" 'indent-rigidly-right-to-tab-stop))


(provide 'evil-collection-indent)
;;; evil-collection-indent.el ends here
