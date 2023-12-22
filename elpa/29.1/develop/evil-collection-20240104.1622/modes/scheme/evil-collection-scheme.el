;;; evil-collection-scheme.el --- Evil bindings for scheme -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, lisp, languages

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
;; Evil bindings for scheme.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-scheme-maps '(scheme-mode-map))

(defun evil-collection-scheme-repl ()
  "Open the scheme REPL."
  (interactive)
  (pop-to-buffer (get-buffer-create "*scheme*"))
  (call-interactively #'run-scheme))

;;;###autoload
(defun evil-collection-scheme-setup ()
  "Set up `evil' bindings for `scheme'."
  (evil-collection-define-key 'normal 'scheme-mode-map
    "gz" 'evil-collection-scheme-repl))

(provide 'evil-collection-scheme)
;;; evil-collection-scheme.el ends here
