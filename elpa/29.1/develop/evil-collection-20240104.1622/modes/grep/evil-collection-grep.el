;;; evil-collection-grep.el --- Bindings for `grep' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
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
;; Bindings for `grep'.

;;; Code:
(require 'evil-collection)
(require 'grep)

(defconst evil-collection-grep-maps '(grep-mode-map))

;;;###autoload
(defun evil-collection-grep-setup ()
  "Set up `evil' bindings for `grep'."
  (evil-collection-define-key 'normal 'grep-mode-map
    "n" 'evil-search-next
    "\C-j" 'next-error-no-select
    "\C-k" 'previous-error-no-select)

  ;; `wgrep' integration
  (when (fboundp 'wgrep-setup)
    (evil-collection-define-key 'normal 'grep-mode-map
      "i" 'wgrep-change-to-wgrep-mode)))

(provide 'evil-collection-grep)
;;; evil-collection-grep.el ends here
