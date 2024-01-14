;;; evil-collection-flymake.el --- Evil Bindings for Flymake -*- lexical-binding: t -*-

;; Copyright (C) 2018, 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, flymake, tools

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
;; Evil bindings for `flymake-mode'.

;;; Code:
(require 'flymake)
(require 'evil-collection)

(defconst evil-collection-flymake-maps '(flymake-mode-map
					 flymake-project-diagnostics-mode-map
					 flymake-diagnostics-buffer-mode-map))

;;;###autoload
(defun evil-collection-flymake-setup ()
  "Set up `evil' bindings for `flymake'."
  (dolist (map '(flymake-diagnostics-buffer-mode-map flymake-project-diagnostics-mode-map))
    (evil-collection-set-readonly-bindings map)
    (evil-collection-define-key 'normal map
      (kbd "C-j") 'flymake-goto-next-error
      (kbd "C-k") 'flymake-goto-prev-error
      (kbd "RET") 'flymake-goto-diagnostic
      (kbd "S-RET") 'flymake-show-diagnostic
      (kbd "M-RET") 'flymake-show-diagnostic
      (kbd "go") 'flymake-show-diagnostic
      (kbd "gO") 'flymake-show-diagnostic)))

(provide 'evil-collection-flymake)
;;; evil-collection-flymake.el ends here
