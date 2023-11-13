;;; evil-collection-vundo.el --- Bindings for `vundo-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, vundo, convenience

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
;;; Bindings for `vundo-mode'.

;;; Code:
(require 'vundo nil t)
(require 'evil-collection)

(defgroup evil-collection-vundo nil
  "Evil bindings for `vundo-mode'."
  :group 'evil-collection)

(defvar vundo-mode-map)

(defconst evil-collection-vundo-maps '(vundo-mode-map))

;;;###autoload
(defun evil-collection-vundo-setup ()
  "Set up `evil' bindings for `vundo'."
  (add-hook 'vundo-mode-hook #'evil-normalize-keymaps)
  ;; `vundo' defaults to emacs state.
  (add-hook 'vundo-mode-hook #'evil-normal-state)

  (evil-collection-define-key 'normal 'vundo-mode-map
    [remap evil-backward-char] 'vundo-backward
    [remap evil-forward-char]  'vundo-forward
    [remap evil-next-line]     'vundo-next
    [remap evil-previous-line] 'vundo-previous
    [remap evil-window-top]    'vundo-stem-root
    [remap evil-window-bottom] 'vundo-stem-end
    [remap evil-ret]           'vundo-confirm
    [remap evil-quit]          'vundo-quit
    [remap evil-save-and-close] 'vundo-confirm
    [remap evil-save-modified-and-close] 'vundo-confirm
    [remap evil-delete-backward-char] 'vundo-backward
    (kbd "C-n") 'vundo-next
    (kbd "C-p") 'vundo-previous
    (kbd "C-j") 'vundo-next
    (kbd "C-k") 'vundo-previous
    (kbd "M-j") 'vundo-next
    (kbd "M-k") 'vundo-previous
    "i" 'vundo--inspect
    "d" 'vundo--debug
    "q" 'vundo-quit
    "ZZ" 'vundo-quit
    "ZQ" 'vundo-quit))

(provide 'evil-collection-vundo)
;;; evil-collection-vundo.el ends here
