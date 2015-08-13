;;; hybrid-mode.el --- Put one foot in the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Justin Burkett <justin@burkett.cc>
;; Keywords: convenience editing
;; Created: 12 Aug 2015
;; Version: 1.00
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.

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

;;; Code:

(defvar hybrid-mode-evil-insert-state-keys
  '("\C-v" "\C-k" "\C-o" "\C-r" "\C-y" "\C-e" "\C-n" "\C-p" "\C-x\C-n"
    "\C-x\C-p" "\C-t" "\C-d" "\C-a" "\C-w" [remap delete-backward-char]
    [delete])
  "Taken from evil-maps.el")

(defvar hybrid-mode-insert-state-map-backup nil
  "Backup of `evil-insert-state-map'.")

(defvar hybrid-mode-insert-state-cursor-backup nil
  "Backup of `evil-insert-state-cursor'")

(defface hybrid-mode-insert-face-backup
  '((t . (:inherit spacemacs-insert-face)))
  "Backup of `spacemacs-insert-face'")

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to repulse the evil from spacemacs (in insert mode).

`evil-insert-state-map' is prevented from shadowing emacs key bindings."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (hybrid-mode-setup-keymaps)
    (hybrid-mode-restore-keymaps)))

(defun hybrid-mode-setup-keymaps ()
  "Enter the church of Emacs in insert mode only."
  (setq hybrid-mode-insert-state-map-backup evil-insert-state-map
        hybrid-mode-insert-state-cursor-backup evil-insert-state-cursor)
  (setq evil-insert-state-cursor (list (car evil-emacs-state-cursor)
                                       (cadr evil-insert-state-cursor)))
  (copy-face 'spacemacs-insert-face 'hybrid-mode-insert-face-backup)
  (copy-face 'spacemacs-emacs-face 'spacemacs-insert-face)
  ;; (setcdr evil-insert-state-map nil)
  ;; (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (dolist (key hybrid-mode-evil-insert-state-keys)
    (define-key evil-insert-state-map key nil))
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) nil)
  ;; see if the above works withou this
  ;; (when (bound-and-true-p evil-escape-mode)
  ;;   (define-key evil-insert-state-map "f" 'evil-escape-insert-state))
  )

(defun hybrid-mode-restore-keymaps ()
  "Go home."
  (setq evil-insert-state-map hybrid-mode-insert-state-map-backup
        evil-insert-state-cursor hybrid-mode-insert-state-cursor-backup)
  (copy-face 'hybrid-mode-insert-face-backup 'spacemacs-insert-face))
