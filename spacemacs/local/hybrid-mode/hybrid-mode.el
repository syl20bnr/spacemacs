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

(require 'evil)

(defgroup hybrid-mode nil
  "Customization options for hybrid-mode"
  :group 'emulations
  :prefix "hybrid-mode-")

(defcustom hybrid-mode-insert-state-cursor 'bar
  "Cursor spec for hybrid mode in insert state."
  :group 'hybrid-mode)

(defvar hybrid-mode-default-evil-insert-state-bindings
  `(("\C-v" . quoted-insert)
    ("\C-k" . evil-insert-digraph)
    ("\C-o" . evil-execute-in-normal-state)
    ("\C-r" . evil-paste-from-register)
    ("\C-y" . evil-copy-from-above)
    ("\C-e" . evil-copy-from-below)
    ("\C-n" . evil-complete-next)
    ("\C-p" . evil-complete-previous)
    ("\C-p" . hippie-expand) ;; auto-complete layer sets this
    ("\C-w" . evil-window-map)
    ("\C-w" . evil-delete-backward-word)
    ("\C-x\C-n" . evil-complete-next-line)
    ("\C-x\C-p" . evil-complete-previous-line)
    ("\C-t" . evil-shift-right-line)
    ("\C-d" . evil-shift-left-line)
    ("\C-a" . evil-paste-last-insertion)
    ([remap delete-backward-char] . evil-delete-backward-char-and-join)
    ([delete] . delete-char)
    (,(read-kbd-macro evil-toggle-key) . evil-emacs-state))
  "Taken from evil-maps.el")

(defvar hybrid-mode-insert-state-map-backup nil
  "Backup of `evil-insert-state-map'.")

(defvar hybrid-mode-insert-state-cursor-backup evil-insert-state-cursor
  "Backup of `evil-insert-state-cursor'")

(defvar hybrid-mode-insert-state-map (make-sparse-keymap)
  "Keymap that only applies in insert mode with hybrid mode
activated.")

(defvar hybrid-mode-insert-state-map-active nil
  "Flag for acitvating `hybrid-mode-insert-state-map'")

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

(defun hybrid-mode-activate-keymap ()
  "Activate `hybrid-mode-insert-state-map'"
  (setq hybrid-mode-insert-state-map-active t))

(defun hybrid-mode-deactivate-keymap ()
  "Deactivate `hybrid-mode-insert-state-map'"
  (setq hybrid-mode-insert-state-map-active nil))

(defun hybrid-mode-setup-keymaps ()
  "Enter the church of Emacs in insert mode only."
  (add-hook 'evil-insert-state-entry-hook 'hybrid-mode-activate-keymap)
  (add-hook 'evil-insert-state-exit-hook 'hybrid-mode-deactivate-keymap)
  (add-to-list 'minor-mode-map-alist
               `(hybrid-mode-insert-state-map-active . ,hybrid-mode-insert-state-map))
  (setq hybrid-mode-insert-state-map-backup evil-insert-state-map
        hybrid-mode-insert-state-cursor-backup evil-insert-state-cursor
        evil-insert-state-cursor hybrid-mode-insert-state-cursor)
  ;; Remove default evil insert mode bindings from `evil-insert-state-map'. This
  ;; will not remove any bindings that the user explicitly set in this keymap,
  ;; the important ones being any bindings related to escaping insert mode.
  (dolist (key-bnd hybrid-mode-default-evil-insert-state-bindings)
    (when (eq (cdr key-bnd) (lookup-key evil-insert-state-map (car key-bnd)))
      (define-key evil-insert-state-map (car key-bnd) nil))))

(defun hybrid-mode-restore-keymaps ()
  "Go home."
  (hybrid-mode-deactivate-keymap)
  (remove-hook 'evil-insert-state-entry-hook 'hybrid-mode-activate-keymap)
  (remove-hook 'evil-insert-state-exit-hook 'hybrid-mode-deactivate-keymap)
  (setq evil-insert-state-map hybrid-mode-insert-state-map-backup
        evil-insert-state-cursor hybrid-mode-insert-state-cursor-backup))
