;;; hybrid-mode.el --- Put one foot in the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Justin Burkett <justin@burkett.cc>, Chris Ewald <chrisewald@gmail.com>
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

(defvar hybrid-mode-insert-state-cursor-backup evil-insert-state-cursor
  "Backup of `evil-insert-state-cursor'")

(defvar hybrid-mode-emacs-state-map (make-sparse-keymap)
  "Keymap that only applies in insert mode with hybrid mode
activated. Inherits bindings from `evil-emacs-state-map', which
may be overridden here.")

(defvar hybrid-mode-insert-state-local-map (make-sparse-keymap)
  "Local keymap for hybrid-mode. May be customized. Sets
  `evil-insert-state-local-map' as its parent. Takes precedence
  over `hybrid-mode-emacs-state-map'.")

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to repulse the evil from spacemacs (in insert mode).
Emacs in insert mode. Replaces the `evil-insert-state' keymap
with `hybrid-mode-emacs-state-map'."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (hybrid-mode-setup-keymaps)
    (hybrid-mode-restore-keymaps)))

(defun hybrid-mode-setup-keymaps ()
  "Enter the church of Emacs in insert mode only."

  ;; Backup and set the insert-state-cursor to the hybrid variant.
  (setq hybrid-mode-insert-state-cursor-backup evil-insert-state-cursor
        evil-insert-state-cursor hybrid-mode-insert-state-cursor)

  ;; Create the hybrid mode parent keymap by overriding the essential changes
  ;; from `evil-emacs-state-map'. Set this as the parent keymap of
  ;; `hybrid-mode-emacs-state-map' so that all key bindings may be overridden.
  (setq hybrid-mode-parent-map (make-sparse-keymap))
  (set-keymap-parent hybrid-mode-parent-map evil-emacs-state-map)
  (define-key hybrid-mode-parent-map (read-kbd-macro evil-toggle-key) nil)
  (define-key hybrid-mode-parent-map [escape] 'evil-normal-state)
  (set-keymap-parent hybrid-mode-emacs-state-map hybrid-mode-parent-map)

  ;; Set evil-insert-state-local-map to be the parent map of
  ;; hybrid-mode-insert-state-local-map. Makes for a more clear category
  ;; `helm-descbinds'.
  (set-keymap-parent hybrid-mode-insert-state-local-map evil-insert-state-local-map)

  ;; Override the mode and keymap of evil-insert-state to use the hybrid-mode
  ;; variants.
  (evil-put-property 'evil-state-properties 'insert
                     :mode 'hybrid-mode-emacs-state
                     :keymap 'hybrid-mode-emacs-state-map
                     :local 'hybrid-mode-local-insert-state
                     :local-keymap 'hybrid-mode-insert-state-local-map))

(defun hybrid-mode-restore-keymaps ()
  "Restore `evil-insert-state' to it's original form."
  ;; Return evil-insert-state cursor and properties to how they are defined in
  ;; evil-states.el
  (setq evil-insert-state-cursor hybrid-mode-insert-state-cursor-backup)
  (evil-put-property 'evil-state-properties 'insert
                     :mode 'evil-insert-state-minor-mode
                     :keymap 'evil-insert-state-map
                     :local 'evil-insert-state-local-minor-mode
                     :local-keymap 'evil-insert-state-local-map))
