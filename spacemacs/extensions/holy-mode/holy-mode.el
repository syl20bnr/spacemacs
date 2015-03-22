;;; holy-mode.el --- Enter the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing
;; Created: 18 Mar 2015
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

(defvar holy-mode-normal-state-modes-backup nil
  "Backup of `evil-normal-state-modes'.")

(defvar holy-mode-motion-state-modes-backup nil
  "Backup of `evil-motion-state-modes'.")

(defadvice evil-insert-state (around benedictus-dominus disable)
  "Preparing the holy water flasks."
  (evil-emacs-state))

;;;###autoload
(define-minor-mode holy-mode
  "Global minor mode to repulse the evil from spacemacs.

The `insert state' is replaced by the `emacs state'."
  :global t
  :lighter " holy"
  :group 'spacemacs
  (if holy-mode
      (in-nominus-patris-et-filii-et-sipritus-sancti)
    (amen)))

(defun in-nominus-patris-et-filii-et-sipritus-sancti ()
  "Enter the church of Emacs (wash your hands)."
  ;; allow to return to `normal state' with escape
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  ;; replace `insert state' by `emacs state'
  (ad-enable-advice 'evil-insert-state 'around 'benedictus-dominus)
  (ad-activate 'evil-insert-state)
  ;; change the cursor shape (not sure about this for now)
  ;; (let ((c (when dotspacemacs-colorize-cursor-according-to-state
  ;;            (spacemacs/state-color 'emacs))))
  ;;   (setq evil-emacs-state-cursor `(,c (bar . 2))))
  ;; start all buffers in `emacs state'
  (setq evil-default-state 'emacs)
  (setq holy-mode-normal-state-modes-backup evil-normal-state-modes)
  (setq evil-normal-state-modes nil)
  (setq holy-mode-motion-state-modes-backup evil-motion-state-modes)
  (setq evil-motion-state-modes nil)
  ;; initiate `emacs state' and enter the church
  (evil-emacs-state))

(defun amen ()
  "May the force be with you my son (or not)."
  ;; restore `insert state'
  (ad-disable-advice 'evil-insert-state 'around 'benedictus-dominus)
  (ad-activate 'evil-insert-state)
  ;; restore `normal state'
  (setq evil-default-state 'normal)
  ;; restore per mode default states
  (setq evil-normal-state-modes holy-mode-normal-state-modes-backup)
  (setq evil-motion-state-modes holy-mode-motion-state-modes-backup)
  ;; (set-default-evil-emacs-state-cursor)
  (evil-normal-state))

(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; holy-mode specific key bindings
(global-set-key (kbd "C-a") 'smart-move-beginning-of-line)
(evil-leader/set-key
  "jo" 'evil-open-below
  "jO" 'evil-open-above)
